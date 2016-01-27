module ThirdLight.Api (
    HasApi(..),
    request,
    request',
    withSession,
    withSession',

    -- our basic HasApi instance:
    runApi,
    runApi',

    -- our basic error context:
    ApiError,
    apiE,
    runApiE,

    Session,
    getSessionKey,
    setSessionKey,
    getImsUrl,
    setImsUrl,

    Err(..),
    noData,

    post,
    postWith
) where

import qualified Network.Wreq.Session     as Sess
import qualified Network.Wreq.Types       as Wreq
import qualified Network.Wreq             as Wreq
import qualified Data.Text                as Text
import           Control.Concurrent       (MVar,newMVar,readMVar,modifyMVar_)
import           Data.ByteString.Lazy     (ByteString)
import           Data.Text                (Text)
import           Data.Monoid              ((<>))
import           Control.Applicative      ((<|>))
import           Control.Lens             hiding ((.=))
import           Control.Exception        (Handler(..), catches, SomeException)
import           Network.Wreq.Lens        (responseBody)
import           Data.Aeson.Lens          (key, _String)
import qualified Control.Monad.Reader     as Reader
import           Control.Monad.Reader     (ReaderT)
import qualified Data.Aeson               as Json
import           Data.Aeson               ((.=),FromJSON(..))
import qualified Data.Map.Strict          as Map
import           Data.Map.Strict          (Map)
import qualified Data.Set                 as Set
import           Data.Set                 (Set)
import           Control.Monad.Except

type JSON = Json.Value
type URL = Text
type CacheKey = (Text,ByteString)

--
-- We can wrap API calls in this basic error context to
-- avoid having to check for errors every step:
--
type ApiError m a = ExceptT Err m a

apiE :: (Json.FromJSON res, HasApi m) => Text -> JSON -> ApiError m res
apiE txt json = lift (makeApiCall txt json) >>= \e -> case e of
    Left err  -> throwError err
    Right res -> return res

runApiE :: HasApi m => ApiError m a -> m (Either Err a)
runApiE = runExceptT

--
-- Things can become a member of this class to say
-- that they have access to the API. This allows us
-- to write api stuff separate from any particular monad
-- eg our RouteM stuff
--
class Monad m => HasApi m where
    makeApiCall :: FromJSON res => Text -> JSON -> m (ChorusResult res)

-- a quick reader impl to test HasApi functions:
type ApiReader a = ReaderT Session IO a

runApi' :: URL -> ApiReader a -> IO a
runApi' url r = withSession url "" (\s -> runApi s r)

runApi :: Session -> ApiReader a -> IO a
runApi = flip Reader.runReaderT

instance HasApi (ReaderT Session IO) where
    makeApiCall txt json = do
        sess <- Reader.ask
        liftIO $ request sess txt json

-- Wrap the session data in an mvar so it can be shared
-- around and will stay uptodate.
type Session = MVar ChorusSession
data ChorusSession = ChorusSession
    { wreqSession :: Sess.Session
    , imsUrl      :: URL
    , sessionKey  :: Text
    , cache       :: Map CacheKey JSON
    }
    deriving Show

readSession :: MonadIO m => (ChorusSession -> a) -> Session -> m a
readSession fn sess = liftIO $ readMVar sess >>= return . fn

writeSession :: MonadIO m => (ChorusSession -> ChorusSession) -> Session -> m ()
writeSession fn sess = liftIO $ modifyMVar_ sess (return . fn)

getSessionKey :: MonadIO m => Session -> m Text
getSessionKey = readSession sessionKey

setSessionKey :: MonadIO m => Text -> Session -> m ()
setSessionKey k = writeSession (\s -> s{sessionKey = k})

getImsUrl :: MonadIO m => Session -> m URL
getImsUrl = readSession imsUrl

setImsUrl :: MonadIO m => URL -> Session -> m ()
setImsUrl url = writeSession (\s -> s{imsUrl = url})

-- Short names for things as we expect to import qualified
data Err = ActionError     Text Text
         | ApiError        Text Text
         | ConnectionError Text
         | DecodeError     ByteString
         | ShapeError      Text
         | CastError       Text
         deriving Show

type ChorusResult res = Either Err res

noData :: JSON
noData = Json.object []

--
-- create a chorus session and provide it to an inner func:
--
withSession :: MonadIO m => URL -> Text -> (Session -> IO a) -> m a
withSession url sessionKey fn = liftIO $ do
    Sess.withAPISession $ \sess -> newMVar (ChorusSession sess url sessionKey Map.empty) >>= fn

withSession' :: MonadIO m => (Session -> IO a) -> m a
withSession' fn = withSession "" "" fn

--
-- creates a session to some URL and then runs the request;
-- this is handy for quick debugging, but afaik the wreq session
-- is cleaned up each time and not reused.
--
request' :: (FromJSON res, MonadIO m) => URL -> Text -> JSON -> m (ChorusResult res)
request' url action reqData = liftIO $ withSession url "" (\sess -> request sess action reqData)

--
-- A low level API interface which just does basic error handling
-- and session updating.
--
request :: (FromJSON res, MonadIO m) => Session -> Text -> JSON -> m (ChorusResult res)
request svar action reqData = liftIO $ flip catches handleErrs $ do

    s <- readMVar svar
    r <- case getFromCache s action reqData of
        Just result -> return $ Right result
        Nothing     -> callApi s

    return $ case r of
        Left err -> Left err
        Right json -> case Json.fromJSON json of
            Json.Error   err -> Left $ CastError (Text.pack err)
            Json.Success val -> Right val

  where
    url s = imsUrl s <> "/api.json.tlx"
    --
    -- Construct an API request
    --
    req s = Json.object
        [ "action"     .= action
        , "inParams"   .= reqData
        , "sessionId"  .= sessionKey s
        , "apiVersion" .= ("1.0" :: String)
        ]
    --
    -- Make the request
    --
    callApi :: ChorusSession -> IO (ChorusResult Json.Value)
    callApi s = do
        res <- Sess.post (wreqSession s) (Text.unpack $ url s) (req s)
        let resBody = (res ^. responseBody)
        case Json.decode resBody :: Maybe JSON of
            Nothing  -> return $ Left (DecodeError resBody)
            Just val -> handleVal s val
    --
    -- Handle the response, updating the session if a new sessionId comes back
    --
    handleVal :: ChorusSession -> JSON -> IO (ChorusResult Json.Value)
    handleVal s val =
        let (Just sessId) = (val ^? key "sessionId" . _String)
                 <|> (val ^? key "outParams" . key "sessionId" . _String)
                 <|> Just (sessionKey s)
            update res = modifyMVar_ svar $ \s' -> return $ case res of
                Left _ -> s'
                Right json -> (updateCache s' action reqData json){sessionKey = sessId}
        in case Json.fromJSON val of
            Json.Success res -> update res >> return res
            Json.Error err   -> return $ Left $ ShapeError ("decode to Result failure: " <> showText err)
    --
    -- Handle wreq spitting out some error. Add more specific handlers to
    -- catch things like bad status codes, or bad URLs
    --
    handleErrs =
        [ Handler (\(e :: SomeException) -> msg $ "Something went wrong connecting: " <> showText e)
        ]
      where msg str = return $ Left $ ConnectionError str

--
-- alias wreq functionality using the chorus session,
-- so we dont need to expose the underlying wreq session.
--
post :: (Wreq.Postable p, MonadIO m) => Session -> Text -> p -> m (Wreq.Response ByteString)
post = postWith Wreq.defaults

postWith :: (Wreq.Postable p, MonadIO m) => Wreq.Options -> Session -> Text -> p -> m (Wreq.Response ByteString)
postWith opts svar txt body = liftIO $ do
    s <- readMVar svar
    Sess.postWith opts (wreqSession s) (Text.unpack txt) body

--
-- basic cache handling with unwrapped session
--
-- if a call is on the whitelist, using it does not trigger any changes.
-- otherwise, clear cache incase it is out of date. This means we should
-- get a reasonable degree of caching very easily. A separate process will
-- need to update the cache in response to external events.
--

getFromCache :: ChorusSession -> Text -> JSON -> Maybe JSON
getFromCache s action reqData = Map.lookup (keyFrom action reqData) (cache s)

updateCache :: ChorusSession -> Text -> JSON -> JSON -> ChorusSession
updateCache s action reqData res = if Set.member (Text.toLower action) cacheWhitelist
    then s{cache = Map.insert (keyFrom action reqData) res (cache s) }
    else s{cache = Map.empty }

keyFrom :: Text -> JSON -> CacheKey
keyFrom txt json = (txt, Json.encode json)

cacheWhitelist :: Set Text
cacheWhitelist = Set.fromList $ fmap Text.toLower
    [ "Core.GetEnvironment"
    , "Core.GetApiListing"
    , "Users.GetRootContexts"
    , "Folders.GetFolderDetails"
    ]

--
-- Convert response JSON to a Result object, which is either an
-- error or a JSON object.
--
instance FromJSON (ChorusResult Json.Value) where
    parseJSON v@(Json.Object _) = return $ do
        apiRes    <- toEither (v ^? key "result" . key "api"    . _String) (ShapeError "no res.result.api")
        actionRes <- toEither (v ^? key "result" . key "action" . _String) (ShapeError "no res.result.action")
        let body  = maybeDef (v ^? key "outParams")                       (Json.object [])
        let debug = maybeDef (v ^? key "result" . key "debug"  . _String) ""
        case (apiRes,actionRes) of
            ("OK","OK") -> Right body
            ("OK",err)  -> Left (ActionError err debug)
            (err,_)     -> Left (ApiError    err debug)
    parseJSON _ = return $ Left (ShapeError "response not an object")

toEither :: Maybe a -> err -> Either err a
toEither (Just val) _   = Right val
toEither Nothing    err = Left err

maybeDef :: Maybe a -> a -> a
maybeDef (Just val) _   = val
maybeDef Nothing    val = val

showText :: Show a => a -> Text
showText = Text.pack . show