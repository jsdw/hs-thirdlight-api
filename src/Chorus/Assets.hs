{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Chorus.Assets (

    -- get details of assets given some listof AssetUIDs:
    details,
    detail,
    children,
    parent,
    childIds,

    -- re-expose asset types:
    module Chorus.Assets.Types

) where

import qualified Data.Text     as Text
import           Data.Text     (Text)
import qualified Data.Aeson    as Json
import           Data.Aeson    ((.=))
import qualified Chorus.Api    as Chorus
import           Chorus.Api    (HasApi)
import qualified Data.Map      as Map
import           Data.Map      (Map)
import           Data.Monoid   ((<>))
import           Data.Foldable (foldl')
import           Control.Monad.Except

import Chorus.Assets.Types

type JSON = Json.Value
type ApiError m a = ExceptT Chorus.Err m a

-- this is the only external function we need; instances of
-- HasApi will decide how to actually make it work.
api :: (Json.FromJSON res, HasApi m) => Text -> JSON -> m (Either Chorus.Err res)
api = Chorus.makeApiCall

-- this one can be used in our ApiError context so that errors
-- are handled transparently
apiE :: (Json.FromJSON res, HasApi m) => Text -> JSON -> ApiError m res
apiE txt json = lift (api txt json) >>= wrapEither

wrapEither :: HasApi m => Either Chorus.Err a -> ApiError m a
wrapEither e = case e of
    Left err  -> throwError err
    Right res -> return res

runApiE :: HasApi m => ApiError m a -> m (Either Chorus.Err a)
runApiE = runExceptT

--
-- Expose a nicer API by unwrapping from the error context:
--
children a = runApiE $ childrenE a
parent   a = runApiE $ parentE a
detail   a = runApiE $ detailE a
childIds a = runApiE $ childIdsE a
details  a = runApiE $ detailsE a

--
-- Combine childIds and details to get children.
--
childrenE :: HasApi m => Asset -> ApiError m (Map AssetUID Asset)
childrenE a = childIdsE a >>= detailsE

parentE :: (HasApi m, AssetLike a) => a -> ApiError m (Maybe Asset)
parentE a = case assetParentUid a of
    Nothing -> return Nothing
    Just assetId -> fmap Just (detailE assetId)

detailE :: HasApi m => AssetUID -> ApiError m Asset
detailE a = detailsE [a] >>= return . snd . head . Map.toList

--
-- Gets child Ids for any Asset provided.
--
childIdsE :: forall m. HasApi m => Asset -> ApiError m [AssetUID]
childIdsE a = case a of
    FolderAsset     f   -> getSorted f
    FolderLinkAsset f _ -> getSorted f
    ContextAsset    f _ -> getSorted f
    PseudoFolder Top    -> return [UsersID, GroupsID]
    PseudoFolder Users  -> getUserIds
    PseudoFolder Groups -> getGroupIds
    _                   -> return []
  where
    getSorted :: Folder -> ApiError m [AssetUID]
    getSorted f = apiE "folders.GetSortedChildren" (sortedParams $ folderId f)

    getCtxs :: ApiError m (Map Text Context)
    getCtxs = apiE "users.getRootContexts" (Json.object [])

    cid :: Context -> Text
    cid = unwrapId . contextId

    getUserIds =
        let fn c = cid c == "me" || (Text.isPrefixOf "user" $ cid c)
        in getCtxs >>= return . fmap assetUid . filter fn . foldr (:) []
    getGroupIds =
        let fn c = Text.isPrefixOf "group" $ cid c
        in getCtxs >>= return . fmap assetUid . filter fn . foldr (:) []
    sortedParams folderId = Json.object
        [ "folderId" .= folderId
        , "sortInfo" .= [Json.object ["field" .= ("ims:filename" :: Text)]]
        ]

--
-- Get details given some UIDs. batches api calls where possible
-- scope m with forall so we can use it in inner type sigs
--
detailsE :: forall m. HasApi m => [AssetUID] -> ApiError m (Map AssetUID Asset)
detailsE allIds = do

    files <- fmap toList $ getFiles fileIds
    folders <- getFolders folderIds >>= embelishFolders . toList
    links <- getLinks linkIds >>= embelishLinks . toList
    folderLinks <- getFolderLinks folderLinkIds >>= embelishFolderLinks . toList

    -- add users/groups/top pseudofolders to list if needbe
    let users  = if isUsersId then [PseudoFolder Users]  else []
    let groups = if isGroupId then [PseudoFolder Groups] else []
    let top    = if isTopId   then [PseudoFolder Top]    else []

    return $ Map.fromList $ fmap
        (\a -> (assetUid a, a))
        (files <> folders <> links <> folderLinks <> users <> groups <> top)

  where
    GroupedIds{..} = groupIds allIds

    getFiles ids = if null fileIds
        then return Map.empty
        else apiE "files.GetAssetdetails" (Json.object [ "assetId" .= ids ])
    getFolders ids = if null folderIds
        then return Map.empty
        else apiE "folders.GetFolderdetails" (Json.object [ "folderId" .= ids ])
    getLinks ids = if null linkIds
        then return Map.empty
        else apiE "files.GetLinkDetails" (Json.object [ "linkId" .= ids ])
    getFolderLinks ids = if null linkIds
        then return Map.empty
        else apiE "links.GetDetails" (Json.object [ "linkId" .= ids ])

    toList :: Map Text val -> [val]
    toList = foldr (:) []

    embelishLinks :: [Link] -> ApiError m [Asset]
    embelishLinks links = do
        fileMap <- getFiles $ fmap (unwrapId . linkedFileId) links
        return $ foldr (fn fileMap) [] links
      where
        fn fileMap l as = case Map.lookup (unwrapId $ linkedFileId l) fileMap of
            Nothing -> as
            Just f  -> LinkAsset f l : as

    embelishFolderLinks :: [FolderLink] -> ApiError m [Asset]
    embelishFolderLinks links = do
        folderMap <- getFolders $ fmap (unwrapId . flinkedFolderId) links
        return $ foldr (fn folderMap) [] links
      where
        fn folderMap l as = case Map.lookup (unwrapId $ flinkedFolderId l) folderMap of
            Nothing -> as
            Just f  -> FolderLinkAsset f l : as

    embelishFolders :: [Folder] -> ApiError m [Asset]
    embelishFolders folders = do
        userContextMap <- apiE "users.getRootContexts" (Json.object [])
        return $ foldr (fn userContextMap) [] folders
      where
        fn ctxMap f as = case (folderParentId f, Map.lookup (folderContext f) ctxMap) of
            (Nothing, Just c) -> ContextAsset f c : as
            _                 -> FolderAsset f : as

--
-- Group UIDs by types, splitting out the core bits.
--
data GroupedIds = GroupedIds
    { fileIds       :: [Text]
    , folderIds     :: [Text]
    , linkIds       :: [Text]
    , folderLinkIds :: [Text]
    , isUsersId     :: Bool
    , isGroupId     :: Bool
    , isTopId       :: Bool
    }

emptyGroupedIds :: GroupedIds
emptyGroupedIds = GroupedIds [] [] [] [] False False False

groupIds :: [AssetUID] -> GroupedIds
groupIds ids = foldl' byType emptyGroupedIds ids where
    byType gs UsersID  = gs { isUsersId = True }
    byType gs GroupsID = gs { isGroupId = True }
    byType gs TopID    = gs { isTopId = True }
    byType gs (AssetID ty (ID i)) = case ty of
        IsFile       -> gs { fileIds = i : fileIds gs }
        IsFolder     -> gs { folderIds = i : folderIds gs }
        IsLink       -> gs { linkIds = i : linkIds gs }
        IsFolderLink -> gs { folderLinkIds = i : folderLinkIds gs }