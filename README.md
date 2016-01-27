# Haskell Bindings to Third Light's API

These bindings provide a way to access the Third Light API through Haskell.

The interface is split roughly into:

- **ThirdLight.Api**
  This is the underlying API client responsible for the actual communication.
- **ThirdLight.Assets**
  Higher level bindings to a few core Asset related functions, for example
  getting assets in some folder. This comes with types from which JSON can
  be decoded into, and assumes a hierarchy starting from Top , then to Users/Groups
  , then to User Folders/Group Folders and beneath those, regular content.

The `HasApi` typeclass allows our API modules, such as `ThirdLight.Assets`, to
remain quite generic. There is a basic implementation along the lines of:

```
type ApiReader a = ReaderT Session IO a

runApi' :: URL -> ApiReader a -> IO a
runApi' url r = withSession url "" (\s -> runApi s r)

runApi :: Session -> ApiReader a -> IO a
runApi = flip Reader.runReaderT

instance HasApi (ReaderT Session IO) where
    makeApiCall txt json = do
        sess <- Reader.ask
        liftIO $ api sess txt json
```

Allowing one to make use of the API modules in an IO context by doing
something along the lines of:

```
import ThirdLight.Api (runApi, runApiE, withSession)
import ThirdLight.Assets (children, AssetUID(TopID))

withSession $ \sess -> do
    runApi sess $ children TopID
```

In addition, the `runApiE` and `xxxE` methods are exposed from the API,
allowing for the composition of functions with easy behind-the-scenes error
bailout. One might use these as:

```
import ThirdLight.Api (runApi, runApiE, withSession)
import ThirdLight.Assets (childrenE, detailsE, AssetUID(AssetID, TopID, AssetType(..)))

withSession $ \sess -> do
    eResponse <- runApi sess $ runApiE $ do
        topAssets <- childrenE TopID
        otherLark <- childrenE (AssetID IsFolder 12345)
        details   <- detailsE (AssetID IsFile 45362632)
        return (topAssets, otherLark, details)

    -- we only have to handle errors once for everything in runApiE:
    case eResponse of
        Left err -> whoops
        Right (top,other,details) -> yay
```

The use of the `HasApi` typeclass allows alternate implementations which can
make use of our Api modules, such as a mock API for testing without need for IO,
or embedding the calls straight into a Free Monad.

# Disclaimer

This is all very much a work in progress, and will be expanded upon and
changed as required to suit other projects I am working on. That said, It's a good
place to start if you'd like to interact with the Third Light API. Feel free to fork!