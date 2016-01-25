# Haskell Bindings to Third Light's Chorus API

These bindings provide a way to access the Chorus API through Haskell.

The interface is split roughly into:

- **Chorus.Api**
  This is the underlying API client responsible for the actual communication.
- **Chorus.Assets**
  Higher level bindings to a few core Asset related functions, for example
  getting assets in some folder. This comes with types from which JSON can
  be decoded into, and assumes a hierarchy starting from Top , then to Users/Groups
  , then to User Folders/Group Folders and beneath those, regular content.

The `HasApi` typeclass allows our API modules, such as `Chorus.Assets`, to
remain quite generic. There is a basic implementation along the lines of:

```
type ApiReader a = ReaderT Session IO a

runApi' :: URL -> ApiReader a -> IO a
runApi' url r = withChorusSession url "" (\s -> runApi s r)

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
import qualified Chorus.Assets as Assets

withChorusSession $ \sess -> do
    runApi sess $ Assets.children Assets.Top
```

This generality allows us to provide alternate implementations which can
make use of our Api modules, such as a mock API for testing, or embedding
the calls straight into a Free Monad.


# Disclaimer

This is all very much a work in progress, and will be expanded upon and
changed as required to suit other projects I am working on. That said, It's a good
place to start if you'd like to interact with Chorus. Feel free to fork!