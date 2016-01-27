{-# LANGUAGE ViewPatterns #-}
--
-- This module gives certain backend objects types on the front so that they
-- are easier to work with, eg Files, Folders etc
--
module Chorus.Assets.Types (
    ID(..),
    -- unique ID for each unique "asset"
    AssetUID(..),
    -- asset type to distinguish overlapping ids:
    AssetType(..),
    -- regular files
    File(..),
    -- regular folders
    Folder(..),
    FolderType(..),
    -- file links
    Link(..),
    -- folder links
    FolderLink(..),
    SharerType(..),
    -- context details (shares UID with backing folder)
    Context(..),
    ContextType(..),
    -- fake folders (Users, Group..)
    PseudoFolder(..),
    -- sum type to hold anything "assetish"
    Asset(..),
    -- class w/props available on everything "assetish"
    AssetLike(..)
) where

import qualified Data.Aeson             as Json
import           Data.Aeson             ((.:),(.=),parseJSON)
import qualified Data.Text              as Text
import           Data.Text              (Text)
import           Control.Applicative    (empty,(<|>))

--
-- ID's
--
newtype ID = ID { unwrapId :: Text }
    deriving (Show, Eq, Ord)

instance Json.FromJSON ID where
    parseJSON (Json.String s) = return (ID s)
    parseJSON (Json.Number n) = return $ ID $ Text.pack $ show $ floor n
    parseJSON _ = empty

instance Json.ToJSON ID where
    toJSON (ID txt) = Json.String txt

--
-- Unique Ids for Assets
--
data AssetUID = AssetID AssetType ID
              | UsersID
              | GroupsID
              | TopID
    deriving (Show, Eq, Ord)

instance Json.FromJSON AssetUID where
    parseJSON obj@(Json.Object o) = fromTypeIdHash <|> fromSomeAsset
      where
        fromSomeAsset = parseJSON obj >>=  \(a :: Asset) -> return (assetUid a)
        fromTypeIdHash = AssetID <$> o .: "type" <*> o .: "id"
    parseJSON _ = empty

instance Json.ToJSON AssetUID where
    toJSON (AssetID ty i) = Json.object ["type" .= ty, "id" .= i]
    toJSON UsersID  = Json.object ["type" .= ("users" :: Text)]
    toJSON GroupsID = Json.object ["type" .= ("groups" :: Text)]
    toJSON TopID    = Json.object ["type" .= ("top" :: Text)]


data AssetType = IsFile
               | IsFolder
               | IsLink
               | IsFolderLink
               deriving (Eq, Show, Ord)

instance Json.FromJSON AssetType where
    parseJSON (Json.String s) = case s of
        "folder"     -> return IsFolder
        "container"  -> return IsFolder
        "file"       -> return IsFile
        "link"       -> return IsLink
        "folderlink" -> return IsFolderLink
        _            -> empty
    parseJSON _ = empty

instance Json.ToJSON AssetType where
    toJSON IsFile       = Json.String "file"
    toJSON IsFolder     = Json.String "folder"
    toJSON IsLink       = Json.String "link"
    toJSON IsFolderLink = Json.String "folderlink"

--
-- Regular files
--
data File = File
    { fileName       :: Text
    , fileParentId   :: ID
    , fileId         :: ID
    , fileParentType :: FolderType
    , filePreviewUrl :: Text
    } deriving (Eq, Show)

instance Json.FromJSON File where
    parseJSON (Json.Object o) =
        File <$> o .: "filename"
             <*> o .: "parentId"
             <*> o .: "id"
             <*> o .: "parentType"
             <*> o .: "previewUrl"
    parseJSON _ = empty

instance AssetLike File where
    assetName      = fileName
    assetUid       = AssetID IsFile . fileId
    assetParentUid = Just . AssetID IsFolder . fileParentId

--
-- Folders/smartfolders/collections
--
data Folder = Folder
    { folderName        :: Text
    , folderDescription :: Text
    , folderParentId    :: Maybe ID
    , folderParentType  :: Maybe FolderType
    , folderId          :: ID
    , folderType        :: FolderType
    , folderContext     :: Text
    } deriving (Eq, Show)

instance Json.FromJSON Folder where
    parseJSON (Json.Object o) =
        Folder <$> o .: "name"
               <*> o .: "description"
               <*> o .: "parentId"
               <*> o .: "parentType"
               <*> o .: "id"
               <*> o .: "folderType"
               <*> o .: "context"
    parseJSON _ = empty

instance AssetLike Folder where
    assetName        = folderName
    assetUid       f = AssetID IsFolder (folderId f)
    assetParentUid f = case folderParentId f of
        Just id' -> Just (AssetID IsFolder id')
        Nothing -> Just (ctxParent $ folderContext f)

data FolderType = ContextFolder
                | SmartFolder
                | Collection
                | NormalFolder
                | PublicFolder
                deriving (Eq, Show, Ord)

instance Json.FromJSON FolderType where
    parseJSON (Json.String s) = case s of
        "contextfolder" -> return ContextFolder
        "smartfolder"   -> return SmartFolder
        "folder"        -> return NormalFolder
        "collection"    -> return Collection
        "publicfolder"  -> return PublicFolder
        _               -> empty
    parseJSON _ = empty

--
-- File Links
--
data Link = Link
    { linkedFileId :: ID
    , linkParentType :: FolderType
    , linkParentId :: ID
    , linkId :: ID
    } deriving (Eq, Show)

instance Json.FromJSON Link where
    parseJSON (Json.Object o) =
        Link <$> o .: "linkedFileId"
             <*> o .: "parentType"
             <*> o .: "parentId"
             <*> o .: "id"
    parseJSON _ = empty

instance AssetLike Link where
    assetName      = const ""
    assetUid       = AssetID IsLink . linkId
    assetParentUid = Just . AssetID IsFolder . linkParentId

--
-- Folder links
--
data FolderLink = FolderLink
    { flinkName       :: Text
    , flinkParentId   :: ID
    , flinkId         :: ID
    , flinkedFolderId :: ID
    , flinkSharerType :: SharerType
    , flinkSharerId   :: ID
    } deriving (Eq, Show)

instance Json.FromJSON FolderLink where
    parseJSON (Json.Object o) =
        FolderLink <$> (o .: "name" <|> return "")
             <*> o .: "parentId"
             <*> o .: "id"
             <*> o .: "linkedFolderId"
             <*> o .: "sharerType"
             <*> o .: "sharerId"
    parseJSON _ = empty

instance AssetLike FolderLink where
    assetName      = flinkName
    assetUid       = AssetID IsFolderLink . flinkId
    assetParentUid = Just . AssetID IsFolder . flinkParentId

data SharerType = UserShare
                | GroupShare
                deriving (Eq, Show, Ord)

instance Json.FromJSON SharerType where
    parseJSON (Json.String s) = case s of
        "user"  -> return UserShare
        "group" -> return GroupShare
        _               -> empty
    parseJSON _ = empty

--
-- a root context
--
data Context = Context
    { contextDomain :: Text
    , contextName :: Text
    , contextId :: ID
    , contextType :: ContextType
    , contextBackingFolder :: ID
    } deriving (Eq, Show)

instance AssetLike Context where
    assetName      = contextName
    -- the unique ID for this is identical to that of the
    -- backing folder; they are two sides of the same coin.
    assetUid         = AssetID IsFolder . contextBackingFolder
    -- this should match the parent of the corresponding
    -- backing folder, so we go through the same call rather
    -- than base decision on contexttype:
    assetParentUid = Just . ctxParent . unwrapId . contextId

instance Json.FromJSON Context where
    parseJSON (Json.Object o) =
        Context <$> o .: "domain"
                <*> o .: "name"
                <*> o .: "id"
                <*> o .: "type"
                <*> o .: "backingFolderId"
    parseJSON _ = empty

data ContextType = UserContext
                 | GroupContext
                 | DomainContext
                 deriving (Eq, Show, Ord)

instance Json.FromJSON ContextType where
    parseJSON (Json.String s) = case s of
        "user"   -> return UserContext
        "group"  -> return GroupContext
        "domain" -> return DomainContext
        _               -> empty
    parseJSON _ = empty

--
-- Fake folders. These only exist for our file hierarchy
--
data PseudoFolder = Users
                  | Groups
                  | Top
                  deriving (Eq, Show, Ord)

instance AssetLike PseudoFolder where
    assetName Users  = "Users"
    assetName Groups = "Groups"
    assetName Top    = "Top"

    assetUid Users  = UsersID
    assetUid Groups = GroupsID
    assetUid Top    = TopID

    assetParentUid Top = Nothing
    assetParentUid _   = Just TopID

--
-- Aggregate the various asset types into one, so that we
-- can use them alongside eachother.
--

data Asset = FileAsset       File
           | FolderAsset     Folder
           -- these guys come in two parts, so some
           -- assembly required!
           | LinkAsset       File   Link
           | FolderLinkAsset Folder FolderLink
           | ContextAsset    Folder Context
           -- fake folders:
           | PseudoFolder    PseudoFolder
           deriving (Eq, Show)

instance Json.FromJSON Asset where
    parseJSON o@(Json.Object _) = FileAsset       <$> parseJSON o
                              <|> FolderAsset     <$> parseJSON o
    parseJSON _ = empty

instance AssetLike Asset where

    -- specific definitions needed for links,
    -- and folderlinks, since they pull info from
    -- more than one place:
    assetName (LinkAsset       f _) = assetName f
    assetName (FolderLinkAsset f _) = assetName f
    assetName a = runOnAsset assetName a

    assetUid (LinkAsset       _ l) = assetUid l
    assetUid (FolderLinkAsset _ l) = assetUid l
    assetUid a = runOnAsset assetUid a

    assetParentUid (LinkAsset       _ l) = assetParentUid l
    assetParentUid (FolderLinkAsset _ l) = assetParentUid l
    assetParentUid a = runOnAsset assetParentUid a

runOnAsset :: (forall a. AssetLike a => a -> b) -> Asset -> b
runOnAsset fn (FileAsset f)      = fn f
runOnAsset fn (FolderAsset f)    = fn f
runOnAsset fn (PseudoFolder f)   = fn f
runOnAsset fn (ContextAsset _ c) = fn c
runOnAsset _ _ = error "runOnAsset called on variant it shouldnt have been"

--
-- A general interface to pull things out of the assets that
-- are common to all of them
--

class AssetLike a where
    assetName      :: a -> Text
    assetUid       :: a -> AssetUID
    assetParentUid :: a -> Maybe AssetUID

ctxParent :: Text -> AssetUID
ctxParent (Text.isPrefixOf "user" -> True) = UsersID
ctxParent (Text.isPrefixOf "group" -> True) = GroupsID
ctxParent _ = UsersID
