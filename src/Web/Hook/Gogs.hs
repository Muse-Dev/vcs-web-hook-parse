{- This file is part of vcs-web-hook-parse.
 -
 - Written in 2015 by fr33domlover <fr33domlover@rel4tion.org>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - The author(s) have dedicated all copyright and related and neighboring
 - rights to this software to the public domain worldwide. This software is
 - distributed without any warranty.
 -
 - You should have received a copy of the CC0 Public Domain Dedication along
 - with this software. If not, see
 - <http://creativecommons.org/publicdomain/zero/1.0/>.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Web.Hook.Gogs
    ( CommitID
    , Url
    , User (..)
    , Commit (..)
    , Repository (..)
    , Push (..)
    , parse
    )
where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

type CommitID = T.Text

type Url = T.Text

data User = User
    { userName     :: T.Text
    , userEmail    :: T.Text
    , userUsername :: T.Text
    }
    deriving Show

data Commit = Commit
    { commitId      :: CommitID
    , commitMessage :: T.Text
    , commitUrl     :: Url
    , commitAuthor  :: User
    }
    deriving Show

data Repository = Repository
    { repoId          :: Int
    , repoName        :: T.Text
    , repoUrl         :: Url
    , repoDescription :: T.Text
    , repoWebsite     :: Url
    , repoWatchers    :: Int
    , repoOwner       :: User
    , repoPrivate     :: Bool
    }
    deriving Show

data Push = Push
    { pushSecret     :: T.Text
    , pushRef        :: T.Text
    , pushCommits    :: [Commit]
    , pushRepository :: Repository
    , pushPusher     :: User
    , pushBefore     :: CommitID
    , pushAfter      :: CommitID
    , pushCompareUrl :: Url
    }
    deriving Show

instance FromJSON User where
    parseJSON (Object v) =
        User <$>
        (v .: "name" <|> v .: "full_name") <*>
        v .: "email" <*>
        v .: "username"
    parseJSON _          = mzero

instance FromJSON Commit where
    parseJSON (Object v) =
        Commit <$>
        v .: "id" <*>
        v .: "message" <*>
        v .: "url" <*>
        v .: "author"
    parseJSON _          = mzero

instance FromJSON Repository where
    parseJSON (Object v) =
        Repository <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "html_url" <*>
        v .: "description" <*>
        v .: "website" <*>
        v .: "watchers_count" <*>
        v .: "owner" <*>
        v .: "private"
    parseJSON _          = mzero

instance FromJSON Push where
    parseJSON (Object v) =
        Push <$>
        v .:? "secret" .!= "" <*>
        v .: "ref" <*>
        v .: "commits" <*>
        v .: "repository" <*>
        v .: "pusher" <*>
        v .: "before" <*>
        v .: "after" <*>
        v .: "compare_url"
    parseJSON _          = mzero

-- | Parse a JSON string (the body of the HTTP request) into event information.
-- If parsing fails, return 'Left' an error message.
parse :: B.ByteString -> Either String Push
parse = eitherDecode
