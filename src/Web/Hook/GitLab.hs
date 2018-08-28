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

module Web.Hook.GitLab
    ( CommitID
    , Url
    , File
    , Author (..)
    , User (..)
    , Commit (..)
    , MergeEndpoint (..)
    , Repository (..)
    , Diff (..)
    , Snippet (..)
    , NoteTarget (..)
    , Issue (..)
    , MergeRequest (..)
    , Note (..)
    , Push (..)
    , IssueEvent (..)
    , MergeRequestEvent (..)
    , NoteEvent (..)
    , Event (..)
    , Project (..)
    , parse
    )
where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

type CommitID = T.Text

type Url = T.Text

type File = T.Text

text :: Parser T.Text -> T.Text -> Parser T.Text
text parser expected = do
    got <- parser
    if got == expected
        then return got
        else mzero

data Author = Author
    { authorName  :: T.Text
    , authorEmail :: T.Text
    }
    deriving (Eq,Ord,Show)

instance FromJSON Author where
    parseJSON (Object o) =
        Author <$>
        o .: "name" <*>
        o .: "email"
    parseJSON v          = typeMismatch "Author" v

data User = User
    { userName     :: T.Text
    , userUsername :: T.Text
    , userAvatar   :: Url
    }
    deriving (Eq,Ord,Show)

instance FromJSON User where
    parseJSON (Object o) =
        User <$>
        o .: "name" <*>
        o .: "username" <*>
        o .: "avatar_url"
    parseJSON v          = typeMismatch "User" v

data Commit = Commit
    { commitId        :: CommitID
    , commitMessage   :: T.Text
    , commitTimestamp :: T.Text
    , commitUrl       :: Url
    , commitAuthor    :: Author
    , commitAdded     :: [File]
    , commitModified  :: [File]
    , commitRemoved   :: [File]
    }
    deriving (Eq,Ord,Show)

instance FromJSON Commit where
    parseJSON (Object o) =
        Commit <$>
        o .:  "id"              <*>
        o .:  "message"         <*>
        o .:  "timestamp"       <*>
        o .:  "url"             <*>
        o .:  "author"          <*>
        o .:? "added"    .!= [] <*>
        o .:? "modified" .!= [] <*>
        o .:? "removed"  .!= []
    parseJSON v          = typeMismatch "Commit" v

data MergeEndpoint = MergeEndpoint
    { mepName       :: T.Text
    , mepSshUrl     :: Url
    , mepHttpUrl    :: Url
    , mepWebUrl     :: Url
    , mepVisibility :: Int
    , mepNamespace  :: T.Text
    }
    deriving (Eq,Ord,Show)

instance FromJSON MergeEndpoint where
    parseJSON (Object o) =
        MergeEndpoint <$>
        o .: "name" <*>
        o .: "git_ssh_url" <*>
        o .: "git_http_url" <*>
        o .: "web_url" <*>
        o .: "visibility_level" <*>
        o .: "namespace"
    parseJSON v          = typeMismatch "MergeEndpoint" v

data Repository = Repository
    { repoName       :: T.Text
    , repoUrl        :: Url
    , repoDesc       :: T.Text
    , repoHomepage   :: Url
    , repoGitHttpUrl :: Maybe Url
    , repoGitSshUrl  :: Maybe Url
    , repoVisibility :: Maybe Int
    }
    deriving (Eq,Ord,Show)

instance FromJSON Repository where
    parseJSON (Object o) =
        Repository <$>
        o .:  "name" <*>
        o .:  "url" <*>
        o .:  "description" <*>
        o .:  "homepage" <*>
        o .:? "git_http_url" <*>
        o .:? "git_ssh_url" <*>
        o .:? "visibility_level"
    parseJSON v          = typeMismatch "Repository" v

data Project = Project
    { projName              :: T.Text
    , projDesc              :: T.Text
    , projAvatarUrl         :: Maybe Url
    , projNamespace         :: T.Text
    , projVisibility        :: Int
    , projPathWithNamespace :: T.Text
    , projDefaultBranch     :: Maybe T.Text
    , projHomepage          :: Url
    , projWebUrl            :: Url
    , projGitHttpUrl        :: Url
    , projGitSshUrl         :: Url
    }
    deriving (Eq,Ord,Show)

instance FromJSON Project where
    parseJSON (Object o) =
        Project <$>
            o .:  "name" <*>
            o .:  "description" <*>
            o .:? "avatar_url" <*>
            o .:  "namespace" <*>
            o .:  "visibility_level" <*>
            o .:  "path_with_namespace" <*>
            o .:? "default_branch" <*>
            o .:  "homepage" <*>
            o .:  "web_url" <*>
            o .:  "git_http_url" <*>
            o .:  "git_ssh_url"
    parseJSON v = typeMismatch "Project" v

data Issue = Issue
    { issueInternalId  :: Int
    , issueTitle       :: T.Text
    , issueAssigneeId  :: Maybe Int
    , issueAuthorId    :: Int
    , issueProjectId   :: Int
    , issueCreatedAt   :: T.Text
    , issueUpdatedAt   :: T.Text
    , issuePosition    :: Int
    , issueBranch      :: Maybe T.Text
    , issueDescription :: T.Text
    , issueMilestoneId :: Maybe Int
    , issueState       :: T.Text
    , issueId          :: Int
    , issueUrl         :: Url
    }
    deriving (Eq,Ord,Show)

instance FromJSON Issue where
    parseJSON (Object o) =
        Issue <$>
        o .: "id" <*>
        o .: "title" <*>
        o .:? "assignee_id" <*>
        o .: "author_id" <*>
        o .: "project_id" <*>
        o .: "created_at" <*>
        o .: "updated_at" <*>
        o .: "position" <*>
        o .: "branch_name" <*>
        o .: "description" <*>
        o .: "milestone_id" <*>
        o .: "state" <*>
        o .: "iid" <*>
        o .:? "url" .!= T.empty
    parseJSON v          = typeMismatch "Issue" v

data MergeRequest = MergeRequest
    { mrInternalId      :: Int
    , mrTargetBranch    :: T.Text
    , mrSourceBranch    :: T.Text
    , mrSourceProjectId :: Int
    , mrAuthorId        :: Int
    , mrAssigneeId      :: Maybe Int
    , mrTitle           :: T.Text
    , mrCreatedAt       :: T.Text
    , mrUpdatedAt       :: T.Text
    --, mrStCommits       :: [Int] -- dummy type; what is this?
    --, mrStDiffs         :: [Int] -- dummy type; what is this?
    , mrMilestoneId     :: Maybe Int
    , mrState           :: T.Text
    , mrMergeStatus     :: T.Text
    , mrTargetProjectId :: Int
    , mrId              :: Int
    , mrDescription     :: T.Text
    , mrSource          :: MergeEndpoint
    , mrTarget          :: MergeEndpoint
    , mrLastCommit      :: Commit
    , mrWorkInProgress  :: Bool
    , mrUrl             :: Url
    }
    deriving (Eq,Ord,Show)

instance FromJSON MergeRequest where
    parseJSON (Object o) =
        MergeRequest <$>
        o .: "id" <*>
        o .: "target_branch" <*>
        o .: "source_branch" <*>
        o .: "source_project_id" <*>
        o .: "author_id" <*>
        o .:? "assignee_id" <*>
        o .: "title" <*>
        o .: "created_at" <*>
        o .: "updated_at" <*>
        --v .: "st_commits" <*>
        --v .: "st_diffs" <*>
        o .: "milestone_id" <*>
        o .: "state" <*>
        o .: "merge_status" <*>
        o .: "target_project_id" <*>
        o .: "iid" <*>
        o .: "description" <*>
        o .: "source" <*>
        o .: "target" <*>
        o .: "last_commit" <*>
        o .: "work_in_progress" <*>
        o .:? "url" .!= T.empty
    parseJSON v          = typeMismatch "MergeRequest" v

data Diff = Diff
    { diffDiff        :: T.Text
    , diffNewPath     :: T.Text
    , diffOldPath     :: T.Text
    , diffAMode       :: T.Text
    , diffBMode       :: T.Text
    , diffNewFile     :: Bool
    , diffRenamedFile :: Bool
    , diffDeletedFile :: Bool
    }
    deriving (Eq,Ord,Show)

instance FromJSON Diff where
    parseJSON (Object o) =
        Diff <$>
        o .: "diff" <*>
        o .: "new_path" <*>
        o .: "old_path" <*>
        o .: "a_mode" <*>
        o .: "b_mode" <*>
        o .: "new_file" <*>
        o .: "renamed_file" <*>
        o .: "deleted_file"
    parseJSON v          = typeMismatch "Diff" v

data Note = Note
    { noteId         :: Int
    , noteNote       :: T.Text
    --, noteType       :: NoteableType
    , noteAuthorId   :: Int
    , noteCreatedAt  :: T.Text
    , noteUpdatedAt  :: T.Text
    , noteProjectId  :: Int
    --, noteAttachment :: Maybe ()
    , noteLineCode   :: Maybe T.Text
    , noteCommitId   :: CommitID
    , noteNoteableId :: Maybe Int
    , noteSystem     :: Bool
    , noteStDiff     :: Maybe Diff
    , noteUrl        :: Url
    }
    deriving (Eq,Ord,Show)

instance FromJSON Note where
    parseJSON (Object o) =
        Note <$>
        o .: "id" <*>
        o .: "note" <*>
        --o .: "noteable_type" <*>
        o .: "author_id" <*>
        o .: "created_at" <*>
        o .: "updated_at" <*>
        o .: "project_id" <*>
        --o .: "attachment" <*>
        o .: "line_code" <*>
        o .: "commit_id" <*>
        o .: "noteable_id" <*>
        o .: "system" <*>
        o .: "st_diff" <*>
        o .: "url"
    parseJSON v          = typeMismatch "Note" v

data Push = Push
    { pushBefore       :: CommitID
    , pushAfter        :: CommitID
    , pushProject      :: Project
    , pushRef          :: T.Text
    , pushUserId       :: Int
    , pushUserName     :: T.Text
    , pushUserEmail    :: T.Text
    , pushProjectId    :: Int
    , pushRepository   :: Repository
    , pushCommits      :: [Commit]
    , pushCommitsTotal :: Int
    }
    deriving (Eq,Ord,Show)

instance FromJSON Push where
    parseJSON (Object v) =
        Push <$>
        v .: "before" <*>
        v .: "after" <*>
        v .: "project" <*>
        v .: "ref" <*>
        v .: "user_id" <*>
        v .: "user_name" <*>
        v .: "user_email" <*>
        v .: "project_id" <*>
        v .: "repository" <*>
        v .: "commits" <*>
        v .: "total_commits_count"
    parseJSON _          = mzero

data IssueEvent = IssueEvent
    { ieUser    :: User
    , ieRepo    :: Repository
    , ieProject :: Project
    , ieIssue   :: Issue
    , ieAction  :: T.Text
    }
    deriving (Eq,Ord,Show)

instance FromJSON IssueEvent where
    parseJSON (Object o) = do
        user <- o .: "user"
        repo <- o .: "repository"
        proj <- o .: "project"
        attrs <- o .: "object_attributes"
        issue <- o .: "object_attributes"
        action <- attrs .: "action"
        return $ IssueEvent user repo proj issue action
    parseJSON v          = typeMismatch "IssueEvent" v

data MergeRequestEvent = MergeRequestEvent
    { mreUser    :: User
    , mreRequest :: MergeRequest
    }
    deriving (Eq,Ord,Show)

instance FromJSON MergeRequestEvent where
    parseJSON (Object o) = do
        user <- o .: "user"
        mr <- o .: "object_attributes"
        return $ MergeRequestEvent user mr
    parseJSON v          = typeMismatch "MergeRequestEvent" v

data Snippet = Snippet
    { snippetId         :: Int
    , snippetTitle      :: T.Text
    , snippetContent    :: T.Text
    , snippetAuthorId   :: Int
    , snippetProjectId  :: Int
    , snippetCreatedAt  :: T.Text
    , snippetUpdatedAt  :: T.Text
    , snippetFileName   :: T.Text
    , snippetExpiresAt  :: Maybe T.Text
    , snippetType       :: T.Text
    , snippetVisibility :: Int
    }
    deriving (Eq,Ord,Show)

instance FromJSON Snippet where
    parseJSON (Object o) =
        Snippet <$>
        o .: "id" <*>
        o .: "title" <*>
        o .: "content" <*>
        o .: "author_id" <*>
        o .: "project_id" <*>
        o .: "created_at" <*>
        o .: "updated_at" <*>
        o .: "file_name" <*>
        o .: "expires_at" <*>
        o .: "type" <*>
        o .: "visibility_level"
    parseJSON v          = typeMismatch "Snippet" v

data NoteTarget
    = NTCommit Commit
    | NTIssue Issue
    | NTMergeRequest MergeRequest
    | NTSnippet Snippet
    deriving (Eq,Ord,Show)

data NoteEvent = NoteEvent
    { neUser      :: User
    , neProjectId :: Int
    , neProject   :: Project
    , neRepo      :: Repository
    , neNote      :: Note
    , neTarget    :: NoteTarget
    }
    deriving (Eq,Ord,Show)

instance FromJSON NoteEvent where
    parseJSON (Object o) =
        NoteEvent <$>
        o .: "user" <*>
        o .: "project_id" <*>
        o .: "project" <*>
        o .: "repository" <*>
        o .: "object_attributes" <*>
        ( NTCommit       <$> o .: "commit"        <|>
          NTMergeRequest <$> o .: "merge_request" <|>
          NTIssue        <$> o .: "issue"         <|>
          NTSnippet      <$> o .: "snippet"
        )
    parseJSON v          = typeMismatch "NoteEvent" v

data Event
    = EventPush Push
    | EventPushTag Push
    | EventIssue IssueEvent
    | EventMergeRequest MergeRequestEvent
    | EventNote NoteEvent
    deriving (Eq,Ord,Show)

instance FromJSON Event where
    parseJSON v@(Object o) =
        let kind = text $ o .: "object_kind"
        in  kind "push"          *> (EventPush         <$> parseJSON v) <|>
            kind "tag_push"      *> (EventPushTag      <$> parseJSON v) <|>
            kind "issue"         *> (EventIssue        <$> parseJSON v) <|>
            kind "merge_request" *> (EventMergeRequest <$> parseJSON v) <|>
            kind "note"          *> (EventNote         <$> parseJSON v)
    parseJSON v            = typeMismatch "Event" v

-- | Parse a JSON string (the body of the HTTP request) into event information.
-- If parsing fails, return 'Left' an error message.
parse :: B.ByteString -> Either String Event
parse = eitherDecode
