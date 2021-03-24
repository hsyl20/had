{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.Had.GitLab where

import qualified Data.Text as Text

import GitLab
import GitLab.WebRequests.GitLabWebCalls

import Haskus.Had.State

gitlabRequest :: State -> GitLab m -> IO m
gitlabRequest s act = runGitLabWithManager (stateHttpMgr s) (stateGitlab s) act

getGhcProject :: State -> IO Project
getGhcProject s = do
  r <- gitlabRequest s (GitLab.searchProjectId 1)
  case r of
    Left err       -> error $ "Gitlab connection issue: " <> show err
    Right Nothing  -> error $ "Couldn't find GHC project (id 1)"
    Right (Just v) -> return v

-- | Get active Milestones
getMilestones :: State -> Project -> IO [Milestone]
getMilestones s p = gitlabRequest s (gitlabWithAttrsUnsafe path opts)
  where
    path = "/projects/" <> Text.pack (show (GitLab.project_id p)) <> "/milestones"
    opts = "&state=active"

-- | Get number of issues associated with a milestone
getMilestoneIssueCount :: State -> Project -> Milestone -> IO (Maybe Stats)
getMilestoneIssueCount s p m = gitlabRequest s (gitlabWithAttrsOneUnsafe path opts)
  where
    path = "/projects/" <> Text.pack (show (GitLab.project_id p)) <> "/issues_statistics"
    opts = "&scope=all&milestone=" <> milestone_title m

-- | Get number of issues without any label
getNoLabelIssueCount :: State -> Project -> IO (Maybe Stats)
getNoLabelIssueCount s p = gitlabRequest s (gitlabWithAttrsOneUnsafe path opts)
  where
    path = "/projects/" <> Text.pack (show (GitLab.project_id p)) <> "/issues_statistics"
    opts = "&scope=all&labels=None"

-- | Get number of open issues associated with a milestone
getMilestoneOpenIssueCount :: State -> Project -> Milestone -> IO Word
getMilestoneOpenIssueCount s p m = do
  Just r <- getMilestoneIssueCount s p m
  return (statistics_counts_opened . statistics_counts . statistics $ r)

getLabels :: State -> Project -> IO [Label]
getLabels s p = gitlabRequest s (gitlabWithAttrsUnsafe path opts)
  where
    path = "/projects/" <> Text.pack (show (GitLab.project_id p)) <> "/labels"
    opts = "&with_counts=true"
