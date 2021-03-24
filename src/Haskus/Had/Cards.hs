{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Haskus.Had.Cards where

import Control.Concurrent.Async
import Control.Monad

import Lucid
import qualified GitLab
import qualified Data.Text as Text
import qualified Data.List as List

import Haskus.Had.State
import Haskus.Had.GitLab
import Haskus.Had.Misc

-- | Show the number of forks for the given project
cardForks :: GitLab.Project -> Card
cardForks p = Card Default $ do
  "Forks: "
  toHtml (show (GitLab.forks_count p))

-- | Show the milestones with number of issues and MRs
cardMilestones :: State -> GitLab.Project -> IO Card
cardMilestones s p = do
  milestones <- (List.reverse . List.sortOn GitLab.milestone_title)
                <$> getMilestones s p
  issue_counts <- forConcurrently milestones \m -> getMilestoneOpenIssueCount s p m
  return $ Card Default do
    h2_ "Milestones"
    ul_ $ forM_ (milestones `zip` issue_counts) \(m,ic) -> do
      li_ $ do
        a_ [ href_ ("https://gitlab.haskell.org/ghc/ghc/-/milestones/"
                    <> Text.pack (show (GitLab.milestone_iid m)))
           , target_ "_blank"
           ] $
          toHtml (GitLab.milestone_title m)
        " - "
        a_ [ target_ "_blank"
           , href_ ("https://gitlab.haskell.org/ghc/ghc/-/issues?scope=all&state=opened&milestone_title=" <> GitLab.milestone_title m)
           ] do
          toHtml (show ic)
          " issues"

-- | Show the number of issues without labels
cardIssuesWithoutLabels :: State -> GitLab.Project -> IO Card
cardIssuesWithoutLabels s p = do
  Just n <- getNoLabelIssueCount s p
  return $ Card Default do
    "Issues without any label: "
    a_ [ target_ "_blank"
       , href_ "https://gitlab.haskell.org/ghc/ghc/-/issues?scope=all&state=opened&label_name[]=None"] do
      toHtml (show (GitLab.statistics_counts_opened (GitLab.statistics_counts (GitLab.statistics n))))


-- | Show labels with the number of issues/MRs
--
-- Filter labels without issues/MRs
cardLabels :: State -> GitLab.Project -> IO Card
cardLabels s p = do
  labels <- getLabels s p
  return $ Card Full do
    h2_ "Labels"
    div_ [style_ "column-width: 15em"] do
      let is_used_label l = GitLab.label_open_issues_count l > 0
                            || GitLab.label_open_merge_requests_count l > 0
          used_labels = filter is_used_label labels
      ul_ $ forM_ used_labels \l -> do
        li_ $ do
          toHtml (GitLab.label_name l)
          " "
          let issue_count = GitLab.label_open_issues_count l
          let mr_count = GitLab.label_open_merge_requests_count l
          when (issue_count > 0) do
            a_ [ href_ ("https://gitlab.haskell.org/ghc/ghc/-/issues?label_name%5B%5D="
                      <> GitLab.label_name l)
               , title_ $ Text.pack (show issue_count) <>
                          (if issue_count > 1 then " issues" else " issue")
               ] do
                toHtml (show issue_count)
                "#"
            " "
          when (mr_count > 0) do
            a_ [ href_ ("https://gitlab.haskell.org/ghc/ghc/-/merge_requests?label_name%5B%5D="
                      <> GitLab.label_name l)
               , title_ $ Text.pack (show mr_count) <>
                          (if mr_count > 1 then " MRs" else " MR")
               ] do
                toHtml (show mr_count)
                "!"

