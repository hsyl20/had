{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.Had.Misc where

import Lucid
import Data.Text
import Control.Monad

welcome :: Html ()
welcome = do
  div_ [class_ "welcome"]
    "Unofficial GHC dashboard"

widget :: Text -> Html () -> Html ()
widget title body =
  div_ [class_ "widget"] do
    div_ [class_ "title"] (toHtml title)
    div_ [class_ "body"] body



-- dashboard layout taken from:
-- https://mxb.dev/blog/css-grid-admin-dashboard/

layout :: [MenuEntry] -> [Card] -> Html ()
layout menu cards = do
  div_ [class_ "admin"] do
    header_ [class_ "admin__header"] do

      div_ [class_ "logo"] do
        a_ [href_ "/"] do
          h1_ do
            "GHC devs"
            br_ []
            "dashboard"

      div_ [class_ "toolbar"] do
        ""
        -- button_ [class_ "btn btn--primary"] "Why?"
        -- div_ [class_ "logout"] "Log out" 

    nav_ [class_ "admin__nav"] do
      ul_ [class_ "menu"] do
        forM_ menu \e ->
          menuItem (menuUrl e) (menuBody e)

    main_ [class_ "admin__main"] do
      div_ [class_ "dashboard"] do
        forM_ cards \e -> do
          let c = case cardSize e of
                    Default -> card
                    Full    -> full
                    Col     -> colCard
                    Half    -> half
          c (cardBody e)
          
    footer_ [class_ "admin__footer"] do
      text "GHC devs dashboard - Contact: sylvain@haskus.fr"

data MenuEntry = MenuEntry
  { menuUrl  :: Text
  , menuBody :: Html ()
  }

data CardSize
  = Default
  | Half
  | Full
  | Col

data Card = Card
  { cardSize :: CardSize
  , cardBody :: Html ()
  }

menuItem :: Text -> Html () -> Html ()
menuItem url title =
  li_ [class_ "menu_item"] do
    a_ [class_ "menu__link", href_ url] title

card :: Html () -> Html ()
card body =
  div_ [class_ "dashboard__item"] do
    div_ [class_ "card"] body

full :: Html () -> Html ()
full body =
  div_ [class_ "dashboard__item dashboard__item--full"] do
    div_ [class_ "card"] body

half :: Html () -> Html ()
half body =
  div_ [class_ "dashboard__item dashboard__item--half"] do
    div_ [class_ "card"] body

colCard :: Html () -> Html ()
colCard body =
  div_ [class_ "dashboard__item dashboard__item--col"] do
    div_ [class_ "card"] body

text :: Text -> Html ()
text = toHtml
