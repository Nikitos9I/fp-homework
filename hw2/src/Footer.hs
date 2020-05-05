module Footer where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

type FooterElem = String

type FooterElemTitle = String

help :: FooterElem
help = "Help"

author :: FooterElem
author = "  It was created by Nikitos9I  "

search :: FooterElem
search = "Search"

back :: FooterElem
back = "Go back"

helpT :: FooterElemTitle
helpT = " Ctrl + H "

authorT :: FooterElemTitle
authorT = " Author "

searchT :: FooterElemTitle
searchT = " Ctrl + F "

backT :: FooterElemTitle
backT = " Ctrl + Z "

renderFooterWidget :: (FooterElem, FooterElemTitle) -> Widget String
renderFooterWidget (fe, fet) = modifs . vLimit 2 $ hLimit (wdt + 2) $ vBox [top, middle]
  where
    modifs = withBorderStyle unicodeRounded
    wdt = max (length fe) (length fet)
    top =
      hBox
        [ borderElem bsCornerTL
        , hBorderWithLabel $ str fet
        , borderElem bsCornerTR
        ]
    middle = hBox [vBorder, hCenter $ str fe, vBorder]

draw :: Widget String
draw =
  hBox
    [ renderFooterWidget (help, helpT)
    , renderFooterWidget (search, searchT)
    , renderFooterWidget (back, backT)
    , hCenter $ fill ' '
    , renderFooterWidget (author, authorT)
    ]