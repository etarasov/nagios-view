{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes
import Text.Blaze.Svg.Renderer.String

-- Redefine lineargradient, it has 'lineargradient :: Svg' type in the library
lineargradient' :: Svg -> Svg
lineargradient' = Parent "lineargradient" "<linearGradient" "</linearGradient>"

picture :: Svg
picture =
    svg ! customAttribute "xmlns" "http://www.w3.org/2000/svg"
        ! version "1.1"
        ! height "600"
        ! width "1000"
        $ do
        defs $ do
            lineargradient' ! id_ "background"
                            ! x2 "0"
                            ! x1 "0"
                            ! y2 "1"
                            ! y1 "0"
                            $ do
                stop ! offset "5%"
                     ! stopColor "#eeeeee"
                stop ! offset "95%"
                     ! stopColor "#eeeeb0"
        rect ! fill "url(#background)"
             ! height "600"
             ! width "1000"
             ! y "0"
             ! x "0"
        g ! transform (translate 10 20) $
            rect ! ry "2"
                 ! rx "2"
                 ! fill "#052C6E"
                 ! height "15"
                 ! width "60"
                 ! y "0"
                 ! x "50"
        g ! transform (translate 10 40) $
            rect ! ry "2"
                 ! rx "2"
                 ! fill "#6A92D4"
                 ! height "15"
                 ! width "40"
                 ! y "0"
                 ! x "150"

main :: IO ()
main = do
    print "hello"
