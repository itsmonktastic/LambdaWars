{-# LANGUAGE OverloadedStrings #-}
module UI where

import Core (BotState, Bullet, bulletPosition)
import Data.String (fromString)

import Text.Blaze.Svg (Svg)
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes

showBoard :: [BotState] -> [Bullet] -> IO ()
showBoard = undefined

renderSvg :: [BotState] -> [Bullet] -> Svg 
renderSvg = undefined

renderBot :: BotState -> Svg
renderBot = undefined

renderBullet :: Bullet -> Svg
renderBullet bullet =
    let (x,y) = bulletPosition bullet
    in
        circle
           ! class_ "bullet"
           ! cx (toValue x)
           ! cy (toValue y)
           ! r "5"

server :: IO ()
server = undefined 

javascript :: String
javascript = undefined
