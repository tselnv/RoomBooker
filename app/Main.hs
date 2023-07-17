module Main (main) where

import Booker (runBot)

main :: IO ()
main = runBot "app-db-config.env"
