{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Aeson
import Data.Text (unpack)
import Data.Maybe
import Data.List
import qualified Data.HashMap.Strict as HM

data Song = Song { title :: String
                 , artist :: String
                 , album :: String
                 }

parseSong :: Value -> Song
parseSong obj = let
    unpackWithDefault v = Data.Maybe.fromMaybe "..." $ fmap unpack v
    extract id = unpackWithDefault $ obj ^? key id . _String
    in Song (extract "title") (extract "authors") (extract "titreAlbum")

formatSong :: Song -> String
formatSong (Song title artist album) = title ++ " -- " ++ artist ++ " -- " ++ album

main :: IO ()
main = do
    r <- get "https://www.fip.fr/livemeta/7"
    let songs = case r ^? responseBody . key "steps" of
                    Just (Object hm) -> HM.elems hm
                    _ -> []
    let lines = fmap (formatSong . parseSong) songs
    putStrLn $ concat $ intersperse "\n" lines
