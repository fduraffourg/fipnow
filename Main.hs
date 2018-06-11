{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Aeson
import Data.Text (unpack)
import Data.Maybe

data Song = Song { title :: String
                 , artist :: String
                 , album :: String
                 }

parseSong obj = let
    unpackWithDefault v = Data.Maybe.fromMaybe "..." $ fmap unpack v
    extract id = unpackWithDefault $ obj ^? key id . _String
    in Song (extract "titre") (extract "interpreteMorceau") (extract "titreAlbum")

formatSong :: Song -> String
formatSong (Song title artist album) = title ++ " -- " ++ artist ++ " -- " ++ album

displaySong :: String -> Maybe Song -> IO ()
displaySong prefix (Just song) = putStrLn $ (prefix ++ " -> " ++ formatSong song)
displaySong prefix Nothing = putStrLn $ (prefix ++ " -> Unable to parse song")

main :: IO ()
main = do
    r <- get "http://www.fipradio.fr/sites/default/files/import_si/si_titre_antenne/FIP_player_current.json"
    displaySong "current  " $ fmap parseSong $ r ^? responseBody . key "current" . key "song"
    displaySong "next     " $ fmap parseSong $ r ^? responseBody . key "next1" . key "song"
    displaySong "previous1" $ fmap parseSong $ r ^? responseBody . key "previous1" . key "song"
    displaySong "previous2" $ fmap parseSong $ r ^? responseBody . key "previous2" . key "song"

