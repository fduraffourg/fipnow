{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key, _Integer)
import Data.Aeson
import Data.Text (unpack)
import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX

data Song = Song { title :: String
                 , artist :: String
                 , album :: String
                 , startTime :: NominalDiffTime
                 , end :: NominalDiffTime
                 }

instance Eq Song where
    (==) (Song _ _ _ a _) (Song _ _ _ b _) = a == b

instance Ord Song where
    compare (Song _ _ _ a _) (Song _ _ _ b _) = compare a b

parseSong :: Value -> Song
parseSong obj = let
    unpackWithDefault v = Data.Maybe.fromMaybe "..." $ fmap unpack v
    extract id = unpackWithDefault $ obj ^? key id . _String
    extractTime id = fromIntegral (Data.Maybe.fromMaybe 0 $ obj ^? key id . _Integer)
    in Song (extract "title") (extract "authors") (extract "titreAlbum") (extractTime "start") (extractTime "end")

formatSong :: NominalDiffTime -> Song -> String
formatSong now (Song title artist album start end) = if (start <= now && now < end)
    then " --> " ++ line ++ " <--"
    else "     " ++ line
    where line = title ++ " -- " ++ artist ++ " -- " ++ album

main :: IO ()
main = do
    r <- get "https://www.fip.fr/livemeta/7"
    let songObjects = r ^.. responseBody . key "steps" . members
    let songs = fmap parseSong songObjects
    let orderedSongs = sort songs
    now <- getPOSIXTime
    let lines = fmap (formatSong now) orderedSongs
    putStrLn $ concat $ intersperse "\n" lines
