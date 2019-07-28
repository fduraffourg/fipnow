{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.Wreq
import System.Console.ANSI
import System.Console.ANSI.Types

fipMetadataUrl =
  "https://www.fip.fr/latest/api/graphql?operationName=Now&variables=%7B%22bannerPreset%22%3A%22600x600-noTransform%22%2C%22stationId%22%3A7%2C%22previousTrackLimit%22%3A3%7D&extensions=%7B%22persistedQuery%22%3A%7B%22version%22%3A1%2C%22sha256Hash%22%3A%228a931c7d177ff69709a79f4c213bd2403f0c11836c560bc22da55628d8100df8%22%7D%7D"

data Song =
  Song
    { title :: T.Text
    , artist :: T.Text
    , isCurrentSong :: Bool
    , startTime :: Int
    }

instance Eq Song where
  (==) (Song _ _ _ a) (Song _ _ _ b) = a == b

instance Ord Song where
  compare (Song _ _ _ a) (Song _ _ _ b) = compare a b

parseSong :: Bool -> Value -> Song
parseSong isCurrentSong obj =
  let unpackWithDefault v = T.toTitle $ Data.Maybe.fromMaybe "..." v
      extract id = unpackWithDefault $ obj ^? key id . _String
      extractTime id =
        fromIntegral (Data.Maybe.fromMaybe 0 $ obj ^? key id . _Integer)
   in Song
        (extract "subtitle")
        (extract "title")
        isCurrentSong
        (extractTime "start_time")

formatSong :: Song -> String
formatSong (Song title artist isCurrentSong _) =
  if isCurrentSong
    then bold $ " --> " ++ line
    else "   - " ++ line
  where
    prepend a b (first:rest) = (a ++ first) : (fmap ((++) b) rest)
    bold l =
      setSGRCode [SetConsoleIntensity BoldIntensity] ++ l ++ setSGRCode [Reset]
    line = T.unpack artist ++ " | " ++ T.unpack title

main :: IO ()
main = do
  r <- get fipMetadataUrl
  let previousSongsData =
        r ^.. responseBody . key "data" . key "previousTracks" . key "edges" .
        values .
        key "node"
  let currentSongData =
        r ^? responseBody . key "data" . key "now" . key "playing_item"
  let nextSongsData =
        r ^.. responseBody . key "data" . key "nextTracks" . values
  let songs =
        fmap (parseSong False) previousSongsData ++
        (Data.Maybe.maybeToList $ fmap (parseSong True) currentSongData) ++
        fmap (parseSong False) nextSongsData
  let formated = fmap formatSong . sort $ songs
  putStrLn $ concat $ intersperse "\n" formated
