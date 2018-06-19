{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Console.ANSI
import System.Console.ANSI.Types

data Song = Song { title :: T.Text
                 , artist :: T.Text
                 , album :: T.Text
                 , startTime :: NominalDiffTime
                 , end :: NominalDiffTime
                 }

instance Eq Song where
    (==) (Song _ _ _ a _) (Song _ _ _ b _) = a == b

instance Ord Song where
    compare (Song _ _ _ a _) (Song _ _ _ b _) = compare a b

parseSong :: Value -> Song
parseSong obj = let
    unpackWithDefault v = T.toTitle $ Data.Maybe.fromMaybe "..." v
    extract id = unpackWithDefault $ obj ^? key id . _String
    extractTime id = fromIntegral (Data.Maybe.fromMaybe 0 $ obj ^? key id . _Integer)
    in Song (extract "title") (extract "authors") (extract "titreAlbum") (extractTime "start") (extractTime "end")

formatSong :: NominalDiffTime -> Song -> [String]
formatSong now (Song title artist album start end) = if (start <= now && now < end)
    then bold $ prepend " --> " "     " lines
    else prepend "   - " "     " lines
    where
        prepend a b (first:rest) = (a ++ first) : (fmap ((++) b) rest)
        bold t = fmap (\l -> setSGRCode [SetConsoleIntensity BoldIntensity] ++ l ++ setSGRCode [Reset]) t
        lines = [ fArtist ++ " | " ++ fTitle
                , "  album: " ++ (T.unpack album)
                ]
        fArtist = T.unpack artist
        fTitle = T.unpack title

main :: IO ()
main = do
    r <- get "https://www.fip.fr/livemeta/7"
    let songObjects = r ^.. responseBody . key "steps" . members
    let songs = fmap parseSong songObjects
    let orderedSongs = sort songs
    now <- getPOSIXTime
    let lines = orderedSongs >>= formatSong now
    putStrLn $ concat $ intersperse "\n" lines
