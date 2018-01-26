{-# LANGUAGE DeriveGeneric #-}

module BugHHunt.Objects.HighScore where

import Data.Maybe
import Data.Aeson
import Data.List

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS

-- highscore datatype
data HighScore = HighScore {
  highScoreName :: String,
  highScoreScore :: Int
} deriving (Ord, Generic)

instance Eq HighScore where
  (==) a b = highScoreScore a == highScoreScore b
  (/=) a b = not ((==) a b)

instance Show HighScore where
  show h = highScoreName h ++ " : " ++ show (highScoreScore h)

instance ToJSON HighScore
instance FromJSON HighScore

-- read string json to list of highscores
readHighScores :: String -> [HighScore]
readHighScores s = fromMaybe [] decoded
  where decoded = decode (BS.pack s) :: Maybe [HighScore]

-- save list of highscores to file
saveHighScores :: [HighScore] -> IO ()
saveHighScores s = writeFile "res/highscores.json" (BS.unpack (encode s))

-- check if current score is a new highscore
isNewHighScore :: Int -> [HighScore] -> Bool
isNewHighScore n hs = null hs || any (\s -> n > highScoreScore s) hs

-- add score to highscores
addHighScore :: HighScore -> [HighScore] -> [HighScore]
addHighScore s h = if length new > 10 then init new else new
  where new = insert s h