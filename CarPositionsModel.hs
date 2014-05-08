{-# LANGUAGE OverloadedStrings #-}

module CarPositionsModel where

import GameInitModel(CarId(..))

import Data.Aeson ((.:), (.:?), decode, encode, (.=), object, FromJSON(..), ToJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad        (liftM)

import Data.List

-- CarLane

data CarLane = CarLane {
  startLaneIndex :: Int,
  endLaneIndex   :: Int
} deriving (Show, Read, Eq)

instance FromJSON CarLane where
  parseJSON (Object v) =
    CarLane <$>
    (v .: "startLaneIndex") <*>
    (v .: "endLaneIndex")

-- PiecePosition

data PiecePosition = PiecePosition {
  pieceIndex      :: Int,
  inPieceDistance :: Float,
  lane            :: CarLane,
  lap             :: Int
} deriving (Show, Read, Eq)

instance FromJSON PiecePosition where
  parseJSON (Object v) =
    PiecePosition <$>
    (v .: "pieceIndex")      <*>
    (v .: "inPieceDistance") <*>
    (v .: "lane")            <*>
    (v .: "lap")

-- CarPosition

data CarPosition = CarPosition {
  carId         :: CarId,
  angle         :: Float,
  piecePosition :: PiecePosition
} deriving (Show, Read)

instance FromJSON CarPosition where
  parseJSON (Object v) =
    CarPosition <$>
    (v .: "id") <*>
    (v .: "angle") <*>
    (v .: "piecePosition")

data CrashData = CrashData String

instance FromJSON CrashData where
    parseJSON (Object v) = CrashData <$> (v .: "name")

data SpawnData = SpawnData String

instance FromJSON SpawnData where
    parseJSON (Object v) = SpawnData <$> (v .: "name")

data TurboData = TurboData Float

instance FromJSON TurboData where
    parseJSON (Object v) = TurboData <$> (v .: "turboFactor")

data DNFData = DNFData CarId

instance FromJSON DNFData where
    parseJSON (Object v) = DNFData <$> (v .: "car")

-- Helpers

findCar :: String -> [CarPosition] -> Maybe CarPosition
findCar carName positions =
  find nameMatches positions
  where nameMatches carPosition = carIdName (carId carPosition) == carName
