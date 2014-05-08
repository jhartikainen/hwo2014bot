{-# LANGUAGE TemplateHaskell, MultiWayIf #-}
module Brain where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Ord
import Data.Maybe
import Data.List
import Data.Monoid
import qualified Data.Map.Strict as Map
import Debug.Trace

import GameInitModel
import CarPositionsModel

import qualified CarPositionsModel as CPM
import qualified GameInitModel as GIM

type Tick = Int
type LaneNumber = Int
type Speed = Float
type Angle = Float
type TrackSegment = [(Int, Piece)]

type CarDatas = Map.Map String CarData

data CarData = CarData { cdSpeed :: Speed
                       , cdPrevSpeed :: Speed
                       , cdAngleSpeed :: Speed
                       , cdCrashed :: Bool
                       , cdAngle :: Float
                       , cdPosition :: PiecePosition
                       , cdCrashTick :: Tick
                       , cdDNF :: Bool
                       }
                       deriving (Show, Read)

data PieceData = PieceData { pdPosition :: CarPosition
                           , pdSpeed :: Speed
                           }
                           deriving (Show, Read)

data PieceKnowledge = PieceKnowledge
                        { speed :: Speed
                        , maxSpeed :: Speed
                        , crashed :: Bool
                        }
                        deriving (Show, Read)

data Brain = Brain { _bTrack :: Track
                   , _bSwitchPending :: Bool
                   , _bSwitchingLane :: Bool
                   , _bMyPosition :: PiecePosition
                   , _bMyCarPosition :: CarPosition
                   , _bCurrentThrottle :: Float
                   , _bCurrentSpeed :: Float
                   , _bPieceKnowledge :: [PieceKnowledge]
                   , _bCars :: CarDatas
                   , _bCarPieceData :: Map.Map String [PieceData]
                   , _bRegulateIntegral :: Float
                   , _bRegulateError :: Float
                   , _bTurboAvailable :: Bool
                   , _bTurboFactor :: Float
                   , _bTurboOn :: Bool
                   , _bTick :: Tick
                   , _bDrag :: Maybe Float
                   , _bMass :: Maybe Float
                   }

makeLenses ''Brain

drag :: Brain -> Float
drag = fromMaybe 0.02 . _bDrag

mass :: Brain -> Float
mass = fromMaybe 0.02 . _bMass

otherCars :: Brain -> CarDatas
otherCars b = Map.filterWithKey (\k _ -> k /= myName b) $ _bCars b

myName :: Brain -> String
myName = carIdName . CPM.carId . _bMyCarPosition

adjustThrottleForTurbo :: Brain -> Float -> Float
adjustThrottleForTurbo b t | _bTurboOn b == False = t
                           | otherwise            = t / (_bTurboFactor b)

segmentsBy :: Brain -> ((Int, Piece) -> Bool) -> [TrackSegment]
segmentsBy b f = filter (f . head) . groupBy (\a b -> f a == f b) $ zip [0..] (pieces $ _bTrack b)

defaultKnowledge :: Track -> (Int, Piece) -> PieceKnowledge
defaultKnowledge t (i, StraightPiece _ _) = PieceKnowledge 1 1 False
defaultKnowledge t (i, p)                 = PieceKnowledge (calcSegSpeed i t) 1 False

pieceCrashed :: Speed -> PieceKnowledge -> PieceKnowledge
pieceCrashed s p = p { maxSpeed = min s (maxSpeed p - 0.01), speed = clampThrottle newSpeed, crashed = True }
    where
        newSpeed = speed p - (speed p / 4)

knownSpeed :: Int -> Brain -> Speed
knownSpeed pieceIdx b = speed ((_bPieceKnowledge b) !! pieceIdx)

nextPiece :: (Monad a) => BotM a Piece
nextPiece = get >>= (\b -> return ((cycle $ pieces $ _bTrack b) !! ((pieceIndex $ _bMyPosition b) + 1)))

clampThrottle :: Float -> Speed
clampThrottle = clamp 0 1

clampSpeed :: Float -> Speed
clampSpeed = max 0

isOnTrack :: CarData -> Bool
isOnTrack cd = not (cdDNF cd) && not (cdCrashed cd)

initCarData :: [Car] -> CarDatas
initCarData = Map.fromList . map toData
    where
        toData (Car { GameInitModel.carId = c }) = (carIdName c, CarData 0 0 0 False 0 (PiecePosition 0 0 (CarLane 0 0) 0) 0 False)

initCarPieceData :: [Car] -> Map.Map String [PieceData]
initCarPieceData cs = Map.fromList $ map conv cs
    where
        conv (Car { GIM.carId = cid }) = (carIdName cid, [])

leaderboard :: CarDatas -> [String]
leaderboard cds = map fst $ sortBy rank $ Map.toList cds
    where
        rank (_, a) (_, b) = comparing pi a b `mappend` comparing pd a b
        pi = pieceIndex . cdPosition
        pd = inPieceDistance . cdPosition

updateCarData :: Track -> CarPosition -> CarData -> CarData
updateCarData t cp cd = cd { cdAngle = CPM.angle cp, cdPosition = pp, cdSpeed = newSpeed, cdPrevSpeed = cdSpeed cd, cdAngleSpeed = newAngleSpeed }
    where
        pp = piecePosition cp
        newSpeed = calculateSpeed (pieces t !! (pieceIndex $ cdPosition cd)) (cdPosition cd) pp (lanes t !! (endLaneIndex $ lane pp))
        newAngleSpeed = cdAngle cd - CPM.angle cp

calculateSpeed :: Piece -> PiecePosition -> PiecePosition -> Lane -> Speed
calculateSpeed oldPiece oldPos newPos newLane = if positionDiff >= 0
                                                  then positionDiff / 10
                                                  else (positionDiff + pieceLen (distanceFromCenter newLane) oldPiece) / 10
    where
        positionDiff = inPieceDistance newPos - inPieceDistance oldPos

pdPiecePosition :: PieceData -> PiecePosition
pdPiecePosition = piecePosition . pdPosition

pdPieceIndex :: PieceData -> Int
pdPieceIndex = pieceIndex . pdPiecePosition

newPieceData :: PiecePosition -> CarPosition -> Piece -> Lane -> PieceData
newPieceData old newCp p l = PieceData { pdPosition = newCp
                                       , pdSpeed = calculateSpeed p old (piecePosition newCp) l
                                       }

updateCarPieceData :: [PieceData] -> PieceData -> [PieceData]
updateCarPieceData pds pd | length pds == 0 = [pd]
                          | otherwise       = if pdPieceIndex (last pds) /= pdPieceIndex pd
                                                then [pd]
                                                else pds ++ [pd]

data PlayMode = Create | Join | DefaultJoin deriving (Show, Read, Eq)
data BotConfig = BotConfig { bcName :: String, bcTrackName :: String, bcPassword :: Maybe String, bcMode :: PlayMode, bcNumCars :: Int }

type BotM a = StateT Brain (ReaderT BotConfig a)

runBotM :: BotM IO a -> BotConfig -> Brain -> IO (a, Brain)
runBotM f c b = runReaderT (runStateT f b) c

data ActionChoice = SwitchChoice Side
                  | BrakeChoice
                  | TurboChoice
                  | AccelerateChoice Speed
                  | PingChoice
                  | NoChoice
                  deriving (Show, Read, Eq, Ord)

noChoice :: (Monad m) => BotM m ActionChoice
noChoice = return NoChoice

choose :: (Monad m) => ActionChoice -> BotM m ActionChoice
choose = return

segmentLength :: Int -> TrackSegment -> Float
segmentLength offset segment = sum $ map (pieceLen offset . snd) segment

data BendDirection = BendLeft | BendRight deriving (Show, Read, Eq)

data Side = Left | Right deriving (Show, Read, Eq, Ord)

laneSwitchSide :: Lane -> Lane -> Maybe Side
laneSwitchSide from to = if | fromDist == toDist -> Nothing
                            | fromDist > toDist  -> Just Brain.Left
                            | otherwise          -> Just Brain.Right
    where
        fromDist = distanceFromCenter from
        toDist = distanceFromCenter to

laneAfterSwitch :: Track -> Side -> LaneNumber -> LaneNumber
laneAfterSwitch t s current = clamp 0 maxLane (change current)
    where
        maxLane = (length $ lanes t) - 1
        change = case s of
                   Brain.Left  -> (-) 1
                   Brain.Right -> (+) 1

slowCarOnLane :: Brain -> PiecePosition -> Float -> Lane -> Bool
slowCarOnLane b pp distance l = any (\cd -> distanceToCar pp (cdPosition cd) l ps < distance) slower
    where
        slower = filter (\cd -> isOnTrack cd && isSlower cd) $ map snd $ Map.toList $ otherCars b
        isSlower cd = let s = cdSpeed cd + 0.3 in s < _bCurrentSpeed b && s < knownSpeed (pieceIndex $ cdPosition cd) b
        ps = pieces $ _bTrack b


getSegment :: Track -> Int -> Int -> TrackSegment
getSegment t start end = zip [start..end] $ take (end - start) $ drop start ps
    where
        ps = pieces t

switchToAvoidSlow :: Brain -> Maybe Side
switchToAvoidSlow b = if not $ slowCarOnLane b pp 50 myLane
                        then Nothing
                        else mSwitch >>= (\s -> bestSwitchDir b s `mplus` (availableLane >>= laneSwitchSide myLane))
    where
        pp = _bMyPosition b
        myLane = (lanes $ _bTrack b) !! (endLaneIndex $ lane pp)
        mSwitch = nextSwitchIndex $ drop (pieceIndex pp) (pieces $ _bTrack b)
        availableLane = find (not . slowCarOnLane b pp 50) $ possibleLanes b

possibleLanes :: Brain -> [Lane]
possibleLanes b = map getLane $ filter (\n -> n >= 0 && n < laneCount) [currentLane + 1, currentLane - 1]
    where
        laneCount = length $ lanes $ _bTrack b
        currentLane = endLaneIndex $ lane $ _bMyPosition b
        getLane i = (lanes $ _bTrack b) !! i

bestSwitchDir :: Brain -> Int -> Maybe Side
bestSwitchDir b switchIdx = switchDir >>= (\s -> if shouldSwitch s then Just s else Nothing)
    where
        circularTrack = circularize (pieces $ _bTrack b)
        (nextSwitchIdx, _) = fromJust $ find (isSwitch . snd) $ drop (switchIdx + 1) circularTrack
        bends = map snd $ filter (\(i, (_, p)) -> (i < nextSwitchIdx) && (isBend p)) $ drop (switchIdx + 1) $ zip [0..] circularTrack
        (lefts, rights) = partition (\(_, p) -> (bendDirection p) == BendLeft) bends
        switchDir = if (length lefts) == (length rights)
                      then Nothing
                      else Just (if (length lefts) > (length rights) then Brain.Left else Brain.Right)
        myLane = endLaneIndex $ lane $ _bMyPosition b
        shouldSwitch Brain.Left = (myLane - 1) >= 0
        shouldSwitch Brain.Right = (myLane + 1) < (length $ lanes $ _bTrack b)

needsSwitch :: Brain -> Maybe Side
needsSwitch b = if (_bSwitchPending b) || (_bSwitchingLane b) || (not $ fromMaybe False $ liftM2 (\a b -> (fst a) <= (fst b)) mSwitch mBend)
                  then trace "No switch" Nothing
                  else
                      let (switchIdx, _) = fromJust mSwitch
                      in bestSwitchDir b switchIdx
    where
        circularTrack = concat $ replicate 2 $ zip [0..] (pieces $ _bTrack b)
        mSwitch = find (isSwitch . snd) $ drop (pieceIndex $ _bMyPosition b) $ circularTrack
        mBend = mSwitch >>= (\(i, _) -> find (isBend . snd) $ drop (i + 1) $ circularTrack)

nextSwitchIndex :: [Piece] -> Maybe Int
nextSwitchIndex = findIndex isSwitch

nextBendIndex :: [Piece] -> Int -> Maybe Int
nextBendIndex ps start = fmap fst $ find (isBend . snd) $ drop start $ circularize ps

circularize :: [a] -> [(Int, a)]
circularize = concat . replicate 2 . zip [0..]

bendDirection :: Piece -> BendDirection
bendDirection (StraightPiece _ _) = BendLeft
bendDirection p = if bendAngle p < 0 then BendLeft else BendRight

isSwitch :: Piece -> Bool
isSwitch = switch

isBend :: Piece -> Bool
isBend (BendPiece {}) = True
isBend _ = False

isStraight :: Piece -> Bool
isStraight p@(StraightPiece {}) = not $ switch p
isStraight _ = False

clamp :: (Num n, Ord n) => n -> n -> n -> n
clamp a b = min b . max a

distanceTo' :: PiecePosition -> Lane -> Int -> [Piece] -> Float
distanceTo' pos lane targetIndex pieces = firstLen + (sum $ map (pieceLen offset . snd) $ takeWhile (\(i, _) -> i /= targetIndex) $ drop (1 + (pieceIndex pos)) cps)
    where
        cps = circularize pieces
        offset = distanceFromCenter lane
        firstLen = (pieceLen offset $ head pieces) - (inPieceDistance pos)

distanceToCar :: PiecePosition -> PiecePosition -> Lane -> [Piece] -> Float
distanceToCar src dst lane ps = (distanceTo' src lane (pieceIndex dst) ps) + (inPieceDistance dst)

-- | Return number of ticks at speed it would take to travel distance
ticksForDistance :: Speed -> Float -> Tick
ticksForDistance speed dist = ceiling (dist / (speed * 10))

pieceRadius :: Float -> Piece -> Float
pieceRadius def (StraightPiece {}) = def
pieceRadius _ p@(BendPiece {}) = bendRadius p

pieceAngle :: Float -> Piece -> Float
pieceAngle def (StraightPiece {}) = def
pieceAngle _ p@(BendPiece {}) = bendAngle p

calcSegSpeed :: Int -> Track -> Speed
calcSegSpeed startPoint t = k / (turnAmount / angleAmount)
    where
        seg = drop startPoint . circularize $ pieces t
        k = 0.063
        maxLen = 160 :: Float
        (turnAmount, angleAmount) = foldl sumAll (0, 0) $ takeWhile (\(d,_,_,_) -> d < maxLen) $ iterate calc (0, 0, 0, map snd seg)
        calc (d, ta, aa, p:ps) = let pLen = pieceLen 0 p
                                     takeL = maxLen - d
                                     takeL' = if takeL > pLen then pLen else takeL
                                     anglePerc = (abs $ pieceAngle 0 p) * takeL' / pLen
                                 in if not $ isBend p
                                      then (d + pLen, ta, aa, ps)
                                      else (d + pLen, ta + anglePerc / sqrt (pieceRadius 0 p), aa + anglePerc, ps)

        calc (_, ta, aa, []) = (maxLen, ta, aa, [])
        sumAll (t, a) (_, t', a', _) = (t + t', a + a')


pieceLen :: Int -> Piece -> Float
pieceLen _ p@(StraightPiece {}) = straightLength p
pieceLen offset p = abs ((fixedOffset + bendRadius p) * (deg2rad $ bendAngle p))
    where
        fixedOffset = fromIntegral $ if (bendDirection p) == BendRight then offset * (-1) else offset

deg2rad :: Float -> Float
deg2rad d = d / 180 * pi

calculateBrakeDistance :: Brain -> Speed -> Speed -> Float
calculateBrakeDistance b tar cur = (mass b / drag b) * current * (1 - (exp ((0 - drag b) * ticks / mass b)))
    where
        ticks = (log (target / current)) * (mass b) / (0 - drag b)
        current = cur * 10
        target = tar * 10

crashDuration :: Tick
crashDuration = 400
