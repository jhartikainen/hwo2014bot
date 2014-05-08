{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)

import Network(connectTo, PortID(..))
import System.IO(hPutStrLn, hGetLine, hSetBuffering, BufferMode(..), Handle, hIsEOF)
import Data.Ord
import Data.List
import Data.Maybe
import Data.Map (adjust, fromList)
import qualified Data.Map as Map
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson(decode, FromJSON(..), fromJSON, parseJSON, eitherDecode, Value(..), (.:), (.:?), Result(..))
import Debug.Trace
import Criterion.Measurement

import GameInitModel
import CarPositionsModel
import qualified CarPositionsModel as CPM

import Brain

type ClientMessage = String

joinMessage botname botkey = "{\"msgType\":\"join\",\"data\":{\"name\":\"" ++ botname ++ "\",\"key\":\"" ++ botkey ++ "\"}}"
createMessage botname botkey track pass num = "{\"msgType\":\"createRace\",\"data\":{\"botId\":{\"name\":\"" ++ botname ++ "\",\"key\":\"" ++ botkey ++ "\"},\"trackName\":\"" ++ track ++ "\",\"password\":\"" ++ pass ++ "\",\"carCount\":" ++ num ++ "}}"
joinRaceMessage botname botkey track pass num = "{\"msgType\":\"joinRace\",\"data\":{\"botId\":{\"name\":\"" ++ botname ++ "\",\"key\":\"" ++ botkey ++ "\"},\"trackName\":\"" ++ track ++ "\",\"password\":" ++ pass ++ ",\"carCount\":" ++ num ++"}}"
throttleMessage amount = "{\"msgType\":\"throttle\",\"data\":" ++ (show amount) ++ "}"
switchMessage side = "{\"msgType\":\"switchLane\",\"data\":\"" ++ (show side) ++ "\"}"
pingMessage = "{\"msgType\":\"ping\",\"data\":{}}"
turboMessage = "{\"msgType\":\"turbo\",\"data\":\"IT'S TURBO TIME!\"}"

connectToServer server port = connectTo server (PortNumber (fromIntegral (read port :: Integer)))

main = do
    args <- getArgs
    case (splitAt 4 args) of
      ([server, port, botname, botkey], xs) -> do
        run server port botname botkey (arg 0 xs) (arg 1 xs) (arg 2 xs) (arg 3 xs)
      _ -> do
        putStrLn "Usage: hwo2014bot <host> <port> <botname> <botkey> [track] [mode] [password] [num bots]"
        exitFailure
    where
        arg n xs | n >= length xs = Nothing
                 | otherwise      = Just (xs !! n)


run :: String -> String -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO ()
run server port botname botkey mtrack mmode mpass mnumbots = do
  let c = BotConfig botname (fromMaybe "keimola" mtrack) mpass (maybe Brain.DefaultJoin (\s -> if s == "join" then Brain.Join else Create) mmode) (maybe 1 read mnumbots)

  h <- connectToServer server port
  hSetBuffering h LineBuffering
  case (bcMode c) of
    Create -> hPutStrLn h $ createMessage botname botkey (bcTrackName c) (fromMaybe "" $ bcPassword c) (show $ bcNumCars c)
    Brain.Join  -> hPutStrLn h $ joinRaceMessage botname botkey (bcTrackName c) (maybe "null" (\p -> if p == "" then "null" else "\"" ++ p ++ "\"") $ bcPassword c) (show $ bcNumCars c)
    DefaultJoin -> hPutStrLn h $ joinMessage botname botkey

  (t,_) <- runBotM (handleMessages h 0) c undefined
  putStrLn $ secs t

handleMessages :: Handle -> Double -> BotM IO Double
handleMessages h t = do
  eof <- liftIO $ hIsEOF h
  if eof
    then return t
    else do
      msg <- liftIO $ hGetLine h
      -- liftIO $ print msg
      case decode (L.pack msg) of
        Just json ->
          let decoded = fromJSON json >>= decodeMessage in
          case decoded of
            Success serverMessage -> liftM (+ t) $ handleServerMessage h serverMessage
            Error s -> fail $ "Error decoding message: " ++ s
        Nothing -> do
          fail $ "Error parsing JSON: " ++ (show msg)

data ServerMessage = Join
                   | GameInit GameInitData
                   | CarPositions [CarPosition] Int
                   | GameStart
                   | LapFinished
                   | Crash Tick CrashData
                   | Spawn Tick SpawnData
                   | GameEnd
                   | TurboAvailable TurboData
                   | TurboEnd CarId
                   | DNF DNFData
                   | Unknown String

handleServerMessage :: Handle -> ServerMessage -> BotM IO Double
handleServerMessage h serverMessage = do
  config <- ask
  b <- get
  (t, (r, b')) <- liftIO $ time $ runBotM (respond serverMessage >>= handleChoice) config b
  put b'
  liftIO $ putStrLn $ secs t
  case r of
    Nothing -> handleMessages h t
    Just s  -> (liftIO $ hPutStrLn h $ choiceToMsg s) >> (handleMessages h t)

choiceToMsg :: ActionChoice -> ClientMessage
choiceToMsg BrakeChoice = throttleMessage 0
choiceToMsg (AccelerateChoice s) = throttleMessage s
choiceToMsg (SwitchChoice s)     = switchMessage s
choiceToMsg TurboChoice          = turboMessage
choiceToMsg _                    = pingMessage

handleChoice :: ActionChoice -> BotM IO (Maybe ActionChoice)
handleChoice c = do
        b <- get
        let rs = leaderboard (b^.bCars)

        turboMsg <- if c /= NoChoice && b^.bTurboAvailable then handleTurbo else return Nothing

        if isJust turboMsg
          then return turboMsg
          else case c of
                   BrakeChoice ->
                       if carInFront b && b^.bTurboOn
                         then do
                             liftIO $ print "RAMMING SPEED"
                             return $ Just $ AccelerateChoice 1
                         else
                            return $ Just c

                   SwitchChoice side -> do
                       let newLane = laneAfterSwitch (b^.bTrack) side (endLaneIndex $ lane $ b^.bMyPosition)
                       liftIO $ print ("Testing for obstacle on lane " ++ (show newLane))
                       bSwitchPending .= True
                       if slowCarOnLane b (b^.bMyPosition) 200 ((lanes $ b^.bTrack) !! newLane)
                         then trace "Slow car, no switch" $ return $ Just NoChoice
                         else do
                             trace "Switch OK" $ return $ Just c

                   otherwise   -> return (if c /= NoChoice then Just c else Nothing)

handleTurbo :: BotM IO (Maybe ActionChoice)
handleTurbo = do
        b <- get

        if not $ carInFront b
          then return Nothing
          else do
              bTurboAvailable .= False
              bTurboOn .= True

              liftIO $ print "IT'S TURBO RAM TIME"
              return $ Just TurboChoice

carInFront :: Brain -> Bool
carInFront b = isJust $ find (\(_, cd) -> isOnTrack cd && _bMyPosition b /= cdPosition cd && inPieceInFront (_bMyPosition b) (cdPosition cd) b) $ Map.toList $ _bCars b
    where
        inPieceInFront me them b = (endLaneIndex $ lane me) == (endLaneIndex $ lane them) && (distanceToCar me them ((lanes $ _bTrack b) !! (endLaneIndex $ lane me)) (pieces $ _bTrack b)) < 100

respond :: ServerMessage -> BotM IO ActionChoice
respond message = case message of
  Main.Join -> do
      liftIO $ putStrLn "Joined"
      noChoice
  GameInit gameInit -> do
      let trackName = name $ track $ race gameInit
      liftIO $ writeFile (trackName ++ ".trackdata") (show $ track $ race gameInit)

      liftIO $ putStrLn $ "GameInit: " ++ (reportGameInit gameInit)
      put $ Brain { _bTrack = track $ race $ gameInit
                  , _bSwitchPending = False
                  , _bSwitchingLane = False
                  , _bMyPosition = PiecePosition 0 0 undefined 0
                  , _bMyCarPosition = undefined
                  , _bCurrentThrottle = 1.0
                  , _bCurrentSpeed = 0.0
                  , _bPieceKnowledge = map (defaultKnowledge $ track $ race gameInit) $ zip [0..] $ pieces $ track $ race gameInit
                  , _bCars = initCarData $ cars $ race gameInit
                  , _bCarPieceData = initCarPieceData $ cars $ race gameInit
                  , _bRegulateIntegral = 0
                  , _bRegulateError = 0
                  , _bTurboAvailable = False
                  , _bTurboFactor = 1
                  , _bTurboOn = False
                  , _bTick = 0
                  , _bDrag = Nothing
                  , _bMass = Nothing
                  }

      noChoice
  LapFinished -> do
      noChoice
  Crash tick (CrashData name) -> do
      myName <- liftM bcName ask
      b <- get

      let cd = fromJust $ Map.lookup name $ _bCars b

      bPieceKnowledge . (ix . pieceIndex . cdPosition $ cd) %= (pieceCrashed . cdSpeed $ cd)
      bCars           %= adjust (\c -> c { cdCrashed = True, cdCrashTick = tick }) name

      when (name == myName) $ do
          liftIO $ print "##########################################"
          liftIO $ print ("######### CRASH    Tick: " ++ (show tick) ++ " ########")
          liftIO $ print "##########################################"

      noChoice

  Spawn tick (SpawnData name) -> do
      bCars %= adjust (\c -> c { cdCrashed = False }) name
      noChoice

  DNF (DNFData (CarId { carIdName = n })) -> do
      bCars %= adjust (\c -> c { cdDNF = True }) n
      noChoice

  GameStart -> do
      liftIO $ print "Game start"
      bSwitchPending .= False
      choose PingChoice
  GameEnd -> do
      liftIO $ print "Game end: Saving piece data"
      b <- get
      liftIO $ writeFile ((name $ _bTrack b) ++ ".show") $ show $ _bPieceKnowledge b
      noChoice
  TurboAvailable (TurboData factor)-> do
      bTurboAvailable .= True
      bTurboFactor .= factor
      noChoice
  TurboEnd (CarId { carIdName = n }) -> do
      me <- liftM myName get
      when (n == me) $ do
          bTurboFactor .= 1
          bTurboOn .= False

      noChoice
  CarPositions carPositions tick -> do
      bTick .= tick

      myName <- liftM bcName ask

      let myPos = piecePosition $ fromJust $ findCar myName carPositions

      modify $ updateCars carPositions
      modify $ updateBrain myName carPositions

      brain' <- get
      let switchSide = switchToAvoidSlow brain' `mplus` needsSwitch brain'
      nextIsSwitch <- liftM isSwitch nextPiece

      if nextIsSwitch && isJust switchSide
        then switchLane (fromJust switchSide) tick
        else adjustThrottle myPos tick

  Unknown msg -> do
      liftIO $ putStrLn $ "Unknown message: " ++ msg
      noChoice

updateCars :: [CarPosition] -> Brain -> Brain
updateCars carPositions b =
    let newDatas = map (\cp -> let n   = carIdName $ CPM.carId cp
                                   ocp = cdPosition $ fromJust $ Map.lookup n $ _bCars b
                               in (n, newPieceData ocp cp ((pieces $ _bTrack b) !! (pieceIndex ocp)) ((lanes $ _bTrack b) !! (endLaneIndex $ lane $ piecePosition cp)))
                       ) carPositions
        newCps = Map.mapWithKey (\n pds -> fromMaybe pds (lookup n newDatas >>= (Just . updateCarPieceData pds))) $ _bCarPieceData b
        updatePieces = map fst $ filter (\(_,(_,n)) -> length n == 1) $ zip (Map.toList $ _bCarPieceData b) (Map.toList newCps)

    in b { _bCars = foldl (\cs cp -> adjust (updateCarData (_bTrack b) cp) (carIdName $ CPM.carId cp) cs) (_bCars b) carPositions
         , _bCarPieceData = newCps
         }

switchLane :: Side -> Int -> BotM IO ActionChoice
switchLane s tick = do
    liftIO $ print ("TICK " ++ (show tick) ++ " Sending switch.. " ++ (show s))
    choose $ SwitchChoice s

adjustThrottle :: PiecePosition -> Int -> BotM IO ActionChoice
adjustThrottle piecePos tick = do
    brain <- get
    let pieceIdx = pieceIndex piecePos
        speed = knownSpeed pieceIdx brain
        ps = pieces $ _bTrack brain
        thisPiece = ps !! pieceIdx
        thisIsBend = isBend thisPiece
        myLane = (lanes $ _bTrack brain) !! (endLaneIndex $ lane $ piecePos)
        pix = (until (\p -> not $ (ps !! pieceIdx) == (ps !! p)) (\pi -> let pi' = pi + 1 in if pi' == length ps then 0 else pi') pieceIdx)
        slowPointIdx = fromJust $ nextBendIndex ps pix
        sp = ps !! slowPointIdx
        slowSpeed = knownSpeed slowPointIdx brain
        distanceToSlow = distanceTo' piecePos myLane slowPointIdx ps
        slowBrakeDist = calculateBrakeDistance brain slowSpeed (_bCurrentSpeed brain)

    when ((_bCurrentSpeed brain) < 0) $ do
        liftIO $ print ((pieces $ _bTrack brain) !! ((pieceIndex $ _bMyPosition brain) - 1))
        liftIO $ print (_bMyPosition brain)

    if (distanceToSlow <= slowBrakeDist) || (_bCurrentSpeed brain) > speed
      then do
        liftIO $ print ("TICK " ++ (show tick) ++ ", " ++ (show pieceIdx) ++ " Braking " ++ (show $ _bCurrentSpeed brain) ++ "/" ++ (show speed) ++ " (" ++ (show $ CarPositionsModel.angle $ _bMyCarPosition brain) ++ ") :: " ++ (show distanceToSlow) ++ "/" ++ (show slowPointIdx) ++ "/" ++ (show slowBrakeDist) ++ "/" ++ (show slowSpeed))
        choose BrakeChoice
      else do
        -- spd <- if thisIsBend then regulateSpeed thisPiece else return (if _bCurrentSpeed brain < 1 then clampThrottle speed else adjustThrottleForTurbo brain speed)
        spd <- liftM (min 1) $ regulateSpeed thisPiece
        liftIO $ print ("TICK " ++ (show tick) ++ ", " ++ (show pieceIdx) ++ " Speed " ++ (show $ _bCurrentSpeed brain) ++ "/" ++ (show spd) ++ "/" ++ (show speed) ++ " (" ++ (show $ CarPositionsModel.angle $ _bMyCarPosition brain) ++ ") :: " ++ (show distanceToSlow) ++ "/" ++ (show slowPointIdx) ++ "/" ++ (show slowBrakeDist) ++ "/" ++ (show slowSpeed))

        bCurrentThrottle .= spd

        choose $ AccelerateChoice spd

regulateSpeed :: Piece -> BotM IO Speed
regulateSpeed p = do
        b <- get
        let maxAngle = (sqrt (pieceRadius 200 p)) * 3.8
            err = (maxAngle - (abs $ CPM.angle $ _bMyCarPosition b)) / 30
            perr = if (_bRegulateError b) == 0 then err else _bRegulateError b
            integral = min 0 ((_bRegulateIntegral b) + err)
            derivative = err - perr
            kp = 1
            ki = 0
            kdp = 10
            kdn = 6
            kd = if derivative > 0 then kdp else kdn
            output = 0.5 + kp * err + ki * integral + derivative * kd

        bRegulateError .= err
        bRegulateIntegral .= integral

        return $ clampThrottle $ adjustThrottleForTurbo b (output)

updateBrain :: String -> [CarPosition] -> Brain -> Brain
updateBrain name cps b = b
                      { _bSwitchPending = _bSwitchPending b && (not switchingLane) && (not passedSwitch)
                      , _bSwitchingLane = switchingLane
                      , _bMyPosition    = myPiece
                      , _bMyCarPosition = myCar
                      , _bCurrentSpeed  = newSpeed
                      , _bRegulateIntegral = if outOfBend then 0 else _bRegulateIntegral b
                      , _bRegulateError    = if outOfBend then 0 else _bRegulateError b
                      , _bMass = newMass
                      , _bDrag = newDrag
                      }
    where
        myCar = fromJust $ findCar name cps
        myPiece = piecePosition myCar
        myLane = lane myPiece
        switchingLane = startLaneIndex myLane /= endLaneIndex myLane
        ps = pieces $ _bTrack b
        fromPiece = ps !! (pieceIndex $ _bMyPosition b)
        toPiece = ps !! (pieceIndex myPiece)
        passedSwitch = (isSwitch fromPiece) && (not $ isSwitch toPiece)
        outOfBend = (not $ isBend toPiece) && (isBend fromPiece)
        newSpeed = max 0 $ calculateSpeed fromPiece (b^.bMyPosition) myPiece ((lanes $ b^.bTrack) !! endLaneIndex myLane)
        myData = Map.lookup name (_bCarPieceData b)
        goodSpeeds = myData >>= return . map ((* 10) . pdSpeed) . take 3 . filter ((> 0) . pdSpeed)
        newDrag = _bDrag b `mplus` (goodSpeeds >>= (\ss -> if length ss < 3 then Nothing else Just $ calcDrag ss))
        newMass = _bMass b `mplus` (goodSpeeds >>= (\ss -> if length ss < 3 then Nothing else Just $ calcMass ss (calcDrag ss)))
        calcDrag [a, b, c] = let d = (a - (b - a)) / (a * a) in trace ("NEW DRAG: " ++ (show [a,b,c,d])) d
        calcDrag _ = 0.02
        calcMass [a, b, c] drag = let m = 1 / ((log ((c - (1 / drag)) / (b - (1 / drag)))) / (0 - drag)) in trace ("NEW MASS: " ++ (show [a,b,c,drag,m])) m
        calcMass _ _ = 0.02

decodeMessage :: (String, Value, Maybe Int) -> Result ServerMessage
decodeMessage (msgType, msgData, mTick)
  | msgType == "join" = Success Main.Join
  | msgType == "gameInit" = GameInit <$> (fromJSON msgData)
  | msgType == "carPositions" = CarPositions <$> (fromJSON msgData) <*> pure (fromMaybe (-1) mTick)
  | msgType == "gameStart" = Success GameStart
  | msgType == "lapFinished" = Success LapFinished
  | msgType == "crash" = Crash <$> pure (fromMaybe (-1) mTick) <*> (fromJSON msgData)
  | msgType == "spawn" = Spawn <$> pure (fromMaybe (-1) mTick) <*> (fromJSON msgData)
  | msgType == "gameEnd" = Success GameEnd
  | msgType == "turboAvailable" = TurboAvailable <$> (fromJSON msgData)
  | msgType == "turboEnd" = TurboEnd <$> (fromJSON msgData)
  | msgType == "dnf" = DNF <$> (fromJSON msgData)
  | otherwise = Success $ Unknown (msgType ++ " " ++ (show msgData))

instance FromJSON a => FromJSON (String, a, Maybe Int) where
  parseJSON (Object v) = do
    msgType <- v .: "msgType"
    msgData <- v .: "data"
    maybeTick <- v .:? "gameTick"
    return (msgType, msgData, maybeTick)
  parseJSON x          = fail $ "Not an JSON object: " ++ (show x)
