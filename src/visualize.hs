{-# LANGUAGE LambdaCase, TypeOperators, OverloadedStrings #-}
import Call
import qualified Match as M
import System.Environment
import qualified Data.Map as Map
import Call.Data.Bitmap as Bitmap
import qualified Data.BoundingBox as BB
import Control.Lens
import Codec.Picture.Types
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Data.OpenUnion1.Clean
import Data.Array
import System.IO.Unsafe
import Text.Printf
import Numeric
import Network (withSocketsDo)
import Network.HTTP.Types
import Network.HTTP.Conduit
import Solve as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent
import Data.String
import Control.Exception hiding (evaluate)
import Data.Char
import Data.List (intercalate)

fAssocs = assocs

fBuild c r = array (V2 0 0, V2 c r - V2 1 1)

traceIt x = traceShow x x

initialWorld = M.World
  { M._board = Map.fromList [(V2 0 0, map (M.Entry 0) $ V2 <$> [0..3] <*> [0..3])]
  , M._position = V2 0 0
  , M._focus = V2 0 0
  , M._clicking = False
  , M._shift = False
  }

token = "0295806849"

sendContent ident str = do
  req <- parseUrl $ serverUrl ++ "/SubmitAnswer"
  let qs = renderSimpleQuery False [("playerid", token), ("problemid", fromString ident), ("answer", fromString str)]
  B.putStrLn qs
  let req' = req {method = "POST", requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")], requestBody = RequestBodyBS qs}
  withManager $ \manager -> httpLbs req' manager >>= liftIO . BL.putStrLn . responseBody

printAns ident ans = do
  putStrLn "******************"
  let str = answerStr ans
  putStrLn str
  case ident of
    Just s -> sendContent s str
    Nothing -> return ()

answerStr ans = show (length ans) ++ "\r\n"
  ++ intercalate "\r\n" (concat [[showHex x "" ++ showHex y ""
    ,show (length os)
    ,concatMap show (reverse os)
    ]
    | Op cost t0@(V2 x y) _ os <- reverse ans])

(.>-.) :: (f ∈ s, Functor m) => Object e f -> Object (Union s) m -> Object (e |> Union s) m
(.>-.) obj o = Object $ fmap (\((a, obj'), o') -> (a, obj' .>-. o')) . runObject o . liftU . runObject obj
  ||> fmap (\(a, o') -> (a, obj .>-. o')) . runObject o

handler :: Maybe String -> M.ProblemInfo -> V2 Double -> M.Env -> Object (Keyboard |> State M.World |> Mouse |> Nil) (System s)
handler ident prob size env = liftO handle' .>-. sharing handle initialWorld where
  handle' = acceptM $ \case
    Down KeyW -> M.position -= V2 0 1
    Down KeyS -> M.position += V2 0 1
    Down KeyA -> M.position -= V2 1 0
    Down KeyD -> M.position += V2 1 0
    Down KeyLeftShift -> M.shift .= True
    Up KeyLeftShift -> M.shift .= False
    Down KeyEnter -> do
      b <- use M.board
      p <- use M.position
      let inner (M.Entry _ x:_) = x
      let costS = fromIntegral $ M.selectCost prob + M.swapCost prob
      let prob' = S.Problem
            { choiceCost = fromIntegral (M.selectCost prob) * 4 / costS
            , swapCost = fromIntegral (M.swapCost prob) * 4 / costS
            , rounds = M.rounds prob
            , columns = M.columns prob
            , rows = M.rows prob
            , evaluate = \field -> fromIntegral
              $ sum $ map (\(i, v) -> S.dist i v) (fAssocs field)
            , isComplete = \field -> (0==) $ fromIntegral
              $ sum $ map (\(i, v) -> S.dist i v) (fAssocs field) }

      let xs = [(x, i - p + V2 1 1) | (i, M.Entry _ x:_) <- Map.toList b]
      return $! unsafePerformIO $ print xs
      let ans = unsafePerformIO $ solve prob' $ fBuild (M.columns prob) (M.rows prob) xs
      return $! unsafePerformIO $ printAns ident ans
    _ -> return ()
  handle = acceptM $ \case
    Cursor v -> do
      f0 <- use M.focus
      p <- use M.position
      let f = fmap floor ((v - V2 80 80) / size) + p
      M.focus .= f
      c <- use M.clicking
      when (c && f /= f0) (enter f)
    Button (Down 0) -> do
      M.clicking .= True
      use M.focus >>= enter
    Button (Up 0) -> M.clicking .= False
    Button (Down 1) -> do
      f <- use M.focus
      M.board . ix f %= drop 1
    _ -> return ()
  enter i = do
    b <- use M.board
    s <- use M.shift
    if s
      then M.board . at i .= Nothing
      else case filter (`notElem`(b^..traverse.element 0)) $ M.expand env b i of
        [] -> return ()
        xs -> M.board . at i ?= xs

tryFetch :: String -> IO B.ByteString
tryFetch url = do
    try (simpleHttp url) >>= \case
        Left e -> print (e :: HttpException) >> threadDelay (2 * 1000 * 1000) >> tryFetch url
        Right a -> return (a ^. strict)

serverUrl = "http://172.16.1.2"

runInteractive ident prob = runSystem Windowed (BB.Box (V2 0 0) (V2 1200 900)) $ do
  let env = M.mkEnv prob
  bmp <- liftImage' (promoteImage $ M.theImage prob)
  let w = imageWidth (M.theImage prob) `div` M.columns prob
      h = imageHeight (M.theImage prob) `div` M.rows prob
  hnd <- new $ handler ident prob (fmap fromIntegral $ V2 w h) env
  linkKeyboard hnd
  linkMouse hnd
  let pieces = Map.fromList [(V2 c r, Bitmap.clip bmp $ BB.sizePos zero # (V2 w h, V2 c r * V2 w h)) | c <- [0..M.columns prob - 1], r <- [0..M.rows prob-1]]
  hist <- new $ liftO $ acceptM $ \_ -> do
    ofs <- hnd .& use M.position
    b <- hnd .& use M.board
    return $ translate (V2 80 80) $ translate (fmap fromIntegral (V2 w h) ^* 0.5)
      $ forM_ (Map.keys b) $ \i -> case b ^? ix i . element 0 of
        Just (M.Entry _ r) -> translate (fmap fromIntegral $ (i - ofs) * V2 w h) $ bitmap $ pieces Map.! r
        Nothing -> return ()
  linkGraphic hist
  stand

main = withSocketsDo $ getArgs >>= \case
  ("remote":ident:_) -> do
    let name = "prob" ++ ident ++ ".ppm"
    let url = serverUrl ++ "/problem/" ++ name
    putStrLn url
    tryFetch url >>= B.writeFile name
    prob <- M.loadProblem name
    runInteractive (Just ident) prob
  (path:_) -> do
    prob <- M.loadProblem path
    runInteractive Nothing prob
  