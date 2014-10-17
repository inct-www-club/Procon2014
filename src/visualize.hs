{-# LANGUAGE LambdaCase, TypeOperators #-}
import Call
import Match
import System.Environment
import qualified Data.Map as Map
import Call.Data.Bitmap as B
import Data.BoundingBox as BB
import Control.Lens
import Codec.Picture.Types
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Data.OpenUnion1.Clean

traceIt x = traceShow x x

handler :: Monad m => Object (State (Int, V2 Int) |> Keyboard |> Nil) m
handler = sharing handle (0, V2 0 0) where
  handle = acceptM $ \case
    Down KeyUp -> _2 -= V2 0 1
    Down KeyDown -> _2 += V2 0 1
    Down KeyLeft -> _2 -= V2 1 0
    Down KeyRight -> _2 += V2 1 0
    Down KeyZ -> _1 -= 1
    Down KeyX -> _1 += 1
    _ -> return ()

main = runSystemDefault $ do
    (path:_) <- liftIO getArgs
    prob <- liftIO $ loadProblem path
    let board = generate prob
    liftIO $ print $ length board
    bmp <- liftImage' (promoteImage $ theImage prob)
    let w = imageWidth (theImage prob) `div` columns prob
        h = imageHeight (theImage prob) `div` rows prob
    hnd <- new handler
    linkKeyboard hnd
    let pieces = Map.fromList [(V2 c r, B.clip bmp $ traceIt $ BB.sizePos zero # (V2 w h, V2 c r * V2 w h)) | c <- [0..columns prob - 1], r <- [0..rows prob-1]]
    hist <- new $ liftO $ acceptM $ \_ -> do
      n <- hnd .& use _1
      ofs <- hnd .& use _2
      return $ translate (fmap fromIntegral $ V2 w h)
        $ forM_ (V2 <$> [0..columns prob - 1] <*> [0..rows prob - 1]) $ \i -> case board ^? ix n . ix (ofs + i) of
          Just (Entry _ r) -> translate (fmap fromIntegral $ i * V2 w h) $ bitmap $ pieces Map.! r
          Nothing -> return ()
    linkGraphic hist
    stand