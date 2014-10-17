{-# LANGUAGE LambdaCase, TypeOperators #-}
import Call
import Match
import System.Environment
import qualified Data.Map as Map
import Call.Data.Bitmap as B
import qualified Data.BoundingBox as BB
import Control.Lens
import Codec.Picture.Types
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Data.OpenUnion1.Clean

traceIt x = traceShow x x

initialWorld = World
  { _board = Map.fromList [(V2 0 0, map (Entry 0) $ V2 <$> [0..3] <*> [0..3])]
  , _position = V2 0 0
  , _focus = V2 0 0
  , _clicking = False
  , _shift = False
  }

(.>-.) :: (f ∈ s, Functor m) => Object e f -> Object (Union s) m -> Object (e |> Union s) m
(.>-.) obj o = Object $ fmap (\((a, obj'), o') -> (a, obj' .>-. o')) . runObject o . liftU . runObject obj
  ||> fmap (\(a, o') -> (a, obj .>-. o')) . runObject o

handler :: V2 Double -> Env -> Object (Keyboard |> State World |> Mouse |> Nil) (System s)
handler size env = liftO handle' .>-. sharing handle initialWorld where
  handle' = acceptM $ \case
    Down KeyUp -> position -= V2 0 1
    Down KeyDown -> position += V2 0 1
    Down KeyLeft -> position -= V2 1 0
    Down KeyRight -> position += V2 1 0
    Down KeyLeftShift -> shift .= True
    Up KeyLeftShift -> shift .= False
    _ -> return ()
  handle = acceptM $ \case
    Cursor v -> do
      f0 <- use focus
      p <- use position
      let f = fmap floor ((v - V2 80 80) / size) + p
      focus .= f
      c <- use clicking
      when (c && f /= f0) (enter f)
    Button (Down 0) -> do
      clicking .= True
      use focus >>= enter
    Button (Up 0) -> clicking .= False
    Button (Down 1) -> do
      f <- use focus
      board . ix f %= drop 1
    _ -> return ()
  enter i = do
    b <- use board
    s <- use shift
    if s
      then board . at i .= Nothing
      else case expand env b i of
        [] -> return ()
        xs -> board . at i ?= xs

main = runSystemDefault $ do
    (path:_) <- liftIO getArgs
    prob <- liftIO $ loadProblem path
    let env = mkEnv prob
    bmp <- liftImage' (promoteImage $ theImage prob)
    let w = imageWidth (theImage prob) `div` columns prob
        h = imageHeight (theImage prob) `div` rows prob
    hnd <- new $ handler (fmap fromIntegral $ V2 w h) env
    linkKeyboard hnd
    linkMouse hnd
    let pieces = Map.fromList [(V2 c r, B.clip bmp $ BB.sizePos zero # (V2 w h, V2 c r * V2 w h)) | c <- [0..columns prob - 1], r <- [0..rows prob-1]]
    hist <- new $ liftO $ acceptM $ \_ -> do
      ofs <- hnd .& use position
      b <- hnd .& use board
      return $ translate (V2 80 80) $ translate (fmap fromIntegral (V2 w h) ^* 0.5)
        $ forM_ (Map.keys b) $ \i -> case b ^? ix i . element 0 of
          Just (Entry _ r) -> translate (fmap fromIntegral $ (i - ofs) * V2 w h) $ bitmap $ pieces Map.! r
          Nothing -> return ()
    linkGraphic hist
    stand