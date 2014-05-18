{-# LANGUAGE LambdaCase, TemplateHaskell, NoMonomorphismRestriction #-}
{-

$ cabal update
$ cabal install free-game --force-reinstalls
$ ghc -O2 main.hs
$ ./main

Usage:
    Z: Undo swapping
    Ctrl+Z: Undo stroke
    R: Randomize
    Del: Reset
-}
import FreeGame
import qualified Data.Sequence as Seq
import Control.Monad
import Data.Array
import Control.Lens
import Control.Monad.Trans.Iter
import Control.Monad.Trans
import Control.Monad.State
import System.Environment
import Control.Arrow
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import System.Random

data World = World
    { _board :: Array (V2 Int) (V2 Int, Frame ())
    , _initialBoard :: Array (V2 Int) (V2 Int, Frame ())
    , _queue :: Seq.Seq (V2 Int, V2 Int)
    , _panelSize :: Vec2
    , _selection :: Maybe (V2 Int)
    , _stroke :: [V2 Int]
    , _strokes :: [[V2 Int]]
    , _clock :: Double }

initialWorld w h c r pic = World
    { _board = listArray bnd $ map (id &&& panel) $ range bnd
    , _initialBoard = listArray bnd $ map (id &&& panel) $ range bnd
    , _queue = Seq.empty
    , _panelSize = fmap fromIntegral (V2 w h)
    , _selection = Nothing
    , _stroke = []
    , _clock = 0
    , _strokes = [] }
    where
        bnd = (V2 0 0, V2 (c-1) (r-1))
        panel (V2 i j) = do
            bitmap $ cropBitmap pic (w, h) (w * i, h * j)
makeLenses ''World

toPanelCoord :: Vec2 -> Vec2 -> V2 Int
toPanelCoord size = fmap floor . flip (liftA2 (/)) size . (+size/2)

fromPanelCoord :: Vec2 -> V2 Int -> Vec2
fromPanelCoord size = (*size) . fmap fromIntegral

gameControl = liftA2 (,) (use selection) mouseButtonL >>= \case
    (Nothing, False) -> delay gameControl
    (Nothing, True) -> do
        pos <- mousePosition
        siz <- use panelSize
        fieldSize <- uses board bounds
        let p = toPanelCoord siz pos
        stroke .= [p]
        when (inRange fieldSize p) $ selection .= Just p
        delay gameControl
    (Just k, False) -> do
        s <- use stroke
        strokes %= (s:)
        stroke .= []
        selection .= Nothing
        delay gameControl
    (Just k, True) -> do
        pos <- mousePosition
        siz <- use panelSize
        fieldSize <- uses board bounds
        let p = toPanelCoord siz pos
        when (p - k `elem` [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)] && inRange fieldSize p) $ do
            queue %= flip snoc (k, p)
            s <- use stroke
            if s ^? ix 1 == Just p
                then stroke .= drop 1 s
                else stroke .= p : s

            selection ?= p
        delay gameControl

gameView :: IterT (StateT World Frame) ()
gameView = preuse (queue . _Cons) >>= \case
    Just ((i, j), ss) -> do
        queue .= ss
        b <- use board
        siz <- use panelSize
        let m = b ^?! ix i
            n = b ^?! ix j
            p = fromPanelCoord siz i
            q = fromPanelCoord siz j
        forM_ [0,0.125..1] $ \t -> do
            translate (p ^* (1 - t) + q ^* t) (reFrame $ snd m)
            translate (p ^* t + q ^* (1 - t)) (reFrame $ snd n)
            forM_ (assocs b) $ \(k, f) -> when (k `notElem` [i, j])
                $ translate (fromPanelCoord siz k) (reFrame $ snd f)
            tick
        board . ix j .= m
        board . ix i .= n
        gameView
    Nothing -> do
        b <- use board
        siz <- use panelSize
        forM_ (assocs b) $ \(k, f) -> translate (fromPanelCoord siz k) (reFrame $ snd f)
        delay gameView

nav = do
    siz <- use panelSize
    pos' <- fromPanelCoord siz <$> toPanelCoord siz <$> mousePosition
    let V2 hw hh = siz / 2
    color red $ translate pos' $ polygonOutline [V2 (-hw) (-hh), V2 hw (-hh), V2 hw hh , V2 (-hw) hh]
    delay nav

guide = do
    b <- use board
    siz <- use panelSize
    forM_ (assocs b) $ \(k, (o, _)) -> when (k /= o) $ do
        
        let dr = do
                let p = fromPanelCoord siz k
                let q = fromPanelCoord siz o
                let m = (p * 3 + q) / 4
                line [p, q]
                line [m, m + normalize (m - p - perp (m - p)) * 12]
                line [m, m + normalize (m - p + perp (m - p)) * 12]
        thickness 4 $ color (Color 1 1 1 0.7) dr
        case Foldable.sum o `mod` 2 of
            0 -> thickness 2 $ color (Color 1 0 0 0.7) dr
            1 -> thickness 2 $ color (Color 0 0 1 0.7) dr
    vs <- uses stroke $ map (fromPanelCoord siz)
    t <- use clock
    color (Color 1 0.5 0 0.7) $ forM (zip vs (tail vs))
        $ \(q, p) -> do
            translate (p ^* (1 - t) + q ^* t) $ circle 8
    clock += 0.05
    whenM (uses clock (>1)) $ clock -= 1
    delay guide

shuffleIO :: [a] -> IO [a]
shuffleIO [] = return []
shuffleIO [x] = return [x]
shuffleIO xs = do
    i <- randomRIO (0, length xs - 1)
    let (ys, z:zs) = splitAt i xs
    (z :) <$> shuffleIO (ys ++ zs)

command = do
    whenM (keyDown KeyDelete) $ board <~ use initialBoard
    whenM (keyDown KeyR) $ do
        b <- use board
        xs <- embedIO $ shuffleIO (elems b)
        board .= listArray (bounds b) xs
    whenM (keyDown KeyZ) $ do
        b <- keyPress KeyLeftControl
        if b
            then use stroke >>= \case
                [] -> use strokes >>= \case
                    (s:ss) -> do
                        queue .= Seq.fromList (zip s (tail s))
                        strokes .= ss
                    [] -> return ()
                s -> queue .= Seq.fromList (zip s (tail s))
            else do
                use stroke >>= \case
                    (_:_:_) -> return ()
                    _ -> use strokes >>= \case
                        (s:ss) -> do
                            stroke .= s
                            strokes .= ss
                        _ -> return ()
                use stroke >>= \case
                    (s:t:ss) -> do
                        queue %= flip snoc (s, t)
                        stroke .= t : ss
                    _ -> return ()

    delay command

stat font = do
    s <- uses stroke length
    translate (V2 400 100) $ color black $ text font 32 $ "<" ++ show s ++ ">"
    delay (stat font)

mainLoop :: s -> IterT (StateT s Frame) a -> IterT Frame a
mainLoop s m = lift (runStateT (runIterT m) s) >>= \case
    (Left a, _) -> return a
    (Right cont, s') -> delay $ mainLoop s' cont

main = runGame Windowed (Box (V2 0 0) (V2 1024 768)) $ do
    (path:_) <- liftIO getArgs
    clearColor white
    setTitle "詫び石&ダメコンズ"
    font <- loadFont "VL-PGothic-Regular.ttf"
    bmp <- readBitmap path
    let (w,h) = bitmapSize bmp
    mainLoop (initialWorld (w`div`4) (h`div`4) 4 4 bmp)
        $ translate (V2 64 64)
        $ scale (min (1024/fromIntegral w) (768/fromIntegral h))
        $ interleave_ [gameControl, gameView, nav, guide, command, stat font]