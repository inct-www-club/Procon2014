module Util where
import Data.Heap as H
import Control.Lens
_payload :: Lens' (H.Entry p a) a
_payload f (H.Entry p a) = f a <&> \a' -> H.Entry p a'
