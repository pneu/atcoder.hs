--import Data.Ix
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.List as L
import Data.Array.IO hiding (range)
import Text.Printf
import Foreign.Ptr
import Foreign.Storable
import Debug.Trace (trace)
import Prelude hiding (cycle)

-- | Remove the stack size limit in the RTS (+RTS -K).
unlimitStackSize :: IO ()
unlimitStackSize = do
  current <- peek maxStackSizePtr
  when (current == 0x100000) $ -- default limit: 1M words
    poke maxStackSizePtr (-1)

maxStackSizePtr :: Ptr Word32
maxStackSizePtr = plusPtr rtsFlagsPtr 12

foreign import ccall "&RtsFlags" rtsFlagsPtr :: Ptr Word32

readInt :: String -> Int
readInt = read

getInt :: IO Int
getInt = getLine >>= return . readInt

getInts :: IO [Int]
getInts = getLine >>= return . map readInt . words

toT :: [a] -> (a, a)
toT (x:y:[]) = (x,y)

uc :: (Integral a, Num b) => a -> b
uc = fromIntegral

toBits :: Integral a => Int -> [a]
toBits = L.unfoldr (\b ->
          if b == 0
          then Nothing
          else Just (fromIntegral $ b `mod` 2, b `div` 2))

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f,g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f,g) = pair (f.fst, g.snd)

main :: IO ()
main = maximum <$> getInts >>= print
