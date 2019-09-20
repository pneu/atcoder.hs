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
 
debug x = trace (show x) x
 
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
 
tup :: [a] -> (a, a)
tup (x:y:[]) = (x,y)

untup :: (a, a) -> [a]
untup (x,y) = [x,y]
 
uc :: (Integral a, Num b) => a -> b
uc = fromIntegral
 
toBits :: Integral a => Int -> [a]
toBits = L.unfoldr (\b ->
          if b == 0
          then Nothing
          else Just (fromIntegral $ b `mod` 2, b `div` 2))
 
marray :: (Int, Int) -> Int -> IO (IOUArray Int Int)
marray (a,b) x = newListArray (a,b) (repeat x)
-- dist <- newListArray ((1,1),(n,n)) (repeat inf) :: IO (IOUArray (Int, Int) Int)
-- let getDist = readArray dist :: (Int, Int) -> IO Int
-- putDist = writeArray dist :: (Int, Int) -> Int -> IO ()
 
marray' :: Ix i => (i,i) -> Int -> IO (IOUArray i Int)
marray' (a,b) x = newListArray (a,b) (repeat x)
 
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f,g) x = (f x, g x)
 
cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f,g) = pair (f.fst, g.snd)
 
win = "You can win"
lose = "You will lose"

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  let a = zip s t
      b = zipWith (==) s t
      c = zip a b
  if (all id $ map f c) == True
    then putStrLn win
    else putStrLn lose
  where
    f = \ts -> case snd ts of
          True -> True
          False -> let t = untup (fst ts)
                    in if any (== '@') t 
                        then any id $ collection t
                        else False
    collection t = (==) <$> "atcoder" <*> t
