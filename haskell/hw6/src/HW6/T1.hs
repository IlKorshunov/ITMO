module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where
import           Control.Concurrent.Classy     (MonadConc (atomically), STM)
import           Control.Concurrent.Classy.STM (MonadSTM (TVar, newTVar, readTVar, writeTVar),
                                                TArray, modifyTVar')
import           Control.Monad                 (forM_)
import           Data.Array.Base
import           Data.Hashable
import           Data.List                     (find)
import           GHC.Base

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets  :: TVar stm (BucketsArray stm k v)
  , chtSize     :: TVar stm Int
  , chtCapacity :: TVar stm Int
  }


newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  buckets <- newCHTBuckets initCapacity
  bucketsVar <- newTVar buckets
  sizeVar <- newTVar 0
  capacityVar <- newTVar initCapacity
  return $ CHT {chtBuckets = bucketsVar, chtSize = sizeVar, chtCapacity = capacityVar}


getCHT :: (MonadConc m, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key cht = atomically $ do
    bucketArray <- readTVar (chtBuckets cht)
    curCap <- readTVar (chtCapacity cht)
    let index = choseBucketIndex key curCap
    localBucket <- readArray bucketArray index
    return (snd <$> find (\(k, _) -> k == key) localBucket)


putCHT :: (MonadConc m, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key value cht = atomically $ do
  curCap <- readTVar (chtCapacity cht)
  currentSize <- getAllElementSize cht
  when (checkIfNeedResize currentSize curCap) $
    resizeCHT curCap cht
  newCap <- readTVar (chtCapacity cht)
  bucketArray <- readTVar (chtBuckets cht)
  let bucketIndex = choseBucketIndex key newCap
  currentBucket <- readArray bucketArray bucketIndex
  (newBucket, flag) <- updateBucket key value currentBucket
  writeArray bucketArray bucketIndex newBucket
  when flag $ modifyTVar' (chtSize cht) (+ 1)



sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = atomically . getAllElementSize

updateBucket :: (Eq k, MonadSTM stm) => k -> v -> Bucket k v -> stm (Bucket k v, Bool)
updateBucket key value bucket = do
  let newBucket = map (\(k, v) -> if k == key then (k, value) else (k, v)) bucket
  if any (\(k, _) -> k == key) bucket
    then return (newBucket, False)
    else return ((key, value) : bucket, True)

checkIfNeedResize :: Int -> Int -> Bool
checkIfNeedResize curSizeBuckets capacity = fromIntegral curSizeBuckets >= fromIntegral capacity * loadFactor

-- getAllBucketSize :: MonadSTM stm => CHT stm k v -> stm Int
-- getAllBucketSize cht = readTVar (chtBuckets cht) >>= getNumElements

resizeCHT :: (Hashable k, MonadSTM stm) => Int -> CHT stm k v -> stm ()
resizeCHT curCap cht = do
  let newCapacity = curCap * 2
  newarray <- newCHTBuckets newCapacity
  allElements <- getAllElements cht
  forM_ allElements (uncurry (writePairToBucket newarray newCapacity))
  writeTVar (chtBuckets cht) newarray
  writeTVar (chtCapacity cht) newCapacity

writePairToBucket :: (MonadSTM stm, Hashable k) => BucketsArray stm k v -> Int -> k -> v -> stm ()
writePairToBucket bucketArray cap key value = do
  let index = choseBucketIndex key cap
  currentBucket <- readArray bucketArray index
  let newBucket = (key, value) : currentBucket
  writeArray bucketArray index newBucket

-- returnEqualKeyPosition :: (Eq k, MonadSTM stm) => k -> Bucket k v -> stm (Maybe Int)
-- returnEqualKeyPosition key bucket =
--   return $ fst <$> find (\ (_, (k, _)) -> k == key) (zip [0 .. ] bucket)

choseBucketIndex :: (Hashable k) => k -> Int -> Int
choseBucketIndex key capacity = hash key `mod` capacity

newCHTBuckets :: (MonadSTM stm) => Int -> stm (BucketsArray stm k v)
newCHTBuckets size = newArray (0, size - 1) []

getAllElementSize :: MonadSTM stm => CHT stm k v -> stm Int
getAllElementSize cht = readTVar (chtSize cht)

getAllElements :: MonadSTM stm => CHT stm k v -> stm (Bucket k v)
getAllElements cht = concat <$> (readTVar (chtBuckets cht) >>= \bucketArray -> getBounds bucketArray >>= \(startBucketArray, endBucketArray) -> Prelude.mapM (readArray bucketArray) [startBucketArray .. endBucketArray])
