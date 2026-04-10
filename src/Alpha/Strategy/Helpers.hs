module Alpha.Strategy.Helpers
    (
        RunningSum,
        emptyRunningSum,
        pushValue, 
        currentSum, 
        currentAverage, 
        rsFull, 
        rsCount, 
    )
where 

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Sequence (Seq, ViewL (..), (|>)) 
import qualified Data.Sequence as Seq

-- | Bounded ring buffer that maintains a running sum for O(1) 
-- moving average updates, Capacity is fixed at construction 
data RunningSum = RunningSum 
    { 
        rsBuffer :: !(Seq Scientific),
        rsSum :: !Scientific,
        rsCapacity :: !Int
    }
    deriving (Show, Eq) 

emptyRunningSum :: Int -> RunningSum 
emptyRunningSum = RunningSum Seq.empty 0

-- | Push a value into the buffer. When full, the oldest value is 
-- evicted and subtracted from the running sum in O(1) amortized 
pushValue :: Scientific -> RunningSum -> RunningSum 
pushValue x rs 
    | Seq.length (rsBuffer rs) < rsCapacity rs = 
        rs 
        { rsBuffer = rsBuffer rs |> x, 
           rsSum = rsSum rs + x
        } 
    | otherwise = 
        case Seq.viewl (rsBuffer rs) of 
            EmptyL -> rs 
            oldest :< rest -> 
                rs 
                { rsBuffer = rest |> x, 
                   rsSum = rsSum rs - oldest + x 
                } 

currentSum :: RunningSum -> Scientific 
currentSum = rsSum  

currentAverage :: RunningSum -> Maybe Scientific 
currentAverage rs 
    | Seq.null (rsBuffer rs) = Nothing 
    | otherwise = Just $ fromFloatDigits
        (toRealFloat (rsSum rs) / fromIntegral (Seq.length (rsBuffer rs)) :: Double)

rsFull :: RunningSum -> Bool 
rsFull rs = Seq.length (rsBuffer rs) == rsCapacity rs 

rsCount :: RunningSum -> Int 
rsCount = Seq.length . rsBuffer  

