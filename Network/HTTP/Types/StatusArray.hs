module Network.HTTP.Types.StatusArray (
    statusCodeToByteString
  ) where

import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Text.Printf

-- | Converting numeric status code to 'ByteString' in O(1).
statusCodeToByteString :: Int -> ByteString
statusCodeToByteString sc
  | sc < statusMin || sc > statusMax = B8.pack $ printf "%03d" (abs sc `mod` 1000)
statusCodeToByteString sc = statusArray ! sc

statusMin :: Int
statusMin = 100
statusMax :: Int
statusMax = 599

statusArray :: Array Int ByteString
statusArray = listArray (statusMin,statusMax) bss
  where
    bss = map (B8.pack . show) [statusMin .. statusMax]
