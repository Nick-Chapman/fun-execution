
module SpeedJsonParser(main) where

import Data.Time (getCurrentTime,diffUTCTime)
import Numeric (showFFloat)
import JsonParser (pipeline)
--import JsonParserGADT (pipeline)

main :: IO ()
main = sequence_ $ repeat $ do

  let n = 20

  before <- getCurrentTime
  let !size = pipeline n
  after <- getCurrentTime

  --let size_k :: Double = fromIntegral size / 1e3
  let size_b :: Double = fromIntegral size
  let duration_s :: Double = realToFrac (diffUTCTime after before)
  let speed_k_per_s = size_b / 1000 / duration_s

  let mes = concat
        [ "n = ", show n
        , ", size(b) = ", showFFloat (Just 0) size_b ""
        , ", duration(s) = ", showFFloat (Just 3) duration_s ""
        , ", speed(k/s) = ", showFFloat (Just 0) speed_k_per_s ""
        ]

  putStrLn mes
