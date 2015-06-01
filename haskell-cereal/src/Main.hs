{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad       (replicateM)
import           Criterion.Main      (bench, bgroup, defaultMain, env, nf, nf)
import           Control.DeepSeq     (NFData(..))
import qualified Data.ByteString     as B
import           Data.Serialize.Get hiding (skip, remaining, label)
import           Data.Word
import qualified Data.Vector         as V

data LV = LV {-# UNPACK #-} !Int {-# UNPACK #-} !BOX deriving Show
data BOX
    = FTYP {-# UNPACK #-} !B.ByteString
           {-# UNPACK #-} !Word32
           {-# UNPACK #-} !(V.Vector B.ByteString)
    | FREE
    | MOOV
    | SKIP
    | MDAT deriving Show

instance NFData LV where
  rnf (LV i box) = i `seq` rnf box

instance NFData BOX where
  rnf (FTYP b w bs) = b `seq` w `seq` rnf bs
  rnf a = a `seq` ()

ftypBox :: Int -> Get BOX
ftypBox l = do
  major_brand         <- getBytes 4
  major_brand_version <- getWord32be
  let remaining       = (l - 8) `div` 4
  compatible_brands   <- V.replicateM remaining (getBytes 4)
  return $ FTYP major_brand major_brand_version compatible_brands

mp4Box :: Get LV
mp4Box = do
  length <- fmap fromIntegral getWord32be
  let remaining = length - 4
  bs <- getBytes remaining
  let Right value = runGet (anyBox remaining) bs
  return (LV length value)

anyBox :: Int -> Get BOX
anyBox l = do label <- getBytes 4
              case label of
                "ftyp" -> ftypBox (l - 4)
                "free" -> return FREE
                "mdat" -> return MDAT
                "moov" -> return MOOV
                "skip" -> return SKIP
                _      -> fail "Type not recognized"

mp4Parser :: Get [LV]
mp4Parser = do
  empty <- isEmpty
  if empty
    then return []
    else do trade <- mp4Box
            trades <- mp4Parser
            return (trade:trades)

smallFile :: FilePath
smallFile = "../small.mp4"
bunnyFile :: FilePath
bunnyFile = "../bigbuckbunny.mp4"

setupEnv :: IO (B.ByteString, B.ByteString)
setupEnv = do
  small <- B.readFile smallFile
  bunny <- B.readFile bunnyFile
  return (small, bunny)

criterion :: IO ()
criterion = defaultMain
    [
      env setupEnv $ \ ~(small,bunny) ->
      bgroup "IO"
        [
          bench "small"          $ nf (runGet mp4Parser) small
        , bench "big buck bunny" $ nf (runGet mp4Parser) bunny
        ]
    ]

main :: IO ()
main = criterion
-- main = do B.readFile smallFile >>= print . runGet mp4Parser
--           B.readFile bunnyFile >>= print . runGet mp4Parser
