{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative              ((<$>), (<*>))
import           Criterion.Main                   (bench, bgroup, defaultMain,
                                                   env, nf)
import           Control.DeepSeq                  (NFData(..))
import qualified Data.Attoparsec.Binary           as Bin
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as C8
import qualified Data.Vector                      as V
import           Data.Word

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

ftypBox :: Int -> C.Parser BOX
ftypBox l = do
  major_brand         <- C.take 4
  major_brand_version <- Bin.anyWord32be
  let remaining       = (l - 8) `div` 4
  compatible_brands   <- V.replicateM remaining (C.take 4)
  return $ FTYP major_brand major_brand_version compatible_brands

mp4Box :: C.Parser LV
mp4Box = do
  length <- fromIntegral <$> Bin.anyWord32be
  let remaining = length - 4
  bs <- C.take remaining
  let Right value = C.parseOnly (anyBox remaining) bs
  return $ LV length value

anyBox l = do
  name <- C.take 4
  case name of
    "ftyp" -> ftypBox (l - 4)
    "free" -> return FREE
    "mdat" -> return MDAT
    "moov" -> return MOOV
    "skip" -> return SKIP
    _      -> fail "Unknown box type"

mp4Parser = C.many1 mp4Box

smallFile :: FilePath
smallFile = "../small.mp4"
bunnyFile :: FilePath
bunnyFile = "../bigbuckbunny.mp4"

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
          bench "small"          $ nf (C.parseOnly mp4Parser) small
        , bench "big buck bunny" $ nf (C.parseOnly mp4Parser) bunny
        ]
    ]

main :: IO ()
main = criterion
--main = B.readFile smallFile >>= print . C.parseOnly mp4Parser
