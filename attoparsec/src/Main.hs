{-# LANGUAGE OverloadedStrings #-}

-- This attoparsec module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or ISO-8859-15.
--import qualified Data.Attoparsec.ByteString.Char8
import           Control.Applicative              ((<$>), (<*>))
import qualified Data.Attoparsec.Binary           as Bin
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as C8
import           Data.Word

--data LV = LV Int B.ByteString deriving Show
data LV = LV Int BOX deriving Show
data BOX = FTYP B.ByteString Word32 [B.ByteString] | FREE deriving Show

ftypBox :: C.Parser BOX
ftypBox = do
  C.string "ftyp"
  major_brand         <- C.take 4
  major_brand_version <- Bin.anyWord32be
  compatible_brands   <- C.many' (C.take 4)
  return $ FTYP major_brand major_brand_version compatible_brands

freeBox = do
  C.string "free"
  return $ FREE

mp4Box :: C.Parser LV
mp4Box = do
  length <- fromIntegral <$> Bin.anyWord32be
  Right value  <- C.parseOnly anyBox <$> (C.take $ length - 4)
  return $ LV length value

anyBox = C.choice [ftypBox, freeBox]

mp4Parser = C.many1 mp4Box

smallFile :: FilePath
smallFile = "../small.mp4"
bunnyFile :: FilePath
bunnyFile = "../bigbuckbunny.mp4"

main :: IO ()
main = B.readFile smallFile >>= print . C.parseOnly mp4Parser
