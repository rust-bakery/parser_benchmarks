{-# LANGUAGE OverloadedStrings #-}

-- This attoparsec module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or ISO-8859-15.
--import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Binary           as Bin
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString                  as B
import           Data.Word

data LV = LV Word32 B.ByteString deriving Show

mp4Box :: C.Parser LV
mp4Box = do
  length <- Bin.anyWord32be
  value  <- C.take . fromIntegral $ length - 4
  return $ LV length value

mp4Parser = C.many1 mp4Box

smallFile :: FilePath
smallFile = "small.mp4"
bunnyFile :: FilePath
bunnyFile = "../bigbuckbunny.mp4"

main :: IO ()
main = B.readFile smallFile >>= print . C.parseOnly mp4Parser
