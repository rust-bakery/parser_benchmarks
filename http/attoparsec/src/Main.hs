{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main                   (bench, bgroup, defaultMain, env, whnf)
import qualified Data.ByteString                  as B

import           Control.Applicative
import           Data.Attoparsec.ByteString       as P
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8)
import           Data.Attoparsec.ByteString.Char8 (isEndOfLine, isHorizontalSpace)
import           Data.ByteString                  (ByteString)
import           Data.Word                        (Word8)

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

data Request = Request {
      requestMethod  :: ByteString
    , requestUri     :: ByteString
    , requestVersion :: ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = Request <$> (takeWhile1 isToken <* char8 ' ')
                      <*> (takeWhile1 (/=32) <* char8 ' ')
                      <*> (httpVersion <* endOfLine)

data Header = Header {
      headerName  :: ByteString
    , headerValue :: [ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = Header
  <$> (P.takeWhile isToken <* char8 ':' <* skipWhile isHorizontalSpace)
  <*> ((:) <$> (takeTill isEndOfLine <* endOfLine)
           <*> (many $ skipSpaces *> takeTill isEndOfLine <* endOfLine))

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

allRequests :: Parser [(Request, [Header])]
allRequests = many1 request

smallFile :: FilePath
smallFile = "../http-requests.txt"

biggerFile :: FilePath
biggerFile = "../bigger.txt"

setupEnv :: IO (ByteString, ByteString)
setupEnv = do
  small  <- B.readFile smallFile
  bigger <- B.readFile biggerFile
  return (small, bigger)

main :: IO ()
main = defaultMain
    [
      env setupEnv $ \ ~(small, bigger) ->
      bgroup "IO"
        [
          bench "small"          $ whnf (P.parseOnly allRequests) small
        , bench "bigger"         $ whnf (P.parseOnly allRequests) bigger
        ]
    ]
