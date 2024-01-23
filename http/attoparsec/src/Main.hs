{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main                   (bench, defaultMain, env, whnf)
import qualified Data.ByteString                  as B

import           Control.Applicative
import           Data.Attoparsec.ByteString       as P
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8, isEndOfLine, isHorizontalSpace)
import           Data.ByteString                  (ByteString)
import           Data.Word                        (Word8)

isToken :: Word8 -> Bool
isToken w =
  w <= 127
    && w > 31 -- "\0-\31"
    && ( w /= 40 -- "("
           && w /= 41 -- ")"
           && w /= 60 -- "<"
           && w /= 62 -- ">"
           && w /= 64 -- "@"
           && w /= 44 -- ","
           && w /= 59 -- ";"
           && w /= 58 -- ":"
           && w /= 92 -- "\\"
           && w /= 34 -- "\""
           && w /= 91 -- "["
           && w /= 93 -- "]"
           && w /= 63 -- "?"
           && w /= 61 -- "="
           && w /= 123 -- "{"
           && w /= 125 -- "}"
           && w /= 32 -- " "
           && w /= 9 -- "\t")
       )

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

data Request = Request {
      requestMethod  :: !ByteString
    , requestUri     :: !ByteString
    , requestVersion :: !ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = Request <$> (takeWhile1 isToken <* char8 ' ')
                      <*> (takeWhile1 (/=32) <* char8 ' ')
                      <*> (httpVersion <* endOfLine)

data Header = Header {
      headerName  :: !ByteString
    , headerValue :: ![ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = Header
  <$> (P.takeWhile isToken <* char8 ':' <* skipWhile isHorizontalSpace)
  <*> ((:) <$> (takeTill isEndOfLine <* endOfLine)
           <*> many ( skipSpaces *> takeTill isEndOfLine <* endOfLine))

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

allRequests :: Parser [(Request, [Header])]
allRequests = many1 request

smallFile :: FilePath
smallFile = "../http-requests.txt"

biggerFile :: FilePath
biggerFile = "../bigger.txt"

main :: IO ()
main = defaultMain
    [ env (B.readFile smallFile) $ \small ->
        bench "small" $ whnf (P.parseOnly allRequests) small,
      env (B.readFile biggerFile) $ \big ->
        bench "big" $ whnf (P.parseOnly allRequests) big
    ]
