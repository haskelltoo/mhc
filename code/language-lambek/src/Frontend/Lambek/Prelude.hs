module Frontend.Lambek.Prelude
  (
    module Prelude
  , Type
  , module Data.Bifunctor
  , module Data.Int
  , module Data.Word
  , module Data.Maybe
  , takeWhileEnd
  , stripSuffix
  , openFileM
  , openTmpFile
  , appendDot
  )
  where

import Prelude
import Data.Bifunctor
import Control.Exception
import Data.Int
import Data.Kind
import Data.List
import Data.Maybe
import Data.Text (Text, append, pack)
import Data.Word
import System.Environment
import System.IO

takeWhileEnd :: forall a . (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

stripSuffix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s t =
  case stripPrefix (reverse s) (reverse t) of
    Nothing -> Nothing
    Just x -> Just (reverse x)

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM path m = do
  r <- (try $ openFile path m) :: IO (Either IOError Handle)
  case r of
    Left _ -> return Nothing
    Right h -> return (Just h)

openTmpFile :: String -> IO (String, Handle)
openTmpFile tmplt = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
  res <- try $ openTempFile tmp tmplt
  case res of
    Right x -> return x
    Left (_::SomeException) -> openTempFile "." tmplt

appendDot :: Text -> Text -> Text
appendDot x y = x `append` pack "." `append` y
