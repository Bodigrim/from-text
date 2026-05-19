{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.From
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified System.OsString as OS
import qualified System.OsString.Posix as OSP
import qualified System.OsString.Windows as OSW
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), choose, getUnicodeString, testProperty, (===))

main :: IO ()
main =
  defaultMain $
    testGroup
      "IsText"
      [ testProperty "String" $
          \xs -> T.pack (fromText xs) === xs
      , testProperty "Identity" $
          \xs -> fromText xs === Identity (fromText xs :: String)
      , testProperty "Const" $
          \xs -> (fromText xs :: Const String ()) === Const (fromText xs)
      , testProperty "ByteArray" $
          \xs -> TE.decodeUtf8 (BS.fromShort (BS.ShortByteString (fromText xs))) === xs
      , testProperty "StrictByteString" $
          \xs -> TE.decodeUtf8 (fromText xs) === xs
      , testProperty "LazyByteString" $
          \xs -> TE.decodeUtf8 (BL.toStrict (fromText xs)) === xs
      , testProperty "Builder (ByteString)" $
          \xs -> TE.decodeUtf8 (BL.toStrict (BB.toLazyByteString (fromText xs))) === xs
      , testProperty "StrictText" $
          \xs -> fromText xs === xs
      , testProperty "LazyText" $
          \xs -> TL.toStrict (fromText xs) === xs
      , testProperty "Builder (Text)" $
          \xs -> TL.toStrict (TLB.toLazyText (fromText xs)) === xs
      , testProperty "PosixString" $
          \xs -> OSP.decodeUtf (fromText xs) === Just (T.unpack xs)
      , testProperty "WindowsString" $
          \xs -> OSW.decodeUtf (fromText xs) === Just (T.unpack xs)
      , testProperty "OsString" $
          \xs -> OS.decodeUtf (fromText xs) === Just (T.unpack xs)
      ]

instance Arbitrary T.Text where
  arbitrary = do
    t <- T.pack . getUnicodeString <$> arbitrary
    t' <- (`T.drop` t) <$> choose (0, T.length t `quot` 2)
    (`T.take` t) <$> choose (0, T.length t' `quot` 2)
  shrink = map T.pack . shrink . T.unpack
