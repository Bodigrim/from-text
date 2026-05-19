{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Convert strict 'T.Text' to other textual types,
-- including t'ByteArray', 'B.ByteString' and 'System.OsPath.OsPath'.
module Data.Text.From (
  IsText (..),
) where

import Data.Array.Byte (ByteArray (..))
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Unsafe as TU
import GHC.Exts (sizeofByteArray#, writeWord16Array#)
import GHC.Int (Int (..))
import GHC.ST (ST (..), runST)
import GHC.Word (Word16 (..))
import qualified System.OsString as OS
import qualified System.OsString.Data.ByteString.Short.Internal as OSI
import qualified System.OsString.Internal.Types as OSIT
import qualified System.OsString.Posix as OSP

-- | Convert strict 'T.Text' to other textual types.
--
-- This is modeled after 'Data.String.IsString'
-- with the aim to avoid dealing with 'String' ever.
class IsText a where
  fromText :: T.Text -> a

instance a ~ Char => IsText [a] where
  fromText = T.unpack

instance IsText a => IsText (Identity a) where
  fromText = coerce (fromText @a)

instance IsText a => IsText (Const a b) where
  fromText = coerce (fromText @a)

-- | Encodes as UTF-8.
instance IsText ByteArray where
  fromText (T.Text arr@(ByteArray ba) 0 len)
    | I# (sizeofByteArray# ba) == len = arr
  fromText (T.Text arr off len) = TA.run $ do
    marr <- TA.new len
    TA.copyI len marr 0 arr off
    pure marr

-- | Encodes as UTF-8.
instance IsText B.StrictByteString where
  fromText = TE.encodeUtf8

-- | Encodes as UTF-8.
instance IsText BL.LazyByteString where
  fromText = BL.fromStrict . TE.encodeUtf8

-- | Encodes as UTF-8.
instance IsText BB.Builder where
  fromText = TE.encodeUtf8Builder

-- | Encodes as UTF-8.
instance IsText BS.ShortByteString where
  fromText = coerce (fromText @ByteArray)

instance IsText T.StrictText where
  fromText = id

instance IsText TL.LazyText where
  fromText = TL.fromStrict

instance IsText TLB.Builder where
  fromText = TLB.fromText

-- | Encodes as UTF-8.
instance IsText OSP.PosixString where
  fromText = coerce (fromText @ByteArray)

-- | Encodes as UTF-16 LE.
instance IsText OSIT.WindowsString where
  fromText (T.Text src off len) = runST $ do
    marr <- TA.new (len * 2)
    let go !srcOff !dstOff
          | srcOff >= len + off = do
              TA.shrinkM marr (dstOff * 2)
              arr <- TA.unsafeFreeze marr
              pure $ coerce arr
          | otherwise = do
              let !(TU.Iter c d) = TU.iterArray src srcOff
                  n = ord c
              d' <-
                if n <= 0xFFFF
                  then do
                    writeWord16LE marr dstOff (fromIntegral n)
                    pure 1
                  else do
                    let n1 = n - 0x10000
                    writeWord16LE marr dstOff (fromIntegral $ n1 `shiftR` 10 + 0xD800)
                    writeWord16LE marr (dstOff + 1) (fromIntegral $ n1 .&. 0x3FF + 0xDC00)
                    pure 2
              go (srcOff + d) (dstOff + d')
    go off 0
    where
      writeWord16LE :: TA.MArray s -> Int -> Word16 -> ST s ()
      writeWord16LE (TA.MutableByteArray marr) (I# offset) (W16# w16) = ST $ \s ->
        case writeWord16Array# marr offset (OSI.word16ToLE# w16) s of
          s' -> (# s', () #)

-- | Also known as 'System.OsPath.OsPath'.
instance IsText OS.OsString where
  fromText = case OS.coercionToPlatformTypes of
    Left {} -> coerce (fromText @OSIT.WindowsString)
    Right {} -> coerce (fromText @OSP.PosixString)
