{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiAction(..)
  , HiMonad(..)
  ) where


import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Sequence ( Seq, fromList )
import           Control.Exception.Base (throw)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception (Exception)
import Control.Monad.IO.Class
import System.Directory (setCurrentDirectory, createDirectory, doesDirectoryExist, listDirectory)
import qualified Data.ByteString        as BString
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Map
import System.Random (randomRIO)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  | HiFunRand
  deriving (Show, Eq, Ord)


data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueList (Seq HiValue)
  | HiValueNull
  | HiValueString Text
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq, Ord)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

newtype PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO
  { runHIO :: Set HiPermission -> IO a
  }

instance Functor HIO where
  fmap :: (a -> b) -> HIO a -> HIO b
  fmap f (HIO g) = HIO $ \permissions -> fmap f (g permissions)

instance Applicative HIO where
  pure :: a -> HIO a
  pure x = HIO $ \_ -> return x

  (<*>) :: HIO (a -> b) -> HIO a -> HIO b
  (HIO f) <*> (HIO x) = HIO $ \permissions -> do
    func <- f permissions
    value <- x permissions
    return (func value)

instance Monad HIO where
  (>>=) :: HIO a -> (a -> HIO b) -> HIO b
  (HIO v) >>= f = HIO $ \permissions -> do
    result <- v permissions
    let (HIO h) = f result
    h permissions

instance MonadIO HIO where
  liftIO :: IO a -> HIO a
  liftIO ioAction = HIO $ const ioAction


class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction action = HIO $ \input_perm ->
    case action of
      HiActionWrite f content -> do
        checkPermission input_perm AllowWrite
        liftIO (BString.writeFile f content)
        return HiValueNull
      HiActionMkDir f -> do
        checkPermission input_perm AllowWrite
        liftIO (createDirectory f)
        return HiValueNull
      HiActionChDir dir -> do
        checkPermission input_perm AllowWrite
        liftIO (setCurrentDirectory dir)
        return HiValueNull
      HiActionRead f -> do
        checkPermission input_perm AllowRead
        isDir <- liftIO (doesDirectoryExist f)
        if isDir
          then do
            dirs <- liftIO (listDirectory f)
            return $ HiValueList (Data.Sequence.fromList (HiValueString . T.pack <$> dirs))
          else do
            file_content <- liftIO $ BString.readFile f
            case decodeUtf8' file_content of
              Left _        -> return (HiValueBytes file_content)
              Right content -> return (HiValueString content)
      HiActionNow -> do
        checkPermission input_perm AllowTime
        currentTime <- liftIO getCurrentTime
        return $ HiValueTime currentTime
      HiActionEcho input -> do
        checkPermission input_perm AllowWrite
        _ <- putStrLn (T.unpack input)
        return HiValueNull
      HiActionRand f s -> do
        result <- liftIO $ randomRIO (f, s)
        return (HiValueNumber (toRational result))


checkPermission :: Set HiPermission -> HiPermission -> IO ()
checkPermission permissions perm =
  if Set.member perm permissions
    then return ()
    else throw (PermissionRequired perm)
