{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad

import Control.Concurrent
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import DBus
import DBus.Client
import System.Directory(doesDirectoryExist, executable, getDirectoryContents, getPermissions)
import System.Environment(lookupEnv)
import System.Exit(ExitCode(..))
import System.FilePath((</>))
import System.Process(readProcessWithExitCode)

callDmenu :: String -> [String] -> IO (Maybe String)
callDmenu title options = do
   (code, out, _) <- readProcessWithExitCode "/usr/bin/dmenu" ["-b", "-p", title] (unlines options)
   case code of
    ExitSuccess -> return $ Just $ take (length out - 1) out
    _ -> return Nothing

doCall :: String -> [String] -> IO (Maybe String)
doCall = callDmenu

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split x (y : ys)
 | x == y = [] : split x ys
 | z : zs <- split x ys = (y : z) : zs
 | otherwise = [[y]]

populateFromPath :: IO (Map String String)
populateFromPath = do
   mpath <- lookupEnv "PATH"
   case mpath of
    Nothing -> return Map.empty
    Just path -> (Map.unions . map Map.fromList) <$> forM (split ':' path) go
 where go dir = do
          exists <- doesDirectoryExist dir
          if exists then do
             files <- filterM (isExecutable dir) =<< getDirectoryContents dir
             return [(f, dir </> f) | f <- files]
          else return []
       isExecutable dir file = do
          res <- getPermissions $ dir </> file
          return $ executable res

doCallWithPath :: String -> IO (Maybe String)
doCallWithPath title = do
   progs <- populateFromPath
   res <- doCall title (Map.keys progs)
   return $ flip Map.lookup progs =<< res

dbusWrapperCall :: MethodCall -> IO Reply
dbusWrapperCall mcall
 | vtitle:vargs:_ <- methodCallBody mcall
 , Just title <- fromVariant vtitle
 , Just args <- fromVariant vargs
 = do
    res <- doCall title args
    case res of
     Nothing -> return $ replyReturn [toVariant (False, "" :: String)]
     Just r -> return $ replyReturn [toVariant (True, r)]
 | otherwise = return $ replyReturn [toVariant (False, "" :: String)]

dbusWrapperCallWithPath :: MethodCall -> IO Reply
dbusWrapperCallWithPath mcall
 | vtitle:_ <- methodCallBody mcall
 , Just title <- fromVariant vtitle
 = do
    res <- doCallWithPath title
    case res of
     Nothing -> return $ replyReturn [toVariant (False, "" :: String)]
     Just r -> return $ replyReturn [toVariant (True, r)]
 | otherwise = return $ replyReturn [toVariant (False, "" :: String)]

main :: IO ()
main = do
   client <- connectSession
   void $ requestName client "org.hsmenu.Control" [nameDoNotQueue]
   export client "/org/hsmenu/Control"
      [ method "org.hsmenu.Control" "Call"
         (signature_ [TypeString, TypeArray TypeString])
         (signature_ [TypeStructure [TypeBoolean, TypeString]])
         dbusWrapperCall
      , method "org.hsmenu.Control" "CallWithPath"
         (signature_ [TypeString])
         (signature_ [TypeStructure [TypeBoolean, TypeString]])
         dbusWrapperCallWithPath
      ]
   forever $ threadDelay maxBound
