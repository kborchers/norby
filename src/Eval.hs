module Eval where

-- import Control.Monad.Reader
import Data.Char
import Data.List
-- import Mueval.ArgsParse
-- import Mueval.Context (defaultModules)
-- import Mueval.Interpreter

import Language.Haskell.Interpreter as I

import System.Process
import Types
import Utils

hsFile = "L" -- A bit dumb, relies on the current working directory

-- Call out to the mueval binary
evalHsExt :: Message -> IO String
evalHsExt (Message _ _ params) = do
    (_, out, err) <- liftIO $ readProcessWithExitCode "mueval" args ""
    return $ "  " ++ (unwords $ words out ++ words err)
    where args  = [ "-XExtendedDefaultRules"
                  , "--no-imports"
                  , "-l", hsFile ++ ".hs"
                  , "--expression=" ++ (drop 2 . last) params
                  , "-t30" ]

-- Get inferred type of an expression
typeOf :: Message -> IO String
typeOf (Message _ _ params) = do
       -- ".type expr" -> "expr"
       let expr = drop 6 $ last params
       t <- liftIO . I.runInterpreter
                   $ I.loadModules [hsFile]
                  >> I.setTopLevelModules [hsFile]
                  >> I.setImports ["Prelude"]
                  >> I.typeOf expr
       case t of
            Left  (I.WontCompile errs) -> return $ niceErrors errs
            Left  err                  -> return $ show err
            Right val                  -> return val

niceErrors = excerpt' . intercalate " " . concatMap lines . fmap I.errMsg

-- Pointfree refactoring
pointy :: FilePath -> Message -> IO String
pointy p (Message _ _ params) = do
    let expr = trim . dropWhile (not . isSpace) $ last params
    (_, out, _) <- liftIO $ readProcessWithExitCode p [expr] ""
    return . intercalate " " $ lines out

pointFree = pointy "pointfree"
pointFul  = pointy "pointful"
