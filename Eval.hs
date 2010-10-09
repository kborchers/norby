module Eval where

import           Control.Monad.Reader
import           Data.Char
import           Data.List

import           Mueval.ArgsParse
import           Mueval.Interpreter
import           Mueval.Parallel

import qualified Language.Haskell.Interpreter as I

import           System.IO.Error
import           System.Process
import           Types
import           Utils

hsFile = "L"

-- Call out to the mueval binary
evalHsExt :: Message -> IO String
evalHsExt (Message _ _ params) = do
    (_, out, _) <- liftIO $ readProcessWithExitCode "mueval" args ""
    return $ "  " ++ (unwords $ words out)
    where args  = ["-XExtendedDefaultRules",
                   "-XUnicodeSyntax",
                   "--noimports",
                   "-l", hsFile ++ ".hs",
                   "--expression=" ++ (drop 2 . last) params,
                   "-t30",
                   "+RTS", "-N2", "-RTS"]

-- Evaluate a Haskell expression
evalHs :: String -> IO String
evalHs expr = do
    r <- liftIO . I.runInterpreter . interpreter $ muOptions { expression = expr }
    case r of
           Left  (I.WontCompile errs) -> return $ niceErrors errs
           Left  err                  -> error  $ intercalate " " . lines $ show err
           -- Expr, type, and result
           Right (_, _, r)            -> return $ "  " ++ r
    
    where muOptions = (getOptions []) { expression = expr,
                                        loadFile   = hsFile,
                                        timeLimit  = 5 }

-- Get inferred type of an expression
typeOf :: Message -> IO String
typeOf (Message _ _ params) = do
       -- ".type expr" -> "expr"
       let expr = drop 6 $ last params
       t <- liftIO . I.runInterpreter $ I.loadModules [hsFile]
                                     >> I.setTopLevelModules [hsFile]
                                     >> I.setImports ["Prelude"]
                                     >> I.typeOf expr
       case t of
            Left  (I.WontCompile errs) -> return $ niceErrors errs
            Left  err                  -> return $ show err
            Right val                  -> return val

niceErrors = excerpt' . intercalate " " . concatMap lines . fmap I.errMsg

-- Pointfree refactoring
pointFree :: Message -> IO String
pointFree = pointy "pointfree"

pointFul :: Message -> IO String
pointFul = pointy "pointful"

pointy p (Message _ _ params) = do
    let expr = trim . dropWhile (not . isSpace) $ last params
    (_, out, _) <- liftIO $ readProcessWithExitCode p [expr] ""
    return . intercalate " " $ lines out
