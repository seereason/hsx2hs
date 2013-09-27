{-# LANGUAGE CPP #-}
module Main where

#ifdef BASE4
import Control.OldException           (handle,ErrorCall(..))
#else
import Control.Exception              (handle,ErrorCall(..))
#endif
import Data.List                      (isPrefixOf)
import Prelude                        hiding (readFile, writeFile)
import Language.Haskell.Exts          hiding (parse)
import Language.Haskell.HSX.Transform (transform)
import System.Exit                    (exitFailure)
import System.Environment             (getArgs)
import System.IO.UTF8                 (readFile, writeFile,hPutStrLn)
import System.IO                      (stderr)

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc {srcFilename=srcFilename,srcLine=srcLine,srcColumn=srcColumn}) =
  srcFilename ++ ":" ++ show srcLine ++ ":" ++ show srcColumn

checkParse :: ParseResult b -> b
checkParse p = case p of
                  ParseOk m -> m
                  ParseFailed loc s -> error $ showSrcLoc loc ++ ": " ++ s

transformFile :: String -> String -> String -> IO ()
transformFile origfile infile outfile = do
        f <- readFile infile
        let fm = process origfile f
        writeFile outfile fm

testFile :: String -> IO ()
testFile file = do
        f <- readFile file
        putStrLn $ process file f

testTransform :: String -> IO ()
testTransform file = do
        f <- readFile file
        putStrLn $ show $ transform $ checkParse $ parse file f

testPretty :: String -> IO ()
testPretty file = do
        f <- readFile file
        putStrLn $ prettyPrint $ checkParse $ parse file f

testParse :: String -> IO ()
testParse file = do
        f <- readFile file
        putStrLn $ show $ parse file f

main :: IO ()
main = do args <- getArgs
          handle (\(ErrorCall text) -> hPutStrLn stderr text >> exitFailure ) $
           case args of
            [origfile, infile, outfile] -> transformFile origfile infile outfile
            [infile, outfile] -> transformFile infile infile outfile
            [infile] -> testFile infile
            _ -> putStrLn usageString

process :: FilePath -> String -> String
process fp fc = prettyPrintWithMode (defaultMode {linePragmas=True}) $
                 transform $ checkParse $ parse fp fc

parse :: String -> String -> ParseResult Module
parse fn fc = parseModuleWithMode (ParseMode fn Haskell2010 allExtensions False True (Just baseFixities)) fcuc
  where fcuc= unlines $ filter (not . isPrefixOf "#") $ lines fc

usageString :: String
usageString = "Usage: hsx2hs <infile> [<outfile>]"

allExtensions :: [Extension]
allExtensions = map EnableExtension
                   [RecursiveDo,ParallelListComp,MultiParamTypeClasses,FunctionalDependencies,RankNTypes,ExistentialQuantification,
                    ScopedTypeVariables,ImplicitParams,FlexibleContexts,FlexibleInstances,EmptyDataDecls,KindSignatures,
                    BangPatterns,TemplateHaskell,ForeignFunctionInterface,Arrows,Generics,NamedFieldPuns,PatternGuards,
                    MagicHash,TypeFamilies,StandaloneDeriving,TypeOperators,RecordWildCards,GADTs,UnboxedTuples,
                    PackageImports,QuasiQuotes,TransformListComp,ViewPatterns,XmlSyntax,RegularPatterns]
