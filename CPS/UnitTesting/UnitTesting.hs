import System.Directory
import System.FilePath
import System.Environment
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Control.Applicative
import Data.Traversable
import System.Console.ANSI
import System.Process
import System.Exit
import Control.Exception
import Control.Monad
import System.IO.Error

data TestResult
  = Success
  | Failure String
  | Multiple (Map.Map String (IO TestResult))

runMultiple :: FilePath -> FilePath -> [String] -> IO TestResult
runMultiple ehc path names =
  do
    let paths = map (path </>) names
    let results = map (runTest ehc) paths
    return (Multiple (Map.fromList (zip names results)))

runMbMultiple :: FilePath -> FilePath -> IO (Maybe TestResult)
runMbMultiple ehc path = 
  do
    let indexFile = path </> "index.txt"
    indexExists <- doesFileExist indexFile
    if indexExists
      then
        do
          names <- fmap lines (readFile indexFile)
          fmap Just (runMultiple ehc path names)
      else
        return Nothing

tryRemoveFile :: FilePath -> IO ()
tryRemoveFile fname =
  do
    tryJust (guard . isDoesNotExistError) (removeFile fname)
    return ()

runSingle :: FilePath -> FilePath -> String -> IO TestResult
runSingle ehc fname expected =
  do
    let fnameIl = replaceExtension fname ".il"
    let fnameExe = replaceExtension fname ".exe"
    let fnameOutput = replaceExtension fname ".output"
    let fnameError = replaceExtension fname ".error"
    tryRemoveFile fnameIl
    tryRemoveFile fnameExe
    tryRemoveFile fnameOutput
    tryRemoveFile fnameError
    (compileCode,_,compileError) <- readProcessWithExitCode ehc ["-t","mscil","--verbose",fname] ""
    if (compileCode /= ExitSuccess)
      then return (Failure compileError)
      else do
        (progCode,progOutput,progError) <- readProcessWithExitCode "mono" [fnameExe] ""
        writeFile fnameOutput progOutput
        writeFile fnameError progError
        if (progCode /= ExitSuccess)
          then return (Failure "Program failed to run")
          else
            if (progOutput == expected)
              then return Success
              else return (Failure "Output does not match")

runMbSingle :: FilePath -> FilePath -> IO (Maybe TestResult)
runMbSingle ehc path =
  do
    let name = takeFileName path
    let hsFile = path </> name <.> ".hs"
    let expectedFile = path </> name <.> ".expected"
    hsFileExists <- doesFileExist hsFile
    if (not hsFileExists)
      then return Nothing
      else
        do
          expectedContent <- tryJust (guard . isDoesNotExistError) (readFile expectedFile)
          case expectedContent of
            Left _ -> return (Just (Failure (expectedFile ++ " not found")))
            Right x -> fmap Just (runSingle ehc hsFile x)

runTest :: FilePath -> FilePath -> IO TestResult
runTest ehc path =
  do
    multiResult <- runMbMultiple ehc path
    singleResult <- runMbSingle ehc path
    return (fromMaybe (Failure "No unit test found") (multiResult <|> singleResult))

alignResult :: Int
alignResult = 40

padRight :: String -> Int -> String
padRight s n = take (max (length s) n) (s ++ repeat ' ')

printResult :: String -> String -> TestResult -> IO ()
printResult indent name Success =
  do
    putStr (padRight (indent ++ name ++ ": ") alignResult)
    setSGR [SetColor Foreground Vivid Green]
    putStr "OK"
    setSGR [Reset]
    putStrLn ""
printResult indent name (Failure msg) =
  do
    putStr (padRight (indent ++ name ++ ": ") alignResult)
    setSGR [SetColor Foreground Vivid Red]
    putStr "FAIL"
    setSGR [Reset]
    putStrLn ""
    putStrLn (indent ++ "  " ++ msg)
printResult indent name (Multiple m) =
  do
    putStrLn (indent ++ name ++ ":")
    forM (Map.toList m)
      (\(name, result) -> do
        res <- result
        printResult (indent ++ "  ") name res
      )
    return ()

main =
  do
    args <- getArgs
    if (length args < 2)
      then putStrLn "Usage: UnitTesting <ehc> <directory>"
      else
        do
          let ehc = args !! 0
          let dir = args !! 1
          result <- runTest ehc dir
          printResult "" dir result

