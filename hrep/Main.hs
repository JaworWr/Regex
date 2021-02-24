{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

import System.Environment
import System.IO
import System.IO.Error
import Rainbow
import Data.List
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Except
import System.Console.GetOpt

import Regex

data MatchingPart = Matched T.Text | Unmatched T.Text deriving (Eq, Show)

printMatchings :: String -> [Matching] -> IO ()
printMatchings line = mapM_ printMatching . toMatchingParts 0 line where
    toMatchingParts _ s [] = [Unmatched (T.pack s) | not (null s)]
    toMatchingParts !pos s (m:ms) =
        let (p1, s1) = splitAt (matchingStart m - pos) s
            (p2, s2) = splitAt (matchingLen m) s1
        in [Unmatched (T.pack p1) | not (null p1)] ++ 
            [Matched (T.pack p2) | not (null p2)] ++ 
            toMatchingParts (matchingEnd m) s2 ms
    printMatching (Matched s) = putChunk . bold . fore red . chunk $ s
    printMatching (Unmatched s) = putChunk . chunk $ s

handleIOError = withExceptT show . ExceptT . tryIOError
handleParseError = withExceptT prettyError . liftEither

data Options = Options {
    optIgnoreCase :: Bool,
    optHelp :: Bool,
    optPattern :: Maybe String,
    optFile :: Maybe String
}

defaultOptions = Options {
    optIgnoreCase = False,
    optHelp = False,
    optPattern = Nothing,
    optFile = Nothing
}

header = "Usage: hrep [OPTION...] pattern [file]"

options = [
    Option "i" ["ignore-case"] (NoArg $ \opts -> opts { optIgnoreCase = True }) "Ignore the letter case",
    Option "h" ["help"] (NoArg $ \opts -> opts {optHelp = True}) "Show this help message"
    ]

noPatternError = "no pattern specified\n" ++ usageInfo header options

parseArgs :: ExceptT String IO Options
parseArgs = do
    argv <- liftIO getArgs
    case getOpt RequireOrder options argv of
        (o, args, []) -> do
            let opts = case args of
                    [] -> defaultOptions
                    [p] -> defaultOptions { optPattern = Just p }
                    (p:f:_) -> defaultOptions { optPattern = Just p, optFile = Just f }
            return $ foldl (flip ($)) opts o
        (_, _, errs) -> throwError $ concat errs ++ usageInfo header options

run :: ExceptT String IO ()
run = do
    opts <- parseArgs
    if optHelp opts
        then liftIO . putStrLn $ usageInfo header options 
        else do
        handle <- maybe (return stdin) (\path -> handleIOError $ openFile path ReadMode) $
            optFile opts
        re0 <- maybe (throwError noPatternError) (handleParseError . parse) $ optPattern opts
        let re = if optIgnoreCase opts then ignoreCaseRegex re0 else re0
        fContents <- handleIOError $ lines <$> hGetContents handle
        forM_ fContents $ \line -> liftIO $ do
            let matchings = findAllRe re line
            unless (null matchings) $ do
                printMatchings line matchings
                putStrLn ""

main :: IO ()
main = runExceptT run >>= \case
    Left err -> putStrLn err
    Right _ -> return ()
