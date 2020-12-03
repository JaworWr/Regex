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

run :: ExceptT String IO ()
run = do
    args <- liftIO getArgs
    (pat, argsTail) <- case uncons args of
        Just r -> return r
        Nothing -> throwError "No pattern specified"
    handle <- case argsTail of
        [] -> return stdin
        (path:_) -> handleIOError $ openFile path ReadMode
    re <- handleParseError $ parse pat
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
