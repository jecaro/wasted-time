module Main where

import Cartridge (bankCount, hasRightSize, mapper, zone)
import Control.Exception (IOException)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT)
import qualified Data.ByteString as B
import Data.Text.IO (hPutStrLn)

import qualified Options
import System.Environment (getArgs, getProgName)

data Error
    = EOptions Options.Error
    | EReadFile Text IOException

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    -- Execute the program
    res <- runExceptT $ do
        Options.Options{..} <-
            firstExceptT EOptions . hoistEither $ Options.parse progName args
        bs <-
            handleIOExceptT (EReadFile optCartridge)
                . readFileBS
                $ toString optCartridge
        -- Read the cartridge
        print $ B.length bs
        print $ hasRightSize bs
        print $ zone bs
        print . bankCount $ B.length bs
        print . mapper $ B.length bs
    -- Handle error
    whenLeft_ res $ \e -> do
        hPutStrLn stderr $ render e
        exitFailure

-- | Render the error message
render :: Error -> Text
render (EOptions eOptions) = Options.render eOptions
render (EReadFile filename e) =
    "Error reading '" <> filename <> "' : " <> show e
