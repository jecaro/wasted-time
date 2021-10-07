-- | Command line options
module Options (
    Options (..),
    Error (..),
    options,
    render,
    parse,
) where

import qualified Options.Applicative as Opt

newtype Options = Options
    { optCartridge :: Text
    }
    deriving (Show)

-- | The different kinds of error
data Error = Parse Text | CompletionInvoked

-- | Run the command line parser with the command interpreter
parse :: String -> [String] -> Either Error Options
parse progName args =
    case Opt.execParserPure Opt.defaultPrefs opts args of
        Opt.Success o -> Right o
        Opt.Failure help ->
            Left . Parse . toText . fst $ Opt.renderFailure help progName
        Opt.CompletionInvoked _ -> Left CompletionInvoked
  where
    opts = Opt.info (Opt.helper <*> options) Opt.idm

-- | Get a readable message out of the 'Error'
render :: Error -> Text
render (Parse msg) = msg
render CompletionInvoked = "Completion is not handled"

-- | Command line parser
options :: Opt.Parser Options
options = Options <$> Opt.strOption (Opt.long "cartridge" <> Opt.metavar "CART")
