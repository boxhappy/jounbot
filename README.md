TODO:
    - Finish all the functions
        - Translate
            - New Microsoft App ID fixes this.
        - Dictionary
        - Variable
            - Global variables
            - List a user's variables.
            - Better `Remove' syntax.
            - Better syntax in general.
                - `.$ Reminder var contents here`
                - `.$ var contents here`
                - Get out, `=' character!
        - Calc
            - Needs to be rewritten.
        - Help
            - Make it print whether the function/operator is black/whitelisted
              or not.
        - AniDB
        - Lambdas ?
            - .\ x y => .an:x y
            - Can be stored in .$ variables
            - Can be used with planned parens
        - Diff
            - f x y = filter (not . (`elem` y)) x
        - Lewd
            - Add an argument, how many lewds to print.
              No more `.lewd ++ .lewd ++ .lewd ++ .lewd`!
    - Add command line arguments, such as making it not autojoin any
      servers/channels and instead only join ones specified by a certain
      parameter.
    - Core data needs to be parsed, such as `getservers'.
    - Add `owner' to Server data.
        - Be able to control KawaiiBot through ownership
            - Quit servers/channels
            - Join Servers/channels
            - Other stuff
    - Add operator escaping or make certain functions ignore operators.
    - Add parens for the functions/operators
        - `.lewd -> (.ra 100 -> .sed s/matcher/replacer/)`
    - Add timeouts between usage of functions that can be customized per channel
      in Config.hs.
        - Per channel and user or just channel timeout?
    - Let `events' store data for later use.
        - Make event functions loop in their own threads and pass data to each
          other. This will allow functions such as `diff' to work properly.
            - Even functions that output the same to all channels.
            - Odd functions that run the event function for each channel.
    - Add more lewdness
        - pls giv ideas
    - Remove unnecessary modules.
    - Customizable function prefix.
    - Add data storing for functions somewhere.
        - Use MVar ?
- Switch from `Network.Curl' to `Network.HTTP'.
- Find a new weather API.
- Use Parsec or Attoparsec to parse the history.
-}

module Main where


import Config

import KawaiiBot.Bot
import KawaiiBot.IRC
import KawaiiBot.Types
import KawaiiBot.Utils

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)
import System.IO
import System.Process


printVersion :: IO ()
printVersion = putStrLn botversion

-- Opens the config file for reading and passes the data to concurrent
-- serverConnect methods.
main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then checkArg args
        else run serverConnect
  where checkArg x  | head x == "--version" = printVersion
                    | head x == "-v" = printVersion
                    | head x == "--test" = run mainTest
                    | otherwise = run serverConnect
        run f = runReaderT f metaConfig
        metaConfig = MetaConfig { getMeta = emptyMeta
                                , getConfig = config
                                }

mainTest :: Memory ()
mainTest = forever $ do
    line <- liftIO getLine
    let msg = "localhost:Owner!Owner@home PRIVMSG #kawaiibot :" ++ line
    parseIRC stdout msg
  where meta = Meta dest nick name host chans server temp
        dest = "#KawaiiBot"
        nick = "Owner"
        name = "Owner"
        host = "localhost"
        userlist = [('~', "Owner"), ('&', "KawaiiBot")]
        chans = ["#KawaiiBot"]
        server = "localhost"
        temp = []
