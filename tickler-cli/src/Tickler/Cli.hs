module Tickler.Cli
    ( ticklerCli
    , dispatch
    ) where

import Import

import Tickler.Cli.Commands.Add
import Tickler.Cli.Commands.Done
import Tickler.Cli.Commands.Login
import Tickler.Cli.Commands.Logout
import Tickler.Cli.Commands.Register
import Tickler.Cli.Commands.Review
import Tickler.Cli.Commands.Show
import Tickler.Cli.Commands.Size
import Tickler.Cli.Commands.Sync
import Tickler.Cli.OptParse

ticklerCli :: IO ()
ticklerCli = do
    Instructions disp sett <- getInstructions
    runReaderT (dispatch disp) sett

dispatch :: Dispatch -> CliM ()
dispatch d =
    case d of
        DispatchRegister rs -> register rs
        DispatchLogin ls -> login ls
        DispatchPostPostAddItem t -> addItem t
        DispatchShowItem -> showItem
        DispatchDoneItem -> doneItem
        DispatchSize -> size
        DispatchReview -> review
        DispatchLogout -> logout
        DispatchSync -> sync
