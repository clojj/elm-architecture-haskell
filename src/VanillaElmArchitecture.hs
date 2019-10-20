{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VanillaElmArchitecture (run , Config(..), Cmd, getInput) where

import           System.Exit              (exitSuccess)
import           System.Console.ANSI      (clearScreen)

data Cmd msg = InputCmd (String -> msg)

getInput :: (String -> msg) -> Cmd msg 
getInput msg = 
  InputCmd msg

data Config model msg = Config
    { _init   :: (model, [ Cmd msg ])
    , _update :: msg -> model -> (model, [ Cmd msg  ])
    , _view   :: model -> IO ()
    }

run :: forall model msg. Config model msg -> IO ()
run config = do
    run' initCmds initModel
    where
        (initModel, initCmds) = _init config

        update' = _update config
        view'   = _view config

        run' :: [ Cmd msg ] -> model -> IO ()
        run' cmds model = do
            if null cmds
            then do
                print "Finished"
                exitSuccess
            else
                let cmd : tailCmds = cmds
                in
                case cmd of
                    InputCmd msg -> do
                        line <- getLine
                        let (newModel, newCmds) = update' (msg line) model
                        clearScreen
                        view' newModel
                        run' (newCmds ++ tailCmds) newModel
