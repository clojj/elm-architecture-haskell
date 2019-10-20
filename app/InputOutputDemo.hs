{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import VanillaElmArchitecture (run, Config(..), Cmd(..), getInput)


main :: IO ()
main = do
    run $ Config
        { _init = init'
        , _update = update
        , _view = view
        }

data Model = Model
    { linesEntered  :: [String]
    }


data Msg
    = InputEntered String


init' :: (Model, [ Cmd Msg ])
init' =
    ( Model { linesEntered = [] }, [ getInput InputEntered ] )


update :: Msg -> Model -> ( Model, [ Cmd Msg ] )
update msg model =
    case msg of
      InputEntered input ->
        if null input
          then ( model, [] )
          else
            let lines = input : linesEntered model
            in
              ( Model { linesEntered = lines }, [ getInput InputEntered ] )

view :: Model -> IO ()
view model =
  putStrLn $ unlines (linesEntered model)
