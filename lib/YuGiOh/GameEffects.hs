{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module YuGiOh.GameEffects
  ( chooseTribute,
  )
where

import Data.String.Interpolate
import Polysemy (Member, Sem)
import Polysemy.Embed
import Polysemy.State (State, get)
import YuGiOh.ChooseOption
import YuGiOh.Classes.Displayable
import YuGiOh.Duel
import YuGiOh.Space

chooseTribute ::
  Member ChooseOption e =>
  Member (Embed IO) e =>
  Member (State Duel) e =>
  [MonsterSpace] ->
  Sem e MonsterSpace
chooseTribute tributes = do
  duel <- get
  embed $ putStrLn $ "Choose tribute: " ++ show (display <$> tributes)
  chooseOption duel displayTribute tributes
  where
    displayTribute :: MonsterSpace -> String
    displayTribute monster = [i|Tribute #{display monster}|]
