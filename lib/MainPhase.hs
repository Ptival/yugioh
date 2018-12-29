{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the main phase, a player may choose to summon a monster.

module MainPhase (
  mainPhase,
  ) where

import           Control.Eff     (Eff)
import           Control.Lens    (view)
import           Control.Monad   (replicateM_)

import           Card
import qualified Lenses          as L
import           Move
import           Operation
import           Phase
import           Position
import           Prelude         hiding (log)
import qualified Space           as S
import           Victory

tributeFor :: Int -> Int
tributeFor n | n >= 5 && n <= 6 = 1
             | n >= 7 && n <= 8 = 2
             | n >= 9           = 3
             | otherwise        = error "Only monsters above level 4 can be tribute summoned"

validMoves ::
  Operations e =>
  Eff e [Move 'Main]
validMoves = do
  currentPlayerHand              <- getHand              L.currentPlayer
  currentPlayerHasNormalSummoned <- getHasNormalSummoned L.currentPlayer
  currentPlayerMainMonsterZone   <- getMainMonsterZone   L.currentPlayer

  let normalSummonMoves =
        if not currentPlayerHasNormalSummoned
           && any S.isEmpty currentPlayerMainMonsterZone
        then [ NormalSummon card position
             | card <- currentPlayerHand
             , view level card <= 4
             , position <- [ Position.Attack, FaceDownDefense ]
             ]
        else []

  let tributeSummonMoves =
        if not currentPlayerHasNormalSummoned
        then [ TributeSummon card position
             | card <- currentPlayerHand
             , let cardLevel = view level card
             , cardLevel > 4
             , length (S.monsterSpaces currentPlayerMainMonsterZone) >= tributeFor cardLevel
             , position <- [ Position.Attack, FaceDownDefense ]
             ]
        else []

  let switchPositionMoves =
        [ SwitchPosition monster
        | monster <- S.monsterSpaces currentPlayerMainMonsterZone
        , not $ view L.hasSwitchedPosition monster
        ]

  return $
    [ Move.EndMainPhase, Move.EndTurn ]
    ++ normalSummonMoves
    ++ tributeSummonMoves
    ++ switchPositionMoves

mainPhase ::
  Operations e =>
  Eff e (Maybe Victory)
mainPhase = validMoves >>= chooseMove >>= \case

  Move.EndMainPhase -> do
    enterBattlePhase
    return Nothing

  Move.EndTurn -> do
    endTurn
    return Nothing

  Move.NormalSummon card position -> do
    summonMonster L.currentPlayer card position
    return Nothing

  Move.TributeSummon card position -> do
    let cardLevel        = view level card
    let requiredTributes = tributeFor cardLevel
    replicateM_ requiredTributes $ tributeMonster L.currentPlayer
    summonMonster L.currentPlayer card position
    return Nothing

  Move.SwitchPosition monster -> do
    Operation.switchPosition L.currentPlayer monster
    return Nothing
