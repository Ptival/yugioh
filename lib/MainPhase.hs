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
import           Data.List       (delete, findIndex)
import           Data.List.Index (modifyAt)

import           Card
import           EndPhase
import           GameEffects
import qualified Lenses          as L
import           Log
import           Move
import           Phase
import           Position
import           Prelude         hiding (log)
import           Space
import           Utils
import           Victory

validMoves :: ( GameEffects e ) => Eff e [Move 'Main]
validMoves = do
  currentPlayerHasNormalSummoned <- getLensed L.currentPlayerHasNormalSummoned
  currentPlayerMainMonsterZone   <- getLensed L.currentPlayerMainMonsterZone
  currentPlayerHand              <- getLensed L.currentPlayerHand

  let normalSummonMoves =
        if not currentPlayerHasNormalSummoned
           && any isEmpty currentPlayerMainMonsterZone
        then [ NormalSummon card position
             | card <- currentPlayerHand
             , view level card <= 4
             , position <- [ Position.Attack, FaceDownDefense ]
             ]
        else []

  let switchPositionMoves =
        [ SwitchPosition monster
        | monster <- monsterSpaces currentPlayerMainMonsterZone
        , not $ view hasSwitchedPosition monster
        ]

  return $
    [ Move.EndMainPhase, Move.EndTurn ]
    ++ normalSummonMoves
    ++ switchPositionMoves

mainPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
mainPhase = validMoves >>= GameEffects.chooseMove >>= \case

  Move.EndMainPhase -> do
    setLensed L.phase Battle
    log Log.EndMainPhase
    return Nothing

  Move.EndTurn -> endTurn

  Move.NormalSummon card position -> do
    currentPlayer                <- getLensed L.currentPlayer
    currentPlayerMainMonsterZone <- getLensed L.currentPlayerMainMonsterZone

    overLensed L.currentPlayerHand $ delete card
    monsterSpace <- summonMonster card position
    case findIndex isEmpty currentPlayerMainMonsterZone of
      Nothing -> fail "Could not find a space on the mat to summon the card"
      Just index -> do
        overLensed L.currentPlayerMainMonsterZone
          $ modifyAt index
          $ const $ ScopedSpace monsterSpace
        log $ NormalSummoned currentPlayer monsterSpace index
    setLensed L.currentPlayerHasNormalSummoned $ True

    return Nothing

  Move.SwitchPosition monsterSpace -> do
    currentPlayer                <- getLensed L.currentPlayer
    currentPlayerMainMonsterZone <- getLensed L.currentPlayerMainMonsterZone

    let targetIdentifier = view identifier monsterSpace

    let isTarget s       = view identifier s == targetIdentifier
    let monsters         = filterMonsterCards currentPlayerMainMonsterZone
    case findIndex isTarget monsters of
      Nothing -> fail "Could not find the monster whose position to switch"
      Just index -> do
        overLensed L.currentPlayerMainMonsterZone
          $ modifyAt index (whenMonster switchPosition)
        newMainMonsterZone <- getLensed L.currentPlayerMainMonsterZone
        let newMonsterSpace = filterMonsterCards newMainMonsterZone !! index
        log $ SwitchedPosition currentPlayer newMonsterSpace index
    return Nothing
