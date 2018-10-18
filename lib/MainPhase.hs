{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the main phase, a player may choose to summon a monster.

module MainPhase (
  mainPhase,
  ) where

import           Control.Eff
import           Control.Lens
import           Data.List

import           Card
import           EndPhase
import           GameEffects
import qualified Lenses       as L
import           Log
import           Mat
import           Move
import           Phase
import           Prelude      hiding (log)
import           Space
import           Utils
import           Victory
import           Zipper

validMoves :: ( GameEffects e ) => Eff e [Move 'Main]
validMoves = do
  currentPlayerHasNormalSummoned <- getLensed L.currentPlayerHasNormalSummoned
  currentPlayerMainMonsterZone   <- getLensed L.currentPlayerMainMonsterZone
  currentPlayerHand              <- getLensed L.currentPlayerHand

  let normalSummonMoves =
        if not currentPlayerHasNormalSummoned
           && any isEmpty currentPlayerMainMonsterZone
        then [ NormalSummon zipper position
             | zipper <- allZippers currentPlayerHand
             , let monster = view cursor zipper
             , view level monster <= 4
             , position <- [ Attack, FaceDownDefense ]
             ]
        else []

  let switchPositionMoves =
        [ SwitchPosition zipper
        | zipper <- monsterZippers currentPlayerMainMonsterZone
        , let monster = view cursor zipper
        , not $ view hasSwitchedPosition monster
        ]

  return $
    [ Move.EndMainPhase ]
    ++ normalSummonMoves
    ++ switchPositionMoves

mainPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
mainPhase = validMoves >>= GameEffects.chooseMove >>= \case

  Move.EndMainPhase -> do
    setLensed L.phase End
    log Log.EndMainPhase
    return Nothing

  Move.EndTurn -> endTurn

  Move.NormalSummon handZipper position -> do
    currentPlayer                <- getLensed L.currentPlayer
    currentPlayerMainMonsterZone <- getLensed L.currentPlayerMainMonsterZone

    let card           = view cursor handZipper
    let cardToSummon   = summonMonster card position
    let restOfHand     = toListWithCursorDeleted handZipper
    let summonZipper   =
          case anyEmptyZipper currentPlayerMainMonsterZone of
          Nothing     -> error "Tried to normal summon, but no empty space"
          Just zipper -> zipper
    let summonedZipper = set cursor cardToSummon summonZipper

    setLensed L.currentPlayerMainMonsterZone   $ monsterZipperToList summonedZipper
    setLensed L.currentPlayerHand              $ restOfHand
    setLensed L.currentPlayerHasNormalSummoned $ True

    log $ NormalSummoned currentPlayer summonedZipper
    return Nothing

  Move.SwitchPosition monsterZipper -> do
    currentPlayer                <- getLensed L.currentPlayer

    let monsterZipper'     = over cursor switchPosition monsterZipper
    let newMainMonsterZone = monsterZipperToList monsterZipper'

    setLensed L.currentPlayerMainMonsterZone newMainMonsterZone

    log $ SwitchedPosition currentPlayer monsterZipper'
    return Nothing
