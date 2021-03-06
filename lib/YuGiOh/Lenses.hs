{-# LANGUAGE RankNTypes #-}

-- | A bunch of useful lenses.
module YuGiOh.Lenses
  ( currentPlayer,
    deck,
    hand,
    hasDrawnCard,
    hasNormalSummoned,
    hasSwitchedPosition,
    lifePoints,
    mainMonsterZone,
    mat,
    otherPlayer,
    phase,
    turn,
  )
where

import Control.Lens (Lens')
import YuGiOh.Duel
import qualified YuGiOh.Mat as Mat
import qualified YuGiOh.Player as Player
import YuGiOh.Space

deck :: PlayerLens -> Lens' Duel Mat.Deck
deck player = player . Player.mat . Mat.deck

hand :: PlayerLens -> Lens' Duel Player.Hand
hand player = player . Player.hand

hasDrawnCard :: PlayerLens -> Lens' Duel Bool
hasDrawnCard player = player . Player.hasDrawnCard

hasNormalSummoned :: PlayerLens -> Lens' Duel Bool
hasNormalSummoned player = player . Player.hasNormalSummoned

lifePoints :: PlayerLens -> Lens' Duel Int
lifePoints player = player . Player.lifePoints

mainMonsterZone :: PlayerLens -> Lens' Duel [ScopedSpace]
mainMonsterZone player = mat player . Mat.mainMonsterZone

mat :: PlayerLens -> Lens' Duel Mat.Mat
mat player = player . Player.mat
