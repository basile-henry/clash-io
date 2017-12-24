{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Chip8.Types where

import           Clash.Prelude

pattern (:%:)
  :: (KnownNat n1, KnownNat n2)
  => BitVector n1 -> BitVector n2 -> BitVector (n1 + n2)
pattern a :%: b <- (split -> (a, b))
  where a :%: b = a ++# b
infixl 6 :%:

newtype Addr =
  Addr { unAddr :: BitVector 12 } deriving (Eq, Show)
newtype Byte =
  Byte { unByte :: BitVector 8 } deriving (Eq, Show)
newtype Nibble =
  Nibble { unNibble :: BitVector 4 } deriving (Eq, Show)
newtype Reg =
  Reg { unReg :: BitVector 4 } deriving (Eq, Show)

data Instr
  = SYS Addr
  | CLS
  | RET
  | JP Addr
  | CALL Addr
  | SEB Reg Byte
  | SNEB Reg Byte
  | SE Reg Reg
  | LDB Reg Byte
  | ADDB Reg Byte
  | LD Reg Reg
  | OR Reg Reg
  | AND Reg Reg
  | XOR Reg Reg
  | ADD Reg Reg
  | SUB Reg Reg
  | SHR Reg
  | SUBN Reg Reg
  | SHL Reg
  | SNE Reg Reg
  | LDI Addr
  | JPV0 Addr
  | RND Reg Byte
  | DRW Reg Reg Nibble
  | SKP Reg
  | SKNP Reg
  | LDVDT Reg
  | LDK Reg
  | LDDTV Reg
  | LDSTV Reg
  | ADDI Reg
  | LDSprite Reg
  | LDBCD Reg
  | LDIV Reg
  | LDVI Reg
  deriving (Eq, Show)

instance BitPack Addr where
  type BitSize Addr = 12
  pack = pack . unAddr
  unpack = Addr . unpack

instance BitPack Byte where
  type BitSize Byte = 8
  pack = pack . unByte
  unpack = Byte . unpack

instance BitPack Nibble where
  type BitSize Nibble = 4
  pack = pack . unNibble
  unpack = Nibble . unpack

instance BitPack Reg where
  type BitSize Reg = 4
  pack = pack . unReg
  unpack = Reg . unpack

instance BitPack Instr where
  type BitSize Instr = 16
  pack =
    let i :: BitVector 4 -> BitVector 4
        i = id
        b :: BitVector 8 -> BitVector 8
        b = id
    in \case
      SYS addr     -> pack (i 0x0, addr)
      CLS          -> 0x00E0
      RET          -> 0x00EE
      JP addr      -> pack (i 0x1, addr)
      CALL addr    -> pack (i 0x2, addr)
      SEB vx byte  -> pack (i 0x3, vx, byte)
      SNEB vx byte -> pack (i 0x4, vx, byte)
      SE vx vy     -> pack (i 0x5, vx, vy, i 0x0)
      LDB vx byte  -> pack (i 0x6, vx, byte)
      ADDB vx byte -> pack (i 0x7, vx, byte)
      LD vx vy     -> pack (i 0x8, vx, vy, i 0x0)
      OR vx vy     -> pack (i 0x8, vx, vy, i 0x1)
      AND vx vy    -> pack (i 0x8, vx, vy, i 0x2)
      XOR vx vy    -> pack (i 0x8, vx, vy, i 0x3)
      ADD vx vy    -> pack (i 0x8, vx, vy, i 0x4)
      SUB vx vy    -> pack (i 0x8, vx, vy, i 0x5)
      SHR vx       -> pack (i 0x8, vx, i 0x0, i 0x6)
      SUBN vx vy   -> pack (i 0x8, vx, vy, i 0x7)
      SHL vx       -> pack (i 0x8, vx, i 0x0, i 0xE)
      SNE vx vy    -> pack (i 0x9, vx, vy, i 0x0)
      LDI addr     -> pack (i 0xA, addr)
      JPV0 addr    -> pack (i 0xB, addr)
      RND vx byte  -> pack (i 0xC, vx, byte)
      DRW vx vy n  -> pack (i 0xD, vx, vy, n)
      SKP vx       -> pack (i 0xE, vx, b 0x9E)
      SKNP vx      -> pack (i 0xE, vx, b 0xA1)
      LDVDT vx     -> pack (i 0xF, vx, b 0x07)
      LDK vx       -> pack (i 0xF, vx, b 0x0A)
      LDDTV vx     -> pack (i 0xF, vx, b 0x15)
      LDSTV vx     -> pack (i 0xF, vx, b 0x18)
      ADDI vx      -> pack (i 0xF, vx, b 0x1E)
      LDSprite vx  -> pack (i 0xF, vx, b 0x29)
      LDBCD vx     -> pack (i 0xF, vx, b 0x33)
      LDIV vx      -> pack (i 0xF, vx, b 0x55)
      LDVI vx      -> pack (i 0xF, vx, b 0x65)

  unpack =
    let i :: BitVector 4 -> BitVector 4
        i = id
        b :: BitVector 8 -> BitVector 8
        b = id
    in \case
      (0x0 :%: addr)                           -> SYS (Addr addr)
      0x00E0                                   -> CLS
      0x00EE                                   -> RET
      (0x1 :%: addr)                           -> JP (Addr addr)
      (0x2 :%: addr)                           -> CALL (Addr addr)
      (0x3 :%: vx :%: byte)                    -> SEB (Reg vx) (Byte byte)
      (0x4 :%: vx :%: byte)                    -> SNEB (Reg vx) (Byte byte)
      (0x5 :%: vx :%: vy :%: (i -> 0x0))       -> SE (Reg vx) (Reg vy)
      (0x6 :%: vx :%: byte)                    -> LDB (Reg vx) (Byte byte)
      (0x7 :%: vx :%: byte)                    -> ADDB (Reg vx) (Byte byte)
      (0x8 :%: vx :%: vy :%: (i -> 0x0))       -> LD (Reg vx) (Reg vy)
      (0x8 :%: vx :%: vy :%: (i -> 0x1))       -> OR (Reg vx) (Reg vy)
      (0x8 :%: vx :%: vy :%: (i -> 0x2))       -> AND (Reg vx) (Reg vy)
      (0x8 :%: vx :%: vy :%: (i -> 0x3))       -> XOR (Reg vx) (Reg vy)
      (0x8 :%: vx :%: vy :%: (i -> 0x4))       -> ADD (Reg vx) (Reg vy)
      (0x8 :%: vx :%: vy :%: (i -> 0x5))       -> SUB (Reg vx) (Reg vy)
      (0x8 :%: vx :%: (i -> _) :%: (i -> 0x6)) -> SHR (Reg vx)
      (0x8 :%: vx :%: vy :%: (i -> 0x7))       -> SUBN (Reg vx) (Reg vy)
      (0x8 :%: vx :%: (i -> _) :%: (i -> 0xE)) -> SHL (Reg vx)
      (0x9 :%: vx :%: vy :%: (i -> 0x0))       -> SNE (Reg vx) (Reg vy)
      (0xA :%: addr)                           -> LDI (Addr addr)
      (0xB :%: addr)                           -> JPV0 (Addr addr)
      (0xC :%: vx :%: byte)                    -> RND (Reg vx) (Byte byte)
      (0xD :%: vx :%: vy :%: n)                -> DRW (Reg vx) (Reg vy) (Nibble n)
      (0xE :%: vx :%: (b -> 0x9E))             -> SKP (Reg vx)
      (0xE :%: vx :%: (b -> 0xA1))             -> SKNP (Reg vx)
      (0xF :%: vx :%: (b -> 0x07))             -> LDVDT (Reg vx)
      (0xF :%: vx :%: (b -> 0x0A))             -> LDK (Reg vx)
      (0xF :%: vx :%: (b -> 0x15))             -> LDDTV (Reg vx)
      (0xF :%: vx :%: (b -> 0x18))             -> LDSTV (Reg vx)
      (0xF :%: vx :%: (b -> 0x1E))             -> ADDI (Reg vx)
      (0xF :%: vx :%: (b -> 0x29))             -> LDSprite (Reg vx)
      (0xF :%: vx :%: (b -> 0x33))             -> LDBCD (Reg vx)
      (0xF :%: vx :%: (b -> 0x55))             -> LDIV (Reg vx)
      (0xF :%: vx :%: (b -> 0x65))             -> LDVI (Reg vx)
