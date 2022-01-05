{-# OPTIONS_GHC -fno-warn-tabs #-}

module PVS2BSV where

import LexerTypes

-- | converts a PVS package to a BSV package for the purposes of the pvs to bsv compilation process.
-- pvs2bsv :: PVSPackage -> BSVPackage
-- pvs2bsv = ...

p2bConstDecl :: PVSConstantDec -> BSVConstantDec  
p2bConstDecl (nom, typ, lit) = (nom, (p2bType typ), lit)

p2bTypeDef :: PVSTypeDef -> BSVTypeDef
p2bTypeDef (PVS_Synonym nom typ) = (BSV_Synonym nom (p2bType typ))
p2bTypeDef (PVS_Enumeration nom enums) = (BSV_Enumeration nom enums)

-- | Converts a lexer-bound PVS type to a lexer-bound BSV type.  PVS interprets Bit types as Int.  As such, There is no way to generate a bit type from a pvs file.  
p2bType :: PVSType -> BSVType
p2bType (PVS_Bool) = (BSV_Bool)
p2bType (PVS_Bit n) = (BSV_Bit n)
p2bType (PVS_Int n) = (BSV_Int n)
p2bType (PVS_UInt n) = (BSV_UInt n)
p2bType (PVS_Real) = (BSV_Real)
p2bType (PVS_Custom n) = (BSV_Custom n)

