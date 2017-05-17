------------------------------------------------------------------------------
--- This library contains a version of FlatCurry's abstract syntax tree which
--- can be annotated with arbitrary information due to a polymorphic type
--- parameter.
--- For instance, this could be used to annotate function declarations
--- and expressions with their corresponding type.
---
--- For more information about the abstract syntax tree of `FlatCurry`,
--- see the documentation of the respective module.
---
--- @author  Jonas Oberschweiber, Bjoern Peemoeller, Michael Hanus
--- @version October 2015
--- @category meta
------------------------------------------------------------------------------

module FlatCurry.Annotated.Types
  ( module FlatCurry.Annotated.Types
  , module FlatCurry.Types
  ) where

import FlatCurry.Types ( QName, VarIndex, Visibility (..), TVarIndex
                       , TypeDecl (..), OpDecl (..), Fixity (..)
                       , TypeExpr (..), ConsDecl (..)
                       , Literal (..), CombType (..), CaseType (..)
                       )

--- Annotated FlatCurry program (corresponds to a module)
data AProg a = AProg String [String] [TypeDecl] [AFuncDecl a] [OpDecl]

--- Arity of a function declaration
type Arity = Int

--- Annotated function declaration
data AFuncDecl a = AFunc QName Arity Visibility TypeExpr (ARule a)

--- Annotated function rule
data ARule a
  = ARule     a [(VarIndex, a)] (AExpr a)
  | AExternal a String

--- Annotated expression
data AExpr a
  = AVar   a VarIndex
  | ALit   a Literal
  | AComb  a CombType (QName, a) [AExpr a]
  | ALet   a [((VarIndex, a), AExpr a)] (AExpr a)
  | AFree  a [(VarIndex, a)] (AExpr a)
  | AOr    a (AExpr a) (AExpr a)
  | ACase  a CaseType (AExpr a) [ABranchExpr a]
  | ATyped a (AExpr a) TypeExpr

--- Annotated case branch
data ABranchExpr a = ABranch (APattern a) (AExpr a)

--- Annotated pattern
data APattern a
  = APattern  a (QName, a) [(VarIndex, a)] --- constructor pattern
  | ALPattern a Literal                    --- literal pattern
