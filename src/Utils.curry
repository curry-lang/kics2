--- --------------------------------------------------------------------------
--- Various utility functions
---
--- This module should be further divided if it contains too much unrelated
--- things.
---
--- @author  Bjoern Peemoeller
--- @version January 2015
--- --------------------------------------------------------------------------
module Utils(showMonoTypeExpr, notNull, strip, lpad, rpad) where

import AbstractCurry
import AbstractCurryGoodies (isFunctionalType, pre)
import List                 (intercalate)
import Char                 (isSpace)


--------------------------------------------------------------------------
--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, all occurrences of type variables
--- are replaced by "()".
showMonoTypeExpr :: Bool -> CTypeExpr -> String
showMonoTypeExpr mono ty = showMonoTypeExpr' mono False ty

--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, all occurrences of type variables
--- are replaced by "()".
--- If the second argument is True, the type expression is enclosed
--- in brackets.
showMonoTypeExpr' :: Bool -> Bool -> CTypeExpr -> String
showMonoTypeExpr' mono _      (CTVar             (_,name)) =
  if mono then "()" else showIdentifier name
showMonoTypeExpr' mono nested (CFuncType     domain range) = parens nested $
  showMonoTypeExpr' mono (isFunctionalType domain) domain
  ++ " -> " ++ showMonoTypeExpr' mono False range
showMonoTypeExpr' mono nested (CTCons mn@(mod,name) typelist)
  | mn == pre "untyped"
  = "-"
  | otherwise
  = parens (nested && notNull typelist) (showTypeCons mono mod name typelist)
showMonoTypeExpr' mono nested (CRecordType       fields _) =
  '{' : intercalate ", " (map (showField mono nested) fields) ++ "}"

showTypeCons :: Bool -> String -> String -> [CTypeExpr] -> String
showTypeCons _    _   name []       = name
showTypeCons mono mod name ts@(_:_)
  | mod == "Prelude" = showPreludeTypeCons mono name ts
  | otherwise        = name ++ prefixMap (showMonoTypeExpr' mono True) ts " "

showPreludeTypeCons :: Bool -> String -> [CTypeExpr] -> String
showPreludeTypeCons mono name typelist
  | name == "[]" && head typelist == CTCons (pre "Char") []
  = "String"
  | name == "[]"
  = "[" ++ showMonoTypeExpr' mono False (head typelist) ++ "]"
  | isTuple name
  = "(" ++ combineMap (showMonoTypeExpr' mono False) typelist "," ++ ")"
  | otherwise
  = name ++ prefixMap (showMonoTypeExpr' mono True) typelist " "

showField :: Bool -> Bool -> CField CTypeExpr -> String
showField mono nested (lbl, ty)
  = lbl ++ " :: " ++ showMonoTypeExpr' mono nested ty

-- Remove characters '<' and '>' from identifiers since these characters
-- are sometimes introduced in new identifiers generated by the front end
-- (for sections)
showIdentifier :: String -> String
showIdentifier = filter (`notElem` "<>")

-- enclose string with parentheses if required by first argument
parens :: Bool -> String -> String
parens True  s = '(' : s ++ ")"
parens False s = s

prefixMap :: (a -> [b]) -> [a] -> [b] -> [b]
prefixMap f xs s = concatMap (s ++) (map f xs)

combineMap :: (a -> [b]) -> [a] -> [b] -> [b]
combineMap _ []     _ = []
combineMap f (x:xs) s = f x ++ prefixMap f xs s

isTuple :: String -> Bool
isTuple []     = False
isTuple (x:xs) = x == '(' && p1_isTuple xs
  where
  p1_isTuple []         = False
  p1_isTuple (z:[])     = z == ')'
  p1_isTuple (z1:z2:zs) = z1 == ',' && p1_isTuple (z2:zs)

---------------------------------------------------------------------------

notNull :: [a] -> Bool
notNull = not . null

--- Remove leading and trailing whitespace
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--- Extend a String to a given minimal length by adding *leading* spaces.
lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

--- Extend a String to a given minimal length by adding *trailing* spaces.
rpad :: Int -> String -> String
rpad n s = s ++ replicate (n - length s) ' '

---------------------------------------------------------------------------
