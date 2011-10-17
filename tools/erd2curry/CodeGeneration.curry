------------------------------------------------------------------------------
--- This module generates the Curry code for all database operations
--- from an already transformed ERD term.
---
--- @author Michael Hanus, Marion Mueller
--- @version August 2011
------------------------------------------------------------------------------

module CodeGeneration(Option,Storage(..),ConsistencyTest(..),erd2code) where

import ERD
import ERDGoodies
import AbstractCurry
import List
import Char
import FiniteMap
import Maybe

type Option          = (Storage, ConsistencyTest)
data Storage         = Files DBPath | SQLite DBPath | DB
type DBPath          = String
data ConsistencyTest = WithConsistencyTest | WithoutConsistencyTest

erd2code :: Option -> ERD -> CurryProg
erd2code opt@(_, consistencyTest) (ERD n es rs) =
  let imports = "ERDGeneric"  
              : "KeyDatabaseSQLite"
              : fmSortBy (<) (concatMap getImports es)
      entities = filter (not . isGenerated) es
      generatedEntities = filter isGenerated es
  in
  CurryProg n
            imports
            (concatMap (entity2datatype opt n) entities 
             ++ map (entity2datatypeKey opt n) entities
             ++ concatMap (generatedEntity2datatype opt n) generatedEntities)
            (concatMap (entity2trans opt n) entities
             ++ concatMap (generatedEntity2trans opt n) generatedEntities 
             ++ concatMap (entity2selmod opt n) es
             ++ concatMap (entity2DBcode opt n entities es rs) entities
             ++ concatMap (generatedEntity2DBcode opt n rs) generatedEntities
             ++ concatMap (rel2code opt n es) rs
             ++ (case consistencyTest of
                  WithConsistencyTest -> 
                    [checkAll n es]
                       ++ (map (\e -> checkEntity n e) es)
                       ++ (map (\e -> checkEntry n e es rs) es)
                  WithoutConsistencyTest -> [])
             ++ [saveAll    n entities generatedEntities,
                 restoreAll n entities generatedEntities])
            []


generatedEntity2DBcode :: Option -> String -> [Relationship] -> Entity
                       -> [CFuncDecl]
generatedEntity2DBcode (storage, _) name allrels
                       (Entity en attrs@[Attribute a1 (KeyDom d1) _ _, 
                                         Attribute a2 (KeyDom d2) _ _]) =
  let enrels = relationshipsForEntityName en allrels
      d1rels = relationshipsForEntityName d1 enrels
      d2rels = relationshipsForEntityName d2 enrels
      e = lowerFirst en
  in
  (case storage of Files dbpath  -> [predEntry (name,en) Private dbpath]
                   SQLite dbpath -> [predEntrySQLite (name,en) attrs
                                                     Private dbpath]
                   DB            -> [predEntry1 (name,en) Private,
                                     entitySpec (name,en) attrs Private] ) ++
  [cfunc (name, e++a1) 1 Public
         ((baseType (name,en)) ~> entityKeyType (name,d1))
         [CRule [CPComb (name, en) [x, nix]]
                [noGuard (applyF (name, d1++"Key") [cvar "x"])]
                []],
   cfunc (name, e++a2) 1 Public
         ((baseType (name,en)) ~> entityKeyType (name,d2))
         [CRule [CPComb (name, en) [nix, x]]
                [noGuard (applyF (name, d2++"Key") [cvar "x"])]
                []],
   cmtfunc
     ("Dynamic predicate representing the "++en++" relation between "++
      d1++" entities and "++d2++" entities")
     (name, e) 2 Public
     ((baseType (name, d1++"Key")) ~> entityKeyType (name,d2) 
                                 ~> (baseType (db "Dynamic")))
     [CRule [CPComb (name, d1++"Key") [var "key1"], 
             CPComb (name, d2++"Key") [var "key2"]]
            [noGuard (applyF (name,e++"Entry")
                             [constF (pre "unknown"),
                              tupleExpr [cvar "key1", cvar "key2"]])]
                []],
   let d1maxrelsA = filter (isRelWithRangeForEntityA isFiniteRange d1) d1rels
       d1maxrelsB = filter (isRelWithRangeForEntityB isFiniteRange d1) d1rels
       d2maxrelsA = filter (isRelWithRangeForEntityA isFiniteRange d2) d2rels
       d2maxrelsB = filter (isRelWithRangeForEntityB isFiniteRange d2) d2rels
       -- integrity test for maximum checking:
       maxtests = map (relToMaxTestA "key1") d1maxrelsA
                  ++ map (relToMaxTestB "key1") d1maxrelsB
                  ++ map (relToMaxTestA "key2") d2maxrelsA
                  ++ map (relToMaxTestB "key2") d2maxrelsB
       newEntryCall = applyF (erdgen "newEntryR")
                        [constF (name,e++"Entry"),
                         applyF (name,lowerFirst d1++"KeyToKey") [cvar "key1"],
                         applyF (name,lowerFirst d2++"KeyToKey") [cvar "key2"]]
   in
   cmtfunc
     ("Inserts a new "++en++" relation between a "++d1++" entity and a "++
      d2++" entity")
     (name, "new"++en) 2 Public
     ((baseType (name, d1++"Key")) ~> (baseType (name, d2++"Key")) 
                                   ~> transactType)
     [CRule [var "key1", var "key2"]
            [noGuard 
              (seqTrans
                  ([existsDBKeyCall (name,d1) (Just (cvar "key1")),
                    existsDBKeyCall (name,d2) (Just (cvar "key2")),
                    applyF (erdgen "unique2")
                      [CSymbol (name, e++"Entry"),
                       applyF (name,lowerFirst d1++"KeyToKey") [cvar "key1"],
                       applyF (name,lowerFirst d2++"KeyToKey") [cvar "key2"]]] ++
                    maxtests ++ [newEntryCall]))]
            []],
   let d1minrelsA = filter (isRelWithRangeForEntityA isMinRange d1) d1rels
       d1minrelsB = filter (isRelWithRangeForEntityB isMinRange d1) d1rels
       d2minrelsA = filter (isRelWithRangeForEntityA isMinRange d2) d2rels
       d2minrelsB = filter (isRelWithRangeForEntityB isMinRange d2) d2rels
       -- integrity test for minimum checking:
       mintests = map (relToMinTestA "key1") d1minrelsA
                  ++ map (relToMinTestB "key1") d1minrelsB
                  ++ map (relToMinTestA "key2") d2minrelsA
                  ++ map (relToMinTestB "key2") d2minrelsB
       deleteCall = applyF (erdgen "deleteEntryR")
                      [constF (name,e++"Entry"),
                       applyF (name,lowerFirst d1++"KeyToKey") [cvar "key1"],
                       applyF (name,lowerFirst d2++"KeyToKey") [cvar "key2"]]
   in
   cmtfunc
     ("Deletes an existing "++en++" relation between a "++d1++" entity and a "++
      d2++" entity")
     (name, "delete"++en) 2 Public
     ((baseType (name, d1++"Key")) ~> (baseType (name, d2++"Key")) 
                                   ~> transactType)
     [CRule [var "key1", var "key2"]
            [noGuard (foldr (\a b -> applyF (db "|>>") [a,b]) deleteCall
                            mintests)]
            []],
   cmtfunc
     ("Gets the associated "++d1++" entities for a given "++d2++" entity")
     (name, "get"++d1++d2++"s") 2 Public
     (baseType (name,d1) ~> CTCons transTC [listType (baseType (name,d2))])
     [CRule [var "e"]
            [noGuard $
              CLetDecl
                [CLocalPat (var "ekey")
                           (applyF (name,lowerFirst d1++"Key") [cvar "e"]) []]
                (applyF (db "|>>=")
                  [applyF (db "getDB")
                    [applyF (name,"queryCond"++en)
                      [CLambda [var "t"]
                        (applyF (pre "==")
                          [applyF (name,e++d1++en++"Key") [cvar "t"],
                           cvar "ekey"])]],
                   applyF (pre ".")
                    [applyF (db "mapT") [constF (name,"get"++d2)],
                     applyF (pre "map") [constF (name,e++d2++en++"Key")]]]
                )
            ]
            []]  ] ++
   queryGeneratedEntity (name,en) Public
 where
  relToMaxTestA :: String -> Relationship -> CExpr
  relToMaxTestA vname (Relationship _ [REnd e1 _ _, REnd e2 _ c2]) =
    relToMinMaxTest "maxTestInsert"
                    vname (cardMaximum c2) (combineIds [e1,e2,"Key"]) e2      

  relToMaxTestB :: String -> Relationship -> CExpr
  relToMaxTestB vname (Relationship _ [REnd e1 _ c1, REnd e2 _ _]) =
    relToMinMaxTest "maxTestInsert"
                    vname (cardMaximum c1) (combineIds [e2,e1,"Key"]) e1  

  relToMinTestA :: String -> Relationship -> CExpr
  relToMinTestA vname (Relationship _ [REnd e1 _ _, REnd e2 _ c2]) =
    relToMinMaxTest "minTestDelete"
                    vname (cardMinimum c2) (combineIds [e1,e2,"Key"]) e2      

  relToMinTestB :: String -> Relationship -> CExpr
  relToMinTestB vname (Relationship _ [REnd e1 _ c1, REnd e2 _ _]) =
    relToMinMaxTest "minTestDelete"
                    vname (cardMinimum c1) (combineIds [e2,e1,"Key"]) e1  

  relToMinMaxTest :: String -> String -> Int -> String -> String -> CExpr
  relToMinMaxTest testname vname m attrName rName =
    let rname = lowerFirst rName in
    applyF (erdgen testname)
           [string2ac rName,
            CSymbol (name,rname++"Entry"),
            CSymbol (name,"keytuple2"++rName),
            CSymbol (name,rname++attrName),
            CLit (CIntc m),
            cvar vname]

-- Generate code for querying relationship entities
queryGeneratedEntity :: QName -> CVisibility -> [CFuncDecl]
queryGeneratedEntity (s,eName) v =
  let e = lowerFirst eName
  in
  [cmtfunc
    ("Gets all "++eName++" relationship entities stored in the database.")
    (s,"queryAll"++eName++"s") 0 v
    (CTCons (db "Query") [CTCons (pre "[]") [baseType (s,eName)]])
    [CRule []
           [noGuard
             (applyF (db "transformQ")
                     [applyF (pre "map")
                       [applyF (pre "uncurry")
                           [CSymbol (s,"keytuple2"++eName)]],
                      applyF (db "allDBKeyInfos") [CSymbol (s,e++"Entry")]])]
           []],
   cmtfunc
    ("Gets all "++eName++" relationship entities satisfying a given condition.")
    (s,"queryCond" ++ eName) 1 v
    ((baseType (s,eName) ~> prelType "Bool")
     ~> CTCons (db "Query") [listType (baseType (s,eName))])
    [CRule [var "econd"]
           [noGuard
             (applyF (db "transformQ")
                     [applyF (pre "filter") [cvar "econd"],
                      constF (s,"queryAll"++eName++"s")])]
           []]]


getImports :: Entity -> [String]
getImports (Entity _ attrs) = getImportsAttrs attrs
  where
   getImportsAttrs :: [Attribute] -> [String]
   getImportsAttrs [] = []
   getImportsAttrs ((Attribute _ t _ _) : ats) =
     case t of UserDefined s _ -> takeWhile (/= '.') s : getImportsAttrs ats
               DateDom _       -> "Time" : getImportsAttrs ats
               _               -> getImportsAttrs ats

-- Create data type definitions for an entity:
entity2datatype :: Option -> String -> Entity -> [CTypeDecl]
entity2datatype _ ername (Entity name attrs) =
  [CType (ername,name) Public [] [CCons (ername,name) arity Private argTypes],
   CTypeSyn (ername,name++"Tuple") Private [] (tupleType (tail argTypes))]
 where
  arity = length attrs
  argTypes = map attrType attrs

-- Create transformation between entity type and tuple representation:
entity2trans :: Option -> String -> Entity -> [CFuncDecl]
entity2trans _ ername (Entity name attrs) =
  [cmtfunc
    ("Transforms entity "++name++" into tuple representation.")
    (ername,lowerFirst name++"2tuple")
    1
    Private
    (baseType (ername,name) ~> baseType (ername,name++"Tuple"))
    [CRule [CPComb (ername,name) (replace nix 0 (map xn [1..arity]))]
           [noGuard 
             (tupleExpr (map (\i->cvar ("x"++show i)) [2..arity]))]
           []],
   cmtfunc
    ("Transforms key and tuple into a "++name++" entity.")
    (ername,"keytuple2"++name)
    2
    Private
    (baseType (erdgen "Key") ~> baseType (ername,name++"Tuple")
     ~> baseType (ername,name))
    [CRule [xn 1, tuplePattern (map xn [2..arity])]
           [noGuard 
             (applyF (ername,name) (map (\i->cvar ("x"++show i)) [1..arity]))]
           []]]
 where
  arity = length attrs

-- Create transformation between generated entity type and tuple representation:
generatedEntity2trans :: Option -> String -> Entity -> [CFuncDecl]
generatedEntity2trans _ ername (Entity name attrs) =
  [cmtfunc
    ("Transforms relationship entity "++name++" into tuple representation.")
    (ername,lowerFirst name++"2tuple")
    1
    Private
    (baseType (ername,name) ~> baseType (ername,name++"Tuple"))
    [CRule [CPComb (ername,name) (map xn [1..arity])]
           [noGuard 
             (tupleExpr (map (\i->cvar ("x"++show i)) [1..arity]))]
           []],
   cmtfunc
    ("Transforms key and tuple into a "++name++" relationship entity.")
    (ername,"keytuple2"++name)
    2
    Private
    (baseType (erdgen "Key") ~> baseType (ername,name++"Tuple")
     ~> baseType (ername,name))
    [CRule [nix, tuplePattern (map xn [1..arity])]
           [noGuard 
             (applyF (ername,name) (map (\i->cvar ("x"++show i)) [1..arity]))]
           []]]
 where
  arity = length attrs


entity2datatypeKey :: Option -> String -> Entity -> CTypeDecl
entity2datatypeKey _ ername (Entity name attrs) =
  datatypeKey (ername, name) (getKeyType attrs)
  where
    getKeyType :: [Attribute] -> CTypeExpr
    getKeyType [] = error "entity2datatypeKey: missing key!"
    getKeyType (a@(Attribute _ _ key _) : atr) 
      | key == PKey = attrType a
      | otherwise   = getKeyType atr

datatypeKey :: QName -> CTypeExpr -> CTypeDecl
datatypeKey (s, name) argType =
  let n = (s, name ++ "Key")
  in
  CType n Public [] [CCons n 1 Private [argType]] 

generatedEntity2datatype :: Option -> String -> Entity -> [CTypeDecl]
generatedEntity2datatype _ n (Entity name attrs) =
  [CType (n,name) Public [] 
         [CCons (n,name) (length attrs) Private
                (replicate (length attrs) (baseType (erdgen "Key")))],
   CTypeSyn (n,name++"Tuple") Private []
            (tupleType (replicate (length attrs) (baseType (erdgen "Key"))))]


---------------------------------------------------------------
-- Generate getter + setter operations for an entity:
---------------------------------------------------------------
entity2selmod :: Option -> String -> Entity -> [CFuncDecl]  
entity2selmod _ ername (Entity name attrs) =
  f (ername, name) 
    (length attrs) 
    1
    attrNames
    (map ((\y -> (ername,y)) . ((lowerFirst name) ++)) attrNames)
    (map ((\y -> (ername,y)) . (("set"++name) ++))     attrNames)
    (map getAType attrs)
    (map getNull attrs)
    (map isPKey attrs)
    (map getFKeyDom attrs)
  where
    getANames :: [Attribute] -> [String]
    getANames [] = []
    getANames ((Attribute an _ _ _): ats) = an : getANames ats

    attrNames = getANames attrs

    getAType :: Attribute -> QName
    getAType (Attribute _ t k _) =
      case t of IntDom _        -> if k==PKey then erdgen "Key" else pre "Int"
                FloatDom _      -> pre "Float"
                StringDom _     -> pre "String"
                BoolDom _       -> pre "Bool"
                DateDom _       -> ("Time","CalendarTime")
                UserDefined s _ -> userMod s
                KeyDom s        -> (ername,s++"Key")
                _               -> pre "" -- should not occur

    -- null values are only handled in a specific way (i.e., as Maybe types)
    -- if they are not strings
    getNull :: Attribute -> Bool
    getNull (Attribute _ t _ null) = null && not (isStringDom t)

    isPKey :: Attribute -> Bool
    isPKey (Attribute _ _ key _) = key == PKey 

    getFKeyDom :: Attribute -> String
    getFKeyDom (Attribute _ t _ _) =
      case t of KeyDom kd -> kd
                _         -> "" 

    f :: QName -> Int -> Int -> [String]
        -> [QName] -> [QName] -> [QName]
        -> [Bool] -> [Bool] -> [String] -> [CFuncDecl]
    f _ _ _ [] [] [] [] [] [] [] = []
    f n l nth (attr:attrnames) (s:selnames) (m:modnames) (t:types)
              (null:nulls) (key:keys) (fkeydom:fkeydoms) 
      --| fkey      = (selector n l nth s t null Private) :
      --              (f n l (nth+1) selnames modnames types nulls keys fkeys)             
      | key       = (mutator n l nth attr m t null Private fkeydom)
         : (f n l (nth+1) attrnames selnames modnames types nulls keys fkeydoms)
      | otherwise = (selector n l nth attr s t null Public fkeydom) 
         : (mutator n l nth attr m t null Public fkeydom)
         : (f n l (nth+1) attrnames selnames modnames types nulls keys fkeydoms)

-- enAttrName :: EN -> (Maybe) AttrType
-- enAttrName (EN x _ ... _) = x
selector :: QName -> Int -> Int -> String -> QName -> QName -> Bool
         -> CVisibility -> String -> CFuncDecl
selector consname arity nth attr selname nthType isnull v fKeyDom = 
  cmtfunc
    ("Gets the value of attribute \""++attr++"\" of a "++snd consname++
     " entity.")
    selname 1 v selType
    (if null fKeyDom
     then [CRule [CPComb consname (replace x (nth-1) (replicate arity nix))] 
                 [noGuard (cvar "x")]
                 []]
     else
      if isnull
      then [CRule [CPComb consname (replace (CPComb (pre "Nothing") [])
                                            (nth-1)
                                            (replicate arity nix))] 
                  [noGuard (constF (pre "Nothing"))]
                  [],
            CRule [CPComb consname (replace (CPComb (pre "Just") [x])
                                           (nth-1)
                                           (replicate arity nix))] 
                  [noGuard (applyJust
                             (applyF (fst selname,fKeyDom++"Key") [cvar "x"]))]
                  []]
      else [CRule [CPComb consname (replace x (nth-1) (replicate arity nix))] 
                  [noGuard (applyF (fst selname,fKeyDom++"Key") [cvar "x"])]
                  []])
  where
    selType =
      if isnull then baseType consname ~> maybeType (baseType nthType)
                else baseType consname ~> baseType nthType

--setStudentName :: Student -> String -> Student
--setStudentName (Student x1 _ x3 x4) x = Student x1 x x3 x4
--setStudentName :: Student -> Maybe String -> Student
--setStudentName (Student x1 _ x3 x4) x = Student x1 x x3 x4
mutator :: QName -> Int -> Int -> String -> QName
        -> QName -> Bool -> CVisibility -> String -> CFuncDecl
mutator consname arity nth attr modname nthType isnull v fKeyDom =
  cmtfunc
    ("Sets the value of attribute \""++attr++"\" in a "++snd consname++
     " entity.")
    modname 1 v
    (modType consname nthType isnull)
    [CRule [CPComb consname (replace nix (nth-1) (map xn [1..arity])),
            if True --null fKeyDom
            then x
            else CPComb (fst modname,fKeyDom++"Key") [x]]
           [noGuard 
             (applyF consname
                     (replace (rhsArg (cvar "x")) (nth-1)
                              (map (\i->cvar ("x"++show i)) [1..arity])))]
           []]
  where
    modType typeCons typeArg n
      | n         = (baseType typeCons) ~> maybeType (baseType typeArg) 
                                      ~> baseType typeCons
      | otherwise = (baseType typeCons) ~> (baseType typeArg) ~> (baseType typeCons)

    rhsArg arg = if null fKeyDom
                 then arg
                 else applyF (fst modname,
                              if isnull
                              then "maybe" ++  fKeyDom ++ "KeyToKey"
                              else lowerFirst fKeyDom ++ "KeyToKey")
                             [arg]


entityKey :: QName -> [Attribute] -> CVisibility -> CFuncDecl
entityKey (s,eName) attrs v =
  cmtfunc
    ("Gets the key of a "++eName++" entity.")
    (s,(lowerFirst eName) ++ "Key") 1 v
    ((baseType (s,eName)) ~> (baseType (s,eName ++ "Key")))
    [CRule [CPComb (s,eName) 
                   (replace x (key attrs) 
                            (replicate (length attrs) nix))]
           [noGuard (applyF (s, eName ++ "Key")
                            [cvar "x"])]
           []]    
  where 
    key :: [Attribute] -> Int
    key ((Attribute _ _ k _) : ats) 
      | k == PKey = 0 
      | otherwise = 1 + key ats

-- Generate "show" function for database keys.
showEntityKey :: QName -> CVisibility -> CFuncDecl
showEntityKey (s,eName) v =
 cmtfunc
   ("Shows the key of a "++eName++" entity as a string.\n"++
    "This is useful if a textual representation of the key is necessary\n"++
    "(e.g., as URL parameters in web pages), but it should no be used\n"++
    "to store keys in other attributes!")
   (s,"show"++eName++"Key") 1 v
   (baseType (s,eName) ~> prelType "String")
   [CRule [var "obj"]
          [noGuard (applyF (erdgen "showDatabaseKey")
                      [string2ac eName,
                       constF (s,(lowerFirst eName) ++ "KeyToKey"),
                       applyF (s,(lowerFirst eName) ++ "Key")
                              [cvar "obj"]])]
          []]

-- Generate "read" function for database keys.
readEntityKey :: QName -> CVisibility -> CFuncDecl
readEntityKey (s,eName) v =
 cmtfunc
   ("Transforms a string into a key of a "++eName++" entity.\n"++
    "Nothing is returned if the string does not represent a reasonable key.")
   (s,"read"++eName++"Key") 1 v
   (prelType "String" ~> maybeType (baseType (s,eName++"Key")))
   [CRule [var "s"]
          [noGuard (applyF (erdgen "readDatabaseKey")
                      [string2ac eName,
                       constF (s,eName ++ "Key"),
                       cvar "s"])]
          []]

entityKeyToKey :: QName -> CVisibility -> CFuncDecl
entityKeyToKey (s,eName) v =
 cfunc (s,(lowerFirst eName) ++ "KeyToKey") 1 v
       ((baseType (s,eName ++ "Key")) ~> (baseType (erdgen "Key")))
       [CRule [CPComb (s,eName ++ "Key") [var "k"]]         
              [noGuard (cvar "k")]
              []]    

entityMaybeKeyToKey :: QName -> CVisibility -> CFuncDecl
entityMaybeKeyToKey (s,eName) v =
 cfunc (s,"maybe" ++ eName ++ "KeyToKey") 1 v
       (maybeType (baseType (s,eName ++ "Key"))
        ~> maybeType (baseType (erdgen "Key")))
       [CRule [CPComb (pre "Nothing") []]
              [noGuard (constF (pre "Nothing"))]
              [],
        CRule [CPComb (pre "Just") [CPComb (s,eName ++ "Key") [var "k"]]]
              [noGuard (applyJust (cvar "k"))]
              []]

------------------------------------------------------------------------
-- dynamic predicates for tables
----------------------------------------------------------------------

-- Generate dynamic predicate definition for an entity.
-- en :: ENKey -> EN -> Dynamic
-- en (ENKey i) (EN x1 ... xn) | i =:= x1 = enEntry (EN i x2 ... xn)
pred :: QName -> Int -> CVisibility -> CFuncDecl
pred (s,eName) _ v = let ename = lowerFirst eName in
  cmtfunc
    ("Dynamic predicate representing the relation\nbetween keys and "++eName++
     " entities.")
    (s,ename) 1 v 
    (baseType (s,eName++"Key") ~> baseType (s,eName) ~> baseType (db "Dynamic"))
    [CRule [var "key",var "obj"]
           [(applyF (pre "=:=")
                    [cvar "key",
                     applyF (s,ename++"Key") [cvar "obj"]],
             applyF (s,ename++"Entry")
               [applyF (s,ename++"KeyToKey") [cvar "key"],
                applyF (s,ename++"2tuple")   [cvar "obj"]])]
           []]

-- Generate persistent dynamic predicate for an entity using file-based
-- implementation of PAKCS.
-- enEntry :: Key -> ENTuple -> Dynamic
-- enEntry = persistent "file:enDB"
predEntry :: QName -> CVisibility -> String -> CFuncDecl
predEntry (s, eName) v dbpath =
  cmtfunc
    ("Database predicate representing the relation between keys and "++eName++
     " tuple entities.")
    (s,(lowerFirst eName) ++ "Entry") 2 v 
    (baseType (erdgen "Key") ~> baseType (s,eName++"Tuple")
     ~> baseType (db "Dynamic"))
    [CRule []
           [noGuard
             (applyF (db "persistent") 
                     [cvar ("\"file:" ++ dbpath ++ "/" ++ eName ++ "DB\"")])]
           []]

-- Generate persistent dynamic predicate for an entity using DB implementation
-- of Sebastian Fischer.
-- enEntry :: EN -> Dynamic
-- enEntry = persistent1 "db:file" enSpec
predEntry1 :: QName -> CVisibility -> CFuncDecl
predEntry1 (s, eName) v =
  cmtfunc
    ("Database predicate representing "++eName++" entities.")
    (s,(lowerFirst eName) ++ "Entry") 1 v 
    (baseType (s,eName) ~> baseType (db "Dynamic"))
    [CRule []
           [noGuard
             (applyF
               (db "persistent1") 
               [cvar ("\"db:" ++ eName ++ "DB\""),
                CSymbol (s, (lowerFirst eName) ++ "Spec")])]
           []]

entitySpec :: QName -> [Attribute] -> CVisibility -> CFuncDecl
entitySpec (s,eName) attrs v = 
  cfunc (s,(lowerFirst eName) ++ "Spec") 0 v
        (CTCons (db "DBSpec") [baseType (s,eName)]) 
        [CRule [] 
               [noGuard (applyCons (s, eName) (length attrs) (reverse attrs))]
               []]
  where 
    applyCons :: QName -> Int -> [Attribute] -> CExpr
    applyCons (m, cons) i [] = applyF (db ("cons" ++ show i))
                                      [CSymbol (m, cons)]
    applyCons (m, cons) i ((Attribute n d _ _):ats) =
      CApply (applyCons (m, cons) i ats)
             (applyF (db (typeF d)) [cvar ("\""++n++"\"")])  

    typeF :: Domain -> String
    typeF (IntDom _) = "int"
    typeF (FloatDom _) = "float"
    typeF (StringDom _ ) = "string"
    typeF (BoolDom _) = "bool"
    typeF (DateDom _) = "date"
    typeF (KeyDom _) = "int"
    typeF (UserDefined str _) = 
       let (m,f) = userMod str
        in m ++ "." ++ lowerFirst f

-- Generate persistent dynamic predicate for an entity using SQLite3 DB
-- of Sebastian Fischer's KeyDatabaseSQLite module.
predEntrySQLite :: QName -> [Attribute] -> CVisibility -> String -> CFuncDecl
predEntrySQLite (s,eName) attrs v dbpath = 
  cmtfunc
    ("Database predicate representing the relation between keys and "++eName++
     " tuple entities.")
    (s, lowerFirst eName ++ "Entry") 2 v
    (baseType (erdgen "Key") ~> baseType (s,eName++"Tuple")
     ~> baseType (db "Dynamic"))
    [CRule [] 
           [noGuard
             (applyF (db "persistentSQLite")
                [string2ac (dbpath ++ "/" ++ s ++ ".db"),
                 string2ac eName,
                 list2ac (map att2string attrs)])]
           []]
  where 
    att2string :: Attribute -> CExpr
    att2string (Attribute n _ _ _) = string2ac n

--------------------------------------------------------------------

-- DB operations
-------------------------------------------------------------------
--- Generates a get-Operation for an entity.
-- getDozent :: DozentKey -> IO Dozent
-- getDozent key = getEntry key dozent
getEntity :: QName -> CVisibility -> [CFuncDecl]
getEntity (s,eName) v =
  let e = lowerFirst eName
  in
  [cmtfunc
    ("Gets a "++eName++" entity stored in the database with the given key.")
    (s,"get" ++ eName) 1 v
    ((baseType (s, eName ++ "Key")) ~> (CTCons transTC [baseType (s,eName)])) 
    [CRule [var "key"]
           [noGuard
             (applyF (erdgen "getEntry")
                     [CSymbol (s,e++"Entry"),
                      CSymbol (s,"keytuple2"++eName),
                      applyF (s,e++"KeyToKey") [cvar "key"]])]
           []],
   cmtfunc
    ("Gets all "++eName++" entities stored in the database.")
    (s,"queryAll"++eName++"s") 0 v
    (CTCons (db "Query") [listType (baseType (s,eName))])
    [CRule []
           [noGuard
             (applyF (db "transformQ")
                     [applyF (pre "map")
                       [applyF (pre "uncurry")
                           [CSymbol (s,"keytuple2"++eName)]],
                      applyF (db "allDBKeyInfos") [CSymbol (s,e++"Entry")]])]
           []],
   cmtfunc
    ("Gets all "++eName++" entities satisfying a given condition.")
    (s,"queryCond" ++ eName) 1 v
    ((baseType (s,eName) ~> baseType (pre "Bool"))
     ~> CTCons (db "Query") [listType (baseType (s,eName))])
    [CRule [var "econd"]
           [noGuard
             (applyF (db "transformQ")
                     [applyF (pre "filter") [cvar "econd"],
                      constF (s,"queryAll"++eName++"s")])]
           []]]

-------------------------------------------------------------------
entity2DBcode :: Option -> String -> [Entity] -> [Entity] -> [Relationship]
              -> Entity -> [CFuncDecl]
entity2DBcode (storage,_) ername es esAll rsAll e@(Entity name attrs) = 
  let n = (ername, name)
  in
  (case storage of Files dbpath  -> [predEntry n Private dbpath]
                   SQLite dbpath -> [predEntrySQLite n (tail attrs)
                                                     Private dbpath]
                   DB            -> [predEntry1 n Private,
                                     entitySpec n attrs Private]) ++
  [pred n (length attrs) Public,
   entityKey n attrs Public,
   showEntityKey n Public,
   readEntityKey n Public,
   entityKeyToKey n Private,
   entityMaybeKeyToKey n Private,
   newEntity n attrs es (relationshipsForEntityName (entityName e) rsAll)
             Public esAll rsAll,
   updateEntity n es rsAll Public,
   deleteEntity n es esAll rsAll Public] ++
   getEntity n Public

-- newEntity (emod,ename) attrs ens rels vis allens allrels
-- (emod,ename): qualified name of entity
-- attrs: attributes that are provided as parameters to the new operation
--        (of type Maybe if attribute has a default value)
-- ens:  entities that are not generated for relationships
-- rels: relationships related to this entity
-- vis: visibility
-- allens: all entities
-- allrels: all relationships
newEntity :: QName -> [Attribute] -> [Entity] -> [Relationship] -> CVisibility 
         -> [Entity] -> [Relationship] -> CFuncDecl
newEntity (str,eName) attrs ens rels v esAll rsAll =
  let e = lowerFirst eName

      generatedRs = filter isGeneratedR rels
      exactRs  = filter isExactB  generatedRs --(i,i), i>1
      maxRs    = filter isMaxB    generatedRs --(0,i), i>1
      minMaxRs = filter isMinMaxB generatedRs --(i,j), i>0, j>i
      newFunType = newType (str,eName) (filter notPKey attrs)
                           exactRs maxRs minMaxRs rsAll

      l = length (exactRs ++ maxRs ++ minMaxRs)
      attributeP = map ((++"_p") . lowerFirst . attributeName)
                       (filter notPKey attrs)
      exactP = duplicate exactRs (map (("k"++) . show) [1 .. length exactRs])
      maxP = map (("ks"++) . show)
                 [length exactRs + 1 .. length (exactRs ++ maxRs)]
      minMaxP = duplicate' minMaxRs 
                  (map (("k" ++) . show) [length (exactRs ++ maxRs) + 1 .. l])
                  (map (("ks"++) . show) [length (exactRs ++ maxRs) + 1 .. l])
      parameter = attributeP ++ exactP ++ maxP ++ minMaxP
                  
      ts = tests (str,eName) ens rels
                 (New exactRs exactP maxRs maxP minMaxRs minMaxP rsAll) 

      entryCall =
          applyF (erdgen "newEntry")
                 [CSymbol (str, e++"Entry"),
                  CSymbol (str, "keytuple2"++eName),
                  tupleExpr (map (keyToKeyCvar str)
                                 (zipWith (\a b -> (a,b))
                                          (filter notPKey attrs)
                                          parameter))]

      newSuffix = concatMap ("With"++)
                            (map attributeName (filter isForeignKey attrs)) ++
                  if l==0
                  then ""
                  else concatMap (\k->"With"++k++"Keys")
                                 (map (relatedRelation eName)
                                      (exactRs++maxRs++minMaxRs))
  in
  cmtfunc
    ("Inserts a new "++eName++" entity.")
    (str,"new" ++ eName ++ newSuffix) (length attrs) v
    newFunType
    [CRule
      (map var parameter)
      [noGuard 
        (foldr (\a b -> applyF (db "|>>") [a,b])
          (if null (exactP++maxP++minMaxP)
           then entryCall
           else applyF (db "|>>=")
                  [entryCall,
                   CLambda 
                     [var "entry"]
                     (foldr
                        (\a b -> applyF (db "|>>") [a,b])
                        (applyF (db "returnT") [cvar "entry"]) 
                        (newEntryExact exactRs exactP (str,e) esAll rsAll ++
                         newEntryMax maxRs maxP (str,e) esAll rsAll ++
                         newEntryMinMax minMaxRs minMaxP (str,e) esAll rsAll))])
          ts)]
         []]

 where
   -- extracts the name of the relationship related to a given entity name
   relatedRelation :: String -> Relationship -> String
   relatedRelation en (Relationship _ [REnd en1 _ _, REnd en2 _ _]) =
     if en==en1 then en2 else en1

   correctOrder :: String -> String -> [Entity] -> Bool
   correctOrder _ _ [] = error "entity not found" --False
   correctOrder en en1 (Entity name atts :es) 
     | en == name = case atts of
         [Attribute _ (KeyDom e1) _ _, _] -> e1==en1 
         [_, Attribute _ (KeyDom _) _ _] -> False
         _                                -> error "correctOrder: wrong attributes"
     | otherwise = correctOrder en en1 es

   -- extract the lowercase domain names of a derived relationship entity:
   lcDomainsOfRelEntity :: [Entity] -> String -> (String,String)
   lcDomainsOfRelEntity allens derivedename =
     maybe (error $ "Entity "++derivedename++" not found!")
           (\e -> let domnames = map ((\ (KeyDom kdn) -> kdn) . attributeDomain)
                                     (entityAttributes e)
                   in (lowerFirst (head domnames), lowerFirst (domnames!!1)))
           (find (\e -> entityName e == derivedename) allens)

   newEntryExact [] _ _ _ _ = []
   newEntryExact (Relationship _ [REnd en _ _, REnd rn _ (Exactly i)]:exactRs) exactP (s,e) es rs = 
     let (ip,restp) = splitAt i exactP
         (d1,d2) = lcDomainsOfRelEntity es rn
     in
     (map (\p -> applyF (erdgen "newEntryR") 
                        (if correctOrder rn en es
                         then [constF (s, lowerFirst rn++"Entry"),
                               applyF (s,d1++"KeyToKey")
                                      [applyF (s,e++"Key") [cvar "entry"]], 
                               applyF (s,d2++"KeyToKey") [cvar p]] 
                         else [constF (s,lowerFirst rn++"Entry"),
                               applyF (s,d1++"KeyToKey") [cvar p],
                               applyF (s,d2++"KeyToKey")
                                      [applyF (s,e++"Key") [cvar "entry"]]]))
          ip)
       ++ (newEntryExact exactRs restp (s,e) es rs)

   newEntryMax [] _ _ _ _ = []
   newEntryMax (Relationship _ [REnd en _ _, REnd rn _ _]:maxRs) (p:maxP) (s,e) es rs = 
     let (d1,d2) = lcDomainsOfRelEntity es rn
     in
     [applyF (pre "mapT_")
             [CLambda [var "a"]
                      (applyF (erdgen "newEntryR")
                              (if correctOrder rn en es
                               then [constF (s, lowerFirst rn++"Entry"),
                                     applyF (s,d1++"KeyToKey")
                                       [applyF (s,e++"Key") [cvar "entry"]], 
                                     applyF (s,d2++"KeyToKey") [cvar "a"]] 
                               else [constF (s, lowerFirst rn++"Entry"),
                                     applyF (s,d1++"KeyToKey") [cvar "a"],
                                     applyF (s,d2++"KeyToKey")
                                       [applyF (s,e++"Key") [cvar "entry"]]])),
              cvar p]]
        ++ newEntryMax maxRs maxP (s,e) es rs
  
   newEntryMinMax [] _ _ _ _ = []
   newEntryMinMax
          (Relationship _ [REnd en _ _, REnd rn _ (Between i _)] : minMaxRs)
          minMaxP (s,e) es rs = 
     let (ip,(p:restp)) = splitAt i minMaxP
         (d1,d2) = lcDomainsOfRelEntity es rn
     in
     (map (\a -> applyF (erdgen "newEntryR") 
                        (if correctOrder rn en es
                         then [constF (s, lowerFirst rn++"Entry"),
                               applyF (s,d1++"KeyToKey")
                                      [applyF (s,e++"Key") [cvar "entry"]], 
                               applyF (s,d2++"KeyToKey") [cvar a]]
                         else [constF (s, lowerFirst rn++"Entry"),
                               applyF (s,d1++"KeyToKey") [cvar a],
                               applyF (s,d2++"KeyToKey")
                                      [applyF (s,e++"Key") [cvar "entry"]]]))
          ip)
       ++ [applyF (pre "mapT_")
                  [CLambda [var "a"]
                           (applyF (erdgen "newEntryR")
                              (if correctOrder rn en es
                               then [constF (s, lowerFirst rn++"Entry"),
                                     applyF (s,d1++"KeyToKey")
                                       [applyF (s,e++"Key") [cvar "entry"]],
                                     applyF (s,d2++"KeyToKey") [cvar "a"]] 
                               else [constF (s, lowerFirst rn++"Entry"),
                                     applyF (s,d1++"KeyToKey") [cvar "a"],
                                     applyF (s,d2++"KeyToKey")
                                       [applyF (s,e++"Key") [cvar "entry"]]])),
                   cvar p]]
        ++ newEntryMinMax minMaxRs restp (s,e) es rs


   isExactB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
     case c of Exactly i -> i>1
               _         -> False
   isMaxB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
     case c of (Between 0 (Max i)) -> i>1
               _                   -> False
   isMinMaxB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
     case c of (Between i (Max j)) -> i>0 && j>i
               _                   -> False

   getExactB (Relationship _ [REnd _ _ _, REnd _ _ (Exactly i)]) = i 
   getMinB (Relationship _ [REnd _ _ _, REnd _ _ (Between i _)]) = i

   duplicate [] [] = []
   duplicate (Relationship _ [REnd _ _ _, REnd _ _ (Exactly i)]:exactRs) (p:ps) =
     (map ((p++) . show) [1..i]) ++ (duplicate exactRs ps)

   duplicate' [] [] [] = []
   duplicate' (Relationship _ [REnd _ _ _, REnd _ _ (Between i _)]:minMaxRs) (p:ps) (kp:kps) =
     (map ((p++) . show) [1..i]) ++ (kp:(duplicate' minMaxRs ps kps))

   newType :: QName -> [Attribute] -> [Relationship] -> [Relationship]
             -> [Relationship] -> [Relationship] -> CTypeExpr
   newType (m,n) [] exactRs maxRs minMaxRs rs 
     | null exactRs && null maxRs && null minMaxRs = CTCons transTC [baseType (m,n)] 
     | length exactRs > 0 = nTExact (m,n) exactRs maxRs minMaxRs rs
     | length maxRs > 0 = nTMax (m,n) maxRs minMaxRs rs
     | otherwise = nTMinMax (m,n) minMaxRs rs
   newType n (a@(Attribute _ d _ nu):ats) exactRs maxRs minMaxRs rs = 
     let t = case d of KeyDom s -> if nu 
                                   then maybeType (ctvar (s++"Key"))
                                   else ctvar (s++"Key")
                       _        -> attrTypeNew a 
     in
     CFuncType t  (newType n ats exactRs maxRs minMaxRs rs) 
   
   nTExact (m,n) [] maxRs minMaxRs rs = nTMax (m,n) maxRs minMaxRs rs
   nTExact (m,n) (Relationship _ [REnd en1 _ _, REnd en2 _ (Exactly e)]:exactRs) maxRs minMaxRs rs =
     let keyType = (startsIn en1 en2 rs) ++ "Key"
     in
     foldr (~>) (nTExact (m,n) exactRs maxRs minMaxRs rs) (replicate e (ctvar keyType))

   nTMax (m,n) [] minMaxRs rs = nTMinMax (m,n) minMaxRs rs
   nTMax (m,n) (Relationship _ [REnd en1 _ _, REnd en2 _ _]:maxRs) minMaxRs rs =
     let keyType = (startsIn en1 en2 rs) ++ "Key"
     in
     (listType (ctvar keyType)) ~> (nTMax (m,n) maxRs minMaxRs rs) 

   nTMinMax (m,n) [] _ = CTCons transTC [baseType (m,n)] 
   nTMinMax (m,n) (Relationship _ [REnd en1 _ _,REnd en2 _ (Between min _)]:minMaxRs) rs =
     let keyType = (startsIn en1 en2 rs) ++ "Key"
     in
     foldr (~>) (nTMinMax (m,n) minMaxRs rs) 
           ((replicate min (ctvar keyType)) ++ [listType (ctvar keyType)])

   startsIn :: String -> String -> [Relationship] -> String
   startsIn _ _ [] = error "missing relationship"
   startsIn n en (Relationship _ [REnd en1 _ _, REnd en2 _ _]:rs) 
     | en == en2 && en1 /= n = en1
     | otherwise = startsIn n en rs

   keyToKeyCvar :: String -> (Attribute,String) -> CExpr
   keyToKeyCvar name (Attribute _ d _ isnull, p) =  case d of
     KeyDom s -> applyF (name, if isnull then "maybe"++s++"KeyToKey"
                                         else lowerFirst s++"KeyToKey")
                        [cvar p]
     _  -> if hasDefault d 
           then let defaultmaybe = if isStringDom d
                                   then applyF (erdgen "defaultString")
                                               [getDefault d, cvar p]
                                   else applyMaybe (getDefault d)
                                                   (constF (pre "id")) (cvar p)
                 in if isnull && not (isStringDom d)
                    then applyF (pre "Just") [defaultmaybe]
                    else defaultmaybe
           else cvar p

   -- Maybe if null values allowed or default values provided
   -- (except for string types!)
   attrTypeNew :: Attribute -> CTypeExpr  
   attrTypeNew (Attribute _ t k False) =
     case t of (IntDom Nothing)        -> if k==PKey 
                                          then baseType (erdgen "Key")
                                          else prelType "Int"
               (IntDom (Just _))       -> maybeType (prelType "Int")
               (FloatDom Nothing)      -> prelType "Float"
               (FloatDom (Just _))     -> maybeType (prelType "Float")
               (StringDom Nothing)     -> prelType "String"
               (StringDom (Just _))    -> prelType "String"
               (BoolDom Nothing)       -> prelType "Bool"
               (BoolDom (Just _))      -> maybeType (prelType "Bool")
               (DateDom Nothing)       -> baseType ("Time","CalendarTime")
               (DateDom (Just _))      -> maybeType
                                            (baseType ("Time","CalendarTime"))
               (UserDefined s Nothing) -> baseType (userMod s)
               (UserDefined s (Just _))-> maybeType (baseType (userMod s))
               (KeyDom _)              -> baseType (erdgen "Key")
   attrTypeNew (Attribute _ t k True) = 
     case t of (IntDom _)       -> if k==PKey 
                                   then maybeType (baseType (erdgen "Key"))
                                   else maybeType (prelType "Int")
               (FloatDom _)     -> maybeType (prelType "Float")
               (StringDom _)    -> prelType "String"
               (BoolDom _)      -> maybeType (prelType "Bool")
               (DateDom _)      -> maybeType (baseType ("Time","CalendarTime"))
               (UserDefined s _)-> maybeType (baseType (userMod s))
               (KeyDom _)       -> maybeType (baseType (erdgen "Key"))
          

updateEntity :: QName -> [Entity] -> [Relationship] -> CVisibility -> CFuncDecl
updateEntity (s,eName) es rs v =
  let e = lowerFirst eName
      p = e ++ "_p"
      ts = tests (s,eName) es rs Update
      f = applyF (db "updateDBEntry")
                 [CSymbol (s,e++"Entry"),
                  applyF (s,e++"KeyToKey")
                         [applyF (s,e++"Key") [cvar p]],
                  applyF (s,e++"2tuple") [cvar p]]
  in
  cmtfunc ("Updates an existing "++eName++" entity.")
        (s,"update" ++ eName) 1 v
        ((baseType (s,eName)) ~> transactType)
        [CRule [var p]
               [noGuard (foldr (\a b -> applyF (db "|>>") [a,b]) f ts)]
               []]                     

deleteEntity :: QName -> [Entity] -> [Entity] -> [Relationship] -> CVisibility
             -> CFuncDecl
deleteEntity (s,eName) es esAll rsAll v =
  let e = lowerFirst eName
      p = e ++ "_p"
      ts = tests (s,eName) es rsAll (Delete esAll)
      f = applyF (db "deleteDBEntry")
                 [CSymbol (s,e++"Entry"),
                  applyF (s,e++"KeyToKey")
                         [applyF (s,e++"Key") [cvar p]]]
  in
  cmtfunc ("Deletes an existing "++eName++" entity.")
        (s,"delete" ++ eName) 1 v
        ((baseType (s,eName)) ~> transactType)
        [CRule [var p]
               [noGuard (foldr (\a b -> applyF (db "|>>") [a,b]) f ts)]
               []]                     

data TestType = New [Relationship] [String] [Relationship] [String] [Relationship] [String] [Relationship]
              | Update 
              | Delete [Entity]
              | Consistency

tests :: QName -> [Entity] -> [Relationship] -> TestType -> [CExpr]
tests (str,enName) entities rels tt = 
  let entity  = head (filter (isEntityNamed enName) entities)
      uas     = filter isUnique (entityAttributes entity)
      fkas    = filter isForeignKey (entityAttributes entity)
      ers     = relationshipsForEntityName (entityName entity) rels
      maxrsA  = filter (isMaxRelForEntityA enName) ers 
      maxrsB  = filter (isMaxRelForEntityB enName) ers
      maxrsAC = filter (isMaxRelForEntityAC enName) ers
      maxrsBC = filter (isMaxRelForEntityBC enName) ers
      minrsA  = filter (isRelWithRangeForEntityA isMinRange enName) ers
      minrsB  = filter (isRelWithRangeForEntityB isMinRange enName) ers
  in
  case tt of New exactRs exactP maxRs maxP minMaxRs minMaxP rsAll
                    -> (map (attributeToUniqueTest (str,enName)) uas)
                         ++ (map (fKeyExistTest (str,enName)) fkas)
                         ++ (map (relToMaxTestA (str,enName)) maxrsA)
                         ++ (map (relToMaxTestB (str,enName)) maxrsB)
                         ++ (dupTestExact exactRs exactP)
                         ++ (dupTestMax maxP)
                         ++ (dupTestMinMax minMaxRs minMaxP)
                         ++ (fKeyExistExact exactRs exactP str rsAll)
                         ++ (fKeyExistMax maxRs maxP str rsAll)
                         ++ (fKeyExistMinMax minMaxRs minMaxP str rsAll)
                         ++ (maxTestMax maxRs maxP)
                         ++ (maxTestMinMax minMaxRs minMaxP)

             Update -> (map (attributeToUniqueTestUpdate (str,enName)) uas)
                         ++ (map (fKeyExistTestUpdate (str,enName)) fkas)
                         ++ (map (relToMaxTestUpdateA (str,enName)) maxrsA)
                         ++ (map (relToMaxTestUpdateB (str,enName)) maxrsB)

             Delete esAll ->
               let entinfk = filter (hasForeignKey enName) esAll
                in (map (fKeyExistTestDelete (str,enName)) entinfk)

             Consistency -> (dupKeyTest (str,enName)
                              : (map (attributeToUniqueTestC (str,enName)) uas)
                              ++ (map (fKeyExistTestUpdate (str,enName)) fkas)
                              ++ (map (relToMaxTestAC (str,enName)) maxrsAC)
                              ++ (map (relToMaxTestBC (str,enName)) maxrsBC)
                              ++ (map (relToMinTestA (str,enName)) minrsA)
                              ++ (map (relToMinTestB (str,enName)) minrsB))

  where
    startsIn :: String -> String -> [Relationship] -> String
    startsIn _ _ [] = error "missing relationship"
    startsIn n en (Relationship _ [REnd en1 _ _, REnd en2 _ _]:rs) 
      | en == en2 && n/=en1 = en1
      | otherwise = startsIn n en rs

    dupTestExact :: [Relationship] -> [String] -> [CExpr]
    dupTestExact [] _ = []
    dupTestExact (Relationship _ [_, REnd _ _ (Exactly i)]:exactRs) exactP =
      let (ip, restp) = splitAt i exactP
      in
      applyF (erdgen "duplicatePTest")
             [foldr (\a b -> applyF (pre ":") [a, b])
                    (constF (pre "[]"))
                    (map cvar ip)]
        : dupTestExact exactRs restp
   
    dupTestMax :: [String] -> [CExpr]
    dupTestMax [] = []
    dupTestMax (p:maxP) =
      applyF (erdgen "duplicatePTest")
             [cvar p]
        : dupTestMax maxP

    dupTestMinMax :: [Relationship] -> [String] -> [CExpr]
    dupTestMinMax [] _ = []
    dupTestMinMax (Relationship _ [_, REnd _ _ (Between i _)]:minMaxRs) minMaxP =
      let (ip, (p:restp)) = splitAt i minMaxP
      in
      applyF (erdgen "duplicatePTest")
             [foldr (\a b -> applyF (pre ":") [a, b]) (cvar p) (map cvar ip)]
        : dupTestMinMax minMaxRs restp

    maxTestMax :: [Relationship] -> [String] -> [CExpr]
    maxTestMax [] _ = []
    maxTestMax (Relationship _ [_, REnd _ _ (Between _ (Max max))]:maxRs) (p:maxP) =
      applyF (erdgen "maxPTest") [CLit (CIntc max), cvar p]
        : maxTestMax maxRs maxP

    maxTestMinMax :: [Relationship] -> [String] -> [CExpr]
    maxTestMinMax [] _ = []
    maxTestMinMax (Relationship _ [_, REnd _ _ (Between min (Max max))]
                    : minMaxRs) minMaxP =
      let (_, (p:restp)) = splitAt min minMaxP
      in
      applyF (erdgen "maxPTest") [CLit (CIntc (max-min)), cvar p]
        : maxTestMinMax minMaxRs restp
   

    fKeyExistExact :: [Relationship] -> [String] -> String -> [Relationship]-> [CExpr]
    fKeyExistExact [] _ _ _ = []
    fKeyExistExact (Relationship _ [REnd n _ _, REnd rn _ (Exactly i)]:exactRs) exactP s rs =
      let (ip, restp) = splitAt i exactP
          eN = startsIn n rn rs
       in map (\v -> existsDBKeyCall (s,eN) (Just (cvar v))) ip ++
          fKeyExistExact exactRs restp s rs

    fKeyExistMax :: [Relationship] -> [String] -> String -> [Relationship]-> [CExpr]
    fKeyExistMax [] _ _ _ = []
    fKeyExistMax (Relationship _ [REnd n _ _, REnd rn _ _]:maxRs) (p:maxP) s rs =
      let eN = startsIn n rn rs
      in
      applyF (pre "mapT_") [existsDBKeyCall (s,eN) Nothing, cvar p]
        : fKeyExistMax maxRs maxP s rs
 
    fKeyExistMinMax :: [Relationship] -> [String] -> String -> [Relationship]-> [CExpr]
    fKeyExistMinMax [] _ _ _ = []
    fKeyExistMinMax (Relationship _ [REnd n _ _, REnd rn _ (Between i _)]:minMaxRs) minMaxP s rs =
      let (ip, (p:restp)) = splitAt i minMaxP
          eN = startsIn n rn rs
      in
      (map (\v -> existsDBKeyCall (s,eN) (Just (cvar v))) ip)    
        ++ [applyF (pre "mapT_") [existsDBKeyCall (s,eN) Nothing, cvar p]]    
        ++ (fKeyExistMinMax minMaxRs restp s rs)
              

    isUnique :: Attribute -> Bool
    isUnique (Attribute _ _ k _) = k == Unique

    isMaxRelForEntityA :: EName -> Relationship -> Bool
    isMaxRelForEntityA e (Relationship _ [REnd e1 _ c1, _]) = e==e1 &&
      case c1 of Between _ (Max i) -> i>1
                 _                 -> False

    isMaxRelForEntityB :: EName -> Relationship -> Bool
    isMaxRelForEntityB e (Relationship _ [_, REnd e2 _ c2]) = e==e2 &&
      case c2 of Between _ (Max i) -> i>1
                 _                 -> False

    isMaxRelForEntityAC :: EName -> Relationship -> Bool
    isMaxRelForEntityAC e (Relationship _ [REnd e1 _ _, REnd _ _ c2]) = e==e1 &&
      case c2 of Between _ (Max i) -> i>1
                 Exactly i         -> i>1
                 _                 -> False

    isMaxRelForEntityBC :: EName -> Relationship -> Bool
    isMaxRelForEntityBC e (Relationship _ [REnd _ _ c1, REnd e2 _ _]) = e==e2 &&
      case c1 of Between _ (Max i) -> i>1
                 Exactly i         -> i>1
                 _                 -> False

    fKeyExistTest :: QName -> Attribute -> CExpr
    fKeyExistTest (s,_) (Attribute an (KeyDom kd) _ isnull) =
      let existsKeyCall = existsDBKeyCall (s,kd)
          keyvar = cvar (lowerFirst an ++"_p")
       in if isnull
          then applyMaybe (constF (db "doneT")) (existsKeyCall Nothing) keyvar
          else existsKeyCall (Just keyvar)

    fKeyExistTestUpdate :: QName -> Attribute -> CExpr
    fKeyExistTestUpdate (s,eName) (Attribute an (KeyDom kd) _ isnull) =
      let existsKeyCall = existsDBKeyCall (s,kd)
          keyarg = applyF (s,lowerFirst eName ++ an) 
                          [cvar (lowerFirst eName ++"_p")]
       in if isnull
          then applyMaybe (constF (db "doneT")) (existsKeyCall Nothing) keyarg
          else existsKeyCall (Just keyarg)

    fKeyExistTestDelete :: QName -> Entity -> CExpr
    fKeyExistTestDelete (s,eName) (Entity feName attrs) =
      let fkattrs = map (\a -> (attributeName a, isNullAttribute a))
                        (foreignKeyAttributes eName attrs)
          fkarg = applyF (s,lowerFirst eName++"Key") 
                         [cvar (lowerFirst eName ++"_p")]
       in seqTrans (map (\ (fkaName,fkisnull) ->
                           applyF (erdgen "requiredForeignDBKey")
                                  [string2ac feName,
                                   CSymbol (s,lowerFirst feName ++"Entry"),
                                   CSymbol (s,"keytuple2"++feName),
                                   CSymbol (s,lowerFirst feName ++ fkaName),
                                   if fkisnull
                                   then applyJust fkarg
                                   else fkarg ])
                        fkattrs)

    attributeToUniqueTest :: QName -> Attribute -> CExpr
    attributeToUniqueTest (s,eName) (Attribute an dom _ _) =
      let ename = lowerFirst eName in
      applyF (erdgen "unique")
             [string2ac s,
              CSymbol (s,ename++"Entry"),
              CSymbol (s,"keytuple2"++eName),
              CSymbol (s,ename++an),
              let cv = cvar (lowerFirst an ++"_p")
               in if hasDefault dom
                  then applyMaybe (getDefault dom) (constF (pre "id")) cv
                  else cv]

    attributeToUniqueTestUpdate :: QName -> Attribute -> CExpr
    attributeToUniqueTestUpdate (s,eName) (Attribute an _ _ _) =
      let ename = lowerFirst eName in
      applyF (erdgen "uniqueUpdate")
             [string2ac s,
              CSymbol (s,ename++"Entry"),
              CSymbol (s,"keytuple2"++eName),
              applyF (pre ".") [CSymbol (s,ename++"KeyToKey"),
                                CSymbol (s,ename++"Key")],
              CSymbol (s,ename++an),
              cvar (ename++"_p")]

    {- different unique for update,
       and instead of cvar (lowerFirst an ++"_p") : 
       applyF (s, lowerFirst eName ++an) [cvar (lowerFirst eName++"_p")]-}

    attributeToUniqueTestC :: QName -> Attribute -> CExpr
    attributeToUniqueTestC (s,eName) (Attribute an _ _ _) =
      let ename = lowerFirst eName in
      applyF (erdgen "uniqueC") 
             [string2ac s,
              CSymbol (s,ename++"Entry"),
              CSymbol (s,"keytuple2"++eName),
              CSymbol (s,ename++an),
              cvar (ename++"_p")]

    relToMaxTestA :: QName -> Relationship -> CExpr
    relToMaxTestA (s,eName) (Relationship rn [REnd _ _ c1, REnd e2 _ _]) =
      relToMaxTest (s,eName) (cardMaximum c1) (combineIds [e2,rn,"Key"])

    relToMaxTestB :: QName -> Relationship -> CExpr
    relToMaxTestB (s,eName) (Relationship rn [REnd e1 _ _, REnd _ _ c2]) =
      relToMaxTest (s,eName) (cardMaximum c2) (combineIds [e1,rn,"Key"])

    relToMaxTest :: QName -> Int -> String -> CExpr
    relToMaxTest (s,eName) m attrName =
      let ename = lowerFirst eName in
      applyF (erdgen "maxTest")
             [string2ac eName,
              CSymbol (s,ename++"Entry"),
              CSymbol (s,"keytuple2"++ename),
              CSymbol (s,ename++attrName),
              CLit (CIntc m), 
              cvar (lowerFirst attrName++"_p")]
      
    relToMaxTestUpdateA :: QName -> Relationship -> CExpr
    relToMaxTestUpdateA (s,eName) (Relationship rn [REnd _ _ c1, REnd e2 _ _]) =
      relToMaxTestUpdate (s,eName) (cardMaximum c1) (combineIds [e2,rn,"Key"])

    relToMaxTestUpdateB :: QName ->  Relationship -> CExpr
    relToMaxTestUpdateB (s,eName) (Relationship rn [REnd e1 _ _, REnd _ _ c2]) =
      relToMaxTestUpdate (s,eName) (cardMaximum c2) (combineIds [e1,rn,"Key"])

    relToMaxTestUpdate :: QName -> Int -> String -> CExpr
    relToMaxTestUpdate (s,eName) m attrName =
      let ename = lowerFirst eName in
      applyF (erdgen "maxTestUpdate")
             [string2ac eName,
              CSymbol (s,ename++"Entry"),
              CSymbol (s,"keytuple2"++ename),
              CSymbol (s,ename++"Key"),
              CSymbol (s,ename++attrName),
              CLit (CIntc m),
              cvar (ename++"_p")]

    relToMaxTestAC :: QName -> Relationship -> CExpr
    relToMaxTestAC (s,eName) (Relationship _ [REnd e1 _ _, REnd e2 _ c2]) =
      relToMaxTestC (s,eName)
                    (cardMaximum c2)
                    (combineIds [e1,e2,"Key"])         
                    e2        
    relToMaxTestBC :: QName -> Relationship -> CExpr
    relToMaxTestBC (s,eName) (Relationship _ [REnd e1 _ c1, REnd e2 _ _]) =
      relToMaxTestC (s,eName)
                    (cardMaximum c1)
                    (combineIds [e2,e1,"Key"])         
                    e1
    relToMaxTestC :: QName -> Int -> String -> String -> CExpr
    relToMaxTestC (s,eName) m attrName eN =
      let en = lowerFirst eN in
      applyF (erdgen "maxTestC")
             [string2ac eN,
              CSymbol (s,en++"Entry"),
              CSymbol (s,"keytuple2"++eN),
              CSymbol (s,en++attrName),
              CLit (CIntc m), 
              applyF (s, lowerFirst eName ++ "Key")
                     [cvar (lowerFirst eName++"_p")]]

    dupKeyTest :: QName -> CExpr
    dupKeyTest (s,eName) = 
      applyF (erdgen "duplicateKeyTest")
             [CSymbol (s, lowerFirst eName ++"Entry")]

    relToMinTestA :: QName -> Relationship -> CExpr
    relToMinTestA (s,eName) (Relationship _ [REnd e1 _ _, REnd e2 _ c2]) =
      relToMinTest (s,eName)
                   (cardMinimum c2)
                   (combineIds [e1,e2,"Key"])         
                   e2      
    relToMinTestB :: QName -> Relationship -> CExpr
    relToMinTestB (s,eName) (Relationship _ [REnd e1 _ c1, REnd e2 _ _]) =
      relToMinTest (s,eName)
                   (cardMinimum c1)
                   (combineIds [e2,e1,"Key"])         
                   e1  
    relToMinTest :: QName -> Int -> String -> String -> CExpr
    relToMinTest (s,eName) m attrName eN =
      let en = lowerFirst eN in
      applyF (erdgen "minTestC")
             [string2ac eN,
              CSymbol (s,en++"Entry"),
              CSymbol (s,"keytuple2"++eN),
              CSymbol (s,en++attrName),
              CLit (CIntc m), 
              applyF (s, lowerFirst eName ++ "Key")
                     [cvar (lowerFirst eName++"_p")]]


------------------------------------------------------------------
-- Generation of dynamic predicates for relationships
rel2code :: Option -> String -> [Entity] -> Relationship -> [CFuncDecl]
rel2code _ name es r = 
  if isGeneratedR r 
  then rolesR name r es
  else roles name r

isGeneratedR (Relationship n _) = n == ""

--generierte Beziehung als Teil der Umsetzung einer n:m Beziehung
--Pfeil zeigt auf generierte Entitaet, also steht der Rollenname immer im 2. REnd
-- bei (Exactly i), i>1, i Parameter fuer die Fremdschluessel
rolesR :: String -> Relationship -> [Entity] -> [CFuncDecl]
rolesR name (Relationship _ [REnd e1 _ _, REnd e2 r2 c2]) es =
  let e = head (filter (\en -> isEntityNamed e2 en) es)
      (f1,f2) = (nthFK 1 e, nthFK 2 e)
      exactly = case c2 of Exactly i -> i>1
                           _         -> False
      rolecmt = "Dynamic predicate representing role \""++r2++"\"."
  in
  if exactly
  then [] -- TODO: add some implementation for SQLite database
          -- (e.g., instead of Dynamic, implement a Query for this task)
    {-
       let i = exact c2
           f = if f1 == e1 then f2 else f1
       in
       [cmtfunc rolecmt
          (name, r2) (i+1) Public 
          ((baseType (name, e1++"Key")) 
             ~> (foldr (\a b -> a ~> b)
                      (baseType (db "Dynamic"))
                      (replicate i (baseType (name, f++"Key")))))  
          [CRule (var "key" : map (var . ("key"++) . show) [1..i])
                 [noGuard 
                   (applyF (db "|>")
                      [foldr1 (\a b -> applyF (db "<>") [a,b])
                              (map ((\k -> applyF (name, lowerFirst e2) [k, cvar "key"]) . 
                                              cvar . ("key"++) . show) 
                                   [1..i]),
                       foldr1 (\a b -> applyF (pre "&&") [a,b]) 
                              (map (\ (a,b) -> 
                                       applyF (pre "/=") 
                                              (map (cvar . ("key"++) . show) [a,b]))
                                   [(a,b) | a <- [1..i], b <- [a ..i], a /= b])])]
                 []]]
    -}
  else if (f1 == e1)
       then [cmtfunc rolecmt (name,r2) 2 Public
              ((baseType (name, f1 ++ "Key")) ~> (baseType (name, f2 ++ "Key"))
                 ~> (baseType (db "Dynamic")))
              [CRule []
                     [noGuard (CSymbol (name, lowerFirst e2))]
                     []]]
       else [cmtfunc rolecmt (name,r2) 2 Public
              ((baseType (name, f2 ++ "Key")) ~> (baseType (name, f1 ++ "Key"))
                 ~> (baseType (db "Dynamic")))
              [CRule []
                     [noGuard (applyF (pre "flip") [CSymbol (name, lowerFirst e2)])]
                     []]]
  where
    exact (Exactly i) = i      
    
    nthFK :: Int -> Entity -> EName
    nthFK _ (Entity _ []) = error "Keine Fremdschluessel mehr vorhanden"
    nthFK nth (Entity n ((Attribute _ t  _ _):attrs)) = 
       case t of KeyDom ename -> if nth == 1 
                                 then ename
                                 else nthFK (nth-1) (Entity n attrs)
                 _            -> nthFK nth (Entity n attrs)
          
    

-- generate code for a relationship that is implemented by a foreign key
roles :: String -> Relationship -> [CFuncDecl]
roles name (Relationship rname [REnd en1 role1 range1, REnd en2 role2 range2]) =
  let rtype = entityKeyType (name,en1) ~> entityKeyType (name,en2)
               ~> baseType (db "Dynamic")
      len1 = lowerFirst en1
      len2 = lowerFirst en2
  in
  [cmtfunc
    ("Dynamic predicate representing the "++rname++" relation\nbetween "++
     en1++" entities and "++en2++" entities.")
    (name, lowerFirst rname) 2 Public rtype
    [CRule
      [var "key1", var "key2"]
      [if isExactRange range1
       then (applyF (pre "=:=")
               [applyF (name,len2 ++ combineIds [en1,rname,"Key"]) [cvar "en"],
                (if isNullRange range1 then applyJust else id) (cvar "key1")],
             applyF (name,len2++"Entry")
               [applyF (name,len2++"KeyToKey") [cvar "key2"],
                applyF (name,len2++"2tuple")   [cvar "en"]])
       else (applyF (pre "=:=")
               [applyF (name,len1 ++ combineIds [en2,rname,"Key"]) [cvar "en"],
                (if isNullRange range2 then applyJust else id) (cvar "key2")],
             applyF (name,len1++"Entry")
               [applyF (name,len1++"KeyToKey") [cvar "key1"],
                applyF (name,len1++"2tuple")   [cvar "en"]])]
      [CLocalVar (1,"en")]],  -- where en free
   cmtfunc
     ("Dynamic predicate representing role \""++role2++"\".")
     (name, role2) 2 Public rtype
     [CRule [] [noGuard (CSymbol (name, lowerFirst rname))] []],    
   cmtfunc
    ("Dynamic predicate representing role \""++role2++"\".")
     (name, role1) 2 Public
     (entityKeyType (name,en2) ~> entityKeyType (name,en1)
        ~> (baseType (db "Dynamic")))
     [CRule [] [noGuard (applyF (pre "flip") [CSymbol (name, role2)])] []]]
 where
  isNullRange range = case range of
    (Between 0 _) -> True
    _             -> False
    
  isExactRange range = case range of
    (Exactly _) -> True
    _           -> False          

relationshipsForEntityName :: String -> [Relationship] -> [Relationship]
relationshipsForEntityName ename rels = filter endsIn rels
 where
  endsIn (Relationship _ ends) = any (\ (REnd n _ _) -> ename == n) ends

-- all attributes are foreign keys
isGenerated :: Entity -> Bool
isGenerated (Entity _ attrs) = length (filter (not . isForeignKey) attrs) == 0

notPKey :: Attribute -> Bool
notPKey (Attribute _ _ k _) = k /= PKey


attrType :: Attribute -> CTypeExpr         -- Null: Maybe
attrType (Attribute _ t k False) =
  case t of (IntDom _)       -> if k==PKey 
                                then baseType (erdgen "Key") 
                                else prelType "Int"
            (FloatDom _)     -> prelType "Float"
            (StringDom _ )   -> prelType "String"
            (BoolDom _)      -> prelType "Bool"
            (DateDom _)      -> baseType ("Time","CalendarTime")
            (UserDefined s _)-> baseType (userMod s)
            (KeyDom _)       -> baseType (erdgen "Key")
            _                -> prelType "Int"
attrType (Attribute _ t k True) = 
  case t of (IntDom _)       -> if k==PKey 
                                then maybeType (baseType (erdgen "Key"))
                                else maybeType (prelType "Int")
            (FloatDom _)     -> maybeType (prelType "Float")
             -- string null values are not handles as Maybe types
            (StringDom _ )   -> prelType "String"
            (BoolDom _)      -> maybeType (prelType "Bool")
            (DateDom _)      -> maybeType (baseType ("Time","CalendarTime"))
            (UserDefined s _)-> maybeType (baseType (userMod s))
            (KeyDom _)       -> maybeType (baseType (erdgen "Key"))
            _                -> maybeType (prelType "Int")


-------------------------------------------------------------------------------
-- Generation of operations for global consistency tests

checkAll :: String -> [Entity] -> CFuncDecl
checkAll name es = 
  cmtfunc "Checks the consistency of the complete database."
          (name, "checkAllData") 0 Public
          transactType
          [CRule []
                 [noGuard (seqTrans (map (checkFunction name) es))]
                 []]

checkFunction :: String -> Entity -> CExpr
checkFunction name (Entity en _) = CSymbol (name, "check"++en)


checkEntity :: String -> Entity -> CFuncDecl
checkEntity name (Entity en _) =
  cmtfunc ("Checks the consistency of the database for "++en++" entities.")
        (name, "check"++en) 0 Public
        transactType
        [CRule []
               [noGuard 
                  (applyF (db "|>>=")
                    [applyF (db "getDB")
                            [applyF (db "allDBKeyInfos")
                                    [CSymbol (name, lowerFirst en++"Entry")]],
                     applyF (pre ".")
                       [applyF (db "mapT_")
                               [CSymbol (name, "check"++en++"Entry")],
                        applyF (pre "map")
                          [applyF (pre "uncurry")
                                  [CSymbol (name, "keytuple2"++en)]]]])]
               []]

checkEntry :: String -> Entity -> [Entity] -> [Relationship] -> CFuncDecl
checkEntry name entity@(Entity en _) es rs =
  let e = lowerFirst en
      t = if (isGenerated entity)
          then generatedEntityTests name entity
          else tests (name, en) es rs Consistency
      argvar = e++"_p"
      arginrhs = any (containsCVar argvar) t
  in
  cfunc (name, "check"++en++"Entry") 1 Private
        ((baseType (name,en)) ~> transactType)
        [CRule [var (if arginrhs then argvar else "_")]
               [noGuard  (if null t
                          then constF (db "returnT")
                          else seqTrans t)] 
               []]

generatedEntityTests :: String -> Entity -> [CExpr]
generatedEntityTests name (Entity en [Attribute a1 (KeyDom d1) _ _, 
                                      Attribute a2 (KeyDom d2) _ _]) =
  let e = lowerFirst en
  in
  [existsDBKeyCall (name,d1)
                   (Just (applyF (name, e++a1) [cvar (e++"_p")])),
   existsDBKeyCall (name,d2) 
                   (Just (applyF (name, e++a2) [cvar (e++"_p")])),
   applyF (erdgen "unique2C")
          [CSymbol (name, e++"Entry"),
           applyF (name,lowerFirst d1 ++ "KeyToKey")
                  [applyF (name, e++a1) [cvar (e++"_p")]],
           applyF (name,lowerFirst d2 ++ "KeyToKey")
                  [applyF (name, e++a2) [cvar (e++"_p")]]]]


-------------------------------------------------------------------------------
-- functions for saving and restoring all data

saveAll :: String -> [Entity] -> [Entity] -> CFuncDecl
saveAll name entities relentities = 
  cmtfunc ("Saves the complete database as Curry terms.\n"++
           "The first argument is the directory where the term files should be stored.")
   (name, "saveAllData") 0 Public
   (prelType "String" ~> CTCons (pre "IO") [prelType "()"])
   [CRule [var "path"]
          [noGuard (CDoExpr (map CSExpr (map saveFunction
                                             (entities ++ relentities))))]
          []]
 where
  saveFunction (Entity en _) =
    applyF (erdgen "saveDBTerms")
           [cvar "path",
            string2ac en,
            CSymbol (name, lowerFirst en++"Entry"),
            CSymbol (name, "keytuple2"++en)]

restoreAll :: String -> [Entity] -> [Entity] -> CFuncDecl
restoreAll name entities relentities = 
  cmtfunc ("Restore the complete database from files containing Curry terms.\n"++
     "The first argument is the directory where the term files are stored.")
    (name, "restoreAllData") 0 Public
    (prelType "String" ~> CTCons (pre "IO") [prelType "()"])
    [CRule [var "path"]
           [noGuard (CDoExpr (map CSExpr (map restoreFunction entities ++
                                          map rRestoreFunction relentities)))]
           []]
 where
  restoreFunction (Entity en _) = let e = lowerFirst en in
    applyF (erdgen "restoreDBTerms")
           [cvar "path",
            string2ac en,
            CSymbol (name,e++"Entry"),
            applyF (pre ".") [CSymbol (name,e++"KeyToKey"),
                              CSymbol (name,e++"Key")],
            CSymbol (name,e++"2tuple")]

  rRestoreFunction (Entity en _) = let e = lowerFirst en in
    applyF (erdgen "restoreDBRelTerms")
           [cvar "path",
            string2ac en,
            CSymbol (name,e++"Entry"),
            CSymbol (name,e++"2tuple")]


---------------------------------------------------------------
-- Auxiliary operations on ERD structures
---------------------------------------------------------------

-- Is the attribute domain a string domain?
isStringDom :: Domain -> Bool
isStringDom dom = case dom of
                   StringDom _ -> True
                   _           -> False

-- Has an attribute domain a default value?
hasDefault :: Domain -> Bool
hasDefault (KeyDom    _) = False
hasDefault (IntDom    d) = isJust d
hasDefault (FloatDom  d) = isJust d
hasDefault (StringDom d) = isJust d
hasDefault (BoolDom   d) = isJust d
hasDefault (DateDom   d) = isJust d
hasDefault (UserDefined _ d) = isJust d

-- Get the default value of the attribute domain:
getDefault (IntDom    (Just d)) = CLit (CIntc d)
getDefault (FloatDom  (Just d)) = CLit (CFloatc d)
getDefault (StringDom (Just d)) = string2ac d
getDefault (BoolDom   (Just d)) =
  CSymbol (pre (if d then "True" else "False"))
getDefault (DateDom   (Just _)) = error "Date default not yet implemented!"
getDefault (UserDefined _ (Just _)) =
  error "UserDefined default not yet implemented!"

--- Checks a range property in a relationship for left entity.
isRelWithRangeForEntityA :: (Cardinality->Bool) -> EName -> Relationship -> Bool
isRelWithRangeForEntityA isc e (Relationship _ [REnd e1 _ _, REnd _ _ c2]) =
  e==e1 && isc c2

--- Checks a range property in a relationship for right entity.
isRelWithRangeForEntityB :: (Cardinality->Bool) -> EName -> Relationship -> Bool
isRelWithRangeForEntityB isc e (Relationship _ [REnd _ _ c1, REnd e2 _ _]) =
  e==e2 && isc c1

--- Is a cardinality with a maximum that must be checked?
isFiniteRange :: Cardinality -> Bool
isFiniteRange card = case card of Between _ Infinite -> False
                                  _                  -> True
    
--- Is a cardinality with a minimum that must be checked?
isMinRange :: Cardinality -> Bool
isMinRange card = case card of Exactly i   -> i>1
                               Between j _ -> j>0

---------------------------------------------------------------
-- Auxiliary functions for AbstractCurry
---------------------------------------------------------------

x :: CPattern
x = CPVar (1,"x")

var :: String -> CPattern
var s = CPVar (1, s)

xn :: Int -> CPattern
xn i = CPVar (1,"x"++(show i))

nix :: CPattern
nix = CPVar (1,"_")

cvar :: String -> CExpr
cvar s = CVar (1,s)

ctvar :: String -> CTypeExpr
ctvar s = CTVar (1,s)

-- A base type.
baseType :: QName -> CTypeExpr
baseType t = CTCons t []

-- A base type from the prelude.
prelType :: String -> CTypeExpr
prelType t = CTCons (pre t) []

-- Construct a Maybe type with a given type argument.
maybeType :: CTypeExpr -> CTypeExpr
maybeType t = CTCons (pre "Maybe") [t]

-- Construct a List type from a given element type.
listType :: CTypeExpr -> CTypeExpr
listType t = CTCons (pre "[]") [t]

--- Constructs a tuple type from list of component types.
tupleType :: [CTypeExpr] -> CTypeExpr
tupleType ts | l==0 = baseType (pre "()")
             | l==1 = head ts
             | otherwise = CTCons (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  ts
 where l = length ts

--- Construct type "Transaction ()"
transactType :: CTypeExpr
transactType = CTCons transTC [prelType "()"]

noGuard :: CExpr -> (CExpr, CExpr)
noGuard e = (CSymbol (pre "success"), e)

-- Construct the key type for an entity from a qualified entitiy name.
entityKeyType :: QName -> CTypeExpr
entityKeyType (modname,ename) = baseType (modname, ename ++ "Key")

-- A symbol from the prelude.
pre :: String -> QName
pre f = ("Prelude", f)

-- A symbol from module Database.
db :: String -> QName
db f = ("KeyDatabaseSQLite", f)

-- Extract a qualified string into a QName:
userMod :: String -> QName
userMod name = let (modname,rname) = break (=='.') name
                in if null rname then ("Prelude",name)
                                 else (modname,tail rname)

transTC :: QName
transTC = db "Transaction"

erdgen :: String -> QName
erdgen f = ("ERDGeneric", f)

lowerFirst :: String -> String
lowerFirst (y:ys) = (toLower y) : ys

applyF :: QName -> [CExpr] -> CExpr
applyF f es = foldl CApply (CSymbol f) es 

constF :: QName -> CExpr
constF f = applyF f []

infixr 9 ~>
(~>) :: CTypeExpr -> CTypeExpr -> CTypeExpr
t1 ~> t2 = CFuncType t1 t2

-- generate function declaration.
cfunc :: QName -> Int -> CVisibility -> CTypeExpr -> [CRule] -> CFuncDecl
cfunc name arity v t rules = 
  CFunc name arity v t (CRules CFlex rules)

-- generate commented function declaration.
cmtfunc :: String -> QName -> Int -> CVisibility -> CTypeExpr -> [CRule]
        -> CFuncDecl
cmtfunc cmt name arity v t rules = 
  CmtFunc cmt name arity v t (CRules CFlex rules)

--- Converts a list of AbstractCurry expressions into an
--- AbstractCurry representation of this list.
list2ac :: [CExpr] -> CExpr
list2ac []     = constF (pre "[]")
list2ac (c:cs) = applyF (pre ":") [c, list2ac cs]

string2ac :: String -> CExpr
string2ac []     = constF (pre "[]")
string2ac (c:cs) = applyF (pre ":") [CLit (CCharc c), string2ac cs]

-- does an expression contain a variable with a given name?
containsCVar :: String -> CExpr -> Bool
containsCVar v cexp = case cexp of
  CVar (_,n)   -> v==n
  CLit _       -> False
  CSymbol _    -> False
  CApply e1 e2 -> containsCVar v e1 ||  containsCVar v e2
  CLambda _ e  -> containsCVar v e -- we ingore shadowing for simplicity
  _            -> True -- for simplicity we ignore the other case
                       -- since they don't occur in our context

-- AbstractCurry call to existsDBKey for an entity and a last key argument:
existsDBKeyCall :: QName -> Maybe CExpr -> CExpr
existsDBKeyCall (mname,eName) Nothing =
  let ename = lowerFirst eName
   in CLambda [var "x"]
        (applyF (erdgen "existsEntryWithDBKey")
                [string2ac eName,
                 CSymbol (mname,ename++"Entry"),
                 applyF (mname,ename++"KeyToKey") [cvar "x"]])
existsDBKeyCall (mname,eName) (Just lastarg) =
  let ename = lowerFirst eName
   in applyF (erdgen "existsEntryWithDBKey")
             [string2ac eName,
              CSymbol (mname,ename++"Entry"),
              applyF (mname,ename++"KeyToKey") [lastarg]]

-- Sequential composition of a non-empty list of AbstractCurry calls with "|>>"
seqTrans :: [CExpr] -> CExpr
seqTrans = foldr1 (\a b -> applyF (db "|>>") [a,b])

-- Code to apply the Just constructor:
applyJust :: CExpr -> CExpr
applyJust a = applyF (pre "Just") [a]

-- Code to apply the maybe function:
applyMaybe :: CExpr -> CExpr -> CExpr -> CExpr
applyMaybe a1 a2 a3 = applyF (pre "maybe") [a1,a2,a3]

--- Constructs a tuple expression from list of component expressions.
tupleExpr :: [CExpr] -> CExpr
tupleExpr es | l==0 = constF (pre "()")
             | l==1 = head es
             | otherwise = applyF (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  es
 where l = length es

--- Constructs a tuple pattern from list of component patterns.
tuplePattern :: [CPattern] -> CPattern
tuplePattern ps
  | l==0 = CPComb (pre "()") []
  | l==1 = head ps
  | otherwise = CPComb (pre ('(' : take (l-1) (repeat ',') ++ ")")) ps
 where l = length ps

