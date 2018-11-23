--- Implementation of the Pretty library using
--- [linear-time, bounded implementation](http://www.cs.kent.ac.uk/pubs/2006/2381/index.html)
---  by Olaf Chitil.
---
--- @author Sebastian Fischer, Bjoern Peemoeller, Jan Tikovsky
--- @version October 2017
--- @category general
------------------------------------------------------------------------------

module Text.PrettyImpl where

import qualified Dequeue as Q (Queue, cons, empty, matchHead, matchLast)

-- The abstract data type Doc represents pretty documents.
data Doc = Doc (Tokens -> Tokens)

-- Extract the internal representation from a document.
deDoc :: Doc -> Tokens -> Tokens
deDoc (Doc d) = d

type Horizontal      = Bool
type Remaining       = Int
type Width           = Int
type Position        = Int
type StartPosition   = Position
type EndPosition     = Position
type Out             = Remaining -> Margins -> FormatHistory -> String

-- Type of a `group output function`: Takes information whether group content
-- should be formatted horizontally or vertically and a continuation to output
-- parts of the document which come after the group
type OutGroupPrefix  = Horizontal -> Out -> Out
type Margins         = [Int]

-- A nesting is either an alignment or a relative indentation
data Nesting         = Align | Inc Int

-- text colorisation
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default

-- console intensity
data Intensity       = Faint | Normal | Bold

-- support of blinking text
data BlinkMode       = Off | Slow | Rapid

-- text formatting statement
data FormatStm
  = SetForeground   Color
  | SetBackground   Color
  | SetIntensity    Intensity
  | SetBlinkMode    BlinkMode
  | SetItalicized   Bool
  | SetUnderlined   Bool
  | SetCrossedout   Bool
  | InverseColoring Bool

type FormatHistory = [FormatStm]

resetFormat :: FormatHistory -> (FormatStm, FormatHistory)
resetFormat [] = error "Pretty.resetFormat2: illegal format history"
resetFormat (stm:stms) = case stm of
  SetForeground   _ -> (SetForeground   (prevFGColor   stms), stms)
  SetBackground   _ -> (SetBackground   (prevBGColor   stms), stms)
  SetIntensity    _ -> (SetIntensity    (prevIntensity stms), stms)
  SetBlinkMode    _ -> (SetBlinkMode    (prevBlinkMode stms), stms)
  SetItalicized   b -> (SetItalicized   (not              b), stms)
  SetUnderlined   b -> (SetUnderlined   (not              b), stms)
  SetCrossedout   b -> (SetCrossedout   (not              b), stms)
  InverseColoring b -> (InverseColoring (not              b), stms)

-- Find previous foreground color in history
prevFGColor :: FormatHistory -> Color
prevFGColor history = case history of
  []                     -> Default
  (SetForeground c : _ ) -> c
  (_               : hs) -> prevFGColor hs

-- Find previous background color in history
prevBGColor :: FormatHistory -> Color
prevBGColor history = case history of
  []                     -> Default
  (SetBackground c : _ ) -> c
  (_               : hs) -> prevBGColor hs

-- Find previous text intensity in history
prevIntensity :: FormatHistory -> Intensity
prevIntensity history = case history of
  []                    -> Normal
  (SetIntensity i : _ ) -> i
  (_              : hs) -> prevIntensity hs

-- Find previous blinking mode in history
prevBlinkMode :: FormatHistory -> BlinkMode
prevBlinkMode history = case history of
  []                    -> Off
  (SetBlinkMode b : _ ) -> b
  (_              : hs) -> prevBlinkMode hs

applyFormat :: FormatStm -> String
applyFormat (SetForeground   c) = txtMode (colorMode c)
applyFormat (SetBackground   c) = txtMode (colorMode c + 10)
applyFormat (SetIntensity    i) = txtMode (intensityMode i)
applyFormat (SetBlinkMode    b) = txtMode (blinkMode b)
applyFormat (SetItalicized   b) = txtMode (if b then 3 else 23)
applyFormat (SetUnderlined   b) = txtMode (if b then 4 else 24)
applyFormat (SetCrossedout   b) = txtMode (if b then 9 else 29)
applyFormat (InverseColoring b) = txtMode (if b then 7 else 27)

-- Text mode
txtMode :: Int -> String
txtMode m = csiCmd ++ show m ++ "m"
 where
  csiCmd :: String
  csiCmd = '\ESC' : '[' : ""

-- Color mode
colorMode :: Color -> Int
colorMode c = case c of
  Black   -> 30
  Red     -> 31
  Green   -> 32
  Yellow  -> 33
  Blue    -> 34
  Magenta -> 35
  Cyan    -> 36
  White   -> 37
  Default -> 39

-- Intensity mode
intensityMode :: Intensity -> Int
intensityMode i = case i of
  Faint  -> 2
  Normal -> 22
  Bold   -> 1

-- Blink mode
blinkMode :: BlinkMode -> Int
blinkMode b = case b of
  Off   -> 25
  Slow  -> 5
  Rapid -> 6

-- Token sequence. Note that the data type linearizes a document so that
-- a fragment is usually followed by a remaining document.
data Tokens
  = EOD                         -- end of document
  | Empty                Tokens -- empty document
  | Text       String    Tokens -- string
  | LineBreak  (Maybe String) Tokens -- linebreak that will be replaced by the
                                -- separator if the linebreak is undone
  | OpenGroup            Tokens -- Beginning of a group
  | CloseGroup           Tokens -- End       of a group
  | OpenNest   Nesting   Tokens -- Beginning of a nesting
  | CloseNest            Tokens -- End       of a nesting
  | OpenFormat FormatStm Tokens -- Beginning of a formatting statement
  | CloseFormat          Tokens -- End       of a formatting statement

applyNesting :: Nesting -> Width -> Remaining -> Margins -> Margins
applyNesting Align   w r ms = (w - r) : ms
applyNesting (Inc i) _ _ ms = case ms of
  m:_ -> (m + i) : ms
  _   -> error "Pretty.applyNesting: empty margin list"

unApplyNesting :: Margins -> Margins
unApplyNesting []     = error "Pretty.unApplyNesting: empty margin list"
unApplyNesting (_:ms) = ms

addSpaces :: Int -> Tokens -> String
addSpaces m ts = case ts of
  LineBreak  _ _   -> ""
  EOD              -> ""
  Empty        ts' -> addSpaces m ts'
  OpenGroup    ts' -> addSpaces m ts'
  CloseGroup   ts' -> addSpaces m ts'
  OpenNest   _ ts' -> addSpaces m ts'
  CloseNest    ts' -> addSpaces m ts'
  OpenFormat _ ts' -> addSpaces m ts'
  CloseFormat  ts' -> addSpaces m ts'
  Text       _ _   -> replicate m ' '

-- Normalise a token sequence using the following rewriting rules:
--
--   CloseGroup (Text     s ts) => Text s (CloseGroup ts)
--   OpenGroup  (Text     s ts) => Text s (OpenGroup  ts)
--   OpenGroup  (CloseGroup ts) => ts
--
-- Rewriting moves `Text` tokens in and out of groups. The set of `lines`
-- "belonging" to each group, i.e., the set of layouts, is left unchanged.
normalise :: Tokens -> Tokens
normalise = go id
  where
  go co EOD                = co EOD
  go co (Empty         ts) = go co ts
  -- there should be no deferred opening brackets
  go co (OpenGroup     ts) = go (co . open)        ts
  go co (CloseGroup    ts) = go (co . CloseGroup)  ts
  go co (LineBreak  ms ts) = (co . LineBreak ms . go id) ts
  go co (Text       s  ts) = Text s       (go co ts)
  go co (OpenNest   n  ts) = OpenNest   n (go co ts)
  go co (CloseNest     ts) = CloseNest    (go co ts)
  go co (OpenFormat f  ts) = OpenFormat f (go co ts)
  go co (CloseFormat   ts) = CloseFormat  (go co ts)

  open t = case t of
    CloseGroup ts -> ts
    _             -> OpenGroup t

-- Transform a document into a group-closed document by normalising its token
-- sequence.
-- A document is called group-closed, if between the end of every `group` and
-- the next `text` document there is always a `line` document.
doc2Tokens :: Doc -> Tokens
doc2Tokens (Doc d) = normalise (d EOD)

--- `(showWidth w d)` pretty prints document `d` with a page width of `w` characters
--- @param w - width of page
--- @param d - a document
--- @return pretty printed document
showWidth :: Width -> Doc -> String
showWidth w d = noGroup (doc2Tokens d) w 1 w [0] []

-- Compute number of visible ASCII characters
lengthVis :: String -> Int
lengthVis = Prelude.length . filter isVisible
  where
  isVisible c = ord c `notElem` ([5, 6, 7] ++ [16 .. 31])

-- Basic pretty printing algorithm:
--
-- 1. Determine for each group in the document its width, i.e. the space it
--    requires for printing if it was printed horizontally, all in one line.
-- 2. Traverse document tree and keep track of remaining free space in current
--    output line.
--    At the start of a group compare remaining space with width of the group:
--    If the width is smaller or equal, the group is formatted horizontally,
--    otherwise vertically.

-- Determine widths of all groups and produce actual layout by traversing token
-- sequence a single time using continuations:
-- At the start of each group construct a `group output function` which receives
-- formate information and information about the remaining space at the
-- beginning of the group.
-- Since groups can be nested we don't want to update a width value for each
-- surrounding group when processing a token. Instead we introduce an absolute
-- measure of a token's position: The width of a group is the difference between
-- the position of its `CloseGroup` token and the position of its `OpenGroup` token.
-- When traversing the document only the `group output function` of the
-- innermost group is extended. All the other `group output function`s are
-- passed on unchanged. When we come across a `CloseGroup` token we merge the
-- function for the innermost group with the function for the next inner group.

-- noGroup is used when there is currently no deferred group
noGroup :: Tokens -> Width -> Position -> Out
noGroup EOD              _ _ _ _  _  = ""
-- should not occur:
noGroup (Empty       ts) w p r ms fs = noGroup ts w p r ms fs
noGroup (Text      t ts) w p r ms fs = t ++ noGroup ts w (p + l) (r - l) ms fs
  where l = lengthVis t
noGroup (LineBreak _ ts) w p _ ms fs = case ms of
  []  -> error "Pretty.noGroup: illegal line"
  m:_ -> '\n' : addSpaces m ts ++      noGroup  ts w (p + 1) (w - m) ms fs
noGroup (OpenGroup   ts) w p r ms fs = oneGroup ts w p       (p + r) (\_ c -> c) r ms fs
noGroup (CloseGroup  ts) w p r ms fs = noGroup  ts w p       r       ms fs -- may have been pruned
noGroup (OpenNest  n ts) w p r ms fs = noGroup  ts w p       r       (applyNesting n w r ms) fs
noGroup (CloseNest   ts) w p r ms fs = noGroup  ts w p       r       (unApplyNesting ms) fs
noGroup (OpenFormat f ts) w p r ms fs = applyFormat f ++ noGroup ts w p r ms (f:fs)
noGroup (CloseFormat  ts) w p r ms fs = applyFormat f ++ noGroup ts w p r ms ofs
 where
  (f, ofs) = resetFormat fs

-- oneGroup is used when there is one deferred group
-- Whenever the tokens `Text` or `LineBreak` are processed,
-- i.e. the current position is increased,
-- pruneOne checks whether whether the group still fits the line
-- Furthermore the `group output function` is extended with the current token
oneGroup :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix -> Out
oneGroup EOD              _ _ _ _         = error "Pretty.oneGroup: EOD"
-- should not occur:
oneGroup (Empty       ts) w p e outGrpPre = oneGroup ts w p e outGrpPre
oneGroup (Text      s ts) w p e outGrpPre =
  pruneOne ts w (p + l) e (\h cont -> outGrpPre h (outText cont))
 where
  l = lengthVis s
  outText cont r ms fs = s ++ cont (r - l) ms fs
oneGroup (LineBreak Nothing ts) w p _ outGrpPre = outGrpPre False (outLine (noGroup ts w p))
 where
  outLine _    _ []       _  = error "Pretty.oneGroup.outLine: empty margins"
  outLine cont _ ms@(m:_) fs = '\n' : addSpaces m ts ++ cont (w - m) ms fs
oneGroup (LineBreak (Just s) ts) w p e outGrpPre =
  pruneOne ts w (p + l) e (\h cont -> outGrpPre h (outLine h cont))
 where
  l = lengthVis s
  outLine _ _    _ []       _  = error "Pretty.oneGroup.outLine: empty margins"
  outLine h cont r ms@(m:_) fs =
    if h then s ++ cont (r - l) ms fs
         else '\n' : addSpaces m ts ++ cont (w - m) ms fs
oneGroup (OpenGroup   ts) w p e outGrpPre =
  multiGroup ts w p e outGrpPre Q.empty p (\_ cont -> cont)
oneGroup (CloseGroup  ts) w p e outGrpPre = outGrpPre (p <= e) (noGroup ts w p)
oneGroup (OpenNest  n ts) w p e outGrpPre = oneGroup ts w p e
  (\h cont -> outGrpPre h (\r ms fs -> cont r (applyNesting n w r ms) fs))
oneGroup (CloseNest   ts) w p e outGrpPre = oneGroup ts w p e
  (\h cont -> outGrpPre h (\r ms fs -> cont r (unApplyNesting ms) fs))
oneGroup (OpenFormat f ts) w p e outGrpPre = oneGroup ts w p e
  (\h cont -> outGrpPre h (outFormat cont))
 where
  outFormat cont r ms fs = applyFormat f ++ cont r ms (f:fs)
oneGroup (CloseFormat  ts) w p e outGrpPre = oneGroup ts w p e
  (\h cont -> outGrpPre h (outUnformat cont))
 where
  outUnformat cont r ms fs = applyFormat f ++ cont r ms ofs
   where
    (f, ofs) = resetFormat fs

-- multiGroup is used when there are at least two deferred groups
-- Whenever the tokens `Text` or `LineBreak` are processed, i.e. the current position
-- is increased, pruneMulti checks whether whether the outermost group still
-- fits the line.
-- Furthermore the `group output function` of the innermost group is extended
-- with the current token.
-- When we come across a `OpenGroup` token during traversal of the token sequence,
-- the current innermost `group output function` is added to the queue.
-- Reaching a `CloseGroup` token it is checked whether the queue still contains a
-- deferred `group output function`: If the queue is empty, there is only one
-- group left, otherwise there are at least two groups left.
-- In both cases the function for the innermost group is merged with the
-- function for the next inner group
multiGroup :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix
           -> Q.Queue (StartPosition, OutGroupPrefix)
           -> StartPosition -> OutGroupPrefix -> Out
multiGroup EOD              _ _ _ _              _  _  _
  = error "Pretty.multiGroup: EOD"
-- should not occur:
multiGroup (Empty       ts) w p e outGrpPreOuter qs s  outGrpPreInner
  = multiGroup ts w p e outGrpPreOuter qs s outGrpPreInner
multiGroup (Text      t ts) w p e outGrpPreOuter qs s  outGrpPreInner
  = pruneMulti ts w (p+l) e outGrpPreOuter qs s
    (\h cont -> outGrpPreInner h (outText cont))
 where
  l = lengthVis t
  outText cont r ms fs = t ++ cont (r-l) ms fs
multiGroup (LineBreak Nothing ts) w p _ outGrpPreOuter qs _ outGrpPreInner
  = pruneAll outGrpPreOuter qs
 where
  pruneAll outGrpPreOuter' qs' = outGrpPreOuter' False (\r ->
              (case Q.matchLast qs' of
                Nothing -> outGrpPreInner False (outLine (noGroup ts w p))
                Just ((_,outGrpPre),qss) -> pruneAll outGrpPre qss)
                    r)
  outLine _    _ []       _  = error "Pretty.oneGroup.outLine: empty margins"
  outLine cont _ ms@(m:_) fs = '\n' : addSpaces m ts ++ cont (w - m) ms fs
multiGroup (LineBreak (Just s) ts) w p e outGrpPreOuter qs si outGrpPreInner =
  pruneMulti ts w (p + l) e outGrpPreOuter qs si
    (\h cont -> outGrpPreInner h (outLine h cont))
 where
  l = lengthVis s
  outLine _ _    _ []       _  = error "Pretty.multiGroup.outLine: empty margins"
  outLine h cont r ms@(m:_) fs =
    if h then s ++ cont (r-l) ms fs else '\n': addSpaces m ts ++ cont (w-m) ms fs
multiGroup (OpenGroup   ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter (Q.cons (si,outGrpPreInner) qs) p (\_ cont -> cont)
multiGroup (CloseGroup  ts) w p e outGrpPreOuter qs si outGrpPreInner =
  case Q.matchHead qs of
    Nothing -> oneGroup ts w p e
                 (\h cont -> outGrpPreOuter h
                            (\ri -> outGrpPreInner (p<=si+ri) cont ri))
    Just ((s,outGrpPre),qs') ->
      multiGroup ts w p e outGrpPreOuter qs' s
        (\h cont -> outGrpPre h (\ri -> outGrpPreInner (p<=si+ri) cont ri))
multiGroup (OpenNest  n ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si
    (\h cont -> outGrpPreInner h (\r ms fs -> cont r (applyNesting n w r ms) fs))
multiGroup (CloseNest   ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si
    (\h cont -> outGrpPreInner h (\r ms fs -> cont r (unApplyNesting ms) fs))
multiGroup (OpenFormat f ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si
    (\h cont -> outGrpPreInner h (outFormat cont))
 where
  outFormat cont r ms fs = applyFormat f ++ cont r ms (f:fs)
multiGroup (CloseFormat  ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si
    (\h cont -> outGrpPreInner h (outUnformat cont))
 where
  outUnformat cont r ms fs = applyFormat f ++ cont r ms ofs
   where
    (f, ofs) = resetFormat fs

-- pruneOne checks whether the outermost group (in this case there is only one
-- group) still fits in the current line. If it doesn't fit, it applies the
-- corresponding `group output function` (the group is formatted vertically)
-- and continues processing the token sequence
pruneOne :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix -> Out
pruneOne ts w p e outGrpPre | p <= e    = oneGroup ts w p e outGrpPre
                            | otherwise = outGrpPre False (noGroup ts w p)

-- pruneMulti checks whether the outermost group (in this case there are at
-- least two groups) still fits in the current line. If it doesn't fit, it
-- applies the corresponding `group output function` (the last queue entry) and
-- continues checking whether the next outermost group fits
pruneMulti :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix
              -> Q.Queue (StartPosition, OutGroupPrefix)
              -> StartPosition -> OutGroupPrefix -> Out
pruneMulti ts w p e outGrpPreOuter qs si outGrpPreInner
  | p <= e    = multiGroup ts w p e outGrpPreOuter qs si outGrpPreInner
  | otherwise = outGrpPreOuter False (\r ->
                   (case Q.matchLast qs of
                      Nothing -> pruneOne ts w p (si+r) outGrpPreInner
                      Just ((s,outGrpPre),qs') ->
                        pruneMulti ts w p (s+r) outGrpPre qs' si outGrpPreInner)
                          r)

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

-- inspect the token sequence of a document
inspect :: Doc -> Tokens
inspect (Doc d) = normalise (d EOD)
