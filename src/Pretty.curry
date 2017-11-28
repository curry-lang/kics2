------------------------------------------------------------------------------
--- This library provides pretty printing combinators.
--- The interface is that of
--- [Daan Leijen's library](<http://www.cs.uu.nl/~daan/download/pprint/pprint.html)
--- [linear-time, bounded implementation](http://www.cs.kent.ac.uk/pubs/2006/2381/index.html)
---  by Olaf Chitil.
--- Note that the implementation of `fill` and `fillBreak` is not linear-time bounded
--- Support of ANSI escape codes for formatting and colorisation of documents
--- in text terminals (see https://en.wikipedia.org/wiki/ANSI_escape_code)
---
--- @author Sebastian Fischer, Bjoern Peemoeller, Jan Tikovsky
--- @version September 2015
--- @category general
------------------------------------------------------------------------------

module Pretty (

  -- pretty printer and document type
  Doc, pPrint, pretty,

  -- basic document combinators
  empty, isEmpty, text, linesep, line, linebreak, group, softline, softbreak,
  hardline,

  -- alignment combinators
  nest, hang, align, indent,

  -- composition combinators
  combine, (<>), (<+>), ($$), (<$+$>), (</>), (<$$>), (<//>), (<$!$>),

  -- list combinators
  compose, hsep, vsep, vsepBlank, fillSep, sep, hcat,
  vcat, fillCat, cat, punctuate, encloseSep, encloseSepSpaced, hEncloseSep,
  fillEncloseSep, fillEncloseSepSpaced, list, listSpaced, set, setSpaced,
  tupled, tupledSpaced, semiBraces, semiBracesSpaced,

  -- bracketing combinators
  enclose, squotes, dquotes, bquotes, parens,
  parensIf, angles, braces, brackets,

  -- fillers
  fill, fillBreak,

  -- primitive type documents
  char, string, int, float,

  -- character documents
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  squote, dquote, semi, colon, comma, space, dot, backslash, equals,
  larrow, rarrow, doubleArrow, doubleColon, bar, at, tilde,

  -- formatting combinators
  bold, faint, blinkSlow, blinkRapid, italic, underline, crossout, inverse,

  -- colorisation combinators
  black, red, green, yellow, blue, magenta, cyan, white,
  bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite
  ) where

import qualified Dequeue as Q (Queue, cons, empty, matchHead, matchLast)

infixl 5 $$, <$$>, </>,  <//>, <$!$>, <$+$>
infixl 6 <>, <+>

--- Standard printing with a column length of 80.
pPrint :: Doc -> String
pPrint = pretty 80

--- The abstract data type Doc represents pretty documents.
data Doc = Doc (Tokens -> Tokens)

--- Extract the internal representation from a document.
deDoc :: Doc -> Tokens -> Tokens
deDoc (Doc d) = d

--- The empty document
--- @return an empty document
empty :: Doc
empty = Doc Empty

--- Is the document empty?
isEmpty :: Doc -> Bool
isEmpty (Doc d) = isEmptyText (d EOD)
 where isEmptyText t = case t of Empty EOD -> True
                                 _         -> False

--- The document `(text s)` contains the literal string `s`.
--- The string shouldn't contain any newline ('\n') characters.
--- If the string contains newline characters,
--- the function `string` should be used.
--- @param s - a string without newline (`'\n'`) characters
--- @return a document which contains the literal string
text :: String -> Doc
text s = Doc (Text s)

--- The document `(linesep s)` advances to the next line and indents
--- to the current nesting level. Document `(linesep s)`
--- behaves like `(text s)` if the line break is undone by `group`.
--- @param s - a string
--- @return a document which advances to the next line or behaves
---         like `(text s)`
linesep :: String -> Doc
linesep = Doc . LineBreak . Just

--- The document `hardline` advances to the next line and indents
--- to the current nesting level. `hardline` cannot be undone by `group`.
--- @return a document which advances to the next line
hardline :: Doc
hardline = Doc (LineBreak Nothing)

--- The document `line` advances to the next line and indents to the current
--- nesting level. Document `line` behaves like `(text " ")` if the line break
--- is undone by `group`.
--- @return a document which advances to the next line or behaves
---         like `(text " ")`
line :: Doc
line = linesep " "

--- The document `linebreak` advances to the next line and indents to
--- the current nesting level. Document `linebreak` behaves like `(text "")`
--- if the line break is undone by `group`.
--- @return a document which advances to the next line or behaves like
---         `(text "")`
linebreak :: Doc
linebreak = linesep ""

--- The document `softline` behaves like `space` if the resulting output
--- fits the page, otherwise it behaves like `line`.
--- `softline  = group line`
--- @return a document which behaves like `space` or `line`
softline :: Doc
softline = group line

--- The document `softbreak` behaves like `(text "")` if the resulting output
--- fits the page, otherwise it behaves like `line`.
--- `softbreak  = group linebreak`
--- @return a document which behaves like `(text "")` or `line`
softbreak :: Doc
softbreak = group linebreak

--- The combinator `group` is used to specify alternative layouts.
--- The document `(group x)` undoes all line breaks in document `x`.
--- The resulting line is added to the current line if that fits the page.
--- Otherwise, the document `x` is rendered without any changes.
--- @param d - a document
--- @return document d without line breaks if that fits the page.
group :: Doc -> Doc
group d = Doc (OpenGroup . deDoc d . CloseGroup)

--- The document `(nest i d)` renders document `d` with the current
--- indentation level increased by `i` (See also `hang`,
--- `align` and `indent`).
---
---     nest 2 (text "hello" $$ text "world") $$ text "!"
---
--- outputs as:
---
---     hello
---       world
---     !
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level increased by i
nest :: Int -> Doc -> Doc
nest i d = Doc (OpenNest (Inc i) . deDoc d . CloseNest)

--- The combinator `hang` implements hanging indentation.
--- The document `(hang i d)` renders document `d` with a nesting level set
--- to the current column plus `i`. The following example uses hanging
--- indentation for some text:
---
---     test = hang 4
---              (fillSep
---                 (map text
---                      (words "the hang combinator indents these words !")))
---
--- Which lays out on a page with a width of 20 characters as:
---
---     the hang combinator
---         indents these
---         words !
---
--- The hang combinator is implemented as:
---
---     hang i x  = align (nest i x)
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level set to the current column plus i
hang :: Int -> Doc -> Doc
hang i x = align (nest i x)

--- The document `(align d)` renders document `d with the nesting level
--- set to the current column. It is used for example to implement `hang`.
---
--- As an example, we will put a document right above another one,
--- regardless of the current nesting level:
---
---     x $$ y  = align (x $$ y)
---     test    = text "hi" <+> (text "nice" $$ text "world")
---
--- which will be layed out as:
---
---     hi nice
---        world
---
--- @param d - a document
--- @return document d with the nesting level set to the current column
align :: Doc -> Doc
align d = Doc (OpenNest Align . deDoc d . CloseNest)

--- The document `(indent i d)` indents document `d` with `i` spaces.
---
---     test  = indent 4 (fillSep (map text
---             (words "the indent combinator indents these words !")))
---
--- Which lays out with a page width of 20 as:
---
---     the indent
---     combinator
---     indents these
---     words !
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level set to the current column
---         plus i
indent :: Int -> Doc -> Doc
indent i d = hang i (spaces i <> d)

--- The document `(combine c d1 d2)` combines document `d1` and `d2` with
--- document `c` in between using `(<>)` with identity `empty`.
--- Thus, the following equations hold.
---
---     combine c d1    empty == d1
---     combine c empty d2    == d2
---     combine c d1    d2    == d1 <> c <> d2 if neither d1 nor d2 are empty
---
--- @param c  - the middle document
--- @param d1 - the left document
--- @param d2 - the right document
--- @return concatenation of d1 and d2 with c in between unless one
---         of the documents is empty
combine :: Doc -> Doc -> Doc -> Doc
combine c d1 d2
  | isEmpty d1 = d2
  | isEmpty d2 = d1
  | otherwise  = enclose d1 d2 c

--- The document `(x <> y)` concatenates document `x` and document `y`.
--- It is an associative operation having `empty` as a left and right unit.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y without seperator with identity empty
(<>) :: Doc -> Doc -> Doc
d1 <> d2
  | isEmpty d1 = d2
  | isEmpty d2 = d1
  | otherwise  = Doc (deDoc d1 . deDoc d2)

--- The document `(x <+> y)` concatenates document `x` and `y` with a
--- `space` in between with identity `empty`.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a space in between
(<+>) :: Doc -> Doc -> Doc
(<+>) = combine space

--- The document `(x $$ y)` concatenates document x and y with a
--- `line` in between with identity `empty`.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a line in between
($$) :: Doc -> Doc -> Doc
($$) = combine line

--- The document `(x <$+$> y)` concatenates document `x` and `y` with a
--- blank line in between with identity `empty`.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a blank line in between
(<$+$>) :: Doc -> Doc -> Doc
(<$+$>) = combine (line <> linebreak)

--- The document `(x </> y)` concatenates document `x` and `y` with
--- a `softline` in between with identity `empty`.
--- This effectively puts `x` and `y` either next to each other
--- (with a `space` in between) or underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a softline in between
(</>) :: Doc -> Doc -> Doc
(</>) = combine softline

--- The document `(x <$$> y)` concatenates document `x` and `y` with a
--- `linebreak` in between with identity `empty`.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a linebreak in between
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = combine linebreak

--- The document `(x <//> y)` concatenates document `x` and `y` with a
--- `softbreak` in between with identity `empty`.
--- This effectively puts `x` and `y` either right next to each other
--- or underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a softbreak in between
(<//>) :: Doc -> Doc -> Doc
(<//>) = combine softbreak

--- The document `(x <$!$> y)` concatenates document `x` and `y` with a
--- `hardline` in between with identity `empty`.
--- This effectively puts `x` and `y` underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a hardline in between
(<$!$>) :: Doc -> Doc -> Doc
(<$!$>) = combine hardline

--- The document `(compose f xs)` concatenates all documents `xs`
--- with function `f`.
--- Function `f` should be like `(<+>)`, `($$)` and so on.
--- @param f  - a combiner function
--- @param xs - a list of documents
--- @return concatenation of documents
compose :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
compose _ []        = empty
compose op ds@(_:_) = foldr1 op ds -- no seperator at the end

--- The document `(hsep xs)` concatenates all documents `xs`
--- horizontally with `(<+>)`.
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
hsep :: [Doc] -> Doc
hsep = compose (<+>)

--- The document `(vsep xs)` concatenates all documents `xs` vertically with
--- `($$)`. If a group undoes the line breaks inserted by `vsep`,
--- all documents are separated with a `space`.
---
---     someText = map text (words ("text to lay out"))
---     test     = text "some" <+> vsep someText
---
--- This is layed out as:
---
---     some text
---     to
---     lay
---     out
---
--- The `align` combinator can be used to align the documents
--- under their first element:
---
---     test     = text "some" <+> align (vsep someText)
---
--- This is printed as:
---
---     some text
---          to
---          lay
---          out
---
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vsep :: [Doc] -> Doc
vsep = compose ($$)

--- The document `vsep xs` concatenates all documents `xs` vertically with
--- `(<$+$>)`. If a group undoes the line breaks inserted by `vsepBlank`,
--- all documents are separated with a `space`.
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vsepBlank :: [Doc] -> Doc
vsepBlank = compose (<$+$>)

--- The document `(fillSep xs)` concatenates documents `xs` horizontally with
--- `(</>)` as long as its fits the page, than inserts a
--- `line` and continues doing that for all documents in `xs`.
--- `fillSep xs  = foldr (</>) empty xs`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
fillSep :: [Doc] -> Doc
fillSep = compose (</>)

--- The document `(sep xs)` concatenates all documents `xs` either horizontally
--- with `(<+>)`, if it fits the page, or vertically
--- with `($$)`.
--- `sep xs  = group (vsep xs)`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents, if it fits the page,
--- or vertical concatenation else
sep :: [Doc] -> Doc
sep = group . vsep

--- The document `(hcat xs)` concatenates all documents `xs` horizontally
--- with `(<>)`.
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
hcat :: [Doc] -> Doc
hcat = compose (<>)

--- The document `(vcat xs)` concatenates all documents `xs` vertically
--- with `(<$$>)`. If a `group` undoes the line breaks inserted by `vcat`,
--- all documents are directly concatenated.
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vcat :: [Doc] -> Doc
vcat = compose (<$$>)

--- The document `(fillCat xs)` concatenates documents `xs` horizontally
--- with `(<//>)` as long as its fits the page, than inserts a `linebreak`
--- and continues doing that for all documents in `xs`.
--- `fillCat xs  = foldr (<//>) empty xs`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
fillCat :: [Doc] -> Doc
fillCat = compose (<//>)

--- The document `(cat xs)` concatenates all documents `xs` either horizontally
--- with `(<>)`, if it fits the page, or vertically with
--- `(<$$>)`.
--- `cat xs  = group (vcat xs)`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
cat :: [Doc] -> Doc
cat = group . vcat

--- `(punctuate p xs)` concatenates all documents `xs` with document `p` except
--- for the last document.
---
---     someText = map text ["words","in","a","tuple"]
---     test     = parens (align (cat (punctuate comma someText)))
---
--- This is layed out on a page width of 20 as:
---
---     (words,in,a,tuple)
---
--- But when the page width is 15, it is layed out as:
---
---     (words,
---      in,
---      a,
---      tuple)
---
--- (If you want put the commas in front of their elements instead of at the
--- end, you should use `tupled` or, in general, `encloseSep`.)
--- @param p  - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of documents with p in between
punctuate :: Doc -> [Doc] -> [Doc]
punctuate d ds = go ds
 where
  go []           = []
  go [x]          = [x]
  go (x:xs@(_:_)) = (x <> d) : go xs

--- The document `(encloseSep l r s xs)` concatenates the documents `xs`
--- seperated by `s` and encloses the resulting document by `l` and `r`.
--- The documents are rendered horizontally if that fits the page. Otherwise
--- they are aligned vertically. All seperators are put in front of the
--- elements.
---
--- For example, the combinator `list` can be defined with `encloseSep`:
---
---     list xs  = encloseSep lbracket rbracket comma xs
---     test     = text "list" <+> (list (map int [10,200,3000]))
---
--- Which is layed out with a page width of 20 as:
---
---     list [10,200,3000]
---
--- But when the page width is 15, it is layed out as:
---
---     list [10
---          ,200
---          ,3000]
---
--- @param l  - left document
--- @param r  - right document
--- @param s  - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with s in between) and r
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep l r _ []     = l <> r
encloseSep l r s (d:ds) = align (enclose l r (cat (d : map (s <>) ds)))

--- The document `(encloseSepSpaced l r s xs)` concatenates the documents `xs`
--- seperated by `s` and encloses the resulting document by `l` and `r`.
--- In addition, after each occurrence of `s`, after `l`, and before `r`,
--- a `space` is inserted.
--- The documents are rendered horizontally if that fits the page. Otherwise
--- they are aligned vertically. All seperators are put in front of the
--- elements.
---
--- @param l  - left document
--- @param r  - right document
--- @param s  - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with s in between) and r
encloseSepSpaced :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSepSpaced l r s = encloseSep (l <> space) (space <> r) (s <> space)

--- The document `(hEncloseSep l r s xs)` concatenates the documents `xs`
--- seperated by `s` and encloses the resulting document by `l` and `r`.
---
--- The documents are rendered horizontally.
--- @param l  - left document
--- @param r  - right document
--- @param s  - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with s in between) and r
hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r _ []     = l <> r
hEncloseSep l r s (d:ds) = align (enclose l r (hcat (d : map (s <>) ds)))

--- The document `(fillEncloseSep l r s xs)` concatenates the documents `xs`
--- seperated by `s` and encloses the resulting document by `l` and `r`.
---
--- The documents are rendered horizontally if that fits the page.
--- Otherwise they are aligned vertically.
--- All seperators are put in front of the elements.
--- @param l  - left document
--- @param r  - right document
--- @param s  - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with s in between) and r
fillEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
fillEncloseSep l r _ []     = l <> r
fillEncloseSep l r s (d:ds) = align (enclose l r (fillCat (d : map (s <>) ds)))

--- The document `(fillEncloseSepSpaced l r s xs)` concatenates the documents
--- `xs` seperated by `s` and encloses the resulting document by `l` and `r`.
--- In addition, after each occurrence of `s`, after `l`, and before `r`,
--- a `space` is inserted.
---
--- The documents are rendered horizontally if that fits the page.
--- Otherwise, they are aligned vertically.
--- All seperators are put in front of the elements.
--- @param l  - left document
--- @param r  - right document
--- @param s  - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with s in between) and r
fillEncloseSepSpaced :: Doc -> Doc -> Doc -> [Doc] -> Doc
fillEncloseSepSpaced l r s =
  fillEncloseSep (l <> space) (space <> r) (s <> space)

--- The document `(list xs)` comma seperates the documents `xs` and encloses
--- them in square brackets. The documents are rendered horizontally if
--- that fits the page. Otherwise they are aligned vertically.
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed in square brackets
list :: [Doc] -> Doc
list = encloseSep lbracket rbracket comma

--- Spaced version of `list`
listSpaced :: [Doc] -> Doc
listSpaced = encloseSepSpaced lbracket rbracket comma

--- The document `(set xs)` comma seperates the documents `xs` and encloses
--- them in braces. The documents are rendered horizontally if
--- that fits the page. Otherwise they are aligned vertically.
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed in braces
set :: [Doc] -> Doc
set = encloseSep lbrace rbrace comma

--- Spaced version of `set`
setSpaced :: [Doc] -> Doc
setSpaced = encloseSepSpaced lbrace rbrace comma

--- The document `(tupled xs)` comma seperates the documents `xs` and encloses
--- them in parenthesis. The documents are rendered horizontally if that fits
--- the page. Otherwise they are aligned vertically.
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed in parenthesis
tupled :: [Doc] -> Doc
tupled = encloseSep lparen rparen comma

--- Spaced version of `tupled`
tupledSpaced :: [Doc] -> Doc
tupledSpaced = encloseSepSpaced lparen rparen comma

--- The document `(semiBraces xs)` seperates the documents `xs` with semi colons
--- and encloses them in braces. The documents are rendered horizontally
--- if that fits the page. Otherwise they are aligned vertically.
--- All semi colons are put in front of the elements.
--- @param xs - a list of documents
--- @return documents xs seperated with semi colons and enclosed in braces
semiBraces :: [Doc] -> Doc
semiBraces = encloseSep lbrace rbrace semi

--- Spaced version of `semiBraces`
semiBracesSpaced :: [Doc] -> Doc
semiBracesSpaced = encloseSepSpaced lbrace rbrace semi

--- The document `(enclose l r x)` encloses document `x` between
--- documents `l` and `r` using `(<>)`.
--- `enclose l r x   = l <> x <> r`
--- @param l - the left document
--- @param r - the right document
--- @param x - the middle document
--- @return concatenation of l, x and r
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

--- Document `(squotes x)` encloses document `x` with single quotes `"'"`.
--- @param x - a document
--- @return document x enclosed by single quotes
squotes :: Doc -> Doc
squotes = enclose squote squote

--- Document `(dquotes x)` encloses document `x` with double quotes.
--- @param x - a document
--- @return document x enclosed by double quotes
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

--- Document `(bquotes x)` encloses document `x` with back quotes `"\`"`.
--- @param x - a document
--- @return document x enclosed by `\`` quotes
bquotes  :: Doc -> Doc
bquotes = enclose bquote bquote

--- Document `(parens x)` encloses document `x` in parenthesis,
--- `"("` and `")"`.
--- @param x - a document
--- @return document x enclosed in parenthesis
parens :: Doc -> Doc
parens = enclose lparen rparen

--- Document `(parensIf x)` encloses document `x` in parenthesis,`"("` and `")"`,
--- iff the condition is true.
--- @param x - a document
--- @return document x enclosed in parenthesis iff the condition is true
parensIf :: Bool -> Doc -> Doc
parensIf b s = if b then parens s else s

--- Document `(angles x)` encloses document `x` in angles, `"<"` and `">"`.
--- @param x - a document
--- @return document x enclosed in angles
angles :: Doc -> Doc
angles = enclose langle rangle

--- Document `(braces x)` encloses document `x` in braces, `"{"` and `"}"`.
--- @param x - a document
--- @return document x enclosed in braces
braces :: Doc -> Doc
braces = enclose lbrace rbrace

--- Document `(brackets x)` encloses document `x` in square brackets,
--- `"["` and `"]"`.
--- @param x - a document
--- @return document x enclosed in square brackets
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

--- The document `(char c)` contains the literal character `c`.
--- The character should not be a newline (`\n`),
--- the function `line` should be used for line breaks.
--- @param c - a character (not `\n`)
--- @return a document which contains the literal character c
char :: Char -> Doc
char c = text [c]

--- The document `(string s)` concatenates all characters in `s` using
--- `line` for newline characters and `char` for all other characters.
--- It is used instead of `text` whenever the text contains newline characters.
--- @param s - a string
--- @return a document which contains the string s
string :: String -> Doc
string = hcat . map (\c -> if elem c ['\n','\r'] then line else char c)

--- The document `(int i)` shows the literal integer `i` using `text`.
--- @param i - an integer
--- @return a document which contains the integer i
int :: Int -> Doc
int n = text (show n)

--- The document `(float f)` shows the literal float `f` using `text`.
--- @param f - a float
--- @return a document which contains the float f
float :: Float -> Doc
float x = text (show x)

--- The document `lparen` contains a left parenthesis, `"("`.
--- @return a document which contains a left parenthesis
lparen :: Doc
lparen = char '('

--- The document `rparen` contains a right parenthesis, `")"`.
--- @return a document which contains a right parenthesis
rparen :: Doc
rparen = char ')'

--- The document `langle` contains a left angle, `"<"`.
--- @return a document which contains a left angle
langle :: Doc
langle = char '<'

--- The document `rangle` contains a right angle, `">"`.
--- @return a document which contains a right angle
rangle :: Doc
rangle = char '>'

--- The document `lbrace` contains a left brace, `"{"`.
--- @return a document which contains a left brace
lbrace :: Doc
lbrace = char '{'

--- The document `rbrace` contains a right brace, `"}"`.
--- @return a document which contains a right brace
rbrace :: Doc
rbrace = char '}'

--- The document `lbracket` contains a left square bracket, `"["`.
--- @return a document which contains a left square bracket
lbracket :: Doc
lbracket = char '['

--- The document `rbracket` contains a right square bracket, `"]"`.
--- @return a document which contains a right square bracket
rbracket :: Doc
rbracket = char ']'

--- The document `squote` contains a single quote, `"'"`.
--- @return a document which contains a single quote
squote :: Doc
squote = char '\''

--- The document `dquote` contains a double quote.
--- @return a document which contains a double quote
dquote :: Doc
dquote = char '\"'

--- The document `dquote` contains a `'`'` quote.
--- @return a document which contains a `'`'` quote
bquote :: Doc
bquote = char '`'

--- The document `semi` contains a semi colon, `";"`.
--- @return a document which contains a semi colon
semi :: Doc
semi = char ';'

--- The document `colon` contains a colon, `":"`.
--- @return a document which contains a colon
colon :: Doc
colon = char ':'

--- The document `comma` contains a comma, `","`.
--- @return a document which contains a comma
comma :: Doc
comma = char ','

--- The document `space` contains a single space, `" "`.
---
---     x <+> y   = x <> space <> y
---
--- @return a document which contains a single space
space :: Doc
space = char ' '

--- The document `(spaces n)` contains `n` spaces, when `n` is greater than 0.
--- Otherwise the document is empty.
---
--- @return a document which contains n spaces or the empty document,
---  if n <= 0
spaces :: Int -> Doc
spaces n | n <= 0    = empty
         | otherwise = text $ replicate n ' '

--- The document `dot` contains a single dot, `"."`.
--- @return a document which contains a single dot
dot :: Doc
dot = char '.'

--- The document `backslash` contains a back slash, `"\\"`.
--- @return a document which contains a back slash
backslash :: Doc
backslash = char '\\'

--- The document `equals` contains an equal sign, `"="`.
--- @return a document which contains an equal
equals :: Doc
equals = char '='

--- The document `larrow` contains a left arrow sign, `"<-"`.
--- @return a document which contains a left arrow sign
larrow :: Doc
larrow = text "<-"

--- The document `rarrow` contains a right arrow sign, `"->"`.
--- @return a document which contains a right arrow sign
rarrow :: Doc
rarrow = text "->"

--- The document `doubleArrow` contains an double arrow sign, `"=>"`.
--- @return a document which contains an double arrow sign
doubleArrow :: Doc
doubleArrow = text "=>"

--- The document `doubleColon` contains a double colon sign, `"::"`.
--- @return a document which contains a double colon sign
doubleColon :: Doc
doubleColon = text "::"

--- The document `bar` contains a vertical bar sign, `"|"`.
--- @return a document which contains a vertical bar sign
bar :: Doc
bar = char '|'

--- The document `at` contains an at sign, `"@"`.
--- @return a document which contains an at sign
at :: Doc
at = char '@'

--- The document `tilde` contains a tilde sign, `"~"`.
--- @return a document which contains a tilde sign
tilde :: Doc
tilde = char '~'

--- The document `(fill i d)` renders document `d`. It than appends
--- `space`s until the width is equal to `i`. If the width of `d` is
--- already larger, nothing is appended. This combinator is quite
--- useful in practice to output a list of bindings. The following
--- example demonstrates this.
---
---     types  = [("empty","Doc")
---              ,("nest","Int -> Doc -> Doc")
---              ,("linebreak","Doc")]
---
---     ptype (name,tp)
---            = fill 6 (text name) <+> text "::" <+> text tp
---
---     test   = text "let" <+> align (vcat (map ptype types))
---
--- Which is layed out as:
---
---     let empty  :: Doc
---         nest   :: Int -> Doc -> Doc
---         linebreak :: Doc
---
--- Note that `fill` is not guaranteed to be linear-time bounded since it has to
--- compute the width of a document before pretty printing it
fill :: Int -> Doc -> Doc
fill i d = d <> fill'
  where w     = width d
        fill' = if w >= i then empty else spaces (i - w)

--- The document `(fillBreak i d)` first renders document `d`. It
--- than appends `space`s until the width is equal to `i`. If the
--- width of `d` is already larger than `i`, the nesting level is
--- increased by `i` and a `line` is appended. When we redefine `ptype`
--- in the previous example to use `fillBreak`, we get a useful
--- variation of the previous output:
---
---     ptype (name,tp)
---          = fillBreak 6 (text name) <+> text "::" <+> text tp
---
--- The output will now be:
---
---     let empty  :: Doc
---         nest   :: Int -> Doc -> Doc
---         linebreak
---                :: Doc
---
--- Note that `fillBreak` is not guaranteed to be linear-time bounded since it
--- has to compute the width of a document before pretty printing it
fillBreak :: Int -> Doc -> Doc
fillBreak i d = d <> fill'
  where w     = width d
        fill' = if w >= i then nest i linebreak
                          else spaces (i - w)

--- Compute the width of a given document
width :: Doc -> Int
width (Doc d) = width' 0 (d EOD)
  where width' w EOD                     = w
        width' w (Empty              ts) = width' w ts
        width' w (Text             s ts) = width' (w + length s) ts
        width' w (LineBreak Nothing  ts) = width' w ts
        width' w (LineBreak (Just s) ts) = width' (w + length s) ts
        width' w (OpenGroup          ts) = width' w              ts
        width' w (CloseGroup         ts) = width' w              ts
        width' w (OpenNest         _ ts) = width' w              ts
        width' w (CloseNest          ts) = width' w              ts
        width' w (OpenFormat       _ ts) = width' w              ts
        width' w (CloseFormat        ts) = width' w              ts

-- -----------------------------------------------------------------------------
-- Formatting combinators
-- -----------------------------------------------------------------------------

--- The document `(bold d)` displays document `d` with bold text
--- @param d - a document
--- @return document d displayed with bold text
bold :: Doc -> Doc
bold d = Doc (OpenFormat (SetIntensity Bold) . deDoc d . CloseFormat)

--- The document `(faint d)` displays document `d` with faint text
--- @param d - a document
--- @return document d displayed with faint text
faint :: Doc -> Doc
faint d = Doc (OpenFormat (SetIntensity Faint) . deDoc d . CloseFormat)

--- The document `(blinkSlow d)` displays document `d` with slowly blinking text
--- (rarely supported)
--- @param d - a document
--- @return document d displayed with slowly blinking text
blinkSlow :: Doc -> Doc
blinkSlow d = Doc (OpenFormat (SetBlinkMode Slow) . deDoc d . CloseFormat)

--- The document `(blinkRapid d)` displays document `d` with rapidly blinking
--- text (rarely supported)
--- @param d - a document
--- @return document d displayed with rapidly blinking text
blinkRapid :: Doc -> Doc
blinkRapid d = Doc (OpenFormat (SetBlinkMode Rapid) . deDoc d . CloseFormat)

--- The document `(italic d)` displays document `d` with italicized text
--- (rarely supported)
--- @param d - a document
--- @return document d displayed with italicized text
italic :: Doc -> Doc
italic d = Doc (OpenFormat (SetItalicized True) . deDoc d . CloseFormat)

--- The document `(underline d)` displays document `d` with underlined text
--- @param d - a document
--- @return document d displayed with underlined text
underline :: Doc -> Doc
underline d = Doc (OpenFormat (SetUnderlined True) . deDoc d . CloseFormat)

--- The document `(crossout d)` displays document `d` with crossed out text
--- @param d - a document
--- @return document d displayed with crossed out text
crossout :: Doc -> Doc
crossout d = Doc (OpenFormat (SetCrossedout True) . deDoc d . CloseFormat)

--- The document `(inverse d)` displays document `d` with inversed coloring,
--- i.e. use text color of `d` as background color and background color of `d`
--- as text color
--- @param d - a document
--- @return document d displayed with inversed coloring
inverse :: Doc -> Doc
inverse d = Doc (OpenFormat (InverseColoring True) . deDoc d . CloseFormat)

-- -----------------------------------------------------------------------------
-- Colorisation combinators
-- -----------------------------------------------------------------------------

-- foreground colors

--- The document `(black d)` displays document `d` with black text color
--- @param d - a document
--- @return document d displayed with black text color
black :: Doc -> Doc
black d = Doc (OpenFormat (SetForeground Black) . deDoc d . CloseFormat)

--- The document `(red d)` displays document `d` with red text color
--- @param d - a document
--- @return document d displayed with red text color
red :: Doc -> Doc
red d = Doc (OpenFormat (SetForeground Red) . deDoc d . CloseFormat)

--- The document `(green d)` displays document `d` with green text color
--- @param d - a document
--- @return document d displayed with green text color
green :: Doc -> Doc
green d = Doc (OpenFormat (SetForeground Green) . deDoc d . CloseFormat)

--- The document `(yellow d)` displays document `d` with yellow text color
--- @param d - a document
--- @return document d displayed with yellow text color
yellow :: Doc -> Doc
yellow d = Doc (OpenFormat (SetForeground Yellow) . deDoc d . CloseFormat)

--- The document `(blue d)` displays document `d` with blue text color
--- @param d - a document
--- @return document d displayed with blue text color
blue :: Doc -> Doc
blue d = Doc (OpenFormat (SetForeground Blue) . deDoc d . CloseFormat)

--- The document `(magenta d)` displays document `d` with magenta text color
--- @param d - a document
--- @return document d displayed with magenta text color
magenta :: Doc -> Doc
magenta d = Doc (OpenFormat (SetForeground Magenta) . deDoc d . CloseFormat)

--- The document `(cyan d)` displays document `d` with cyan text color
--- @param d - a document
--- @return document d displayed with cyan text color
cyan :: Doc -> Doc
cyan d = Doc (OpenFormat (SetForeground Cyan) . deDoc d . CloseFormat)

--- The document `(white d)` displays document `d` with white text color
--- @param d - a document
--- @return document d displayed with white text color
white :: Doc -> Doc
white d = Doc (OpenFormat (SetForeground White) . deDoc d . CloseFormat)

-- background colors

--- The document `(bgBlack d)` displays document `d` with black background color
--- @param d - a document
--- @return document d displayed with black background color
bgBlack :: Doc -> Doc
bgBlack d = Doc (OpenFormat (SetBackground Black) . deDoc d . CloseFormat)

--- The document `(bgRed d)` displays document `d` with red background color
--- @param d - a document
--- @return document d displayed with red background color
bgRed :: Doc -> Doc
bgRed d = Doc (OpenFormat (SetBackground Red) . deDoc d . CloseFormat)

--- The document `(bgGreen d)` displays document `d` with green background color
--- @param d - a document
--- @return document d displayed with green background color
bgGreen :: Doc -> Doc
bgGreen d = Doc (OpenFormat (SetBackground Green) . deDoc d . CloseFormat)

--- The document `(bgYellow d)` displays document `d` with yellow background
--- color
--- @param d - a document
--- @return document d displayed with yellow background color
bgYellow :: Doc -> Doc
bgYellow d = Doc (OpenFormat (SetBackground Yellow) . deDoc d . CloseFormat)

--- The document `(bgBlue d)` displays document `d` with blue background color
--- @param d - a document
--- @return document d displayed with blue background color
bgBlue :: Doc -> Doc
bgBlue d = Doc (OpenFormat (SetBackground Blue) . deDoc d . CloseFormat)

--- The document `(bgMagenta d)` displays document `d` with magenta background
--- color
--- @param d - a document
--- @return document d displayed with magenta background color
bgMagenta :: Doc -> Doc
bgMagenta d = Doc (OpenFormat (SetBackground Magenta) . deDoc d . CloseFormat)

--- The document `(bgCyan d)` displays document `d` with cyan background color
--- @param d - a document
--- @return document d displayed with cyan background color
bgCyan :: Doc -> Doc
bgCyan d = Doc (OpenFormat (SetBackground Cyan) . deDoc d . CloseFormat)

--- The document `(bgWhite d)` displays document `d` with white background color
--- @param d - a document
--- @return document d displayed with white background color
bgWhite :: Doc -> Doc
bgWhite d = Doc (OpenFormat (SetBackground White) . deDoc d . CloseFormat)

-- -----------------------------------------------------------------------------
-- Implementation
-- -----------------------------------------------------------------------------

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

--- `(pretty w d)` pretty prints document `d` with a page width of `w` characters
--- @param w - width of page
--- @param d - a document
--- @return pretty printed document
pretty :: Width -> Doc -> String
pretty w d = noGroup (doc2Tokens d) w 1 w [0] []

-- Compute number of visible ASCII characters
length :: String -> Int
length = Prelude.length . filter isVisible
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
  where l = length t
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
  l = length s
  outText cont r ms fs = s ++ cont (r - l) ms fs
oneGroup (LineBreak Nothing ts) w p _ outGrpPre = outGrpPre False (outLine (noGroup ts w p))
 where
  outLine _    _ []       _  = error "Pretty.oneGroup.outLine: empty margins"
  outLine cont _ ms@(m:_) fs = '\n' : addSpaces m ts ++ cont (w - m) ms fs
oneGroup (LineBreak (Just s) ts) w p e outGrpPre =
  pruneOne ts w (p + l) e (\h cont -> outGrpPre h (outLine h cont))
 where
  l = length s
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
  l = length t
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
  l = length s
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
