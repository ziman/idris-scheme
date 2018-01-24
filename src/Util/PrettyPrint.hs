-- This module is equivalent to Text.PrettyPrint.
-- The only difference is slightly different indentation behaviour.
-- (Plus support of code comments).

module Util.PrettyPrint
    ( Doc
    , int, text
    , comma, colon
    , lparen, rparen, lbracket, rbracket, lbrace, rbrace
    , (<>), (<+>), ($+$), ($$)
    , (<?>)
    , nest
    , parens, brackets
    , empty
    , render
    , vcat, hsep
    , punctuate
    , size, width
    )
    where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU

type Line = (Int, Text, Text)  -- nesting, text, comment
newtype Doc = Doc [Line]
instance Show Doc where
    show = T.unpack . render "(* " " *)"

infixr 6 <>, <+>
infixr 5 $$, $+$
infixl 1 <?>

int :: Int -> Doc
int i = text . T.pack $ show i

text :: Text -> Doc
text s = Doc [(0, s, "")]

comma, colon :: Doc
comma    = text ","
colon    = text ":"

lparen, rparen, lbracket, rbracket, lbrace, rbrace :: Doc
lparen   = text "("
rparen   = text ")"
lbracket = text "["
rbracket = text "]"
lbrace   = text "{"
rbrace   = text "}"

(<>) :: Doc -> Doc -> Doc
Doc xs <> Doc ys = Doc $ meld "" xs ys

(<+>) :: Doc -> Doc -> Doc
Doc xs <+> Doc ys = Doc $ meld " " xs ys

($+$) :: Doc -> Doc -> Doc
Doc xs $+$ Doc ys = Doc $ xs ++ ys

($$) :: Doc -> Doc -> Doc
($$) = ($+$)

-- | Add a comment to the first line of the Doc.
(<?>) :: Doc -> Text -> Doc
Doc [] <?> comment = Doc [(0, "", comment)]
Doc ((n, t,c) : lines) <?> comment = Doc $ (n, t, merge comment c) : lines
  where
    merge "" y  = y
    merge x  "" = x
    merge x  y  = T.concat [x, " (", y, ")"]

meld :: Text -> [Line] -> [Line] -> [Line]
meld sep [] ys = ys
meld sep xs [] = xs
meld sep [(n,x,xc)] ((_n',y,yc) : ys) = (n, T.concat[x, sep, y], merge xc yc) : ys
  where
    merge "" y  = y
    merge x  "" = x
    merge x  y  = T.concat[x, ", ", y]
meld sep (x : xs) ys = x : meld sep xs ys

nest :: Int -> Doc -> Doc
nest n' (Doc xs) = Doc [(n+n', t, c) | (n, t, c) <- xs]

parens :: Doc -> Doc
parens d = lparen <> d <> rparen

brackets :: Doc -> Doc
brackets d = lbracket <> d <> rbracket

render :: Text -> Text -> Doc -> Text
render cmtL cmtR (Doc xs) = T.unlines $ map (renderLine cmtL cmtR) xs

renderLine :: Text -> Text -> (Int, Text, Text) -> Text
renderLine cmtL cmtR (n, "", "") = ""
renderLine cmtL cmtR (n, "", comment) = T.concat [T.replicate n " ", cmtL, comment, cmtR]
renderLine cmtL cmtR (n, content, "") = T.concat [T.replicate n " ", content]
renderLine cmtL cmtR (n, content, comment) = T.concat [T.replicate n " ", content, "  ", cmtL, comment, cmtR]

empty :: Doc
empty = Doc []

vcat :: [Doc] -> Doc
vcat = foldr ($+$) empty

hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate sep [] = []
punctuate sep [x] = [x]
punctuate sep (x : xs) = (x <> sep) : punctuate sep xs

size :: Doc -> Int
size (Doc xs) = sum [TU.lengthWord16 t | (n, t, c) <- xs]

width :: Doc -> Int
width (Doc xs) = maximum [TU.lengthWord16 t | (n, t, c) <- xs]
