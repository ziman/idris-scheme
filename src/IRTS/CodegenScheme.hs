module IRTS.CodegenScheme (codegenScheme) where

import IRTS.CodegenCommon
import IRTS.Lang hiding (lift)
import IRTS.Simplified
import IRTS.Defunctionalise hiding (lift)

import Idris.Core.TT
import Debug.Trace

import Numeric
import Data.Maybe
import Data.Char
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative hiding (empty, Const)
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Util.PrettyPrint
import IRTS.SchemeRTS (schemeRTS)

type Expr = Doc

data Ctx = Ctx
    { zeroArgFuns :: S.Set Name
    , ctorTags :: M.Map Name Int
    }
    deriving Show

-- use numeric ctor tags?
-- (as opposed to 'Constructor symbols)
useCtorTags :: Bool
useCtorTags = True

tshow :: Show a => a -> T.Text
tshow = T.pack . show

indent :: Doc -> Doc
indent = nest 2

blankLine :: Doc
blankLine = text ""

schemeLauncher :: Doc
schemeLauncher = sexp [cgName $ sNS (sUN "main") ["Main"], text "'World"]

mangle :: Name -> Text
mangle n = T.pack $ "idr_" ++ concatMap mangleChar (showCG n)
  where
    mangleChar x
        | isAlpha x || isDigit x = [x]
        | otherwise = "_" ++ show (ord x) ++ "_"

-- We could generate from:
-- simpleDecls / defunDecls / liftDecls
codegenScheme :: CodeGenerator
codegenScheme ci = TIO.writeFile (outputFile ci) (render "; " "" source)
  where
    source =
      text schemeRTS
      $$ text ""
      $$ definitions
      $$ schemeLauncher

    decls = liftDecls ci
    definitions = vcat $ map (cgFun ctx) [d | (_, d@(LFun _ _ _ _)) <- decls]
    ctx = Ctx
        { zeroArgFuns = S.fromList [n | (n, LFun _ _ [] _) <- decls]
        , ctorTags = M.fromList [(n, tag) | (n, LConstructor _n tag _arity) <- decls] 
        }

cgFun :: Ctx -> LDecl -> Doc

-- we must put zero-arg functions under at least one lambda
cgFun ctx (LFun opts n [] body) = parens (
        text "define" <+> parens (cgName n)
        $$ (text "" <?> tshow n)
        $$ indent (cgExp ctx body)
    ) $$ text ""

cgFun ctx (LFun opts n args body) = parens (
        text "define" <+> cgName n
        $$ (text "" <?> tshow n)
        $$ indent (cgLam args $ cgExp ctx body)
    ) $$ text ""

cgLam :: [Name] -> Doc -> Doc
cgLam ns body = parens (
    text "clambda" <+> parens (hsep $ map cgName ns)
    $$ indent body
  )

cgApp :: Doc -> [Doc] -> Doc
cgApp f [] = f
cgApp f (x:xs) = cgApp (sexp [f, x]) xs

-- ctx = zero-arg functions
cgExp :: Ctx -> LExp -> Doc
cgExp ctx (LV n)
    | n `S.member` zeroArgFuns ctx
    = parens (cgName n)

    | otherwise
    = cgName n
cgExp ctx (LApp _tail_call f args) = cgApp (cgExp ctx f) (map (cgExp ctx) args)
cgExp ctx (LLazyApp fn args) = cgError ("lazy app: " `T.append` tshow (fn, args))
cgExp ctx (LLazyExp e) = kwexp "lambda" [parens (text "_force"), cgExp ctx e]
cgExp ctx (LForce e) = sexp [cgExp ctx e, text "'force"]
cgExp ctx (LLet n val rhs) = kwexp "let" [parens (parens (cgName n <+> cgExp ctx val)), cgExp ctx rhs]
cgExp ctx (LLam args rhs) = kwexp "lambda" [parens (hsep $ map cgName args), cgExp ctx rhs]
cgExp ctx (LProj e i) = kwexp "list-ref" [cgExp ctx e, int (i+1)]  -- skip the tag
cgExp ctx (LCon _maybe_cell tag n args)
    | useCtorTags = kwexp "list" (int tag : map (cgExp ctx) args) <?> tshow n
    | otherwise   = kwexp "list" ((text "'" <> cgName n) : map (cgExp ctx) args) <?> tshow n
cgExp ctx (LCase _caseType scrut alts) = cgCase ctx scrut alts
cgExp ctx (LConst x) = cgConst x
cgExp ctx (LForeign fdesc ret args) = cgForeign ctx fdesc ret args
cgExp ctx (LOp op args) = cgPrimOp ctx op args
cgExp ctx (LNothing) = text "'nothing"
cgExp ctx (LError msg) = cgError $ T.pack msg

cgError :: Text -> Doc
cgError msg = kwexp "error" [cgStr msg]

cgError' :: Text -> [Doc] -> Doc
cgError' msg vals = kwexp "error" [cgStr msg, cgList vals]

cgList :: [Doc] -> Doc
cgList = kwexp "list"

cgCase :: Ctx -> LExp -> [LAlt] -> Doc
cgCase ctx scrut alts
    | isADT alts
        = kwexp "let*" [
            sexp [
              sexp [text "_scrut", cgExp ctx scrut],
              sexp [text "_tag", kwexp "car" [text "_scrut"]]
            ],
            sexp (
              text "cond"
              : map (cgAlt ctx) (clean alts)
            )
        ]
    | otherwise
        = kwexp "let" [
            sexp [
              sexp [text "_scrut", cgExp ctx scrut]
            ],
            sexp (
              text "cond"
              : map (cgAlt ctx) (clean alts)
            )
        ]
  where
    isADT (LConCase _ _ _ _ : _) = True
    isADT (LConstCase _ _ : _) = False
    isADT (LDefaultCase _ : alts) = isADT alts
    isADT [] = error "cannot determine ADTness of case tree"

    -- remove everything after DefaultCase
    -- causes syntax errors in scheme
    clean (alt@(LConCase _ _ _ _) : alts) = alt : clean alts
    clean (alt@(LConstCase _ _) : alts) = alt : clean alts
    clean (alt@(LDefaultCase _) : alts) = [alt]
    clean [] = []

unpackAlt :: [Name] -> Doc -> Doc
unpackAlt [] body = body
unpackAlt args body = kwexp "rts-unpack" [
    text "(cdr _scrut)",
    sexp (map cgName args),
    body
  ]

cgAlt :: Ctx -> LAlt -> Doc
cgAlt ctx (LConCase _tag n args rhs) 
    | useCtorTags = sexp
        -- _tag is always -1 in LAlt so we need to look it up by name
        [ kwexp "=" [text "_tag", int (ctorTags ctx M.! n) <?> tshow n]
        , unpackAlt args $ cgExp ctx rhs
        ]
    | otherwise = sexp
        [ kwexp "equal?" [text "_tag", text "'" <> cgName n]
        , unpackAlt args $ cgExp ctx rhs
        ]
cgAlt ctx (LConstCase const rhs) = sexp
    [ kwexp "equal?" [text "_scrut", cgConst const]
    , cgExp ctx rhs
    ]
cgAlt ctx (LDefaultCase rhs) = sexp
    [ text "else"
    , cgExp ctx rhs
    ]

cgForeign :: Ctx -> FDesc -> FDesc -> [(FDesc, LExp)] -> Doc
cgForeign ctx _ (FStr fn) args = cFFI fn (map (cgExp ctx . snd) args)
cgForeign ctx fn fty args = cgError ("foreign not implemented: " `T.append` tshow (fn, fty, args))

-- C FFI emulation
cFFI :: String -> [Doc] -> Doc
cFFI fname = kwexp (T.pack $ "cffi-" ++ fname)

boolOp :: Ctx -> Text -> [LExp] -> Doc
boolOp ctx op args = kwexp "if" [kwexp op (map (cgExp ctx) args), int 1, int 0]

-- some primops are implemented here for efficiency
cgPrimOp :: Ctx -> PrimFn -> [LExp] -> Doc
cgPrimOp ctx (LSExt _ _) [x] = cgExp ctx x  -- scheme ints are arbitrary precision
cgPrimOp ctx (LEq (ATInt ITChar)) args = boolOp ctx "char=?" args  -- chars can't be compared using (=) in Scheme
cgPrimOp ctx (LEq _) args = boolOp ctx "=" args
cgPrimOp ctx (LSLt (ATInt ITChar)) args = boolOp ctx "char<?" args
cgPrimOp ctx (LSLt _) args = boolOp ctx "<" args
cgPrimOp ctx (LSGt (ATInt ITChar)) args = boolOp ctx "char>?" args
cgPrimOp ctx (LSGt _) args = boolOp ctx ">" args
cgPrimOp ctx LStrEq args = boolOp ctx "string=?" args
cgPrimOp ctx LStrLt args = boolOp ctx "string<?" args
cgPrimOp ctx LWriteStr [_, s] = kwexp "display" [cgExp ctx s]
cgPrimOp ctx LStrHead [s] = kwexp "string-ref" [cgExp ctx s, int 0]
cgPrimOp ctx LStrTail [s] = kwexp "substring" [cgExp ctx s, int 1]
cgPrimOp ctx LStrCons [c, s] = kwexp "string-append" [kwexp "string" [cgExp ctx c], cgExp ctx s]
cgPrimOp ctx op args = sexp (cgOp op : map (cgExp ctx) args)

cgOp :: PrimFn -> Doc
cgOp (LMinus _) = text "-"
cgOp (LPlus _) = text "+"
cgOp (LTimes _) = text "*"
cgOp LStrConcat = text "string-append"
cgOp LStrCons = text "string-append"
cgOp (LIntStr _) = text "number->string"
cgOp (LStrInt _) = text "string->number"
cgOp (LChInt _) = text "char->integer"
cgOp (LIntCh _) = text "integer->char"
cgOp (LExternal n)
    | n == sUN "prim__stdout" = ext "'stdout"
    | n == sUN "prim__stdin" = ext "'stdin"
    | n == sUN "prim__stderr" = ext "'stderr"
    | n == sUN "prim__sizeofPtr" = ext "1"
    | n == sUN "prim__null" = ext "'null"
    | n == sUN "prim__readChars" = text "cffi-readChars"
    | n == sUN "prim__vm" = extW "'vm"
  where
    ext n = parens (text "lambda ()" <+> text n)
    extW n = parens (text "lambda (_world)" <+> text n)
cgOp op = cgError ("unsupported primop: " `T.append` tshow op)

cgConst :: Const -> Doc
cgConst (I i) = text $ tshow i
cgConst (BI i) = text $ tshow i
cgConst (Fl f) = text $ tshow f
cgConst (Ch c) = cgChar c
cgConst (Str s) = cgStr $ T.pack s
cgConst c = cgError ("unimplemented constant: " `T.append` tshow c)

cgStr :: Text -> Doc
cgStr s = text "\"" <> text (T.pack $ concatMap (scmShowChr True) (T.unpack s)) <> text "\""

cgChar :: Char -> Doc
cgChar c
    | c >= ' ' && c < '\x7F' = text (T.pack $ "#\\" ++ [c])
    | otherwise = text (T.pack $ "#\\u" ++ showHexN 4 (ord c))

-- bool flag = True: we are between double quotes
-- bool flag = False: we are between single quotes
scmShowChr :: Bool -> Char -> String
scmShowChr True  '"' = "\\\""
scmShowChr False '\'' = "\\'"
scmShowChr isStr '\\' = "\\\\"
scmShowChr isStr c
    | c >= ' ' && c < '\x7F'  = [c]
    | c <= '\xFF' = "\\x" ++ showHexN 2 (ord c)
    | otherwise = error $ "char > 255: " ++ show c

showHexN :: Int -> Int -> String
showHexN 0 _ = ""
showHexN w n =
  let (p,q) = n `divMod` 16
    in showHexN (w-1) p ++ showHex q ""

-- Let's not mangle /that/ much. Especially function parameters
-- like e0 and e1 are nicer when readable.
cgName :: Name -> Expr
cgName (MN i n) | all (\x -> isAlpha x || x `elem` ['_']) (T.unpack n)
    = text $ T.concat [n, tshow i]
cgName n = text (mangle n)

cgBigLet :: [(Name, Doc)] -> Doc -> Doc
cgBigLet defs body
    = parens (text "letrec*"
        <+> parens (
                text ""
                $$ indent (vcat [
                    parens (cgName n $$ indent val) $$ text ""
                    | (n, val) <- defs
                ])
            )
        $$ body
    )

sexp :: [Doc] -> Doc
sexp [] = text "()"
sexp [x] = parens x
sexp xxs@(x:xs)
    | size shortLayout <= 100 = shortLayout
    | otherwise = longLayout
  where
    shortLayout = parens $ hsep xxs
    longLayout  = parens (x $$ indent (vcat xs))

kwexp :: Text -> [Doc] -> Doc
kwexp n (x : xs) = sexp (text n <+> x : xs)
kwexp n []       = parens (text n)
