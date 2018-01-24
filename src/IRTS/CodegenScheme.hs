{-# LANGUAGE PatternGuards #-}
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
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative hiding (empty, Const)
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Util.PrettyPrint

type Expr = Doc

useCtorTags :: Bool
useCtorTags = False  -- otherwise, use ctor names

indent :: Doc -> Doc
indent = nest 2

schemePreamble :: Doc
schemePreamble = vcat . map text $
    [ "; generated by idris-scheme"
    , ""
    , "; (rts-unpack names-to-bind list-with-values)"
    , "(define-syntax rts-unpack"
    , "  (syntax-rules ()"
    , "    ((rts-unpack xs () rhs) rhs)"
    , "    ((rts-unpack xs (v . vs) rhs)"
    , "      (let ((v (car xs)) (rest (cdr xs)))"
    , "        (rts-unpack rest vs rhs)))))"
    , ""
    , "; curried lambda"
    , "(define-syntax clambda"
    , "  (syntax-rules ()"
    , "    ((clambda () body) body)"
    , "    ((clambda (x . xs) body)"
    , "      (lambda (x) (clambda xs body)))))"
    ]

blankLine :: Doc
blankLine = text ""

schemeLauncher :: Doc
schemeLauncher = sexp [cgName $ sNS (sUN "main") ["Main"], text "'World"]

mangle :: Name -> String
mangle n = "idr_" ++ concatMap mangleChar (showCG n)
  where
    mangleChar x
        | isAlpha x || isDigit x = [x]
        | otherwise = "_" ++ show (ord x) ++ "_"

-- We could generate from:
-- simpleDecls / defunDecls / liftDecls
codegenScheme :: CodeGenerator
codegenScheme ci = writeFile (outputFile ci) (render "; " "" source)
  where
    source =
      schemePreamble
      $$ text ""
      $$ definitions
      $$ schemeLauncher

    decls = liftDecls ci
    definitions = vcat $ map (cgFun zeroArgs) [d | (_, d@(LFun _ _ _ _)) <- decls]
    zeroArgs = S.fromList [n | (n, LFun _ _ [] _) <- decls]

cgFun :: S.Set Name -> LDecl -> Doc

-- we must put zero-arg functions under at least one lambda
cgFun za (LFun opts n [] body) = parens (
        text "define" <+> parens (cgName n)
        $$ (text "" <?> show n)
        $$ indent (cgExp za body)
    ) $$ text ""

cgFun za (LFun opts n args body) = parens (
        text "define" <+> cgName n
        $$ (text "" <?> show n)
        $$ indent (cgLam args $ cgExp za body)
    ) $$ text ""

cgLam :: [Name] -> Doc -> Doc
cgLam ns body = parens (
    text "clambda" <+> parens (hsep $ map cgName ns)
    $$ indent body
  )

cgApp :: Doc -> [Doc] -> Doc
cgApp f [] = f
cgApp f (x:xs) = cgApp (sexp [f, x]) xs

-- za = zero-arg functions
cgExp :: S.Set Name -> LExp -> Doc
cgExp zeroArgFuns (LV n)
    | n `S.member` zeroArgFuns = parens (cgName n)
    | otherwise = cgName n
cgExp za (LApp _tail_call f args) = cgApp (cgExp za f) (map (cgExp za) args)
cgExp za (LLazyApp fn args) = cgError $ "lazy app: " ++ show (fn, args)
cgExp za (LLazyExp e) = kwexp "lambda" [parens (text "_force"), cgExp za e]
cgExp za (LForce e) = sexp [cgExp za e, text "'force"]
cgExp za (LLet n val rhs) = kwexp "let" [parens (parens (cgName n <+> cgExp za val)), cgExp za rhs]
cgExp za (LLam args rhs) = kwexp "lambda" [parens (hsep $ map cgName args), cgExp za rhs]
cgExp za (LProj e i) = kwexp "list-ref" [cgExp za e, int (i+1)]  -- skip the tag
cgExp za (LCon _maybe_cell tag n args)
    | useCtorTags = kwexp "list" (int tag : map (cgExp za) args) <?> show n
    | otherwise   = kwexp "list" ((text "'" <> cgName n) : map (cgExp za) args) <?> show n
cgExp za (LCase _caseType scrut alts) = cgCase za scrut alts
cgExp za (LConst x) = cgConst x
cgExp za (LForeign fdesc ret args) = cgForeign za fdesc ret args
cgExp za (LOp op args) = cgPrimOp za op args
cgExp za (LNothing) = text "'nothing"
cgExp za (LError msg) = cgError msg

cgError :: String -> Doc
cgError msg = kwexp "error" [cgStr msg]

cgCase :: S.Set Name -> LExp -> [LAlt] -> Doc
cgCase za scrut alts
    | isADT alts
        = kwexp "let*" [
            sexp [
              sexp [text "_scrut", cgExp za scrut],
              sexp [text "_tag", kwexp "car" [text "_scrut"]]
            ],
            sexp (
              text "cond"
              : map (cgAlt za) (clean alts)
            )
        ]
    | otherwise
        = kwexp "let" [
            sexp [
              sexp [text "_scrut", cgExp za scrut]
            ],
            sexp (
              text "cond"
              : map (cgAlt za) (clean alts)
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

cgAlt :: S.Set Name -> LAlt -> Doc
cgAlt za (LConCase tag n args rhs) 
    | useCtorTags = sexp
        [ kwexp "eq?" [text "_tag", int tag]
        , unpackAlt args $ cgExp za rhs
        ]
    | otherwise = sexp
        [ kwexp "eq?" [text "_tag", text "'" <> cgName n]
        , unpackAlt args $ cgExp za rhs
        ]
cgAlt za (LConstCase const rhs) = sexp
    [ kwexp "eq?" [text "_scrut", cgConst const]
    , cgExp za rhs
    ]
cgAlt za (LDefaultCase rhs) = sexp
    [ text "else"
    , cgExp za rhs
    ]

cgForeign :: S.Set Name -> FDesc -> FDesc -> [(FDesc, LExp)] -> Doc
cgForeign za _ (FStr fn) args = cFFI fn (map (cgExp za . snd) args)
cgForeign za fn fty args = cgError $ "foreign not implemented: " ++ show (fn, fty, args)

-- C FFI emulation
cFFI :: String -> [Doc] -> Doc

-- IORefs are represented as single-element lists
cFFI "idris_newRef" [x] = kwexp "list" [x]
cFFI "idris_readRef" [ref] = kwexp "car" [ref]
cFFI "idris_writeRef" [ref, x] = kwexp "set-car!" [ref, x]

-- scheme does not include argv[0] so we hack around that
cFFI "idris_numArgs" [] = text "(+ 1 (length (command-line-arguments)))"
cFFI "idris_getArg" [i] = kwexp "list-ref"
    [ text "(cons \"this-program\" (command-line-arguments))"
    , i
    ]

cFFI fn args = cgError $ "unsupported C FFI: " ++ show (fn, args)

boolOp :: S.Set Name -> String -> [LExp] -> Doc
boolOp za op args = kwexp "if" [kwexp op (map (cgExp za) args), int 1, int 0]

-- some primops are implemented here for efficiency
cgPrimOp :: S.Set Name -> PrimFn -> [LExp] -> Doc
cgPrimOp za (LSExt _ _) [x] = cgExp za x  -- scheme ints are arbitrary precision
cgPrimOp za (LEq (ATInt ITChar)) args = boolOp za "char=?" args  -- chars can't be compared using (=) in Scheme
cgPrimOp za (LEq _) args = boolOp za "=" args
cgPrimOp za (LSLt (ATInt ITChar)) args = boolOp za "char<?" args
cgPrimOp za (LSLt _) args = boolOp za "<" args
cgPrimOp za (LSGt (ATInt ITChar)) args = boolOp za "char>?" args
cgPrimOp za (LSGt _) args = boolOp za ">" args
cgPrimOp za LStrEq args = boolOp za "string=?" args
cgPrimOp za LStrLt args = boolOp za "string<?" args
cgPrimOp za LWriteStr [_, s] = kwexp "display" [cgExp za s]
cgPrimOp za LStrHead [s] = kwexp "string-ref" [cgExp za s, int 0]
cgPrimOp za LStrTail [s] = kwexp "substring" [cgExp za s, int 1]
cgPrimOp za LStrCons [c, s] = kwexp "string-append" [kwexp "string" [cgExp za c], cgExp za s]
cgPrimOp za op args = sexp (cgOp op : map (cgExp za) args)

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
  where
    ext n = parens (text "lambda ()" <+> text n)
cgOp op = cgError $ "unsupported primop: " ++ show op

cgConst :: Const -> Doc
cgConst (I i) = text $ show i
cgConst (BI i) = text $ show i
cgConst (Fl f) = text $ show f
cgConst (Ch c) = cgChar c
cgConst (Str s) = cgStr s
cgConst c = cgError $ "unimplemented constant: " ++ show c

cgStr :: String -> Doc
cgStr s = text "\"" <> text (concatMap (scmShowChr True) s) <> text "\""

cgChar :: Char -> Doc
cgChar c
    | c >= ' ' && c < '\x7F' = text ("#\\" ++ [c])
    | otherwise = text ("#\\x" ++ showHex (ord c) "")

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
cgName (MN i n) | all (\x -> isAlpha x || x `elem` "_") (T.unpack n)
    = text $ T.unpack n ++ show i
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

kwexp :: String -> [Doc] -> Doc
kwexp n (x : xs) = sexp (text n <+> x : xs)
kwexp n []       = parens (text n)
