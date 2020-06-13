
module Parse(parse) where

import Control.Monad (mfilter)
import EarleyM (Gram,Lang,fail,alts,fix,produce,declare,getToken,many,skipWhile)
import Prelude hiding (exp, fail, lookup, pred)
import qualified Data.Char as Char
import qualified EarleyM as EM(parse,Parsing(..))

import Builtin
import Rep_Ast (Def(..),Exp(..),Var(..),mkELam,mkEApp)

newtype ParseError = ParseError { unParseError :: String }
instance Show ParseError where show = unParseError

parse :: String -> Either ParseError (Maybe (Either Def Exp))
parse s =
  case EM.parse lang s of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> Left $ ParseError $ show pe
      Right exp -> return exp

keywords :: [String] -- which are not allowed as identifiers
keywords = ["let","in","if","then","else","fix"]

lang :: Lang Char (Gram (Maybe (Either Def Exp)))
lang = do

    token <- getToken

    let symbol x = do t <-token; if t==x then return () else fail
    let sat pred = do c <- token; if pred c then return c else fail

    let eps = return ()
    let skip p = do _ <- p; eps
    let optional p = alts [p,eps]

    let alpha = sat Char.isAlpha
    let numer = sat Char.isDigit
    let prime = sat (== '\'')
    let under = sat (== '_')
    let digit = do c <- numer; return (digitOfChar c)
    let space = skip (sat Char.isSpace)

    digits <- fix"digits" $ \digits -> return $ alts [
        do n <- digits; d <- digit; return (10 * n + d),
        digit]

    let ident0 = do x <- alpha; xs <- many (alts [alpha,numer,prime,under]); return (x : xs)
    let ident = Var <$> mfilter (`notElem` keywords) ident0

    let keyword string = mapM_ symbol string

    let ws = skipWhile space -- white*
    let ws1 = do space; ws -- white+

    let num = fmap (ECon . Builtin.Num) digits
    let var = fmap EVar ident

    let q = symbol '\''
    let charLit = do q; x <- token; q; return $ ECon $ Builtin.Char x

    let dq = symbol '"'
    let notdq = sat (/= '"')
    let stringLit = do dq; cs <- many notdq; dq; return $ ECon $ Builtin.Str cs

    let parenthesized p = do symbol '('; ws; x <- p; ws; symbol ')'; return x

    let mkApp p1 sep p2 = do
            e1 <- p1
            sep::Gram()
            e2 <- p2;
            return $ mkEApp e1 e2

    let mkBin f c left right = do
            a <- left
            ws; keyword c; ws
            b <- right
            return (f a b)

    let infixOpsL = [ ".", "%", "+", "-", "*", "^", ">", "<", ">=", "<=", "==", "===", "&&", "||", "++" ]

    let infixOpsR = [ ":", ">>" ]

    let infixOps = infixOpsL ++ infixOpsR

    let mkBinOp c = mkBin (\x y -> mkEApp (mkEApp (EVar (Var c)) x) y) c
    let makeBinopL a b = alts (map (\s -> mkBinOp s a b) infixOpsL)
    let makeBinopR a b = alts (map (\s -> mkBinOp s a b) infixOpsR)

    let infixedOpAsIdent = alts [ do keyword s; return (Var s) | s <- infixOps ]
    let nonInfixedUseOfInfixOp = do symbol '('; ws; x <- infixedOpAsIdent; symbol ')'; return x

    let underscore = do symbol '_'; return $ Var "_"

    let formal = alts [ident,underscore,nonInfixedUseOfInfixOp]
    let formals = parseListSep formal ws1

    let abstraction exp = do
          symbol '\\'
          ws; (x,xs) <- formals
          ws; alts [symbol '.', do symbol '-'; symbol '>']
          ws; e <- exp
          return $ (x, foldr mkELam e xs)

    (exp',exp) <- declare "exp"

    let lam = do (x,body) <- abstraction exp; return $ mkELam x body

    let letSyntax = do
            keyword "let"
            ws; x <- formal
            ws; symbol '='
            ws; defined <- exp
            ws; keyword "in"
            ws; body <- exp
            return $ ELet x defined body

    let ifThenElseSyntax = do
            keyword "if"
            ws; i <- exp
            ws; keyword "then"
            ws; t <- exp
            ws; keyword "else"
            ws; e <- exp
            return $ EIf i t e

    let commaSepExps = alts
          [ do ws; return []
          , do ws; (e,es) <- parseListSep exp (do ws; symbol ','; ws); ws; return (e:es)
          ]

    let listLiteral = do
          symbol '['
          es <- commaSepExps
          symbol ']'
          return $ eList es
          where
            eNil = EVar $ Var "nil"
            eCons x y = mkEApp (mkEApp (EVar (Var ":")) x) y
            eList = foldr eCons eNil

    let open = alts [num,var] -- requiring whitespace to avoid juxta-collision
    let closed = alts [listLiteral, parenthesized exp, charLit, stringLit, EVar <$> nonInfixedUseOfInfixOp]

    -- application: juxta position; but whitespace is required for open@open
    (capp',capp) <- declare "capp"
    (oapp',oapp) <- declare "oapp"

    let app = alts [oapp,capp]

    produce capp' $ alts [
        mkApp (alts [open,closed, app])  ws1  closed,
        mkApp (alts [open,closed, app])  eps  closed
        ]

    produce oapp' $ alts [
        mkApp (alts [open,closed, app])  ws1  open,
        mkApp (alts [     closed,capp])  eps  open
        ]

    let app_lam = mkApp (alts [open,closed,app]) ws lam

    let fix_lam = do
          keyword "fix"
          ws; (x,body) <- abstraction exp
          return $ EFix x body

    -- left associative operators
    (opl',opl) <- declare "opl"
    produce opl' $ makeBinopL (alts [open,closed,app,opl]) (alts [open,closed,app])
    let opl_lam  = makeBinopL (alts [open,closed,app,opl]) lam

    -- right associative operators
    (opr',opr) <- declare "opr"
    produce opr' $ makeBinopR (alts [open,closed,app]) (alts [open,closed,app,opr])

    let opr_lam  = makeBinopR (alts [open,closed,app,opl]) lam

    produce exp' $ alts [open,closed,lam,app,opl,opr,
                         app_lam, fix_lam, opl_lam, opr_lam,
                         letSyntax, ifThenElseSyntax]

    let def = do
            name <- formal
            args <- many $ do ws1; formal
            ws; symbol '='
            ws; body <- exp
            return $ Def name (foldr mkELam body args)

    let top = alts
          [ do d <- def; return $ Left d
          , do e <- exp; return $ Right e
          ]

    let dash = skip (sat (== '-'))
    let lineComment = do dash; dash; skipWhile (skip (sat (/= '\n')))

    return $ alts
      [ do eps; return Nothing
      , do lineComment; return Nothing
      , do ws1; optional lineComment; return Nothing
      , do ws; x <- top; ws; optional lineComment; return $ Just x
      ]

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

parseListSep :: Gram a -> Gram () -> Gram (a,[a])
parseListSep p sep = alts [
    do x <- p; sep; (x1,xs) <- parseListSep p sep; return (x,x1:xs),
    do x <- p; return (x,[])]
