
-- Linearized code seqeunces
module Rep_Linear where

import qualified Builtin

data CodeSequence
  = UnconditionalJump CodeRef
  | ArityCheck Index CodeSequence
  | Return ValRef
  | Tail ValRef [ValRef]
  | Prim1 Builtin.Prim1 ValRef CodeSequence
  | Prim2 Builtin.Prim2 ValRef ValRef CodeSequence
  | JumpIfTrue CodeRef ValRef CodeSequence
  | PushContinuation CodeRef [ValRef] CodeSequence
  | MakeClosure CodeRef [ValRef] CodeSequence
  deriving (Eq)

data ValRef = VArg Index | VFree Index | VLit LitRef  deriving (Eq)

newtype Index = Index Int deriving (Eq)

newtype LitRef = LitRef Index deriving (Eq)
newtype CodeRef = CodeRef Index deriving (Eq)

data Code = Code
  { lits :: [Builtin.BV]
  , defs :: [CodeSequence]
  }

----------------------------------------------------------------------

instance Show Code where
  show Code{lits,defs} =
    unlines $
    [ "#include \"value.h\""
    , "value lits[] = {"
    ] ++
    [ "   (value) " ++ show lit ++ "," | lit <- lits ] ++
    [ "  };"
    , "char* prog[] ="
    , "  {" ] ++
    [ "   \"" ++ show seq ++ "\", //" ++ show i | (i,seq) <- zip [0::Int ..] defs ] ++
    [ "  };" ]

instance Show ValRef where
  show = \case
    VArg n -> "*" ++ show n
    VFree n -> "~" ++ show n
    VLit n -> "$" ++ show n

instance Show CodeRef where show = \case CodeRef n -> show n
instance Show LitRef where show = \case LitRef n -> show n

instance Show CodeSequence where
  show = \case
    UnconditionalJump r -> "u" ++ show r
    ArityCheck n c  -> "n" ++ show n ++ show c
    Return v -> "r" ++ show v
    Tail func args -> "t" ++ show func ++ showVals args
    Prim1 p a c -> showPrim1 p ++ show a ++ show c
    Prim2 p a1 a2 c -> showPrim2 p ++ show a1 ++ show a2 ++ show c
    JumpIfTrue r v c -> "j" ++ show v ++ show r ++ show c
    PushContinuation r vs c -> "p" ++ show r ++ showVals vs ++ show c
    MakeClosure r vs c -> "c" ++ show r ++ showVals vs ++ show c

instance Show Index where
  show = \case
    Index n -> showInt n

showVals :: [ValRef] -> String
showVals vs = showInt (length vs) ++ concat (map show vs)

showInt :: Int -> String
showInt n = do
  take (length ds - 1) (repeat 'x') ++ concat (map showDigit ds)
  where
    ds = digits [] n
    digits acc n = if n<10 then (n:acc) else
      digits ((n `mod` 10) : acc) (n `div` 10)

showDigit :: Int -> String
showDigit n = if n<0 || n>9 then error "showDigit" else show n

showPrim1 :: Builtin.Prim1 -> String
showPrim1 = \case
  Builtin.ShowChar -> "C"
  Builtin.ShowInt -> "S"
  Builtin.ReadInt -> "R"
  Builtin.Argv -> "A"
  Builtin.StrSize -> "Z"
  Builtin.Error -> "!"

showPrim2 :: Builtin.Prim2 -> String
showPrim2 = \case
  Builtin.Add -> "+"
  Builtin.Sub -> "-"
  Builtin.Mul -> "M"
  Builtin.ModInt -> "%"
  Builtin.EqNumOrChar -> "="
  Builtin.EqString -> "~"
  Builtin.LessNumOrChar -> "<"
  Builtin.StringAppend -> "^"
  Builtin.StrIndex -> "I"
