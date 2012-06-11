
module Events.Core (
  getCoreExpr,
  CoreExpr(..),
  renderExpr, dumpExpr
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get
import Data.Word
import Data.Char
import Data.List
import Data.Function ( on )

import Control.Applicative
import Control.Monad

import Text.PrettyPrint as PP

-- | We will find pretty-printed representations of a lot of things in
-- the debug data. Name the types so we can document that they
-- actually mean different things.
type Bind = BS.ByteString
type Cons = BS.ByteString
type Type = BS.ByteString

data CoreExpr
  = Misc BS.ByteString
  | App CoreExpr CoreExpr
  | Ref Bind Cons
  | Lam Bind CoreExpr
  | Let [(Bind, Type, CoreExpr)] CoreExpr
  | Case CoreExpr Bind Type [CoreExpr]
  | Alt Cons [Bind] CoreExpr
  | Abbr CoreExpr

instance Show CoreExpr where
  show = dumpExpr

-- | Constants for core binary representation
coreMisc, coreApp, coreRef, coreLam, coreLet, coreCase, coreAlt :: Word8
coreMisc = 0
coreApp  = 1
coreRef  = 2
coreLam  = 3
coreLet  = 4
coreCase = 5
coreAlt  = 6

-- | Some indeed very friendly helpers, copied from GHC
collectBinders :: CoreExpr -> ([Bind], CoreExpr)
collectBinders expr
  = go [] expr
  where
    go bs (Lam b e) = go (b:bs) e
    go bs e         = (reverse bs, e)
collectArgs :: CoreExpr -> (CoreExpr, [CoreExpr])
collectArgs expr
  = go expr []
  where
    go (App f a) as = go f (a:as)
    go e         as = (e, as)

-------------------------------------------------------------
-- Binary reading
-------------------------------------------------------------

readString :: Get BS.ByteString
readString = BS.concat . LBS.toChunks <$> getLazyByteStringNul

readCoreExpr :: Get CoreExpr
readCoreExpr = getWord8 >>= \c ->
  case () of
    _ | c == coreMisc -> Misc <$> readString
      | c == coreApp  -> App  <$> readCoreExpr <*> readCoreExpr
      | c == coreRef  -> Ref  <$> readString <*> readString
      | c == coreLam  -> Lam  <$> readString <*> readCoreExpr
      | c == coreLet  -> Let  <$> readSome readCoreLet <*> readCoreExpr
      | c == coreCase -> Case <$> readCoreExpr <*> readString <*> readString
                              <*> readSome readCoreExpr
      | c == coreAlt  -> Alt  <$> readString <*> readSome readString
                              <*> readCoreExpr
      | otherwise     -> fail ("invalid core id: " ++ show c)

readSome :: Get a -> Get [a]
readSome get = do
  n1 <- getWord8
  n2 <- getWord8
  let n = 256 * fromIntegral n1 + fromIntegral n2 -- little endian
  replicateM n get

readCoreLet :: Get (Bind, Type, CoreExpr)
readCoreLet = (,,) <$> readString  <*> readString <*> readCoreExpr

getCoreExpr :: LBS.ByteString -> CoreExpr
getCoreExpr str = runGet readCoreExpr str

-------------------------------------------------------------
-- Abbreviating
-------------------------------------------------------------
{-
abbrExpr :: Bind -> Cons -> CoreExpr -> Maybe CoreExpr
abbrExpr b c (App f a)   = abbr b c f <|> abbr b c a
abbrExpr b c (Ref b' c')
  | b == b' && c == c'   = Just (Ref b' c')
abbrExpr b c (Lam _ e)   = abbr b c e
abbrExpr b c (Let bs e)  = 
abbrExpr _ _ _           = Nothing
 where abbr b c e = case abbrExpr b c e of
         Just (e@Abbr x) -> Just e
         Just e          -> Just (Abbr e)
         Nothing         -> Nothing
-}
-------------------------------------------------------------
-- Pretty-Printing
-------------------------------------------------------------

-- All loosely based on coreSyn/PprCore.lhs in the GHC source,
-- simplified where it seemed to not be *too* wrong

bs2st :: BS.ByteString -> String
bs2st = map (chr . fromIntegral) . BS.unpack

pprBS :: BS.ByteString -> Doc
pprBS = mltext . bs2st

ppr :: (Doc -> Doc) -> CoreExpr -> Doc
ppr _ (Misc str) = pprBS str
ppr p (e@App{}) =
  let (fun, args) = collectArgs e
  in p $ hang (ppr parens fun) 2 (gsep (map (ppr parens) args))
ppr p (e@Lam{}) =
  let (bs, body) = collectBinders e
  in p $ hang (text "\\" <+> gsep (map pprBS bs) <+> text "->")
              2 (ppr id body)
ppr p (Case expr bind _ [Alt con args rhs]) =
  p $ sep [sep [text "case" <+> ppr id expr,
                sep [text "of" <+> pprBS bind,
                     lbrace <+> pprPat con args <+> text "->"]
               ]
          , ppr id rhs <+> rbrace ]
ppr p (Case expr bind _ alts) =
  p $ sep [sep [text "case" <+> ppr id expr,
                sep [text "of" <+> pprBS bind <+> lbrace]
               ]
          , nest 2 $ vcat $ punctuate semi $ map (ppr id) $
              sortBy (compare `on` isDefaultExpr) alts
          , rbrace ]
ppr p (Let binds expr) =
  p $ sep [hang (text "let") 2 $
             vcat [ hang (pprBS bnd <+> equals) 2 $ ppr id rhs
                  | (bnd, _ty, rhs) <- binds
                  ]
          , text "} in"
          , ppr id expr ]
ppr _ (Ref bind cons) = ptext (placeholder bind cons)
ppr _ (Alt cons binds expr)
  = hang (pprPat cons binds <+> text "->") 2 (ppr id expr)

-- | Pretty-print multi-line text
mltext :: String -> Doc
mltext s = case break (=='\n') s of
  (_ , []  ) -> text s
  (pr, _:su) -> text pr $$ mltext su

-- | Tests whether a constructor name represents the "default" case alternative
isDefault :: Cons -> Bool
isDefault s = s == (BS.pack $ map (fromIntegral . ord) "__DEFAULT") || BS.null s

-- | Tests whether a core expression is the default cause alternative
isDefaultExpr :: CoreExpr -> Bool
isDefaultExpr (Alt c _ _) = isDefault c
isDefaultExpr (Ref _ c)   = isDefault c
isDefaultExpr _other      = False

-- | A nicer version of "sep" that tries to find a middle-ground
-- between hsep and vcat where possible.
gsep :: [Doc] -> Doc
gsep ds = case unfoldr split' ds of
  []    -> PP.empty
  [_]   -> sep ds
  dss   -> gsep (map sep dss)
 where split' [] = Nothing
       split' xs = Just $ splitAt 2 xs

pprPat :: Cons -> [Bind] -> Doc
pprPat cons binds
  | isDefault cons  = text "_"
  | otherwise       = pprBS cons <+> gsep (map pprBS binds)

pprExpr :: CoreExpr -> Doc
pprExpr = ppr id

placeholderPrefix, placeholderPostfix :: String
placeholderPrefix = "####"
placeholderPostfix = replicate 100 '#'

-- | Placeholder for a different piece of Core. This will produce a
-- string that is guaranteed to not occur in Core normally, as well as
-- being long enough to ensure that it will get formatted as a
-- separate line in pretty-printed output.
placeholder :: Bind -> Cons -> String
placeholder bind cons =
  placeholderPrefix ++ show (bs2st bind, bs2st cons) ++ placeholderPostfix

parsePlaceholder :: String -> Maybe (String, String)
parsePlaceholder s
  | placeholderPrefix `isPrefixOf` s
    = case reads (drop (length placeholderPrefix) s) of
        [(ref, _post)] -- | _post == placeholderPostfix
          -> Just ref
        _ -> Nothing
  | otherwise
    = Nothing

renderExpr :: Int -- ^ Line length to assume
           -> Int -- ^ Indention
           -> (Either String (String,String) -> a -> a) -- ^ What to do with text
           -> a   -- ^ What to do at the end
           -> CoreExpr
           -> a
renderExpr lineLen indent printer finisher
  = render . nest indent . pprExpr
 where
  render = fullRender PageMode lineLen (ribbonsPerLine PP.style) printer' finisher
  printer' (Chr c) = printer (Left [c])
  printer' (Str s) = printer (Left s)
  printer' (PStr s)
    | Just ref <- parsePlaceholder s
                   = printer (Right ref)
    | otherwise    = printer (Left s)

dumpExpr :: CoreExpr -> String
dumpExpr = renderExpr (lineLength PP.style) 0 printer ""
 where printer (Left  s) ss = s ++ ss
       printer (Right _) ss = "#ref#" ++ ss
