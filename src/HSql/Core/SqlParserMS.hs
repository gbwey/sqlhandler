-- todo: compile times are slow deriving Generic?
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- |
 Module      : HSql.Core.SqlParserMS
 Description : simple sql server parser
 Copyright   : (c) Grant Weyburne, 2021
 License     : BSD-3
-}
module HSql.Core.SqlParserMS where

import Control.Applicative
import qualified Control.Applicative.Permutations as ZP
import Control.Arrow hiding ((<+>))
import Control.DeepSeq
import Control.Lens
import qualified Control.Monad.Combinators.NonEmpty as ZN
import Data.Char
import Data.Coerce
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Generics.Product
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy.Builder (fromText)
import Data.These
import DocUtils.Condition
import DocUtils.Doc
import DocUtils.Parser
import qualified GHC.Generics as G
import GHC.Stack
import Prettyprinter (Doc, Pretty (..), dot, (<+>))
import qualified Prettyprinter as P
import Text.Megaparsec
import qualified Text.Megaparsec as Z
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as ZL
import Text.Shakespeare.Text
import Utils.Error
import Utils.Parser
import qualified Validation as V

-- | space consumer for megaparsec
spaceconsumer :: ZParser ()
spaceconsumer =
  ZL.space
    space1
    (ZL.skipLineComment "//" <|> ZL.skipLineComment "--")
    (ZL.skipBlockComment "/*" "*/")

-- | wrapper for lexemes
lexeme :: ZParser a -> ZParser a
lexeme = ZL.lexeme spaceconsumer

-- | wrapper for symbols
symbol :: Text -> ZParser Text
symbol = ZL.symbol spaceconsumer

-- | validates the first character in a sql name
isValidTNameFirstChar :: Char -> Bool
isValidTNameFirstChar c = isAlpha c || elem @[] c "_@#"

-- | validates the subsequent character in a sql name
isValidTNameNextChar :: Char -> Bool
isValidTNameNextChar c = isValidTNameFirstChar c || isDigit c || c == '$'

-- | parse an unquoted sql name
unquotedTName1P :: ZParser NonEmptyText
unquotedTName1P = lexeme $ NonEmptyText <$> Z.satisfy isValidTNameFirstChar <*> Z.takeWhileP (Just "rest of unquotedTName1P") isValidTNameNextChar

-- | parser for a 'TName'
tNameP :: ZParser TName
tNameP = tNameP' msprefix

-- | holds the value for a name and optional delimiters used
data TName = TName
  { tNameRaw :: !NonEmptyText
  , tNamePrefix :: !(Maybe (Char, Char))
  }
  deriving stock (G.Generic, Show, Ord, Eq)

instance NFData TName

instance Pretty TName where
  pretty (TName tne ma) =
    pretty tne & case ma of
      Just (s, e) -> P.enclose (pretty s) (pretty e)
      Nothing -> id

-- | lens for 'tNameRaw' from 'TName'
tNameRawLens :: Lens' TName (NonEmpty Char)
tNameRawLens = the @"tNameRaw" . nonEmptyTextIso

-- | iso from 'NonEmptyText' to a non empty list of characters
nonEmptyTextIso :: Iso' NonEmptyText (NonEmpty Char)
nonEmptyTextIso =
  iso
    (\(NonEmptyText c ts) -> c :| T.unpack ts)
    toNonEmptyText

instance ToText TName where
  toText = fromText . showTName

-- | pretty print 'TName'
showTName :: TName -> Text
showTName = renderLong . pretty

-- | extract the text from 'TName'
fromTNameRaw :: TName -> Text
fromTNameRaw (TName a _) = fromNonEmptyText a

-- | unsafe convert text to 'TName'
toTNameUnsafe :: HasCallStack => Text -> TName
toTNameUnsafe ts = TName (toNonEmptyTextUnsafe ts) Nothing

-- | convert from 'NonEmptyText' to text
fromNonEmptyText :: NonEmptyText -> Text
fromNonEmptyText (NonEmptyText c ts) = c `T.cons` ts

instance IsString NonEmptyText where
  fromString = toNonEmptyText . fromList1 "fromString: NonEmptyText"

-- | unsafe convert from text to 'NonEmptyText'
toNonEmptyTextUnsafe :: HasCallStack => Text -> NonEmptyText
toNonEmptyTextUnsafe ts' = case T.uncons ts' of
  Nothing -> normalError "toNonEmptyText: no data"
  Just (c, ts) -> NonEmptyText c ts

-- | convert non empty list of characters to 'NonEmptyText'
toNonEmptyText :: NonEmpty Char -> NonEmptyText
toNonEmptyText (c :| cs) = NonEmptyText c (T.pack cs)

instance NFData NonEmptyText

-- | parser for 'TName' using possible delimiting quotes
tNameP' :: NonEmpty (Char, Char) -> ZParser TName
tNameP' ns = do
  mse <- optional $ asum $ N.map (\(s, e) -> (,e) <$> char s) ns
  case mse of
    z@(Just (s, e)) -> (\cs1 -> TName (toNonEmptyText cs1) z) <$> ZN.some (try (char e <* char e) <|> (anySingleBut e <?> ("anything but " <> [e]))) <* char e <?> ("delimiter(" ++ [e] ++ ") found (" ++ [s] ++ ")")
    Nothing -> flip TName Nothing <$> unquotedTName1P <?> "undelimited"

-- | fully qualified sql server table name
data TNames = TNames
  { tNamesDb :: !(Maybe TName)
  , tNamesSchema :: !(Maybe TName)
  , tNamesTable :: !TName
  }
  deriving stock (G.Generic, Show, Eq)

-- | parser for sql server tablename using only valid delimiters
tNamesP :: ZParser TNames
tNamesP = tNamesP' msprefix

-- | delimiters for mssql
msprefix :: NonEmpty (Char, Char)
msprefix = bq :| [dq]

-- | parse a sql serve table name
tableParser :: NonEmpty (Char, Char) -> Text -> Either Text TNames
tableParser ns = runP (tNamesP' ns <* eof <?> "tableParser")

-- | parser for a tablename using the given delimiters if quoted
tNamesP' :: NonEmpty (Char, Char) -> ZParser TNames
tNamesP' ns = do
  void space
  let p = tNameP' ns
  try ((\a b c -> TNames (Just a) (Just b) c) <$> p <* char '.' <*> p <* char '.' <*> p) -- a.b.c
    <|> try ((\a c -> TNames (Just a) Nothing c) <$> p <* ".." <*> p) -- a..c
    <|> try (TNames Nothing . Just <$> p <* char '.' <*> p) -- b.c
    <|> (TNames Nothing Nothing <$> p) -- c

instance Pretty CreateTable where
  pretty (CreateTable tns lrs mpk uniqs fks) =
    "create table" <+> pretty tns
      <+> P.parens
        ( P.hardline
            <> P.indent
              2
              ( P.vcat
                  ( zipWith
                      (<+>)
                      (" " : repeat P.comma)
                      ( pFilterEmpty
                          ( map (either pretty pretty) lrs
                              <> maybe mempty (pure . pretty) mpk
                              <> map pretty uniqs
                              <> map pretty fks
                          )
                      )
                  )
              )
            <> P.hardline
        )

-- | create table record
data CreateTable = CreateTable
  { ctTNames :: !TNames
  , ctColumns :: ![Either ColumnComputed Column]
  , ctConstraintPK :: !(Maybe ConstraintPK)
  , ctConstraintUnique :: ![ConstraintUnique]
  , ctConstraintFKs :: ![ConstraintFK]
  }
  deriving stock (G.Generic, Show, Eq)

-- | convert a complex nested construct into a 'CreateTable'
toCreateTable ::
  TNames ->
  NonEmpty (Either (TT2 (Either ConstraintPK ConstraintUnique) ConstraintFK) (Either ColumnComputed Column')) ->
  VE CreateTable
toCreateTable tns lrs =
  let prefix :: Text
      prefix = "toCreateTable:"
      mpk' = expectOneM Pre (prefix <> "pk constraint") (lrs ^.. types @ConstraintPK)
      uniqs = lrs ^.. types @ConstraintUnique
      fks = lrs ^.. types @ConstraintFK
   in case mpk' of
        V.Failure e -> V.Failure e
        V.Success mpk ->
          let mdefs = lrs ^.. types @ConstraintDefault . position @1
              cxnames = catMaybes (mdefs <> (mpk ^.. _Just . position @1) <> fks ^.. traverse . the @"fkTNameM" <> uniqs ^.. traverse . the @"uniqTNameM")
              chk1 = expectNoDupsBy Pre (prefix <> "constraint names") (T.toLower . fromTNameRaw) cxnames
              lrcs = map (right fromColumn') (rights $ N.toList lrs)
              colnames = map (ccName ||| cmName) lrcs
              chk2 = expectNoDupsBy Pre (prefix <> "column names") (T.toLower . fromTNameRaw) colnames
           in CreateTable tns lrcs mpk uniqs fks <$ chk1 <* chk2

-- | three way partition on a list
partitionTT3s :: [TT3 a b c] -> ([a], [b], [c])
partitionTT3s = foldMap' partitionTT3

-- | three way partition
partitionTT3 :: TT3 a b c -> ([a], [b], [c])
partitionTT3 = \case
  TT31 a -> ([a], [], [])
  TT32 b -> ([], [b], [])
  TT33 c -> ([], [], [c])

-- | four way partition on a list
partitionTT4s :: [TT4 a b c d] -> ([a], [b], [c], [d])
partitionTT4s = foldMap' partitionTT4

-- | four way partition
partitionTT4 :: TT4 a b c d -> ([a], [b], [c], [d])
partitionTT4 = \case
  TT41 a -> ([a], [], [], [])
  TT42 b -> ([], [b], [], [])
  TT43 c -> ([], [], [c], [])
  TT44 d -> ([], [], [], [d])

-- | convert from 'Column'' to 'Column'
fromColumn' :: Column' -> Column
fromColumn' (Column' a b mrg mns mid mdef _mlrpkuniq _mcfk) =
  Column a b mrg mns mid mdef

-- | run a sql table parser
createTable :: Text -> VE CreateTable
createTable txt = do
  case runP createTableP txt of
    Left e -> V.failure (ExpectFail, e)
    Right (a, b) -> toCreateTable a b

-- | sql table parser
createTableP :: ZParser (TNames, NonEmpty (Either (TT2 (Either ConstraintPK ConstraintUnique) ConstraintFK) (Either ColumnComputed Column')))
createTableP =
  (,)
    <$ space
    <* lexstring "create"
    <* lexstring "table"
    <*> lexeme tNamesP
    <*> between (lexchar '(') (lexchar ')') columnsP
    <* eof <?> "createTableP"

instance Pretty TNames where
  pretty (TNames ma mb c) =
    (<> pretty c) $ case (ma, mb) of
      (Nothing, Nothing) -> mempty
      (Just a, Just b) -> pretty a <> dot <> pretty b <> dot
      (Just a, Nothing) -> pretty a <> dot <> dot
      (Nothing, Just b) -> pretty b <> dot

instance Pretty Column where
  pretty (Column tnm (ColumnType tp mextra) mrg mnull mid mdef) =
    pretty tnm
      <+> pretty tp
      <+> P.sep
        ( catMaybes
            [ pretty <$> mextra
            , pretty <$> mrg
            , pretty <$> mnull
            , pretty <$> mid
            , pretty <$> mdef
            ]
        )

instance Pretty ColumnComputed where
  pretty (ColumnComputed tnm tks per) =
    pretty tnm
      <+> "as"
      <+> P.sep (map pretty (N.toList tks))
      <+> pretty per

instance Pretty Persisted where
  pretty =
    \case
      Persisted mnotnull mth ->
        let ret :: forall ann. Doc ann
            ret =
              case mth of
                Nothing -> mempty
                Just th -> mergeTheseWith (either pretty pretty) pretty (<+>) th <+> mempty
         in ret <> "persisted" <> docPref (pretty <$> mnotnull)
      NotPersisted -> mempty

-- | pretty print a constraint
prettyConstraintPrefix :: TName -> Doc ann
prettyConstraintPrefix cnm = "constraint" <+> pretty cnm

instance Pretty ConstraintUnique where
  pretty (ConstraintUnique mnm mclustered keys) =
    docSuff (prettyConstraintPrefix <$> mnm)
      <> "unique"
      <> docBoth (pretty <$> mclustered)
      <> pretty keys

instance Pretty SqlToken where
  pretty =
    \case
      STParens ts -> P.parens $ P.sep (map pretty ts)
      STQuoted x -> pretty x
      STUnquoted ts -> pretty ts

instance Pretty ConstraintPK where
  pretty (ConstraintPK mnm mclustered keys) =
    docSuff (prettyConstraintPrefix <$> mnm)
      <> "primary key"
      <> docBoth (pretty <$> mclustered)
      <> pretty keys

instance Pretty ConstraintFK where
  pretty (ConstraintFK mnm keys1 tnms keys2 mth) =
    docSuff (prettyConstraintPrefix <$> mnm)
      <> "foreign key"
      <+> pretty keys1
      <+> " references"
      <+> pretty tnms
      <> pretty keys2
      <> docPref (mergeTheseWith pretty pretty (<+>) <$> mth)

-- | find a field in a list of 'Column's
findField :: TName -> [Column] -> Maybe (Int, Column)
findField tn =
  toMaybeF "findField"
    . filter (on (==) (T.toLower . fromTNameRaw) tn . coerce . cmName . snd)
    . zip [0 ..]

-- | parser for a sql table column
columnsP :: ZParser (NonEmpty (Either (TT2 (Either ConstraintPK ConstraintUnique) ConstraintFK) (Either ColumnComputed Column')))
columnsP = ZN.sepBy1 (eitherP (try constraintTableP) columnLRP) (lexchar ',')

-- | character lexeme
lexchar :: Char -> ZParser Char
lexchar = lexeme . char

-- | holder for guid column
data RowGuidCol = RowGuidCol deriving stock (G.Generic, Show, Eq)

instance Pretty RowGuidCol where
  pretty RowGuidCol = "rowguidcol"

-- | identity column type
newtype IdentityType = IdentityType (Maybe (Int, Int)) deriving stock (G.Generic, Show, Eq)

instance Pretty IdentityType where
  pretty (IdentityType mij) =
    "identity" <> case mij of
      Nothing -> mempty
      Just (i, j) -> P.tupled (map pretty [i, j])

-- | parse an identity column
identityTypeP :: ZParser IdentityType
identityTypeP = do
  void $ lexstring "identity"
  ret <- optional $ do
    (,) <$ lexchar '(' <*> lexeme ZL.decimal <* lexchar ',' <*> lexeme ZL.decimal <* lexchar ')'
  return $ IdentityType ret

-- | column of mssql table
data Column = Column
  { cmName :: !TName
  , cmColumnType :: !ColumnType
  , cmRowGuidCol :: !(Maybe RowGuidCol)
  , cmNullable :: !(Maybe Nullable)
  , cmIdentity :: !(Maybe IdentityType)
  , cmConstraintDefault :: !(Maybe ConstraintDefault)
  }
  deriving stock (G.Generic, Show, Eq)

-- | computed column
data ColumnComputed = ColumnComputed
  { ccName :: !TName
  , ccTokens :: !(NonEmpty SqlToken)
  , ccPersisted :: !Persisted
  }
  deriving stock (G.Generic, Show, Eq)

-- | persisted vs non persisted column type
data Persisted
  = Persisted !(Maybe NotNullComputed) !(Maybe (These (Either ConstraintPK ConstraintUnique) ConstraintFK))
  | NotPersisted
  deriving stock (G.Generic, Show, Eq)

-- | parser for a computed column
columnComputedP :: TName -> ZParser ColumnComputed
columnComputedP nm = do
  (ct, mpp) <-
    try ((\a mb c -> (a, Just (mb, c))) <$> ZN.someTill sqlTokenP (lexstring "persisted") <*> optional (NotNullComputed <$ lexstring "not" <* lexstring "null") <*> Z.optional (constraintColumnComputedP (Just nm)))
      <|> (,Nothing) <$> ZN.some sqlTokenP
  return $ ColumnComputed nm ct (maybe NotPersisted (uncurry Persisted) mpp)

-- | parentheses delimiter
pq :: (Char, Char)
pq = ('(', ')')

-- | single quote delimiter
sq :: (Char, Char)
sq = ('\'', '\'')

-- | double quote delimiter
dq :: (Char, Char)
dq = ('"', '"')

-- | bracket delimiter
bq :: (Char, Char)
bq = ('[', ']')

-- | parser for a computed column or a normal column
columnLRP :: ZParser (Either ColumnComputed Column')
columnLRP = do
  nm <- lexeme tNameP
  try $ eitherP (lexstring "as" *> columnComputedP nm) (columnP nm)

-- | holder for a sql column
data Column'
  = Column'
      !TName
      !ColumnType
      !(Maybe RowGuidCol)
      !(Maybe Nullable)
      !(Maybe IdentityType)
      !(Maybe ConstraintDefault)
      !(Maybe (Either ConstraintPK ConstraintUnique))
      !(Maybe ConstraintFK)
  deriving stock (G.Generic, Show, Eq)

-- | parser for a column
columnP :: TName -> ZParser Column'
columnP nm = do
  ct <- columnTypeP
  let pmcx = optional $ lexstring "constraint" *> lexeme tNameP
  (a, b, c, d, e, f) <-
    ZP.runPermutation
      ( (,,,,,)
          <$> ZP.toPermutationWithDefault Nothing (Just RowGuidCol <$ lexstring "rowguidcol")
          <*> ZP.toPermutationWithDefault Nothing (Just NotNull <$ lexstring "not" <* lexstring "null" <|> Just Null <$ lexstring "null")
          <*> ZP.toPermutationWithDefault Nothing (Just <$> lexeme identityTypeP)
          <*> ZP.toPermutationWithDefault
            Nothing
            ( try $ do
                mcx <- pmcx
                Just <$> lexeme (constraintDefaultP mcx)
            )
          <*> ZP.toPermutationWithDefault
            Nothing
            ( try $ do
                mcx <- pmcx
                Just <$> lexeme (constraintPKPOrUnique mcx (Just nm))
            )
          <*> ZP.toPermutationWithDefault
            Nothing
            ( try $ do
                mcx <- pmcx
                Just <$> lexeme (constraintFKP mcx (Just nm))
            )
      )
  return $ Column' nm ct a b c d e f

-- | nullable type
data Nullable = NotNull | Null deriving stock (Show, Eq, G.Generic)

instance Pretty Nullable where
  pretty = \case
    NotNull -> "not null"
    Null -> "null"

-- | not null computed
data NotNullComputed = NotNullComputed deriving stock (Show, Eq, G.Generic)

instance Pretty NotNullComputed where
  pretty NotNullComputed = "not null"

-- | column type
data ColumnType = ColumnType
  { ctType :: !Text -- cant be wrapped
  , ctDetail :: !(Maybe ColumnTypeDetail)
  }
  deriving stock (G.Generic, Show, Eq)

-- | 'ColumnType' parser
columnTypeP :: ZParser ColumnType
columnTypeP = do
  x <- lexeme tNameP -- can be wrapped in dquotes or brackets!
  ret <- optional $ lexeme columnTypeDetailP
  return $ ColumnType (fromTNameRaw x) ret

-- | column size information
data ColumnTypeDetail = CTDMax | CTDInts !(NonEmpty Int) deriving stock (G.Generic, Show, Eq)

instance Pretty ColumnTypeDetail where
  pretty CTDMax = "(max)"
  pretty (CTDInts is) = P.tupled (map pretty (N.toList is))

-- | 'ColumnTypeDetail' parser
columnTypeDetailP :: ZParser ColumnTypeDetail
columnTypeDetailP = between (lexchar '(') (lexchar ')') ((CTDMax <$ lexstring "max") <|> (CTDInts <$> ZN.sepBy1 (lexeme ZL.decimal) (lexchar ',')))

-- | keys used in a constraint
newtype Keys = Keys (NonEmpty TName) deriving stock (G.Generic, Show, Eq)

instance Pretty Keys where
  pretty (Keys ns) = P.tupled (map pretty (N.toList ns))

-- | 'Keys' parser
keysP :: ZParser Keys
keysP = Keys <$> between (lexchar '(') (lexchar ')') (ZN.sepBy1 (lexeme tNameP) (lexchar ','))

-- | four item sum type
data TT4 a b c d = TT41 !a | TT42 !b | TT43 !c | TT44 !d deriving stock (G.Generic, Show, Eq)

-- | three item sum type
data TT3 a b c = TT31 !a | TT32 !b | TT33 !c deriving stock (G.Generic, Show, Eq)

-- | two item sum type
data TT2 a b = TT21 !a | TT22 !b deriving stock (G.Generic, Show, Eq)

-- | constraint column parser
constraintColumnP :: Maybe TName -> ZParser (TT3 ConstraintDefault (Either ConstraintPK ConstraintUnique) ConstraintFK)
constraintColumnP mnm = do
  mcx <- optional $ lexstring "constraint" *> lexeme tNameP
  (TT31 <$> constraintDefaultP mcx) <|> (TT32 <$> constraintPKPOrUnique mcx mnm) <|> (TT33 <$> constraintFKP mcx mnm)

-- | constraint computed column parser
constraintColumnComputedP :: Maybe TName -> ZParser (These (Either ConstraintPK ConstraintUnique) ConstraintFK)
constraintColumnComputedP mnm = do
  mcx <- optional $ lexstring "constraint" *> lexeme tNameP
  theseP (constraintPKPOrUnique mcx mnm) (constraintFKP mcx mnm)

-- | table constraint parser
constraintTableP :: ZParser (TT2 (Either ConstraintPK ConstraintUnique) ConstraintFK)
constraintTableP = do
  mcx <- optional $ lexstring "constraint" *> lexeme tNameP
  (TT21 <$> constraintPKPOrUnique mcx Nothing) <|> (TT22 <$> constraintFKP mcx Nothing)

-- | clustered vs non clustered index option
data Clustered = Clustered | NonClustered deriving stock (G.Generic, Show, Eq)

instance Pretty Clustered where
  pretty = \case
    Clustered -> "clustered"
    NonClustered -> "nonclustered"

-- | primary key or unique field
data PrimaryKey = PrimaryKey | Unique deriving stock (G.Generic, Show, Eq)

instance Pretty PrimaryKey where
  pretty = \case
    PrimaryKey -> "primary key"
    Unique -> "unique"

-- | primary key constraint
data ConstraintPK = ConstraintPK
  { pkTNameM :: !(Maybe TName)
  , pkClustered :: !(Maybe Clustered)
  , pkKeys :: !Keys
  }
  deriving stock (G.Generic, Show, Eq)

-- | unique constraint
data ConstraintUnique = ConstraintUnique
  { uniqTNameM :: !(Maybe TName)
  , uniqClustered :: !(Maybe Clustered)
  , uniqKeys :: !Keys
  }
  deriving stock (G.Generic, Show, Eq)

-- | parser for primary key or unique constraint
constraintPKPOrUnique :: Maybe TName -> Maybe TName -> ZParser (Either ConstraintPK ConstraintUnique)
constraintPKPOrUnique mcx mcolname = do
  lr <- eitherP (lexstring "primary" <* lexstring "key") (lexstring "unique")
  mcl <- optional (Clustered <$ lexstring "clustered" <|> NonClustered <$ lexstring "nonclustered")
  kys <- keysP <|> maybe (zfail "constraintPKPOrUnique: missing keys and not an inline pk constraint!") (pure . Keys . pure) mcolname
  pure $ case lr of
    Left{} -> Left $ ConstraintPK mcx mcl kys
    Right{} -> Right $ ConstraintUnique mcx mcl kys

-- | default constraint
data ConstraintDefault = ConstraintDefault
  { cdfTNameM :: !(Maybe TName)
  , cdfValue :: !SqlToken
  }
  deriving stock (G.Generic, Show, Eq)

instance Pretty ConstraintDefault where
  pretty (ConstraintDefault mnm val) =
    docSuff (prettyConstraintPrefix <$> mnm)
      <> pretty val

-- | non space token parser
nonSpaceTokenP :: ZParser Text
nonSpaceTokenP = lexeme (Z.takeWhile1P (Just "nonSpaceTokenP value") (not . isSpace))

-- | default constraint parser
constraintDefaultP :: Maybe TName -> ZParser ConstraintDefault
constraintDefaultP mcx =
  ConstraintDefault mcx
    <$ lexstring "default"
    <*> sqlTokenNonSpaceP

instance Pretty XQuotedString where
  pretty (XQuotedString (s, e) ts) = P.enclose (pretty s) (pretty e) (pretty ts)

-- | restore a 'XQuotedString' to its original text value
reifyXQuotedString :: XQuotedString -> Text
reifyXQuotedString (XQuotedString (s, e) ts) = s `T.cons` ts `T.snoc` e

-- | non empty text
data NonEmptyText = NonEmptyText
  { neChar :: !Char
  , neText :: !Text
  }
  deriving stock (G.Generic, Show, Eq, Ord)

instance Pretty NonEmptyText where
  pretty = pretty . fromNonEmptyText

-- | restore a 'XQuotedString' to its original text value
reifyXQuotedString1 :: XQuotedString1 -> Text
reifyXQuotedString1 (XQuotedString1 (s, e) net) = s `T.cons` fromNonEmptyText net `T.snoc` e

-- | non empty quoted string
data XQuotedString1 = XQuotedString1
  { xq1StartEnd :: !(Char, Char)
  , xq1NonEmptyText :: !NonEmptyText
  }
  deriving stock (G.Generic, Show, Eq)

-- | 'XQuotedString1' parser
xQuotedString1P :: (Char, Char) -> ZParser XQuotedString1
xQuotedString1P z@(s, e) = lexeme $ do
  void $ char s
  cs1 <- ZN.some (try (char e <* char e) <|> (anySingleBut e <?> ("anything but " <> [e])))
  void $ char e
  pure $ XQuotedString1 z (toNonEmptyText cs1)

-- | parse a 'XQuotedString' using sql server delimiters
singleQuotedStringP :: ZParser XQuotedString
singleQuotedStringP = xQuotedStringP sq

-- | parse a 'XQuotedString1' using sql server delimiters
singleQuotedString1P :: ZParser XQuotedString1
singleQuotedString1P = xQuotedString1P sq

-- | quoted string with delimiters
data XQuotedString = XQuotedString
  { xqStartEnd :: !(Char, Char)
  , xqText :: !Text
  }
  deriving stock (G.Generic, Show, Eq)

-- | 'XQuotedString' parser
xQuotedStringP :: (Char, Char) -> ZParser XQuotedString
xQuotedStringP z@(s, e) = lexeme $ do
  void $ char s
  xs <- Z.many (try (char e <* char e) <|> (anySingleBut e <?> ("anything but " <> [e])))
  void $ char e
  pure $ XQuotedString z (T.pack xs)

-- | foreign key constraint
data ConstraintFK = ConstraintFK
  { fkTNameM :: !(Maybe TName)
  , fkKeysFrom :: !Keys
  , fkTableRef :: !TNames
  , fkKeysRef :: !Keys
  , fkOns :: !(Maybe (These FKOnDelete FKOnUpdate))
  }
  deriving stock (G.Generic, Show, Eq)

-- | parser for 'ConstraintFK'
constraintFKP :: Maybe TName -> Maybe TName -> ZParser ConstraintFK
constraintFKP mcx mcolname =
  ConstraintFK mcx
    <$ optional (lexstring "foreign" <* lexstring "key")
    <*> (keysP <|> maybe (zfail "constraintFKP: missing keys and not an inline fk constraint!") (pure . Keys . pure) mcolname)
    <* lexstring "references"
    <*> lexeme tNamesP
    <*> keysP
    <*> optional (theseP (try fkOnDeleteP) (try fkOnUpdateP))

-- identity and constraint can be swapped: handle not null

-- | case insensitive string parser
lexstring :: Text -> ZParser Text
lexstring = lexeme . string'

--- starts with a-z _@# and then can allow numbers
-- can x.y.z or x..z or x.z [can have double quotes or [] for any section
-- [a-zA-Z_@#]

-- | bcp filename parser
matchBcpP :: Text -> ZParser Int
matchBcpP ss = string' ss *> ZL.decimal <* string' ".bcp" <* eof

-- | foreign key on delete action
newtype FKOnDelete = FKOnDelete FKOnAction deriving stock (G.Generic, Show, Eq)

instance Pretty FKOnDelete where
  pretty (FKOnDelete x) = "on delete" <+> pretty x

-- | foreign key on update action
newtype FKOnUpdate = FKOnUpdate FKOnAction deriving stock (G.Generic, Show, Eq)

instance Pretty FKOnUpdate where
  pretty (FKOnUpdate x) = "on update" <+> pretty x

-- | foreign key extra keywords
data FKOnAction = FKNoAction | FKCascade | FKSetNull | FKSetDefault deriving stock (G.Generic, Show, Eq)

instance Pretty FKOnAction where
  pretty =
    \case
      FKNoAction -> "no action"
      FKCascade -> "cascade"
      FKSetNull -> "set null"
      FKSetDefault -> "set default"

-- | 'FKOnDelete' parser
fkOnDeleteP :: ZParser FKOnDelete
fkOnDeleteP =
  FKOnDelete
    <$ lexstring "on"
    <* lexstring "delete"
    <*> fkOnActionP

-- | 'FKOnUpdate' parser
fkOnUpdateP :: ZParser FKOnUpdate
fkOnUpdateP =
  FKOnUpdate
    <$ lexstring "on"
    <* lexstring "update"
    <*> fkOnActionP

-- | 'FKOnAction' parser
fkOnActionP :: ZParser FKOnAction
fkOnActionP =
  FKNoAction <$ lexstring "no" <* lexstring "action"
    <|> FKCascade <$ lexstring "cascade"
    <|> lexstring "set" *> (FKSetNull <$ lexstring "null" <|> FKSetDefault <$ lexstring "default")

-- | parser for an unquoted non empty string
unquotedTokenPY :: ZParser Text
unquotedTokenPY =
  lexeme $
    takeWhile1P (Just "unquotedTokenPY") f
 where
  f c =
    (isAlphaNum c || isPunctuation c || isSymbol c)
      && notElem @[] c ",()[]'\","

-- | non space token parser helper
nonSpaceTokenPX :: ZParser Text
nonSpaceTokenPX = lexeme $ do
  zz <- takeWhile1P Nothing (\c -> not (isSpace c) && notElem @[] c "(,)")
  yy <- optional (renderLong . pretty <$> sqlParenP)
  return $ zz <> fromMaybe mempty yy

-- | non space token
sqlTokenNonSpaceP :: ZParser SqlToken
sqlTokenNonSpaceP = STQuoted <$> asum (map xQuotedStringP [bq, dq, sq]) <|> sqlParenP <|> STUnquoted <$> nonSpaceTokenPX

-- | token types
data SqlToken
  = STParens ![SqlToken]
  | STQuoted !XQuotedString
  | STUnquoted !Text
  deriving stock (G.Generic, Show, Eq)

-- | 'SqlToken' parser
sqlTokenP :: ZParser SqlToken
sqlTokenP = STQuoted <$> asum (map xQuotedStringP [bq, dq, sq]) <|> sqlParenP <|> STUnquoted <$> unquotedTokenPY

-- | 'SqlToken' parser with parentheses
sqlParenTokenP :: ZParser SqlToken
sqlParenTokenP = STQuoted <$> asum (map xQuotedStringP [bq, dq, sq]) <|> sqlParenP <|> STUnquoted <$> lexeme (takeWhile1P (Just "unquotedTokenPY") (\c -> notElem @[] c "'\"[]()"))

-- | 'SqlToken' parser that pulls out the elements within parentheses
sqlParenP :: ZParser SqlToken
sqlParenP = lexeme $ do
  void $ lexchar '('
  xs <- Z.many $ lexeme sqlParenTokenP
  void $ lexchar ')'
  return $ STParens xs

-- | pretty print the sql table
prettyCreateTable :: Text -> IO ()
prettyCreateTable =
  T.putStrLn . V.validation psiT (renderLong . pretty) . createTable

-- | pretty print table name and columns
getInsertableFields :: CreateTable -> (Text, [Text])
getInsertableFields ct =
  let cols = map (renderLong . pretty . cmName) . filter (isNothing . cmIdentity) $ rights (ctColumns ct)
   in (renderLong (pretty (ctTNames ct)), cols)
