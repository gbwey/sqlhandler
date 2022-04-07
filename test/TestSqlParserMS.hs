{-
tm = toUtcTime (2021,12,15) (12,15,59)
(renderLong . pretty <$> tableParser msprefix "a.b.c" & _Right . the @"tNamesTable" . tNameRawLens <>~ ('_' :| TimeUtils.formatUtc tm))
@?= "a.b.c_20210920_135111"
(renderLong . pretty <$> tableParser msprefix "[a].b.[c]" & _Right . the @"tNamesTable" . tNameRawLens <>~ ('_' :| TimeUtils.formatUtc tm))
@?= "[a].b.[c_20210920_135111]"
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module TestSqlParserMS where

import Control.Lens
import Control.Monad
import Data.Generics.Product
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import DocUtils.Condition
import DocUtils.Doc
import DocUtils.Parser
import DocUtils.Time
import HSql.Core.SqlParserMS
import Prettyprinter (Pretty (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Shakespeare.Text (st)
import qualified Validation as V

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestSqlParserMS"
    [ testCase "unquoted tname" $ runP (tNameP' (pure dq)) "abc" @?= Right (TName "abc" Nothing)
    , testCase "quoted tname1" $ runP (tNameP' (pure dq)) "\"abc\"" @?= Right (TName "abc" (Just dq))
    , testCase "quoted tname1 with embedded quotes" $ runP (tNameP' (pure dq)) "\"a\"\"bc\"" @?= Right (TName "a\"bc" (Just dq))
    , testCase "quoted tname but wrong delimiter" $ expectLeftInfix (pure "expecting '[' or undelimited") (runP (tNameP' (pure bq)) "\"abc\"") @?= Right ()
    , testCase "unquoted tname with invalid char" $ expectLeftInfix (pure "expecting end of input or rest of") (runP (tNameP' (pure bq) *> eof) "abcd+") @?= Right ()
    , testCase "missing end quote" $ expectLeftInfix (pure "expecting ']'") (runP (tNameP' (pure bq)) "[abc  ") @?= Right ()
    , testCase "quoted tname2" $ runP (tNameP' (pure bq)) "[abc]" @?= Right (TName "abc" (Just bq))
    , testCase "quoted tname3" $ runP (tNameP' msprefix) "[abc]" @?= Right (TName "abc" (Just bq))
    , testCase "quoted tname4" $ runP (tNameP' msprefix) "\"abc\"" @?= Right (TName "abc" (Just dq))
    , testCase "tname with extra junk" $ expectLeftInfix (pure "end of input") (runP (tNameP' msprefix <* eof) "[abc] extra junk") @?= Right ()
    , testCase "quoted tname can start with a number" $ runP (tNameP' msprefix) "\" 4abc   \"" @?= Right (TName " 4abc   " (Just dq))
    , testCase "unquoted tname cannot start with a digit" $ expectLeftInfix (pure "unexpected '4'") (runP (tNameP' msprefix) "4abc") @?= Right ()
    , testCase "paren empty" $ runP sqlParenP "()" @?= Right (STParens [])
    , testCase "paren 1 char" $ runP sqlParenP "(x)" @?= Right (STParens [STUnquoted "x"])
    , testCase "paren quotes iside" $ runP sqlParenP "(x'11''a'b)" @?= Right (STParens [STUnquoted "x", STQuoted (XQuotedString ('\'', '\'') "11'a"), STUnquoted "b"])
    , testCase "nested parens" $ runP sqlParenP "(((x))y)" @?= Right (STParens [STParens [STParens [STUnquoted "x"]], STUnquoted "y"])
    , testCase "paren comma" $ runP sqlParenP "(1,2)" @?= Right (STParens [STUnquoted "1,2"])
    , testCase "paren complex" $ runP sqlParenP "( 1 (,2)[bb])" @?= Right (STParens [STUnquoted "1 ", STParens [STUnquoted ",2"], STQuoted (XQuotedString ('[', ']') "bb")])
    , testCase "append to tnames" $
        let tm = toUtcTime' (2021, 9, 20) (13, 51, 11)
            v = tableParser msprefix "a.b.c" & _Right . the @"tNamesTable" . tNameRawLens <>~ ('_' :| formatUtc tm)
         in fmap (renderLong . pretty) v @?= Right "a.b.c_20210920_135111"
    , testCase "append to tnames with delimiters" $
        let tm = toUtcTime' (2021, 9, 20) (13, 51, 11)
            v = tableParser msprefix "\"abc\"\"def\".b.[c]" & _Right . the @"tNamesTable" . tNameRawLens <>~ ('_' :| formatUtc tm)
         in fmap (renderLong . pretty) v @?= Right "\"abc\"def\".b.[c_20210920_135111]"
    , testCase "pretty tname1" $ renderLong (pretty (TName "abc" (Just ('[', ']')))) @?= "[abc]"
    , testCase "pretty tname2" $ renderLong (pretty (TName "abc" Nothing)) @?= "abc"
    , testCase "pretty tname3" $ renderLong (pretty (TName "ab\"c" (Just ('"', '"')))) @?= "\"ab\"c\""
    , testCase "pretty tname complex delimiters" $ (renderLong . pretty <$> runP (tNamesP' msprefix) "[ab ]]c].\"x!+/yz\".abc") @?= Right "[ab ]c].\"x!+/yz\".abc"
    , testCase "tableParser unquoted simple" $ tableParser (pure dq) "abc" @?= Right (TNames Nothing Nothing (TName "abc" Nothing))
    , testCase "tableParser unquoted complex" $ tableParser (pure dq) "abc.def.ghi" @?= Right (TNames (Just (TName "abc" Nothing)) (Just (TName "def" Nothing)) (TName "ghi" Nothing))
    , testCase "tableParser unquoted a..b" $ tableParser (pure dq) "abc..ghi" @?= Right (TNames (Just (TName "abc" Nothing)) Nothing (TName "ghi" Nothing))
    , testCase "tableParser unquoted b.c" $ tableParser (pure dq) "def.ghi" @?= Right (TNames Nothing (Just (TName "def" Nothing)) (TName "ghi" Nothing))
    , testCase "tableParser quoted complex" $ tableParser msprefix "[abc].[def].[ghi]" @?= Right (TNames (Just (TName "abc" (Just bq))) (Just (TName "def" (Just bq))) (TName "ghi" (Just bq)))
    , testCase "create table simple" $
        createTable "create table x (a int)"
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                ]
                Nothing
                []
                []
            )
    , testCase "create table computed persisted" $
        createTable "create table x (a as getdate persisted not null)"
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Left $
                    ColumnComputed
                      (TName "a" Nothing)
                      (STUnquoted "getdate" :| [])
                      (Persisted (Just NotNullComputed) Nothing)
                ]
                Nothing
                []
                []
            )
    , testCase "create table computed not persisted" $
        createTable "create table x (a as x.y/2)"
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Left $
                    ColumnComputed
                      (TName "a" Nothing)
                      (STUnquoted "x.y/2" :| [])
                      NotPersisted
                ]
                Nothing
                []
                []
            )
    , testCase "create table complex with mixed columns" $
        createTable
          [st|
create table x
  ( a0 int not null
  , a2 as x.y / 2 persisted
  , a3 as getdate()
  , a4 varchar(10) identity
  )|]
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a0" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      (Just NotNull)
                      Nothing
                      Nothing
                , Left $
                    ColumnComputed
                      (TName "a2" Nothing)
                      ( STUnquoted "x.y"
                          :| [ STUnquoted "/"
                             , STUnquoted "2"
                             ]
                      )
                      (Persisted Nothing Nothing)
                , Left $
                    ColumnComputed
                      (TName "a3" Nothing)
                      (STUnquoted "getdate" :| [STParens []])
                      NotPersisted
                , Right $
                    Column
                      (TName "a4" Nothing)
                      (ColumnType "varchar" (Just (CTDInts (pure 10))))
                      Nothing
                      Nothing
                      (Just (IdentityType Nothing))
                      Nothing
                ]
                Nothing
                []
                []
            )
    , testCase "create table simple with pk col" $
        createTable "create table x.[ y  ].\"z4w\" (aa varchar(10) not null identity(3,4) primary key nonclustered)"
          @?= V.Success
            ( CreateTable
                ( TNames
                    (Just (TName "x" Nothing))
                    (Just (TName " y  " (Just ('[', ']'))))
                    (TName "z4w" (Just ('"', '"')))
                )
                [ Right $
                    Column
                      (TName "aa" Nothing)
                      (ColumnType "varchar" (Just (CTDInts (pure 10))))
                      Nothing
                      (Just NotNull)
                      (Just (IdentityType (Just (3, 4))))
                      Nothing
                ]
                ( Just $
                    ConstraintPK
                      Nothing
                      (Just NonClustered)
                      (Keys (pure (TName "aa" Nothing)))
                )
                []
                []
            )
    , testCase "create with unique constraints" $
        createTable
          [st|
create table bb.aa
(a int unique
,  b int constraint www unique
, constraint xxx unique ([c])
)|]
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    (Just (TName "bb" Nothing))
                    (TName "aa" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                , Right $
                    Column
                      (TName "b" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                ]
                Nothing
                [ ConstraintUnique Nothing Nothing (Keys (pure (TName "a" Nothing)))
                , ConstraintUnique (Just (TName "www" Nothing)) Nothing (Keys (pure (TName "b" Nothing)))
                , ConstraintUnique (Just (TName "xxx" Nothing)) Nothing (Keys (pure (TName "c" (Just ('[', ']')))))
                ]
                []
            )
    , testCase "create table with default constraint with ()" $
        createTable
          [st|
create table x
(a int constraint dfd default getdate() null
, b int not null
)|]
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      (Just Null)
                      Nothing
                      (Just (ConstraintDefault (Just (TName "dfd" Nothing)) (STUnquoted "getdate()")))
                , Right $
                    Column
                      (TName "b" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      (Just NotNull)
                      Nothing
                      Nothing
                ]
                Nothing
                []
                []
            )
    , testCase "create table simple with default and identity" $
        createTable "create table x (a int default 'xx' identity)"
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      (Just (IdentityType Nothing))
                      (Just (ConstraintDefault Nothing (STQuoted (XQuotedString ('\'', '\'') "xx"))))
                ]
                Nothing
                []
                []
            )
    , testCase "create table simple with comments" $
        createTable
          [st|create table x // gnort
(a int default /* hello
   */ 1.244
   )
|]
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      (Just (ConstraintDefault Nothing (STUnquoted "1.244")))
                ]
                Nothing
                []
                []
            )
    , testCase "create table with duplicate constraint names at table level" $
        expectVFailureWith
          ["constraint names"]
          (pure ExpectNoDups)
          ( createTable
              [st|
create table x
(a int
, b int
, constraint "abc" foreign key ( [a]) references xyz (aaa)
, constraint [abc] primary key (a))|]
          )
          @?= Right ()
    , testCase "create table duplicate constraint names at column level" $
        expectVFailureWith
          ["constraint names"]
          (pure ExpectNoDups)
          (createTable "create table x (a int, b int constraint \"abc\" foreign key ( [a]) references xyz (aaa) identity(9,2) constraint zzz default '13' constraint [abc] primary key (a))")
          @?= Right ()
    , testCase "create table simple with foreign key primary key at table level" $
        createTable "create table x (a int, b int, constraint [mypk] primary key (a) , constraint \"myfk\" foreign key ( [a]) references [xy]]z]..\"ab\"\"cd\" (aaa) )"
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                , Right $
                    Column
                      (TName "b" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                ]
                ( Just $
                    ConstraintPK
                      (Just (TName "mypk" (Just ('[', ']'))))
                      Nothing
                      (Keys (pure (TName "a" Nothing)))
                )
                []
                ( pure $
                    ConstraintFK
                      (Just (TName "myfk" (Just ('"', '"'))))
                      (Keys (pure (TName "a" (Just ('[', ']')))))
                      (TNames (Just (TName "xy]z" (Just ('[', ']')))) Nothing (TName "ab\"cd" (Just ('"', '"'))))
                      (Keys (pure (TName "aaa" Nothing)))
                      Nothing
                )
            )
    , testCase "create table simple with foreign key + on update and primary key at column level" $
        createTable "create table x (a int, b int constraint \"myfk\" foreign key ( [a]) references xyz (aaa) on update set null  identity(9,2) constraint zzz default '13' constraint [def] primary key (a))"
          @?= V.Success
            ( CreateTable
                (TNames Nothing Nothing (TName "x" Nothing))
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                , Right $
                    Column
                      (TName "b" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      (Just $ IdentityType (Just (9, 2)))
                      (Just $ ConstraintDefault (Just (TName "zzz" Nothing)) (STQuoted (XQuotedString ('\'', '\'') "13")))
                ]
                ( Just $
                    ConstraintPK
                      (Just (TName "def" (Just ('[', ']'))))
                      Nothing
                      (Keys (pure (TName "a" Nothing)))
                )
                []
                ( pure $
                    ConstraintFK
                      (Just (TName "myfk" (Just ('"', '"'))))
                      (Keys (pure (TName "a" (Just ('[', ']')))))
                      (TNames Nothing Nothing (TName "xyz" Nothing))
                      (Keys (pure (TName "aaa" Nothing)))
                      (Just (That (FKOnUpdate FKSetNull)))
                )
            )
    , testCase "create table simple with foreign key and on action primary key at table level" $
        createTable "create table x (a int, b int, constraint [mypk] primary key (a) , constraint \"myfk\" foreign key ( [a]) references xyz (aaa) on update no action   on delete cascade)"
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "x" Nothing)
                )
                [ Right $
                    Column
                      (TName "a" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                , Right $
                    Column
                      (TName "b" Nothing)
                      (ColumnType "int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                ]
                ( Just $
                    ConstraintPK
                      (Just (TName "mypk" (Just ('[', ']'))))
                      Nothing
                      (Keys (pure (TName "a" Nothing)))
                )
                []
                ( pure $
                    ConstraintFK
                      (Just (TName "myfk" (Just ('"', '"'))))
                      (Keys (pure (TName "a" (Just ('[', ']')))))
                      (TNames Nothing Nothing (TName "xyz" Nothing))
                      (Keys (pure (TName "aaa" Nothing)))
                      (Just (These (FKOnDelete FKCascade) (FKOnUpdate FKNoAction)))
                )
            )
    , testCase "multiple column fkey" $
        createTable
          [st|
CREATE TABLE Course_Strength_TSQL (
Course_ID Int,
Course_Strength Varchar(20)
CONSTRAINT FK FOREIGN KEY (Course_ID,a,[b] )
REFERENCES COURSE (Course_IDX,[c],  "d", e )
)|]
          @?= V.Success
            ( CreateTable
                ( TNames
                    Nothing
                    Nothing
                    (TName "Course_Strength_TSQL" Nothing)
                )
                [ Right $
                    Column
                      (TName "Course_ID" Nothing)
                      (ColumnType "Int" Nothing)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                , Right $
                    Column
                      (TName "Course_Strength" Nothing)
                      (ColumnType "Varchar" (Just (CTDInts (pure 20))))
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                ]
                Nothing
                []
                ( pure $
                    ConstraintFK
                      (Just (TName "FK" Nothing))
                      (Keys (TName "Course_ID" Nothing :| [TName "a" Nothing, TName "b" (Just ('[', ']'))]))
                      (TNames Nothing Nothing (TName "COURSE" Nothing))
                      (Keys (TName "Course_IDX" Nothing :| [TName "c" (Just ('[', ']')), TName "d" (Just ('"', '"')), TName "e" Nothing]))
                      Nothing
                )
            )
    , testCase "test bulk createTables" $ do
        forM_ (zip @Int [1 ..] tests) $ \(i, t) -> case createTable t of
          V.Success{} -> return ()
          V.Failure e ->
            assertFailure $
              show i
                <> " of "
                <> show (length tests)
                <> " failed: \nsql>>>\n"
                <> T.unpack t
                <> "<<<\n\n"
                <> psiS e
    ]

testall :: VE [CreateTable]
testall = traverse createTable tests

tests :: [Text]
tests =
  L.unfoldr f (T.lines testdata)
 where
  f = \case
    [] -> Nothing
    xs@(_ : _) ->
      let (as, bs) = break (T.null . T.strip) xs
       in Just (T.unlines as, dropWhile (T.null . T.strip) bs)

testdata :: Text
testdata =
  [st|CREATE TABLE #MyTempTable (
    col1 INT PRIMARY KEY
)

CREATE TABLE Grade3Students
(
StudentId    int          NOT NULL,
FirstName    varchar(20)  NOT NULL,
LastName     varchar(20)  NOT NULL,
DateOfBirth  date         NOT NULL,
Address      varchar(30)  NULL,
PhoneNumber  nvarchar(10) NULL,
DepartmentId int          NOT NULL
)

CREATE TABLE Grade3Students
(
StudentId    int PRIMARY KEY IDENTITY(1,1),
FirstName    varchar(20) NOT NULL,
LastName     varchar(20) NOT NULL,
DateOfBirth  date NOT NULL,
Address      varchar(30) NULL,
PhoneNumber  nvarchar(10) NULL,
DepartmentId int NOT NULL
)

CREATE TABLE Grade3Students
(
StudentId int IDENTITY(1,1) CONSTRAINT pk_Grade3Students_StudentId PRIMARY KEY,
FirstName varchar(20) NOT NULL,
LastName varchar(20) NOT NULL,
DateOfBirth date NOT NULL,
Address varchar(30) NULL,
PhoneNumber nvarchar(10) NULL,
DepartmentId int NOT NULL
)

CREATE TABLE Grade3Students
(
StudentId int IDENTITY(1,1),
FirstName varchar(20) NOT NULL,
LastName varchar(20) NOT NULL,
DateOfBirth date NOT NULL,
Address varchar(30) NULL,
PhoneNumber nvarchar(10) NULL,
DepartmentId int NOT NULL,
CONSTRAINT pk_Grade3Students_StudentId PRIMARY KEY(StudentId)
)

CREATE TABLE Grade3Students
(
StudentId int PRIMARY KEY IDENTITY(1,1),
FirstName varchar(20) NOT NULL,
LastName varchar(20) NOT NULL,
DateOfBirth date NOT NULL,
Address varchar(30) NULL,
PhoneNumber nvarchar(10) NULL,
DepartmentId int NOT NULL,
FOREIGN KEY (DepartmentId) REFERENCES Departments (DepartmentId)
)

CREATE TABLE Grade3Students
(
StudentId int PRIMARY KEY IDENTITY(1,1),
FirstName varchar(20) NOT NULL,
LastName varchar(20) NOT NULL,
DateOfBirth date NOT NULL,
Address varchar(30) NULL,
PhoneNumber nvarchar(10) NULL,
DepartmentId int NOT NULL,
CONSTRAINT FK_Grade3Students_DepartmentId FOREIGN KEY (DepartmentId) REFERENCES Departments
(DepartmentId)
)

create table Log (
   logMetaId int
  ,name varchar(100)
  ,schemaname varchar(100)
  ,cnt int
  ,creationDate datetime2
  ,lastModifiedDate datetime2
  ,constraint fk_log_logMeta foreign key (logMetaId) references LogMeta (id)
)

create table LogMeta (
   id int identity(1,1) primary key
  ,dt datetime2
  ,dbname varchar(100)
  ,dbextra varchar(200)
  ,txt varchar(100)
)

create table LogCmd (
   id int identity(1,1) primary key
  ,cmd varchar(200) not null
  ,started datetime2 not null
  ,ended datetime2 not null
  ,ok tinyint not null
  ,errmsg varchar(max) null
  ,returntext varchar(max) null
  ,fullcmd varchar(4000) not null
  ,extra varchar(2000) not null
  ,machine varchar(100) not null
  ,directory varchar(500) not null
  ,gitinfo varchar(4000) null
  ,comments varchar(4000) null
  ,logname varchar(2000) null
  ,token varchar(2000) null
)

create table dVH_ETLOUT (
  ARIA_PlanEitherSer bigint null
, CourseSer int not null
, PatientSer int not null
, PlanSumSer bigint null
, PlanSetupSer bigint null
, id int not null
, PatientID varchar(20) not null
, CourseID varchar(40) not null
, PlanSetupID varchar(40) not null
, IsPlanSum tinyint not null
, PlanUIDs varchar(500) not null
, StructureID varchar(40) not null
, Volume float null
, Min_Gy float null
, Max_Gy float null
, Mean_Gy float null
, Median_Gy float null
, Stdev_Gy float null
, D0p05cc_Gy float null
, DC0p05cc_Gy float null
, V20cc_Gy float null
, Coverage float null
, DvhRelativeVolume varchar(1000) null
, DvhAbsoluteVolume varchar(max) null
, DvhBiometric25 varchar(max) null constraint [dVH_ETLOUT_pk] primary key clustered (id)
, DvhBiometric5 varchar(4000) null
, DvhBiometric10 varchar(4000) null
, dateadded datetime not null
, fromfilename varchar(100) not null
, StandardStructureName varchar(50) null
)

CREATE TABLE dbo.Products
   (
      ProductID int IDENTITY (1,1) NOT NULL
      , QtyAvailable smallint
      , UnitPrice money
      , InventoryValue AS QtyAvailable * UnitPrice
    )

CREATE TABLE dbo.PurchaseOrderDetail
(
    PurchaseOrderID int NOT NULL
        REFERENCES Purchasing.PurchaseOrderHeader(PurchaseOrderID),
    LineNumber smallint NOT NULL,
    ProductID int NULL
        REFERENCES Production.Product(ProductID),
    UnitPrice money NULL,
    OrderQty smallint NULL,
    ReceivedQty float NULL,
    RejectedQty float NULL,
    DueDate datetime NULL,
    rowguid uniqueidentifier ROWGUIDCOL NOT NULL
        CONSTRAINT DF_PurchaseOrderDetail_rowguid DEFAULT (NEWID()),
    ModifiedDate datetime NOT NULL
        CONSTRAINT DF_PurchaseOrderDetail_ModifiedDate DEFAULT (GETDATE()),
    LineTotal AS ((UnitPrice*OrderQty)),
    StockedQty AS ((ReceivedQty-RejectedQty)),
    CONSTRAINT PK_PurchaseOrderDetail_PurchaseOrderID_LineNumber
               PRIMARY KEY CLUSTERED (PurchaseOrderID, LineNumber)
    /*           WITH (IGNORE_DUP_KEY = OFF) */
)

CREATE TABLE dbo.mytable
(
    low INT,
    high INT,
    myavg AS (low + high)/2
)

CREATE TABLE UDTypeTable
(
    u UTF8STRING,
    ustr AS u.ToString() PERSISTED
)

CREATE TABLE dbo.Globally_Unique_Data
(
    GUID UNIQUEIDENTIFIER
        CONSTRAINT Guid_Default DEFAULT
        NEWSEQUENTIALID() ROWGUIDCOL,
    Employee_Name VARCHAR(60)
    CONSTRAINT Guid_PK PRIMARY KEY (GUID)
)

CREATE TABLE Course_Strength_TSQL
(
Course_ID Int,
Course_Strength Varchar(20)
CONSTRAINT FK FOREIGN KEY (Course_ID)
REFERENCES COURSE (Course_ID)
)

CREATE TABLE Course_Strength_TSQL
(
Course_ID Int,
Course_Strength Varchar(20)
CONSTRAINT FK FOREIGN KEY (Course_ID,a,[b] )
REFERENCES COURSE (Course_IDX,[c],  "d", e )
)

|]
