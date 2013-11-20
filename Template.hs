{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
module Template where

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import GHC.Generics
import qualified Data.ByteString as BS
import GHC.Int
import GHC.Word
import Control.Monad
import Control.Monad.Loops
import Control.Applicative
import Data.List
import Data.Maybe

type Fixed = Word32
type FWord = Int16
type DateTime = Word64
newtype AString = AString [Char] deriving Show
data PString = PString Word8 [Char] deriving Show

instance Serialize PString where
    get = do
        n <- get
        str <- getN n
        return (PString n str)
    put (PString l s) = do put l; put s

getN n = replicateM (fromIntegral n) get
 
instance Serialize AString where
    get = do return $ AString ""
    put (AString cs) = puts cs

getStr n = do
    str <- getN n
    return (AString str)

puts x = mapM_ put x

data TTF = TTF {
    ttfOffset :: Offset,
    tableDefs :: [TableDef],
    tables    :: [Table]
    } deriving (Generic, Show)

instance Serialize TTF where
    get = do
        o@(Offset s n r e sh) <- get
        tdefs <- replicateM (fromIntegral n) get
        let start = 12 + 16*(fromIntegral n)
        tabs <- mapM (\e -> lookAhead (tableFromTag start e)) tdefs
        return $ TTF o tdefs tabs

tableFromTag start (TableDef (AString tag) _ off len) = do
    skip $ fromIntegral (off-start)
    bs <- getBytes $ fromIntegral len
    case (runGet (fromTag tag) bs) of
        (Left m) -> error m
        (Right t) -> return t

fromTag tag = do
    case tag of
        "cmap" -> liftM CmapTable get
        "glyf" -> liftM GlyfTable get
        "head" -> liftM HeadTable get
        "hhea" -> liftM HheaTable get
        "hmtx" -> liftM HmtxTable get
        "loca" -> liftM LocaTable get
        "maxp" -> liftM MaxpTable get
        "name" -> liftM NameTable get
        "post" -> liftM PostTable get
        _      -> return $ UnknownTable tag

tagFromTable t =
    case t of
        (CmapTable _) -> AString "cmap"
        (GlyfTable _) -> AString "glyf"
        (LocaTable _) -> AString "loca"
        (HeadTable _) -> AString "head"
        (HheaTable _) -> AString "hhea"
        (HmtxTable _) -> AString "hmtx"
        (MaxpTable _) -> AString "maxp"
        (NameTable _) -> AString "name"
        (PostTable _) -> AString "post"
        (UnknownTable tag) -> AString tag

ttfTables tables = do
    let n = fromIntegral $ length tables
    let tabs  = map (\t -> (tagFromTable t,encode t)) tables
    let (_, tdefs) = mapAccumL (\a (tag, bs) -> (a + (fromIntegral $ BS.length bs), tableEntry tag bs (fromIntegral a))) (12 + 16*n) tabs
    put $ offset n
    puts tdefs
    puts $ map snd tabs

data Offset = Offset {
    scalarType      :: Word32,
    numTables       :: Word16,
    searchRange     :: Word16,
    entrySelector   :: Word16,
    rangeShift      :: Word16
    } deriving (Generic, Show)
instance Serialize Offset

offset numTables =
    let largest2 n = if 2^n > numTables then n else largest2 $ n+1
        eSel = largest2 1
        sRange = 2^eSel
    in Offset 0x10000 numTables (sRange*16) eSel (numTables*16 - sRange*16)
    
data TableDef = TableDef {
    tag         :: AString,
    checkSum    :: Word32,
    off         :: Word32,
    len         :: Word32
    } deriving (Generic, Show)

instance Serialize TableDef where
    get = do
        tag <- getStr 4
        liftM3 (TableDef tag) get get get

getBS tc d = let r = runGet tc $ d
    in case r of
        Left m -> error m
        Right s -> s

tableChecksum table =
    let tc = do
        e <- remaining
        if e < 4
            then return 0
            else do w <- getWord32be
                    r <- tc
                    return $ w + r
    in getBS tc table

tableEntry tag table offset = TableDef tag (tableChecksum table) offset (fromIntegral $ BS.length table)

data Table = CmapTable Cmap
    | GlyfTable Glyf
    | HeadTable Head
    | HheaTable Hhea
    | HmtxTable Hmtx
    | LocaTable Loca
    | MaxpTable Maxp
    | NameTable Name
    | PostTable Post
    | UnknownTable String deriving (Generic, Show)
instance Serialize Table

data Cmap = Cmap {
    version :: Word16,
    nSub    :: Word16,
    encs    :: [CmapEnc],
    formats :: [CmapFormat]
    } deriving (Generic, Show)

instance Serialize Cmap where
    get = do
        vers <- get
        nSub <- get
        encs <- getN nSub
        fmts <- getN nSub 
        return $ Cmap vers nSub encs fmts

{-
cmap entries =
    let n = fromIntegral $ length entries
        c ((CEntry p ps t):cs) o = ((cmapEnc p ps o), t):(c cs (o+(fromIntegral $ slen t)))
        c [] _ = []
        cs = c entries (4+6*n)
        encs = sequence_ $ map fst cs
        tables = sequence_ $ map snd cs
    in _cmapTable 0 n encs tables
-}

data CmapEnc = CmapEnc {
    pID       :: Word16,
    pSpecID   :: Word16,
    encOff    :: Word16
    } deriving (Generic, Show)
instance Serialize CmapEnc

data CmapFormat = CmapFormat0 {
    cmapFormat  :: Word16,
    formatLen   :: Word16,
    formatLang  :: Word16,
    glyphIndices:: [Word8]
    } | CmapUnknown Word16 deriving (Generic, Show)

instance Serialize CmapFormat where
    get = do
        fmt  <- get
        flen <- get
        case fmt of
            0 -> liftM2 (CmapFormat0 0 flen) get (getN 4)
            _ -> return $ CmapUnknown fmt

{-
cmapFormat0 language glyphIndices = do
    let n = fromIntegral $ length glyphIndices
    cmapFormat 0 n language
    bytes glyphIndices
-}

data Glyf = Glyf GlyfDesc deriving (Generic, Show)
instance Serialize Glyf

data GlyfDesc = GlyfDesc {
    nContours   :: Word16,
    xMin        :: Int16,
    yMin        :: Int16,
    xMax        :: Int16,
    yMax        :: Int16,
    endPts      :: [Word16],
    iLength     :: Word16,
    instrs      :: [Word8],
    pflags      :: [Word8],
    xs          :: [Word8],
    ys          :: [Word8]
    } deriving (Generic, Show)

instance Serialize GlyfDesc where
    get = do
        n <- get
        xi <- get; yi <- get
        xa <- get; ya <- get
        end <- getN n
        ilen <- get
        liftM4 (GlyfDesc n xi yi xa ya end ilen) (getN ilen) (getN n) (getN n) (getN n)

{-
data GlyphPoint = GPoint Word16 Word8 Int Int -- TODO: full glyph support

simpleGlyph :: [Word16] -> (State Program ()) -> [Word8] -> [Word8] -> [Word8] -> (State Program ()) 
simpleGlyph endPts instrs flags xs ys =_simpleGlyph endPts (slen instrs) instrs flags xs ys

glyph xMin yMin xMax yMax endPts instrs flags xs ys =
    let g = do glyphDesc (fromIntegral $ length endPts) xMin yMin xMax yMax ; simpleGlyph endPts instrs flags xs ys
    in if ((length endPts) == (length xs)) && ((length endPts) == (length ys) && ((length endPts) == (length flags))) then g else error "Mismatched lengths"
-}

data Head = Head {
    headVersion     :: Fixed,
    revision        :: Fixed,
    csAdjust        :: Word32,
    magic           :: Word32,
    flags           :: Word16,
    unitsPerEm      :: Word16,
    created         :: DateTime,
    modified        :: DateTime,
    allXMin         :: FWord,
    allYMin         :: FWord,
    allXMax         :: FWord,
    allYMax         :: FWord,
    macStyle        :: Word16,
    lowestRec       :: Word16,
    direction       :: Int16,
    iToLoc          :: Int16,
    format          :: Int16
    } deriving (Generic, Show)
instance Serialize Head
{-
headTable = _head 0x00010000 0 0x5f0f3cf5
-}

data Hhea = Hhea {
    hheaVersion     :: Fixed,
    ascent          :: Int16,
    descent         :: Int16,
    lineGap         :: Int16,
    aWidthMax       :: Word16,
    minLeft         :: Int16,
    minRight        :: Int16,
    xMaxExtent      :: Int16,
    cSlopeRise      :: Int16,
    cSlopeRun       :: Int16,
    cOffset         :: Int16,
    r0 :: Int16, r1 :: Int16, r2 :: Int16, r3 :: Int16,
    mDataFormat     :: Int16,
    nMetrics        :: Word16
    } deriving (Generic, Show)
instance Serialize Hhea
{-
hhea ascent descent lineGap aWidthMax minLeft minRight cSlopeRise cSlopeRun cOffset nMetrics =
    _hhea 0x00010000 ascent descent lineGap aWidthMax minLeft minRight 0 cSlopeRise cSlopeRun cOffset 0 0 0 0 0 nMetrics

-- TODO short version
_loca entries = do mapM_ uint entries

-- TODO ordering
loca m = _loca $ map (\(k,v) -> v) (M.toList m)
-}

data Hmtx = Hmtx HmtxEntry Int16 deriving (Generic, Show)
instance Serialize Hmtx

data HmtxEntry = HmtxEntry {
    advanceWidth    :: Word16,
    leftSideBearing :: Int16
    } deriving (Generic, Show)
instance Serialize HmtxEntry

data Loca = Loca Word8 deriving (Generic, Show)
instance Serialize Loca

data Maxp = Maxp {
    maxpVersion             :: Fixed,
    numGlyphs               :: Word16,
    maxPoints               :: Word16,
    maxContours             :: Word16,
    maxComponentPoints      :: Word16,
    maxComponentContours    :: Word16,
    maxZones                :: Word16,
    maxTwilightPoints       :: Word16,
    maxStorage              :: Word16,
    maxFunctionDefs         :: Word16,
    maxInstructionDefs      :: Word16,
    maxStackElements        :: Word16,
    maxSizeOfInstructions   :: Word16,
    maxComponentElements    :: Word16,
    maxComponentDepth       :: Word16
    } deriving (Generic, Show)
instance Serialize Maxp
{-
maxp numGlyphs maxPoints maxContours maxComponentPoints maxComponentContours = _maxp 0x00010000 numGlyphs maxPoints maxContours maxComponentPoints maxComponentContours
-}

data Name = Name {
    nameFormat  :: Word16,
    count       :: Word16,
    strOffset   :: Word16,
    nameRecords :: [NameRecord]
--    names       :: [AString]
} deriving (Generic, Show)
instance Serialize Name where
    get = do
        nf <- get; c <- get
        liftM2 (Name nf c) get (getN c)

{-
name nameRecords names count = _name 0 count (count*12+6) nameRecords names

data NameRecord = NRecord Word16 Word16 Word16 Word16 String

data MSNameRecord = MSNRecord NameID String
data NameID = Copyright | Family | Subfamily | UUID | Fullname | Version | Postscript | Trademark deriving Enum
-}
data NameRecord = NameRecord {
    namepID     :: Word16,
    namepsID    :: Word16,
    namelID     :: Word16,
    namenID     :: Word16,
    nameLength  :: Word16,
    nameOffset  :: Word16
    } deriving (Generic, Show)
instance Serialize NameRecord

{-
string16 s = '\00' : (intersperse '\00' s)

nameHeaderMS records = nameHeader $ map (\(MSNRecord nid str) -> NRecord 3 1 0x409 (fromIntegral $ fromEnum nid) (string16 str)) records 

nameHeader records =
    let nh ((NRecord pid psid lid nid str):rs) c = (pid, psid, lid, fromIntegral $ fromEnum nid, fromIntegral (length str), c):(nh rs (c + fromIntegral(length str)))
        nh [] c = []
        recs    = nh records 0
        names   = map (\(NRecord _ _ _ _ s) -> s) records
    in name (do mapM_ (\(pid, psid, lid, nid, len, off) -> nameRecord pid psid lid nid len off) recs) (do mapM_ string names) (fromIntegral $ length records)
-}

data Post = Post {
    postFormat  :: Fixed,
    iAngle      :: Fixed,
    uPos        :: Int16,
    uThick      :: Int16,
    fixed       :: Word32,
    minMemT42   :: Word32,
    maxMemT42   :: Word32,
    minMemT1    :: Word32,
    maxMemT1    :: Word32,
    postf       :: PostFormat
    } deriving (Generic, Show)
instance Serialize Post

data PostFormat = PostFormat2 {
    pnGlyph     :: Word16,
    glyphNameInd:: [Word16],
    pNames      :: [PString]
    } deriving (Generic, Show)

instance Serialize PostFormat where
    get = do
        n <- get
        i <- getN n
        s <- get
        return $ PostFormat2 n i [s]
{-
post format iAngle uPos uThick fixed = _post format iAngle uPos uThick (if fixed then 1 else 0) 0 0 0 0

post3 iAngle uPos uThick fixed = post 0x00030000 iAngle uPos uThick fixed
-}
