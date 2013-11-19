{-# LANGUAGE DeriveGeneric #-}
module Template where

import Data.Serialize
import Data.Serialize.Put
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import GHC.Int
import GHC.Word
import Control.Monad

data TTF = TTF {
    ttfOffset :: Offset,
    tableDefs :: [TableDef],
    tables    :: [Table]
    } deriving (Show)

instance Serialize TTF where
    get = do
        o@(Offset s n r e sh) <- get
        tdefs <- replicateM (fromIntegral $ n) get
        let start = 12 + 16*(fromIntegral $ n)
        tabs <- mapM (\e -> lookAhead (tableFromTag start e)) tdefs
        return $ TTF o tdefs tabs
    put (TTF o d t) = do
        put o; put d; put t

tableFromTag start (TableDef tag _ off len) = do
    skip $ fromIntegral (off-start)
    case tag of
        "cmap" -> liftM CmapTable get
        "head" -> liftM HeadTable get
        "hhea" -> liftM HheaTable get
        "maxp" -> liftM MaxpTable get
        "post" -> liftM PostTable get
        _      -> return $ UnknownTable tag

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
    tag         :: [Char],
    checkSum    :: Word32,
    off         :: Word32,
    length      :: Word32
    } deriving (Generic, Show)

instance Serialize TableDef where
    get = do
        tag <- replicateM 4 get
        liftM3 (TableDef tag) get get get
    put (TableDef t c o l) = do
        put t; put c; put o; put l

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

data Table = CmapTable Cmap | HeadTable Head | HheaTable Hhea | MaxpTable Maxp | PostTable Post | UnknownTable String deriving (Generic, Show)
instance Serialize Table

data Cmap = Cmap {
    version :: Word16,
    nSub    :: Word16,
    encs    :: [CmapEnc]
    } deriving Show

instance Serialize Cmap where
    get = do
        vers <- get
        nSub <- get
        encs <- replicateM (fromIntegral nSub) get
        return $ Cmap vers nSub encs
    put (Cmap v n e) = do
        put v; put n; put e

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
    --encFormat :: CmapFormat
    } deriving (Generic, Show)

instance Serialize CmapEnc where
    get = do
        pid <- get
        psp <- get
        off <- get
        return $ CmapEnc pid psp off
    put (CmapEnc p s e) = do
        put p; put s; put e

data CmapFormat = CmapFormat0 {
    cmapFormat  :: Word16,
    formatLen   :: Word16,
    formatLang  :: Word16,
    glyphIndices:: [Word8]
    } deriving (Generic, Show)
instance Serialize CmapFormat

{-
cmapFormat0 language glyphIndices = do
    let n = fromIntegral $ length glyphIndices
    cmapFormat 0 n language
    bytes glyphIndices
-}


data GlyphDesc = GlyphDesc {
    nContours   :: Word16,
    xMin        :: Int16,
    yMin        :: Int16,
    xMax        :: Int16,
    yMax        :: Int16
    } deriving (Generic, Show)
{-
_simpleGlyph endPts iLength instrs flags xs ys = do
    mapM_ ushort endPts
    ushort iLength
    instrs
    bytes flags
    bytes xs
    bytes ys

data GlyphPoint = GPoint Word16 Word8 Int Int -- TODO: full glyph support

simpleGlyph :: [Word16] -> (State Program ()) -> [Word8] -> [Word8] -> [Word8] -> (State Program ()) 
simpleGlyph endPts instrs flags xs ys =_simpleGlyph endPts (slen instrs) instrs flags xs ys

glyph xMin yMin xMax yMax endPts instrs flags xs ys =
    let g = do glyphDesc (fromIntegral $ length endPts) xMin yMin xMax yMax ; simpleGlyph endPts instrs flags xs ys
    in if ((length endPts) == (length xs)) && ((length endPts) == (length ys) && ((length endPts) == (length flags))) then g else error "Mismatched lengths"
-}

type Fixed = Word32
type FWord = Int16
-- TODO
data DateTime = DateTime Word32 Word32 deriving (Generic, Show)
instance Serialize DateTime

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

hmtxEntry advanceWidth leftSideBearing = do
    ushort advanceWidth
    short leftSideBearing

hmtx entries = do
    entries
--    sequence bearings

-- TODO short version
_loca entries = do mapM_ uint entries

-- TODO ordering
loca m = _loca $ map (\(k,v) -> v) (M.toList m)
-}

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
    nameRecords :: [NameRecord],
    names       :: [NameS]
} deriving (Generic, Show)

type NameS = String
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
    maxMemT1    :: Word32
    } deriving (Generic, Show)
instance Serialize Post
{-
post format iAngle uPos uThick fixed = _post format iAngle uPos uThick (if fixed then 1 else 0) 0 0 0 0

post3 iAngle uPos uThick fixed = post 0x00030000 iAngle uPos uThick fixed
-}
