{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleInstances, OverlappingInstances #-}
module Template where

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import GHC.Generics
import qualified Data.ByteString as BS
import Data.Bits
import GHC.Int
import GHC.Word
import Control.Monad
import Control.Monad.Loops
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace

type Fixed = Word32
type FWord = Int16
type DateTime = Word64
data PArray w a = PArray w [a] deriving Show

puts x = mapM_ put x
getN n = replicateM (fromIntegral n) get

instance (Serialize w, Serialize as, Integral w) => Serialize (PArray w as) where
    get = do
        n  <- get
        as <- getN n
        return (PArray n as)

    put (PArray n s) = do
        put n
        puts s

type PString = PArray Word8 Char

newtype BArray a = BArray [a] deriving Show

instance (Serialize as) => Serialize (BArray as) where
    get = do error "Infinite array get!"
    put (BArray cs) = puts cs


type BString = BArray Char
 
getb n = do
    a <- getN n
    return (BArray a)

instance Serialize (BArray PString) where
    get = do
        bs <- whileM (do
            r <- remaining
            if r > 0 then liftM ((<= r) . fromIntegral) (lookAhead get :: Get Word8)
                else return False
            ) get
        return $ BArray bs
    put (BArray ps) = do mapM_ put ps

data TTF = TTF {
    ttfOffset :: Offset,
    tableDefs :: BArray TableDef,
    tables    :: BArray Table
    } deriving (Generic, Show)

instance Serialize TTF where
    get = do
        o@(Offset s n r e sh) <- get
        tdefs <- replicateM (fromIntegral n) get
        let sdefs = sortBy (\(TableDef {off=l}) (TableDef {off=r}) -> l `compare` r) tdefs
        let mdefs = M.fromList $ map (\t@(TableDef {tag=(BArray tag)}) -> (tag, t)) sdefs
        let start = 12 + 16*(fromIntegral n)
        head <- lookAhead (tableFromTag start (mdefs M.! "head") (M.fromList []))
        hhea <- lookAhead (tableFromTag start (mdefs M.! "hhea") (M.fromList []))
        maxp <- lookAhead (tableFromTag start (mdefs M.! "maxp") (M.fromList []))
        let neededTabs = M.fromList [("head",head),("hhea",hhea),("maxp",maxp)]
        tabs <- mapM (\e -> lookAhead (tableFromTag start e neededTabs)) sdefs 
        return $ TTF o (BArray tdefs) (BArray tabs)
    put (TTF o d (BArray ts)) = do
        put o
        put d
        mapM_ putByteString $ map padbs $ map encode ts

tableFromTag :: Word32 -> TableDef -> M.Map String Table -> Get Table 
tableFromTag start (TableDef (BArray tag) _ off len) tabs = do
    skip $ fromIntegral (off-start)
    bs <- getBytes $ fromIntegral len
    case (runGet (fromTag tag bs tabs) bs) of
        (Left m)  -> error m
        (Right t) -> return t

fromTag :: String -> BS.ByteString -> M.Map String Table -> Get Table
fromTag tag bs tabs = do
    case tag of
        "cmap" -> liftM CmapTable get
        "fpgm" -> liftM FpgmTable get
        --"glyf" -> liftM GlyfTable (getGlyf (tabs M.! "maxp") (tabs M.! "head"))
        "head" -> liftM HeadTable get
        "hhea" -> liftM HheaTable get
        "hmtx" -> liftM HmtxTable (getHmtx (tabs M.! "maxp") (tabs M.! "hhea"))
        "loca" -> liftM LocaTable (getLoca (tabs M.! "maxp") (tabs M.! "head"))
        "maxp" -> liftM MaxpTable get
        "name" -> liftM NameTable get
        "post" -> liftM PostTable get
        _      -> return $ UnknownTable bs

tagFromTable t =
    case t of
        (CmapTable _) -> BArray "cmap"
        (FpgmTable _) -> BArray "fpgm"
        (GlyfTable _) -> BArray "glyf"
        (HeadTable _) -> BArray "head"
        (HheaTable _) -> BArray "hhea"
        (HmtxTable _) -> BArray "hmtx"
        (LocaTable _) -> BArray "loca"
        (MaxpTable _) -> BArray "maxp"
        (NameTable _) -> BArray "name"
        (PostTable _) -> BArray "post"
        (UnknownTable _) -> BArray "????" --tag

padbs bs = 
    let bsl = fromIntegral $ BS.length bs
        pad = 3 - ((bsl - 1)`mod` 4)
    in bs `BS.append` (BS.pack (take pad $ repeat 0))

accum a (tag, bs) = let
    padd  = padbs bs
    anext = a + (fromIntegral $ BS.length padd)
    dnext = tableEntry tag padd (fromIntegral a)
    in (anext, dnext)

ttfTables tables = do
    let n = fromIntegral $ length tables
    let tabs = map (\t -> (tagFromTable t,encode t)) tables
    let (_, tdefs) = mapAccumL accum (12 + 16*(fromIntegral n)) tabs
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
    tag         :: BString,
    checkSum    :: Word32,
    off         :: Word32,
    len         :: Word32
    } deriving (Generic, Show)

instance Serialize TableDef where
    get = do
        tag <- getb 4
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
    | FpgmTable Fpgm
    | HeadTable Head
    | HheaTable Hhea
    | HmtxTable Hmtx
    | LocaTable Loca
    | MaxpTable Maxp
    | NameTable Name
    | PostTable Post
    | UnknownTable BS.ByteString deriving (Generic, Show)
instance Serialize Table where
    put (CmapTable t) = put t
    put (FpgmTable t) = put t
    put (GlyfTable t) = put t
    put (HeadTable t) = put t
    put (HheaTable t) = put t
    put (LocaTable t) = put t
    put (HmtxTable t) = put t
    put (MaxpTable t) = put t
    put (NameTable t) = put t
    put (PostTable t) = put t
    put (UnknownTable bs) = putByteString bs

data Cmap = Cmap {
    version :: Word16,
    encs    :: PArray Word16 CmapEnc,
    formats :: BArray CmapFormat
    } deriving (Generic, Show)

instance Serialize Cmap where
    get = do
        vers <- get
        encs@(PArray _ es) <- get
        let fmts = length $ nub $ map encOff es
        liftM (Cmap vers encs) (getb fmts)

data CmapEnc = CmapEnc {
    pID       :: Word16,
    pSpecID   :: Word16,
    encOff    :: Word32
    } deriving (Generic, Show)
instance Serialize CmapEnc

data CmapFormat = CmapFormat0 {
    cmapFormat  :: Word16,
    formatLen   :: Word16,
    formatLang  :: Word16,
    glyphIndices:: BArray Word8
    } | CmapUnknown16 Word16 Word16 BS.ByteString | CmapUnknown32 Word32 Word32 BS.ByteString deriving (Generic, Show)

instance Serialize CmapFormat where
    get = do
        fmt <- get
        if fmt == 0 then do
            flen <- get
            liftM2 (CmapFormat0 0 flen) get (getb (flen-6))
        else if fmt == 2 || fmt == 4 || fmt == 6 then do
            flen <- get
            liftM (CmapUnknown16 fmt flen) (getBytes $ (fromIntegral flen)-4)
        else do
            fmt2 <- get :: Get Word16
            flen <- get
            let fmt32 = ((fromIntegral fmt) `shift` 16) .|. (fromIntegral fmt2)
            liftM (CmapUnknown32 fmt32 flen) (getBytes $ (fromIntegral flen)-8)
            
    put (CmapFormat0 f l a g) = do
        puts [f, l, a]
        put g
    put (CmapUnknown16 f l bs) = do
        put f; put l
        putByteString bs
    put (CmapUnknown32 f l bs) = do
        put f; put l
        putByteString bs


blength (BArray bs) = fromIntegral $ length bs

cmapFormat0 language glyphIndices =
    let n = (blength glyphIndices)+6
    in CmapFormat0 0 n language glyphIndices

data Fpgm = Fpgm (BArray Word8) deriving (Generic, Show)
instance Serialize Fpgm where
    get = do
        r <- remaining
        bs <- getByteString r
        return $ Fpgm $ BArray (BS.unpack bs)

data Glyf = Glyf (BArray GlyphDesc) deriving (Generic, Show)
instance Serialize Glyf

getAligned long = do
    pre <- remaining
    x <- get
    post <- remaining
    let diff = pre - post
    let npad = if long == 1
        then 3 - ((diff - 1) `mod` 4)
        else (if diff `mod` 2 == 1 then 1 else 0)
    got <- getN npad :: Get [Word8]
    return x

getGlyf (MaxpTable m) (HeadTable h) = do
    xs <- replicateM (fromIntegral $ numGlyphs m) $ getAligned $ iToLoc h
    return $ Glyf $ BArray xs

data Point = PointB Word8 | PointS Int16 deriving (Generic, Show)

instance Serialize Point where
    put (PointB p) = put p
    put (PointS p) = put p

data GlyphDesc = GlyphDesc {
    nContours   :: Int16,
    xMin        :: FWord,
    yMin        :: FWord,
    xMax        :: FWord,
    yMax        :: FWord,
    glyph       :: Glyph
} deriving (Generic, Show)

data Glyph = SimpleGlyph {
    endPts      :: BArray Word16,
    instrs      :: PArray Word16 Word8,
    pflags      :: BArray Word8,
    xs          :: BArray Point,
    ys          :: BArray Point
    } | CompoundGlyph {
    cflags      :: Word16,
    glyphIndex  :: Word16,
    arg1        :: Point,
    arg2        :: Point,
    trans       :: Point
    } deriving (Generic, Show)

instance Serialize Glyph where
    get = error "Serializing lone glyph"
    put (SimpleGlyph e i p x y) = do
        put e; put i; put p
        put x; put y
    put (CompoundGlyph c g a1 a2 t) = do
        put c; put g
        put a1; put a2
        put t

getpflags n
    | n > 0 = do
        flag <- get 
        if flag .&. 8 == 8
            then do
                rep <- get :: Get Word8
                let flags = flag:(take (fromIntegral rep) $ repeat flag)
                rest <- getpflags (n - 1 - (fromIntegral rep))
                return (flags ++ rest)
            else do
                rest <- getpflags (n - 1)
                return (flag:rest)
    | otherwise = return []

getps i j (p:ps) prev = do
    if p .&. i /= 0
        then do
            num <- get
            let new = PointB (if p .&. j == 0 then num else -num)
            rest <- getps i j ps new
            return (new:rest)
        else if p .&. j == 0
            then do
                new  <- liftM PointS get
                rest <- getps i j ps new 
                return (new:rest)
            else do
                rest <- getps i j ps prev 
                return (prev:rest)

getps _ _ [] _ = return []
 
instance Serialize GlyphDesc where
    get = do
        nc <- get
        x0 <- get; y0 <- get
        xm <- get; ym <- get
        let gp = GlyphDesc nc x0 y0 xm ym
        if nc < 0
            then error "Compound glyph!"
            else liftM gp (simpleGlyph nc)

simpleGlyph nc = do
    end <- getN nc
    instrs <- get
    let n = foldr max 0 $ map (+ 1) end
    pflag <- getpflags n
    xs <- getps 2 16 pflag (PointS 0)
    ys <- getps 4 32 pflag (PointS 0)
    return $ SimpleGlyph (BArray end) instrs (BArray pflag) (BArray xs) (BArray ys) 

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

data Hmtx = Hmtx (BArray HmtxEntry) (BArray FWord) deriving (Generic, Show)
instance Serialize Hmtx

getHmtx (MaxpTable m) (HheaTable h) = do
    let nhor = nMetrics h
    let nglf = numGlyphs m
    liftM2 Hmtx (getb nhor) (getb (nglf - nhor))

data HmtxEntry = HmtxEntry {
    advanceWidth    :: Word16,
    leftSideBearing :: Int16
    } deriving (Generic, Show)
instance Serialize HmtxEntry

data Loca = LocaShort (BArray Word16) | LocaLong (BArray Word32) deriving (Generic, Show)
instance Serialize Loca where
    put (LocaShort ws) = put ws
    put (LocaLong ws)  = put ws

getLoca (MaxpTable m) (HeadTable h) =
    let fmt = iToLoc h
        n = (numGlyphs m)+1
    in case fmt of
        0 -> liftM LocaShort (getb n)
        1 -> liftM LocaLong (getb n)

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
    nameRecords :: BArray NameRecord,
    names       :: BS.ByteString
} deriving (Generic, Show)

instance Serialize Name where
    get = do
        nf <- get; c <- get; o <- get; nr <- getb c
        r  <- remaining
        ns <- getBytes r
        return $ Name nf c o nr ns
    put (Name f c s r n) = do
        puts [f,c,s]
        put r
        putByteString n

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
    glyphNameInd    :: PArray Word16 Word16,
    pNames          :: BArray PString
    } deriving (Generic, Show)


instance Serialize PostFormat

{-
post format iAngle uPos uThick fixed = _post format iAngle uPos uThick (if fixed then 1 else 0) 0 0 0 0

post3 iAngle uPos uThick fixed = post 0x00030000 iAngle uPos uThick fixed
-}
