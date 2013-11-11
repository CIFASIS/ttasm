module Instructions where
 
 
import qualified Data.Map as M
import Control.Monad.State
import Data.Bits
import qualified Data.Char as C
import qualified Data.Bits.Bitwise as B
import qualified Data.ByteString.Lazy as BS
import GHC.Word
import qualified GHC.Int as I
import Data.Binary.Put
import Data.Binary.Get
import Unsafe.Coerce
import Data.List
 
data Op = Byte Word8 | Char I.Int8 | UShort Word16 | Short I.Int16 | UInt Word32 | Int I.Int32 | Str String | Ref String | Label String
    deriving (Show)

data Program = Program [Op]
    deriving (Show)

emptyProg = Program []

data Table = Table String (State Program ())

stage1 prog = let (_, p) = runState prog emptyProg in p

stage2 :: [Op] -> (M.Map String Word32) -> Word32 -> (M.Map String Word32)
stage2 ((Byte b):bs) m d   = stage2 bs m (d+1)
stage2 ((Char b):bs) m d   = stage2 bs m (d+1)
stage2 ((UShort b):bs) m d = stage2 bs m (d+2)
stage2 ((Short b):bs) m d  = stage2 bs m (d+2)
stage2 ((UInt b):bs) m d   = stage2 bs m (d+4)
stage2 ((Int b):bs) m d    = stage2 bs m (d+4)
stage2 ((Label b):bs) m d  = stage2 bs (M.insert b d m) d
stage2 ((Ref b):bs) m d    = stage2 bs m (d+4)
stage2 ((Str b):bs) m d    = stage2 bs m (d+(fromIntegral $ length b))
stage2 [] m d = m

stage3 :: [Op] -> (M.Map String Word32) -> [Op]
stage3 ((Ref b):bs) m = (UInt (m M.! b)):(stage3 bs m)
stage3 ((Label b):bs) m = stage3 bs m
stage3 (b:bs) m = b:(stage3 bs m)
stage3 [] m = []

stage4 ((Byte b):bs)   = do putWord8 b ; stage4 bs
stage4 ((Char b):bs)   = do putWord8 $ unsafeCoerce b ; stage4 bs
stage4 ((UShort b):bs) = do putWord16be b ; stage4 bs
stage4 ((Short b):bs)  = do putWord16be $ unsafeCoerce b ; stage4 bs
stage4 ((UInt b):bs)   = do putWord32be b ; stage4 bs
stage4 ((Int b):bs)    = do putWord32be $ unsafeCoerce b ; stage4 bs
stage4 ((Str b):bs)    = do mapM (\c -> putWord8 $ fromIntegral $ C.ord c) b ; stage4 bs
stage4 [] = do flush

compile prog = 
    let (Program bs) = stage1 prog
        m = stage2 bs (M.fromList []) 0
        reduced = stage3 bs m
        final   = runPut $ stage4 reduced
    in (final, m)

compileTables tables head =
    let check1 = foldl (\a (Table _ code) -> a + (tableChecksum $ fst $ compile code)) 0 ((Table "head" (head 0)):tables)
        check2 = tableChecksum $ fst $ compile $ compileFinal $ (Table "head" (head check1)):tables
    in  compileFinal $ (Table "head" (head $ 0xb1b0afba - check2)):tables

compileFinal tables =
    let tcomp = sortBy (\(a,_,_) (b,_,_) -> a `compare` b) $ map (\(Table name code) -> (name, code, (compile code))) tables
        compose ((name, _, (code, _)):ts) offs = do
            tableEntry name code offs
            let l = fromIntegral $ BS.length code
            compose ts (offs + l + (4 - l `mod` 4))
        compose [] offs = do return ()
        numTables = length tables
        pad (_, code, (compiled, _)) = do
            code
            bytes $ take (4 - (fromIntegral $ BS.length compiled) `mod` 4) (repeat 0)
    in do
        offset $ fromIntegral $ numTables
        compose tcomp ((fromIntegral numTables)*16+12)
        sequence $ map pad tcomp
    
ops :: [Op] -> State Program ()
ops x = do
    (Program a) <- get
    put $ Program (a ++ x)

op x = ops [x]

byte x = op $ Byte x
bytes x = ops $ map (\e -> Byte e) x 

int i    = op $ Int i
uint i   = op $ UInt i
short i  = op $ Short i
ushort i = op $ UShort i
char i   = op $ Char i

ref name = op $ Ref name

string s = op $ Str s

label name = op $ Label name

slen p = let (_, (Program e)) = runState p emptyProg in fromIntegral $ length e

_offset scalarType numTables searchRange entrySelector rangeShift = do
    uint scalarType
    ushort numTables
    ushort searchRange
    ushort entrySelector
    ushort rangeShift

offset numTables = let largest2 n = if 2^n > numTables then n else largest2 $ n+1
                       eSel = largest2 1
                       sRange = 2^eSel
                   in _offset 0x10000 numTables (sRange*16) eSel (numTables*16 - sRange*16)

_tableDef tag checkSum offset len
    | (length tag == 4) = do
    string tag
    uint checkSum
    uint offset
    uint len

tableChecksum table =
    let tc = do
        e <- remaining
        if e < 4
            then return 0
            else do w <- getWord32be
                    r <- tc
                    return $ w + r
    in runGet tc $ table

tableEntry tag table offset = _tableDef tag (tableChecksum table) offset (fromIntegral $ BS.length table)

_cmapTable version nSub tables = do
    ushort version
    ushort nSub
    tables

cmapTable tables = do
    let n = slen tables
    _cmapTable 0 n tables

cmapEnc pID pSpecID offset = do
    ushort pID
    ushort pSpecID
    ushort offset

cmapFormat format length language = do
    ushort format
    ushort length
    ushort language

cmapFormat0 language glyphIndices
    | (length glyphIndices) == 262 = do
    cmapFormat 0 262 language
    bytes glyphIndices

glyphDesc nContours xMin yMin xMax yMax = do
    ushort nContours
    short xMin
    short yMin
    short xMax
    short yMax

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

_head version revision magic flags unitsPerEm created modified xMin yMin xMax yMax macStyle lowestRec direction iToLoc format csAdjust = do
    uint version  -- Fixed
    uint revision -- Fixed
    uint csAdjust
    uint magic
    ushort flags
    ushort unitsPerEm
    uint 0 ; uint 0 -- longDateTime
    uint 0 ; uint 0 -- longDateTime
    short xMin
    short yMin
    short xMax
    short yMax
    ushort macStyle
    ushort lowestRec
    short direction
    short iToLoc
    short format

headTable = _head 0x00010000 0 0x5f0f3cf5

_hhea version ascent descent lineGap aWidthMax minLeft minRight xMaxExtent cSlopeRise cSlopeRun cOffset r0 r1 r2 r3 mDataFormat nMetrics = do
    uint version -- Fixed
    short ascent
    short descent
    short lineGap
    ushort aWidthMax
    short minLeft
    short minRight
    short xMaxExtent
    short cSlopeRise
    short cSlopeRun
    short cOffset
    short r0 ; short r1 ; short r2 ; short r3
    short mDataFormat
    ushort nMetrics


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

_maxp version numGlyphs maxPoints maxContours maxComponentPoints maxComponentContours maxZones maxTwilightPoints maxStorage maxFunctionDefs maxInstructionDefs maxStackElements maxSizeOfInstructions maxComponentElements maxComponentDepth = do
    uint version -- fixed
    ushort numGlyphs
    ushort maxPoints
    ushort maxContours
    ushort maxComponentPoints
    ushort maxComponentContours
    ushort maxZones
    ushort maxTwilightPoints
    ushort maxStorage
    ushort maxFunctionDefs
    ushort maxInstructionDefs
    ushort maxStackElements
    ushort maxSizeOfInstructions
    ushort maxComponentElements
    ushort maxComponentDepth

maxp numGlyphs maxPoints maxContours maxComponentPoints maxComponentContours = _maxp 0x00010000 numGlyphs maxPoints maxContours maxComponentPoints maxComponentContours

_name format count strOffset nameRecords names = do
    ushort format
    ushort count
    ushort strOffset
    nameRecords
    names

name nameRecords names count = _name 0 count (count*12+6) nameRecords names

data NameRecord = NRecord Word16 Word16 Word16 Word16 String

data MSNameRecord = MSNRecord NameID String
data NameID = Copyright | Family | Subfamily | UUID | Fullname | Version | Postscript | Trademark deriving Enum

nameRecord pID psID lID nID length offset = do
    ushort pID
    ushort psID
    ushort lID
    ushort nID
    ushort length
    ushort offset

string16 s = '\00' : (intersperse '\00' s)

nameHeaderMS records = nameHeader $ map (\(MSNRecord nid str) -> NRecord 3 1 0x409 (fromIntegral $ fromEnum nid) (string16 str)) records 

nameHeader records =
    let nh ((NRecord pid psid lid nid str):rs) c = (pid, psid, lid, fromIntegral $ fromEnum nid, fromIntegral (length str), c):(nh rs (c + fromIntegral(length str)))
        nh [] c = []
        recs    = nh records 0
        names   = map (\(NRecord _ _ _ _ s) -> s) records
    in name (do mapM_ (\(pid, psid, lid, nid, len, off) -> nameRecord pid psid lid nid len off) recs) (do mapM_ string names) (fromIntegral $ length records)

_post format iAngle uPos uThick fixed minMemT42 maxMemT42 minMemT1 maxMemT1 = do
    uint format -- fixed
    uint iAngle -- fixed
    short uPos
    short uThick
    uint fixed
    uint minMemT42
    uint maxMemT42
    uint minMemT1
    uint maxMemT1

post format iAngle uPos uThick fixed = _post format iAngle uPos uThick (if fixed then 1 else 0) 0 0 0 0

post3 iAngle uPos uThick fixed = post 0x00030000 iAngle uPos uThick fixed

aa       = byte 0x7f
abs      = byte 0x64
add      = byte 0x60
alignpts = byte 0x27
alignrp  = byte 0x3c
and      = byte 0x5a
call     = byte 0x2b
ceiling  = byte 0x67
cindex   = byte 0x25
clear    = byte 0x22
debug    = byte 0x5f
deltac1  = byte 0x73
deltac2  = byte 0x74
deltac3  = byte 0x75
deltap1  = byte 0x5d
deltap2  = byte 0x71
deltap3  = byte 0x72
depth    = byte 0x24
div      = byte 0x67
dup      = byte 0x20
eif      = byte 0x59
elset    = byte 0x1b
endf     = byte 0x2d
eq       = byte 0x54
even     = byte 0x57
fdef     = byte 0x2c
flipoff  = byte 0x4e
flipon   = byte 0x4d
flippt   = byte 0x80
fliprgoff= byte 0x82
fliprgon = byte 0x81
floor    = byte 0x66
gc orig  = byte $ if orig then 0x47 else 0x46
getinfo  = byte 0x88
gvf      = byte 0x0d
gpf      = byte 0x0c
gt       = byte 0x52
gteq     = byte 0x53
idef     = byte 0x89
ift      = byte 0x58
instctrl = byte 0x8e
ip       = byte 0x39
isect    = byte 0x0f
iup x    = byte $ if x then  0x31 else 0x30
jmpr     = byte 0x1c
jrof     = byte 0x79
jrot     = byte 0x78
lobytecall = byte 0x2a
lt       = byte 0x50
lteq     = byte 0x51
max      = byte 0x8b
md orig  = byte $ if orig then 0x4a else 0x49
mdap round = byte $ if round then 0x2f else 0x2e
mdrp reset min round dtype = byte $ (B.fromListBE [reset, min, round, dtype]) .|. 0xc0
miap round = byte $ if round then 0x3f else 0x3e
min      = byte 0x8c
mindex   = byte 0x26
mirp reset min round dtype = byte $ (B.fromListBE [reset, min, round, dtype]) .|. 0xe0
mppem    = byte 0x4b
mps      = byte 0x4c
msirp reset  = byte $ if reset then 0x3b else 0x3a
nround dtype = byte $ dtype .|. 0x6f
pushb n  
    | n < 8 = byte $ n .|. 0xb0
pushbs l
    | (length l) < 9 = bytes $ (((fromIntegral $ length l) - 1) .|. 0xb0):l
pushi l = do
    let f = sequence l
    pushb (slen f)
    f
pushw n  
    | n < 8 = byte $ n .|. 0xb8
pushws l
    | (length l) < 9 = do
	pushw $ fromIntegral $ (length l) - 1
	mapM_ short l

odd      = byte 0x56
or       = byte 0x5b
pbyte    = byte 0x21
rcvt     = byte 0x45
rdtg     = byte 0x7d
roff     = byte 0x7a
roll     = byte 0x8a
round dtype = byte $ dtype .|. 0x68
rs       = byte 0x43
rtdg     = byte 0x3d
rtg      = byte 0x18
rthg     = byte 0x19
rutg     = byte 0x7c
s45round = byte 0x77
sangw    = byte 0x7e
scanctrl = byte 0x85
scantype = byte 0x8d
scfs     = byte 0x48
scvtci   = byte 0x1d
sdb      = byte 0x5e
sdpvtl perp = byte $ if perp then 0x87 else 0x86
sds      = byte 0x5f
sfvfs    = byte 0x0b
sfvtca x = byte $ if x then 0x05 else 0x04
sfvtl perp = byte $ if perp then 0x09 else 0x08
sfvtpv   = byte 0x0e
shc rp   = byte $ if rp then 0x35 else 0x34
shp rp   = byte $ if rp then 0x33 else 0x32
shpix    = byte 0x38
shz rp   = byte $ if rp then 0x37 else 0x36
slobyte  = byte 0x17
smd      = byte 0x1a
spvfs    = byte 0x0a
spvtca x = byte $ if x then 0x03 else 0x02
spvtl perp = byte $ if perp then 0x07 else 0x06
sround   = byte 0x76
srp0     = byte 0x10
srp1     = byte 0x11
srp2     = byte 0x12
ssw      = byte 0x1f
sswci    = byte 0x1e
sub      = byte 0x61
svtca x  = byte $ if x then 0x01 else 0x00
swap     = byte 0x23
szp0     = byte 0x13
szp1     = byte 0x14
szp2     = byte 0x15
szps     = byte 0x16
utp      = byte 0x29
wcvtf    = byte 0x70
wcvtp    = byte 0x44
ws       = byte 0x42
