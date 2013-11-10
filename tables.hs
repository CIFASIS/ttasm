import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as CB
import Data.Binary.Get
import System.Environment
import Control.Monad

tables = do
    skip 4
    n <- getWord16be
    skip 6
    replicateM (fromIntegral $ n) tableEntry

tableEntry = do
    _tag <- getBytes 4
    let tag = CB.unpack _tag
    skip 4 -- checksum
    offset <- getWord32be
    len    <- getWord32be
    return (tag, offset, len)

tableData (tag, offset, len) = do
    skip $ (fromIntegral $ offset)
    cont <- getByteString $ fromIntegral len
    return (tag, cont)


main = do
    (fileName:name:_) <- getArgs
    f <- LB.readFile $ fileName
    let entries = runGet tables f
    if (name == "tables")
    then putStrLn $ show $ map (\(t, _, _) -> t) entries
    else B.putStr $ head $ map (\e -> snd $ runGet (tableData e) f) $ filter (\(t, _, _) -> t == name) entries
