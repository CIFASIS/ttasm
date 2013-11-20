import Template

import Data.Serialize
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as B
import System.Environment
import Text.Show.Pretty

main = do
    (fileName:_) <- getArgs
    s <- BS.readFile $ fileName
    let v = decodeLazy s :: Either String TTF
    putStrLn $ ppShow v
    --let t = ttfTables [PostTable $ Post 0 0 0 0 0 0 0 0 0]
    --B.writeFile "out.ttf" $ runPut t

