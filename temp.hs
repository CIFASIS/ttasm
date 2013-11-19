import Template

import Data.Serialize
import qualified Data.ByteString.Lazy as BS
import System.Environment

main = do
    (fileName:_) <- getArgs
    s <- BS.readFile $ fileName
    let v = decodeLazy s :: Either String TTF
    print $ v

