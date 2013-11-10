import Instructions
import qualified Data.ByteString.Lazy as B

tab = compileTables [
        Table "cmap" (cmapFormat0 0 (take 262 $ repeat 0)),
        Table "glyf" (glyph 0 0 10 10 [0,1,2] (return ()) [3,3,3] [3,4,5] [6,7,8]),
        Table "hhea" (hhea 0 0 0 1 0 0 0 0 0 1),
        Table "hmtx" (hmtx (hmtxEntry 0 0)),
        Table "loca" (_loca [0]),
        Table "maxp" (maxp 1 1 1 1 1 1 1 1 1 1 1 1 1 1),
        Table "name" (nameHeaderMS [MSNRecord Copyright "bacon", MSNRecord Fullname "tree", MSNRecord UUID "fish"]),
        Table "post" (post3 0 0 0 True)
    ] (headTable 1 1 0 0 0 0 1 1 0 0 0 1 0)    

main = do B.putStr $ fst $ compile tab
