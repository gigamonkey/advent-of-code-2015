import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)

puzzle = "yzbqklnj"

h key = unpack . encode . hash . pack . (key ++) . show

isCoin' n = (replicate n '0' ==) . take n

mine key = map (h key) [0 .. ]

main = print $ head $ filter (isCoin' 6 . snd) $ zip [0 ..] $ mine puzzle
