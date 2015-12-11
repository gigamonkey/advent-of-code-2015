import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)

puzzle = "yzbqklnj"

h key = unpack . encode . hash . pack . (key ++) . show

isCoin = ("00000" == ) . take 5

main = print $ head $ filter (\(i, h) -> isCoin h) $ map (\i -> (i, h puzzle i)) [0..]
