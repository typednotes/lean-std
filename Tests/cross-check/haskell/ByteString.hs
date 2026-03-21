module Main where

import qualified Data.ByteString as BS
import Data.Word (Word8)

main :: IO ()
main = do
  putStrLn "=== ByteString ==="
  let bs = BS.pack [72, 101, 108, 108, 111]
  putStrLn $ "length = " ++ show (BS.length bs)
  putStrLn $ "head = " ++ show (BS.head bs)
  putStrLn $ "last = " ++ show (BS.last bs)
  putStrLn $ "take 3 = " ++ show (BS.unpack (BS.take 3 bs))
  putStrLn $ "drop 2 = " ++ show (BS.unpack (BS.drop 2 bs))
  putStrLn $ "reverse = " ++ show (BS.unpack (BS.reverse bs))
  putStrLn $ "elem 108 = " ++ show (BS.elem 108 bs)
  putStrLn $ "count 108 = " ++ show (BS.count 108 bs)
  putStrLn $ "isPrefixOf = " ++ show (BS.isPrefixOf (BS.pack [72, 101]) bs)
  putStrLn $ "isSuffixOf = " ++ show (BS.isPrefixOf (BS.pack [108, 111]) (BS.reverse bs))
  let intercalated = BS.intercalate (BS.singleton 0) [BS.singleton 1, BS.singleton 2, BS.singleton 3]
  putStrLn $ "intercalate = " ++ show (BS.unpack intercalated)
  putStrLn $ "replicate = " ++ show (BS.unpack (BS.replicate 3 42))
  putStrLn $ "filter (>105) = " ++ show (BS.unpack (BS.filter (>105) bs))
  putStrLn $ "foldl' sum = " ++ show (BS.foldl' (\acc w -> acc + fromIntegral w) (0::Int) bs)
