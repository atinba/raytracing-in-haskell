module Main where

pixels :: Int -> Int -> [(Int, Int, Int)]
pixels h w = [(div (255 * i) (w - 1), div (255 * j) (h - 1), 0) | j <- [0 .. h - 1], i <- [0 .. w - 1]]

-- pixel2str :: (Int, Int, Int) -> String
-- pixel2str (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

pixels2str :: [(Int, Int, Int)] -> String
pixels2str [] = ""
pixels2str (p : ps) = pixel2str p ++"\n" ++ pixels2str ps where pixel2str (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b


writePPM :: String -> Int -> Int -> IO ()
writePPM file h w = do
  writeFile file ("P3\n" ++ show h ++ " " ++ show w ++ "\n255\n" ++ pixels2str (pixels h w))

main :: IO ()
main = writePPM "img.ppm" 256 256
