import System.Environment
import System.Random
import Text.Printf
import Data.Time.Clock

-- Generar Particulas :: semilla -> numero particulas -> largo del espacio -> lista de particulas
generarParticulas :: StdGen -> Int -> Int -> [(Int,Int)]
generarParticulas gen n l = listaRandom
			    where
				lista = take (2*n) $ randomRs (0,l) gen :: [Int] 
				listaRandom = zip (take n lista) (take n $ reverse lista)

-- QuadTree
data QT b a = L a | N b (QT b a) (QT b a) (QT b a) (QT b a) deriving (Show)

-- Construir quadTree :: lista de particulas -> cantidad maxima de particulas por region -> largo del espacio -> nuevo origen del plano -> quadTree
buildQT :: [(Int,Int)] -> Int -> Int -> (Int,Int) -> QT (Int,Int) [(Int,Int)]
buildQT [] q l _ = L []
buildQT xs q l or
	    | length xs <= q = L xs 
	    | otherwise = N or (buildQT region1 q newL or1) (buildQT region2 q newL or2) (buildQT region3 q newL or3) (buildQT region4 q newL or4)
	    where
		newL = (round ((fromIntegral l)/2))
		or1 = (0,0)
		or2 = (newL,0)
		or3 = (newL,newL)
		or4 = (0,newL)
		region1 = divRegion (<) (<) l xs or 
		region2 = divRegion (>=) (<) l xs or 
		region3 = divRegion (>=) (>=) l xs or 
		region4 = divRegion (<) (>=) l xs or



-- divRegion :: funcoin para comparar con eje x -> funcion para comparar con eje y -> largo -> particulas -> nuevo origen -> particulas filtradas
divRegion :: (Float -> Float -> Bool) -> (Float -> Float -> Bool) -> Int -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
divRegion f1 f2 l ps (xo,yo) = filter (\(x,y) -> if(f1 (fromIntegral x) nl && f2 (fromIntegral y) nl) then True else False) $ map f3 ps 
			where
			    nl = (/2) $ fromIntegral l
		 	    f3 = (\(x,y) -> (x-xo,y-yo))
