import System.Environment
import System.IO
import System.Random
import Data.Time.Clock
import Text.Printf
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Control.DeepSeq
--import Control.Seq (Strategy)

divRegion :: (Float -> Float -> Bool) -> (Float -> Float -> Bool) -> Int -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
divRegion f1 f2 l ps (xo,yo) = filter (\(x,y) -> if(f1 (fromIntegral x) nl && f2 (fromIntegral y) nl) then True else False) $ map f3 ps 
			where
			    nl = (/2) $ fromIntegral l
		 	    f3 = (\(x,y) -> (x-xo,y-yo))



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

-- Imprimir las colisiones
printCol :: [(Int,Int)] -> [[(Int,Int)]] -> IO()
printCol x y =  printf $ genText x y

genText :: [(Int,Int)] -> [[(Int,Int)]] -> String
genText [] [] = ""
genText (x:xs) (y:ys) = if length y/=1  then "Particula: " ++ show x ++ "  Colisiona con: " ++ show (print2 x y) ++ " \n" ++ genText xs ys else genText xs ys

--filters ::
print2 :: (Int, Int) -> [(Int,Int)] -> [(Int,Int)]
print2 r y = filter (\x -> x/=r ) y 


colFB :: [(Int, Int)] -> Int -> [[(Int,Int)]] 
colFB y r = colFB1 y y r
 --   |x == colFB xs r = x
colFB1 :: [(Int, Int)] -> [(Int,Int)]-> Int -> [[(Int,Int)]] 
colFB1 [] _ _ = []
colFB1 (y:ys) z r =  (filter (\x -> (sqrt((fromIntegral (fst y) - fromIntegral(fst x) )^2 + (fromIntegral (snd y) - fromIntegral (snd x ))^2) <= fromIntegral(2*r)) ) z) : colFB1 ys z r

-- Generacion de particulas
genParticulas :: Int-> Int -> Int -> [(Int,Int)]
genParticulas n rango c = unirListas (random2 (mkStdGen c ) rango n) (random2 (mkStdGen (c+1)) rango n )


random2 :: StdGen->Int -> Int -> [Int]
random2 _ _ 0 = []
random2 gs rango n = 
    let (cs, newGen)= randomR (0,rango) gs :: (Int , StdGen)
    in cs : random2 newGen rango (n-1)


unirListas :: [Int] -> [Int] -> [(Int,Int)]
unirListas [] [] = []
unirListas (x:xs) (y:ys) = (x, y) : unirListas xs ys
{-
-- Fuerza bruta
colFB :: [] -> Int-> []
colFB [] _ = []
colFB (x:xs) _ = 
-}

-- Para generar particulas de aqui hasta unirlistas
{-
genParticulas :: Int-> Int -> IO [(Float,Float)]
genParticulas n c = return $ unirListas (random2 (mkStdGen c ) n) (random2 (mkStdGen (c+1)) n )


random2 :: StdGen -> Int -> [Float]
random2 _ 0 = []
random2 gs n = 
    let (cs, newGen)= randomR (0,1) gs :: (Float , StdGen)
    in redond cs : random2 newGen (n-1)

redond :: Float -> Float 
redond x = (fromIntegral (round (x*100)) :: Float) /100

unirListas :: [Float] -> [Float] -> [(Float,Float)]
unirListas [] [] = []
unirListas (x:xs) (y:ys) = (x, y) : unirListas xs ys
-}

main:: IO ()
main = do
    -- I) ARGS
    args <- getArgs
    if (length args) /= 5
        then error $ "Correr el programa: ./prog 1 1 100 8 25"
        else return ()
    let m = read (args !! 0) :: Int
    let r = read (args !! 1) :: Int
    let l = read (args !! 2) :: Int
    let q = read (args !! 3) :: Int
    let n = read (args !! 4) :: Int

    printf "****** Deteccion de colisiones ******\n metodo:     "
    if m==1 then printf " 1 -> QuadTree \n" else printf " 0 -> Fuerza Bruta\n"
    printf "Parametros:     r=%i,   L=%i,    Q=%i,  n=%i\n" r l q n

    hFlush stdout
    t0 <- getCurrentTime
    printf "Generando %i Particulas aleatorias......ok: " n
    --hFlush stdout
    let par = genParticulas n l 88 
    printTimeSince t0
    --print par
    hFlush stdout
    printf "Tiempo por fuerza bruta " 
    t0 <- getCurrentTime
    calc <- colFB par r `usingIO` (parListChunk 1000 rdeepseq ) 
    printTimeSince t0
    --printCol par calc
    --hFlush stdout
    --let qt = buildQT par q l (0,0)
    --print qt
    {-
    t1 <- getCurrentTime
    printf "Calculando colisiones...................ok: " n
    printTimeSince t1
    -}
    return ()


printTimeSince t0 = do
  t1 <- getCurrentTime
  printf " %.5fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)