import System.Environment
import System.IO
import System.Random
import Data.Time.Clock
import Text.Printf


-- Imprimir las colisiones
printCol :: [(Int,Int)] -> [[(Int,Int)]] -> IO()
printCol x y =  printf $ genText x y

genText :: [(Int,Int)] -> [[(Int,Int)]] -> String
genText [] [] = ""
genText (x:xs) (y:ys) = if y/= [] then "Particula: " ++ show x ++ "  Colisiona con: " ++ show y ++ " \n" ++ genText xs ys else genText xs ys
--colision :: (Int,Int) -> (a,a) -> b -> Bool 
colision p1 p2 r 
    | sqrt((fst p2 - fst p1 )^2 + (snd p2 - snd p1 )^2) <= (2*r) = True
    | otherwise = False

colFB :: [(Int, Int)] -> Int -> IO [[(Int,Int)]] 
colFB y r = return $ colFB1 y r
 --   |x == colFB xs r = x
colFB1 :: [(Int, Int)] -> Int -> [[(Int,Int)]] 
colFB1 [] _ = []
colFB1 (y:ys) r =  (filter (\x -> (sqrt((fromIntegral (fst y) - fromIntegral(fst x) )^2 + (fromIntegral (snd y) - fromIntegral (snd x ))^2) <= fromIntegral(2*r)) ) ys) : colFB1 ys r

-- Generacion de particulas
genParticulas :: Int-> Int -> Int -> IO [(Int,Int)]
genParticulas n rango c = return $ unirListas (random2 (mkStdGen c ) rango n) (random2 (mkStdGen (c+1)) rango n )


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
    par <- genParticulas n l 88
    printTimeSince t0
    print par
    hFlush stdout
    t0 <- getCurrentTime
    calc <- colFB par r 
    printCol par calc
    {-
    t1 <- getCurrentTime
    printf "Calculando colisiones...................ok: " n
    printTimeSince t1
    -}
    return ()


printTimeSince t0 = do
  t1 <- getCurrentTime
  printf " %.5fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)