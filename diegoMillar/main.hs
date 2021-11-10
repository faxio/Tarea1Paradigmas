import System.Environment
import System.Random
import Text.Printf
import Data.Time.Clock
import System.IO

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf " %.5fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

-- Imprimir las colisiones
printCol :: [(Float,Float)] -> [[(Float,Float)]] -> IO()
printCol x y =  printf $ genText x y

genText :: [(Float,Float)] -> [[(Float,Float)]] -> String
genText [] [] = ""
genText (x:xs) (y:ys) = if y/= [] then "Particula: " ++ show x ++ "  Colisiona con: " ++ show y ++ " \n" ++ genText xs ys else genText xs ys

-- Generar Particulas :: semilla -> numero particulas -> largo del espacio -> lista de particulas
generarParticulas :: StdGen -> Int -> Float -> [(Float,Float)]
generarParticulas gen n l = listaRandom
			    where
				lista = take (2*n) $ randomRs (0,l) gen :: [Float] 
				listaRandom = zip (take n lista) (take n $ reverse lista)

-- cold Fuerza bruta :: lista de particulas -> radio -> lista de colisiones
coldFB :: [(Float,Float)] -> Float -> [[(Float,Float)]]
coldFB [] r = []
coldFB (p:ps) r = colision ps p r : coldFB ps r

colision :: [(Float,Float)] -> (Float,Float) -> Float -> [(Float,Float)]
colision ps p r = filter (distancia p r) ps

distancia :: (Float,Float) -> Float -> (Float,Float) -> Bool
distancia (px,py) r (opx,opy) 
    | sqrt ((opx-px)**2 + (opy-py)**2)<(r*2) = True
    | otherwise = False


main :: IO ()
main = do
    gen <- getStdGen

    args <- getArgs
    if (length args) /= 5
        then error $ "Correr el programa: ./prog <m> <r> <L> <Q> <n>"
        else return ()
    let
        metodo = read (args !! 0) :: Int
        radio = read (args !! 1) :: Float
        tam = read (args !! 2) :: Float
        maxParRegion = read (args !! 3) :: Int
        numParticulas = read (args !! 4) :: Int

    printf "****** Deteccion de colisiones ******\n metodo:     "

    if metodo==1 then printf " 1 -> QuadTree \n" else printf " 0 -> Fuerza Bruta\n"
    printf ("Metodo:\t" ++ (show metodo))
    printf ("Parametros:\t r=" ++ (show radio) ++ ", L=" ++ (show tam) ++ ", Q=" ++ (show maxParRegion) ++ ", n=" ++ (show numParticulas) ++ "\n")

    hFlush stdout
    t0 <- getCurrentTime
    printf ("Generando " ++ (show numParticulas) ++ " particulas aleatorias....\n")
    let listaParticulas = generarParticulas gen numParticulas tam
    printTimeSince t0
    printf "Ok\n"

    printf ("Lista generada: " ++ (show listaParticulas) ++ "\n")

    printf "\n"

    t0 <- getCurrentTime
    let listaColisiones = coldFB listaParticulas radio

    printCol listaParticulas listaColisiones 
    --printf ("colisiones: \n" ++ (show listaColisiones) ++ "\n")
    
    printTimeSince t0
