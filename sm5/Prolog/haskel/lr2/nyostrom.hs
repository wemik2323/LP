{-# LANGUAGE OverloadedStrings #-}
import qualified Numeric.LinearAlgebra as LA
import System.IO
import Control.Monad
import Data.Time.Clock
import Text.Printf
import System.Directory (createDirectoryIfMissing)
import Prelude hiding (isNaN)

-- Типы данных
type Matrix = LA.Matrix Double
type Vector = LA.Vector Double

-- Чтение матрицы из файла
readMatrixFromFile :: FilePath -> IO Matrix
readMatrixFromFile filename = do
    contents <- readFile filename
    let rows = map (map read . words) $ lines contents
    return $ LA.fromLists rows

-- Уравнение Риккати
riccatiEquation :: Matrix -> Matrix -> Matrix -> Matrix -> Matrix -> Matrix
riccatiEquation e a b q p = 
    (e LA.<> p LA.<> a) + (LA.tr a LA.<> p LA.<> e) + q - (e LA.<> p LA.<> b LA.<> LA.tr b LA.<> p LA.<> e)

-- Метод Рунге-Кутты 4-го порядка
rungeKutta4 :: (Matrix -> Matrix) -> Double -> Matrix -> Matrix
rungeKutta4 f h p = p + LA.scale (h / 6) (k1 + k2 + k2 + k3 + k3 + k4)
  where
    k1 = f p
    k2 = f (p + LA.scale (h / 2) k1)
    k3 = f (p + LA.scale (h / 2) k2)
    k4 = f (p + LA.scale h k3)

-- Метод Нюстрема 2-го порядка с проверкой на NaN
nystrom2 :: (Matrix -> Matrix) -> Double -> Matrix -> Matrix -> Maybe Matrix
nystrom2 f h x1 x = 
    let result = x1 + LA.scale (2 * h) (f x)
    in if any (any isNaN) (LA.toLists result) then Nothing else Just result
  where isNaN x = x /= x

-- Метод Нюстрема 3-го порядка с проверкой на NaN
nystrom3 :: (Matrix -> Matrix) -> Double -> Matrix -> Matrix -> Matrix -> Maybe Matrix
nystrom3 f h x2 x1 x = 
    let result = x2 + LA.scale (h/3) (LA.scale 7 (f x) - LA.scale 2 (f x1) + f x2)
    in if any (any isNaN) (LA.toLists result) then Nothing else Just result
  where isNaN x = x /= x

-- Метод Нюстрема 4-го порядка с проверкой на NaN
nystrom4 :: (Matrix -> Matrix) -> Double -> Matrix -> Matrix -> Matrix -> Matrix -> Maybe Matrix
nystrom4 f h x3 x2 x1 x = 
    let result = x1 + LA.scale (h/3) (LA.scale 8 (f x) - LA.scale 5 (f x1) + LA.scale 4 (f x2) - f x3)
    in if any (any isNaN) (LA.toLists result) then Nothing else Just result
  where isNaN x = x /= x

-- Генерация начальных точек с проверкой
generateInitialPoints :: (Matrix -> Matrix) -> Double -> Matrix -> Int -> Maybe [Matrix]
generateInitialPoints f h p count = 
    foldM (\acc _ -> let next = rungeKutta4 f h (last acc)
                     in if any (any isNaN) (LA.toLists next) 
                        then Nothing 
                        else Just (acc ++ [next])) [p] [1..count-1]
  where isNaN x = x /= x

-- Проверка матрицы на NaN и Inf
isValidMatrix :: Matrix -> Bool
isValidMatrix m = not (any (any isNaN) (LA.toLists m)) && not (any (any isInfinite) (LA.toLists m))
  where isNaN x = x /= x
        isInfinite x = abs x > 1e100

-- Запись в лог-файл
writeLog :: String -> String -> IO ()
writeLog method msg = do
    createDirectoryIfMissing True "logs"
    let filename = "logs/log" ++ method ++ ".txt"
    appendFile filename (msg ++ "\n")

-- Запись прогресса в лог
logProgress :: String -> Double -> Double -> Double -> Double -> Int -> Double -> Matrix -> IO ()
logProgress method targetError error t h step tMax p = do
    let msg = unlines [
            "\nМетод: " ++ method,
            "Требуемая ошибка: " ++ show targetError ++ " | Ошибка: " ++ show error,
            "h: " ++ show h ++ " | Шаг: " ++ show step ++ " | t: " ++ show t ++ " из " ++ show tMax,
            "P (первые 8x8 элементов):",
            show (LA.subMatrix (0,0) (min 8 (LA.rows p), min 8 (LA.cols p)) p)
            ]
    writeLog method msg

-- Запись результатов
writeResults :: String -> Double -> Double -> Double -> Double -> Double -> Int -> Double -> Matrix -> IO ()
writeResults method t0 tMax h targetError error steps execTime p = do
    createDirectoryIfMissing True "Results"
    let filename = "Results/out" ++ method ++ ".txt"
    writeFile filename $ unlines [
        "Метод Нюстрема " ++ method ++ "-го порядка",
        "Время выполнения: " ++ show execTime ++ " сек.",
        "t = " ++ show t0 ++ " ; t_end = " ++ show tMax ++ " ; h = " ++ show h,
        "error_value = " ++ show targetError,
        "error = " ++ show error ++ " ; last_step = " ++ show steps ++ "\n",
        show p
        ]
    putStrLn $ "\ESC[32mРезультаты метода " ++ method ++ " записаны в " ++ filename ++ "\ESC[0m"

-- Измененная функция runNystromMethod (преобразуем время в секунды)
runNystromMethod :: Int -> (Matrix -> Matrix) -> Double -> Double -> Double -> Double -> Int -> Matrix -> IO ()
runNystromMethod order f t0 tMax h targetError maxSteps p0 = do
    let method = show order
    writeLog method $ "Запуск метода Нюстрема " ++ method ++ "-го порядка"
    
    startTime <- getCurrentTime
    let loop step pPrev p = do
            case generateInitialPoints f h p (order-1) of
                Nothing -> do
                    writeLog method "Ошибка: получены недопустимые значения (NaN или Inf) при генерации начальных точек"
                    endTime <- getCurrentTime
                    let execTime = realToFrac (diffUTCTime endTime startTime) :: Double
                    writeResults method t0 tMax h targetError (1/0) step execTime pPrev
                
                Just initPoints -> 
                    let methodResult = case order of
                            2 -> case initPoints of
                                    [x1] -> nystrom2 f h x1 p
                                    _ -> error "Недостаточно точек для метода 2-го порядка"
                            3 -> case initPoints of
                                    [x2, x1] -> nystrom3 f h x2 x1 p
                                    _ -> error "Недостаточно точек для метода 3-го порядка"
                            4 -> case initPoints of
                                    [x3, x2, x1] -> nystrom4 f h x3 x2 x1 p
                                    _ -> error "Недостаточно точек для метода 4-го порядка"
                            _ -> error "Неподдерживаемый порядок метода"
                    in
                        case methodResult of
                            Nothing -> do
                                writeLog method "Ошибка: получены недопустимые значения (NaN или Inf)"
                                endTime <- getCurrentTime
                                let execTime = realToFrac (diffUTCTime endTime startTime) :: Double
                                writeResults method t0 tMax h targetError (1/0) step execTime pPrev
                            
                            Just newP ->
                                if not (isValidMatrix newP)
                                then do
                                    writeLog method "Ошибка: получены недопустимые значения (NaN или Inf)"
                                    endTime <- getCurrentTime
                                    let execTime = realToFrac (diffUTCTime endTime startTime) :: Double
                                    writeResults method t0 tMax h targetError (1/0) step execTime pPrev
                                else do
                                    let error = LA.norm_Inf (newP - pPrev)
                                    
                                    -- Логирование прогресса
                                    logProgress method targetError error t0 h step tMax newP
                                    
                                    -- Условия выхода
                                    if step >= maxSteps || error <= targetError || error > 1e10
                                        then do
                                            endTime <- getCurrentTime
                                            let execTime = realToFrac (diffUTCTime endTime startTime) :: Double
                                            writeResults method t0 tMax h targetError error step execTime newP
                                        else loop (step+1) p newP
    
    loop 0 p0 p0

-- Основная функция
main :: IO ()
main = do
    -- Чтение матриц
    e <- readMatrixFromFile "input/E.dat"
    a <- readMatrixFromFile "input/A.dat"
    b <- readMatrixFromFile "input/B.dat"
    q <- readMatrixFromFile "input/Q.dat"
    
    -- params
    let t0 = 0.0
        tMax = 10.0
        h = 0.0001
        targetError = 1e-4
        maxSteps = 300000
        f = riccatiEquation e a b q
        p0 = LA.konst 0 (LA.rows e, LA.cols e)
    
    -- Создаем папки если их нет
    createDirectoryIfMissing True "logs"
    createDirectoryIfMissing True "Results"
    
    -- Запуск всех методов
    runNystromMethod 2 f t0 tMax h targetError maxSteps p0
    runNystromMethod 3 f t0 tMax h targetError maxSteps p0
    runNystromMethod 4 f t0 tMax h targetError maxSteps p0
    
    putStrLn "\nВсе методы выполнены. Результаты сохранены в папках logs/ и Results/"
