import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA
import System.IO
import Control.Monad (forM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception (catch, SomeException)

-- Функция для чтения матрицы из файла с проверкой данных
readMatrixFromFile :: FilePath -> IO (Matrix Double)
readMatrixFromFile path = do
  contents <- readFile path
  let rows = lines contents
  -- Удаляем пустые строки (если они есть)
  let nonEmptyRows = filter (not . null) rows
  -- Преобразуем строки в списки чисел
  let matrixData = map (map read . words) nonEmptyRows
  -- Проверяем, что все строки имеют одинаковую длину
  if all (\row -> length row == length (head matrixData)) matrixData
    then return $ fromLists matrixData
    else error $ "Ошибка: строки в файле " ++ path ++ " имеют разную длину."

-- Функция для генерации матрицы R (единичная матрица размером n x n)
generateR :: Int -> Matrix Double
generateR n = ident n

-- Функция для вычисления правой части уравнения Риккати
riccatiRHS :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
riccatiRHS a e b q r p = 
  (tr e LA.<> p LA.<> a) + (tr a LA.<> p LA.<> e) + q - (tr e LA.<> p LA.<> b LA.<> inv r LA.<> tr b LA.<> p LA.<> e)

-- Метод Нюстрема второго порядка
nystrom2 :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Matrix Double -> Matrix Double
nystrom2 a e b q r dt p = pNew
  where
    k1 = riccatiRHS a e b q r p
    pMid = p + scale dt k1
    k2 = riccatiRHS a e b q r pMid
    pNew = p + scale (dt / 2) (k1 + k2)

-- Метод Нюстрема третьего порядка
nystrom3 :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Matrix Double -> Matrix Double
nystrom3 a e b q r dt p = pNew
  where
    k1 = riccatiRHS a e b q r p
    pMid1 = p + scale (dt / 2) k1
    k2 = riccatiRHS a e b q r pMid1
    pMid2 = p + scale dt (2 * k2 - k1)
    k3 = riccatiRHS a e b q r pMid2
    pNew = p + scale (dt / 6) (k1 + 4 * k2 + k3)

-- Метод Нюстрема четвертого порядка
nystrom4 :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Matrix Double -> Matrix Double
nystrom4 a e b q r dt p = pNew
  where
    k1 = riccatiRHS a e b q r p
    pMid1 = p + scale (dt / 2) k1
    k2 = riccatiRHS a e b q r pMid1
    pMid2 = p + scale (dt / 2) k2
    k3 = riccatiRHS a e b q r pMid2
    pMid3 = p + scale dt k3
    k4 = riccatiRHS a e b q r pMid3
    pNew = p + scale (dt / 6) (k1 + 2 * k2 + 2 * k3 + k4)

-- Метод Нюстрема пятого порядка
nystrom5 :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Matrix Double -> Matrix Double
nystrom5 a e b q r dt p = pNew
  where
    k1 = riccatiRHS a e b q r p
    pMid1 = p + scale (dt / 4) k1
    k2 = riccatiRHS a e b q r pMid1
    pMid2 = p + scale (dt / 4) k2
    k3 = riccatiRHS a e b q r pMid2
    pMid3 = p + scale (dt / 2) k3
    k4 = riccatiRHS a e b q r pMid3
    pMid4 = p + scale (3 * dt / 4) k4
    k5 = riccatiRHS a e b q r pMid4
    pNew = p + scale (dt / 90) (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k1)

-- Итерационное решение уравнения Риккати
solveRiccati :: (Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Matrix Double -> Matrix Double)
             -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Double -> Matrix Double -> [Matrix Double]
solveRiccati method a e b q r tEnd dt p
  | tEnd <= 0 = [p]
  | otherwise = p : solveRiccati method a e b q r (tEnd - dt) dt (method a e b q r dt p)

-- Запись результата в файл
writeResultToFile :: FilePath -> Int -> Double -> Double -> Double -> [Matrix Double] -> IO ()
writeResultToFile path order t0 tEnd dt solution = do
  let header = "Метод Нюстрема " ++ show order ++ " порядка\nt0 = " ++ show t0 ++ ", tEnd = " ++ show tEnd ++ ", dt = " ++ show dt ++ "\n\nРезультат:\n"
  let result = unlines $ map (unlines . map (unwords . map show) . toLists) solution
  writeFile path (header ++ result)

-- Основная функция
main :: IO ()
main = do
  -- Считываем матрицы из файлов с обработкой ошибок
  aMat <- catch (readMatrixFromFile "src/A.dat") (\e -> print (e :: SomeException) >> return (konst 0 (2, 2)))
  bMat <- catch (readMatrixFromFile "src/B.dat") (\e -> print (e :: SomeException) >> return (konst 0 (2, 1)))
  qMat <- catch (readMatrixFromFile "src/Q.dat") (\e -> print (e :: SomeException) >> return (konst 0 (2, 2)))
  eMat <- catch (readMatrixFromFile "src/E.dat") (\e -> print (e :: SomeException) >> return (konst 0 (2, 2)))
  p0Mat <- catch (readMatrixFromFile "src/P0.dat") (\e -> print (e :: SomeException) >> return (konst 0 (2, 2)))
  
  -- Генерируем матрицу R
  let n = cols bMat
  let rMat = generateR n
  
  -- Запрашиваем у пользователя шаг dt и конечное время tEnd
  putStrLn "Введите шаг dt:"
  dt <- readLn
  putStrLn "Введите конечное время tEnd:"
  tEnd <- readLn
  
  -- Решаем уравнение Риккати методами Нюстрема разных порядков и измеряем время
  let methods = [(nystrom2, 2), (nystrom3, 3), (nystrom4, 4), (nystrom5, 5)]
  forM_ methods $ \(method, order) -> do
    startTime <- getCurrentTime  -- Засекаем время начала
    let solution = solveRiccati method aMat eMat bMat qMat rMat tEnd dt p0Mat
    endTime <- getCurrentTime  -- Засекаем время окончания
    let elapsedTime = diffUTCTime endTime startTime  -- Вычисляем разницу во времени
    
    -- Записываем результат в файл
    let outputFile = "out/out" ++ show order ++ ".txt"
    writeResultToFile outputFile order 0 tEnd dt solution
    
    -- Выводим время выполнения
    putStrLn $ "Метод Нюстрема " ++ show order ++ " порядка выполнен за " ++ show elapsedTime ++ " секунд."
    putStrLn $ "Результат записан в файл " ++ outputFile
