import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Read (readMaybe)
import Control.Monad (when)

-- Тип для представления многочлена: список коэффициентов
type Polynomial = [Double]

-- Вычисление значения многочлена в точке x
evalPolynomial :: Polynomial -> Double -> Double
evalPolynomial coeffs x = sum $ zipWith (*) coeffs (iterate (* x) 1)

-- Запрос многочлена у пользователя
inputPolynomial :: String -> IO Polynomial
inputPolynomial name = do
    putStrLn $ "Введите коэффициенты многочлена " ++ name ++ " (через пробел, например, 1 2 3 для 1 + 2x + 3x^2):"
    input <- getLine
    let coeffs = mapM readMaybe (words input) :: Maybe [Double]
    case coeffs of
        Just cs -> return cs
        Nothing -> do
            putStrLn "Некорректный ввод. Попробуйте ещё раз."
            inputPolynomial name

-- Запрос начального значения у пользователя
inputInitialValue :: IO Double
inputInitialValue = do
    putStrLn "Введите начальное значение y(x0):"
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing -> do
            putStrLn "Некорректный ввод. Попробуйте ещё раз."
            inputInitialValue

-- Итерация метода Иострема
nextApproximation :: (Double -> Double) -> Polynomial -> Polynomial -> Polynomial -> Double -> Double
nextApproximation yk pCoeffs qCoeffs rCoeffs x =
    evalPolynomial pCoeffs x + evalPolynomial qCoeffs x * yk x + evalPolynomial rCoeffs x * yk x ** 2

-- Метод Иострема для n итераций
iostromMethod :: Int -> Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
iostromMethod n pCoeffs qCoeffs rCoeffs x0 x = iterate nextStep y0 !! n $ x
  where
    y0 _ = x0  -- Начальное приближение
    nextStep yk = nextApproximation yk pCoeffs qCoeffs rCoeffs

-- Сравнение скорости работы алгоритмов
compareSpeed :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> IO ()
compareSpeed pCoeffs qCoeffs rCoeffs x0 x = do
    putStrLn "Сравнение скорости работы алгоритмов:"

    start1 <- getCurrentTime
    let result1 = iostromMethod 2 pCoeffs qCoeffs rCoeffs x0 x
    end1 <- getCurrentTime
    putStrLn $ "Метод Иострема 2-го порядка: " ++ show result1 ++ " (время: " ++ show (diffUTCTime end1 start1) ++ ")"

    start2 <- getCurrentTime
    let result2 = iostromMethod 3 pCoeffs qCoeffs rCoeffs x0 x
    end2 <- getCurrentTime
    putStrLn $ "Метод Иострема 3-го порядка: " ++ show result2 ++ " (время: " ++ show (diffUTCTime end2 start2) ++ ")"

    start3 <- getCurrentTime
    let result3 = iostromMethod 4 pCoeffs qCoeffs rCoeffs x0 x
    end3 <- getCurrentTime
    putStrLn $ "Метод Иострема 4-го порядка: " ++ show result3 ++ " (время: " ++ show (diffUTCTime end3 start3) ++ ")"

    start4 <- getCurrentTime
    let result4 = iostromMethod 5 pCoeffs qCoeffs rCoeffs x0 x
    end4 <- getCurrentTime
    putStrLn $ "Метод Иострема 5-го порядка: " ++ show result4 ++ " (время: " ++ show (diffUTCTime end4 start4) ++ ")"

-- Основная функция
main :: IO ()
main = do
    -- Запрос многочленов у пользователя
    pCoeffs <- inputPolynomial "P(x)"
    qCoeffs <- inputPolynomial "Q(x)"
    rCoeffs <- inputPolynomial "R(x)"

    -- Запрос начального значения
    x0 <- inputInitialValue

    -- Запрос точки, в которой нужно вычислить решение
    putStrLn "Введите точку x, в которой нужно вычислить решение:"
    xInput <- getLine
    let x = read xInput :: Double

    -- Вычисление и вывод результатов
    putStrLn $ "Решение уравнения Риккати в точке x = " ++ show x
    putStrLn $ "Метод Иострема 2-го порядка: " ++ show (iostromMethod 2 pCoeffs qCoeffs rCoeffs x0 x)
    putStrLn $ "Метод Иострема 3-го порядка: " ++ show (iostromMethod 3 pCoeffs qCoeffs rCoeffs x0 x)
    putStrLn $ "Метод Иострема 4-го порядка: " ++ show (iostromMethod 4 pCoeffs qCoeffs rCoeffs x0 x)
    putStrLn $ "Метод Иострема 5-го порядка: " ++ show (iostromMethod 5 pCoeffs qCoeffs rCoeffs x0 x)

    -- Сравнение скорости
    compareSpeed pCoeffs qCoeffs rCoeffs x0 x
