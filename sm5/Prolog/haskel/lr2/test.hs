import Control.Monad (forM_)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)

-- Тип для представления полинома: список коэффициентов, начиная с младшей степени
type Polynomial = [Double]

-- Функция для безопасного ввода списка чисел
inputPolynomial :: String -> IO Polynomial
inputPolynomial prompt = do
  putStrLn prompt
  input <- getLine
  let coeffs = map readMaybe (words input) :: [Maybe Double]
  if all isJust coeffs
    then return (map fromJust coeffs)
    else do
      putStrLn "Ошибка: введены некорректные данные. Пожалуйста, введите числа, разделенные пробелами."
      inputPolynomial prompt

-- Функция для безопасного ввода числа
inputNumber :: String -> IO Double
inputNumber prompt = do
  putStrLn prompt
  input <- getLine
  case readMaybe input of
    Just value -> return value
    Nothing -> do
      putStrLn "Ошибка: введено некорректное число. Пожалуйста, введите число."
      inputNumber prompt

-- Функция для безопасного ввода целого числа
inputInt :: String -> IO Int
inputInt prompt = do
  putStrLn prompt
  input <- getLine
  case readMaybe input of
    Just value -> return value
    Nothing -> do
      putStrLn "Ошибка: введено некорректное число. Пожалуйста, введите целое число."
      inputInt prompt

-- Вычисление значения полинома в точке x
evalPolynomial :: Polynomial -> Double -> Double
evalPolynomial coeffs x = sum $ zipWith (*) coeffs (iterate (*x) 1)

-- Функция f(x, y) для уравнения Риккати
f :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
f p q r x y = evalPolynomial p x + evalPolynomial q x * y + evalPolynomial r x * y^2

-- Частные производные для f(x, y)
partialX :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialX p q r x y = evalPolynomial (tail p) x  -- Производная P(x) по x

partialY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialY p q r x y = evalPolynomial q x + 2 * evalPolynomial r x * y  -- Производная f по y

partialXX :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXX p q r x y = evalPolynomial (drop 2 p) x  -- Вторая производная P(x) по x

partialXY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXY p q r x y = evalPolynomial (tail q) x + 2 * evalPolynomial (tail r) x * y  -- Смешанная производная

partialYY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialYY p q r x y = 2 * evalPolynomial r x  -- Вторая производная f по y

partialXXX :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXXX p q r x y = evalPolynomial (drop 3 p) x  -- Третья производная P(x) по x

partialXXY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXXY p q r x y = evalPolynomial (drop 2 q) x + 2 * evalPolynomial (drop 2 r) x * y  -- Смешанная производная

partialXYY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXYY p q r x y = 2 * evalPolynomial (tail r) x  -- Смешанная производная

partialYYY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialYYY p q r x y = 0  -- Третья производная f по y (для уравнения Риккати равна 0)

partialXXXX :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXXXX p q r x y = evalPolynomial (drop 4 p) x  -- Четвертая производная P(x) по x

partialXXXY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXXXY p q r x y = evalPolynomial (drop 3 q) x + 2 * evalPolynomial (drop 3 r) x * y  -- Смешанная производная

partialXXYY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXXYY p q r x y = 2 * evalPolynomial (drop 2 r) x  -- Смешанная производная

partialXYYY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialXYYY p q r x y = 0  -- Смешанная производная (для уравнения Риккати равна 0)

partialYYYY :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double
partialYYYY p q r x y = 0  -- Четвертая производная f по y (для уравнения Риккати равна 0)

-- Метод Нюстрема второго порядка
nystrom2 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom2 p q r h x y =
  let fVal = f p q r x y
      dfdx = partialX p q r x y
      dfdy = partialY p q r x y
  in y + h * fVal + (h^2 / 2) * (dfdx + fVal * dfdy)

-- Метод Нюстрема третьего порядка
nystrom3 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom3 p q r h x y =
  let fVal = f p q r x y
      dfdx = partialX p q r x y
      dfdy = partialY p q r x y
      d2fdx2 = partialXX p q r x y
      d2fdxdy = partialXY p q r x y
      d2fdy2 = partialYY p q r x y
  in y + h * fVal + (h^2 / 2) * (dfdx + fVal * dfdy) + (h^3 / 6) * (d2fdx2 + 2 * fVal * d2fdxdy + fVal^2 * d2fdy2)

-- Метод Нюстрема четвертого порядка
nystrom4 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom4 p q r h x y =
  let fVal = f p q r x y
      dfdx = partialX p q r x y
      dfdy = partialY p q r x y
      d2fdx2 = partialXX p q r x y
      d2fdxdy = partialXY p q r x y
      d2fdy2 = partialYY p q r x y
      d3fdx3 = partialXXX p q r x y
      d3fdx2dy = partialXXY p q r x y
      d3fdxdy2 = partialXYY p q r x y
      d3fdy3 = partialYYY p q r x y
  in y + h * fVal + (h^2 / 2) * (dfdx + fVal * dfdy) + (h^3 / 6) * (d2fdx2 + 2 * fVal * d2fdxdy + fVal^2 * d2fdy2) + (h^4 / 24) * (d3fdx3 + 3 * fVal * d3fdx2dy + 3 * fVal^2 * d3fdxdy2 + fVal^3 * d3fdy3)

-- Метод Нюстрема пятого порядка
nystrom5 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom5 p q r h x y =
  let fVal = f p q r x y
      dfdx = partialX p q r x y
      dfdy = partialY p q r x y
      d2fdx2 = partialXX p q r x y
      d2fdxdy = partialXY p q r x y
      d2fdy2 = partialYY p q r x y
      d3fdx3 = partialXXX p q r x y
      d3fdx2dy = partialXXY p q r x y
      d3fdxdy2 = partialXYY p q r x y
      d3fdy3 = partialYYY p q r x y
      d4fdx4 = partialXXXX p q r x y
      d4fdx3dy = partialXXXY p q r x y
      d4fdx2dy2 = partialXXYY p q r x y
      d4fdxdy3 = partialXYYY p q r x y
      d4fdy4 = partialYYYY p q r x y
  in y + h * fVal + (h^2 / 2) * (dfdx + fVal * dfdy) + (h^3 / 6) * (d2fdx2 + 2 * fVal * d2fdxdy + fVal^2 * d2fdy2) + (h^4 / 24) * (d3fdx3 + 3 * fVal * d3fdx2dy + 3 * fVal^2 * d3fdxdy2 + fVal^3 * d3fdy3) + (h^5 / 120) * (d4fdx4 + 4 * fVal * d4fdx3dy + 6 * fVal^2 * d4fdx2dy2 + 4 * fVal^3 * d4fdxdy3 + fVal^4 * d4fdy4)

-- Решение уравнения методом Нюстрема
solveNystrom :: (Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double) -> Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Int -> IO [(Double, Double)]
solveNystrom method p q r h y0 steps = do
  let solve = iterate (\(x, y) -> (x + h, method p q r h x y)) (0, y0)
  return $ take (steps + 1) solve

-- Основная функция
main :: IO ()
main = do
  -- Ввод полиномов
  p <- inputPolynomial "Введите коэффициенты полинома P(x) (начиная с младшей степени, через пробел):"
  q <- inputPolynomial "Введите коэффициенты полинома Q(x) (начиная с младшей степени, через пробел):"
  r <- inputPolynomial "Введите коэффициенты полинома R(x) (начиная с младшей степени, через пробел):"

  -- Ввод начального значения y(0)
  y0 <- inputNumber "Введите начальное значение y(0):"

  -- Ввод шага h
  h <- inputNumber "Введите шаг h:"

  -- Ввод количества шагов
  steps <- inputInt "Введите количество шагов:"

  -- Решение уравнения всеми методами
  start2 <- getCurrentTime
  result2 <- solveNystrom nystrom2 p q r h y0 steps
  end2 <- getCurrentTime
  let time2 = diffUTCTime end2 start2

  start3 <- getCurrentTime
  result3 <- solveNystrom nystrom3 p q r h y0 steps
  end3 <- getCurrentTime
  let time3 = diffUTCTime end3 start3

  start4 <- getCurrentTime
  result4 <- solveNystrom nystrom4 p q r h y0 steps
  end4 <- getCurrentTime
  let time4 = diffUTCTime end4 start4

  start5 <- getCurrentTime
  result5 <- solveNystrom nystrom5 p q r h y0 steps
  end5 <- getCurrentTime
  let time5 = diffUTCTime end5 start5

  -- Вывод результатов
  putStrLn "Результаты для метода Нюстрема 2-го порядка:"
  forM_ result2 $ \(x, y) -> printf "x = %.2f, y = %.6f\n" x y
  printf "Время выполнения: %.6f секунд\n" (realToFrac time2 :: Double)

  putStrLn "\nРезультаты для метода Нюстрема 3-го порядка:"
  forM_ result3 $ \(x, y) -> printf "x = %.2f, y = %.6f\n" x y
  printf "Время выполнения: %.6f секунд\n" (realToFrac time3 :: Double)

  putStrLn "\nРезультаты для метода Нюстрема 4-го порядка:"
  forM_ result4 $ \(x, y) -> printf "x = %.2f, y = %.6f\n" x y
  printf "Время выполнения: %.6f секунд\n" (realToFrac time4 :: Double)

  putStrLn "\nРезультаты для метода Нюстрема 5-го порядка:"
  forM_ result5 $ \(x, y) -> printf "x = %.2f, y = %.6f\n" x y
  printf "Время выполнения: %.6f секунд\n" (realToFrac time5 :: Double)
