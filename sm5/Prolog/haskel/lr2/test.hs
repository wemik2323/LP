import Control.Monad (forM_, when)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isJust, fromJust)

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

-- Метод Нюстрема второго порядка
nystrom2 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom2 p q r h x y =
  let fVal = f p q r x y
      dfdy = evalPolynomial q x + 2 * evalPolynomial r x * y
  in y + h * fVal + (h^2 / 2) * dfdy * fVal

-- Метод Нюстрема третьего порядка
nystrom3 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom3 p q r h x y =
  let fVal = f p q r x y
      dfdy = evalPolynomial q x + 2 * evalPolynomial r x * y
      d2fdy2 = 2 * evalPolynomial r x
  in y + h * fVal + (h^2 / 2) * dfdy * fVal + (h^3 / 6) * (d2fdy2 * fVal^2)

-- Метод Нюстрема четвертого порядка
nystrom4 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom4 p q r h x y =
  let fVal = f p q r x y
      dfdy = evalPolynomial q x + 2 * evalPolynomial r x * y
      d2fdy2 = 2 * evalPolynomial r x
  in y + h * fVal + (h^2 / 2) * dfdy * fVal + (h^3 / 6) * (d2fdy2 * fVal^2)

-- Метод Нюстрема пятого порядка
nystrom5 :: Polynomial -> Polynomial -> Polynomial -> Double -> Double -> Double -> Double
nystrom5 p q r h x y =
  let fVal = f p q r x y
      dfdy = evalPolynomial q x + 2 * evalPolynomial r x * y
      d2fdy2 = 2 * evalPolynomial r x
  in y + h * fVal + (h^2 / 2) * dfdy * fVal + (h^3 / 6) * (d2fdy2 * fVal^2)

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

  -- Ввод порядка метода
  order <- inputInt "Выберите порядок метода Нюстрема (2, 3, 4, 5):"
  when (order < 2 || order > 5) $ do
    putStrLn "Ошибка: порядок метода должен быть 2, 3, 4 или 5."
    main

  -- Выбор метода
  let method = case order of
                2 -> nystrom2 p q r
                3 -> nystrom3 p q r
                4 -> nystrom4 p q r
                5 -> nystrom5 p q r
                _ -> error "Некорректный порядок метода"

  -- Итерационный процесс
  let solve = iterate (\(x, y) -> (x + h, method h x y)) (0, y0)
  let results = take (steps + 1) solve

  -- Вывод результатов
  putStrLn "Результаты:"
  forM_ results $ \(x, y) -> printf "x = %.2f, y = %.6f\n" x y
