import Numeric.LinearAlgebra

-- Функция для решения уравнения Риккати методом Нюстрема
solveRiccati :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Int -> Matrix Double
solveRiccati a b q r order = iterateRiccati p0
  where
    p0 = 0 -- Начальное приближение для P(t)
    iterateRiccati p = case order of
      2 -> iterateRiccati2 p
      3 -> iterateRiccati3 p
      4 -> iterateRiccati4 p
      5 -> iterateRiccati5 p
      _ -> error "Unsupported order"
    
    iterateRiccati2 p = p + dt * dpdt p
    iterateRiccati3 p = p + dt * (dpdt p + dpdt (p + dt * dpdt p)) / 2
    iterateRiccati4 p = p + dt * (dpdt p + dpdt (p + dt * dpdt p) + dpdt (p + dt * dpdt (p + dt * dpdt p))) / 3
    iterateRiccati5 p = p + dt * (dpdt p + dpdt (p + dt * dpdt p) + dpdt (p + dt * dpdt (p + dt * dpdt p)) + dpdt (p + dt * dpdt (p + dt * dpdt (p + dt * dpdt p)))) / 4
    
    dpdt p = - (tr a <> p + tr p <> a + q - tr p <> b <> inv r <> tr b <> p)
    
    dt = 0.01 -- Шаг по времени

-- Пример использования
main :: IO ()
main = do
  let a = (2><2) [1, 0, 0, 1] -- Матрица A
      b = (2><2) [1, 0, 0, 1] -- Матрица B
      q = (2><2) [1, 0, 0, 1] -- Матрица Q
      r = (2><2) [1, 0, 0, 1] -- Матрица R
      order = 2 -- Порядок метода Нюстрема
  let solution = solveRiccati a b q r order
  print solution
