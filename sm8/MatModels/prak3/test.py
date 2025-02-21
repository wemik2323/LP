import itertools
import math

# Данные
y = [0.03, 0.09, 0.9, 0.002]  # Интенсивности нагрузки
N = 7  # Общее число каналов
J = len(y)  # Количество направлений связи

# Функция для расчета вероятности потерь по формуле Эрланга 
def erlang_b(A, n):
    """ Вычисляет вероятность потерь по формуле Эрланга """
    if n == 0:
        return 1  # Если каналов нет, потери = 100%
    numerator = (A ** n) / math.factorial(n)
    denominator = sum((A ** k) / math.factorial(k) for k in range(n + 1))
    return numerator / denominator

# Функция для оценки качества распределения каналов
def evaluate_distribution(n_alloc):
    """ Рассчитывает средневзвешенную вероятность потерь """
    total_traffic = sum(y)
    weighted_loss = sum((y[j] / total_traffic) * erlang_b(y[j], n_alloc[j]) for j in range(J))
    return weighted_loss

# Генерация всех возможных распределений N каналов между J направлениями
best_distribution = None
min_loss = float('inf')

print("Возможные распределения каналов и их вероятность потерь:\n")
print("№  |  Распределение каналов   |  Средневзвешенная вероятность потерь")
print("-" * 65)

all_distributions = []

# Теперь используем itertools.product() для полного перебора вариантов
for n_alloc in itertools.product(range(1, N + 1), repeat=J):
    if sum(n_alloc) == N:  # Только комбинации, где сумма = N
        loss = evaluate_distribution(n_alloc)
        all_distributions.append((n_alloc, loss))

# Сортируем по возрастанию вероятности потерь
all_distributions.sort(key=lambda x: x[1])

# Вывод всех комбинаций
for idx, (n_alloc, loss) in enumerate(all_distributions, start=1):
    print(f"{idx:2d}  |  {n_alloc}  |  {loss:.6f}")

    # Сохраняем лучшее распределение
    if loss < min_loss:
        min_loss = loss
        best_distribution = n_alloc

# Вывод оптимального распределения
print("\nОптимальное распределение каналов:", best_distribution)
print("Минимальная средневзвешенная вероятность потерь:", round(min_loss, 6))

# Вывод вероятности потерь для каждого направления
print("\nВероятность потерь для каждого направления:")
for j in range(J):
    print(f"Направление {j+1}: {best_distribution[j]} каналов, P_loss = {round(erlang_b(y[j], best_distribution[j]), 6)}")

