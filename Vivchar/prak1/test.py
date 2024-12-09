import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# Твои интервалы
intervals = [
    434000, 804000, 1174000, 1544000, 1914000, 2284000, 2654000, 3024000,
    3394000, 3764000, 4134000, 4504000, 4874000, 5244000, 5614000, 5984000,
    6354000, 6724000, 7094000, 7464000, 7834000
]

# Параметры распределения
mean = 8.463627027  # Математическое ожидание
variance = 30160442.86  # Дисперсия
std_dev = np.sqrt(variance)  # Стандартное отклонение

# Вычисляем центры интервалов
centers = [(intervals[i] + intervals[i+1]) / 2 for i in range(len(intervals) - 1)]

# Вычисляем плотность для каждого центра
densities = [norm.pdf(x, mean, std_dev) for x in centers]

# Численное интегрирование для проверки (используем метод трапеций)
integral = np.trapz(densities, centers)

# Проверка, чтобы интеграл был равен 1 (с точностью до небольшой погрешности)
if abs(integral - 1) < 1e-6:
    print(f"Интеграл плотности вероятности равен {integral:.6f}")
else:
    print(f"Интеграл плотности вероятности равен {integral:.6f}")

# Построение графика
plt.figure(figsize=(10, 6))
plt.plot(centers, densities, marker='o', linestyle='-', color='red', label="Плотность вероятности (нормальное распределение)")

# Оформление графика
plt.title("График плотности вероятности", fontsize=16)
plt.xlabel("Значения", fontsize=12)
plt.ylabel("Плотность", fontsize=12)
plt.legend(fontsize=12)
plt.grid(alpha=0.3)
plt.show()

