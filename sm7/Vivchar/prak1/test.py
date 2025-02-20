import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde

with open("data.txt", "r") as file:
    data = [float(line.strip()) for line in file]

# Заданный порог
threshold = 2000000

# Ширина пропускания для ядра (можно настроить)
bandwidth = 100000

# Оценка ядерной плотности
kde = gaussian_kde(data, bw_method=bandwidth / np.std(data))

# Генерация значений для оценки плотности
x_values = np.linspace(min(data), max(data), 1000)
density = kde(x_values)

# Вероятность P[S <= threshold]
probability = kde.integrate_box_1d(-np.inf, threshold)

# Построение графика
plt.figure(figsize=(10, 6))
plt.plot(x_values, density, label="Ядерная оценка плотности")
plt.axvline(threshold, color='red', linestyle='--', label=f"Порог = {threshold}")
plt.fill_between(x_values, 0, density, where=(x_values <= threshold), color='red', alpha=0.3, label=f"P(S ≤ {threshold}) = {probability:.4f}")
plt.title("Ядерная оценка плотности вероятности")
plt.xlabel("Значение S")
plt.ylabel("Плотность вероятности")
plt.legend()
plt.grid(alpha=0.3)
plt.show()

print(f"Вероятность P(S ≤ {threshold}): {probability:.4f}")

