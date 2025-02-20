import numpy as np
import matplotlib.pyplot as plt

# Параметры
T_o = 700
T_d = 10
T_B = 25       # время восстановления ТУ (часов)

# Функция для вычисления лямбд
def calculate_lambdas(T):
    lambda_12 = 1 / T_o
    lambda_14 = 1 / T  # λ14
    lambda_41 = 1 / T_d  # λ41
    lambda_45 = 1.7 / T_d  # λ45
    lambda_51 = 1 / T_B  # λ51
    
    # λ23
    lambda_23 = (T_o - (T * np.exp(-lambda_12 * T)) / (1 - np.exp(-lambda_12 * T))) ** (-1)
    
    lambda_35 = 1 / T_d  # λ35
    return lambda_12, lambda_14, lambda_41, lambda_45, lambda_51, lambda_23, lambda_35

# Функция для вычисления вероятностей
def calculate_probabilities(T):
    lambdas = calculate_lambdas(T)
    lambda_12, lambda_14, lambda_41, lambda_45, lambda_51, lambda_23, lambda_35 = lambdas

    # Вычисление вероятностей
    P1 = 1 / (1 + (lambda_12 / lambda_23) + (lambda_12 / lambda_35) + (lambda_14 / (lambda_41 + lambda_45)) + 
                  ((lambda_12 + (lambda_45 * lambda_14) / (lambda_41 + lambda_45)) / lambda_51))
    P2 = (lambda_12 / lambda_23) * P1
    P3 = (lambda_12 / lambda_35) * P1
    P4 = (lambda_14 / (lambda_41 + lambda_45)) * P1
    P5 = ((lambda_12 + (lambda_45 * lambda_14) / (lambda_41 + lambda_45)) / lambda_51) * P1

    return P1, P2, P3, P4, P5

# Исследование зависимости вероятности нахождения в работоспособном состоянии от T
T_values = np.arange(1, 1801, 20)  # Периодичность контроля ТО от 30 до 300 с шагом 30
p_operational = []

for T in T_values:
    P1, P2, P3, P4, P5 = calculate_probabilities(T)
    print(P1)
    print(P4)
    p_operational.append(P1 + P4)  # Вероятность нахождения в работоспособном состоянии

# Построение графика
plt.figure(figsize=(10, 6))
plt.plot(T_values, p_operational, marker='o', label='Вероятность нахождения в работоспособном состоянии')
plt.title('Зависимость вероятности нахождения в работоспособном состоянии от периодичности контроля ТО')
plt.xlabel('Периодичность контроля ТО (часов)')
plt.ylabel('Вероятность работоспособного состояния')
plt.grid()
plt.legend()
plt.show()

