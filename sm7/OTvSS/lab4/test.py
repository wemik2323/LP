import psutil
import time
import matplotlib.pyplot as plt

# Списки для хранения значений нагрузки
cpu_usage = []
ram_usage = []
timestamps = []

# Сбор данных в течение 5 минут
start_time = time.time()
while time.time() - start_time < 300:  # 5 минут
    cpu_usage.append(psutil.cpu_percent(interval=1))  # Получаем нагрузку на CPU
    ram_usage.append(psutil.virtual_memory().percent)  # Получаем нагрузку на RAM
    timestamps.append(time.time() - start_time)  # Время в секундах

# Построение графика
plt.figure(figsize=(12, 6))

# График нагрузки на CPU
plt.subplot(2, 1, 1)
plt.plot(timestamps, cpu_usage, label='CPU Usage (%)', color='blue')
plt.title('CPU and RAM Usage Over Time')
plt.ylabel('CPU Usage (%)')
plt.xlabel('Time (seconds)')
plt.grid()
plt.legend()

# График нагрузки на RAM
plt.subplot(2, 1, 2)
plt.plot(timestamps, ram_usage, label='RAM Usage (%)', color='red')
plt.ylabel('RAM Usage (%)')
plt.xlabel('Time (seconds)')
plt.grid()
plt.legend()

# Показать график
plt.tight_layout()
plt.show()
