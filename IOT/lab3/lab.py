import paho.mqtt.client as mqtt

broker = "test.mosquitto.org"
port = 1883
topic = "banana"

# Callback при получении сообщения
def on_message(client, userdata, message):
    print(f"Сообщение: {message.payload.decode()} на тему: {message.topic}")

def on_connect(client, userdata, flags, rc):
    if rc == 0:
        print("Подключено к брокеру MQTT!")
        client.subscribe(topic)
    else:
        print(f"Ошибка подключения. Код: {rc}")

# Подключение к брокеру
client = mqtt.Client()
client.on_connect = on_connect
client.on_message = on_message

client.connect(broker, port, 60)

# Запуск цикла обработки сообщений
client.loop_start()

# Пример отправки сообщения
client.publish(topic, "Привет от Python клиента!")
client.publish(topic, "Стирал белье?")

# Ожидание входящих сообщений
try:
    while True:
        pass  # Можно добавить логику тут
    
except KeyboardInterrupt:
    client.loop_stop()
    client.disconnect()

