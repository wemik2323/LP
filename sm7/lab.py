import paho.mqtt.client as mqtt
import time as time

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
client = mqtt.Client(
    callback_api_version=2,
    client_id='my_beloved_client',
)
client.on_connect = on_connect
client.on_message = on_message

client.connect(broker, port, 60)

# Запуск цикла обработки сообщений
client.loop_start()

client.publish(topic, "Стирал белье на питоне?")

try:
    while True:
        time.sleep(1000)
except KeyboardInterrupt:
    client.loop_stop()
    client.disconnect()

