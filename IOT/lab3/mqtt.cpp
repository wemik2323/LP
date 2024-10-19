#include <WiFi.h>
#include <PubSubClient.h>

const char* ssid = "";  // Сюда сувать название WIFI сети
const char* password = "";         // Сюда сувать пароль WIFI сети
const char* mqttServer = "test.mosquitto.org";
const int mqttPort = 1883;
const char* mqttTopic = "banana"; // Неважно как топик будет называться главное чтоб совпадал с топиком в сервисе питона

WiFiClient espClient;
PubSubClient client(espClient);

void setup_wifi() {
  delay(10);
  Serial.println("Подключение к WiFi...");
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("");
  Serial.println("WiFi подключен");
}

void callback(char* topic, byte* message, unsigned int length) {
  Serial.print("Сообщение на тему: ");
  Serial.print(topic);
  Serial.print(". Сообщение: ");
  String messageTemp;

  for (int i = 0; i < length; i++) {
    messageTemp += (char)message[i];
  }
  Serial.println(messageTemp);
}

void reconnect() {
  if (!client.connected()) {   
	  Serial.println("Подключение к MQTT...");
    
    if (client.connect("ESP32Client")) {
      Serial.println("Подключено");
      client.subscribe(mqttTopic);
    } else {
      Serial.print("Не удалось подключиться, rc=");
      Serial.print(client.state());
      Serial.println(" Попытка снова через 5 секунд");
      delay(5000);  
    }
  }
}

void setup() {
  Serial.begin(115200);
  setup_wifi();
  client.setServer(mqttServer, mqttPort);
  client.setCallback(callback);
}

void loop() {
  if (!client.connected()) {   
	  reconnect();
  }
  client.loop();

  static unsigned long lastMsg = 0;
  unsigned long now = millis();
  if (now - lastMsg > 10000) {   
	  lastMsg = now;
    client.publish(mqttTopic, "Привет от ESP32!");
  }


