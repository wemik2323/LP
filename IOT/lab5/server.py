import asyncio
import websockets

# Загрузка базы данных карт из текстового файла
def load_authorized_cards():
    with open("cards.txt", "r") as file:
        return set(line.strip() for line in file)

# Функция для проверки UID в базе
def check_authorization(uid, authorized_cards):
    if uid in authorized_cards:
        return "Авторизован"
    else:
        return "Доступ запрещен"

# Асинхронная функция для обработки соединения
async def handle_connection(websocket, path):
    authorized_cards = load_authorized_cards()
    async for message in websocket:
        print(f"Received UID: {message}")
        response = check_authorization(message, authorized_cards)
        print(f"Response: {response}")
        await websocket.send(response)

# Запуск WebSocket-сервера
async def main():
    async with websockets.serve(handle_connection, "0.0.0.0", 8000):
        print("Server started on port 8000")
        await asyncio.Future()  # Бесконечное ожидание для удержания сервера активным

# Запуск основного цикла событий
asyncio.run(main())

