import asyncio
import websockets

async def websocket_client():

    # Вместо <IP> физ адресс
    uri = "ws://<IP>:81" 

    async with websockets.connect(uri) as websocket:
        await websocket.send("Привет от клиента на Python!")
        print("Сообщение отправлено серверу")

        response = await websocket.recv()
        print(f"Ответ от сервера: {response}")

asyncio.get_event_loop().run_until_complete(websocket_client())

