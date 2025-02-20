#include <iostream>
#include <windows.h>

const char *sharedMemoryName = "Local\\MatchsticksGameSharedMemory";
const char *mutexNamePlayer1 = "Local\\Player1Mutex";
const char *mutexNamePlayer2 = "Local\\Player2Mutex";

struct SharedData {
    int matchsticks;
    bool gameOver;
};

int main() {
    HANDLE sharedMemory = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, sizeof(SharedData), sharedMemoryName);
    SharedData *sharedData = static_cast<SharedData*>(MapViewOfFile(sharedMemory, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, sizeof(SharedData)));

    HANDLE mutexPlayer1 = CreateMutexA(NULL, FALSE, mutexNamePlayer1);
    HANDLE mutexPlayer2 = CreateMutexA(NULL, FALSE, mutexNamePlayer2);

    // Инициализация игры
    sharedData->matchsticks = 15;
    sharedData->gameOver = false;

    // Сигнализируем второму игроку о готовности
    ReleaseMutex(mutexPlayer1);

    // Ожидаем, пока второй игрок подключится
    std::cout << "Waiting for player2 to connect..." << std::endl;
    WaitForSingleObject(mutexPlayer2, INFINITE);

    // Начинаем игру
    while (true) {
        if (sharedData -> matchsticks == 0) {std::cout << "YOU WON!!!" << std::endl; break;}
        // Ваш код для хода первого игрока
        int player1Move;
        std::cout << "Player 1, enter your move (1-3 matchsticks): ";
        std::cin >> player1Move;

        // Проверка допустимости хода
        if (player1Move < 1 || player1Move > 3 || player1Move > sharedData->matchsticks) {
            std::cout << "Invalid move. Try again." << std::endl;
            player1Move = 0;
        }

        // Обновляем количество спичек
        sharedData->matchsticks -= player1Move;

        // Печать текущего состояния игры
        std::cout << "Matchsticks left: " << sharedData->matchsticks << std::endl;

        // Проверка условия победы/проигрыша
        if (sharedData->matchsticks == 0) {
            std::cout << "Player 2 wins!" << std::endl;
            sharedData->gameOver = true;
            break;
        }

        // Сигнализируем второму игроку о том, что ход первого игрока завершен
        ReleaseMutex(mutexPlayer1);

        // Ожидаем хода второго игрока
        std::cout << "Waiting for player2 to make a move..." << std::endl;
        WaitForSingleObject(mutexPlayer2, INFINITE);

        // Проверка условия победы/проигрыша
        if (sharedData->matchsticks == 0) {
            std::cout << "Player 2 wins!" << std::endl;
            sharedData->gameOver = true;
            break;
        }
    }

    // Закрываем мьютексы и разделяемую память
    CloseHandle(mutexPlayer1);
    CloseHandle(mutexPlayer2);
    UnmapViewOfFile(sharedData);
    CloseHandle(sharedMemory);

    return 0;
}
