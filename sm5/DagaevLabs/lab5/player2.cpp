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
    HANDLE sharedMemory = OpenFileMappingA(FILE_MAP_READ | FILE_MAP_WRITE, FALSE, sharedMemoryName);
    SharedData *sharedData = static_cast<SharedData*>(MapViewOfFile(sharedMemory, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, sizeof(SharedData)));

    HANDLE mutexPlayer1 = OpenMutexA(SYNCHRONIZE, FALSE, mutexNamePlayer1);
    HANDLE mutexPlayer2 = OpenMutexA(SYNCHRONIZE, FALSE, mutexNamePlayer2);

    // Сигнализируем первому игроку о готовности
    ReleaseMutex(mutexPlayer2);

    // Ожидаем, пока первый игрок подключится
    std::cout << "Waiting for player1 to connect..." << std::endl;
    WaitForSingleObject(mutexPlayer1, INFINITE);

    // Начинаем игру
    while (true) {
        if (sharedData -> matchsticks == 0) {std::cout << "YOU WON!!!" << std::endl; break;}
        // Ваш код для хода второго игрока
        int player2Move;
        std::cout << "Player 2, enter your move (1-3 matchsticks): ";
        std::cin >> player2Move;

        // Проверка допустимости хода
        if (player2Move < 1 || player2Move > 3 || player2Move > sharedData->matchsticks) {
            std::cout << "Invalid move. Try again." << std::endl;
            player2Move = 0;
        }

        // Обновляем количество спичек
        sharedData->matchsticks -= player2Move;

        // Печать текущего состояния игры
        std::cout << "Matchsticks left: " << sharedData->matchsticks << std::endl;

        // Проверка условия победы/проигрыша
        if (sharedData->matchsticks == 0) {
            std::cout << "Player 1 wins!" << std::endl;
            sharedData->gameOver = true;
            break;
        }

        // Сигнализируем первому игроку о том, что ход второго игрока завершен
        ReleaseMutex(mutexPlayer2);

        // Ожидаем хода первого игрока
        std::cout << "Waiting for player1 to make a move..." << std::endl;
        WaitForSingleObject(mutexPlayer1, INFINITE);

        // Проверка условия победы/проигрыша
        if (sharedData->matchsticks == 0) {
            std::cout << "Player 1 wins!" << std::endl;
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
