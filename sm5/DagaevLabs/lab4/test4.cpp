#include <iostream>
#include <random>
#include <chrono>
#include <string>
#include <windows.h>

HANDLE mtx;
bool programStop = false;
int secretNumber = 0;

float numbersGenerated = 0;
float numbersGuessed = 0;
float numberGuessAttempts = 0;

DWORD WINAPI generateSecretNumber(LPVOID lpParam) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 100);

    while (!programStop) {
        WaitForSingleObject(mtx, INFINITE);

        secretNumber = distr(gen);
        std::cout << "Thread 1: New secret number is generated: " << secretNumber << std::endl;
        numbersGenerated++;

        ReleaseMutex(mtx);
        Sleep(2000);
    }

    return 0;
}

DWORD WINAPI guessNumber(LPVOID lpParam) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 100);

    while (!programStop) {
        Sleep(500);

        WaitForSingleObject(mtx, INFINITE);

        int guess = distr(gen);
        std::cout << "Thread 2: Guessing " << guess << std::endl;
        numberGuessAttempts++;

        if (guess == secretNumber) {
            std::cout << "Thread 2: Guessed the number " << secretNumber << std::endl;
            numbersGuessed++;
        }

        ReleaseMutex(mtx);
    }

    return 0;
}

DWORD WINAPI userInput(LPVOID lpParam) {
    std::string input;
    while (true) {
        std::getline(std::cin, input);
        if (input == "s") {
            WaitForSingleObject(mtx, INFINITE);

            programStop = true;

            ReleaseMutex(mtx);
            break;
        }
    }

    return 0;
}

int main() {
    mtx = CreateMutex(NULL, FALSE, NULL);

    HANDLE threads[3];
    threads[0] = CreateThread(NULL, 0, generateSecretNumber, NULL, 0, NULL);
    threads[1] = CreateThread(NULL, 0, guessNumber, NULL, 0, NULL);
    threads[2] = CreateThread(NULL, 0, userInput, NULL, 0, NULL);

    WaitForMultipleObjects(3, threads, TRUE, INFINITE);

    CloseHandle(threads[0]);
    CloseHandle(threads[1]);
    CloseHandle(threads[2]);

    CloseHandle(mtx);

    std::cout << "Program finished!" << std::endl;
    std::cout << "Number of times the number was generated: " << numbersGenerated << std::endl;
    std::cout << "Number of times the number was guessed: " << numbersGuessed << std::endl;
    std::cout << "Number of times the number was attempted to guess: " << numberGuessAttempts << std::endl;
    std::cout << std::endl << "Statistics: " << std::endl;
    std::cout << "Success attempts: " << numbersGuessed / numberGuessAttempts << "freq" << " or " << (numbersGuessed / numberGuessAttempts) * 100 << "%" << std::endl;
    std::cout << "Failure attempts: " << (numberGuessAttempts - numbersGuessed) / numberGuessAttempts << "freq" << " or " << ((numberGuessAttempts - numbersGuessed) / numberGuessAttempts) * 100 << "%" << std::endl;

    return 0;
}
