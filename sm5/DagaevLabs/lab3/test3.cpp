#include <iostream>
#include <thread>
#include <mutex>
#include <random>
#include <chrono>
#include <string>

std::mutex mtx;
bool programStop = false;
int secretNumber = 0;

float numbersGenerated = 0;
float numbersGuessed = 0;
float numberGuessAttempts = 0;

void generateSecretNumber() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 100);

    while (!programStop) {
        std::unique_lock<std::mutex> lock(mtx);
        secretNumber = distr(gen);
        std::cout << "Thread 1: New secret number is generated: " << secretNumber << std::endl;
        numbersGenerated++;
        lock.unlock();
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
}

void guessNumber() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 100);

    while (!programStop) {
        std::this_thread::sleep_for(std::chrono::milliseconds(500));

        std::unique_lock<std::mutex> lock(mtx);
        int guess = distr(gen);
        std::cout << "Thread 2: Guessing " << guess << std::endl;
        numberGuessAttempts++;

        if (guess == secretNumber) {
            std::cout << "Thread 2: Guessed the number " << secretNumber << std::endl;
            numbersGuessed++;
        }
    }
}

void userInput() {
    std::string input;
    while (true) {
        std::getline(std::cin, input);
        if (input == "s") {
            std::unique_lock<std::mutex> lock(mtx);
            programStop = true;
            break;
        }
    }
}

int main() {
    std::thread generatorThread(generateSecretNumber);
    std::thread playerThread(guessNumber);
    std::thread inputThread(userInput);

    generatorThread.join();
    playerThread.join();
    inputThread.join();

    std::cout << "Program finished!" << std::endl;
    std::cout << "Number of times the number was generated: " << numbersGenerated << std::endl;
    std::cout << "Number of times the number was guessed: " << numbersGuessed << std::endl;
    std::cout << "Number of times the number was attempted to guess: " << numberGuessAttempts << std::endl;
    std::cout << std::endl << "Statistics: " << std::endl;
    std::cout << "Success attempts: " << numbersGuessed/numberGuessAttempts << "freq" << " or " << (numbersGuessed/numberGuessAttempts)*100 << "%" << std::endl;
    std::cout << "Failure attempts: " << (numberGuessAttempts-numbersGuessed)/numberGuessAttempts << "freq" << " or " << ((numberGuessAttempts-numbersGuessed)/numberGuessAttempts)*100 << "%" << std::endl;
    return 0;
}