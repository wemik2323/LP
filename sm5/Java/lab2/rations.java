package Java.lab2;

import java.util.Scanner;

class rations extends product {
    String typeOfFood;
    double weightKg;

    rations() {
        cost = 0;
        mfr = null;
        typeOfFood = null;
        weightKg = 0;
    }

    rations(Scanner systemIn) {
        System.out.println("Ввод rations:");
        costInput(systemIn);
        mfrInput(systemIn);
        firstPropInput(systemIn);
        secondPropInput(systemIn);
    }

    void firstPropInput(Scanner systemIn) {
        System.out.println("Введите тип продовольствия: ");
        typeOfFood = systemIn.nextLine();
    }

    void secondPropInput(Scanner systemIn) {
        System.out.println("Введите вес продовольствия: ");
        weightKg = readDouble(systemIn);
        systemIn.nextLine();
    }

    String getFirstProp() {
        return typeOfFood;
    }

    double getSecondProp() {
        return weightKg;
    }


    void dataOutput() {
        System.out.println("\nВывод данных продовольствия:");
        System.out.println("Цена продовольствия: " + cost);
        System.out.println("Марка продовольствия: " + mfr);
        System.out.println("Тип продовольствия: " + typeOfFood);
        System.out.println("Вес продовольствия: " + weightKg);
    }

    void virtual() {
        System.out.println("Выполняется виртуальный метод класса rations");
    }
}