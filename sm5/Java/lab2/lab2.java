package Java.lab2;

import java.util.ArrayList;
import java.util.Scanner;

public class lab2 {
    
    public static void main(String[] args) {
        Scanner systemIn = new Scanner(System.in);
        // Scanner systemInStrings = new Scanner(System.in);
        ArrayList<product> arrProducts = new ArrayList<>();
        int amountOfProducts = 0;
        
        while(true) {
            menuOutput();
            int choice = readInt(systemIn);
            switch (choice) {
                case (1) :
                    makeProdRations(systemIn, arrProducts);
                    amountOfProducts++;
                    break;
                case (2) :
                    makeProdClothing(systemIn, arrProducts);
                    amountOfProducts++;
                    break;
                case (3) :
                    for (product p : arrProducts) {
                        p.dataOutput();
                        p.virtual();
                    }
                    System.out.println("Размер массива - " + amountOfProducts);
                    break;
                case (4) :
                    int flag = -1;
                    flag = deleteProduct(systemIn, arrProducts, amountOfProducts);
                    if (flag != -1) {arrProducts.remove(flag); amountOfProducts--;}
                    break;
                case (5) :
                    menuChangeProps(systemIn, arrProducts, amountOfProducts);
                    break;
                case (6) :
                    double sum = 0;
                    for (product p : arrProducts) {
                        sum += p.cost;
                    }
                    System.out.println("Сумма покупок: " + sum);
                    break;
                case (0) :
                    System.out.println("Работа выполнена...");
                    return;
            }
        }
    }

    static void menuOutput() {
        System.out.println("================Выберите действие================");
        System.out.println("1. Создать новый класс товара");
        System.out.println("2. Создать новый класс одежды");
        System.out.println("3. Вывести экземпляры классов в массиве");
        System.out.println("4. Удалить продукт из массива");
        System.out.println("5. Изменить свойства объекта в массиве");
        System.out.println("6. Вычислить сумму покупки");
        System.out.println("0. Выход");
        System.out.println("=================================================");
    }

    static void makeProdRations(Scanner systemIn, ArrayList<product> arrProducts) {
        rations prodRations = new rations(systemIn);
        arrProducts.add(prodRations);
    }

    static void makeProdClothing(Scanner systemIn, ArrayList<product> arrProducts) {
        clothing prodClothing = new clothing(systemIn);
        arrProducts.add(prodClothing);
    }

    static int deleteProduct(Scanner systemIn, ArrayList<product> arrProducts, int amountOfProducts) {
        if(amountOfProducts == 0) {
            System.out.println("Ошибка. Вы не можете изменять свойства несуществующих объектов!");
            return -1;
        }

        System.out.println("Выберите элемент массива ");
        for (int i = 0; i < amountOfProducts; i++) {
            if (arrProducts.get(i) instanceof clothing) {
                System.out.println(i+1 + " - Продукт типа clothing");
            }
            if (arrProducts.get(i) instanceof rations) {
                System.out.println(i+1 + " - Продукт типа rations");
            }
        }

        int choice = 0;
        choice = readInt(systemIn);
        if (choice < 0 || choice > amountOfProducts) {
            System.out.println("Ошибка. Вы не можете выбрать несуществующие объекты!");
            return -1;
        }
        return choice-1;
    }

    static void menuChangeProps(Scanner systemIn, ArrayList<product> arrProducts, int amountOfProducts) {
        if(amountOfProducts == 0) {
            System.out.println("Ошибка. Вы не можете изменять свойства несуществующих объектов!");
            return;
        }

        System.out.println("Выберите элемент массива 1 - " + amountOfProducts);
        int choice = readInt(systemIn);
        if (choice > amountOfProducts || choice < 0) {
            System.out.println("Ошибка выбора!");
            return;
        }

        if (arrProducts.get(choice - 1) instanceof rations) {
            System.out.println("Какое свойство объекта вы хотите изменить?");
            System.out.println("1. Стоимость");
            System.out.println("2. Марка");
            System.out.println("3. Тип продовольствия");
            System.out.println("4. Вес продовольствия");
            int choiceProp = readInt(systemIn);
            switch (choiceProp) {
                case (1) :
                    arrProducts.get(choice - 1).costInput(systemIn);
                    break;
                case (2) :
                    arrProducts.get(choice - 1).mfrInput(systemIn);
                    break;
                case (3) :
                    arrProducts.get(choice - 1).firstPropInput(systemIn);
                    break;
                case (4) :
                    arrProducts.get(choice - 1).secondPropInput(systemIn);
                    break;
            }
        }
        else if (arrProducts.get(choice - 1) instanceof clothing) {
            System.out.println("Какое свойство объекта вы хотите изменить?");
            System.out.println("1. Стоимость");
            System.out.println("2. Марка");
            System.out.println("3. Тип одежды");
            System.out.println("4. Цвет одежды");
            int choiceProp = readInt(systemIn);
            switch (choiceProp) {
                case (1) :
                    arrProducts.get(choice - 1).costInput(systemIn);
                    break;
                case (2) :
                    arrProducts.get(choice - 1).mfrInput(systemIn);
                    break;
                case (3) :
                    arrProducts.get(choice - 1).firstPropInput(systemIn);
                    break;
                case (4) :
                    arrProducts.get(choice - 1).secondPropInput(systemIn);
                    break;
            }
        }
    }

    static int readInt(Scanner systemIn) {
        while(true) {
            if (systemIn.hasNextInt()) {
                return systemIn.nextInt();
            } else {
                System.out.println("Ошибка ввода! Введите число.");
                systemIn.nextLine();
            }
        }
    }
}