import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Scanner;

class lab1 {    
    public static void main(String args[]) {
        double[] array1 = new double[6];
        double[] array2 = new double[4];
        double[] array3 = new double[3];

        ArrayList<Double> arr1 = new ArrayList<Double>();
        ArrayList<Double> arr2 = new ArrayList<Double>();
        LinkedList<Double> arr3 = new LinkedList<Double>();

        Scanner systemIn = new Scanner(System.in);

// Задание 1
        task1(systemIn, array1);
        task1_arr_out(array1);
        task1(systemIn, array2);
        task1_arr_out(array2);
        task1(systemIn, array3);
        task1_arr_out(array3);
        
// Задание 2
        task2_ArrList(systemIn, arr1, 6);
        task2_ArrList_out(arr1);
        task2_ArrList(systemIn, arr2, 4);
        task2_ArrList_out(arr2);
        task2_LinkList(systemIn, arr3, 3);
        task2_LinkList_out(arr3);
    }

    static void task1(Scanner systemIn, double[] array) {
        double sum = 0;
        System.out.println("task1 - in progress\nВведите элементы массива:");
        for(int i = 0; i < Array.getLength(array); ++i) {
            System.out.print(i + " - ");
            array[i] = readDouble(systemIn);
            if (array[i] < 0) {array[i] = 10; continue;}
            sum += array[i];
        }

        System.out.println("Сумма положительных элементов заданного массива = " + sum);
    }

   
    static void task2_ArrList(Scanner systemIn, ArrayList<Double> arr, int size) {
        double sum = 0;
        System.out.println("task2 - in progress\nВведите элементы массива:");
        for(int i = 0; i < size; i++) {
            System.out.print(i + " - ");
            double newDouble = readDouble(systemIn);
            arr.add(newDouble);
            if (newDouble < 0) {
                newDouble = 10;
                arr.set(i, newDouble);
                continue;
            }
            sum += newDouble;
        }

        System.out.println("Сумма положительных элементов заданного массива = " + sum);
    }

    static void task2_LinkList(Scanner systemIn, LinkedList<Double> arr, int size) {
        double sum = 0;
        System.out.println("task2 - in progress\nВведите элементы массива:");
        for(int i = 0; i < size; i++) {
            System.out.print(i + " - ");
            double newDouble = readDouble(systemIn);
            arr.add(newDouble);
            if (newDouble < 0) {
                newDouble = 10;
                arr.set(i, newDouble);
                continue;
            }
            sum += newDouble;
        }

        System.out.println("Сумма положительных элементов заданного массива = " + sum);
    }

    static double readDouble(Scanner systemIn) {
        while(true) {
            if (systemIn.hasNextDouble()) {
                return systemIn.nextDouble();
            } else {
                System.out.println("Ошибка ввода! Введите число.");
                systemIn.nextLine();
            }
        }
    }

    static void task1_arr_out(double[] arr) {
        System.out.println(Arrays.toString(arr));
    }

    static void task2_LinkList_out(LinkedList<Double> arr) {
        System.out.println(arr);
    }

    static void task2_ArrList_out(ArrayList<Double> arr) {
        System.out.println(arr);
    }
}