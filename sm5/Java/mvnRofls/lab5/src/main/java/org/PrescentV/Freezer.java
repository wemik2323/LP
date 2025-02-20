package org.PrescentV;

import java.util.Random;
import java.util.Scanner;

public class Freezer extends Appliances {
    int loaded;
    int mode;
    int status;
    String[] statuses = {"Охлаждает","Замораживает","Размораживает"};
    String[] modes = {"Обычный","Заморозка","Разморозка"};

    public Freezer(String modelName, String brandName) {
        super(modelName, brandName);
        Random rand = new Random();
        this.loaded = rand.nextInt(20);
        logger.info("Freezer device connected");
    }

    public void outputCurrentStatus() {
        if (this.oi == 1) {
            System.out.println("Устройство включено.");
            System.out.println("Текущий статус: " + statuses[this.status]);
            outputCurrentLoad();
        } else {
            System.out.println("Устройство выключено.");
        }
        logger.info("CurrentStatus showed for Freezer");
    }

    public void outputCurrentMode() {
        System.out.println("Текущий режим: " + modes[this.mode]);
        logger.info("CurrentMode showed for Freezer");
    }

    public void outputCurrentLoad() {
        System.out.println("Текущее количество ресурсов: " + this.loaded);
        logger.info("CurrentLoad showed for Freezer");
    }

    public void outputAllModes() {
        for (int i = 0; i < modes.length; i++) {
            System.out.println(i+1 + ". " + modes[i]);
        }
        logger.info("AllModes showed for Freezer");
    }

    public int takeProducts(int amount) {
        if (amount > this.loaded) {
            logger.warn("Tried to take more products than Freezer have");
            this.loaded = 0;
            return 1;
        } else {
            logger.info("Product taken from Freezer");
            this.loaded -= amount;
            return 0;
        }
    }

    public int giveProducts(int amount, Scanner systemIn) {
        if (amount < 0) {
            logger.error("Negative amount of products!");
            cleanScreen();
            System.out.println("Ошибка ввода!");
            pressEnterToContinue(systemIn);
            return -1;
        } else {
            this.loaded += amount;
            return 0;
        }
    }

    public int changeMode(int choice) {
        if (choice > this.modes.length || choice <= 0) {
            logger.error("Trying to choose unexisting mode!");
            return -1;
        } else {
            logger.info("Mode switched for Freezer");
            this.mode = choice - 1;
            this.status = choice - 1;
            return 0;
        }
    }
}
