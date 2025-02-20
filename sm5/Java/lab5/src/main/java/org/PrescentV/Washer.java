package org.PrescentV;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;

import java.util.Random;
import java.util.Scanner;

@JsonTypeInfo(use=JsonTypeInfo.Id.NAME, include=JsonTypeInfo.As.PROPERTY, property="type", visible = true)
@JsonTypeName("type")
@JsonIgnoreProperties(ignoreUnknown = true)
public class Washer extends Appliances {
    long start;
    int loaded;
    int mode;
    int status;
    String[] statuses = {"Ожидает","Моет","Отжимает"};
    String[] modes = {"Смешанная","Детская","Спортивная одежда","Спортивная обувь","Быстрая"};

    public Washer() {

    }
    public Washer(String modelName, String brandName) {
        super(modelName, brandName);
        Random rand = new Random();
        this.loaded = rand.nextInt(3);
        this.status = 0;
        logger.info("Washer device connected");
    }

    public void outputCurrentStatus() {
        if (this.oi == 1) {
            System.out.println("Устройство включено.");
            System.out.println("Текущий статус: " + statuses[this.status]);
            outputCurrentLoad();
        } else {
            System.out.println("Устройство выключено.");
        }
        logger.info("CurrentStatus showed for Washer");
    }

    public void outputCurrentMode() {
        System.out.println("Текущий режим: " + modes[this.mode]);
        logger.info("CurrentMode showed for Washer");
    }

    public void outputCurrentLoad() {
        System.out.println("Текущее количество ресурсов: " + this.loaded);
        logger.info("CurrentLoad showed for Washer");
    }

    public void outputAllModes() {
        for (int i = 0; i < modes.length; i++) {
            System.out.println(i+1 + ". " + modes[i]);
        }
        logger.info("AllModes showed for Washer");
    }
    
    public int changeMode(int choice) {
        if (this.status == 0) {
            if (choice > this.modes.length || choice <= 0) {
                logger.error("Trying to choose unexisting mode!");
                return -1;
            } else {
                logger.info("Mode switched for Washer");
                this.mode = choice-1;
                return 0;
            }
        } else {
            logger.error("Trying to change mode while cleaning!");
            System.out.println("Устройство в работе!");
            return -1;
        }
    }

    public void startCleaning(long currentTimeSeconds, Scanner systemIn) {
        if (this.loaded >= 0) {
            if (this.status == 0 && this.loaded > 0) {
                logger.info("CLEAN IMITATION STARTED");
                this.start = System.currentTimeMillis()/1000;
                this.status = 1;
                this.loaded -= 1;
            } else if (currentTimeSeconds - this.start < 10) {
                logger.info("CLEAN IMITATION SYNC");
                this.status = 2;
            } else if (currentTimeSeconds - this.start < 20) {
                logger.info("CLEAN IMITATION SYNC/FINISHED");
                this.status = 0;
                this.start = 0;
            } else {
                logger.error("Trying to start clean imitation without resources");
                cleanScreen();
                System.out.println("Нет ресурсов для начала стирки!");
                pressEnterToContinue(systemIn);
            }
        }
    }

    public long getStart() {
        return start;
    }
    public int getLoaded() {
        return loaded;
    }
    public int getMode() {
        return mode;
    }
    public int getStatus() {
        return status;
    }
    public String[] getStatuses() {
        return statuses;
    }
    public String[] getModes() {
        return modes;
    }
}
