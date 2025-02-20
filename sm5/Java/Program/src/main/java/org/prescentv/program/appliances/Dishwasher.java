package org.prescentv.program.appliances;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.apache.log4j.Logger;

import java.io.Serializable;
import java.util.Random;

@JsonTypeInfo(use=JsonTypeInfo.Id.NAME, include=JsonTypeInfo.As.PROPERTY, property="type", visible = true)
@JsonTypeName("type")
@JsonIgnoreProperties(ignoreUnknown = true)
public class Dishwasher extends Appliances implements Serializable {
    long start;
    int loaded;
    int mode;
    int status;
    String[] statuses = {"Ожидает","Моет","Смывает"};
    String[] modes = {"Автоматическая","Обычная","Деликатная","Экспресс","Спокойной ночи","Ополаскивание"};

    private static final Logger objectsLogger = Logger.getLogger("objects");

    public Dishwasher() {

    }
    public Dishwasher(String modelName, String brandName) {
        super(modelName, brandName);
        Random rand = new Random();
        this.loaded = rand.nextInt(3);
        this.status = 0;
        Appliances.objectsLogger.info("Dishwasher device connected");
    }
    
    public void outputCurrentStatus() {
        if (this.oi == 1) {
            System.out.println("Устройство включено.");
            System.out.println("Текущий статус: " + statuses[this.status]);
            outputCurrentLoad();
        } else {
            System.out.println("Устройство выключено.");
        }
        Appliances.objectsLogger.info("CurrentStatus showed for Dishwasher");
    }

    public void outputCurrentMode() {
        System.out.println("Текущий режим: " + modes[this.mode]);
    }

    public void outputCurrentLoad() {
        System.out.println("Текущее количество ресурсов: " + this.loaded);
    }

    public void outputAllModes() {
        for (int i = 0; i < modes.length; i++) {
            System.out.println(i+1 + ". " + modes[i]);
        }
        Appliances.objectsLogger.info("AllModes showed for Dishwasher");
    }

    public int changeMode(int choice) {
        if (this.status == 0) {
            if (choice > this.modes.length || choice <= 0) {
                Appliances.objectsLogger.error("Trying to choose unexisting mode!");
                return -1;
            } else {
                Appliances.objectsLogger.info("Mode switched for Dishwasher");
                this.mode = choice-1;
                return 0;
            }
        } else {
            Appliances.objectsLogger.error("Trying to change mode while cleaning!");
            System.out.println("Устройство в работе!");
            return -1;
        }
    }

    public int startCleaning(long currentTimeSeconds) {
        if (this.loaded >= 0) {
            if (this.status == 0 && this.loaded > 0) {
                Appliances.objectsLogger.info("CLEAN IMITATION STARTED");
                this.start = System.currentTimeMillis()/1000;
                this.status = 1;
                this.loaded -= 1;
                return 2;
            } else if (currentTimeSeconds - this.start < 10) {
                Appliances.objectsLogger.info("CLEAN IMITATION SYNC");
                this.status = 2;
                return 1;
            } else if (currentTimeSeconds - this.start < 20) {
                Appliances.objectsLogger.info("CLEAN IMITATION SYNC/FINISHED");
                this.status = 0;
                this.start = 0;
                return 0;
            } else {
                Appliances.objectsLogger.error("Trying to start clean imitation without resources");
                cleanScreen();
                System.out.println("Нет ресурсов для начала стирки!");
                return -1;
            }
        } else {
            return -100;
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
