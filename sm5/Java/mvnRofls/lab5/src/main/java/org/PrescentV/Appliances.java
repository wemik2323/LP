package org.PrescentV;

import java.util.Random;
import java.util.Scanner;

import com.google.gson.annotations.SerializedName;
import org.apache.logging.log4j.*;

public abstract class Appliances {
    protected static final Logger logger = LogManager.getLogger(Main.class);
    @SerializedName("UUID")
    private final String UUID;
    @SerializedName("modelName")
    protected String modelName;
    @SerializedName("brandName")
    protected String brandName;
    @SerializedName("oi")
    protected int oi = 0;

    public Appliances() {
        this.UUID = null;
        this.modelName = null;
        this.brandName = null;
        this.oi = 0;
    }

    public Appliances(String UUID,String modelName, String brandName, int oi) {
        this.UUID = UUID;
        this.modelName = modelName;
        this.brandName = brandName;
        this.oi = oi;
    }
    public Appliances(String modelName, String brandName) {
        String characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        String randomString = "";
        Random rand = new Random();

        char[] id = new char[8];

        for (char c : id) {
            c = characters.charAt(rand.nextInt(characters.length()));
            randomString += c;
        }
        
        this.UUID = randomString;
        this.modelName = modelName;
        this.brandName = brandName;
        this.oi = 0;
    }

    public void toggle() {
        if (oi == 1) {
            logger.info("Device turned OFF");
            oi = 0;
        } else if (oi == 0) {
            logger.info("Device turned ON");
            oi = 1;
        } else {
            logger.error("DEVICE OUT OF SERVICE!");
        }
    }

    abstract int changeMode(int choice);
    abstract void outputCurrentMode();
    abstract void outputCurrentStatus();
    abstract void outputAllModes();

    public String getModelName() {
        return this.modelName;
    }
    public String getBrandName() {
        return this.brandName;
    }
    public int getOi() {
        return this.oi;
    }
    public String getUUID() {
        return this.UUID;
    }
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }
    public void setBrandName(String brandName) {
        this.brandName = brandName;
    }
    public void setOi(int mode) {
        this.oi = mode;
    }
    
    static void pressEnterToContinue(Scanner systemIn)
    { 
        System.out.println("\nНажмите Enter чтобы продолжить...");
        try {
            System.in.read();
            systemIn.nextLine();
        } catch(Exception e) {
            e.getMessage();
        }  
    }

    static void cleanScreen() {
        System.out.printf("\033[2J");
    }
}
