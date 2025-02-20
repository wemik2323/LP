package org.prescentv.program.appliances;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;

import java.util.Random;
import java.util.Scanner;

import org.apache.logging.log4j.*;

@JsonTypeInfo(use=JsonTypeInfo.Id.NAME, include=JsonTypeInfo.As.PROPERTY, property="type", visible = true)
@JsonSubTypes({
        @JsonSubTypes.Type(value = Freezer.class, name = "Freezer"),
        @JsonSubTypes.Type(value = Washer.class, name = "Washer"),
        @JsonSubTypes.Type(value = Dishwasher.class, name = "Dishwasher")
})
@JsonTypeName("type")
public abstract class Appliances {
    protected static final Logger objectsLogger = LogManager.getLogger(Main.class);
    private String UUID;
    protected String modelName;
    protected String brandName;
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
            objectsLogger.info("Device turned OFF");
            oi = 0;
        } else if (oi == 0) {
            objectsLogger.info("Device turned ON");
            oi = 1;
        } else {
            objectsLogger.error("DEVICE OUT OF SERVICE!");
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

    public void setUUID(String UUID) {this.UUID = UUID;}
    
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
