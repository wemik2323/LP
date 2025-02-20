package org.prescentv.program.models;

import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Dishwasher;
import org.prescentv.program.appliances.Freezer;
import org.prescentv.program.appliances.Washer;

import java.io.Serializable;
import java.util.Objects;

public class ApplianceModel implements Serializable {
    String UUID;
    String brandName;
    String modelName;
    String element;

    public Appliances app;
    public ApplianceModel(Appliances app) {
        if (app instanceof Freezer) {
            this.element = "Freezer";
        }
        if (app instanceof Washer) {
            this.element = "Washer";
        }
        if (app instanceof Dishwasher) {
            this.element = "Dishwasher";
        }
        this.app = app;
        this.UUID = app.getUUID();
        this.brandName = app.getBrandName();
        this.modelName = app.getModelName();
    }
    public ApplianceModel(String UUID, String brandName, String modelName, String element) {
        if (Objects.equals(element, "Freezer")) {
            this.element = "Freezer";
            this.app = new Freezer(brandName, modelName);
        }
        if (Objects.equals(element, "Washer")) {
            this.element = "Washer";
            this.app = new Washer(brandName, modelName);
        }
        if (Objects.equals(element, "Dishwasher")) {
            this.element = "Dishwasher";
            this.app = new Dishwasher(brandName, modelName);
        }

        this.UUID = UUID;
        this.brandName = brandName;
        this.modelName = modelName;
    }

    public String getUUID() {
        return this.UUID;
    }
    public String getModelName() {
        return this.modelName;
    }
    public String getBrandName() {
        return brandName;
    }
    public String getElement() {
        return element;
    }

    public void setUUID(String UUID) {
        this.UUID = UUID;
    }
    public void setBrandName(String brandName) {
        this.brandName = brandName;
    }
    public void setModelName (String modelName) {
        this.modelName = modelName;
    }
    public void setElement(String element) {
        this.element = element;
    }
}
