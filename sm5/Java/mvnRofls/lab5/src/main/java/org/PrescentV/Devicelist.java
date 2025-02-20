package org.PrescentV;

import com.google.gson.annotations.SerializedName;

import java.util.ArrayList;
public class DeviceList {
    @SerializedName("arrAppliances")
    public ArrayList<Appliances> arrAppliances;

    public DeviceList() {
        this.arrAppliances = new ArrayList<Appliances>();
    }

    public DeviceList(ArrayList<Appliances> arrAppliances) {
        this.arrAppliances = arrAppliances;
    }
}
