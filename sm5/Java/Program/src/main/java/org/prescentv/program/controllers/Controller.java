package org.prescentv.program.controllers;

import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Dishwasher;
import org.prescentv.program.appliances.Freezer;
import org.prescentv.program.appliances.Washer;

import java.io.IOException;
public class Controller {
    public void start(Appliances model) throws IOException {
        if (model instanceof Freezer) {
            FreezerController.newWindow();
        }
        if (model instanceof Washer) {
            WasherController.newWindow();
        }
        if (model instanceof Dishwasher) {
            DishwasherController.newWindow();
        }
    }
}
