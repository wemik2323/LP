package org.prescentv.program.controllers;

//import java.net.URL;
//import java.util.ResourceBundle;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import org.prescentv.program.appliances.Dishwasher;
import org.prescentv.program.appliances.Freezer;
import org.prescentv.program.appliances.Washer;
import org.prescentv.program.models.ApplianceModel;

public class AddAppController {

    ObservableList<String> elementList = FXCollections.observableArrayList("Freezer","Washer","Dishwasher");

    public static ApplianceModel appModel = null;

    @FXML
    private Button addAppButton;

    @FXML
    private TextField brandTextField;

    @FXML
    private Button canceladdAppButton;

    @FXML
    private TextField modelTextField;

    @FXML
    private ChoiceBox<String> deviceChoiceBox;

    @FXML
    void onActionAddApp() {
        Stage stage = (Stage) addAppButton.getScene().getWindow();
        switch (deviceChoiceBox.getValue()) {
            case "Freezer":
                appModel = new ApplianceModel(new Freezer(modelTextField.getText(), brandTextField.getText()));
                stage.hide();
                break;
            case "Washer":
                appModel = new ApplianceModel(new Washer(modelTextField.getText(), brandTextField.getText()));
                stage.hide();
                break;
            case "Dishwasher":
                appModel = new ApplianceModel(new Dishwasher(modelTextField.getText(), brandTextField.getText()));
                stage.hide();
                break;
            default:
                break;
        }
    }

    @FXML
    void onActionCancel() {
        appModel = null;
        Stage stage = (Stage) canceladdAppButton.getScene().getWindow();
        stage.hide();
    }

    @FXML
    void initialize() {
        deviceChoiceBox.setValue("Freezer");
        deviceChoiceBox.setItems(elementList);
        assert addAppButton != null : "fx:id=\"addAppButton\" was not injected: check your FXML file 'modalAddApp.fxml'.";
        assert brandTextField != null : "fx:id=\"brandTextField\" was not injected: check your FXML file 'modalAddApp.fxml'.";
        assert canceladdAppButton != null : "fx:id=\"canceladdAppButton\" was not injected: check your FXML file 'modalAddApp.fxml'.";
        assert modelTextField != null : "fx:id=\"modelTextField\" was not injected: check your FXML file 'modalAddApp.fxml'.";
    }

    static ApplianceModel getModel() {
        return appModel;
    }
}

