package org.prescentv.program.controllers;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.ResourceBundle;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import org.prescentv.program.Application;
import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Freezer;
import org.prescentv.program.models.ApplianceModel;

public class FreezerController {

    public static ObservableList<String> modelist = FXCollections.observableArrayList();
    public static ApplianceModel model;
    public Label currentModeLabel;

    @FXML
    private ResourceBundle resources;

    @FXML
    private URL location;

    @FXML
    private TextField amountTextField;

    @FXML
    private Label brandLabel;

    @FXML
    private Label currentStatusLabel;

    @FXML
    private Label currentLoadLabel;

    @FXML
    private Label elementLabel;

    @FXML
    private Button increaseAmountButton;

    @FXML
    private ChoiceBox<String> modeChoiceBox;

    @FXML
    private Label modelLabel;

    @FXML
    private Button readuceAmountButton;

    @FXML
    private Button toggleButton;

    @FXML
    private Label toggleLabel;

    @FXML
    void increaseAmount(ActionEvent event) {
        if (amountTextField.getText().isEmpty()) {
            return;
        }

        if (model.app.getOi() == 1) {
            int amountOfProd = Integer.parseInt(amountTextField.getText());
            Runnable runnable = () -> {
                ((Freezer) model.app).giveProducts(amountOfProd);
            };
            Thread thread = new Thread(runnable);
            thread.start();
            amountTextField.setText("");
            currentLoadLabel.setText(Integer.toString(((Freezer) model.app).getLoaded()));
        }
    }

    @FXML
    void reduceAmount(ActionEvent event) {
        if (amountTextField.getText().isEmpty()) {
            return;
        }

        if (model.app.getOi() == 1) {
            int amountOfProd = Integer.parseInt(amountTextField.getText());
            Runnable runnable = () -> {
                ((Freezer) model.app).takeProducts(amountOfProd);
            };
            Thread thread = new Thread(runnable);
            thread.start();
            amountTextField.setText("");
            currentLoadLabel.setText(Integer.toString(((Freezer) model.app).getLoaded()));
        }
    }

    @FXML
    void toggleDevice(ActionEvent event) {
        if (model.app.getOi() == 0) {
            Runnable runnable = () -> {
                model.app.setOi(1);
            };
            Thread thread = new Thread(runnable);
            thread.start();
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Freezer) model.app).getStatuses();
            currentStatusLabel.setText(statuses[((Freezer) model.app).getStatus()]);
            String[] modes = ((Freezer) model.app).getModes();
            currentModeLabel.setText(modes[((Freezer) model.app).getMode()]);
            currentLoadLabel.setText(Integer.toString(((Freezer)model.app).getLoaded()));
            elementLabel.setText("Freezer");
        } else {
            Runnable runnable = () -> {
                model.app.setOi(0);
            };
            Thread thread = new Thread(runnable);
            thread.start();
            toggleLabel.setText("TURNED OFF");
            currentStatusLabel.setText("");
            currentModeLabel.setText("");
            currentLoadLabel.setText("");
        }
    }


    public static void newWindow() throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(Application.class.getResource("freezerFuncs.fxml"));
        Scene scene = new Scene(fxmlLoader.load());

        Stage window = new Stage();
        window.setOnCloseRequest(event -> model.isEditing = false);

        Pane pane = new Pane();

        window.setScene(scene);
        window.setTitle("Control Freezer");
        window.showAndWait();
    }
    @FXML
    void initialize() {
        model = MainController.getSelectedModel();
        modelist = FXCollections.observableArrayList(Arrays.asList(((Freezer) model.app).getModes()));

        if (model.app.getOi() == 1) {
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Freezer) model.app).getStatuses();
            currentStatusLabel.setText(statuses[((Freezer) model.app).getStatus()]);
            String[] modes = ((Freezer) model.app).getModes();
            currentModeLabel.setText(modes[((Freezer) model.app).getMode()]);
            currentLoadLabel.setText(Integer.toString(((Freezer)model.app).getLoaded()));
            modeChoiceBox.setValue(modes[((Freezer) model.app).getMode()]);
        } else {
            toggleLabel.setText("TURNED OFF");
            currentStatusLabel.setText("");
            currentModeLabel.setText("");
            currentLoadLabel.setText("");
            String[] modes = ((Freezer) model.app).getModes();
            modeChoiceBox.setValue(modes[0]);
        }

        elementLabel.setText("Freezer");
        brandLabel.setText(model.getBrandName());
        modelLabel.setText(model.getModelName());

        modeChoiceBox.setItems(modelist);

        assert amountTextField != null : "fx:id=\"amountTextField\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert brandLabel != null : "fx:id=\"brandLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert currentModeLabel != null : "fx:id=\"currentModeLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert currentStatusLabel != null : "fx:id=\"currentStatusLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert elementLabel != null : "fx:id=\"elementLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert increaseAmountButton != null : "fx:id=\"increaseAmountButton\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert modeChoiceBox != null : "fx:id=\"modeChoiceBox\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert modelLabel != null : "fx:id=\"model.appLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert readuceAmountButton != null : "fx:id=\"readuceAmountButton\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert toggleButton != null : "fx:id=\"toggleButton\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert toggleLabel != null : "fx:id=\"toggleLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";

        numbersOnlyListener();
        setModeChoiceBoxListener();
    }

    public void setmodel(Appliances model) {
        FreezerController.model.app = model;
    }

    void numbersOnlyListener() {
        amountTextField.textProperty().addListener((observable, oldValue, newValue) -> {
            if (!newValue.matches("\\d*")) {
                amountTextField.setText(newValue.replaceAll("\\D", ""));
            }
        });
    }

    void setModeChoiceBoxListener() {
        modeChoiceBox.getSelectionModel().selectedIndexProperty().addListener(new ChangeListener<Number>() {

            @Override
            public void changed(ObservableValue<? extends Number> observableValue, Number number, Number number2) {
                if (model.app.getOi() == 1) {
                    ((Freezer) model.app).changeMode((Integer) number2 + 1);
                    currentModeLabel.setText(modeChoiceBox.getItems().get((Integer) number2));
                    String[] statuses = ((Freezer) model.app).getStatuses();
                    currentStatusLabel.setText(statuses[((Freezer) model.app).getStatus()]);
                }
            }
        });
    }
}
