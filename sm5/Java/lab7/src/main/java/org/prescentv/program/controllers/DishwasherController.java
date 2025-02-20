package org.prescentv.program.controllers;

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
import javafx.scene.layout.Pane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import org.prescentv.program.Application;
import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Dishwasher;
import org.prescentv.program.appliances.Freezer;
import org.prescentv.program.appliances.Washer;
import org.prescentv.program.models.ApplianceModel;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.ResourceBundle;

public class DishwasherController {

    public static ObservableList<String> modeList = FXCollections.observableArrayList();
    public static ApplianceModel model;

    @FXML
    private ResourceBundle resources;

    @FXML
    private URL location;

    @FXML
    private Label brandLabel;

    @FXML
    private Label currentModeLabel;

    @FXML
    private Label currentResLabel;

    @FXML
    private Label currentStatusLabel;

    @FXML
    private Label elementLabel;

    @FXML
    private ChoiceBox<String> modeChoiceBox;

    @FXML
    private Label modelLabel;

    @FXML
    private Button toggleButton;

    @FXML
    private Button initializeCleanButton;

    @FXML
    private Label toggleLabel;

    @FXML
    void initializeCleanButton(ActionEvent event) {
        if (model.app.getOi() == 1) {
            if (((Dishwasher)model.app).getLoaded() == 0 && ((Dishwasher)model.app).getStart() == 0) {
                initializeCleanButton.setText("OUT OF RES");
                return;
            }
            if (((Dishwasher)model.app).getStatus() == 2) {
                if (((Dishwasher)model.app).startCleaning(System.currentTimeMillis()/1000) == 0) {
                    initializeCleanButton.setText("INITIALIZE CLEAN");
                    currentResLabel.setText(Integer.toString(((Dishwasher)model.app).getLoaded()));
                    String[] statuses = ((Dishwasher) model.app).getStatuses();
                    currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
                    return;
                }
            }
            if (((Dishwasher)model.app).startCleaning(System.currentTimeMillis() / 1000) == 2) {
                initializeCleanButton.setText("SYNC");
                currentResLabel.setText(Integer.toString(((Dishwasher)model.app).getLoaded()));
                String[] statuses = ((Dishwasher) model.app).getStatuses();
                currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
                return;
            }
            if (((Dishwasher) model.app).startCleaning(System.currentTimeMillis() / 1000) == 0) {
                initializeCleanButton.setText("INITIALIZE CLEAN");
                currentResLabel.setText(Integer.toString(((Dishwasher)model.app).getLoaded()));
                String[] statuses = ((Dishwasher) model.app).getStatuses();
                currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
            } else {
                initializeCleanButton.setText("SYNC");
                currentResLabel.setText(Integer.toString(((Dishwasher)model.app).getLoaded()));
                String[] statuses = ((Dishwasher) model.app).getStatuses();
                currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
            }
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
            String[] statuses = ((Dishwasher) model.app).getStatuses();
            currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
            String[] modes = ((Dishwasher) model.app).getModes();
            currentModeLabel.setText(modes[((Dishwasher) model.app).getMode()]);
            currentResLabel.setText(Integer.toString(((Dishwasher)model.app).getLoaded()));
            elementLabel.setText("Washer");
        } else {
            Runnable runnable = () -> {
                model.app.setOi(0);
            };
            Thread thread = new Thread(runnable);
            thread.start();
            toggleLabel.setText("TURNED OFF");
            currentStatusLabel.setText("");
            currentModeLabel.setText("");
            currentResLabel.setText("");
        }
    }

    public static void newWindow() throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(Application.class.getResource("dishwasherFuncs.fxml"));
        Scene scene = new Scene(fxmlLoader.load());

        Stage window = new Stage();
        window.setOnCloseRequest(event -> model.isEditing = false);
        Pane pane = new Pane();

        window.setScene(scene);
        window.setTitle("Control Dishwasher");
        window.showAndWait();
    }

    @FXML
    void initialize() {
        model = MainController.getSelectedModel();
        modeList = FXCollections.observableArrayList(Arrays.asList(((Dishwasher) model.app).getModes()));

        if (model.app.getOi() == 1) {
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Dishwasher) model.app).getStatuses();
            currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
            String[] modes = ((Dishwasher) model.app).getModes();
            currentModeLabel.setText(modes[((Dishwasher) model.app).getMode()]);
            currentResLabel.setText(Integer.toString(((Dishwasher)model.app).getLoaded()));
            modeChoiceBox.setValue(modes[((Dishwasher) model.app).getMode()]);
        } else {
            toggleLabel.setText("TURNED OFF");
            currentStatusLabel.setText("");
            currentModeLabel.setText("");
            currentResLabel.setText("");
            String[] modes = ((Dishwasher) model.app).getModes();
            modeChoiceBox.setValue(modes[0]);
        }

        elementLabel.setText("Washer");
        brandLabel.setText(model.getBrandName());
        modelLabel.setText(model.getModelName());

        modeChoiceBox.setItems(modeList);

        assert brandLabel != null : "fx:id=\"brandLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert currentModeLabel != null : "fx:id=\"currentModeLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert currentResLabel != null : "fx:id=\"currentResLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert currentStatusLabel != null : "fx:id=\"currentStatusLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert elementLabel != null : "fx:id=\"elementLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert modeChoiceBox != null : "fx:id=\"modeChoiceBox\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert modelLabel != null : "fx:id=\"modelLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert toggleButton != null : "fx:id=\"toggleButton\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert initializeCleanButton != null : "fx:id=\"toggleButton1\" was not injected: check your FXML file 'washerFuncs.fxml'.";
        assert toggleLabel != null : "fx:id=\"toggleLabel\" was not injected: check your FXML file 'washerFuncs.fxml'.";

        setModeChoiceBoxListener();

    }

    void setModeChoiceBoxListener() {
        modeChoiceBox.getSelectionModel().selectedIndexProperty().addListener(new ChangeListener<Number>() {

            @Override
            public void changed(ObservableValue<? extends Number> observableValue, Number number, Number number2) {
                if (model.app.getOi() == 1 && ((Dishwasher)model.app).getStatus() == 0) {
                    ((Dishwasher) model.app).changeMode((Integer) number2 + 1);
                    currentModeLabel.setText(modeChoiceBox.getItems().get((Integer) number2));
                    String[] statuses = ((Dishwasher) model.app).getStatuses();
                    currentStatusLabel.setText(statuses[((Dishwasher) model.app).getStatus()]);
                }
            }
        });
    }

}
