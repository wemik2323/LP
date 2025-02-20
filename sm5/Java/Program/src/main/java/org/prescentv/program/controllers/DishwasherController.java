package org.prescentv.program.controllers;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Label;
import javafx.scene.layout.Pane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.prescentv.program.Application;
import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Dishwasher;
import org.prescentv.program.appliances.Washer;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.ResourceBundle;

public class DishwasherController extends Controller {

    public static ObservableList<String> modeList = FXCollections.observableArrayList();
    public static Appliances model;

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
        if (model.getOi() == 1) {
            if (((Dishwasher)model).getLoaded() == 0 && ((Dishwasher)model).getStart() == 0) {
                initializeCleanButton.setText("OUT OF RES");
                return;
            }
            if (((Dishwasher)model).getStatus() == 2) {
                if (((Dishwasher)model).startCleaning(System.currentTimeMillis()/1000) == 0) {
                    initializeCleanButton.setText("INITIALIZE CLEAN");
                    currentResLabel.setText(Integer.toString(((Dishwasher)model).getLoaded()));
                    String[] statuses = ((Dishwasher) model).getStatuses();
                    currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
                    return;
                }
            }
            if (((Dishwasher)model).startCleaning(System.currentTimeMillis() / 1000) == 2) {
                initializeCleanButton.setText("SYNC");
                currentResLabel.setText(Integer.toString(((Dishwasher)model).getLoaded()));
                String[] statuses = ((Dishwasher) model).getStatuses();
                currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
                return;
            }
            if (((Dishwasher) model).startCleaning(System.currentTimeMillis() / 1000) == 0) {
                initializeCleanButton.setText("INITIALIZE CLEAN");
                currentResLabel.setText(Integer.toString(((Dishwasher)model).getLoaded()));
                String[] statuses = ((Dishwasher) model).getStatuses();
                currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
            } else {
                initializeCleanButton.setText("SYNC");
                currentResLabel.setText(Integer.toString(((Dishwasher)model).getLoaded()));
                String[] statuses = ((Dishwasher) model).getStatuses();
                currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
            }
        }
    }

    @FXML
    void toggleDevice(ActionEvent event) {
        if (model.getOi() == 0) {
            model.setOi(1);
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Dishwasher) model).getStatuses();
            currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
            String[] modes = ((Dishwasher) model).getModes();
            currentModeLabel.setText(modes[((Dishwasher) model).getMode()]);
            currentResLabel.setText(Integer.toString(((Dishwasher)model).getLoaded()));
            elementLabel.setText("Washer");
        } else {
            model.setOi(0);
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
//        window.initModality(Modality.APPLICATION_MODAL);
        Pane pane = new Pane();

        window.setScene(scene);
        window.setTitle("Control Dishwasher");
        window.showAndWait();
    }

    @FXML
    void initialize() {
        model = MainController.getSelectedModel();
        modeList = FXCollections.observableArrayList(Arrays.asList(((Dishwasher) model).getModes()));

        if (model.getOi() == 1) {
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Dishwasher) model).getStatuses();
            currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
            String[] modes = ((Dishwasher) model).getModes();
            currentModeLabel.setText(modes[((Dishwasher) model).getMode()]);
            currentResLabel.setText(Integer.toString(((Dishwasher)model).getLoaded()));
            modeChoiceBox.setValue(modes[((Dishwasher) model).getMode()]);
        } else {
            toggleLabel.setText("TURNED OFF");
            currentStatusLabel.setText("");
            currentModeLabel.setText("");
            currentResLabel.setText("");
            String[] modes = ((Dishwasher) model).getModes();
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
                if (model.getOi() == 1 && ((Dishwasher)model).getStatus() == 0) {
                    ((Dishwasher) model).changeMode((Integer) number2 + 1);
                    currentModeLabel.setText(modeChoiceBox.getItems().get((Integer) number2));
                    String[] statuses = ((Dishwasher) model).getStatuses();
                    currentStatusLabel.setText(statuses[((Dishwasher) model).getStatus()]);
                }
            }
        });
    }

}
