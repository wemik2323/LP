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
import org.prescentv.program.Application;
import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Freezer;

public class FreezerController extends Controller {

    public static ObservableList<String> modeList = FXCollections.observableArrayList();
    public static Appliances model;

    @FXML
    private ResourceBundle resources;

    @FXML
    private URL location;

    @FXML
    private TextField amountTextField;

    @FXML
    private Label brandLabel;

    @FXML
    private Label currentModeLabel;

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

        if (model.getOi() == 1) {
            ((Freezer) model).giveProducts(Integer.parseInt(amountTextField.getText()));
            amountTextField.setText("");
            currentLoadLabel.setText(Integer.toString(((Freezer) model).getLoaded()));
        }
    }

    @FXML
    void reduceAmount(ActionEvent event) {
        if (amountTextField.getText().isEmpty()) {
            return;
        }

        if (model.getOi() == 1) {
            ((Freezer) model).takeProducts(Integer.parseInt(amountTextField.getText()));
            amountTextField.setText("");
            currentLoadLabel.setText(Integer.toString(((Freezer) model).getLoaded()));
        }
    }

    @FXML
    void toggleDevice(ActionEvent event) {
        if (model.getOi() == 0) {
            model.setOi(1);
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Freezer) model).getStatuses();
            currentStatusLabel.setText(statuses[((Freezer) model).getStatus()]);
            String[] modes = ((Freezer) model).getModes();
            currentModeLabel.setText(modes[((Freezer) model).getMode()]);
            currentLoadLabel.setText(Integer.toString(((Freezer)model).getLoaded()));
            elementLabel.setText("Freezer");
        } else {
            model.setOi(0);
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
//        window.initModality(Modality.APPLICATION_MODAL);
        Pane pane = new Pane();

        window.setScene(scene);
        window.setTitle("Control Freezer");
        window.showAndWait();
    }
    @FXML
    void initialize() {
        model = MainController.getSelectedModel();
        modeList = FXCollections.observableArrayList(Arrays.asList(((Freezer) model).getModes()));

        if (model.getOi() == 1) {
            toggleLabel.setText("TURNED ONN");
            String[] statuses = ((Freezer) model).getStatuses();
            currentStatusLabel.setText(statuses[((Freezer) model).getStatus()]);
            String[] modes = ((Freezer) model).getModes();
            currentModeLabel.setText(modes[((Freezer) model).getMode()]);
            currentLoadLabel.setText(Integer.toString(((Freezer)model).getLoaded()));
            modeChoiceBox.setValue(modes[((Freezer) model).getMode()]);
        } else {
            toggleLabel.setText("TURNED OFF");
            currentStatusLabel.setText("");
            currentModeLabel.setText("");
            currentLoadLabel.setText("");
            String[] modes = ((Freezer) model).getModes();
            modeChoiceBox.setValue(modes[0]);
        }

        elementLabel.setText("Freezer");
        brandLabel.setText(model.getBrandName());
        modelLabel.setText(model.getModelName());

        modeChoiceBox.setItems(modeList);

        assert amountTextField != null : "fx:id=\"amountTextField\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert brandLabel != null : "fx:id=\"brandLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert currentModeLabel != null : "fx:id=\"currentModeLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert currentStatusLabel != null : "fx:id=\"currentStatusLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert elementLabel != null : "fx:id=\"elementLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert increaseAmountButton != null : "fx:id=\"increaseAmountButton\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert modeChoiceBox != null : "fx:id=\"modeChoiceBox\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert modelLabel != null : "fx:id=\"modelLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert readuceAmountButton != null : "fx:id=\"readuceAmountButton\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert toggleButton != null : "fx:id=\"toggleButton\" was not injected: check your FXML file 'freezerFuncs.fxml'.";
        assert toggleLabel != null : "fx:id=\"toggleLabel\" was not injected: check your FXML file 'freezerFuncs.fxml'.";

        numbersOnlyListener();
        setModeChoiceBoxListener();
    }

    public void setModel(Appliances model) {
        FreezerController.model = model;
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
                if (model.getOi() == 1) {
                    ((Freezer) model).changeMode((Integer) number2 + 1);
                    currentModeLabel.setText(modeChoiceBox.getItems().get((Integer) number2));
                    String[] statuses = ((Freezer) model).getStatuses();
                    currentStatusLabel.setText(statuses[((Freezer) model).getStatus()]);
                }
            }
        });
    }
}
