package org.prescentv.program.controllers;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import org.apache.log4j.Logger;
import org.prescentv.program.ModalWindowAddApp;
import org.prescentv.program.appliances.Appliances;
import org.prescentv.program.appliances.Dishwasher;
import org.prescentv.program.appliances.Freezer;
import org.prescentv.program.appliances.Washer;
import org.prescentv.program.models.ApplianceModel;
import org.prescentv.program.Database;

public class MainController {
    public Database db;

    public String pathToSave;

    public Button saveButton;
    public Button saveToDbButton;
    public Button dbSyncButton;
    private ObservableList<ApplianceModel> deviceList = FXCollections.observableArrayList();

    private static final Logger serdesLogger = Logger.getLogger("serdes");

    public static Appliances selectedModel;

    @FXML
    private TableView<ApplianceModel> tableViewContainer;

    @FXML
    private TableColumn<ApplianceModel, String> UUIDColumn;

    @FXML
    private TableColumn<ApplianceModel, String> elementColumn;

    @FXML
    private TableColumn<ApplianceModel, String> brandColumn;

    @FXML
    private TableColumn<ApplianceModel, String> modelColumn;

    @FXML
    private Button addAppButton;

    @FXML
    private Button controlDeviceButton;

    @FXML
    private Label nameLabel;

    @FXML
    private Button removeAppButton;

    @FXML
    void addAppliance(ActionEvent event) throws IOException {
        ModalWindowAddApp.newWindow();
        if (AddAppController.getModel() != null) {
            this.deviceList.add(AddAppController.getModel());
            tableViewContainer.setItems(this.deviceList);
        }
    }

    @FXML
    void removeAppliance() {
        TableView.TableViewSelectionModel<ApplianceModel> selectionModel = tableViewContainer.getSelectionModel();
        ObservableList<Integer> list = selectionModel.getSelectedIndices();
        Integer[] selectedIndices = new Integer[list.size()];
        selectedIndices = list.toArray(selectedIndices);

        Arrays.sort(selectedIndices);

        for(int i = selectedIndices.length-1; i>=0; i--) {
            selectionModel.clearSelection(selectedIndices[i].intValue());
            tableViewContainer.getItems().remove(selectedIndices[i].intValue());
        }
    }

    @FXML
    void initialize() {
        deserialization();

        serdesLogger.info("Program initialization started.");

        try (InputStream input = new FileInputStream("src/main/resources/org/prescentv/program/application.properties")) {
            Properties prop = new Properties();
            prop.load(input);
            this.pathToSave = prop.getProperty("pathToSave");
        } catch (IOException e) {

        }

        this.db = new Database();
        this.db.connectToDatabase();

        UUIDColumn.setCellValueFactory(new PropertyValueFactory<ApplianceModel,String>("UUID"));
        elementColumn.setCellValueFactory(new PropertyValueFactory<ApplianceModel,String>("element"));
        brandColumn.setCellValueFactory(new PropertyValueFactory<ApplianceModel, String>("brandName"));
        modelColumn.setCellValueFactory(new PropertyValueFactory<ApplianceModel, String>("modelName"));

        assert addAppButton != null : "fx:id=\"addAppButton\" was not injected: check your FXML file 'main.fxml'.";
        assert removeAppButton != null : "fx:id=\"removeAppButton\" was not injected: check your FXML file 'main.fxml'.";
        assert tableViewContainer != null : "fx:id=\"tableViewContainer\" was not injected: check your FXML file 'main.fxml'.";

        editData();
    }

    private void editData() {
        brandColumn.setCellFactory(TextFieldTableCell.<ApplianceModel>forTableColumn());
        brandColumn.setOnEditCommit(event -> {
            ApplianceModel model = event.getTableView().getItems().get(event.getTablePosition().getRow());
            model.setBrandName(event.getNewValue());
            serdesLogger.info("BRAND изменено для " + event.getTablePosition().getRow());
        });

        modelColumn.setCellFactory(TextFieldTableCell.<ApplianceModel>forTableColumn());
        modelColumn.setOnEditCommit(event -> {
            ApplianceModel model = event.getTableView().getItems().get(event.getTablePosition().getRow());
            model.setModelName(event.getNewValue());
            serdesLogger.info("MODEL изменено для " + event.getTablePosition().getRow());
        });
    }

    @FXML
    void controlDevice() throws IOException {
        Appliances model = getSelModel();

        if (model == null) {return;}
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

    static Appliances getSelectedModel() {
        return selectedModel;
    }

    Appliances getSelModel(){
        ApplianceModel model = tableViewContainer.getSelectionModel().getSelectedItem();
        if (model != null) {
            selectedModel = model.app;
            return model.app;
        }
        return null;
    }

    public void serialization() {
        try {
            ObjectOutputStream objectOutputStream = new ObjectOutputStream(new FileOutputStream("deviceListSave.txt"));
            objectOutputStream.writeObject(new ArrayList<ApplianceModel>(this.deviceList));
            serdesLogger.info("Serialization completed");
        } catch (Exception e) {
            serdesLogger.info("Serialization declined");
            System.out.println(e);
        }
    }
    void deserialization() {
        try {
            ObjectInputStream objectInputStream = new ObjectInputStream(new FileInputStream("deviceListSave.txt"));
            List<ApplianceModel> tempList = (List<ApplianceModel>) objectInputStream.readObject();
            deviceList = FXCollections.observableList(tempList);
            tableViewContainer.setItems(this.deviceList);
            serdesLogger.info("Deserialization completed");
        } catch (Exception e) {
            System.out.println(e);
            serdesLogger.info("Deserialization declined");
        }
    }

    public void saveObservableList(ActionEvent actionEvent) {
        serialization();
    }

    public void saveDatabase(ActionEvent actionEvent) {
        this.db.updateTable(this.deviceList);
    }

    public void syncDB(ActionEvent actionEvent) {
        tableViewContainer.setItems(this.deviceList = this.db.loadTable());
    }
}