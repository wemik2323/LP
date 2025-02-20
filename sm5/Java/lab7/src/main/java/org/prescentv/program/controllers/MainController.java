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
import org.prescentv.program.appliances.*;
import org.prescentv.program.models.ApplianceModel;
import org.prescentv.program.Database;

public class MainController {
    public Database db;

    public String pathToSave;

    public Button saveButton;
    public Button saveToDbButton;
    public Button dbSyncButton;
    private static ObservableList<ApplianceModel> deviceList = FXCollections.observableArrayList();

    private static final Logger serdesLogger = Logger.getLogger("serdes");

    public static ApplianceModel selectedModel;

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
//        for (ApplianceModel model:deviceList) {
//            System.out.println("UUID - " + model.getUUID());
//            System.out.println("ELEMENT - " + model.getElement());
//            System.out.println("BRAND - " + model.getBrandName());
//            System.out.println("MODEL - " + model.getModelName());
//        }
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
    void initialize() throws IOException {
        jsonLoad();
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

    private void jsonLoad() {
        Jackson jackson = new Jackson("out.json");
        ArrayList<Appliances> jsonList = null;
        try {
            jsonList = jackson.read();
        } catch (IOException e) {
            System.out.println("File not Found exception");
            return;
        }
        for (Appliances entry: jsonList) {
            deviceList.add(new ApplianceModel(entry));
        }
    }

    public static void jsonSave() throws IOException {
        Jackson jackson = new Jackson("out.json");
        ArrayList<Appliances> jsonList = new ArrayList<>();
        for (ApplianceModel entry: deviceList) {
            jsonList.add(entry.app);
        }
        jackson.write(jsonList);
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
        ApplianceModel model = getSelModel();

        if (model == null || model.isEditing) {return;}
        if (model.app instanceof Freezer) {
            model.isEditing = true;
            FreezerController.newWindow();
        }
        if (model.app instanceof Washer) {
            model.isEditing = true;
            WasherController.newWindow();
        }
        if (model.app instanceof Dishwasher) {
            model.isEditing = true;
            DishwasherController.newWindow();
        }
        serdesLogger.info("Controlling devices");
    }

    static ApplianceModel getSelectedModel() {
        return selectedModel;
    }

    ApplianceModel getSelModel(){
        ApplianceModel model = tableViewContainer.getSelectionModel().getSelectedItem();
        if (model != null) {
            selectedModel = model;
            return model;
        }
        return null;
    }

    public void serialization() {
        Runnable runnable = () -> {
            try {
                ObjectOutputStream objectOutputStream = new ObjectOutputStream(new FileOutputStream("deviceListSave.txt"));
                objectOutputStream.writeObject(new ArrayList<ApplianceModel>(this.deviceList));
                serdesLogger.info("Serialization completed");
            } catch (Exception e) {
                serdesLogger.info("Serialization declined");
                System.out.println(e);
            }
        };
        Thread thread = new Thread(runnable);
        thread.start();
    }
    void deserialization() {
        Runnable runnable = () -> {
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
        };
        Thread thread = new Thread(runnable);
        thread.start();
    }

    public void saveObservableList(ActionEvent actionEvent) {
        serialization();
    }

    public void saveDatabase(ActionEvent actionEvent) {
        Runnable runnable = () -> {
            this.db.updateTable(this.deviceList);
        };
        Thread thread = new Thread(runnable);
        thread.start();
    }

    public void syncDB(ActionEvent actionEvent) {
        Runnable runnable = () -> {
            tableViewContainer.setItems(this.deviceList = this.db.loadTable());
        };
        Thread thread = new Thread(runnable);
        thread.start();
    }
}