package com.example.course.controllers;

import com.example.course.model.DBModel;
import com.example.course.model.Patient;
import com.fasterxml.jackson.databind.ObjectMapper;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;

import java.io.*;
import java.lang.reflect.Array;
import java.net.*;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;

import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

public class MainController {

    public Label emptyspaceLabel;
    public Label nameLabel;
    public Label emailLabel;
    public Label idLabel;
    public AnchorPane mainAnchorPane;
    public Label patientNameLabel;
    public Label diagnosisLabel;
    public TextField nameTextField;
    public TextField diagnosisTextField;
    public Button addStatementButton;
    private DBModel model = new DBModel();

    @FXML
    private TableView<Patient> patientsTable;
    @FXML
    private TableColumn<Patient, Integer> idColumn;
    @FXML
    private TableColumn<Patient, String> nameColumn;
    @FXML
    private TableColumn<Patient, String> diagnosisColumn;

    @FXML
    private StackPane mainStackPane;

    @FXML
    protected void showPersonal() throws Exception {
        mainStackPane.getChildren().clear();
        emptyspaceLabel.setText("Личный кабинет");

        String[] doctorData = getPersonalData().split(",");

        if (doctorData.length == 3) {
            String id = doctorData[0];
            String email = doctorData[1];
            String name = doctorData[2];

            System.out.println(id);
            System.out.println(email);
            System.out.println(name);

            idLabel.setText(id);
            nameLabel.setText(name);
            emailLabel.setText(email);

            idLabel.setVisible(true);
            nameLabel.setVisible(true);
            emailLabel.setVisible(true);

            mainStackPane.getChildren().addAll(idLabel, nameLabel, emailLabel);
        }
    }

    @FXML
    protected void showPatients() throws IOException {
        mainStackPane.getChildren().clear();
        emptyspaceLabel.setText("Пациенты");
        patientsTable.setVisible(true);

        idColumn.setCellValueFactory(new PropertyValueFactory<>("Id"));
        nameColumn.setCellValueFactory(new PropertyValueFactory<>("Name"));
        diagnosisColumn.setCellValueFactory(new PropertyValueFactory<>("Diagnosis"));

        loadPatientsData();

        mainStackPane.getChildren().add(patientsTable);
    }

    @FXML
    protected void makeStatement() {
        mainStackPane.getChildren().clear();
        emptyspaceLabel.setText("Создать запись");

        mainAnchorPane.setVisible(true);
        patientNameLabel.setVisible(true);
        diagnosisLabel.setVisible(true);
        nameTextField.setVisible(true);
        nameTextField.setPromptText("Введите имя пациента");
        diagnosisTextField.setVisible(true);
        diagnosisTextField.setPromptText("Введите имя пациента");
        addStatementButton.setVisible(true);

        mainStackPane.getChildren().add(mainAnchorPane);
    }

    private void loadPatientsData() throws IOException {
        ObservableList<Patient> patients = FXCollections.observableArrayList(getPatients(model.doctorName));
        patientsTable.setItems(patients);
    }

    public void initialize() throws Exception {
        loadDoctorDataFromJson();
        displayWelcomeMessage();
    }


    public void loadDoctorDataFromJson() throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        model = objectMapper.readValue(new File("output/model_data.json"), DBModel.class);  // Читаем данные из файла JSON
    }
    private void displayWelcomeMessage() {
        String doctorName = model.doctorName;
        String currentTime = getCurrentTime();

        Label welcomeLabel = new Label("Приветствуем вас, " + doctorName + "!\nПоследнее время посещения: " + currentTime);
        mainStackPane.getChildren().add(welcomeLabel);
    }

    private String getCurrentTime() {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");
        return LocalDateTime.now().format(formatter);
    }
    private void getDoctorName(int doctorId) throws Exception {
        String name = null;
        try (Socket socket = new Socket("localhost", 8081);
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {

            out.println("getDoctorName:" + doctorId);
            model.doctorName = in.readLine();

        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Запрос не выслан!");
        }
    }

    private String getPersonalData() throws Exception {
        try (Socket socket = new Socket("localhost", 8081);
             PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {

            out.println("getPersonal:" + model.doctorName);

            System.out.println("Личная информация получена!");
//            System.out.println(in.readLine());

            return in.readLine();
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Запрос не выслан!");
            return null;
        }
    }

    private ArrayList<Patient> getPatients(String doctorName) throws IOException {
        try (Socket socket = new Socket("localhost", 8081);
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {

            socket.setSoTimeout(5000);

            out.println("getPatients:" + model.doctorName);

            String patientData;
            ArrayList<Patient> patients = (new ArrayList<Patient>());

            while (!(patientData = in.readLine()).contains("$$$")) {
                System.out.println("Данные пациентов обрабатываются");

                if (patientData.contains("Неизвестный запрос")) { return patients; }

                String[] data = patientData.split(", ");

                System.out.println("Received patient data: " + patientData); // Логирование для отладки

                if (data.length >= 3) {
                    int id = Integer.parseInt(data[0]);
                    String name = data[1];
                    String diagnosis = data[2];
                    patients.add(new Patient(id, name, diagnosis));
                }
            }
            return patients;
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Пациентов не получили");
            return null;
        }
    }

    public void addStatement(ActionEvent actionEvent) {
        String patientName = nameTextField.getText();
        String diagnosis = diagnosisTextField.getText();

        System.out.println("Добавление записи!");
        System.out.println("Имя пациента: " + patientName);
        System.out.println("Диагноз: " + diagnosis);

        if (patientName.isEmpty() || diagnosis.isEmpty()) {
            System.out.println("Поля не могут быть пустыми");
            return;
        }

        try (Socket socket = new Socket("localhost", 8081);
             PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {

            out.println("createPatient:" + model.doctorName + "," + patientName + "," + diagnosis);

            String response = in.readLine();
            System.out.println("Ответ от сервера: " + response);

            showPatients();

        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Ошибка при создании записи!");
        }
    }
}