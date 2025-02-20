package com.example.course.controllers;

import com.example.course.model.DBModel;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.stage.Stage;

import java.io.*;
import java.net.Socket;
import com.fasterxml.jackson.databind.ObjectMapper;

public class LoginController {

    @FXML
    private TextField doctorIdField;

    @FXML
    private Label errorLabel;

    @FXML
    private void handleLogin() {
        String doctorId = doctorIdField.getText().trim();
        System.out.println("Введён ID: " + doctorId);

        if (doctorId.isEmpty() || !doctorId.matches("\\d+")) {
            errorLabel.setText("Введите корректный ID!");
            System.out.println("Некорректный ID");
            return;
        }

        try {
            String response = sendRequestToServer(doctorId);
            System.out.println("Ответ от сервера: " + response);

            if (response == null || response.isEmpty() || response.equals("null")) {
                errorLabel.setText("Нет такого сотрудника!");
                System.out.println("Сотрудник не найден");
            } else {
                DBModel model = new DBModel();
                model.doctorName = response;
                saveDoctorDataToJson(model);

                System.out.println("Открытие главного окна...");
                openMainWindow(model);

                System.out.println("Закрытие окна авторизации...");
                ((Stage) doctorIdField.getScene().getWindow()).close();
            }
        } catch (IOException e) {
            errorLabel.setText("Нет связи с сервером!");
            e.printStackTrace();
        }
    }

    private String sendRequestToServer(String doctorId) throws IOException {
        try (Socket socket = new Socket("localhost", 8081);
             PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {

            out.println("getDoctorName:" + doctorId);

            return in.readLine();
        }
    }

    private void saveDoctorDataToJson(DBModel model) throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.writeValue(new File("output/model_data.json"), model);  // Сохраняем модель в файл JSON
    }

    private void openMainWindow(DBModel model) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/com/example/course/mainApp.fxml"));
            Parent root = loader.load();

            MainController mainController = loader.getController();

            Stage stage = new Stage();
            stage.setTitle("MainApp");
            stage.setScene(new Scene(root));
            stage.show();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}