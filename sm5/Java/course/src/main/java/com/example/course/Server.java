package com.example.course;

import java.io.*;
import java.net.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.*;
import java.util.Properties;

import org.slf4j.*;

public class Server {

    private static final Logger mysqlLogger = LoggerFactory.getLogger("MysqlLogger");
    private static final Logger clientLogger = LoggerFactory.getLogger("ClientLogger");
    private ServerSocket serverSocket;

    public void start(int port) throws IOException {
        serverSocket = new ServerSocket(port);
        System.out.println("Сервер запущен на порту " + port);
        clientLogger.info("Server started at port" + port);
        while (true) {
            new ClientHandler(serverSocket.accept()).start();
        }
    }

    public void stop() throws IOException {
        serverSocket.close();
    }

    private static class ClientHandler extends Thread {
        private Socket clientSocket;
        private PrintWriter out;
        private BufferedReader in;

        public ClientHandler(Socket socket) {
            this.clientSocket = socket;
            clientLogger.info("New client created.");
            System.out.println("Новый клиент создан!");
        }

        public void run() {
            try {
                out = new PrintWriter(clientSocket.getOutputStream(), true);
                in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    String response = handleRequest(inputLine);
                    clientLogger.info("Client is getting respond: " + response);
                    out.println(response);
                }

                in.close();
                out.close();
                clientSocket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        private String handleRequest(String request) throws IOException {
            System.out.println("ЗАПРОС ПОЛУЧЕН!");
            clientLogger.info("Client send response!");
            if (request.contains("getDoctorName:")) {
                int doctorId = Integer.parseInt(request.replace("getDoctorName:", ""));
                return getDoctorName(doctorId);
            }
            if (request.contains("getPersonal:")) {
                String doctorName = request.replace("getPersonal:", "").trim();
                System.out.println(doctorName);
                return getPersonal(doctorName);
            }
            if (request.contains("getPatients:")) {
                String doctorName = request.replace("getPatients:", "").trim();
                System.out.println(doctorName);
                return getPatients(doctorName);
            }
            if (request.contains("createPatient:")) {
                String patientData = request.replace("createPatient:", "").trim();
                System.out.println(patientData);
                return createPatients(patientData);
            }
            return "Неизвестный запрос";
        }

        private String getPersonal(String doctorName) throws IOException {
            Properties props = new Properties();
            InputStream input = Files.newInputStream(Paths.get("src/main/resources/cfg.properties"));
            props.load(input);

            StringBuilder personalData = new StringBuilder();
            String query = "SELECT * FROM doctors WHERE name = ?";  // Подставьте имя поля, используемого для поиска

            try (Connection conn = DriverManager.getConnection(props.getProperty("dburl"), props.getProperty("dbuser"), props.getProperty("dbpass"));
                 PreparedStatement stmt = conn.prepareStatement(query)) {

                stmt.setString(1, doctorName); // Установка ID врача в запрос
                ResultSet rs = stmt.executeQuery();

                while (rs.next()) {
                    personalData.append(rs.getInt("id")).append(", ")
                            .append(rs.getString("email")).append(", ")
                            .append(rs.getString("name")).append("\n");
                }
                clientLogger.info("Client got a personal data");
            } catch (Exception e) {
                e.printStackTrace();
                mysqlLogger.error("MySQL database encountered trouble.");
                System.out.println("БД ПРОБЛЕМА!");
            }
            return personalData.toString();
        }

        private String getDoctorName(int doctorId) throws IOException {
            Properties props = new Properties();
            InputStream input = new FileInputStream("src/main/resources/cfg.properties");
            props.load(input);
            String doctorName = null;
            String query = "SELECT name FROM doctors WHERE id = ?";  // Подставьте имя поля, используемого для поиска

            try (Connection conn = DriverManager.getConnection(props.getProperty("dburl"), props.getProperty("dbuser"), props.getProperty("dbpass"));
                 PreparedStatement stmt = conn.prepareStatement(query)) {

                stmt.setInt(1, doctorId); // Установка ID врача в запрос
                ResultSet rs = stmt.executeQuery();

                if (rs.next()) {
                    doctorName = rs.getString("name"); // Получаем имя врача из результата
                }

            } catch (Exception e) {
                e.printStackTrace();
                mysqlLogger.error("MySQL database encountered trouble.");
                System.out.println("БД ПРОБЛЕМА!");
            }
            return doctorName;
        }

        private String getPatients(String doctorName) throws IOException {
            Properties props = new Properties();
            InputStream input = new FileInputStream("src/main/resources/cfg.properties");
            props.load(input);
            StringBuilder patientsData = new StringBuilder();
            String query = "SELECT * FROM patients WHERE doctor = ?";
            try (Connection conn = DriverManager.getConnection(props.getProperty("dburl"), props.getProperty("dbuser"), props.getProperty("dbpass"));
                 PreparedStatement stmt = conn.prepareStatement(query)) {

                stmt.setString(1, doctorName);
                ResultSet rs = stmt.executeQuery();

                while (rs.next()) {
                    patientsData.append(rs.getInt("id")).append(", ")
                            .append(rs.getString("name")).append(", ")
                            .append(rs.getString("diagnosis")).append("\n");
                }

                patientsData.append("$$$");
                clientLogger.info("Client got a patients data");

            } catch (SQLException e) {
                mysqlLogger.error("MySQL database encountered trouble.");
                e.printStackTrace();
            }
            return patientsData.toString();
        }

        private String createPatients(String patientData) throws IOException {
            Properties props = new Properties();
            InputStream input = new FileInputStream("src/main/resources/cfg.properties");
            props.load(input);
            String[] parts = patientData.split(","); // Разделяем по запятой: имя пациента и диагноз
            if (parts.length == 3) {
                String doctorName = parts[0].trim();
                String patientName = parts[1].trim();
                String diagnosis = parts[2].trim();

                String query = "INSERT INTO patients (name, diagnosis, doctor) VALUES (?, ?, ?)";
                mysqlLogger.info("Trying to do a statement");
                try (Connection conn = DriverManager.getConnection(props.getProperty("dburl"), props.getProperty("dbuser"), props.getProperty("dbpass"));
                     PreparedStatement stmt = conn.prepareStatement(query)) {

                    stmt.setString(1, patientName);
                    stmt.setString(2, diagnosis);
                    stmt.setString(3, doctorName);

                    int rowsAffected = stmt.executeUpdate();
                    if (rowsAffected > 0) {
                        return "Пациент успешно добавлен!";
                    } else {
                        return "Ошибка при добавлении пациента.";
                    }
                } catch (SQLException e) {
                    e.printStackTrace();
                    mysqlLogger.error("MySQL database encountered trouble");
                    return "Ошибка в базе данных при добавлении пациента.";
                }
            } else {
                mysqlLogger.warn("Wrong data for creating patient");
                return "Неверный формат данных для создания пациента";
            }
        }
    }

    public static void main(String[] args) {
        Server server = new Server();
        try {
            server.start(8081);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
