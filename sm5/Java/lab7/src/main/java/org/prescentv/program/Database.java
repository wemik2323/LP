package org.prescentv.program;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.apache.log4j.Logger;
import org.prescentv.program.models.ApplianceModel;

import java.io.FileInputStream;
import java.io.InputStream;
import java.sql.*;
import java.util.ArrayList;
import java.util.Properties;

public class Database {
    public String url;
    public String user;
    public String pass;

    private static final Logger dbLogger = Logger.getLogger("db");
    public void connectToDatabase() {
        try (InputStream input = new FileInputStream("src/main/resources/org/prescentv/program/application.properties")) {

            Properties prop = new Properties();

            // load a properties file
            prop.load(input);

            this.url = prop.getProperty("url");
            this.user = prop.getProperty("user");
            this.pass = prop.getProperty("pass");
            Connection connection = DriverManager.getConnection(this.url, this.user, this.pass);
            String query = "select * from devices";
            Statement stmt = connection.createStatement();
            ResultSet exec = stmt.executeQuery(query);
            while (exec.next()) {
                System.out.println(exec.getInt("id"));
                System.out.println(exec.getString("UUID"));
                System.out.println(exec.getString("brandName"));
                System.out.println(exec.getString("modelName"));
                System.out.println(exec.getString("element"));
            }
            dbLogger.info("Database connected successfully.");
        } catch (Exception ex) {
            ex.printStackTrace();
            dbLogger.error("Database connection error");
        }
    }

    public void updateTable(ObservableList<ApplianceModel> deviceList) {
        try {
            Connection connection = DriverManager.getConnection(this.url, this.user, this.pass);
            String query = "truncate table devices";
            Statement stmt = connection.createStatement();
            stmt.executeUpdate(query);

            for (ApplianceModel model:deviceList) {
                query = "INSERT INTO devices (UUID, brandName, modelName, element) " +
                        "VALUES ('" + model.getUUID() + "','" + model.getBrandName() + "','" + model.getModelName() + "','" + model.getElement() + "')";
                Statement state = connection.createStatement();
                state.executeUpdate(query);
            }

            dbLogger.info("Database update processed.");
        } catch(Exception e) {
            System.out.println(e);
            dbLogger.info("Database update encountered errors.");
        }
    }

    public ObservableList<ApplianceModel> loadTable() {
        try {
            Connection connection = DriverManager.getConnection(this.url, this.user, this.pass);
            String query = "select * from devices";
            Statement stmt = connection.createStatement();
            ResultSet exec = stmt.executeQuery(query);
            ArrayList<ApplianceModel> tempList = new ArrayList<ApplianceModel>();
            while (exec.next()) {
                ApplianceModel model = new ApplianceModel(exec.getString("UUID"), exec.getString("brandName"), exec.getString("modelName"), exec.getString("element"));
                model.app.setUUID(exec.getString("UUID"));
                tempList.add(model);
            }

            dbLogger.info("Database load processed.");

            return FXCollections.observableArrayList(tempList);
        } catch(Exception e) {
            System.out.println(e);
            dbLogger.error("Database load encountered errors.");
        }
        return null;
    }
}
