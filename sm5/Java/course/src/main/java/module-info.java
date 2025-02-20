module com.example.course {
    requires javafx.controls;
    requires javafx.fxml;
    requires org.controlsfx.controls;
    requires com.dlsc.formsfx;
    requires com.almasb.fxgl.all;
    requires org.json;
    requires java.sql;
    requires com.fasterxml.jackson.databind;
    requires org.slf4j;

    opens com.example.course to javafx.fxml;
    exports com.example.course;
    exports com.example.course.model;
    opens com.example.course.model to javafx.fxml;
    exports com.example.course.controllers;
    opens com.example.course.controllers to javafx.fxml;
}