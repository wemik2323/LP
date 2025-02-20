module org.prescentv.program {
    requires javafx.controls;
    requires javafx.fxml;

    requires org.controlsfx.controls;
    requires com.dlsc.formsfx;
    requires net.synedra.validatorfx;
    requires org.kordamp.ikonli.javafx;
    requires org.kordamp.bootstrapfx.core;
    requires com.almasb.fxgl.all;
    requires com.fasterxml.jackson.annotation;
    requires org.apache.logging.log4j;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.databind;
    requires java.sql;
    requires mysql.connector.java;
    requires log4j;

    opens org.prescentv.program to javafx.fxml;
    opens org.prescentv.program.models to javafx.base;
    exports org.prescentv.program;
    exports org.prescentv.program.controllers;
    opens org.prescentv.program.controllers to javafx.fxml;
    exports org.prescentv.program.appliances;
    opens org.prescentv.program.appliances to javafx.fxml;
}