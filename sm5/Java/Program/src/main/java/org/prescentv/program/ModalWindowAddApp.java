package org.prescentv.program;

import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.IOException;

public class ModalWindowAddApp {

    public static void newWindow() throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(Application.class.getResource("modalAddApp.fxml"));
        Scene scene = new Scene(fxmlLoader.load());

        Stage window = new Stage();
        window.initModality(Modality.APPLICATION_MODAL);
        Pane pane = new Pane();

        window.setScene(scene);
        window.setTitle("Add");
        window.showAndWait();
    }
}
