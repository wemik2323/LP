package org.PrescentV;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

public class Jackson
{
    protected static final Logger logger = LogManager.getLogger(Main.class);
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final String filePath;

    public Jackson(String filePath) {
        this.filePath = filePath;
    }

    public void write(ArrayList<Appliances> objects) throws IOException {
        File file = new File(filePath);

        if (!file.exists()) {
            try {
                logger.info("JSON created.");
                file.createNewFile();
            } catch (IOException exc) {
                logger.error("ERROR while creating file!");
                System.out.println("Error creating input file");
            }
        }

        objectMapper.writerWithType(new TypeReference<ArrayList<Appliances>>() {}).writeValue(file, objects);
    }

    public ArrayList<Appliances> read() throws IOException {
        logger.info("JSON readed.");
        return objectMapper.readValue(
                new File(filePath),
                objectMapper.getTypeFactory().constructCollectionType(ArrayList.class, Appliances.class)
        );
    }
}