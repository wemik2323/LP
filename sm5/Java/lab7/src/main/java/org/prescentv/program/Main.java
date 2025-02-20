package org.prescentv.program;

import org.apache.log4j.xml.DOMConfigurator;

public class Main {
    public static void main(String[] args) {
        DOMConfigurator.configure(Main.class.getResource("log4j.xml"));
        Application.main(args);
    }
}
