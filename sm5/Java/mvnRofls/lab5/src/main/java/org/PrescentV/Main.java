package org.PrescentV;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

import com.google.gson.GsonBuilder;
import org.apache.logging.log4j.*;
import com.google.gson.Gson;

public class Main {
    private static final Logger logger = LogManager.getLogger(Main.class);
    public static void main(String[] args) {
        logger.info("Programm started");
        Scanner systemIn = new Scanner(System.in);
//        DeviceList List = new DeviceList();
        ArrayList<Appliances> arrAppliances = new ArrayList<>();
        
        int amountOfAppliances = 0;

        while(true) {
            logger.info("Entered MAIN MENU");
            cleanScreen();
            menuOutput();
            int choice = readInt(systemIn);
            switch (choice) {
                case (1) :
                    logger.info("User have chosen //1");
                    makeAppliances(systemIn, arrAppliances, choice);
                    amountOfAppliances++;
                    break;
                case (2) :
                    logger.info("User have chosen //2");
                    makeAppliances(systemIn, arrAppliances, choice);
                    amountOfAppliances++;
                    break;
                case (3) :
                    logger.info("User have chosen //3");
                    makeAppliances(systemIn, arrAppliances, choice);
                    amountOfAppliances++;
                    break;
                case (4) :
                    logger.info("User have chosen //4");
                    showArrAppliances(arrAppliances, systemIn, amountOfAppliances);
                    pressEnterToContinue(systemIn);
                    break;
                case (5) :
                    logger.info("User have chosen //5");
                    amountOfAppliances = deleteAppliances(systemIn, arrAppliances, amountOfAppliances);
                    break;
                case (6) :
                    logger.info("User have chosen //6");
                    chooseDevice(systemIn, amountOfAppliances, arrAppliances);
                    break;
                case (7) :
                    logger.info("User have chosen //7");
                    writeJson(arrAppliances, systemIn);
                    break;
                case (8) :
                    logger.info("User have chosen //8");
                    arrAppliances = readJson(systemIn);
                    break;
                case (0) :
                    logger.info("Programm ended");
                    System.out.println("Работа выполнена...");
                    return;
                default :
                    logger.warn("User tried to use unexisting option");
                    break;
            }
        }
    }

    static void menuOutput() {
        logger.info("MAIN MENU showed up");
        System.out.println("================Выберите действие================");
        System.out.println("1. Создать новый класс холодильника");
        System.out.println("2. Создать новый класс стиральной машины");
        System.out.println("3. Создать новый класс посудомоечной машины");
        System.out.println("4. Вывести экземпляры классов в массиве");
        System.out.println("5. Удалить устройство из массива");
        System.out.println("6. Работа с устройствами");
        System.out.println("7. Вывести список устройств в JSON");
        System.out.println("8. Ввести список устройств из JSON");
        System.out.println("0. Выход");
        System.out.println("=================================================");
    }

    static void chooseDevice(Scanner systemIn, int amountOfAppliances, ArrayList<Appliances> arrAppliances) {
        while (true) {
            logger.info("Entered DEVICE MENU");
            showArrAppliances(arrAppliances, systemIn, amountOfAppliances);
            System.out.println("0. Выход.");
            System.out.println("\nВыберите устроство.");
            int choice = readInt(systemIn);
            choice -= 1;
            if (choice < -1 || choice >= amountOfAppliances) {
                logger.error("Wrong choice from DEVICE MENU");
                cleanScreen();
                System.out.println("Ошибка ввода!");
                pressEnterToContinue(systemIn);
                cleanScreen();
            } else if (choice == -1){
                logger.info("Exit from DEVICE MENU");
                break;
            } else {
                logger.info("Entered FUNCTIONS MENU");
                deviceFunctions(arrAppliances.get(choice), systemIn);
            }
        }
    }

    static void deviceFunctions(Appliances device, Scanner systemIn) {
        while(true) {
            if (device instanceof Freezer) {
                logger.info("Changing Freezer settings");
                cleanScreen();
                System.out.println("Холодильник: " + ((Freezer) device).getModelName() + " от компании: " + ((Freezer) device).getBrandName());
            } else if (device instanceof Washer) {
                logger.info("Changing Washer settings");
                cleanScreen();
                System.out.println("Стиральная машина: " + ((Washer) device).getModelName() + " от компании: " + ((Washer) device).getBrandName());
            } else if (device instanceof Dishwasher) {
                logger.info("Changing Dishwasher settings");
                cleanScreen();
                System.out.println("Посудомоечная машина: " + ((Dishwasher) device).getModelName() + " от компании: " + ((Dishwasher) device).getBrandName());
            }
            if (device.oi == 1) {
                device.outputCurrentStatus();
                device.outputCurrentMode();
                System.out.println("----------Выберите действие----------");
                System.out.println("1.Выключить устройство");
                System.out.println("2.Сменить режим работы");
                if (device instanceof Freezer) {
                    System.out.println("3.Имитация загрузки холодильника");
                } else if (device instanceof Washer && ((Washer)device).start == 0) {
                    System.out.println("3.Начать стирку");
                } else if (device instanceof Washer && ((Washer)device).start != 0) {
                    System.out.println("3.Синхронизировать стирку");
                } else if (device instanceof Dishwasher && ((Dishwasher)device).start == 0) {
                    System.out.println("3.Начать мойку");
                } else if (device instanceof Dishwasher && ((Dishwasher)device).start != 0) {
                    System.out.println("3.Синхронизировать мойку");
                }
                if (device instanceof Freezer) {
                    System.out.println("4.Имитация разгрузки холодильника");
                }
                System.out.println("0.Выход");
                int choice = readInt(systemIn);
                switch (choice) {
                    case 1:
                        logger.info("Device toggling");
                        device.toggle();
                        break;
                    case 2:
                        logger.info("Changing mode");
                        cleanScreen();
                        chooseMode(device, systemIn);
                        break;
                    case 3:
                        if (device instanceof Freezer) {
                            logger.info("Changing Freezer loadout");
                            cleanScreen();
                            System.out.println("Введите количество продуктов для загрузки: ");
                            int amount = readInt(systemIn);
                            ((Freezer)device).giveProducts(amount, systemIn);
                            break;
                        }
                        if (device instanceof Washer) {
                            logger.info("Cleaning imitation started");
                            ((Washer) device).startCleaning(System.currentTimeMillis()/1000, systemIn);
                        }
                        if (device instanceof Dishwasher) {
                            logger.info("Cleaning imitation started");
                            ((Dishwasher) device).startCleaning(System.currentTimeMillis()/1000, systemIn);
                        }
                        break;
                    case 4:
                        if (device instanceof Freezer) {
                            logger.info("Changin Freezer loadout");
                            cleanScreen();
                            System.out.println("Введите количество продуктов для разгрузки: ");
                            int amount = readInt(systemIn);
                            ((Freezer)device).takeProducts(amount);
                            break;
                        }
                    case 0:
                        logger.info("Exited FUNCTIONS MENU");
                        return;
                    default:
                        logger.warn("User tried to use unexisting function");
                        break;
                }
            } else if (device.oi == 0) {
                System.out.println("----------Выберите действие----------");
                System.out.println("1.Включить устройство");
                System.out.println("0.Выход");
                int choice = readInt(systemIn);
                if (choice == 1) {
                    logger.info("Device toggling");
                    device.toggle();
                } else if (choice == 0) {
                    logger.info("Exited FUNCTIONS MENU");
                    return;
                } else {
                    logger.warn("User tried to use unexisting function");
                    System.out.println("Ошибка ввода!");
                    pressEnterToContinue(systemIn);
                }
            }
        }
    }

    static void chooseMode(Appliances device, Scanner systemIn) {
        while (true) {
            cleanScreen();
            System.out.println("Выберите режим работы: ");
            if (device instanceof Freezer) {
                ((Freezer)device).outputAllModes();
                System.out.println();
                ((Freezer)device).outputCurrentMode();
                System.out.println("0.Выход");
                int choice = readInt(systemIn);
                if (choice == 0) {return;}
                ((Freezer)device).changeMode(choice);
            }
            if (device instanceof Washer) {
                ((Washer)device).outputAllModes();
                System.out.println();
                ((Washer)device).outputCurrentMode();
                System.out.println("0.Выход");
                int choice = readInt(systemIn);
                if (choice == 0) {return;}
                ((Washer)device).changeMode(choice);
            }
            if (device instanceof Dishwasher) {
                ((Dishwasher)device).outputAllModes();
                System.out.println();
                ((Dishwasher)device).outputCurrentMode();
                System.out.println("0.Выход");
                int choice = readInt(systemIn);
                if (choice == 0) {return;}
                ((Dishwasher)device).changeMode(choice);
            }

        }
    }

    static void makeAppliances(Scanner systemIn, ArrayList<Appliances> arrAppliances, int choice) {
        logger.info("Connecting new Appliance");
        cleanScreen();
        if (choice == 1) {
            System.out.println("Введите название модели: ");
            String model = systemIn.nextLine();
            System.out.println("Введите название бренда: ");
            String brand = systemIn.nextLine();
            Freezer appFreezer = new Freezer(model, brand);
            arrAppliances.add(appFreezer);
        }
        if (choice == 2) {
            System.out.println("Введите название модели: ");
            String model = systemIn.nextLine();
            System.out.println("Введите название бренда: ");
            String brand = systemIn.nextLine();
            Washer appWasher = new Washer(model, brand);
            arrAppliances.add(appWasher);
        }
        if (choice == 3) {
            System.out.println("Введите название модели: ");
            String model = systemIn.nextLine();
            System.out.println("Введите название бренда: ");
            String brand = systemIn.nextLine();
            Dishwasher appDishwasher = new Dishwasher(model, brand);
            arrAppliances.add(appDishwasher);
        }
        cleanScreen();
    }

    static void showArrAppliances(ArrayList<Appliances> arrAppliances, Scanner systemIn, int amountOfAppliances) {
        logger.info("Listing Appliances");
        cleanScreen();
        System.out.println("Количество подключенных устройств: " + amountOfAppliances);
        int i = 1;
        for (Appliances app : arrAppliances) {
            System.out.print(i + ". ");
            i++;
            System.out.println("UUID = " + app.getUUID());
            if (app instanceof Freezer) {
                System.out.println("Холодильник: " + ((Freezer) app).getModelName() + ", от компании: " + ((Freezer) app).getBrandName() + "\n");
            }
            if (app instanceof Washer) {
                System.out.println("Стиральная машина: " + ((Washer) app).getModelName() + ", от компании: " + ((Washer) app).getBrandName() + "\n");
            }
            if (app instanceof Dishwasher) {
                System.out.println("Посудомоечная машина: " + ((Dishwasher) app).getModelName() + ", от компании: " + ((Dishwasher) app).getBrandName() + "\n");
            }
        }
    }

    static int deleteAppliances(Scanner systemIn, ArrayList<Appliances> arrApliances, int amountOfAppliances) {
        logger.info("Deleting Appliance");
        if(amountOfAppliances == 0) {
            logger.error("Trying to delete unexisting device");
            cleanScreen();
            System.out.println("Ошибка. Вы не можете удалять несуществующие устройства!");
            pressEnterToContinue(systemIn);
            return amountOfAppliances;
        }

        int choice = 0;
        System.out.println("Выберите элемент массива: \n");
        showArrAppliances(arrApliances, systemIn, amountOfAppliances);
        choice = readInt(systemIn);
        if (choice < 1 || choice > amountOfAppliances) {
            logger.error("Trying to delete unexisting device");
            cleanScreen();
            System.out.println("Ошибка. Вы не можете выбрать несуществующие объекты!");
            pressEnterToContinue(systemIn);
            return amountOfAppliances;
        } else {
            logger.info("Device deleted");
            arrApliances.remove(choice-1);
            amountOfAppliances--;
        }
        return amountOfAppliances;
    }

    public static int writeJson(ArrayList<Appliances> List, Scanner systemIn) {
        try (Writer writer = new FileWriter("Out.json")) {
            Gson gson = new GsonBuilder().create();
            gson.toJson(List, writer);
            cleanScreen();
            logger.info("Json file written");
            System.out.println("Файл записан.");
            pressEnterToContinue(systemIn);
            return 0;
        } catch (IOException e) {
            logger.error("IOException catched. JSON file was not created.");
            System.out.println("IOException пойман!");
            return 1;
        }
    }

    public static ArrayList<Appliances> readJson(Scanner systemIn) {
        cleanScreen();
        System.out.println("Введите название фала JSON");
        String filename = systemIn.nextLine();
        if (!filename.contains(".json")) {
            filename += ".json";
        }
        try (FileReader reader = new FileReader(filename)) {
            Gson gson = new Gson();
            return new ArrayList<>(Arrays.asList(gson.fromJson(reader, Appliances[].class)));
        } catch (FileNotFoundException e) {
            logger.error("FileNotFoundException catched. JSON file can not be readed.");
            System.out.println("FileNotFoundException пойман!");
            return null;
        } catch (IOException e) {
            logger.error("IOException catched. JSON file can not be readed.");
            System.out.println("IOException пойман!");
            return null;
        }
    }

    static int readInt(Scanner systemIn) {
        while(true) {
            if (systemIn.hasNextInt()) {
                int choice = systemIn.nextInt();
                systemIn.nextLine();
                return choice;
            } else {
                logger.info("Wrong \"choice\" input");
                System.out.println("Ошибка ввода! Введите число.");
                systemIn.nextLine();
            }
        }
    }

    static void pressEnterToContinue(Scanner systemIn)
    {
        System.out.println("\nНажмите Enter чтобы продолжить...");
        try {
            System.in.read();
            systemIn.nextLine();
        } catch(Exception e) {
            e.getMessage();
        }
    }

    static void cleanScreen() {
        System.out.printf("\033[2J");
    }
}
