package Java.lab3;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class football {
    String surname;
    String pos;
    Date birthDate;
    int gamesPlayed;
    int goalsMade;

    football() {
        this.surname = "";
        this.pos = "";
        this.birthDate = null;
        this.gamesPlayed = 0;
        this.goalsMade = 0;
    }

    football(String surname, String pos, Date birthDate, int gamesPlayed, int goalsMade) {
        this.surname = surname;
        this.pos = pos;
        this.birthDate = birthDate;
        this.gamesPlayed = gamesPlayed;
        this.goalsMade = goalsMade;
    }

    void printAll() {
        System.out.println(this.surname);
        System.out.println(this.pos);
        System.out.println(this.birthDate);
        System.out.println(this.gamesPlayed);
        System.out.println(this.goalsMade);
    }

    String getSurname() {
        return this.surname;
    }

    String getPos () {
        return this.pos;
    }

    String getBirthDate() {
        return this.birthDate.toString();
    }

    String getGamesPlayed() {
        String gamesPlayedCopy = this.gamesPlayed + "";
        return gamesPlayedCopy;
    }

    String getGoalsMade() {
        String goalsMadeCopy = this.goalsMade + "";
        return goalsMadeCopy;
    }



    int getAge () {
        int year = this.birthDate.getYear() + 1900;
        int age = 2023 - year;
        return age;
    }
}
