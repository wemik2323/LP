package PrescentV;

import static org.junit.Assert.*;
import org.PrescentV.Dishwasher;
import org.PrescentV.Freezer;
import org.junit.Test;



public class MainTest 
{
@Test
public void changeMode_withInvalidChoice_shouldReturnMinusOne() {
    Dishwasher dishwasher = new Dishwasher("modelName", "brandName");

    int result = dishwasher.changeMode(1000);

    assertEquals(-1,result);

}

@Test
public void changeMode_withValidChoice_shouldReturnZero() {
    Dishwasher dishwasher = new Dishwasher("modelName", "brandName");

    int result = dishwasher.changeMode(3);

    assertEquals(0,result);

}

@Test
public void testChangeModeWithInvalidChoice() {
    Freezer freezer = new Freezer("model", "brand");
    int result = freezer.changeMode(100);

    assertEquals(-1, result);
}

@Test
public void testChangeModeWithValidChoice() {
    Freezer freezer = new Freezer("model", "brand");
    int result = freezer.changeMode(1);

    assertEquals(0, result);
}

}
