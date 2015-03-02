/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution.test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import solution.BaseDeClauses;

/**
 *
 * @author alexis
 */
public class BaseDeClausesTest extends TestCase {
    
    Random Rand;
    
    public BaseDeClausesTest(String testName) {
        super(testName);
        Rand = new Random();
    }
    
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }
    
    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testUploadSmallFile() throws Exception {
        BufferedWriter bf = new BufferedWriter(new FileWriter("BaseDeClausesTest.tmp.txt"));
        List<String> formules = new ArrayList<String>() {{
            add("(bigand ?f in I ?f(0))");
            add("(bigand ?f in (F minus I) ~?f(0))");
            add("(bigand ?f in G ?f(!length))");
        }};
        List<String> sets = new ArrayList<String>() {{
            add("Constant !length = 10");
            add("Set F = (a;b;c;d)");
            add("Set O = (A;B)");
        }};
        bf.write("Begin sets\n");
        for(String s : sets) {
            bf.write(s+"\n");
        }
        bf.write("End sets\n");
        bf.write("Begin formula\n");
       for(String f : formules) {
           bf.write(f+"\n");
       }
       bf.write("End formula\n");
       bf.close();
       
       BaseDeClauses B = new BaseDeClauses();
       B.uploadFile("BaseDeClausesTest.tmp.txt");
       
       assertEquals(B.getFormules(),formules);
       assertEquals(B.getSets(),sets);
       
       File file = new File("BaseDeClausesTest.tmp.txt");
       file.delete();
       
    }
    
    public void testSaveSmallFile() throws Exception {
        
    }
    
    
}

