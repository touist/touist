/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.ArrayList;

/**
 *
 * @author Skander
 * @Modified By Abdel
 */
public class Model {
    
    //private StringBuffer br;
    public ArrayList<String> Literals_T=new ArrayList<String>();
    public Model() {
        
    }
    //Get Correspondance and Valuate Literal item.
    private void addLiteral(String value_T){
        Literals_T.add(value_T);
    }
    public String toString() {
        return "";
    }
}
