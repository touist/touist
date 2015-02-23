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
 */
public class Model {
    
    private StringBuffer br;
    public ArrayList<String> model=new ArrayList<String>();
    public Model(StringBuffer br) {
        this.br=br;
        parse_TR(br);
    }
    //Get Correspondance and Valuate Literal item.
    private void parse_TR(StringBuffer br){
        
    }
    public String toString() {
        
        return br.toString();
    }
}
