/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Skander
 */
public class BaseDeClauses {
    private List<String> formules;

    public BaseDeClauses() {
        formules = new ArrayList<>();
    }

    public List<String> getFormules() {
        return formules;
    }
    
    public void uploadFile(String path) {
        
    }
    
    public void addFormule(String formule) {
        formules.add(formule);
    }
    
    public String removeFormule(int index) {
        return formules.remove(index);
    }
    
    
}
