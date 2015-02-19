/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.util.List;

/**
 *
 * @author Skander
 */
public class Models {
    private List<Model> models;
    private int currentIndex;
    
    public Model getCurrentModel() {
        return models.get(currentIndex);
    }
    
    public void addModel(Model model) {
        models.add(model);
    }
    
    public void export() {
        
    }
    
    public void next(){
        
    }
    
    public void previous() {
        
    }
    
    public boolean reachedEnd() {
        return currentIndex >= models.size()-1;
    }
}
