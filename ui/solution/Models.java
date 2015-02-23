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
 * @Modified by Abdel
 */
//Be careful
//Null Return means not exist.
public class Models {
    private List<Model> models;
    private int currentIndex=0;
    
    public Model getCurrentModel() {
        return models.get(currentIndex);
    }
    
    public void addModel(Model model) {
        models.add(model);
    }
    
    public void export() {
        //Seconde Increment
    }
    
    public Model next(){
        if(this.reachedEnd())
            { 
            this.currentIndex++;
            return models.get(currentIndex);
            }
        else
            return null;
    }
    
    public Model previous() {
        if(this.reachedBegin())
            {
            this.currentIndex++;
            return models.get(currentIndex);
            }
        else
            return null;
    }
    
    private boolean reachedEnd() {
        return currentIndex >= models.size()-1;
    }
    private boolean reachedBegin() {
        return currentIndex <0;
    }
}
