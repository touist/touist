/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package entity;

/**
 *
 * @author alexis
 */
public class LexicographicalTree {
    
    private LexicographicalTree True;
    private LexicographicalTree False;
    
    String literal;
    
    public LexicographicalTree(){
        True = null;
        False = null;
    }
    
    public LexicographicalTree(String literal){
        this();
        this.literal = literal;
    }
    
    public LexicographicalTree getTrue(){
        return True;
    }
    
    public void setTrue(LexicographicalTree True){
        this.True = True;
    }
    
    public LexicographicalTree getFalse(){
        return False;
    }
    
    public void setFalse(LexicographicalTree False){
        this.False = False;
    }
    
    public void add(Model m)
    {
        LexicographicalTree parcours = this;
        for(Literal literal : m.literals) {
            if(literal.isLiteral_positivity()) {
                if(parcours.getTrue() == null) {
                    parcours.setTrue(new LexicographicalTree());
                }
                parcours = parcours.getTrue();
            }
            else {
                if(parcours.getFalse() == null) {
                    parcours.setFalse(new LexicographicalTree());
                }
                parcours = parcours.getFalse();
            }
        }        
    }
    
    public boolean contains(Model m){
        LexicographicalTree parcours = this;
        for(Literal literal : m.literals) {
            if(literal.isLiteral_positivity()) {
                if(parcours.getTrue() == null) {
                    return false;
                }
                parcours = parcours.getTrue();
            }
            else {
                if(parcours.getFalse() == null) {
                    return false;
                }
                parcours = parcours.getFalse();
            }
        }
        return true;
    }
    
}
