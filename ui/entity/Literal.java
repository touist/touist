/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package entity;

/**
 *
 * @author blida
 */
public class Literal {
    private String literal;
    private boolean literal_positivity;

    public Literal(String get, boolean b) {
       literal=get;
       literal_positivity=b;
    }

    public Literal(String rawLiteral) {
        literal=rawLiteral;
    }

    public String getLiteral() {
        return literal;
    }

    public void setLiteral(String literal) {
        this.literal = literal;
    }

    public boolean isLiteral_positivity() {
        return literal_positivity;
    }

    public void setLiteral_positivity(boolean literal_positivity) {
        this.literal_positivity = literal_positivity;
    }
    
}
