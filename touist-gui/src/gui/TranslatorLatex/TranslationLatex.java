/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.TranslatorLatex;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import javax.swing.JFrame;
import javax.swing.JLabel;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

/**
 *
 * @author alexis
 */
public class TranslationLatex {
    
    // Result of touistl string translation to Lat  ex 
    private String latexFormula;
    
    
    
    public TranslationLatex(String touistl) throws Exception {
        parser p = new parser(new Lexi(new StringReader(touistl)));
        latexFormula = p.parse().value.toString();
    }
    
    public String getFormula(){
        return latexFormula;
    }
    
    public void saveLatex(String path) throws IOException {
        BufferedWriter out = new BufferedWriter(new FileWriter(path));
        out.write(latexFormula);
        out.close();
    }
    
    
    public static void main(String args[]){
        BufferedReader in = null ;
        try {
            in = new BufferedReader(new FileReader("touist-translator/test/latex.touistl"));
            StringBuilder sb = new StringBuilder();
            String line;
            while((line = in.readLine()) != null) {
                sb.append(line+"\n");
            }
            TranslationLatex T = new TranslationLatex(sb.toString());
            TeXFormula formula = new TeXFormula(T.getFormula());
            TeXIcon ti = formula.createTeXIcon(TeXConstants.ALIGN_CENTER, 20);
            JLabel label = new JLabel("",ti,JLabel.CENTER);
            JFrame frame = new JFrame();
            frame.setSize(1024,1024);
            frame.add(label);
            frame.setVisible(true);
        }
        catch (IOException e) {
            System.err.println("Erreur ouverture du fichier test");
        }
        catch (Exception e) {
            System.err.println("Erreur lors de la traduction");
        }
        
        
    }
}
