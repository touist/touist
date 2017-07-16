/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.TranslatorLatex;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JLabel;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

import translation.TranslationError;

/**
 *
 * @author alexis
 */
public class TranslationLatex {
    
    // Result of touistl string translation to Latex 
    private String latexFormula = "";
    private Process p;
    private List<TranslationError> errors = new ArrayList<TranslationError>();
    private boolean also_linter;
    /**
     * @param language can be "sat", "smt" or "qbf".
     */
	public TranslationLatex(String touistl, String language, boolean also_linter) throws Exception {
		this.also_linter = also_linter;
        if(touistl.length() == 0) {
            latexFormula = "";
        }
        else {
    		BufferedReader reader = new BufferedReader(new StringReader(touistl));
            try {
            	this.latexify(reader, language);
			} catch (Exception e) {
				System.err.println("latexity(): error with "+touistl);
			}
        }
    }
    
    public String getFormula(){
        return latexFormula;
    }
    
    public void saveLatex(String path) throws IOException {
        BufferedWriter out = new BufferedWriter(new FileWriter(path));
        out.write(latexFormula);
        out.close();
    }
    
    public List<TranslationError> getErrors() {
    	return errors;
    }
    
    public boolean latexify(BufferedReader reader, String language) throws IOException, InterruptedException {
		String pathtouist = touist.TouIST.getTouistBin();
		
		List<String> cmd = new ArrayList<String>();
		
		cmd.add(pathtouist);
		cmd.add("-");
		cmd.add("--latex");
		if(also_linter) cmd.add("--linter");
		if (language == "qbf") {
			cmd.add("--qbf");	
		} else if (language == "sat") {
			cmd.add("--sat");
		} else if (language == "smt") {
			cmd.add("--smt");
			cmd.add("QF_IDL");
		}
		cmd.add("--error-format");
		cmd.add("%l:%L:%b:%B: %t: %m");
		
        System.out.println("latexify(): cmd executed: "+cmd.toString());
		
        this.p = Runtime.getRuntime().exec(cmd.toArray(new String[0]));

        BufferedWriter toProcess = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
        String s = "";
        while ((s = reader.readLine())!=null) {
        	toProcess.write(s + "\n");
        }
        toProcess.flush();
        toProcess.close();
		
        int return_code = p.waitFor();
        
		BufferedReader fromProcess = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String linesStdout = "";
		while (fromProcess.ready())
			linesStdout += fromProcess.readLine() + "\n";;

		BufferedReader fromProcessErr = new BufferedReader(new InputStreamReader(
				this.p.getErrorStream()));
		String linesStdErr = "";
		while (fromProcessErr.ready()) {
			linesStdErr += fromProcessErr.readLine() + "\n";
		}
		fromProcessErr.close();
		fromProcess.close();

		errors = TranslationError.parse(linesStdErr);
		
		if(return_code == 0) {
			latexFormula = linesStdout;
		}
		return return_code == 0;
    }
}
