/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package translation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author Skander
 * @Modified by Abdel
 */
public class Traducteur {
    private ResultatOcaml ocaml;
    private Process p;

    /**
     * XXX Que fait cette fonction exactement ?
     *
     * @return
     */
    public Traducteur() {
	ocaml = new ResultatOcaml();
    }

    /**
     * Calls the translator/compiler to transform the ".bigand" file to
     * a ".dimacs" file (along with a "mapping" file).
     *
     * @param bigandFilePath
     *            is the name of the file that the translator/compiler is going
     *            to compute. The translation is handled by a third-part program
     *            called by an system "exec" command. The translator can return
     *            three kind of things : - A ".dimacs" file and a "mapping" file
     *            if everything goes well. The mapping file allows the user to
     *            get the original litteral name instead of the integer notation
     *            used by the ".dimacs" file. Note that it can be used after the
     *            solver gives results. - If anything goes wrong (syntax error,
     *            semantic error, wrong type...), the translator gives his error
     *            message on the "stderr" file.
     * @throws IOException
     */
    public void appelTraducteurOcaml(String bigandFilePath) throws IOException {
	// Heyy Olivier, Get Commande to Run Ocaml Program like this
	this.p = Runtime.getRuntime().exec("ocamlbuild ......");
	StringBuffer br = new StringBuffer();
	// Response will put 2Lines: first contain dimacs path & seconde TR path
	BufferedReader reader = new BufferedReader(new InputStreamReader(
		p.getInputStream()));
	String line = "";
	while ((line = reader.readLine()) != null) {
	    br.append(line + "\n");
	}
	ocaml.setDimacsFilePath(br.toString().split("\n")[0]);
	TranslatedFileR(br.toString().split("\n")[1]);
    }

    /**
     * XXX Can someone explain what this method is meant to do?
     *
     * @param tr_path
     * @throws FileNotFoundException
     * @throws IOException
     */
    private void TranslatedFileR(String tr_path) throws FileNotFoundException,
	    IOException {
	File TR = new File(tr_path);
	BufferedReader br = new BufferedReader(new FileReader(TR));
	String line = "";
	while ((line = br.readLine()) != null) {
	    ocaml.addLiteraux(Integer.parseInt(line.split(" ")[0]),
		    line.split(" ")[1]);
	}
    }

}
