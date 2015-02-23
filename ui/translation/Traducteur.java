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
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

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
     * @return
     */
<<<<<<< HEAD
    public Traducteur(){
        ocaml=new ResultatOcaml();
    }
    public void appelTraducteurOcaml() throws IOException {
            //Heyy Olivier, Get Commande to Run Ocaml Program like this
            this.p=Runtime.getRuntime().exec("ocamlbuild ......");
            StringBuffer br=new StringBuffer();
            //Response will put 2Lines: first contain dimacs path & seconde TR path
            BufferedReader reader =new BufferedReader(new InputStreamReader(p.getInputStream()));
            String line="";
            while((line=reader.readLine())!=null){
                br.append(line+"\n");
               }
        
    }

    public List<String> getTranslatedFilePath(String tr_path) throws FileNotFoundException, IOException {
        File TR=new File(tr_path);
        BufferedReader br=new BufferedReader(new FileReader(TR));
        String line="";
        while((line=br.readLine())!=null){
            ocaml.addLiteraux(Integer.parseInt(line.split(" ")[0]),line.split(" ")[1]);
        }
=======
    public boolean appelTraducteurOcaml(String bigandFilePath) {
        return false;
    }

    /**
     *
     * @return
     */
    public List<String> getTranslatedFilePath() {
>>>>>>> origin/develop
        return new ArrayList<>();
    }

}
