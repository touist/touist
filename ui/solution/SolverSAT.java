/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import solution.Model;
import translation.ResultatOcaml;

/**
 *
 * @author Skander
 * @Modified by Abdel
 */
public class SolverSAT {
    private Process p;
    private boolean IsFirtCompute=true;
    private PrintWriter out;
    public Model computeModel(ResultatOcaml ocaml,Models all_models) throws IOException {
        if(this.IsFirtCompute)
        {this.p=start(ocaml.getDimacsFilePath());
            this.IsFirtCompute=false;
        }
        //wizz
        out=new PrintWriter (new BufferedWriter (new OutputStreamWriter(p.getOutputStream())));
        out.println("1");
        out.flush();
        out.close();
        //wizz
        StringBuffer br=new StringBuffer();
        BufferedReader reader =new BufferedReader(new InputStreamReader(p.getInputStream()));
        String line="";
        while((line=reader.readLine())!=null){
            br.append(line+"\n");
        }
        Model model=new Model();
        Parse_Model(model,ocaml,br.toString().split(" "));
        all_models.addModel(model);
        return model;
    }
    //Get Correspondance and Valuate Literal item.
    private void Parse_Model(Model model,ResultatOcaml ocaml,String[] output_Value){
        for (String Literal : output_Value){
            model.addLiteral(ocaml.getLiteraux().get(Integer.parseInt(Literal)));
        }
    }
    public Process start(String dimacsFilesPath) throws IOException {
        this.p=Runtime.getRuntime().exec("java -cp .:sat4j-sat.jar Minisat "+dimacsFilesPath);
        return p;
    }
    
    public void stop() {
        out=new PrintWriter (new BufferedWriter (new OutputStreamWriter(p.getOutputStream())));
        out.println("\n0");
        out.close();
        this.IsFirtCompute=true;
        this.p.destroy();
    }
}
