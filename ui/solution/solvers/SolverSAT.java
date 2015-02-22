/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package solution.solvers;

import solution.Model;
import translation.ResultatOcaml;

/**
 *
 * @author Skander
 */
public class SolverSAT {
    Process p;
    boolean IsFirtCompute=true;
    PrintWriter out;
    public Model computeModel(ResultatOcaml ocaml) {
        if(IsFirstCompute)
        {this.p=start(ocaml.getcheminFichierDimacs());
            IsFirstCompute=false;
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
        return br;
    }
    public Process start(String dimacsFilesPath) {
        this.p=Runtime.getRuntime().exec("java -cp .:sat4j-sat.jar Minisat "+dimacsFilesPath);
        return p;
    }
    
    public void stop() {
        out=new PrintWriter (new BufferedWriter (new OutputStreamWriter(p.getOutputStream())));
        out.println("\n0");
        out.close();
        this.p.destroy();
    }
}
