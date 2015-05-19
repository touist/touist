/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import entity.Literal;
import entity.Model;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 *
 * @author WAHAB
 */
public class SolverSMT extends Solver {
    private Process p;
    private BufferedReader stdout;
    private String smtpath;
    private static String CurrentPath=System.getProperty("user.dir");
    public static String pathsolver=CurrentPath+File.separatorChar+"external"+File.separatorChar+"yices-smt2";
    public SolverSMT(String smtpath) throws FileNotFoundException{
        File testfile=new File(smtpath);
        if (testfile.isFile())
        this.smtpath=smtpath;
        else
          throw new FileNotFoundException();
    }
<<<<<<< Updated upstream
=======

    public void setSolverPath(String path){
        pathsolver=path;
    }

>>>>>>> Stashed changes
    
    /**
     * For Java RE 6 compatibility (p.isAlive() is JavaRE7)
     */
	private boolean isAlive(Process process) {
	    try {
	        process.exitValue();
	        return false;
	    } catch (Exception e) {
	        return true;
	    }
	}
    
<<<<<<< Updated upstream
=======

>>>>>>> Stashed changes
     public Model getresult() throws IOException, SolverExecutionException{
      // String command="bin"+File.separatorChar+"yices-smt2"+" "+this.smtpath;
        Model smt=null;
         String command=pathsolver+" "+this.smtpath;
        System.out.println("launch(): cmd executed: "+command);
        StringBuffer br=new StringBuffer();
        this.p = Runtime.getRuntime().exec(command);
	stdout = new BufferedReader(new InputStreamReader(p.getInputStream()));
        String line="";
        while((line=stdout.readLine())!=null && isAlive(p)){
            br.append(line);
        }
        System.out.println("xd:"+br.toString());
        StringTokenizer tokenizer = new StringTokenizer(br.toString(),"()");
        if(tokenizer.hasMoreTokens()){
            if(tokenizer.nextToken().equals("unsat"))
                throw new SolverExecutionException("SMT: "+br.toString());
            else
            {
                smt=new Model();
                ArrayList<String> result=new ArrayList<String>();
                while(tokenizer.hasMoreTokens()){
                    String Token=tokenizer.nextToken();
                        if(!Token.equals(" "))
                        {
                            if(Token.startsWith("/")){
                                String operand1=tokenizer.nextToken();
                                String operand2=tokenizer.nextToken();
                                result.add(operand1+"/"+operand2);
                            }
                            else{
                                result.add(Token);
                            }
                         }
                         
              }
                for(int i=0;i<result.size()-1;i++)
                {
                    if(result.get(i).contains("true") || result.get(i).contains("false"))
                    { String[] separateLV=result.get(i).split(" ");
                        smt.addLiteral(new Literal(separateLV[0],separateLV[1].matches("true")));
                    }
                    else{
                        //System.out.println(result.get(i)+"eo"+result.get(i+1));
                        smt.addLiteral(new Literal(result.get(i),result.get(i+1)));
                        i++;
                    }
                 }
            }
      }

        this.close();
        return smt;
    }
    
    @Override
    public void launch() throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void close() {
       this.p.destroy();
       System.out.println("close(): solver has been closed correctly");
    }

    @Override
    public ModelList getModelList() throws SolverExecutionException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected Model nextModel() throws IOException, SolverExecutionException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected Model parseModel(String[] rawModelOutput) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    
    public static void main(String[] args) throws IOException, SolverExecutionException {
        // TODO code application logic here
     //appel dans ton parametre
      // smt.setSolverPath("/Users/blida/Documents/M1-UPS/Yices/exec/bin/yices-smt2");
        //instance dans main
        SolverSMT smt=new SolverSMT(CurrentPath+File.separatorChar+"test.smt2");
        //appel lors de la réussit du traducteur
        Model model=smt.getresult();
        if(model!=null)
        { System.out.println(model.toString());
         }
        else{
            //pas de model
            System.out.println("pas de modele");
        }
    }
    
}
