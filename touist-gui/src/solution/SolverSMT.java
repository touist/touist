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
    
    public SolverSMT(String smtpath) throws FileNotFoundException{
        File testfile=new File(smtpath);
        if (testfile.isFile())
        this.smtpath=smtpath;
        else
          throw new FileNotFoundException();
    }
     public Model getresult() throws IOException, SolverExecutionException{
      // String command="bin"+File.separatorChar+"yices-smt2"+" "+this.smtpath;
        Model smt=null;
         String command=CurrentPath+File.separatorChar+"external"+File.separatorChar+"yices-smt2"+" "+this.smtpath;
        System.out.println("launch(): cmd executed: "+command);
        StringBuffer br=new StringBuffer();
        this.p = Runtime.getRuntime().exec(command);
	stdout = new BufferedReader(new InputStreamReader(p.getInputStream()));
        String line="";
        while((line=stdout.readLine())!=null && p.isAlive()){
            br.append(line);
        }
        StringTokenizer tokenizer = new StringTokenizer(br.toString(),"()");
        if(tokenizer.hasMoreTokens()){
            if(tokenizer.nextToken().equals("unsat"))
                throw new SolverExecutionException("SMT: "+br.toString());
            else
            {
                smt=new Model();
                while(tokenizer.hasMoreTokens()){
                String Token=tokenizer.nextToken();
                String[] LiterralValuation=Token.split("\n");
                for(String getLV : LiterralValuation){
                String[] separateLV= getLV.split(" ");
                if(separateLV.length==2){
                    //if(separateLV[1].i)
                    smt.addLiteral(new Literal(separateLV[0],separateLV[1].matches("true")));
                }
                //if(!getLV.equals(" "))
                //System.out.println(getLV);
                
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
    protected Model nextModel() throws IOException, NotSatisfiableException, SolverExecutionException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected Model parseModel(String[] rawModelOutput) throws NotSatisfiableException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    
    public static void main(String[] args) throws IOException, SolverExecutionException {
        // TODO code application logic here
      SolverSMT smt=new SolverSMT(CurrentPath+File.separatorChar+"test.smt2");
        Model model=smt.getresult();
        if(model!=null)
        System.out.println(model.toString());
    }
    
}
