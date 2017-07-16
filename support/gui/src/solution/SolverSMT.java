/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package solution;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

import entity.Literal;
import entity.Model;

/**
 *
 * @author WAHAB
 */
public class SolverSMT extends Solver {
    private Process p;
    private BufferedReader stdout;
    private String smtpath;
    private static String CurrentPath=System.getProperty("user.dir");
    public static String pathsolver = touist.TouIST.getTouistExternalDir() + File.separatorChar + "yices-smt2";
    public SolverSMT(String smtpath) throws FileNotFoundException{
        File testfile=new File(smtpath);
        if (testfile.isFile())
        this.smtpath=smtpath;
        else
          throw new FileNotFoundException();
    }

    public void setSolverPath(String path){
        pathsolver=path;
    }

    /**
     * For java jre 1.6 and 1.7 compatibility (p.isAlive() is java jre >= 1.8)
     */
	private boolean isAlive(Process process) {
	    try {
	        process.exitValue();
	        return false;
	    } catch (Exception e) {
	        return true;
	    }
	}
    
     public Model getresult() throws IOException, SolverExecutionException{
      // String command="bin"+File.separatorChar+"yices-smt2"+" "+this.smtpath;
        Model smt=null;
         String [] command = {pathsolver, this.smtpath};
        System.out.println("launch(): cmd executed: "+Arrays.toString(command));
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
            String tokken=tokenizer.nextToken();
            if(!tokken.equals("sat"))
                throw new SolverExecutionException("SMT: "+br.toString());
            else
            { 
             smt=new Model();
                ArrayList<String> result=new ArrayList<String>();
                while(tokenizer.hasMoreTokens()){
                    String Token=tokenizer.nextToken();
                    System.out.println("+++"+Token);
                        if(!Token.equals(" "))
                        {  
                            
                            if(Token.startsWith("/") && Token.split(" ").length==1){
                                System.out.println("je rentre1"+Token+Token.length());
                                String operand1;
                                String operand2;
                                String a=tokenizer.nextToken();
                                System.out.println("a"+a);
                                 String[] ab=a.split(" ");
                                 if(ab.length==2)
                                 {  
                                    operand1=ab[0]+ab[1];
                                    System.out.println("b"+operand1);
                                    operand2=(tokenizer.hasMoreTokens() == true)? tokenizer.nextToken() : null;
                                    System.out.println("wizz12");
                                 }
                                 else{
                                     System.out.println("wizz213"+ab[0]+ab[1]);
                                     operand1=ab[0];
                                     String[] abcd2 = (tokenizer.hasMoreTokens() == true)? tokenizer.nextToken().split(" ") : null;
                                     operand2=abcd2[0]+abcd2[1];
                                     
                                 }
                                result.add(operand1+"/"+operand2);
                            }
                            else{
                                System.out.println("je rentre2");
                                if(Token.startsWith("/")){
                                 String[] abcd=Token.split(" ");
                                String operand1=abcd[1];                                
                                String operand2=abcd[2];
                                result.add(operand1+abcd[0]+operand2);
                                }
                                else{
                                if(Token.startsWith("-")){
                               String[] abcd=Token.split(" ");
                                String operator=abcd[0];
                                String operand1=abcd[1];
                                result.add(operator+operand1);
                                }
                                else
                                { if(Token.split(" ").length==2)
                                    { String[] tt=Token.split(" ");
                                    result.add(tt[0]);
                                    result.add(tt[1]);
                                    }
                                    else{
                                    result.add(Token);
                                    }
                                }
                                }
                            }
                         }
                }
                for(int i=0;i<result.size();i++)
                {   System.out.println(result.get(i));
                    if(result.get(i).contains("true") || result.get(i).contains("false"))
                    { String[] separateLV=result.get(i).split(" ");
                        smt.addLiteral(new Literal(separateLV[0],separateLV[1].matches("true")));
                    }
                    else{
                        System.out.println(result.get(i)+"beug");
                        //String[] abc=result.get(i).split(" ");
                       // System.out.println(result.get(i+1)+"beug");
                        smt.addLiteral(new Literal(result.get(i),result.get(i+1)));
                        System.out.println("xd12");
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
      //  SolverSMT smt=new SolverSMT(CurrentPath+File.separatorChar+"test.smt2");
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
