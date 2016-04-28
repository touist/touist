package pddl4t;

import fr.uga.pddl4j.parser.*;
import fr.uga.pddl4j.encoding.*;
import java.io.*;

class Traduction{

	public Traduction(){
		
	}
	public CodedProblem Traduire(String[] args) {
			Parser parser = new Parser();
		   if (args.length == 2) {
		       
		       try {
		    	   parser.parse(args[1]);
		         } catch (FileNotFoundException e) {
		           System.out.println(e.getMessage());
		         }
		       if (!parser.getErrorManager().isEmpty()) {
		    	   parser.getErrorManager().printAll();
		         }
		     } else if (args.length == 4) {
		    try {
		    	parser.parse(args[1], args[3]);
		         } catch (FileNotFoundException e) {
		           System.out.println(e.getMessage());
		        }
		       if (!parser.getErrorManager().isEmpty()) {
		    	   parser.getErrorManager().printAll();
		         }
		     } else {
		       System.out.println("usage of parser");
		       System.out.println("OPTIONS   DESCRIPTIONS");
		       System.out.println("-p    path for operator and fact file");
		       System.out.println("-o 	operator file name");
		      System.out.println("-f 	fact file name");
		    }
		   final CodedProblem pb = Encoder.encode(parser.getDomain(),parser.getProblem());
		   return pb;
		 }
		
	public static void main(String args []){
		//~ Test de la classe 
		Traduction c = new Traduction() ;
		CodedProblem toto = new CodedProblem(c.Traduire(args));
	
		ATI C = new ATI(toto);
		//~ System.out.println(C.getOp().size());
		System.out.println(C.Effx(C.getOp().get(9)));
		C.Cond(C.getOp().get(9));
		//~ System.out.println(toto.toShortString(toto.getOperators().get(0)));
		//~ System.out.println(C.getOp().get(0));

	}
}
