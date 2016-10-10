/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/touist/touist
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser General Public License
 * (LGPL) version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * Contributors:
 *     Alexis Comte, Abdelwahab Heba, Olivier Lezaud,
 *     Skander Ben Slimane, Maël Valais
 *
 */

package touist;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ListIterator;

import javax.swing.JOptionPane;

import solution.SolverExecutionException;
import solution.SolverTestSAT4J;
import translation.TranslatorSAT;
import entity.Model;
import gui.MainFrame;
/**
 *
 * @author Skander
 */
public class TouIST {
	private static TouistProperties properties = new TouistProperties();
	
	/**
	 * @param args the command line arguments
	 */
	private static String CurrentPath=System.getProperty("user.dir");
	public static void help(){
		
		System.out.println("* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *");
		System.out.println("* Usage: TouistSol [Options...] filename                                    *");
		System.out.println("* General options:                                                          *");
		System.out.println("*  --cnf          read & resolve CNF problem in DIMACS format               *");
		System.out.println("*  --s            read & resolve TouIST problem in TOUISTL format           *");
		System.out.println("*  --t            read TouIST problem in TOUISTL format and build           *");
		System.out.println("*                 (DIMACS format, Hash CNF table)                           *");
		System.out.println("*  -o filename,--output filename                                            *");
		System.out.println("*                 write solution to filename in printable format            *");
		System.out.println("*  -h, --help     display this help information and exit                    *");
		System.out.println("*  -v, --version  display program version and exit                          *");
		System.out.println("*                                                                           *");
		System.out.println("*  -v, --version  display program version and exit                          *");
		System.out.println("* TouISTSOL "+properties.getProperty("version")+", 2015					    *");
		System.out.println("* Easily formalize and solve real-world sized problems                      *");
		System.out.println("* using propositional logic and linear theory of reals                      *");
		System.out.println("* See TouIST web page at : www.irit.fr/softwave/.../touIST.html             *");
		System.out.println("*                                                                           *");
		System.out.println("* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *");
	}
	public static void version(){
		System.out.println("* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *");
		System.out.println("* TouISTSOL: TouIST Propositional logic/Linear Theory of reals Solver, "+properties.getProperty("version")+"	*");
		System.out.println("*                                                                             *");
		System.out.println("* Copyright (C) 2015. Institut de Recherche en Informatique de Toulouse, FR   *");
		System.out.println("* All rights reserved.Email:<.....@irit.fr>                                   *");
		System.out.println("* Contributors:                                                               *");
		System.out.println("*     Khaled Skander Ben Slimane, Alexis Comte, Olivier Gasquet,              *");
		System.out.println("*     Abdelwahab Heba, Olivier Lezaud, Frédéric Maris, Maël Valais            *");
		System.out.println("*                                                                             *");
		System.out.println("* This program is free software; you may re-distrubute it under the terme of  *");
		System.out.println("* the GNU Lesser General Public License (LGPL) version 2.1 which accompanies  *");
		System.out.println("* this distribution, and is available at :                                    *");
		System.out.println("* http://www.gnu.org/licenses/lgpl-2.1.html                                   *");
		System.out.println("* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *");
	}
	public static void empty(String arg,int number){
		if(number==1){
			System.out.println("TouISTSOL "+properties.getProperty("version")+", 2015.");
			System.out.println("No input option; try TouISTSol --help                             ");
		}
		else{
			if(number==2)
			{
				System.out.println("TouISTSOL "+properties.getProperty("version")+", 2015.");
				System.out.println("Invalid option '"+arg+"'; try TouISTSol --help                             ");   
			}
			if(number==3)
			{
				System.out.println("TouISTSOL "+properties.getProperty("version")+", 2015.");
				System.out.println("Parameter(s) specified in the command line:\n "+arg);
				System.out.println("No input problem file specified; try TouISTSol --help                             ");
			}
			if(number==4){
				System.out.println("No output solution file specified");
			}
		}
	}
	public static void solvetouISTL(String path,int nb,boolean control,String pathSave) throws IOException, InterruptedException{
		TranslatorSAT translator = new TranslatorSAT("compiler"+File.separatorChar+"touistc.native");
		if(translator.translate(path))
		{
			// Minisat solver= new Minisat(translator.getDimacsFilePath(),nb,translator.getLiteralsMap());
			// Models m=solver.resolveTouISTLProblem();
			StringBuilder str=new StringBuilder();
			//            for (Model m1: m.getModels()){
			//                str.append(m1.toString()+"\n");
			//            }
			if(control)
				System.out.println(str.toString());
			else
				Output(pathSave,str.toString());
		}
		//File f=new File(translator.getDimacsFilePath());
		//f.delete();
		//File f1=new File()
	}

	public static void solveCNF(String path,int nb,boolean control,String pathSave) throws FileNotFoundException, IOException, SolverExecutionException{
		//Add CurrentPath/dimacsFile

		SolverTestSAT4J solver=new SolverTestSAT4J(path);

		try {
			solver.launch();
		} catch (IOException ex) {
			ex.printStackTrace();
			String errorMessage = "Couldn't launch solver.";
			System.out.println("Solver error "+ errorMessage);
			System.exit(0);
		}       

		// Si il y a au moins un model
		ListIterator<Model> iter = (ListIterator<Model>) solver.getModelList().iterator();
		/**
		 * Si il y a plus d'un model, alors passer à l'état FIRST_RESULT
		 * sinon passer à l'état SINGLE_RESULT
		 */
		System.out.println("eoo je suis ici");
		if (iter.hasNext())
			System.out.println(iter.next().toString());
		//                if (iter.hasNext()) {
		//                   //iter.previous();
		//                    return State.FIRST_RESULT;
		//                } else {
		//                    //iter.previous();
		//                    return State.SINGLE_RESULT;
		//                }
		//            } else {
		//                getFrame().setResultsPanelEmpty();
		//                return State.NO_RESULT;
		//            }
		//        

		//        Map<Integer,String> mp=new HashMap<Integer,String>();
		//        File f=new File(path);
		//        BufferedReader br=new BufferedReader(new FileReader(f));
		//        String line="";
		//        br.readLine();
		//        br.readLine();
		//        while((line=br.readLine())!=null){
		//            String[] line1=line.split(" ");
		//            for (String line2:line1)
		//            {
		//                int number=Integer.parseInt(line2);
		//                number=(number>0? number:number*(-1));
		//                if(number!=0 && !mp.containsKey(number))
		//                    mp.put(number,"P("+number+")");
		//            }
		//        }
		//        
		//       // Minisat solver=new Minisat(path,nb,mp);
		//        
		//        //Models m=solver.resolveTouISTLProblem();
		//        StringBuilder str=new StringBuilder();
		////        for (Model m1: m.getModels()){
		////                str.append(m1.toString()+"\n");
		////            }
		//        if(control)
		//            System.out.println(str.toString());
		//        else
		//           Output(pathSave,str.toString());
	}
	public static void translate(String path,String nameCnf,String nameTable) throws IOException, InterruptedException{
		TranslatorSAT translator = new TranslatorSAT("compiler"+File.separatorChar+"touistc.native");
		//translator.setOutputFilePath(nameCnf);
		//translator.setOutputTableFilePath(nameTable);
		if(translator.translate(path))
		{
			System.out.println("Translate done..");
			//System.out.println("CNF FILE :"+translator.getDimacsFilePath());
			//System.out.println("<Key,Literal>: \n");
			// while(translator.getLiteralsMap().entrySet().iterator().hasNext()){
			//   Map.Entry<Integer, String> e= translator.getLiteralsMap().entrySet().iterator().next();
			//  System.out.println(e.getKey()+" "+e.getValue());
			//}
		}
	}

	public static void Output(String nameFile,String out) throws IOException{
		BufferedWriter wr=new BufferedWriter(new FileWriter(nameFile));
		wr.write(out);
		wr.flush();
		wr.close();
	}
	public static void main(String[] args) throws IOException, InterruptedException, FileNotFoundException, SolverExecutionException {
		String version = System.getProperty("java.version");
		if(Float.valueOf(version.substring(0,3)) < 1.7) {
			JOptionPane.showMessageDialog(null, "Your java version is "+version+" but version higher or equal to 1.7 is required");
			return;
		}
		if(args.length==0) {
			System.out.println("main(): running app from folder '"
					+ System.getProperty("user.dir")+"'");
			MainFrame frame = new MainFrame();
			frame.setVisible(true);
		}
		else{
			String path="";
			File f;

			//General Option
			if(args[0].equals("--help") || args[0].equals("-h")) {
				help();System.exit(0);
			}
			else if(args[0].equals("--version") || args[0].equals("-v")) {
				version();System.exit(0);
			}
			//Solver Option SAT:
			//Using TouIST Language
			else if(args[0].equals("--t")) {   
				path=CurrentPath+File.separatorChar+args[1];
				f=new File(path);
				if(f.isFile() && path.endsWith(".touistl"))
					translate(path,args[3],args[4]);
				System.exit(0);
			}
			else if(args[0].equals("--s")) {
				if(args.length==3){ 
					path=CurrentPath+File.separatorChar+args[1];
					f=new File(path);
					if(f.isFile() && path.endsWith(".touistl"))
						solvetouISTL(CurrentPath+File.separatorChar+args[1],Integer.parseInt(args[2]),true,null);
					else
						System.out.println("le fichier doit etre d'extension .touisl");
				}
				else empty(args[0],3);System.exit(0);
			}
			//Using CNF format
			else if(args[0].equals("--cnf")) { 
				if( args.length==3 || args.length==5){
					path=CurrentPath+File.separatorChar+args[1];
					f=new File(path);
					if(f.isFile() && path.endsWith(".cnf"))
					{  if(args.length==3)
						solveCNF(CurrentPath+File.separatorChar+args[1],Integer.parseInt(args[2]),true,null);
					else
						solveCNF(CurrentPath+File.separatorChar+args[1],Integer.parseInt(args[2]),false,CurrentPath+File.separatorChar+args[4]);
					}else
						System.out.println("le fichier doit etre d'extension .cnf");
				}else{
					//System.out.println("eo"+a.length);
					if(args.length==4)
					{if(args[3].equals("-o") || args[3].equals("--output"))
						empty(null,4);
					else
						empty(args[3],3);}            
				}
				System.exit(0);
			}	
			//Solver Option SMT
			else if(args[0].equals("--smt")) {
				System.exit(0);
			}
			else if(args.length==0) empty(null,1);else empty(args[0],2);System.exit(0);
		}
	}
}

