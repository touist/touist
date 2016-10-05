/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/olzd/touist
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

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

import org.sat4j.minisat.SolverFactory;
import org.sat4j.reader.DimacsReader;
import org.sat4j.reader.ParseFormatException;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.tools.ModelIterator;
/**
 *
 * @author Abdel
 */
public class Minisat {

    /**
     * @param args the command line arguments
     */
    //Use : javac -cp sat4j-sat.jar Minisat.java to compil program
    // &
    //java -cp .:sat4j-sat.jar Minisat DimacsFile to run it
    //press 1 given Next Model
    //and 0 to exiting prog
    public static void main(String[] args) throws ParseFormatException, IOException, ContradictionException {

        //Instanciate MiniSat Solver from org.sat4j.minisat.SolverFactory
        ISolver solver = SolverFactory.newDefault();
        //ModelIterator
        ModelIterator mi = new ModelIterator(solver);
        //TimeLimite for executing this Solver session
        solver.setTimeout(3600); // 1 hour timeout
        //DimacsReader will be an Iterator Reader for Solver Instance to resolve Problem
        DimacsReader reader = new DimacsReader(mi);
        try {
            //IProblem is an Instance who contain Problem Format(CNF)
            //if File contain wron CNF format, ParseFrmatException will be generated.
            IProblem problem = reader.parseInstance(args[0]);
            boolean unsat=true;
            // Buffered Input Reader will able to communicate(pipe) with Main Program
            BufferedReader reader1 =new BufferedReader(new InputStreamReader(System.in));
            // 1 to Run each model and 0 to exit program.
            while(problem.isSatisfiable() && Integer.parseInt(reader1.readLine())==1) {
                unsat=false;
                //System.out.println("Satisfiable !");
                //problem model return int[] Satisfiable model
                System.out.println(reader.decode(problem.model()));
            }
            if(unsat)
                System.err.println("Unsatisfiable !");
                System.exit(1);

        //Catch Exceptions....
        } catch (FileNotFoundException e) {
            System.err.println("Error Loading File");
            System.exit(2);
        } catch (ParseFormatException e) {
            System.err.println("Incorrect Dimacs Content");
            System.exit(3);
        } catch (IOException e) {
            System.err.println("Error StreamReader");
            System.exit(4);
        } catch (ContradictionException e) {
            System.err.println("Unsatisfiable (trivial)!");
            System.exit(1);
        }catch (org.sat4j.specs.TimeoutException ex) {
            System.err.println("Timeout Solver/Please Restart");
            System.exit(6);
        }
        System.exit(0);
    }
}
