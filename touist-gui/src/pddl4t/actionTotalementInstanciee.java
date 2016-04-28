package pddl4t;
import java.util.List ;
import java.util.ArrayList ;
import java.util.Arrays;
import fr.uga.pddl4j.encoding.CodedProblem ;
import fr.uga.pddl4j.util.* ;


class ATI{
	
	protected List<String> initiaux ;
	
	protected List<String> finaux ;
	
	private List<String> fluents ;
	
	private List<String> ajouts;
	
	private List<String> retraits;
	
	private List<String> preconditions;
	
	private List<String> PDDL_operations ;
	
	protected List<String> operations ;
	
	private List<String> constantes ;
	
	private List<String> predicats ; 
	
	public List<String> getInitiaux(){
		return initiaux;
	}
	
	public List<String> getFinaux(){
		return finaux;
	}
	
	public List<String> getFluents(){
		return fluents;
	}
	
	public List<String> getOp(){
		return operations ;
	}
	
	public List<String> getConst(){
		return constantes;
	}
	
	public List<String> getPredicats(){
		return predicats;
	}
	
	public List<String> getAjouts(){
		return ajouts;
	}
	
	public List<String> getRetraits(){
		return retraits;
	}
	
	public String Cond(String operation){
		return operation.substring(operation.indexOf("Preconditions:") + 14,operation.indexOf("Effects:")) ;
	}
	
	public String Effx(String operation){
		return operation.substring(operation.indexOf("Effects:")+8,operation.length());
	}
	
	public List<String> Add(String operation){
		String effx;
		String [] l ;
		List<String> L ;
		effx = Effx(operation);
		l = effx.split("[\n]");
		L = new ArrayList<String>(Arrays.asList(l));
		if(L.get(0) == "(") L.remove(0);
		if(L.get(L.size()-1) == ")") L.remove(L.size()-1);
		for(int i = 0 ; i < L.size() ; i++){
			if(L.get(i).contains("not")){
				L.remove(i);
				i--;
			}
			L.set(i,L.get(i).replace("and",""));
			L.set(i,L.get(i).replace(' ','_'));
			L.set(i,L.get(i).replace("(",""));
			L.set(i,L.get(i).replace(")",""));
		}
		//~ System.out.println(L);
		return L ;
	}
	
	public List<String> Del(String operation){
		String effx;
		String [] l ;
		List<String> L,R ;
		effx = Effx(operation);
		l = effx.split("[\n]");
		L = new ArrayList<String>(Arrays.asList(l));
		R = new ArrayList<String>();
		if(L.get(0) == "(") L.remove(0);
		if(L.get(L.size()-1) == ")") L.remove(L.size()-1);
		for(int i = 0 ; i < L.size() ; i++){
			if(L.get(i).contains("not")){
				R.add(L.get(i));
			}
			L.set(i,L.get(i).replace("not",""));
			L.set(i,L.get(i).replace(' ','_'));
			L.set(i,L.get(i).replace("(",""));
			L.set(i,L.get(i).replace(")",""));
		}
		//~ System.out.println(R);
		return R ;
	}
	
	
	public void Instancier(CodedProblem C){
		for(int i = 0 ; i < C.getRevelantFacts().size() ; ++i){
			fluents.add(C.toString(C.getRevelantFacts().get(i))) ;
			fluents.set(i,fluents.get(i).replace("(",""));
			fluents.set(i,fluents.get(i).replace(' ','_'));
			fluents.set(i,fluents.get(i).replace(")",""));
		}
			
		for(int i = 0 ; i < C.getOperators().size() ; ++i){
			PDDL_operations.add(C.toString(C.getOperators().get(i))) ;
			operations.add(C.toShortString(C.getOperators().get(i))) ;
		}
			
		initiaux = new ArrayList<String>(Arrays.asList(C.toString(C.getInit()).split("[\n]")));
		finaux = new ArrayList<String>(Arrays.asList(C.toString(C.getGoal()).split("[\n]"))) ;
		
		if(initiaux.get(0) == "(") initiaux.remove(0);
		if(initiaux.get(initiaux.size()-1) == ")") initiaux.remove(initiaux.size()-1);
		
		if(finaux.get(0) == "(") finaux.remove(0);
		if(finaux.get(finaux.size()-1) == ")") finaux.remove(finaux.size()-1);
		
		for(int i = 0 ; i < initiaux.size() ; ++i){
			initiaux.set(i,initiaux.get(i).replace("(",""));
			initiaux.set(i,initiaux.get(i).replace(' ','_'));
			initiaux.set(i,initiaux.get(i).replace(")",""));
		}
		for(int i = 0 ; i < finaux.size() ; ++i){
			finaux.set(i,finaux.get(i).replace("(",""));
			finaux.set(i,finaux.get(i).replace(' ','_'));
			finaux.set(i,finaux.get(i).replace(")",""));
		}
		
		constantes = C.getConstants();
		predicats = C.getPredicates();
		
		for(int i = 0 ; i < operations.size() ; ++i){
			ajouts.addAll(Add(PDDL_operations.get(i)));
			retraits.addAll(Del(PDDL_operations.get(i)));
		}
		
	}
	public ATI(CodedProblem C){
		fluents = new ArrayList<String>();
		PDDL_operations = new ArrayList<String>();
		operations = new ArrayList<String>();
		ajouts = new ArrayList<String>();
		retraits = new ArrayList<String>();
		Instancier(C);
	}
	
}
