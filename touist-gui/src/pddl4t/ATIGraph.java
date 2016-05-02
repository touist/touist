package pddl4t;
import java.util.List ;
import java.util.ArrayList ;
import java.util.Arrays;
import fr.uga.pddl4j.encoding.CodedProblem ;
import fr.uga.pddl4j.util.* ;


public class ATIGraph extends ATI{
	private List<ActionRes> Res ;
	private int length ;
	
	public boolean l1inl2(List<String> but, List<String> fl){
		for(String g : but){
			boolean inter = false ;
			for(String f : fl){
				if(g == f) inter = true ;
			}
			if(inter == false) return false ;
		}
		return true ;
	}
	
	public void Comeback(List<String> but, List<ActionRes> res, int currentlvl){
		while(currentlvl > 1){
			for(String g:but){
				for(ActionRes r:res){
					if(r.getAdd().contains(g)){
						but.addAll(r.getCond());
						if(r.getMax() < currentlvl) r.setMax(currentlvl) ;
					}
				}
			}
			currentlvl-- ;
		}
	}
	
	public void Go(List<ActionRes> res, List<String> fl,int currentlvl){
		List<String> new_fluent = new ArrayList<String>() ;
		for(ActionRes r:res){
			if(r.getMin() == 0 && l1inl2(r.getCond(),fl)){
				new_fluent.addAll(r.getAdd()) ;
				r.setMin(currentlvl);
			}
		}
		new_fluent.removeAll(fl);
		fl.addAll(new_fluent);
	}
	
	
	public ATIGraph(ATI ati){
		super(ati);
	}
	
	public ATIGraph(CodedProblem C){
		super(C) ;
		int gp_length = super.getOp().size();
		Res = new ArrayList<ActionRes>() ;
		length = 0 ;
		for(String o : operations){
			Res.add(new ActionRes(o,this));
		}
		List<String> fl= initiaux ;
		
		int currentlvl = 1 ;
		
		boolean solution = false ;
		
		while(currentlvl <= gp_length){
			Go(Res,fl,currentlvl);
			currentlvl++;
			solution = l1inl2(finaux,fl);
			if(solution == false && currentlvl > gp_length) gp_length++;
		}
		length = gp_length ;
		Comeback(finaux,Res,currentlvl);
	}

}
