package pddl4t;
import java.util.List ;
import java.util.ArrayList ;

class ActionRes{
	private String action ;
	private int lvlmin ;
	private int lvlmax ;
	private List<String> add;
	private List<String> del;
	private List<String> cond;
	public ActionRes(String op,ATI ati){
		action = new String();
		action = op ;
		add = new ArrayList<String>();
		del = new ArrayList<String>();
		cond =new ArrayList<String>();
		add = ati.Add(action) ;
		del = ati.Del(action) ;
		cond.add(ati.Cond(action));
		lvlmin = 0 ;
		lvlmax = (int)Math.pow(2.f,31.f) ;
	}
	public String getAction(){return action;}
	public int getMin(){ return lvlmin; }
	public int getMax(){ return lvlmax;}
	public List<String> getAdd(){return add;}
	public List<String> getDel(){return del;}
	public List<String> getCond(){return cond;}
	public void setAdd(List<String> l){ add = l;}
	public void setDel(List<String> l){ del = l;}
	public void setCond(List<String> l){ cond = l;}
	public void setAction(String a){ action = a ;}
	public void setMin(int mn){lvlmin = mn; }
	public void setMax(int mx){lvlmax = mx;}
	

}
