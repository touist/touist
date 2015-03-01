package translation;

import java.util.Map;

/**
* @author Abdel
* @modified by Maël
*/
public interface TranslationResult {
	    public void setDimacsFilePath(String dimacsFilePath);
	    public void addLiteraux(int cleP,String P);
	    public String getDimacsFilePath();
	    public Map<Integer,String> getLiteraux();
}
