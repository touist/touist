package translation;

import java.util.HashMap;
import java.util.Map;

/**
* @author Abdel
* @modified by Maël
*/
public class TranslationResultSAT implements TranslationResult{

	    private String dimacsFilePath;
	    private Map<Integer, String> literaux = new HashMap<Integer,String>();

	    @Override
		public void setDimacsFilePath(String dimacsFilePath) {
	        this.dimacsFilePath = dimacsFilePath;
	    }

	    @Override
		public void addLiteraux(int cleP,String P) {
	        this.literaux.putIfAbsent(cleP, P);
	    }
	    @Override
		public String getDimacsFilePath() {
	        return dimacsFilePath;
	    }
	    @Override
		public Map<Integer, String> getLiteraux() {
	        return literaux;
	    }


}
