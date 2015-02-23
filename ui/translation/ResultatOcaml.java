/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package translation;

import java.util.Map;

/**
 *
 * @author Skander
 * @Modified By Abdel
 */
public class ResultatOcaml {
    private String dimacsFilePath;
    private Map<Integer, String> literaux=new HashMap();

    public void setDimacsFilePath(String dimacsFilePath) {
        this.dimacsFilePath = dimacsFilePath;
    }

    public void addLiteraux(int cleP,String P) {
        this.literaux.putIfAbsent(cleP, P);
    }
    public String getDimacsFilePath() {
        return dimacsFilePath;
    }
    public Map<Integer, String> getLiteraux() {
        return literaux;
    }
   
}
