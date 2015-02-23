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
 */
public class ResultatOcaml {
    private String dimacsFilePath;
    private Map<Integer, String> literaux;
    public String getDimacsFilePath() {
        return dimacsFilePath;
    }

    public Map<Integer, String> getLiteraux() {
        return literaux;
    }
}
