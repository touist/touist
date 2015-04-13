/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

/**
 *
 * @author Skander
 */
public class Lang {
    private Map<Locale, String> supportedLanguages;
    private ResourceBundle translation;

    public Lang(Locale language){
        initSupportedLanguages();
        translation = ResourceBundle.getBundle(supportedLanguages.get(language));
    }
    
    public void setLanguage(Locale language){
        translation = ResourceBundle.getBundle(supportedLanguages.get(language));
    }

    public String getWord(String keyword){
        try {
            return translation.getString(keyword);
        } catch (Exception e) {
            System.out.println("Unknown key : '" + keyword + "'");
        }
        return keyword;
    }
    
    public List<Locale> getSupportedLanguages() {
        List<Locale> locales = new ArrayList<>();
        locales.addAll(supportedLanguages.keySet());
        return locales;
    }
    
    private void initSupportedLanguages() {
        supportedLanguages = new HashMap();
        supportedLanguages.put(Locale.FRENCH, "lang.fr_FR"); // "lang" is the package
        supportedLanguages.put(Locale.ENGLISH, "lang.en_US");
    }
    
    public static final String FRAME_TITLE = "MainFrame.title";
    public static final String FRAME_MENU_FILE = "MainFrame.jMenu1.text";
    public static final String FRAME_MENU_HELP = "MainFrame.jMenu2.text";
    public static final String FRAME_MENU_LANGUAGE = "MainFrame.jMenu3.text";
    public static final String EDITION_FILE_CHOOSER = "ParentEditionPanel.jFileChooser1.title";
    public static final String EDITION_IMPORT = "ParentEditionPanel.importButton.text";
    public static final String EDITION_TEST = "ParentEditionPanel.testButton.text";
    public static final String EDITION_TAB_FORMULAS = "ParentEditionPanel.editorPanelFormulas.TabConstraints.tabTitle";
    public static final String EDITION_TAB_SETS = "ParentEditionPanel.editorPanelSets.TabConstraints.tabTitle";
    public static final String PALETTE_TEXT = "PalettePanel.jLabel1.text";
    public static final String RESULTS_NEXT = "ResultsPanel.jButtonNext.text";
    public static final String RESULTS_PREVIOUS = "ResultsPanel.jButtonPrevious.text";
    public static final String RESULTS_RETURN = "ResultsPanel.jButtonEditor.text";
    public static final String RESULTS_TEXT = "ResultsPanel.jLabel1.text";
    public static final String RESULTS_NAME = "ResultsPanel.jTable1.name";
    public static final String RESULTS_VALUE = "ResultsPanel.jTable1.value";
    public static final String ERROR_TRADUCTION = "ParentEditionPanel.jOptionPane1.traductionError";
    public static final String ERROR_TRADUCTION_IN_SETS = "ParentEditionPanel.jOptionPane1.traductionErrorInSets";
    public static final String ERROR_TRADUCTION_IN_FORMULAS = "ParentEditionPanel.jOptionPane1.traductionErrorInFormulas";
}
