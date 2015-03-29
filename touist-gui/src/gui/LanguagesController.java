/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

/**
 *
 * @author Skander
 */
public class LanguagesController {
    private Map<Locale, String> supportedLanguages;
    private ResourceBundle translation;

    public LanguagesController(Locale language){
        supportedLanguages = new HashMap();
        supportedLanguages.put(Locale.FRENCH, "ressources/lang/fr_FR");
        supportedLanguages.put(Locale.ENGLISH, "ressources/lang/en_US");
        translation = ResourceBundle.getBundle(supportedLanguages.get(language));
    }
    
    public void setLanguage(Locale language){
        translation = ResourceBundle.getBundle(supportedLanguages.get(language));
    }

    public String getWord(String keyword){
        return translation.getString(keyword);
    }
    
    public static final String FRAME_TITLE = "MainFrame.title";
    public static final String FRAME_MENU_FILE = "MainFrame.jMenu1.text";
    public static final String FRAME_MENU_HELP = "MainFrame.jMenu2.text";
    public static final String FRAME_MENU_LANGUAGE = "MainFrame.jMenu3.text";
    public static final String EDITION_OPTION_PANE = "ParentEditionPanel.jOptionPane1.title";
    public static final String EDITION_FILE_CHOOSER = "ParentEditionPanel.jFileChooser1.title";
    public static final String EDITION_IMPORT = "ParentEditionPanel.importButton.text";
    public static final String EDITION_TEST = "ParentEditionPanel.testButton.text";
    public static final String EDITION_TAB_FORMULAS = "ParentEditionPanel.editorPanelFormulas.TabConstraints.tabTitle";
    public static final String EDITION_TAB_SETS = "ParentEditionPanel.editorPanelSets.TabConstraints.tabTitle";
    public static final String PALETTE_TEXT = "PalettePanel.jLabel1.text";
    public static final String RESULTS_NEXT = "esultsPanel.jButtonNext";
    public static final String RESULTS_PREVIOUS = "ResultsPanel.jButtonPrevious.text";
    public static final String RESULTS_RETURN = "ResultsPanel.jButtonEditor.text";
    public static final String RESULTS_TEXT = "ResultsPanel.jLabel1.text";
}
