/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/touist/touist
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
    private Locale language;

    public Lang(Locale language){
        initSupportedLanguages();
        setLanguage(language);
    }
    
    public void setLanguage(Locale language){
        this.language = language;
        try {
        	translation = ResourceBundle.getBundle(supportedLanguages.get(language));
        } catch(Exception e)
        {
        	translation = ResourceBundle.getBundle(supportedLanguages.get(Locale.ENGLISH));
        }
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
        List<Locale> locales = new ArrayList<Locale>();
        locales.addAll(supportedLanguages.keySet());
        return locales;
    }
    
    public Locale getLanguage() {
        return language;
    }
    
    private void initSupportedLanguages() {
        supportedLanguages = new HashMap<Locale,String>();
        supportedLanguages.put(Locale.ENGLISH, "lang.en_US"); // "lang" is the package
        supportedLanguages.put(Locale.FRENCH, "lang.fr_FR");
        supportedLanguages.put(Locale.FRANCE, "lang.fr_FR");
    }
    
    public static final String FRAME_TITLE = "MainFrame.title";
    public static final String EDITION_MENU_FILE = "EditionMenuBar.jMenuFile.text";
    public static final String EDITION_MENU_HELP = "EditionMenuBar.jMenuHelp.text";
    public static final String EDITION_MENU_EDIT = "EditionMenuBar.jMenuEdit.text";
    public static final String EDITION_MENU_VIEW = "EditionMenuBar.jMenuView.text";
    public static final String EDITION_MENU_LANGUAGE = "EditionMenuBar.jMenuLanguage.text";
    public static final String EDITION_FILE_CHOOSER = "ParentEditionPanel.jFileChooser1.title";
    public static final String EDITION_IMPORT = "ParentEditionPanel.importButton.text";
    public static final String EDITION_EXPORT = "ParentEditionPanel.exportButton.text";
    public static final String EDITION_TEST = "ParentEditionPanel.testButton.text";
    public static final String EDITION_EXPORT_FAILURE_TITLE = "ParentEditionPanel.exportFailure.title";
    public static final String EDITION_EXPORT_FAILURE_TEXT = "ParentEditionPanel.exportFailure.text";
    public static final String EDITION_MENUITEM_SAVEFILE = "EditionMenuBar.jMenuItemSaveFile.text";
    public static final String EDITION_MENUITEM_SAVEASFILE = "EditionMenuBar.jMenuItemSaveAsFile.text";
    public static final String EDITION_MENUITEM_LOADFILE = "EditionMenuBar.jMenuItemLoadFile.text";
    public static final String EDITION_MENUITEM_HELPEDITION = "EditionMenuBar.jMenuItemHelpEdition.text";
    public static final String EDITION_MENUITEM_ABOUT = "EditionMenuBar.jMenuItemAbout.text";
    public static final String EDITION_MENUITEM_UNDO = "EditionMenuBar.jMenuItemUndo.text";
    public static final String EDITION_MENUITEM_REDO = "EditionMenuBar.jMenuItemRedo.text";
    public static final String EDITION_MENUITEM_ZOOMMORE = "EditionMenuBar.jMenuItemZoomMore.text";
    public static final String EDITION_MENUITEM_ZOOMLESS = "EditionMenuBar.jMenuItemZoomLess.text";
    public static final String PALETTE_TEXT = "SnippetContainer.jLabel1.text";
    public static final String RESULTS_NEXT = "ResultsPanel.jButtonNext.text";
    public static final String RESULTS_PREVIOUS = "ResultsPanel.jButtonPrevious.text";
    public static final String RESULTS_RETURN = "ResultsPanel.jButtonEditor.text";
    public static final String RESULTS_EXPORT = "ResultsPanel.jButtonExport.text";
    public static final String RESULTS_TEXT = "ResultsPanel.jLabel1.text";
    public static final String RESULTS_NAME = "ResultsPanel.jTable1.name";
    public static final String RESULTS_VALUE = "ResultsPanel.jTable1.value";
    public static final String RESULTS_MENU_FILE = "ResultsMenuBar.jMenuFile.text";
    public static final String RESULTS_MENU_HELP = "ResultsMenuBar.jMenuHelp.text";
    public static final String RESULTS_MENU_LANGUAGE = "ResultsMenuBar.jMenuLanguage.text";
    public static final String RESULTS_MENUITEM_SAVEMODEL = "ResultsMenuBar.jMenuItemSaveModel.text";
    public static final String RESULTS_MENUITEM_HELPRESULTS = "ResultsMenuBar.jMenuItemHelpResults.text";
    public static final String RESULTS_FILE_CHOOSER = "ResultsPanel.jFileChooser.title";
    public static final String SOLVER_SAT_NAME = "SAT";
    public static final String SOLVER_SAT_DESCRIPTION = "A basic solver.";
    public static final String ERROR_TRADUCTION = "ParentEditionPanel.jOptionPane1.traductionError";
    public static final String ERROR_TRADUCTION_IN_SETS = "ParentEditionPanel.jOptionPane1.traductionErrorInSets";
    public static final String ERROR_TRADUCTION_IN_FORMULAS = "ParentEditionPanel.jOptionPane1.traductionErrorInFormulas";
    public static final String ERROR_MESSAGE_TITLE = "ParentEditionPanel.jLabelErrorMessage.title";
    public static final String HELP_PANEL_TITLE = "HelpPanel.title";
    public static final String SETTINGS_TITLE = "SettingsPanel.title";
    public static final String SETTINGS_GENERAL_TITLE = "SettingsPanel.general.title";
    public static final String SETTINGS_GENERAL_TEXT = "SettingsPanel.general.jLabelDefaultDirectory.text";
    public static final String SETTINGS_GENERAL_BUTTON = "SettingsPanel.general.jButtonChangeDirectory.text";
    public static final String SETTINGS_SOLVER_TITLE = "SettingsPanel.solver.title";
    public static final String SETTINGS_SOLVER_TEXT = "SettingsPanel.solver.jLabelSolver.text";
    public static final String SETTINGS_LANGUAGE_TITLE = "SettingsPanel.language.title";
    public static final String SETTINGS_LANGUAGE_TEXT = "SettingsPanel.language.jLabelLanguage.text";
    
}
