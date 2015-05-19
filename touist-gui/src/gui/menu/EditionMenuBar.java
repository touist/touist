/*
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice GUI.
 *
 * https://github.com/olzd/touist
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

package gui.menu;

import gui.Lang;
import gui.MainFrame;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.Locale;
import java.util.Scanner;
import javax.swing.JDialog;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.text.Document;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

/**
 *
 * @author alexis
 */
public class EditionMenuBar extends JMenuBar {
    
    MainFrame parent;
    JMenu jMenuFile;
    JMenu jMenuLanguage;
    JMenu jMenuHelp;
    JMenu jMenuEdit;
    JMenu jMenuView;
    JMenuItem jMenuItemEnglish;
    JMenuItem jMenuItemFrench;
    JMenuItem jMenuItemSaveFile;
    JMenuItem jMenuItemLoadFile;
    JMenuItem jMenuItemHelpEditor;
    JMenuItem jMenuItemAbout;
    JMenuItem jMenuItemUndo;
    JMenuItem jMenuItemRedo;
    JMenuItem jMenuItemZoomMore;
    JMenuItem jMenuItemZoomLess;
    JMenuItem jMenuItemSettings;
    
    
    public EditionMenuBar(MainFrame parent){
        this.parent = parent;
        jMenuFile = new JMenu();    
        jMenuLanguage = new JMenu();
        jMenuHelp = new JMenu();
        jMenuEdit = new JMenu();
        jMenuView = new JMenu();
        
        
        jMenuItemEnglish = new JMenuItem("English");
        jMenuItemFrench = new JMenuItem("Français");
        
        jMenuItemSaveFile = new JMenuItem();
        jMenuItemSaveFile.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.Event.CTRL_MASK));
        jMenuItemLoadFile = new JMenuItem();
        jMenuItemLoadFile.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.Event.CTRL_MASK));
        
        jMenuItemUndo = new JMenuItem();
        jMenuItemUndo.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.Event.CTRL_MASK));
        jMenuItemRedo = new JMenuItem();
        jMenuItemRedo.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Y, java.awt.Event.CTRL_MASK));
        
        jMenuItemZoomMore = new JMenuItem();
        jMenuItemZoomMore.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_PLUS, java.awt.Event.CTRL_MASK));
        jMenuItemZoomLess = new JMenuItem();
        jMenuItemZoomLess.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_MINUS, java.awt.Event.CTRL_MASK));
        
        
        jMenuItemHelpEditor = new JMenuItem();
        jMenuItemAbout = new JMenuItem();
        
        jMenuItemSettings = new JMenuItem();
        
        jMenuItemEnglish.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemEnglishActionPerformed(evt);
            }
        });
        
        jMenuItemFrench.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFrenchActionPerformed(evt);
            }
        });
        
        jMenuItemSaveFile.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemSaveFileActionPerformed(evt);
            }
        });
        
        jMenuItemLoadFile.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemLoadFileActionPerformed(evt);
            }
        });
        
        jMenuItemHelpEditor.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemHelpEditorActionPerformed(evt);
            }
        });
        
        jMenuItemAbout.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemAboutActionPerformed(evt);
            }
        });
        
        jMenuItemUndo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemUndoActionPerformed(evt);
            }
        });
        
        jMenuItemRedo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemRedoActionPerformed(evt);
            }
        });
        
        jMenuItemZoomMore.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemZoomMoreActionPerformed(evt);
            }
        });
        
        jMenuItemZoomLess.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemZoomLessActionPerformed(evt);
            }
        });
        
        jMenuItemSettings.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemSettingsActionPerformed(evt);
            }
        });
        
        
        
        jMenuFile.add(jMenuItemSaveFile);
        jMenuFile.add(jMenuItemLoadFile);
        jMenuLanguage.add(jMenuItemFrench);
        jMenuLanguage.add(jMenuItemEnglish);
        jMenuHelp.add(jMenuItemHelpEditor);
        jMenuHelp.add(jMenuItemSettings);
        jMenuHelp.add(jMenuItemAbout);
        jMenuEdit.add(jMenuItemUndo);
        jMenuEdit.add(jMenuItemRedo);
        jMenuView.add(jMenuItemZoomMore);
        jMenuView.add(jMenuItemZoomLess);
        
        this.add(jMenuFile);
        this.add(jMenuEdit);
        //this.add(jMenuView);
        this.add(jMenuLanguage);
        this.add(jMenuHelp);
        
        updateLanguage();
        
    }
    
    private void jMenuItemEnglishActionPerformed(java.awt.event.ActionEvent evt) {
        parent.setLanguage(Locale.ENGLISH);
        parent.updateLanguage();
        System.out.println("Language set to ENGLISH");
    }                                           
    
    private void jMenuItemFrenchActionPerformed(java.awt.event.ActionEvent evt) { 
        parent.setLanguage(Locale.FRENCH);        
        parent.updateLanguage();
        System.out.println("Language set to FRENCH");
        
    }
    
    private void jMenuItemSaveFileActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().exportHandler();
    }
    
    private void jMenuItemLoadFileActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().importHandler();
    }
    
    private void jMenuItemHelpEditorActionPerformed(java.awt.event.ActionEvent evt) {  
		// create jeditorpane
        JEditorPane jEditorPane = new JEditorPane();
        
        // make it read-only
        jEditorPane.setEditable(false);
        
        // create a scrollpane; modify its attributes as desired
        JScrollPane scrollPane = new JScrollPane(jEditorPane);
        
        // add an html editor kit
        HTMLEditorKit kit = new HTMLEditorKit();
        jEditorPane.setEditorKit(kit);
        
        // add some styles to the html
        StyleSheet styleSheet = kit.getStyleSheet();
        styleSheet.addRule("body {color:#000; font-family:times; margin: 4px; }");
        styleSheet.addRule("h1 {color: blue;}");
        styleSheet.addRule("h2 {color: #ff0000;}");
        styleSheet.addRule("pre {font : 10px monaco; color : black; background-color : #fafafa; }");

        // create a document, set it on the jeditorpane, then add the html
        Document doc = kit.createDefaultDocument();
        jEditorPane.setDocument(doc);
        String text = new Scanner(this.getClass().getResourceAsStream("/help/help.html"), "UTF-8").useDelimiter("\\A").next();
        jEditorPane.setText(text);

        // now add it all to a frame
        JFrame j = new JFrame(parent.getLang().getWord(Lang.HELP_PANEL_TITLE));
        
        j.getContentPane().add(scrollPane, BorderLayout.CENTER);

        // make it easy to close the application
        //j.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        // display the frame
        j.setSize(new Dimension(500,600));
        
        // pack it, if you prefer
        j.pack();
        
        // center the jframe, then make it visible
        j.setLocationRelativeTo(null);
        j.setVisible(true);
    }
    
    private void jMenuItemAboutActionPerformed(java.awt.event.ActionEvent evt) {
        JOptionPane.showMessageDialog(this, 
                new AboutPanel(), 
                parent.getLang().getWord(Lang.EDITION_MENUITEM_ABOUT), 
                JOptionPane.PLAIN_MESSAGE);
    }
    
    private void jMenuItemUndoActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().undo();
    }
    
    private void jMenuItemRedoActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().redo();
    }
    
    private void jMenuItemZoomMoreActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().zoom(1);
    }
    
    private void jMenuItemZoomLessActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().zoom(-1);
    }


    private void jMenuItemSettingsActionPerformed(ActionEvent evt) {
        SettingsPanel settingsPanel = new SettingsPanel(parent);
        settingsPanel.updateLanguage();
        JOptionPane.showMessageDialog(parent, 
                settingsPanel, 
                parent.getLang().getWord(Lang.SETTINGS_TITLE), 
                JOptionPane.PLAIN_MESSAGE);
    }
    
    public void updateLanguage() {
        this.jMenuFile.setText(parent.getLang().getWord(Lang.EDITION_MENU_FILE));
        this.jMenuEdit.setText(parent.getLang().getWord(Lang.EDITION_MENU_EDIT));
        this.jMenuView.setText(parent.getLang().getWord(Lang.EDITION_MENU_VIEW));
        this.jMenuLanguage.setText(parent.getLang().getWord(Lang.EDITION_MENU_LANGUAGE));
        this.jMenuHelp.setText(parent.getLang().getWord(Lang.EDITION_MENU_HELP));
        this.jMenuItemSaveFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_SAVEFILE));
        this.jMenuItemLoadFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_LOADFILE));
        this.jMenuItemHelpEditor.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_HELPEDITION));
        this.jMenuItemAbout.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ABOUT));
        this.jMenuItemUndo.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_UNDO));
        this.jMenuItemRedo.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_REDO));
        this.jMenuItemZoomMore.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ZOOMMORE));
        this.jMenuItemZoomLess.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ZOOMLESS));
        this.jMenuItemSettings.setText(parent.getLang().getWord(Lang.SETTINGS_TITLE));
    }
    
}
