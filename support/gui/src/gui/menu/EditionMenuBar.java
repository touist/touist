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

package gui.menu;

import gui.Lang;
import gui.MainFrame;

import java.awt.*;
import java.awt.event.ActionEvent;

import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.net.URI;
import java.util.Locale;
import java.util.Scanner;

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
    JMenuItem jMenuItemSaveAsFile;
    JMenuItem jMenuItemLoadFile;
    JMenuItem jMenuItemQuit;
    JMenuItem jMenuItemSolve;
    JMenuItem jMenuItemHelpEditor;
    JMenuItem jMenuItemAbout;
    JMenuItem jMenuItemUndo;
    JMenuItem jMenuItemRedo;
    JMenuItem jMenuItemZoomMore;
    JMenuItem jMenuItemZoomLess;
    JMenuItem jMenuItemSettings;
    JMenuItem jMenuItemManualPdf;
    JMenuItem jMenuItemManualHtml;
    JMenuItem jMenuItemChangelog;
    JMenuItem jMenuItemProjectPage;
    JMenuItem jMenuItemLicense;
    
    
    public EditionMenuBar(MainFrame parent){
        this.parent = parent;
        jMenuFile = new JMenu();    
        jMenuLanguage = new JMenu();
        jMenuHelp = new JMenu();
        jMenuEdit = new JMenu();
        jMenuView = new JMenu();
        
        jMenuItemEnglish = new JMenuItem("English");
        jMenuItemFrench = new JMenuItem("Fran\u00E7ais");
        int meta = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        jMenuItemSaveFile = new JMenuItem();
        jMenuItemSaveFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, meta));
        jMenuItemSaveAsFile = new JMenuItem();
        jMenuItemSaveAsFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, KeyEvent.SHIFT_MASK + meta));
        jMenuItemLoadFile = new JMenuItem();
        jMenuItemLoadFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, meta));
        jMenuItemQuit = new JMenuItem();
        jMenuItemQuit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, meta));
        jMenuItemSolve = new JMenuItem();
        jMenuItemSolve.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, meta));
        
        jMenuItemUndo = new JMenuItem();
        jMenuItemUndo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, meta));
        jMenuItemRedo = new JMenuItem();
        jMenuItemRedo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y, meta));
        
        jMenuItemZoomMore = new JMenuItem();
        jMenuItemZoomMore.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, meta));
        jMenuItemZoomLess = new JMenuItem();
        jMenuItemZoomLess.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, meta));
        
        
        jMenuItemHelpEditor = new JMenuItem();
        jMenuItemAbout = new JMenuItem();
        
        jMenuItemSettings = new JMenuItem();

        jMenuItemManualPdf = new JMenuItem();
        jMenuItemManualHtml = new JMenuItem();
        jMenuItemChangelog = new JMenuItem();
        jMenuItemProjectPage = new JMenuItem();
        jMenuItemLicense = new JMenuItem();
        
        jMenuItemEnglish.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemEnglishActionPerformed(evt);
            }
        });
        
        jMenuItemFrench.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemFrenchActionPerformed(evt);
            }
        });
        
        jMenuItemSaveFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) { jMenuItemSaveFileActionPerformed(evt); }
        });

        jMenuItemSaveAsFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) { jMenuItemSaveAsFileActionPerformed(evt); }
        });
        
        jMenuItemLoadFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemLoadFileActionPerformed(evt);
            }
        });
        jMenuItemQuit.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {jMenuItemQuitActionPerformed(evt);}
        });
        jMenuItemSolve.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) { jMenuItemSolveActionPerformed(evt); }
        });
        
        jMenuItemHelpEditor.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemHelpEditorActionPerformed(evt);
            }
        });
        
        jMenuItemAbout.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemAboutActionPerformed(evt);
            }
        });
        
        jMenuItemUndo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemUndoActionPerformed(evt);
            }
        });
        
        jMenuItemRedo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemRedoActionPerformed(evt);
            }
        });
        
        jMenuItemZoomMore.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemZoomMoreActionPerformed(evt);
            }
        });
        
        jMenuItemZoomLess.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemZoomLessActionPerformed(evt);
            }
        });
        
        jMenuItemSettings.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jMenuItemSettingsActionPerformed(evt);
            }
        });

        jMenuItemManualPdf.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                if(Desktop.isDesktopSupported())
                    try {Desktop.getDesktop().browse(new URI("https://www.irit.fr/touist/doc/reference-manual.html"));}
                    catch (Exception e) { System.err.println(e.getMessage());}
            }
        });
        jMenuItemManualHtml.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                if(Desktop.isDesktopSupported())
                    try {Desktop.getDesktop().browse(new URI("https://www.irit.fr/touist/doc/reference-manual.pdf"));}
                    catch (Exception e) { System.err.println(e.getMessage());}
            }
        });
        jMenuItemChangelog.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                if(Desktop.isDesktopSupported())
                    try {Desktop.getDesktop().browse(new URI("https://github.com/touist/touist/blob/master/CHANGELOG"));}
                    catch (Exception e) { System.err.println(e.getMessage());}
            }
        });
        jMenuItemProjectPage.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                if(Desktop.isDesktopSupported())
                    try {Desktop.getDesktop().browse(new URI("https://www.irit.fr/touist"));}
                    catch (Exception e) { System.err.println(e.getMessage());}
            }
        });
        jMenuItemLicense.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                if(Desktop.isDesktopSupported())
                    try {Desktop.getDesktop().browse(new URI("https://github.com/touist/touist/blob/master/LICENSE"));}
                    catch (Exception e) { System.err.println(e.getMessage());}
            }
        });
        

        jMenuFile.add(jMenuItemSaveFile);
        jMenuFile.add(jMenuItemSaveAsFile);
        jMenuFile.add(jMenuItemLoadFile);
        jMenuFile.add(jMenuItemSolve);
        jMenuFile.add(jMenuItemQuit);
        jMenuLanguage.add(jMenuItemFrench);
        jMenuLanguage.add(jMenuItemEnglish);
        jMenuHelp.add(jMenuItemManualPdf);
        jMenuHelp.add(jMenuItemManualHtml);
        jMenuHelp.add(jMenuItemChangelog);
        jMenuHelp.add(jMenuItemProjectPage);
        jMenuHelp.add(jMenuItemLicense);
        jMenuHelp.add(jMenuItemSettings);
        jMenuHelp.add(jMenuItemAbout);
        jMenuHelp.add(jMenuItemHelpEditor);

        jMenuEdit.add(jMenuItemUndo);
        jMenuEdit.add(jMenuItemRedo);
        jMenuView.add(jMenuItemZoomMore);
        jMenuView.add(jMenuItemZoomLess);
        
        this.add(jMenuFile);
        this.add(jMenuEdit);
        this.add(jMenuView);
        this.add(jMenuLanguage);
        this.add(jMenuHelp);
        
        updateLanguage();
        
    }
    
    private void jMenuItemEnglishActionPerformed(ActionEvent evt) {
        parent.setLanguage(Locale.ENGLISH);
        parent.updateLanguage();
        System.out.println("Language set to ENGLISH");
    }                                           
    
    private void jMenuItemFrenchActionPerformed(ActionEvent evt) { 
        parent.setLanguage(Locale.FRENCH);        
        parent.updateLanguage();
        System.out.println("Language set to FRENCH");
    }
    
    private void jMenuItemSaveFileActionPerformed(ActionEvent evt) {  
        parent.getEditorPanel1().saveHandler(false);
    }
    private void jMenuItemSaveAsFileActionPerformed(ActionEvent evt) {
        parent.getEditorPanel1().saveHandler(true);
    }
    
    private void jMenuItemLoadFileActionPerformed(ActionEvent evt) {  
        parent.getEditorPanel1().openHandler();
    }
    private void jMenuItemQuitActionPerformed(ActionEvent evt) {
        parent.dispatchEvent(new WindowEvent(parent, WindowEvent.WINDOW_CLOSING));
    }
    private void jMenuItemSolveActionPerformed(ActionEvent evt) {
        parent.getEditorPanel1().solve();
    }
    
    private void jMenuItemHelpEditorActionPerformed(ActionEvent evt) {  
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
    
    private void jMenuItemAboutActionPerformed(ActionEvent evt) {
        JOptionPane.showMessageDialog(this, 
                new AboutPanel(), 
                parent.getLang().getWord(Lang.EDITION_MENUITEM_ABOUT), 
                JOptionPane.PLAIN_MESSAGE);
    }
    
    private void jMenuItemUndoActionPerformed(ActionEvent evt) {  
        parent.getEditorPanel1().undo();
    }
    
    private void jMenuItemRedoActionPerformed(ActionEvent evt) {  
        parent.getEditorPanel1().redo();
    }
    
    private void jMenuItemZoomMoreActionPerformed(ActionEvent evt) {  
        parent.getEditorPanel1().zoom(1);
    }
    
    private void jMenuItemZoomLessActionPerformed(ActionEvent evt) {  
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
        this.jMenuItemSaveAsFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_SAVEASFILE));
        this.jMenuItemLoadFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_LOADFILE));
        this.jMenuItemQuit.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemQuit.text"));
        this.jMenuItemSolve.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemSolve"));
        this.jMenuItemHelpEditor.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_HELPEDITION));
        this.jMenuItemAbout.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ABOUT));
        this.jMenuItemUndo.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_UNDO));
        this.jMenuItemRedo.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_REDO));
        this.jMenuItemZoomMore.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ZOOMMORE));
        this.jMenuItemZoomLess.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ZOOMLESS));
        this.jMenuItemSettings.setText(parent.getLang().getWord(Lang.SETTINGS_TITLE));

        this.jMenuItemManualPdf.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemManualPdf"));
        this.jMenuItemManualHtml.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemManualHtml"));
        this.jMenuItemChangelog.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemChangelog"));
        this.jMenuItemProjectPage.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemProjectPage"));
        this.jMenuItemLicense.setText(parent.getLang().getWord("EditionMenuBar.jMenuItemLicense"));
    }
}
