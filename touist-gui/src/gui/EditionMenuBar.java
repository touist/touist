/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.io.IOException;
import java.util.Locale;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

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
    JMenuItem jMenuItemUndo;
    JMenuItem jMenuItemRedo;
    JMenuItem jMenuItemZoomMore;
    JMenuItem jMenuItemZoomLess;
    
    
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
        
        
        
        jMenuFile.add(jMenuItemSaveFile);
        jMenuFile.add(jMenuItemLoadFile);
        jMenuLanguage.add(jMenuItemFrench);
        jMenuLanguage.add(jMenuItemEnglish);
        jMenuHelp.add(jMenuItemHelpEditor);
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
    
    public void updateLanguage() {
        this.jMenuFile.setText(parent.getLang().getWord(Lang.EDITION_MENU_FILE));
        this.jMenuEdit.setText(parent.getLang().getWord(Lang.EDITION_MENU_EDIT));
        this.jMenuView.setText(parent.getLang().getWord(Lang.EDITION_MENU_VIEW));
        this.jMenuLanguage.setText(parent.getLang().getWord(Lang.EDITION_MENU_LANGUAGE));
        this.jMenuHelp.setText(parent.getLang().getWord(Lang.EDITION_MENU_HELP));
        this.jMenuItemSaveFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_SAVEFILE));
        this.jMenuItemLoadFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_LOADFILE));
        this.jMenuItemHelpEditor.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_HELPEDITION));
        this.jMenuItemUndo.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_UNDO));
        this.jMenuItemRedo.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_REDO));
        this.jMenuItemZoomMore.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ZOOMMORE));
        this.jMenuItemZoomLess.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_ZOOMLESS));
    }
    
}
