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
    JMenuItem jMenuItemEnglish;
    JMenuItem jMenuItemFrench;
    JMenuItem jMenuItemSaveFile;
    JMenuItem jMenuItemHelpEditor;
    
    
    
    public EditionMenuBar(MainFrame parent){
        this.parent = parent;
        jMenuFile = new JMenu();
        jMenuLanguage = new JMenu();
        jMenuHelp = new JMenu();
        
        jMenuItemEnglish = new JMenuItem("English");
        jMenuItemFrench = new JMenuItem("French");
        jMenuItemSaveFile = new JMenuItem();
        jMenuItemSaveFile.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.Event.CTRL_MASK));
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
                jMenuItemSaveModelActionPerformed(evt);
            }
        });
        
        jMenuItemHelpEditor.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemHelpEditorActionPerformed(evt);
            }
        });
        
        jMenuFile.add(jMenuItemSaveFile);
        jMenuLanguage.add(jMenuItemFrench);
        jMenuLanguage.add(jMenuItemEnglish);
        jMenuHelp.add(jMenuItemHelpEditor);
                
        this.add(jMenuFile);
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
    
    private void jMenuItemSaveModelActionPerformed(java.awt.event.ActionEvent evt) {  
        parent.getEditorPanel1().exportHandler();
    }
    
    private void jMenuItemHelpEditorActionPerformed(java.awt.event.ActionEvent evt) {  
        
    }
    
    public void updateLanguage() {
        this.jMenuFile.setText(parent.getLang().getWord(Lang.FRAME_MENU_FILE));
        this.jMenuHelp.setText(parent.getLang().getWord(Lang.FRAME_MENU_HELP));
        this.jMenuLanguage.setText(parent.getLang().getWord(Lang.FRAME_MENU_LANGUAGE));
        this.jMenuItemSaveFile.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_SAVEFILE));
        this.jMenuItemHelpEditor.setText(parent.getLang().getWord(Lang.EDITION_MENUITEM_HELPEDITION));
    }
    
}
