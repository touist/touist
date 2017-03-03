/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.menu;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import touist.TouistProperties;

/**
 *
 * @author Skander
 */
public class AboutPanel extends javax.swing.JPanel {
	public static TouistProperties properties = new TouistProperties();


    /**
     * Creates new form About
     */
    public AboutPanel() {
        initComponents();
        versionNumber.setText(properties.getProperty("version"));
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        Nothing = new javax.swing.JLabel();
        logoLaboratory = new javax.swing.JLabel();
        appTitle = new javax.swing.JLabel();
        appSubtitle = new javax.swing.JLabel();
        linkToGithub = new javax.swing.JButton();
        versionNumber = new javax.swing.JLabel();

        logoLaboratory.setIcon(new javax.swing.ImageIcon(getClass().getResource("/images/irit.png"))); // NOI18N

        appTitle.setFont(new java.awt.Font("Tahoma", 1, 36)); // NOI18N
        appTitle.setText("TouIST");

        appSubtitle.setFont(new java.awt.Font("Tahoma", 0, 18)); // NOI18N
        appSubtitle.setText("TOUlouse Integrated Satisfiability Tool");

        linkToGithub.setForeground(new java.awt.Color(0, 0, 255));
        linkToGithub.setText("github.com/touist/touist");
        linkToGithub.setBorderPainted(false);
        linkToGithub.setContentAreaFilled(false);
        linkToGithub.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                linkToGithubActionPerformed(evt);
            }
        });

        versionNumber.setText("Version 1.0.0");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(Nothing)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(18, 18, 18)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(appTitle)
                                    .addComponent(appSubtitle)
                                    .addComponent(versionNumber))
                                .addGap(0, 0, Short.MAX_VALUE))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addGap(0, 0, Short.MAX_VALUE)
                                .addComponent(linkToGithub)))
                        .addContainerGap())
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(logoLaboratory)
                        .addGap(24, 24, 24))))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(appTitle)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(appSubtitle)
                        .addGap(18, 18, 18)
                        .addComponent(versionNumber)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(linkToGithub)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(logoLaboratory)
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(Nothing)
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void linkToGithubActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_linkToGithubActionPerformed
        if (Desktop.isDesktopSupported()) {
            try {
                URI uri = null;
                try {
                    uri = new URI("https://github.com/touist/touist");
                } catch (URISyntaxException ex) {
                    ex.printStackTrace();
                }
                Desktop.getDesktop().browse(uri);
            } catch (IOException ex) {
                // do nothing
            }
        } else {
            // do nothing
        }
    }//GEN-LAST:event_linkToGithubActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel Nothing;
    private javax.swing.JLabel appSubtitle;
    private javax.swing.JLabel appTitle;
    private javax.swing.JButton linkToGithub;
    private javax.swing.JLabel logoLaboratory;
    private javax.swing.JLabel versionNumber;
    // End of variables declaration//GEN-END:variables
}
