/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gui.resultsView;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 *
 * @author alexis
 */
public class ExportDialog extends JPanel{
    private JTextField prefix, separator, suffix;
    private JComboBox left, right, predefined;
    private JPanel top, middle, bottom, header;
    private JLabel label;
    public ExportDialog() {
        
        this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        
        header = new JPanel();
        label = new JLabel("Sauvegarder avec la syntaxe suivante:");
        header.add(label);
        this.add(header);
        
        top = new JPanel();
        predefined = new JComboBox(new String[]{"verbose","compact","custom"});
        predefined.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                boolean editable = predefined.getSelectedItem()=="custom";
                prefix.setEditable(editable);
                separator.setEditable(editable);
                suffix.setEditable(editable);
                left.setEnabled(editable);
                right.setEnabled(editable);
                
                if(predefined.getSelectedItem() == "verbose"){
                    prefix.setText("");
                    suffix.setText("");
                    separator.setText(" valuted to ");
                    left.setSelectedIndex(0);
                    right.setSelectedIndex(0);
                }
                if(predefined.getSelectedItem() == "compact"){
                    prefix.setText("");
                    suffix.setText("");
                    separator.setText(" ");
                    left.setSelectedIndex(0);
                    right.setSelectedIndex(0);
                }
            }
        });
        top.add(predefined);
        this.add(top);
        
        middle = new JPanel();
        prefix = new JTextField(10);
        separator = new JTextField(10);
        separator.setText(" valuted to ");
        suffix = new JTextField(10);
        left = new JComboBox(new String[]{"litteral","valeur"});
        right = new JComboBox(new String[]{"valeur","litteral"});
        
        prefix.setEditable(false);
        separator.setEditable(false);
        suffix.setEditable(false);
        left.setEnabled(false);
        right.setEnabled(false);
        
        middle.add(prefix);
        middle.add(left);
        middle.add(separator);
        middle.add(right);
        middle.add(suffix);
        this.add(middle);
        
        
        this.setSize(512, 200);
        
    }
    
    
    public String getPrefixValue() {
        return prefix.getText();
    }
    
    public String getSeparatorValue() {
        return separator.getText();
    }
    
    public String getSuffixValue() {
        return suffix.getText();
    }
    
    public String getLeftValue() {
        return (String)left.getSelectedItem();
    }
    
    public String getRightValue() {
        return (String)right.getSelectedItem();
    }
    
    public String getPredefinedValue() {
        return (String)predefined.getSelectedItem();
    }
    
}
