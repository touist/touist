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

package gui.editionView;

import entity.Model;
import gui.AbstractComponentPanel;
import gui.Lang;
import gui.MainFrame;
import gui.SolverSelection;
import gui.SolverSelection.SolverType;
import gui.State;

import java.awt.AWTException;
import java.awt.FileDialog;
import java.awt.Robot;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.*;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.ListIterator;
import java.util.Map;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.kordamp.ikonli.fontawesome.FontAwesomeIkonHandler;
import org.kordamp.ikonli.swing.FontIcon;
import solution.SolverExecutionException;
import solution.SolverQBF;
import solution.SolverSMT;
import solution.SolverTestSAT4J;
import touist.TouIST;
import translation.TranslationError;

import static javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE;

/**
 *
 * @author Skander
 */
public class ParentEditionPanel extends AbstractComponentPanel {

	private static final int ERROR_MESSAGE_MAX_LENGTH = 76;
    private String jLabelErrorMessageText;
    private Thread testThread;
    private Path openedFile = null;
    protected MainFrame mainframe = null;


    public void setOpenedFile(Path file) {
        String windowTitle = mainframe.getLang().getWord(Lang.FRAME_TITLE) + " "
                + touist.TouIST.properties.getProperty("version");
        if(file == null) {
            mainframe.setTitle(windowTitle);
        } else {
            mainframe.setTitle(windowTitle + " - " + file.toString());
        }
        openedFile = file;
    }

    public boolean hasUnsavedChanges() {
        if(openedFile == null) {
            return ! editor.getEditorTextArea().getText().isEmpty();
        } else {
            String textInFile = "";
            try {
                textInFile = editor.open(openedFile.toString());
            } catch (IOException e) {
                return false;
            }
            // If the texts are the same between the editor and the file, then
            // we have no unsaved files (= we return false).
            return ! editor.getEditorTextArea().getText().equals(textInFile);
        }
    }

    /**
     * Creates new form FormulasPanel
     */
    public ParentEditionPanel(MainFrame parent) {
        initComponents();
        mainframe = parent;
        testThread = new Thread();
        
        editor.initPalette();
        jLabelErrorMessageText = "";
        bottomMessage.setText(jLabelErrorMessageText);
        
        selectSatOrSmt.removeAllItems();
        for (SolverType solverType : SolverSelection.SolverType.values()) {
            selectSatOrSmt.addItem(solverType);
        }

        // Nice icons!
        FontIcon f = new FontIcon();
        f.setIkon(new FontAwesomeIkonHandler().resolve("fa-save"));
        f.setIconSize(18);
        exportButton.setIcon(f);
        f = new FontIcon();
        f.setIkon(new FontAwesomeIkonHandler().resolve("fa-folder-open"));
        f.setIconSize(18);
        importButton.setIcon(f);
        f = new FontIcon();
        f.setIkon(new FontAwesomeIkonHandler().resolve("fa-gears"));
        f.setIconSize(18);
        solveButton.setIcon(f);

        // On macOS, handle the drag-and-drop for opening files
        if(System.getProperty("os.name").toLowerCase().contains("mac")) {
            com.apple.eawt.Application a = com.apple.eawt.Application.getApplication();
            a.setOpenFileHandler(new com.apple.eawt.OpenFilesHandler() {
                @Override
                public void openFiles(com.apple.eawt.AppEvent.OpenFilesEvent e) {
                    if (e.getFiles().get(0) instanceof File) {
                        if(hasUnsavedChanges()) {
                            int confirmed = JOptionPane.showConfirmDialog(null,
                                    getFrame().getLang().getWord("ParentEditionPanel.saveOrLooseOnOpen"),
                                    "", JOptionPane.YES_NO_CANCEL_OPTION);
                            if (confirmed == JOptionPane.YES_OPTION) {
                                saveHandler(false);
                            } else if (confirmed == JOptionPane.NO_OPTION) {
                            } else if (confirmed == JOptionPane.CANCEL_OPTION) {
                                return;
                            }
                        }
                        open(((File) e.getFiles().get(0)).getAbsolutePath());
                    }
                }
            });
        }

        // Ask the user if he wants to save before quitting
        parent.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        parent.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                if(hasUnsavedChanges()) {
                    int confirmed = JOptionPane.showConfirmDialog(null,
                            getFrame().getLang().getWord("ParentEditionPanel.saveOrLooseOnQuit"),
                            "", JOptionPane.YES_NO_CANCEL_OPTION);
                    if (confirmed == JOptionPane.YES_OPTION) {
                        saveHandler(false);
                        System.exit(0);
                    } else if (confirmed == JOptionPane.NO_OPTION) {
                        System.exit(0);
                    } else if (confirmed == JOptionPane.CANCEL_OPTION) {
                        return;
                    }
                } else {
                    System.exit(0);
                }
            }
        });
    }
    
    public void updateComboBoxSelectedSolver() {
        selectSatOrSmt.setSelectedItem(getFrame().getSolverSelection().getSelectedSolver());
    }
    
    /**
     * Update jLabelErrorMessage's text and keep it at ERROR_MESSAGE_MAX_LENGTH.
     * @param text The text used to set the label's text.
     */
    private void setJLabelErrorMessageText(String text) {
        jLabelErrorMessageText = text;
        if (text.length() < ERROR_MESSAGE_MAX_LENGTH) {
            bottomMessage.setText(text);
        } else {
            bottomMessage.setText(text.substring(0, ERROR_MESSAGE_MAX_LENGTH-18) + "... (click to view)");
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        fileChooser = new javax.swing.JFileChooser();
        popupMessage = new javax.swing.JOptionPane();
        editor = new gui.editionView.EditionPanel();
        solveButton = new javax.swing.JButton();
        importButton = new javax.swing.JButton();
        bottomMessage = new javax.swing.JLabel();
        cursorPosition = new javax.swing.JLabel();
        selectSatOrSmt = new javax.swing.JComboBox();
        exportButton = new javax.swing.JButton();

        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.addChoosableFileFilter(new FileNameExtensionFilter("Touistl files(touistl)","touistl"));

        solveButton.setText("Solve");
        solveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                solveButtonActionPerformed(evt);
            }
        });

        importButton.setText("Import");
        importButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                importButtonActionPerformed(evt);
            }
        });

        bottomMessage.setForeground(new java.awt.Color(255, 0, 0));
        bottomMessage.setText("<Error message>");
        bottomMessage.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                bottomMessageMouseClicked(evt);
            }
        });

        cursorPosition.setText("1:1");

        selectSatOrSmt.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "SAT", "SMT" }));
        selectSatOrSmt.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectSatOrSmtActionPerformed(evt);
            }
        });

        exportButton.setText("Export");
        exportButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exportButtonActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(cursorPosition)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(bottomMessage)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 497, Short.MAX_VALUE)
                        .addComponent(exportButton)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(importButton)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(solveButton)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(selectSatOrSmt, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addComponent(editor, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))

                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(editor, javax.swing.GroupLayout.DEFAULT_SIZE, 511, Short.MAX_VALUE)
                .addGap(4, 4, 4)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(solveButton)
                    .addComponent(importButton)
                    .addComponent(cursorPosition)
                    .addComponent(bottomMessage)
                    .addComponent(selectSatOrSmt, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(exportButton))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private void importButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_importButtonActionPerformed
        switch(((MainFrame)(getRootPane().getParent())).state) {
            case EDITION :
                setState(State.EDITION);
                openHandler();
                break;
            case EDITION_ERROR :
                setState(State.EDITION_ERROR);
                openHandler();
                break;
            case NO_RESULT :
                // impossible
                break;
            case SINGLE_RESULT :
                // impossible
                break;
            case FIRST_RESULT :
                // impossible
                break;
            case MIDDLE_RESULT :
                // impossible
                break;
            case LAST_RESULT :
                // impossible
                break;
            default : 
                System.out.println("Undefined action set for the state : " + getState());
        }
    }//GEN-LAST:event_importButtonActionPerformed

    /**
     * For java jre 1.6 and 1.7 compatibility (p.isAlive() is java jre >= 1.8)
     */
	private boolean isAlive(Process process) {
	    try {
	        process.exitValue();
	        return false;
	    } catch (Exception e) {
	        return true;
	    }
	}
    
	private boolean isStopInsteadOfTest = false;
    private void solveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_solveButtonActionPerformed
        solve();
    }//GEN-LAST:event_solveButtonActionPerformed

    public void solve() {
        switch(((MainFrame)(getRootPane().getParent())).state) {
            case EDITION :
                setJLabelErrorMessageText("");

                this.solveButton.setText(isStopInsteadOfTest
                        ?getFrame().getLang().getWord("ParentEditionPanel.testButton.text")
                        :getFrame().getLang().getWord("ParentEditionPanel.stopButton.text"));
                isStopInsteadOfTest = (isStopInsteadOfTest)?false:true;

                if(testThread.isAlive()) {
                    testThread.interrupt();
                }

                Process p = null;

                if (getFrame().getSolverSelection().getSelectedSolver() == SolverSelection.SolverType.SAT) {
                    p = getFrame().getTranslatorSAT().getP();
                } else {
                    p = getFrame().getTranslatorSMT().getP();
                }

                if(p != null && isAlive(p)){
                    p.destroy();
                }

                if(!isStopInsteadOfTest)
                    break;

                Runnable r = new Runnable() {
                    public void run() {
                        State state = initResultView();
                        solveButton.setText(getFrame().getLang().getWord("ParentEditionPanel.testButton.text"));
                        isStopInsteadOfTest = false;
                        if (state != State.EDITION) {
                            setState(state);
                            getFrame().setViewToResults();
                        }
                    }
                };

                testThread = new Thread(r);
                testThread.start();
                break;
            case EDITION_ERROR :
                // interdit
                break;
            case NO_RESULT :
                // impossible
                break;
            case SINGLE_RESULT :
                // impossible
                break;
            case FIRST_RESULT :
                // impossible
                break;
            case MIDDLE_RESULT :
                // impossible
                break;
            case LAST_RESULT :
                // impossible
                break;
            default :
                System.out.println("Undefined action set for the state : " + getState());
        }
    }

    private void exportButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exportButtonActionPerformed
        switch(((MainFrame)(getRootPane().getParent())).state) {
            case EDITION :
                setState(State.EDITION);
                saveHandler(false);
                break;
            case EDITION_ERROR :
                setState(State.EDITION_ERROR);
                saveHandler(false);
                break;
            case NO_RESULT :
                // impossible
                break;
            case SINGLE_RESULT :
                // impossible
                break;
            case FIRST_RESULT :
                // impossible
                break;
            case MIDDLE_RESULT :
                // impossible
                break;
            case LAST_RESULT :
                // impossible
                break;
            default : 
                System.out.println("Undefined action set for the state : " + getState());
        }    }//GEN-LAST:event_exportButtonActionPerformed

    private void bottomMessageMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_bottomMessageMouseClicked
        if (jLabelErrorMessageText.length() > ERROR_MESSAGE_MAX_LENGTH) {
            JOptionPane.showMessageDialog(this,
                    jLabelErrorMessageText, 
                    getFrame().getLang().getWord(Lang.ERROR_MESSAGE_TITLE),
                    JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_bottomMessageMouseClicked

    private void selectSatOrSmtActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectSatOrSmtActionPerformed
        try {
            if (selectSatOrSmt.getSelectedItem() instanceof SolverType) {
                getFrame().getSolverSelection().setSelectedSolver((SolverType)(selectSatOrSmt.getSelectedItem()));
            }
        } catch (NullPointerException ex) {
        }
    }//GEN-LAST:event_selectSatOrSmtActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel bottomMessage;
    private javax.swing.JLabel cursorPosition;
    private gui.editionView.EditionPanel editor;
    private javax.swing.JButton exportButton;
    private javax.swing.JFileChooser fileChooser;
    private javax.swing.JButton importButton;
    private javax.swing.JOptionPane popupMessage;
    private javax.swing.JComboBox selectSatOrSmt;
    private javax.swing.JButton solveButton;
    // End of variables declaration//GEN-END:variables

    public void openHandler() {
        if(hasUnsavedChanges()) {
            int confirmed = JOptionPane.showConfirmDialog(null,
                    getFrame().getLang().getWord("ParentEditionPanel.saveOrLooseOnOpen"),
                    "", JOptionPane.YES_NO_CANCEL_OPTION);
            if (confirmed == JOptionPane.YES_OPTION) {
                saveHandler(false);
            } else if (confirmed == JOptionPane.NO_OPTION) {
            } else if (confirmed == JOptionPane.CANCEL_OPTION) {
                return;
            }
        }

		FileDialog d = new FileDialog(getFrame());
		d.setDirectory(touist.TouIST.getWhereToSave());
    	d.setMode(FileDialog.LOAD);
    	d.setVisible(true);
    	
        editor.setText("");

        if (d.getFile() != null) 
        {
        	String path = d.getDirectory() + d.getFile();
            open(path);
        }
    }
    
    public void open(String filepath) {
    	System.out.println("Opening file '"+filepath+"'");
    	Path file = FileSystems.getDefault().getPath(filepath);
        try {
            editor.loadIntoTextEditor(file.toString());
            setOpenedFile(file);
        } catch(Exception e) {
            System.err.println("Failed to load file: " + file.toString() + "\n" + e.getMessage());
            showErrorMessage(e,"Failed to load file: '" + file.toString() + "'\n" + e.toString(),"");
        }
    }

    /**
     *
     * @param saveAs if set to true, will always behave like 'save as';
     *               if set to false, will behave either as a 'save' (or save as
     *               if no fil is already opened)
     */
    public void saveHandler(boolean saveAs) {

        if(saveAs || openedFile == null) {
        	FileDialog d = new FileDialog(getFrame());
        	d.setFile("*.touist");
        	if(openedFile == null) {
        		d.setDirectory(touist.TouIST.getWhereToSave());
        	} else {
        		d.setDirectory(openedFile.getParent().toString());
        	}
        	
        	d.setMode(FileDialog.SAVE);
        	d.setVisible(true);
        	if(d.getFile() != null){
                setOpenedFile(FileSystems.getDefault().getPath(d.getDirectory() + File.separator + d.getFile()));
           }
        }

        try {
            if(openedFile != null) {
                editor.saveToFile(openedFile.toString());
            }
        } catch (IOException e) {
            String warningWindowTitle = getFrame().getLang().getWord(Lang.EDITION_EXPORT_FAILURE_TITLE);
            String warningWindowText = getFrame().getLang().getWord(Lang.EDITION_EXPORT_FAILURE_TEXT);
            JOptionPane.showMessageDialog(this,warningWindowText,warningWindowTitle,JOptionPane.ERROR_MESSAGE);
        }
    }
    
    private void showErrorMessage(String message, String title) {
        popupMessage.showMessageDialog(getParent(), 
                        message, 
                        title, 
                        JOptionPane.ERROR_MESSAGE);
    }
    
    private void showErrorMessage(Exception e, String message, String title) {
        showErrorMessage(message, title);
        
        /*
         * To enable the exceptions stacks to be filed into log.txt, I added 
         * a system property "debug" that must be added when calling java:
         *     java -Ddebug=true -jar touist.jar
         * 
         * Or you can simply run `ant run`, it will enable that flag.
         */
        if(System.getProperty("debug") == "true") {
        	FileWriter writer = null;
        	String texte = String.valueOf(e.getStackTrace()) + "\n" + "######################";
        	try{
        		writer = new FileWriter("log.txt", true);
        		writer.write(texte,0,texte.length());
        	}catch(IOException ex){
        		ex.printStackTrace();
        	}finally{
        		if(writer != null){
        			try {
        				writer.close();
        			} catch (IOException ex) {
        				e.printStackTrace();
        			}
          }
        }
        }
    }
    

    private State initResultView() {
        // Initialisation de BaseDeClause
        editor.setText(editor.getText());
        
        /*
        Faire appel au solveur avec les fichiers générés par le traducteur
        calculer un model
        Si un model suivant existe
        alors passer a l'état FIRST_RESULT
        sinon passer à l'état SINGLE_RESULT
        Si aucun model n'existe alors passer a l'état NO_RESULT
        */
        String path = touist.TouIST.checkPath(touist.TouIST.getWhereToSave() + File.separator + "temp.touist");
        File touistFile = new File(path); //TODO se mettre d'accord sur un nom standard ou ajouter a Translator et BaseDeClause des méthode pour s'échange de objets File
        String errorMessage;
        
        
        try {
            editor.saveToFile(touistFile.getAbsolutePath());
        } catch (IOException ex) {
            errorMessage = "Couldn't create file '" + touistFile.getAbsolutePath() + "':\n"+ex.getMessage()+"\nPath: "+path;
            showErrorMessage(errorMessage, getFrame().getLang().getWord(Lang.ERROR_TRADUCTION));
            return State.EDITION;
        }
        
        // Button "Solve" now displays "Translating...
        solveButton.setText("Translating");

        if (getFrame().getEditorPanel1().editor.getEditorTextArea().getSyntaxEditingStyle() == "sat") {
           
            try {
            	boolean ok = getFrame().getTranslatorSAT().translate(touistFile.getAbsolutePath());
            	errorMessage = "";
                for (TranslationError error : getFrame().getTranslatorSAT().getErrors()) {
                        errorMessage += error + "\n";
                }
                setJLabelErrorMessageText(errorMessage);
                
                this.editor.getParser().linterFromExisting(getFrame().getTranslatorSAT().getErrors());
                this.editor.getEditorTextArea().forceReparsing(0);
                if(errorMessage != "") {
                    System.out.println("touist returned errors:\n"+ errorMessage + "\n");
                }   
                if(!ok) {
                    return State.EDITION;
                }
                File f = touistFile;
                f.deleteOnExit();
            } catch (IOException ex) {
                ex.printStackTrace();
                errorMessage = "The translator returned an IOException: \n"+ex.getMessage()+"\n"+
                	"Check that touist is in "+TouIST.getTouistExternalDir()+" and that it has the right permissions.";
                showErrorMessage(ex, errorMessage, getFrame().getLang().getWord(Lang.ERROR_TRADUCTION));
                return State.EDITION;
            } catch (InterruptedException ex) {
                System.out.println("touist has been stopped");
                return State.EDITION;
            }

            solveButton.setText("Solving");

            //Add CurrentPath/dimacsFile
            String translatedFilePath = getFrame().getTranslatorSAT().getDimacsFilePath();
            Map<Integer, String> literalsMap = getFrame().getTranslatorSAT().getLiteralsMap();
            getFrame().setSolver(new SolverTestSAT4J(translatedFilePath, literalsMap));

            try {
                getFrame().getSolver().launch();
            } catch (IOException ex) {
                ex.printStackTrace();
                errorMessage = "Couldn't launch solver.";
                showErrorMessage(ex, errorMessage, "Solver error");
                return State.EDITION;
            } catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}    

            // Si il y a au moins un model
            try {
                ListIterator<Model> iter = getFrame().getSolver().getModelList().iterator();
                if(!iter.hasNext()) {
                    System.out.println("This problem is unsatisfiable");
                    errorMessage = "There is no solution";
                    showErrorMessage(errorMessage, "Solver error");
                    return State.EDITION;
                }    
                getFrame().updateResultsPanelIterator(iter);
                /**
                 * Si il y a plus d'un model, alors passer à l'état FIRST_RESULT
                 * sinon passer à l'état SINGLE_RESULT
                 */
                if (iter.hasNext()) {
                    getFrame().setResultView(iter.next());
                    if (iter.hasNext()) {
                       //iter.previous();
                        return State.FIRST_RESULT;
                    } else {
                        //iter.previous();
                        return State.SINGLE_RESULT;
                    }
                } else {
                    return State.SINGLE_RESULT;
                }
            } catch (SolverExecutionException ex) {
                ex.printStackTrace();
                errorMessage = "The solver encountered a problem.";
                showErrorMessage(ex, errorMessage, "Solver error");
                return State.EDITION;
            }
        } else if (getFrame().getEditorPanel1().editor.getEditorTextArea().getSyntaxEditingStyle() == "smt") {
            try {
            	solveButton.setText("Translating");
                String logic = "";
                switch(getFrame().getSolverSelection().getSelectedSolver()) {
                    case QF_IDL : 
                        logic = "QF_IDL";
                        break;
                    case QF_LIA :
                        logic = "QF_LIA";
                        break;
                    case QF_LRA :
                        logic = "QF_LRA";
                        break;
                    case QF_RDL :
                        logic = "QF_RDL";
                        break;
                    default :
                }
                boolean ok = getFrame().getTranslatorSMT().translate(touistFile.getAbsolutePath(), logic);
                errorMessage = "";
                for (TranslationError error : getFrame().getTranslatorSMT().getErrors()) {
                	errorMessage += error + "\n";
                }
                setJLabelErrorMessageText(errorMessage);
                if(errorMessage != "") {
                    System.out.println("touist returned errors:\n"+ errorMessage + "\n");
                }
                if(!ok) {
   
                    return State.EDITION;
                }
                
                solveButton.setText("Solving");
                File f = touistFile;
                f.deleteOnExit();
            } catch (IOException ex) {
                ex.printStackTrace();
                errorMessage = "The translator returned an IOException: \n"+ex.getMessage();
                showErrorMessage(ex, errorMessage, getFrame().getLang().getWord(Lang.ERROR_TRADUCTION));
                return State.EDITION;
            } catch (InterruptedException ex) {
                ex.printStackTrace();
                errorMessage = "Translator has been interrupted.";
                showErrorMessage(ex, errorMessage, getFrame().getLang().getWord(Lang.ERROR_TRADUCTION));
                return State.EDITION;
            }
            try {
                System.out.println(getFrame().getTranslatorSMT().getSMTFilePath());
                getFrame().setSolver(new SolverSMT(getFrame().getTranslatorSMT().getSMTFilePath()));
                //appel lors de la réussit du traducteur
                Model model = ((SolverSMT)(getFrame().getSolver())).getresult();
                System.out.println("eoo le model : "+model.toString());
                getFrame().setResultView(model);
                return State.SINGLE_RESULT;
            } catch (Exception e) {
                //TODO handle exceptions
                
            }
        } else if (getFrame().getEditorPanel1().editor.getEditorTextArea().getSyntaxEditingStyle() == "qbf") {
        	solveButton.setText("Solving");
        	BufferedReader reader = new BufferedReader(new StringReader(editor.getText()));
            SolverQBF s = new SolverQBF(reader);
            getFrame().setSolver(s);
            try {
            	s.launch();
            	errorMessage = "";
                for (TranslationError error : s.getErrors()) {
                	errorMessage += error + "\n";
                }
                setJLabelErrorMessageText(errorMessage);
                if(errorMessage != "") {
                    System.out.println("touist returned errors:\n"+ errorMessage + "\n");
                    return State.EDITION;
                }
                
                ListIterator<Model> iter = getFrame().getSolver().getModelList().iterator();
                if(!iter.hasNext()) {
                	if(s.getReturnCode() == SolverQBF.SOLVER_UNSAT) {
	                    System.out.println("This problem is unsatisfiable");
	                    errorMessage = "There is no solution";
	                    showErrorMessage(errorMessage, "Solver error");
                	} else if(s.getReturnCode() != SolverQBF.OK) {
                		errorMessage = "";
                		for (TranslationError error : s.getErrors())
                			errorMessage += error + "\n";
                		setJLabelErrorMessageText(errorMessage);
                		System.err.println("touist error: "+errorMessage);
                	}
                    return State.EDITION;
                }
                getFrame().updateResultsPanelIterator(iter);
                /**
                 * Si il y a plus d'un model, alors passer à l'état FIRST_RESULT
                 * sinon passer à l'état SINGLE_RESULT
                 */
                if (iter.hasNext()) {
                    getFrame().setResultView(iter.next());
                    if (iter.hasNext()) {
                       //iter.previous();
                        return State.FIRST_RESULT;
                    } else {
                        //iter.previous();
                        return State.SINGLE_RESULT;
                    }
                } else {
                    return State.SINGLE_RESULT;
                }
            } catch (IOException ex) {
                ex.printStackTrace();
                errorMessage = "The translator returned an IOException: \n"+ex.getMessage();
                showErrorMessage(ex, errorMessage, getFrame().getLang().getWord(Lang.ERROR_TRADUCTION));
                return State.EDITION;
            } catch (InterruptedException ex) {
                ex.printStackTrace();
                errorMessage = "Translator has been interrupted.";
                showErrorMessage(ex, errorMessage, getFrame().getLang().getWord(Lang.ERROR_TRADUCTION));
                return State.EDITION;
            } catch (SolverExecutionException e) {
            	errorMessage = "The solver returned an error: \n"+e.getMessage();
            	for (TranslationError error : ((SolverQBF)getFrame().getSolver()).getErrors())
                	errorMessage += error + "\n";
                setJLabelErrorMessageText(errorMessage);
            	return State.EDITION;
			}
        }

        
        return State.EDITION;
    }
    
    public void setJLabelCaretPositionText(String text) {
        cursorPosition.setText(text);
    }

    public void undo() {
        try {
            Robot robot = new Robot();
            robot.keyPress(KeyEvent.VK_CONTROL);
            robot.keyPress(KeyEvent.VK_Z);
            robot.keyRelease(KeyEvent.VK_CONTROL);
            robot.keyRelease(KeyEvent.VK_Z);
        } catch (AWTException e) {
            e.printStackTrace();
        }
    }
    
    public void redo() {
        try {
            Robot robot = new Robot();
            robot.keyPress(KeyEvent.VK_CONTROL);
            robot.keyPress(KeyEvent.VK_Y);
            robot.keyRelease(KeyEvent.VK_CONTROL);
            robot.keyRelease(KeyEvent.VK_Y);
        } catch (AWTException e) {
            e.printStackTrace();
        }
    }
    
    public void zoom(int n) {
    	editor.zoom(n);
    }
    
    @Override
    public void updateLanguage() {
        setOpenedFile(openedFile);
        importButton.setText(getFrame().getLang().getWord(Lang.EDITION_IMPORT));
        importButton.setToolTipText(getFrame().getLang().getWord("ParentEditionPanel.importButton.tooltip"));
        exportButton.setText(getFrame().getLang().getWord(Lang.EDITION_EXPORT));
        exportButton.setToolTipText(getFrame().getLang().getWord("ParentEditionPanel.exportButton.tooltip"));
        solveButton.setText(getFrame().getLang().getWord(Lang.EDITION_TEST));
        solveButton.setToolTipText(getFrame().getLang().getWord("ParentEditionPanel.testButton.tooltip"));
        editor.updateLanguage();
        selectSatOrSmt.setToolTipText(getFrame().getLang().getWord("ParentEditionPanel.comboBoxSATSMT.tooltip"));
        cursorPosition.setToolTipText(getFrame().getLang().getWord("ParentEditionPanel.jLabelCaretPosition.tooltip"));
        bottomMessage.setToolTipText(getFrame().getLang().getWord("ParentEditionPanel.jLabelErrorMessage.tooltip"));
        updateUI();
    }
    public EditionPanel getEditor() {
		return editor;
	}
}
