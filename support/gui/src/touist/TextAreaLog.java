package touist;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

/**
 * This class extends from OutputStream to redirect output to a JTextArrea
 * @author www.codejava.net
 *
 */

class CustomOutputStream extends OutputStream {
    private JTextArea textArea;
    private PrintStream secondary;

    public CustomOutputStream(JTextArea textArea, PrintStream secondary) {
        this.textArea = textArea;
        this.secondary = secondary;
    }

    @Override
    public void write(int b) throws IOException {
        secondary.print((char)b);
        textArea.append(String.valueOf((char)b));
        textArea.setCaretPosition(textArea.getDocument().getLength());
    }
}

/**
 * This class allows us to open a window where stdout and stderr are displayed. Stdout and stderr are also
 * redirected to their standard System.out and System.err.
 */
public class TextAreaLog extends JFrame {
    /**
     * The text area which is used for displaying logging information.
     */
    private JTextArea textArea;
    private JButton buttonClear = new JButton("Clear");
    private PrintStream standardOut, standardErr;

    public TextAreaLog() {
        super("Stdout and stderr");

        // keeps reference of standard output stream
        standardOut = System.out;
        standardErr = System.err;

        textArea = new JTextArea(50, 80);
        textArea.setEditable(false);
        PrintStream printStreamOut = new PrintStream(new CustomOutputStream(textArea, standardOut));
        PrintStream printStreamErr = new PrintStream(new CustomOutputStream(textArea, standardErr));

        // re-assigns standard output stream and error output stream
        System.setOut(printStreamOut);
        System.setErr(printStreamErr);

        // creates the GUI
        setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.insets = new Insets(10, 10, 10, 10);
        constraints.anchor = GridBagConstraints.WEST;

        constraints.gridx = 1;
        add(buttonClear, constraints);

        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 2;
        constraints.fill = GridBagConstraints.BOTH;
        constraints.weightx = 1.0;
        constraints.weighty = 1.0;

        add(new JScrollPane(textArea), constraints);

        // adds event handler for button Clear
        buttonClear.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                // clears the text area
                try {
                    textArea.getDocument().remove(0,
                            textArea.getDocument().getLength());
                    standardOut.println("Log cleared");
                } catch (BadLocationException ex) {
                    ex.printStackTrace();
                }
            }
        });

        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setSize(640, 480);
        setLocationRelativeTo(null);    // centers on screen
    }
}