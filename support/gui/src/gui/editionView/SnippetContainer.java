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

import gui.AbstractComponentPanel;
import gui.Lang;
import gui.editionView.editor.Editor;
import java.awt.Component;

import java.util.ArrayList;

import javax.swing.BoxLayout;

/**
 *
 * @author Skander
 */
public class SnippetContainer extends AbstractComponentPanel {

	@Override
	public void updateLanguage() {
		if(sectionConnect!=null) sectionConnect.setText(getFrame().getLang().getWord("SnippetSection.FormulasSection1"));
		if(sectionBig!=null) sectionBig.setText(getFrame().getLang().getWord("SnippetSection.FormulaBigOps"));
		if(sectionCard!=null) sectionCard.setText(getFrame().getLang().getWord("SnippetSection.FormulasSection2"));
		if(sectionOthers!=null) sectionOthers.setText(getFrame().getLang().getWord("SnippetSection.FormulasSection3"));
		if(sectionSets!=null) sectionSets.setText(getFrame().getLang().getWord("SnippetSection.SetsSection1"));
	}

	public static enum PaletteType {FORMULA, SET};

	private Editor editorTextArea;

	public SnippetContainer() {
		initComponents();
	}

	/**
	 * Creates new form SnippetContainer
	 * @param editorTextArea
	 */
	public SnippetContainer(Editor editorTextArea) {
		initComponents();
		this.editorTextArea = editorTextArea;
	}

	public void setEditorTextArea(Editor editorTextArea) {
		this.editorTextArea = editorTextArea;
	}

	private SnippetSection sectionConnect;
	private SnippetSection sectionBig;
	private SnippetSection sectionOthers;
	private SnippetSection sectionCard;
	private SnippetSection sectionSets;

	public void initPaletteContent() {

		sectionConnect = new SnippetSection("Connectors");
		sectionBig = new SnippetSection("Big ops");
		sectionCard = new SnippetSection("Cardinality");
		sectionOthers = new SnippetSection("Others");

		ArrayList<Integer> snippetsAnd = new ArrayList<Integer>(){{add(0);add(1);add(7);add(8);}};
		ArrayList<Integer> snippetsOr = new ArrayList<Integer>(){{add(0);add(1);add(6);add(7);}};
		ArrayList<Integer> snippetsNot = new ArrayList<Integer>(){{add(4);add(5);}};
		ArrayList<Integer> snippetsIf = new ArrayList<Integer>(){{add(3);add(4);add(14);add(15);add(25);add(26);}};
		ArrayList<Integer> snippetsBigand = new ArrayList<Integer>(){{add(7);add(8);add(13);add(14);}};
		ArrayList<Integer> snippetsXor = new ArrayList<Integer>(){{add(0);add(1);add(7);add(8);}};
		ArrayList<Integer> snippetsImply = new ArrayList<Integer>(){{add(0);add(1);add(6);add(7);}};
		ArrayList<Integer> snippetsEquivalent = new ArrayList<Integer>(){{add(0);add(1);add(7);add(8);}};
		ArrayList<Integer> snippetsBigor = new ArrayList<Integer>(){{add(6);add(7);add(12);add(13);}};
		ArrayList<Integer> snippetsAtMost = new ArrayList<Integer>(){{add(7);add(8);add(10);add(11);add(13);add(14);add(16);add(17);}};
		ArrayList<Integer> snippetsAtLeast = new ArrayList<Integer>(){{add(8);add(9);add(11);add(12);add(14);add(15);add(17);add(18);}};
		ArrayList<Integer> snippetsExact = new ArrayList<Integer>(){{add(6);add(7);add(9);add(10);add(12);add(13);add(15);add(16);}};

		sectionConnect.addInsertButton(new InsertionButton(editorTextArea, "$a and $b", "\\mathbf{a} \\wedge \\mathbf{b}", snippetsAnd, "and"));
		sectionConnect.addInsertButton(new InsertionButton(editorTextArea, "$a or $b", "\\mathbf{a} \\vee \\mathbf{b}" , snippetsOr, "or"));
		sectionConnect.addInsertButton(new InsertionButton(editorTextArea, "not $a", "\\neg \\mathbf{a}", snippetsNot, "not"));
		sectionConnect.addInsertButton(new InsertionButton(editorTextArea, "$a xor $b", "\\mathbf{a} \\oplus \\mathbf{b}", snippetsXor, "xor"));
		sectionConnect.addInsertButton(new InsertionButton(editorTextArea, "$a => $b", "\\mathbf{a} \\Rightarrow \\mathbf{b}", snippetsImply, "imply"));
		sectionConnect.addInsertButton(new InsertionButton(editorTextArea, "$a <=> $b", "\\mathbf{a} \\Leftrightarrow \\mathbf{b}", snippetsEquivalent, "is equivalent to"));

		sectionBig.addInsertButton(new InsertionButton(editorTextArea, "bigand $i in $a: \n\tp($i) \nend", "\\bigwedge\\limits_{\\substack{\\mathbf{i}\\in \\mathbf{a}}}p_{\\mathbf{i}}", snippetsBigand,"bigand"));
		sectionBig.addInsertButton(new InsertionButton(editorTextArea, "bigor $i in $a: \n\tp($i) \nend", "\\bigvee\\limits_{\\substack{\\mathbf{i}\\in \\mathbf{a}}}p_{\\mathbf{i}}", snippetsBigor,"bigor"));

		sectionCard.addInsertButton(new InsertionButton(editorTextArea, "atmost($n,$S)", "\\textrm{atmost}(.,.)", snippetsAtMost, "at most"));
		sectionCard.addInsertButton(new InsertionButton(editorTextArea, "atleast($n,$S)", "\\textrm{atleast}(.,.)", snippetsAtLeast, "at least"));
		sectionCard.addInsertButton(new InsertionButton(editorTextArea, "exact($n,$S)", "\\textrm{exact}(.,.)", snippetsExact, "exact"));

		sectionOthers.addInsertButton(new InsertionButton(editorTextArea, "if $a \nthen \n\t$b \nelse \n\t$c\n", "\\textrm{if}\\mathbf{a}\\textrm{then}\\mathbf{b}\\textrm{else}\\mathbf{c}", snippetsIf, "if then else","if\\,\\$a \\\\ then\\\\\\quad\\$b \\\\ else\\\\\\quad\\$c"));


		snippetSectionsContainer.setLayout(new BoxLayout(snippetSectionsContainer, BoxLayout.Y_AXIS));
		snippetSectionsContainer.add(sectionConnect);
		snippetSectionsContainer.add(sectionBig);


		sectionSets = new SnippetSection("Sets");

		ArrayList<Integer> snippetsSet = new ArrayList<Integer>(){{add(0);add(1);}};

		sectionSets.addInsertButton(new InsertionButton(editorTextArea, "$a = true", "\\mathbf{a} \\leftarrow true", snippetsSet, ""));
		sectionSets.addInsertButton(new InsertionButton(editorTextArea, "$v = 0", "\\mathbf{v} \\leftarrow 0", snippetsSet, ""));
		sectionSets.addInsertButton(new InsertionButton(editorTextArea, "$v = 0.0", "\\mathbf{v} \\leftarrow 0.", snippetsSet, ""));
		sectionSets.addInsertButton(new InsertionButton(editorTextArea, "$a = [a,b]", "\\mathbf{a} \\leftarrow [a,b]", snippetsSet, ""));

		snippetSectionsContainer.add(sectionSets);
		snippetSectionsContainer.add(sectionOthers);
		snippetSectionsContainer.add(sectionCard);

		sectionConnect.unfold();
		sectionBig.unfold();
		sectionSets.unfold();
	}


public int getMaxWidthIcons() {
	int max_icon_width = 0;
	for (Component section : snippetSectionsContainer.getComponents()) {
		if (section instanceof SnippetSection) {
			for (InsertionButton button : ((SnippetSection)section).getButtons()) {
				max_icon_width = (int) Math.max(max_icon_width, button.getIcon().getIconWidth());
			}
		}
	}
	return max_icon_width;
}

/**
 * This method is called from within the constructor to initialize the form.
 * WARNING: Do NOT modify this code. The content of this method is always
 * regenerated by the Form Editor.
 */
@SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        snippetSectionsContainer = new javax.swing.JPanel();

        snippetSectionsContainer.setLayout(new javax.swing.BoxLayout(snippetSectionsContainer, javax.swing.BoxLayout.LINE_AXIS));

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(snippetSectionsContainer, javax.swing.GroupLayout.DEFAULT_SIZE, 70, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(0, 0, 0)
                .addComponent(snippetSectionsContainer, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel snippetSectionsContainer;
    // End of variables declaration//GEN-END:variables
}
