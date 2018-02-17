module touist {
    requires java.logging;

    requires transitive java.datatransfer;
    requires transitive java.desktop;
    requires transitive java.xml;

    exports entity;
    exports gui;
    exports gui.TranslatorLatex;
    exports gui.editionView;
    exports gui.editionView.editor;
    exports gui.menu;
    exports gui.resultsView;
    exports org.fife.io;
    exports org.fife.print;
    exports org.fife.ui.rsyntaxtextarea;
    exports org.fife.ui.rsyntaxtextarea.focusabletip;
    exports org.fife.ui.rsyntaxtextarea.folding;
    exports org.fife.ui.rsyntaxtextarea.modes;
    exports org.fife.ui.rsyntaxtextarea.parser;
    exports org.fife.ui.rsyntaxtextarea.templates;
    exports org.fife.ui.rtextarea;
    exports org.fife.util;
    exports org.kordamp.ikonli;
    exports org.kordamp.ikonli.fontawesome;
    exports org.kordamp.ikonli.swing;
    exports org.scilab.forge.jlatexmath;
    exports org.scilab.forge.jlatexmath.cache;
    exports org.scilab.forge.jlatexmath.cyrillic;
    exports org.scilab.forge.jlatexmath.dynamic;
    exports org.scilab.forge.jlatexmath.greek;
    exports org.scilab.forge.jlatexmath.internal.util;
    exports solution;
    exports touist;
    exports translation;

    provides org.kordamp.ikonli.IkonHandler with
        org.kordamp.ikonli.fontawesome.FontAwesomeIkonHandler,
        org.kordamp.ikonli.IkonliIkonResolver;

    uses org.kordamp.ikonli.IkonHandler;

}
