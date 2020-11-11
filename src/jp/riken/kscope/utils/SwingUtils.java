/*
 * K-scope
 * Copyright 2012-2013 RIKEN, Japan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Version for building code on a server.
 */

package jp.riken.kscope.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.SizeRequirements;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.JTextComponent;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;
import javax.swing.text.Utilities;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.properties.KscopeProperties;

/**
 * Swing utility class
 *
 * @author RIKEN
 */
public class SwingUtils {
  private static Boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;
  /** Tab size check characters */
  private static final char TAB_CHECK_CHARACTOR = 'o';

  /**
   * Set the tab size of the text component
   *
   * @param component Text component
   * @param tabsize Tab size
   */
  public static void setTabSize(JTextComponent component, int tabsize) {
    if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    SimpleAttributeSet attrs = new SimpleAttributeSet();

    FontMetrics fm = component.getFontMetrics(component.getFont());
    int charWidth = fm.charWidth(TAB_CHECK_CHARACTOR);
    int tabLength = charWidth * tabsize;
    TabStop[] tabs = new TabStop[10];
    for (int j = 0; j < tabs.length; j++) {
      tabs[j] = new TabStop((j + 1) * tabLength);
    }
    TabSet tabSet = new TabSet(tabs);
    attrs = new SimpleAttributeSet();
    StyleConstants.setTabSet(attrs, tabSet);

    int length = component.getDocument().getLength();
    ((StyledDocument) component.getDocument()).setParagraphAttributes(0, length, attrs, false);

    return;
  }

  /**
   * Creating a text view without wrapping
   *
   * @return Text view without wrapping
   */
  public static NoWrapEditorKit factoryNoWrapEditorKit() {
    return new SwingUtils().new NoWrapEditorKit();
  }

  /**
   * Line wrap paragraph view class
   *
   * @author RIKEN
   */
  private class NoWrapParagraphView extends ParagraphView {
    /**
     * Constructor
     *
     * @param elem Elements handled by this view
     */
    public NoWrapParagraphView(Element elem) {
      super(elem);
    }

    /**
     * Calculate the row width size requirement. <br>
     * Set the wrapping size for one line.
     *
     * @param axis Line position
     * @param r Component size and position object
     * @return Component size and position object
     */
    @Override
    protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
      SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
      req.minimum = req.preferred;
      return req;
    }

    /**
     * Fetches the constraint span that flows against the specified child index.
     *
     * @param index Index of viewed view
     * @return View constraint span
     */
    @Override
    public int getFlowSpan(int index) {
      return Integer.MAX_VALUE;
    }
  }

  /**
   * View creation class
   *
   * @author RIKEN
   */
  class NoWrapViewFactory implements ViewFactory {
    /**
     * Create a view based on the element.
     *
     * @param elem Elements to be created
     * @return view
     */
    @Override
    public View create(Element elem) {
      String kind = elem.getName();
      if (kind != null) {
        if (kind.equals(AbstractDocument.ContentElementName)) {
          return new LabelView(elem);
        } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
          return new NoWrapParagraphView(elem);
        } else if (kind.equals(AbstractDocument.SectionElementName)) {
          return new BoxView(elem, View.Y_AXIS);
        } else if (kind.equals(StyleConstants.ComponentElementName)) {
          return new ComponentView(elem);
        } else if (kind.equals(StyleConstants.IconElementName)) {
          return new IconView(elem);
        }
      }
      return new LabelView(elem);
    }
  }

  /**
   * Formatted text style
   *
   * @author RIKEN
   */
  @SuppressWarnings("serial")
  class NoWrapEditorKit extends StyledEditorKit {

    /**
     * Get the view creation class.
     *
     * @return View creation class
     */
    @Override
    public ViewFactory getViewFactory() {
      return new NoWrapViewFactory();
    }
  }

  /**
   * Get the value of specified element and attribute from HTML format text element
   *
   * @param elem HTML text element
   * @param tagname Element name
   * @param attrname Attribute name
   * @return attribute value
   */
  public static String getAttributeValue(
      javax.swing.text.Element elem, HTML.Tag tagname, HTML.Attribute attrname) {

    // Search for tags
    javax.swing.text.AttributeSet attrs = elem.getAttributes();
    String attrValue = null;
    for (Enumeration<?> enumAttrs = attrs.getAttributeNames(); enumAttrs.hasMoreElements(); ) {
      Object attrTag = enumAttrs.nextElement();

      // Does it match the search element name?
      if (attrTag instanceof HTML.Tag && (HTML.Tag) attrTag == tagname) {
        javax.swing.text.SimpleAttributeSet atag = (SimpleAttributeSet) attrs.getAttribute(attrTag);

        // Search for attributes
        for (Enumeration<?> enumAtag = atag.getAttributeNames(); enumAtag.hasMoreElements(); ) {
          Object attrAtag = enumAtag.nextElement();

          // Does it match the search attribute?
          if (attrAtag instanceof HTML.Attribute && (HTML.Attribute) attrAtag == attrname) {
            attrValue = (String) atag.getAttribute(attrAtag);
          }
        }
      }
    }

    return attrValue;
  }

  /**
   * Launch an external program. <br>
   * If no program is specified, start by OS association (java.awt.Desktop).
   *
   * @param filename Startup file name
   * @param program Startup program
   * @param options Start argument
   * @return error message (null = normal startup)
   */
  public static String processOpenProgram(String filename, String program, String[] options) {
    String errMsg = null;
    try {
      if (program == null || program.isEmpty()) {

        if (filename == null || filename.isEmpty()) {
          // swingutils.processopenprogram.error.empty = The startup URL or file name is not
          // specified.
          errMsg = Message.getString("swingutils.processopenprogram.error.empty");
          return errMsg;
        }

        // add by @hira at 2013/05/13: Is desktop supported?
        if (!Desktop.isDesktopSupported()) {
          // Desktop is not supported.
          errMsg = Message.getString("swingutils.processopenprogram.error.desktopsupported");
          return errMsg;
        }

        // Boot by OS association
        Desktop desktop = Desktop.getDesktop();
        if (filename.startsWith("http://") || filename.startsWith("https://")) {
          // OS-dependent browser startup
          URI uri = null;
          try {
            uri = new URI(filename);
          } catch (URISyntaxException ex) {
            errMsg = ex.getMessage();
            return errMsg;
          }
          try {
            desktop.browse(uri);
          } catch (IOException ex) {
            errMsg = ex.getMessage();
            return errMsg;
          }
        } else {
          File openFile = null;
          if (filename.startsWith("file:/")) {
            URL url = new URL(filename);
            URI uri = url.toURI();
            openFile = new File(uri);
          } else {
            openFile = new File(filename);
          }
          if (!openFile.exists()) {
            // swingutils.processopenprogram.error.notexists.file = The startup file [% s] does not
            // exist.
            errMsg =
                Message.getString(
                    "swingutils.processopenprogram.error.notexists.file", openFile.getPath());
            return errMsg;
          }
          if (openFile != null) {
            try {
              // File startup
              desktop.open(openFile);
            } catch (IOException e) {
              // e.printStackTrace();
              // swingutils.processopenprogram.error.notexists.program = The program associated with
              // the file [% s] does not exist.
              errMsg =
                  Message.getString(
                      "swingutils.processopenprogram.error.notexists.program", openFile.getName());
              return errMsg;
            }
          }
        }
      } else {
        String openFilename = null;
        if (filename != null && !filename.isEmpty()) {
          if (filename.startsWith("file:/")) {
            URL url = new URL(filename);
            URI uri = url.toURI();
            File openFile = new File(uri);
            openFilename = openFile.getAbsolutePath();
          } else {
            File openFile = new File(filename);
            openFilename = openFile.getAbsolutePath();
          }
        }
        // Start by specifying the program
        List<String> args = new ArrayList<String>();
        args.add(program);
        if (options != null && options.length > 0) {
          args.addAll(Arrays.asList(options));
        }
        if (openFilename != null) {
          args.add(openFilename);
        }
        // swingutils.processopenprogram.error.invalid.process = Failed to start the process by the
        // start program [% s].
        errMsg = Message.getString("swingutils.processopenprogram.error.invalid.process", program);
        try {
          String[] validcommand = args.toArray(new String[0]);
          for (int i = 0; i < validcommand.length; i++) {
            validcommand[i] = StringUtils.trimQuote(validcommand[i]);
          }
          ProcessBuilder pb = new ProcessBuilder(validcommand);
          Process p = pb.start();
          if (p == null) {
            return errMsg;
          }
        } catch (IOException e) {
          return errMsg;
        }
      }
    } catch (Exception ex) {
      ex.printStackTrace();
      errMsg = ex.getMessage();
      return errMsg;
    }
    return null;
  }

  /**
   * Launch an external program. <br>
   * If no program is specified, start by OS association (java.awt.Desktop).
   *
   * @param commands Start command list
   * @param workdirectory Execution folder
   * @param outStream Run console output stream
   * @return error message (null = normal startup)
   * @throws Exception
   */
  public static int processRun(String commands[], File workdirectory, OutputStream outStream)
      throws Exception {
    String errMsg = null;
    int result = -1;
    Process process = null;
    StringBuilder buf = new StringBuilder();
    // String bufStr = new String();
    if (commands == null || commands.length <= 0) return 1;
    String cmdString = "";
    for (int i = 0; i < commands.length; i++) {
      cmdString += commands[i] + " ";
    }
    cmdString = cmdString.trim();

    System.out.println(Arrays.toString(commands));
    try {
      ProcessBuilder pb = new ProcessBuilder(commands);
      pb.redirectErrorStream(true);

      if (workdirectory != null) {
        pb.directory(workdirectory);
        System.out.println("Directory: " + workdirectory);
      }
      process = pb.start();
      if (process == null) {
        errMsg = Message.getString("swingutils.processrun.error.invalid.command", cmdString);
        throw new Exception(errMsg);
      }
      // Get process error stream
      InputStream is = process.getInputStream();
      while (true) {
        int c = is.read();
        if (c == -1) {
          is.close();
          break;
        }
        if (outStream != null) {
          outStream.write(c);
          if (c == 0x0A || c == 0x0D) {
            outStream.flush();
          }
        }
        buf.append((char) c);
      }
      is.close();
      if (outStream != null) {
        outStream.write(0x0A);
        outStream.flush();
      }
      // bufStr = new String(buf.toString().getBytes("iso-8859-1"));
      process.waitFor();
      result = process.exitValue();

    } catch (Exception ex) {
      ex.printStackTrace();
      errMsg = ex.getMessage();
      result = -1;
      if (process != null) {
        result = process.exitValue();
      }
      throw new Exception(errMsg);
    }

    return result;
  }

  /**
   * Get the line number of the specified caret position
   *
   * @param editor Text component
   * @param pos Caret position
   * @return line number (1 ~)
   */
  public static int getRow(JTextComponent editor, int pos) {
    int rn = (pos == 0) ? 1 : 0;
    try {
      int offs = pos;
      while (offs > 0) {
        offs = Utilities.getRowStart(editor, offs) - 1;
        rn++;
      }
    } catch (BadLocationException e) {
      e.printStackTrace();
    }
    return rn;
  }

  /**
   * Get the column number of the specified caret position
   *
   * @param editor Text component
   * @param pos Caret position
   * @return Column number (1 ~)
   */
  public static int getColumn(JTextComponent editor, int pos) {
    try {
      return pos - Utilities.getRowStart(editor, pos) + 1;
    } catch (BadLocationException e) {
      e.printStackTrace();
    }
    return -1;
  }

  /**
   * Set focus listener on all child components
   *
   * @param container Parent component
   * @param listener Focus listener
   */
  public static void addChildFocusListener(Container container, FocusListener listener) {
    Component[] comps = container.getComponents();

    for (Component child : comps) {
      child.addFocusListener(listener);
      if (child instanceof Container) {
        addChildFocusListener((Container) child, listener);
      }
    }
  }

  /**
   * Set the background color for all JPanel child components
   *
   * @param container Parent component
   * @param background Background color
   */
  public static void setBackgroundChildPanel(Container container, Color background) {

    if (container instanceof JPanel) {
      if (background != null) {
        ((JPanel) container).setOpaque(true);
        container.setBackground(background);
      } else {
        ((JPanel) container).setOpaque(false);
        container.setBackground(background);
      }
    }

    Component[] comps = container.getComponents();
    for (Component child : comps) {
      if (child instanceof Container) {
        setBackgroundChildPanel((Container) child, background);
      }
    }
  }

  /**
   * Show a folder selection dialog. For Mac, use java.awt.FileDialog to show a folder selection
   * dialog. Linux, For Windows, use javax.swing.JFileChooser.
   *
   * @param parent Parent component
   * @param title Dialog title
   * @param currentDirectoryPath Display folder
   * @param multiselection Multiple selection (true = multiple selection): (Not available on Mac)
   * @return selection file
   */
  public static File[] showOpenFolderDialog(
      Component parent, String title, String currentDirectoryPath, boolean multiselection) {

    try {
      // for Mac
      // add at 2013/05/31 by @hira
      // Display flag in the folder dialog on Mac
      // Display the folder dialog with AppleScript for java 1.7 and above
      // Use java.awt.FileDialog for java1.6 and below.
      // boolean applescript = KscopeProperties.isJava17Later();
      boolean applescript = KscopeProperties.isApplescript();
      if (debug_l2) {
        if (applescript) System.out.println("Use apple script for file dialog");
        else System.out.println("Use FileDialog for file dialog");
      }
      if (KscopeProperties.isMac() && applescript) {
        File selected = AppleScriptEngine.showFolderDialog(title, currentDirectoryPath);
        if (selected == null) {
          return null;
        }
        File[] files = {selected};
        return files;
      } else if (KscopeProperties.isMac()) {
        // Change to folder selection
        System.setProperty("apple.awt.fileDialogForDirectories", "true");
        // System.setProperty("apple.awt.fileDialogForDirectories", "falase");

        // use java.awt.FileDialog
        FileDialog dialog = null;
        if (parent instanceof Frame) {
          dialog = new FileDialog((Frame) parent, title, FileDialog.LOAD);
        } else if (parent instanceof Dialog) {
          dialog = new FileDialog((Dialog) parent, title, FileDialog.LOAD);
        } else {
          dialog = new FileDialog((Frame) null, title, FileDialog.LOAD);
        }

        // parent folder
        dialog.setDirectory(currentDirectoryPath);
        // Display the folder selection dialog
        dialog.setVisible(true);

        // Selected file
        String file = dialog.getFile();
        if (file == null || file.isEmpty()) return null;

        // Get the selected file
        String selected = dialog.getDirectory() + File.separator + dialog.getFile();
        if (selected != null) {
          File[] files = {new File(selected)};
          return files;
        }
      }
      // for Windows, Linux
      else {

        JFileChooser filechooser = new JFileChooser(currentDirectoryPath);
        filechooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        filechooser.setDialogTitle(title);
        filechooser.setMultiSelectionEnabled(multiselection);
        int selected = filechooser.showOpenDialog(parent);
        if (selected != JFileChooser.APPROVE_OPTION) return null;
        if (multiselection) {
          File[] files = filechooser.getSelectedFiles();
          return files;
        } else {
          File file = filechooser.getSelectedFile();
          File[] files = {file};
          return files;
        }
      }
    } catch (Exception e) {

    } finally {
      // Deselect folder
      System.setProperty("apple.awt.fileDialogForDirectories", "false");
    }

    return null;
  }

  /**
   * Show the project selection dialog. For Mac, use java.awt.FileDialog to show the folder
   * selection dialog. For Linux and Windows, use javax.swing.JFileChooser.
   *
   * @param parent Parent component
   * @param title Dialog title
   * @param currentDirectoryPath Display folder
   * @return selection file
   */
  public static File showOpenProjectDialog(
      Component parent, String title, String currentDirectoryPath) {
    try {
      ProjectFilter filter =
          new SwingUtils().new ProjectFilter(KscopeProperties.PROJECT_FILE + " | Project Folder");

      // for Mac
      // add at 2013/05/31 by @hira
      // Display flag in the folder dialog on Mac
      // Display the folder dialog with AppleScript for java 1.7 and above
      // Use java.awt.FileDialog for java1.6 and below.
      // boolean applescript = KscopeProperties.isJava17Later();
      boolean applescript = KscopeProperties.isApplescript();
      if (KscopeProperties.isMac() && applescript) {
        File projectfile = AppleScriptEngine.showFolderDialog(title, currentDirectoryPath);
        return projectfile;
      } else if (KscopeProperties.isMac()) {
        // Change to folder selection
        System.setProperty("apple.awt.fileDialogForDirectories", "true");
        // System.setProperty("apple.awt.fileDialogForDirectories", "false");

        // use java.awt.FileDialog
        FileDialog dialog = null;
        if (parent instanceof Frame) {
          dialog = new FileDialog((Frame) parent, title, FileDialog.LOAD);
        } else if (parent instanceof Dialog) {
          dialog = new FileDialog((Dialog) parent, title, FileDialog.LOAD);
        } else {
          dialog = new FileDialog((Frame) null, title, FileDialog.LOAD);
        }

        // parent folder
        dialog.setDirectory(currentDirectoryPath);
        // File filter
        // dialog.setFilenameFilter(filter);
        // Display the folder selection dialog
        dialog.setVisible(true);

        // Selected file
        String file = dialog.getFile();
        if (file == null || file.isEmpty()) return null;

        // Get the selected file
        String selected = dialog.getDirectory() + File.separator + dialog.getFile();
        if (selected != null) {
          File projectfile = new File(selected);
          return projectfile;
        }
      }
      // for Windows, Linux
      else {

        JFileChooser filechooser = new JFileChooser(currentDirectoryPath);
        // Make files and folders selectable.
        filechooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        filechooser.setDialogTitle(title);
        filechooser.addChoosableFileFilter(filter);
        // In case of JDK7, the filter is not selected unless setFileFilter is set. at 2013/05/14 by
        // @hira
        filechooser.setFileFilter(filter);
        int selected = filechooser.showOpenDialog(parent);
        if (selected != JFileChooser.APPROVE_OPTION) return null;
        File file = filechooser.getSelectedFile();
        return file;
      }
    } catch (Exception e) {

    } finally {
      // Deselect folder
      System.setProperty("apple.awt.fileDialogForDirectories", "false");
    }

    return null;
  }

  /**
   * Show a folder save dialog. For Mac, use java.awt.FileDialog to show a folder selection dialog.
   * Linux, For Windows, use javax.swing.JFileChooser.
   *
   * @param parent Parent component
   * @param title Dialog title
   * @param currentDirectoryPath Display folder
   * @param multiselection Multiple selection (true = multiple selection): (Not available on Mac)
   * @return selection file
   */
  public static File[] showSaveFolderDialog(
      Component parent, String title, String currentDirectoryPath, boolean multiselection) {
    try {
      // for Mac
      // add at 2013/05/31 by @hira
      // Display flag in the folder dialog on Mac
      // Display the folder dialog with AppleScript for java 1.7 and above
      // Use java.awt.FileDialog for java1.6 and below.
      // boolean applescript = KscopeProperties.isJava17Later();
      boolean applescript = KscopeProperties.isApplescript();
      if (KscopeProperties.isMac() && applescript) {
        File selected = AppleScriptEngine.showFolderDialog(title, currentDirectoryPath);
        if (selected == null) {
          return null;
        }
        File[] files = {selected};
        return files;
      } else if (KscopeProperties.isMac()) {
        // Change to folder selection
        System.setProperty("apple.awt.fileDialogForDirectories", "true");
        // System.setProperty("apple.awt.fileDialogForDirectories", "false");

        // use java.awt.FileDialog
        // modify FileDialog Mode from FileDialog.SAVE to
        // FileDialog.LOAD at 2013/05/31 by @hira
        // Change from the folder save dialog to the selection dialog. Because the existing folder
        // cannot be selected (input is required)
        FileDialog dialog = null;
        if (parent instanceof Frame) {
          dialog = new FileDialog((Frame) parent, title, FileDialog.LOAD);
        } else if (parent instanceof Dialog) {
          dialog = new FileDialog((Dialog) parent, title, FileDialog.LOAD);
        } else {
          dialog = new FileDialog((Frame) null, title, FileDialog.LOAD);
        }
        // parent folder
        dialog.setDirectory(currentDirectoryPath);
        // Display the folder selection dialog
        dialog.setVisible(true);

        // Selected file
        String file = dialog.getFile();
        if (file == null || file.isEmpty()) return null;

        // Get the selected file
        String selected = dialog.getDirectory() + File.separator + dialog.getFile();
        if (selected != null) {
          File[] files = {new File(selected)};
          return files;
        }
      }
      // for Windows, Linux
      else {

        JFileChooser filechooser = new JFileChooser(currentDirectoryPath);
        filechooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        filechooser.setDialogTitle(title);
        filechooser.setMultiSelectionEnabled(multiselection);
        int selected = filechooser.showSaveDialog(parent);
        if (selected != JFileChooser.APPROVE_OPTION) return null;
        if (multiselection) {
          File[] files = filechooser.getSelectedFiles();
          return files;
        } else {
          File file = filechooser.getSelectedFile();
          File[] files = {file};
          return files;
        }
      }
    } catch (Exception e) {

    } finally {
      // Deselect folder
      System.setProperty("apple.awt.fileDialogForDirectories", "false");
    }

    return null;
  }

  /**
   * Show a file selection dialog. For Mac, use java.awt.FileDialog to show a folder selection
   * dialog. Linux, For Windows, use javax.swing.JFileChooser.
   *
   * @param parent Parent component
   * @param title Dialog title
   * @param currentDirectoryPath Display folder
   * @param filter File filter
   * @param multiselection Multiple selection (true = multiple selection): (Not available on Mac)
   * @return selection file
   */
  public static File[] showOpenFileDialog(
      Component parent,
      String title,
      String currentDirectoryPath,
      ExtFileFilter filter,
      boolean multiselection) {
    try {
      if (KscopeProperties.isMac()) {
        FileDialog dialog = null;
        if (parent instanceof Frame) {
          dialog = new FileDialog((Frame) parent, title, FileDialog.LOAD);
        } else if (parent instanceof Dialog) {
          dialog = new FileDialog((Dialog) parent, title, FileDialog.LOAD);
        } else {
          dialog = new FileDialog((Frame) null, title, FileDialog.LOAD);
        }
        // parent folder
        dialog.setDirectory(currentDirectoryPath);

        // File filter
        dialog.setFilenameFilter(filter);

        // Multiselection
        dialog.setMultipleMode(multiselection);

        // Display the folder selection dialog
        dialog.setVisible(true);

        // Selected file
        String file = dialog.getFile();
        if (file == null || file.isEmpty()) return null;

        // Get the selected file
        String selected = dialog.getDirectory() + File.separator + dialog.getFile();
        if (selected != null) {
          File[] files = {new File(selected)};
          return files;
        }
      } else {

        // Display the file selection dialog.
        JFileChooser filechooser = new JFileChooser(currentDirectoryPath);
        filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        filechooser.setMultiSelectionEnabled(multiselection);
        filechooser.setDialogTitle(title);
        if (filter != null) {
          filechooser.addChoosableFileFilter(filter);
          // In case of JDK7, the filter is not selected unless setFileFilter is set. at 2013/05/14
          // by
          // @hira
          filechooser.setFileFilter(filter);
        }
        // Display XML file selection dialog
        int selected = filechooser.showOpenDialog(parent);
        if (selected != JFileChooser.APPROVE_OPTION) return null;

        if (multiselection) {
          File[] files = filechooser.getSelectedFiles();
          return files;
        } else {
          File file = filechooser.getSelectedFile();
          File[] files = {file};
          return files;
        }
      }
    } catch (Exception e) {

    }

    return null;
  }

  /**
   * Extension file filter.
   *
   * @author RIKEN
   */
  public class ExtFileFilter extends FileFilter implements FilenameFilter {
    /** File filter description */
    private String description;
    /** extension */
    private String[] exts;
    /** Filter */
    private FileFilter filter;

    /**
     * Constructor
     *
     * @param description File filter description
     * @param exts Extension list
     */
    public ExtFileFilter(String description, String[] exts) {
      this.description = description;
      this.exts = exts;
    }

    /**
     * Constructor
     *
     * @param filter
     */
    public ExtFileFilter(FileFilter filter) {
      this.description = filter.getDescription();
      this.filter = filter;
    }

    /**
     * Filter files
     *
     * @param file Display file
     * @return true = Filtered files
     */
    @Override
    public boolean accept(File file) {
      if (file.isDirectory()) return true;
      return accept(file.getName());
    }

    /**
     * Get the file filter description
     *
     * @return File filter description
     */
    @Override
    public String getDescription() {
      return this.description;
    }

    /**
     * Filter files
     *
     * @param dir Display folder
     * @param name Display file
     * @return true = Filtered files
     */
    @Override
    public boolean accept(File dir, String name) {
      return accept(name);
    }

    /**
     * Filter files
     *
     * @param name Display file
     * @return true = Filtered files
     */
    public boolean accept(String name) {
      String file_name = name.toLowerCase();
      String ext = file_name.substring(file_name.lastIndexOf(".") + 1);
      if (this.exts == null && this.filter == null) return true;
      if (this.exts != null) {
        for (String filterExt : exts) {
          if (ext.equalsIgnoreCase(filterExt)) {
            return true;
          }
        }
      }
      if (this.filter != null) {
        return this.filter.accept(new File(name));
      }
      return false;
    }
  }

  /**
   * Show the Make file selection dialog. For Mac, use java.awt.FileDialog to display the folder
   * selection dialog. For Linux and Windows, use javax.swing.JFileChooser.
   *
   * @param parent Parent component
   * @param title Dialog title
   * @param currentDirectoryPath Display folder
   * @param multiselection Multiple selection (true = multiple selection): (Not available on Mac)
   * @return selection file
   */
  public static File[] showOpenMakefileDialog(
      Component parent, String title, String currentDirectoryPath, boolean multiselection) {
    try {
      MakefileFilter filter = new SwingUtils().new MakefileFilter("Makefile(Makefile*, makefile*)");
      if (KscopeProperties.isMac()) {
        FileDialog dialog = null;
        if (parent instanceof Frame) {
          dialog = new FileDialog((Frame) parent, title, FileDialog.LOAD);
        } else if (parent instanceof Dialog) {
          dialog = new FileDialog((Dialog) parent, title, FileDialog.LOAD);
        } else {
          dialog = new FileDialog((Frame) null, title, FileDialog.LOAD);
        }
        // parent folder
        dialog.setDirectory(currentDirectoryPath);

        // File filter
        dialog.setFilenameFilter(filter);

        // Display the folder selection dialog
        dialog.setVisible(true);

        // Selected file
        String file = dialog.getFile();
        if (file == null || file.isEmpty()) return null;

        // Get the selected file
        String selected = dialog.getDirectory() + File.separator + dialog.getFile();
        if (selected != null) {
          File[] files = {new File(selected)};
          return files;
        }
      } else {

        // Display the file selection dialog.
        JFileChooser filechooser = new JFileChooser(currentDirectoryPath);
        filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        filechooser.setMultiSelectionEnabled(multiselection);
        filechooser.setDialogTitle(title);
        filechooser.addChoosableFileFilter(filter);
        // In case of JDK7, the filter is not selected unless setFileFilter is set. at 2013/05/14 by
        // @hira
        filechooser.setFileFilter(filter);
        // Display file selection dialog
        int selected = filechooser.showOpenDialog(parent);
        if (selected != JFileChooser.APPROVE_OPTION) return null;

        if (multiselection) {
          File[] files = filechooser.getSelectedFiles();
          return files;
        } else {
          File file = filechooser.getSelectedFile();
          File[] files = {file};
          return files;
        }
      }
    } catch (Exception e) {

    }

    return null;
  }

  /**
   * Makefile file filter.
   *
   * @author RIKEN
   */
  public class MakefileFilter extends FileFilter implements FilenameFilter {
    /** File filter description */
    private String description;

    /**
     * Constructor
     *
     * @param description File filter description
     */
    public MakefileFilter(String description) {
      this.description = description;
    }

    /**
     * Filter files
     *
     * @param file Display file
     * @return true = Filtered files
     */
    @Override
    public boolean accept(File file) {
      if (file.isDirectory()) return true;
      return accept(file.getName());
    }

    /**
     * Get the file filter description
     *
     * @return File filter description
     */
    @Override
    public String getDescription() {
      return this.description;
    }

    /**
     * Filter files
     *
     * @param dir Display folder
     * @param name Display file
     * @return true = Filtered files
     */
    @Override
    public boolean accept(File dir, String name) {
      return accept(name);
    }

    /**
     * Filter files
     *
     * @param name Display file
     * @return true = Filtered files
     */
    public boolean accept(String name) {
      if (name.length() >= 8) {
        String s = name.substring(0, 8);
        if (s.equalsIgnoreCase("makefile")) {
          return true;
        }
      }
      return false;
    }
  }

  /**
   * Project file filter.
   *
   * @author RIKEN
   */
  public class ProjectFilter extends FileFilter implements FilenameFilter {
    /** File filter description */
    private String description;

    /**
     * Constructor
     *
     * @param description File filter description
     */
    public ProjectFilter(String description) {
      this.description = description;
    }

    /**
     * Filter files
     *
     * @param file Display file
     * @return true = Filtered files
     */
    @Override
    public boolean accept(File file) {
      if (file.isDirectory()) return true;
      return accept(file.getName());
    }

    /**
     * Get the file filter description
     *
     * @return File filter description
     */
    @Override
    public String getDescription() {
      return this.description;
    }

    /**
     * Filter files
     *
     * @param dir Display folder
     * @param name Display file
     * @return true = Filtered files
     */
    @Override
    public boolean accept(File dir, String name) {
      return accept(name);
    }

    /**
     * Filter files
     *
     * @param name Display file
     * @return true = Filtered files
     */
    public boolean accept(String name) {
      if (KscopeProperties.PROJECT_FILE.equalsIgnoreCase(name)) {
        return true;
      }
      return false;
    }
  }

  /**
   * Show the file save dialog. On Mac, use java.awt.FileDialog to show the folder selection dialog.
   * For Linux and Windows, use javax.swing.JFileChooser.
   *
   * @param parent Parent component
   * @param title Dialog title
   * @param currentDirectoryPath Display folder
   * @param defaultname Default file name
   * @return Save file
   */
  public static File showSaveFileDialog(
      Component parent, String title, String currentDirectoryPath, String defaultname) {
    try {
      // Mac
      if (KscopeProperties.isMac()) {
        FileDialog dialog = null;
        if (parent instanceof Frame) {
          dialog = new FileDialog((Frame) parent, title, FileDialog.SAVE);
        } else if (parent instanceof Dialog) {
          dialog = new FileDialog((Dialog) parent, title, FileDialog.SAVE);
        } else {
          dialog = new FileDialog((Frame) null, title, FileDialog.SAVE);
        }
        // parent folder
        dialog.setDirectory(currentDirectoryPath);
        // Default file name
        dialog.setFile(defaultname);
        // Display the folder selection dialog
        dialog.setVisible(true);

        // Selected file
        String file = dialog.getFile();
        if (file == null || file.isEmpty()) return null;

        // Get the selected file
        String selected = dialog.getDirectory() + File.separator + dialog.getFile();
        if (selected != null) {
          return new File(selected);
        }
      } else {

        // Display the file save dialog.
        JFileChooser filechooser = new JFileChooser(currentDirectoryPath);
        filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        filechooser.setDialogTitle(title);

        // Default file name
        filechooser.setSelectedFile(new File(defaultname));

        // Display file save dialog
        File file = null;
        while (file == null) {
          int selected = filechooser.showSaveDialog(parent);
          if (selected != JFileChooser.APPROVE_OPTION) break;
          file = filechooser.getSelectedFile();
          if (isOverwriteConfirmed(file, parent)) {
            break;
          }
          file = null;
        }
        return file;
      }
    } catch (Exception e) {

    }

    return null;
  }

  /**
   * Display the file overwrite confirmation dialog.
   *
   * @param file File to be overwritten
   * @param frame Parent component
   * @return true = overwrite
   */
  private static boolean isOverwriteConfirmed(File file, Component frame) {

    if (!file.exists()) return true;
    // swingutils.savefiledialog.overwrite.title = Overwrite confirmation
    String title = Message.getString("swingutils.savefiledialog.overwrite.title");
    // swingutils.savefiledialog.overwrite.message = File [% s] already exists. Do you want to
    // replace the existing file?
    String message =
        Message.getString("swingutils.savefiledialog.overwrite.message", file.getName());
    int confirm =
        JOptionPane.showConfirmDialog(
            frame, message, title, JOptionPane.WARNING_MESSAGE, JOptionPane.OK_CANCEL_OPTION);

    if (confirm == JOptionPane.OK_OPTION) return true;

    return false;
  }

  /**
   * Check the parent-child relationship of the tree node. <br>
   * Returns false if the parent and child nodes are the same
   *
   * @param parent Parent node
   * @param child Child node
   * @return treu = parent-child node
   */
  public static boolean isChildNode(DefaultMutableTreeNode parent, DefaultMutableTreeNode child) {
    if (parent == child) return false;
    DefaultMutableTreeNode node = child;
    while ((node = (DefaultMutableTreeNode) node.getParent()) != null) {
      if (node.getUserObject() == parent.getUserObject()) return true;
    }
    return false;
  }

  /**
   * Get the tree path
   *
   * @param treeNode Tree node
   * @return Tree path
   */
  public static TreePath getTreePath(TreeNode treeNode) {
    List<Object> nodes = new ArrayList<Object>();
    if (treeNode != null) {
      nodes.add(treeNode);
      treeNode = treeNode.getParent();
      while (treeNode != null) {
        nodes.add(0, treeNode);
        treeNode = treeNode.getParent();
      }
    }
    return nodes.isEmpty() ? null : new TreePath(nodes.toArray());
  }

  /**
   * Convert from TreeNode to CSV string
   *
   * @param node Tree node
   * @return CSV string
   */
  public static String toCsv(TreeNode node) {

    String buf = toCsv(node, 0);

    return buf;
  }

  /**
   * Convert from TableModel to CSV string
   *
   * @param model Table model
   * @return CSV string
   */
  public static String toCsv(TableModel model) {
    return toCsv(model, null);
  }

  /**
   * Convert from TableModel to CSV string
   *
   * @param model Table model
   * @param visibled Output column settings
   * @return CSV string
   */
  public static String toCsv(TableModel model, boolean[] visibled) {

    StringBuffer buf = new StringBuffer();

    // Table data
    int column = model.getColumnCount();
    int row = model.getRowCount();

    // header
    StringBuffer header = new StringBuffer();
    for (int i = 0; i < column; i++) {
      if (visibled != null && !visibled[i]) continue;
      String name = model.getColumnName(i);
      // Do not output if the column name is empty.
      if (name == null || name.isEmpty()) continue;
      if (header.length() > 0) header.append(",");
      header.append(escapeCsv(name));
    }
    buf.append(header);
    buf.append("\n");

    // data
    for (int i = 0; i < row; i++) {
      StringBuffer line = new StringBuffer();
      for (int j = 0; j < column; j++) {
        if (visibled != null && !visibled[j]) continue;
        String name = model.getColumnName(j);
        // Do not output if the column name is empty.
        if (name == null || name.isEmpty()) continue;

        // data
        String value = model.getValueAt(i, j) != null ? model.getValueAt(i, j).toString() : "";
        if (line.length() > 0) line.append(",");
        line.append(escapeCsv(value));
      }
      buf.append(line);
      buf.append("\n");
    }

    return buf.toString();
  }

  /**
   * Convert only selected rows from JTable to CSV string
   *
   * @param table table
   * @return CSV string
   */
  public static String toCsvOfSeletedRows(JTable table) {

    TableModel model = table.getModel();
    StringBuffer buf = new StringBuffer();

    // Table data
    int col = model.getColumnCount();

    // Selected line
    int[] selections = table.getSelectedRows();
    if (selections == null || selections.length <= 0) {
      return null;
    }
    for (int i = 0; i < selections.length; i++) {
      // Convert to the number of rows in the table model
      selections[i] = table.convertRowIndexToModel(selections[i]);
    }

    DefaultTableColumnModel columnModel = (DefaultTableColumnModel) table.getColumnModel();
    // header
    StringBuffer header = new StringBuffer();
    for (int i = 0; i < col; i++) {
      // Do not output column width 0
      TableColumn column = columnModel.getColumn(i);
      int width = column.getPreferredWidth();
      if (width <= 0) continue;
      // Do not output if the column name is empty.
      String name = model.getColumnName(i);
      if (name == null || name.isEmpty()) continue;
      if (header.length() > 0) header.append(",");
      header.append(escapeCsv(name));
    }
    buf.append(header);
    buf.append("\n");

    // data
    for (int row : selections) {
      StringBuffer line = new StringBuffer();
      for (int j = 0; j < col; j++) {
        // Do not output column width 0
        TableColumn column = columnModel.getColumn(j);
        int width = column.getPreferredWidth();
        if (width <= 0) continue;
        String name = model.getColumnName(j);
        // Do not output if the column name is empty.
        if (name == null || name.isEmpty()) continue;

        // data
        String value = model.getValueAt(row, j) != null ? model.getValueAt(row, j).toString() : "";
        if (line.length() > 0) line.append(",");
        line.append(escapeCsv(value));
      }
      buf.append(line);
      buf.append("\n");
    }

    return buf.toString();
  }

  /**
   * Convert from TreeNode to CSV string
   *
   * @param node Tree node
   * @param depth Hierarchy
   * @return CSV string
   */
  private static String toCsv(TreeNode node, int depth) {
    StringBuffer buf = new StringBuffer();
    String depthtext = "";
    for (int i = 0; i < depth; i++) {
      depthtext += ",";
    }
    buf.append(depthtext + escapeCsv(node.toString()));
    buf.append("\n");
    for (int i = 0; i < node.getChildCount(); i++) {
      String child = toCsv(node.getChildAt(i), depth + 1);
      buf.append(child);
    }

    return buf.toString();
  }

  /**
   * Escape the CSV output string. <br>
   * Double quotes add'\'. <br>
   * If there are commas or line breaks in the string, enclose them in double quotes. <br>
   *
   * @param text CSV output string
   * @return Escape string
   */
  public static String escapeCsv(String text) {
    if (text == null) return null;
    text = text.replaceAll("\"", "\\\"");
    if (text.indexOf(",") >= 0 || text.indexOf("\n") >= 0) {
      text = "\"" + text + "\"";
    }
    return text;
  }

  /**
   * Synchronize two views
   *
   * @param masterViewport Original view
   * @param slaveViewport Sync destination view
   * @param orientation Synchronous direction (Swing Constants.HORIZONTAL (0x00) = horizontal
   *     direction, SwingConstants.VERTICAL (0x01) = vertical)
   */
  public static void synchronizeView(
      final JViewport masterViewport, final JViewport slaveViewport, final int orientation) {
    final ChangeListener c1 =
        new ChangeListener() {
          public void stateChanged(ChangeEvent e) {
            if (masterViewport.getView() == null || slaveViewport.getView() == null) {
              return;
            }
            if (orientation == SwingConstants.HORIZONTAL) {
              Point v1 = masterViewport.getViewPosition();
              Point v2 = slaveViewport.getViewPosition();
              if (v1.x != v2.x) {
                slaveViewport.setViewPosition(new Point(v1.x, v2.y));
              }
            } else if (orientation == SwingConstants.VERTICAL) {
              Point v1 = masterViewport.getViewPosition();
              Point v2 = slaveViewport.getViewPosition();
              if (v1.y != v2.y) {
                slaveViewport.setViewPosition(new Point(v2.x, v1.y));
              }
            }
          }
        };

    masterViewport.addChangeListener(c1);
  }

  /**
   * Copy the string to the clipboard.
   *
   * @param text Clipboard copy string
   */
  public static void copyClipboard(String text) {
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    StringSelection selection = new StringSelection(text);
    clipboard.setContents(selection, selection);

    return;
  }

  /**
   * Calculate the gradation color in RGB.
   *
   * @param ratio Gradient color ratio (0.0 = minColor ~ 1.0 = maxColor)
   * @param minColor Color when ratio = 0.0
   * @param maxColor Color when ratio = 1.0
   * @return Gradient color
   */
  public static Color getGradientRgbColor(float ratio, Color minColor, Color maxColor) {
    if (ratio < 0) ratio = 0.0F;
    if (ratio > 1.0) ratio = 1.0F;
    int red = (int) (maxColor.getRed() * ratio + minColor.getRed() * (1.0 - ratio));
    int green = (int) (maxColor.getGreen() * ratio + minColor.getGreen() * (1 - ratio));
    int blue = (int) (maxColor.getBlue() * ratio + minColor.getBlue() * (1 - ratio));
    Color gradient = new Color(red, green, blue);
    return gradient;
  }

  /**
   * Calculate the gradation color in HSB.
   *
   * @param ratio Gradient color ratio (0.0 = minColor ~ 1.0 = maxColor)
   * @param minColor Color when ratio = 0.0
   * @param maxColor Color when ratio = 1.0
   * @return Gradient color
   */
  public static Color getGradientHsbColor(float ratio, Color minColor, Color maxColor) {
    float[] startHSB =
        Color.RGBtoHSB(minColor.getRed(), minColor.getGreen(), minColor.getBlue(), null);
    float[] endHSB =
        Color.RGBtoHSB(maxColor.getRed(), maxColor.getGreen(), maxColor.getBlue(), null);

    float brightness = (startHSB[2] + endHSB[2]) / 2;
    float saturation = (startHSB[1] + endHSB[1]) / 2;

    float hueMax = 0;
    float hueMin = 0;
    // if (startHSB[0] > endHSB[0]) {
    hueMax = startHSB[0];
    hueMin = endHSB[0];
    // } else {
    // hueMin = startHSB[0];
    // hueMax = endHSB[0];
    // }

    float hue = ((hueMax - hueMin) * (1.0F - ratio)) + hueMin;

    return Color.getHSBColor(hue, saturation, brightness);
  }

  /**
   * Create a block list-only tree from the block tree.
   *
   * @param root Block tree
   * @param list Block list
   * @return Block list only tree
   */
  public static DefaultMutableTreeNode createTreeNode(
      DefaultMutableTreeNode root, List<Object> list) {
    if (root == null) return null;
    if (list == null || list.size() <= 0) return null;
    DefaultMutableTreeNode top = new DefaultMutableTreeNode();
    if (list.get(0) instanceof BlockList) {
      for (Object obj : list) {
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(obj);
        if (top.getChildCount() == 0) {
          top.add(node);
        } else {
          ((DefaultMutableTreeNode) top.getChildAt(0)).add(node);
        }
      }
      return top;
    }
    for (Object obj : list) {
      DefaultMutableTreeNode node = new DefaultMutableTreeNode(obj);
      if (top.getChildCount() == 0) {
        top.add(node);
      } else {
        Object[] objpath = searchTreePath(root, obj);
        DefaultMutableTreeNode parent = searchParentNode(top, objpath);
        if (parent != null) {
          parent.add(node);
        } else {
          top.add(node);
        }
      }
    }

    return top;
  }

  /**
   * Get the user object path list of the node that matches the user object from the tree node. <br>
   *
   * @param root Tree node
   * @param obj User object
   * @return User object path list
   */
  private static Object[] searchTreePath(DefaultMutableTreeNode root, Object obj) {

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject() == obj) {
        return treeNode.getUserObjectPath();
      }
    }
    return null;
  }

  /**
   * Search the tree node for a node that matches the user object path list. <br>
   * Get the node that best matches the user object path list. <br>
   * Not the same node as the user object path list.
   *
   * @param root Tree node
   * @param objpath User object path list
   * @return Matching node
   */
  private static DefaultMutableTreeNode searchParentNode(
      DefaultMutableTreeNode root, Object[] objpath) {

    // List tree nodes in the forward direction
    int depthCount = -1;
    DefaultMutableTreeNode depthNode = root;
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();
      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }

      for (int i = 0; i < objpath.length; i++) {
        if (((DefaultMutableTreeNode) treeNode).getUserObject() == objpath[i]) {
          if (depthCount < i) {
            depthCount = i;
            depthNode = treeNode;
          }
        }
      }
    }
    if (depthCount == -1) return null;
    return depthNode;
  }

  /**
   * Get a copy of the original node.
   *
   * @param srcNode Original node
   * @return Copy node
   */
  public static DefaultMutableTreeNode cloneTreeNode(DefaultMutableTreeNode srcNode) {
    if (srcNode == null) return null;
    DefaultMutableTreeNode node = new DefaultMutableTreeNode(srcNode.getUserObject());
    for (int i = 0; i < srcNode.getChildCount(); i++) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) srcNode.getChildAt(i);
      addCloneNode(child, node);
    }
    return node;
  }

  /**
   * Add a copy node of the additional child node to the parent node.
   *
   * @param srcNode Additional child node
   * @param root Additional parent node
   * @return Copy node of add-on node
   */
  private static DefaultMutableTreeNode addCloneNode(
      DefaultMutableTreeNode srcNode, DefaultMutableTreeNode root) {
    DefaultMutableTreeNode clone = new DefaultMutableTreeNode(srcNode.getUserObject());
    root.add(clone);
    for (int i = 0; i < srcNode.getChildCount(); i++) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) srcNode.getChildAt(i);
      addCloneNode(child, clone);
    }
    return clone;
  }

  /**
   * Check if it is a circular reference node.
   *
   * @param node Check node
   * @return true = Circular reference node
   */
  public static boolean recursiveTreeNode(DefaultMutableTreeNode node) {
    if (node == null) return false;
    Object[] objs = node.getUserObjectPath();
    if (objs == null) return false;
    for (int i = 0; i < objs.length; i++) {
      for (int j = i; j < objs.length; j++) {
        if (i == j) continue;
        if (objs[i] == objs[j]) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Get the number of all offspring nodes of the tree node.
   *
   * @param node Tree node
   * @return Number of offspring nodes
   */
  public static int getAllChildCount(TreeNode node) {
    if (node == null) return 0;
    int count = 0;
    int childCount = node.getChildCount();
    for (int i = 0; i < childCount; i++) {
      count++;
      TreeNode child = node.getChildAt(i);
      count += getAllChildCount(child);
    }
    return count;
  }

  /**
   * Sort the tree in ascending order.
   *
   * @param root Sorted tree
   * @return Sort result tree
   */
  public static DefaultMutableTreeNode sortTreeNode(DefaultMutableTreeNode root) {
    if (root == null) return root;
    int count = root.getChildCount();
    if (count <= 0) return root;
    for (int i = 0; i < count; i++) {
      for (int j = count - 1; j > i; j--) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) root.getChildAt(j);
        String nt = node.getUserObject().toString();
        DefaultMutableTreeNode prevNode = (DefaultMutableTreeNode) root.getChildAt(j - 1);
        String np = prevNode.getUserObject().toString();
        if (nt.compareToIgnoreCase(np) < 0) {
          root.insert(node, j - 1);
          root.insert(prevNode, j);
        }
      }
      sortTreeNode((DefaultMutableTreeNode) root.getChildAt(i));
    }

    for (int i = 0; i < count; i++) {
      for (int j = count - 1; j > i; j--) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) root.getChildAt(j);
        DefaultMutableTreeNode prevNode = (DefaultMutableTreeNode) root.getChildAt(j - 1);
        if (prevNode.isLeaf() && !node.isLeaf()) {
          root.insert(node, j - 1);
          root.insert(prevNode, j);
        }
      }
    }

    return root;
  }

  /**
   * Get the fonts for "Meiryo UI" and "Dialog". <br>
   * If the "Meiryo UI" and "Dialog" fonts do not exist, use the "Monospaced" font.
   *
   * @return Default font
   */
  public static Font getDefaultFont() {
    final int DEFAULTFONT_SIZE = 11;
    final String[] defaultnames = {"Meiryo UI", "Dialog"};
    // Get the font name list
    GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
    String[] fontNames = env.getAvailableFontFamilyNames();

    // Set default, logical font
    Font defaultFont = new Font("Monospaced", Font.PLAIN, DEFAULTFONT_SIZE);

    // Search for physical fonts
    if (fontNames != null && fontNames.length > 0) {
      for (int j = 0; j < defaultnames.length; j++) {
        String name = defaultnames[j];
        for (int i = 0; i < fontNames.length; i++) {
          if (fontNames[i].equals(name)) {
            defaultFont = new Font(name, Font.PLAIN, DEFAULTFONT_SIZE);
            return defaultFont;
          }
        }
      }
    }
    return defaultFont;
  }

  /**
   * Convert from TreeNode to tree format string
   *
   * @param node Tree node
   * @return CSV string
   */
  public static String toTreeText(TreeNode node) {
    StringBuffer buf = new StringBuffer();
    Enumeration<?> elems = ((DefaultMutableTreeNode) node).preorderEnumeration();
    try {
      while (elems.hasMoreElements()) {
        DefaultMutableTreeNode next = (DefaultMutableTreeNode) elems.nextElement();
        Object obj = next.getUserObject();
        String text = null;
        File file = null;
        if (obj instanceof SourceFile) {
          file = ((SourceFile) obj).getFile();
        } else if (obj instanceof File) {
          file = (File) obj;
        }
        if (file != null) {
          if (next.isRoot()) {
            text = file.getCanonicalPath();
          } else {
            text = file.getName();
          }
        } else {
          text = next.toString();
        }

        StringBuffer leaf = new StringBuffer();
        TreeNode[] paths = next.getPath();
        DefaultMutableTreeNode previous = (DefaultMutableTreeNode) paths[0];
        for (int i = 0; i < paths.length - 1; i++) {
          if (previous == paths[i]) continue;
          if (previous.getLastChild() == paths[i]) {
            leaf.append("    ");
          } else {
            leaf.append("|   ");
          }
        }
        DefaultMutableTreeNode parent = (DefaultMutableTreeNode) next.getParent();
        if (parent == null) {
          leaf.append("");
        } else if (parent.getLastChild() == next) {
          leaf.append("`-- ");
        } else {
          leaf.append("|-- ");
        }
        buf.append(leaf);
        buf.append(text);
        buf.append("\n");
      }
    } catch (IOException e) {
      e.printStackTrace();
    }

    return buf.toString();
  }
}
