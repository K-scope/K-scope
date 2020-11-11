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
 */
package jp.riken.kscope.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;
import javax.swing.BorderFactory;
import javax.swing.BoundedRangeModel;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.JViewport;
// import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Console screen class
 *
 * @author RIKEN
 */
public class ConsolePanel extends AnalisysPanelBase
    implements FocusListener, IAnalisysComponent, ComponentListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Tab size */
  private final int TAB_SIZE = 4;
  /** Console text box */
  private JTextPane consoleTextPane;
  /** Clear button */
  private JButton btnClear;
  /** Label */
  private JLabel label;
  /** Console output queue */
  private Queue<PrintQueue> listout;

  /** Standard output stream: System default */
  private PrintStream sysOut = System.out;
  /** Standard error output stream: system default */
  private PrintStream sysErr = System.err;

  public boolean disable_horizontal_scroll = false;

  /**
   * Console output queue
   *
   * @author RIKEN
   */
  private class PrintQueue {
    public String text = null;
    public SimpleAttributeSet attr = null;

    public PrintQueue(String text, SimpleAttributeSet attr) {
      this.text = text;
      this.attr = attr;
    }
  }

  private WorkerThread worker;
  /** Constructor */
  public ConsolePanel() {
    super();
    initGUI();
    listout = new ArrayBlockingQueue<PrintQueue>(KscopeProperties.CONSOLE_QUEUESIZE);
  }

  /**
   * Constructor
   *
   * @param console Analysis information panel identifier
   */
  public ConsolePanel(ANALYSIS_PANEL console) {
    super(console);
    initGUI();
    listout = new ArrayBlockingQueue<PrintQueue>(KscopeProperties.CONSOLE_QUEUESIZE);
  }

  /** Initialize the GUI */
  @SuppressWarnings("serial")
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      this.setLayout(thisLayout);
      //            setPreferredSize(new Dimension(400, 64));

      // Information label at the top, button placement panel
      {
        JPanel panelTop = new JPanel();
        panelTop.setLayout(new BorderLayout());
        this.add(panelTop, BorderLayout.NORTH);
        panelTop.setBorder(
            new CompoundBorder(
                new LineBorder(Color.BLACK, 1), BorderFactory.createEmptyBorder(0, 5, 0, 20)));
        // Button layout panel
        {
          JPanel panelButtons = new JPanel();
          panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
          panelTop.add(panelButtons, BorderLayout.EAST);

          java.awt.Dimension buttonSize = new java.awt.Dimension(24, 24);
          // Clear button
          {
            Icon icon = ResourceUtils.getIcon("removeall.gif");
            btnClear = new JButton(icon);
            panelButtons.add(btnClear);
            btnClear.setContentAreaFilled(false);
            btnClear.setBorderPainted(false);
            btnClear.setPreferredSize(buttonSize);
            btnClear.setMinimumSize(buttonSize);
            btnClear.setMaximumSize(buttonSize);
            btnClear.addActionListener(
                new ActionListener() {
                  @Override
                  public void actionPerformed(ActionEvent e) {
                    // Clear the console.
                    clearConsole();
                  }
                });
          }
        }

        // Label placement
        {
          label = new JLabel();
          panelTop.add(label, BorderLayout.CENTER);
          // label.setText().getString("");
        }
      }
      // Console text pine
      {
        // Make it read-only.
        consoleTextPane =
            new JTextPane() {
              @Override
              public boolean getScrollableTracksViewportWidth() {
                // Do not wrap
                try {
                  Object parent = getParent();
                  if (parent instanceof JViewport) {
                    JViewport port = (JViewport) parent;
                    int w = port.getWidth(); // Displayable range (upper limit)
                    TextUI ui = getUI();
                    if (ui == null) return true;
                    Dimension sz = ui.getPreferredSize(this); // Actual string size
                    if (sz == null) return true;
                    if (sz.width < w) {
                      return true;
                    }
                  }
                } catch (Exception ex) {
                  ex.printStackTrace();
                }
                return false;
              }
            };
        consoleTextPane.setEditable(false);
        consoleTextPane.getCaret().setVisible(true); // Show the caret

        // Set the tab size.
        SwingUtils.setTabSize(consoleTextPane, TAB_SIZE);

        final JScrollPane scroll = new JScrollPane();
        scroll.setViewportView(consoleTextPane);
        this.add(scroll, BorderLayout.CENTER);

        // disable horizontal scroll
        scroll
            .getHorizontalScrollBar()
            .addAdjustmentListener(
                new AdjustmentListener() {
                  BoundedRangeModel brm = scroll.getHorizontalScrollBar().getModel();
                  JScrollBar sb = scroll.getHorizontalScrollBar();

                  @Override
                  public void adjustmentValueChanged(AdjustmentEvent arg0) {
                    if (!brm.getValueIsAdjusting() && disable_horizontal_scroll)
                      sb.setValue(sb.getMinimum());
                  }
                });
      }

      // OutputStream os = new JTextAreaOutputStream(consoleTextPane, "UTF-8");
      // System.setOut(new PrintStream(os, true));

      // Focus event registration
      this.consoleTextPane.addFocusListener(this);

      this.addComponentListener(this);

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  class TextpaneListener implements CaretListener {

    JScrollPane scroll;

    public TextpaneListener(JScrollPane scroll) {
      this.scroll = scroll;
    }

    @Override
    public void caretUpdate(CaretEvent e) {
      SwingUtilities.invokeLater(
          new Runnable() {
            public void run() {
              JScrollBar sb = scroll.getHorizontalScrollBar();
              sb.setValue(sb.getMinimum());
            }
          });
    }
  }

  /** Clear the console */
  public void clearConsole() {
    StyleContext sc = new StyleContext();
    DefaultStyledDocument doc = new DefaultStyledDocument(sc);
    this.consoleTextPane.setDocument(doc);
    consoleTextPane.setCaretPosition(0);
    consoleTextPane.getCaret().setVisible(true); // Show the caret

    // Clear queue
    listout.clear();
  }

  private void addQueue(PrintQueue queue) {
    if (!listout.offer(queue)) {
      // Delete the beginning
      listout.poll();
      listout.offer(queue);
    }
  }

  /**
   * Output to console
   *
   * @param text Output string
   * @param error true = error output
   */
  private void updateTextPane(final String text, final boolean error) {
    Color fontcolor = error ? Color.RED : Color.BLACK;
    updateTextPane(text, fontcolor);
  }

  /**
   * Output to console
   *
   * @param text Output string
   * @param error true = error output
   */
  private void updateTextPane(final String text, final Color fontcolor) {

    SimpleAttributeSet attr = new SimpleAttributeSet();
    StyleConstants.setForeground(attr, fontcolor);

    // Add to queue
    addQueue(new PrintQueue(text, attr));

    // Start a thread
    if (worker == null) {
      try {
        worker = new WorkerThread();
        worker.execute();
      } catch (Exception ex) {
      }
    }
  }

  /**
   * Output to console
   *
   * @param text Output string
   */
  public void outputString(final String text) {
    Color fontcolor = Color.BLACK;
    updateTextPane(text, fontcolor);
  }

  /**
   * Thread class that displays queue information in a text box
   *
   * @author RIKEN
   */
  private class WorkerThread extends SwingWorker<Object, Object> {

    /**
     * Display queue information in a text box
     *
     * @return Calculation result
     * @throws Exception Queue information display error
     */
    @Override
    protected Object doInBackground() throws Exception {

      SwingUtilities.invokeLater(
          new Runnable() {
            @Override
            public void run() {

              // StyleContext sc = new StyleContext();
              // DefaultStyledDocument doc = new DefaultStyledDocument(sc);
              // consoleTextPane.setDocument(doc);

              Document doc = consoleTextPane.getDocument();
              // Set the tab size.
              SwingUtils.setTabSize(consoleTextPane, TAB_SIZE);

              PrintQueue queue = null;
              while ((queue = listout.poll()) != null) {
                // for (PrintQueue queue : listout) {
                try {
                  doc.insertString(doc.getLength(), queue.text, queue.attr);
                  ConsolePanel.this.validate();
                  ConsolePanel.this.repaint();

                } catch (BadLocationException e) {
                  throw new RuntimeException(e);
                }
              }
              int len = doc.getLength();
              if (len > 0) len = len - 1;
              consoleTextPane.setCaretPosition(len);
              consoleTextPane.getCaret().setVisible(true); // Show the caret
            }
          });
      return null;
    }

    @Override
    protected void done() {
      ConsolePanel.this.worker = null;
    }
  }

  /** Unhook System.out/err. */
  private void terminateSystemStreams() {
    System.setOut(this.sysOut);
    System.setErr(this.sysErr);
  }

  /** Hook System.out/err */
  private void redirectSystemStreams() {
    OutputStream stdout =
        new OutputStream() {
          @Override
          public void write(final int b) throws IOException {
            updateTextPane(String.valueOf((char) b), false);
            this.flush();
          }

          @Override
          public void write(byte[] b, int off, int len) throws IOException {
            updateTextPane(new String(b, off, len), false);
            this.flush();
          }

          @Override
          public void write(byte[] b) throws IOException {
            updateTextPane(new String(b), false);
            this.flush();
          }
        };

    OutputStream errout =
        new OutputStream() {
          @Override
          public void write(final int b) throws IOException {
            updateTextPane(String.valueOf((char) b), true);
          }

          @Override
          public void write(byte[] b, int off, int len) throws IOException {
            updateTextPane(new String(b, off, len), true);
          }

          @Override
          public void write(byte[] b) throws IOException {
            write(b, 0, b.length);
          }
        };

    System.setOut(new PrintStream(stdout, true));
    System.setErr(new PrintStream(errout, true));
  }

  /**
   * Textbox output class (unused).
   *
   * @author RIKEN
   */
  public class JTextAreaOutputStream extends OutputStream {
    private ByteArrayOutputStream os;
    /** Output textbox */
    @SuppressWarnings("unused")
    private JTextPane textBox;
    /** Encode */
    private String encode;

    /**
     * Constructor
     *
     * @param textArea Textbox
     * @param encode encode
     */
    public JTextAreaOutputStream(JTextPane textArea, String encode) {
      this.textBox = textArea;
      this.encode = encode;
      this.os = new ByteArrayOutputStream();
    }
    /** Override OutputStream # write (byte []) */
    @Override
    public void write(int arg) throws IOException {
      this.os.write(arg);
    }
    /** Export to JTextArea with flush () */
    @Override
    public void flush() throws IOException {
      // String encoding
      final String str = new String(this.os.toByteArray(), this.encode);
      // Actual export process
      SwingUtilities.invokeLater(
          new Runnable() {
            @Override
            public void run() {
              Color fontcolor = Color.BLACK;
              updateTextPane(str, fontcolor);

              /*
              Document doc = JTextAreaOutputStream.this.textBox.getDocument ();
              SimpleAttributeSet attr = new SimpleAttributeSet ();
              StyleConstants.setForeground (attr, fontcolor);

              try {
                  doc.insertString (doc.getLength (), str, attr);
              } catch (BadLocationException e) {
                  throw new RuntimeException (e);
              }
              JTextAreaOutputStream.this.textBox.setCaretPosition (doc.getLength () -- 1);
              JTextAreaOutputStream.this.textBox.getCaret (). setVisible (true); // Show caret
              */
            }
          });
      // Clear the exported content
      this.os.reset();
    }
  }

  /**
   * Get the OutputStream of the console output.
   *
   * @return OutputStream
   */
  public OutputStream getOutputStream() {
    OutputStream os = new JTextAreaOutputStream(consoleTextPane, "UTF-8");
    return os;
  }

  /**
   * Focus acquisition event. <br>
   * Since the caret is hidden when the focus is moved, the caret display is reset when the focus is
   * acquired.
   *
   * @param event Event information
   */
  @Override
  public void focusGained(FocusEvent event) {
    this.consoleTextPane.getCaret().setVisible(true);
  }

  /**
   * Loss of focus event
   *
   * @param event Event information
   */
  @Override
  public void focusLost(FocusEvent event) {}

  /**
   * Export analysis information
   *
   * @param file Output file
   */
  @Override
  public void export(File file) {
    if (file == null) return;

    String text = this.consoleTextPane.getText();

    try {
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));
      pw.println(text);
      pw.close();

    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Add a tab focus listener.
   *
   * @param listener Tab focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    consoleTextPane.addFocusListener(listener);
    btnClear.addFocusListener(listener);
  }

  /**
   * Set an action listener on the panel. <br>
   * Assign the created action listener to the menu bar to the panel button.
   *
   * @param menu Menu bar
   */
  @Override
  public void setActionListener(MainMenu menu) {
    // No assign button
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    clearConsole();
  }

  /**
   * Component resizing event.
   *
   * @param e Event information
   */
  @Override
  public void componentResized(ComponentEvent e) {}

  /**
   * Component move event.
   *
   * @param e Event information
   */
  @Override
  public void componentMoved(ComponentEvent e) {}

  /**
   * Component display event
   *
   * @param e Event information
   */
  @Override
  public void componentShown(ComponentEvent e) {
    // Show event also occurs when the component is hidden
    if (isTabVisible()) {
      // Output System output to the console text pane.
      redirectSystemStreams();
    }
  }

  /**
   * Component hiding event
   *
   * @param e Event information
   */
  @Override
  public void componentHidden(ComponentEvent e) {
    // Even if the component is hidden, the hook will continue if the console tab is not closed.
    if (!isTabVisible()) {
      // Unhook the System.out/err.
      terminateSystemStreams();
    }
  }

  /**
   * Check if the console tab is displayed. <br>
   * True if it is included in the tab, even if it is not active
   *
   * @return true = Tabs are displayed and included
   */
  private boolean isTabVisible() {
    int index = ((AnalysisView) this.getParentComponent()).getTabIndex(this.getEnumPanel());
    return (index >= 0);
  }

  /** Close the tab */
  @Override
  public void closeTab() {
    // Unhook the System.out/err.
    terminateSystemStreams();
  }

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  @Override
  public CodeLine getSelectedCodeLine() {
    return null;
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    return null;
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  @Override
  public IInformation getSelectedInformation() {
    return null;
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  @Override
  public void setSourceProperties(SourceProperties properties) {}

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {
    this.consoleTextPane.copy();
  }

  /** Whether there is data to export */
  @Override
  public boolean isExportable() {
    String text = this.consoleTextPane.getText();
    return (!text.isEmpty());
  }

  /** Write the queue string to JTextArea */
  public void flush() {
    try {
      OutputStream out = this.getOutputStream();
      if (out != null) {
        out.flush();
      }
    } catch (Exception ex) {
    }
  }
}
