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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.net.URL;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.html.HTML;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.InformationModel;
// import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Additional information panel class
 *
 * @author RIKEN
 */
public class InformationPanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, HyperlinkListener, ActionListener, MouseListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Edit button */
  private JButton btnEdit;
  /** Open file button */
  private JButton btnOpenFile;
  /** Export button */
  private JButton btnExport;
  /** Lock button */
  private JButton btnLock;
  /** Additional information label */
  private JLabel label;
  /** Content Box */
  private Box contentInfo;
  /** Margin box */
  private final Component glue = Box.createVerticalGlue();
  /** Scroll pine */
  private JScrollPane scrollInfo;

  /** Additional information model */
  private InformationModel model;

  /** Expand button icon */
  private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
  /** Storage button icon */
  private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

  /** Display locked state */
  private boolean viewLock = false;

  /** Select additional information node panel */
  private NodePanel selectedInfo;

  /** Additional information editing action */
  private EditInformationEditAction actionEdit;
  /** Margin under the additional information panel */
  private final int EDITPANE_BOTTOM_MERGE = 16;
  /** Selection panel background color */
  private Color colorSelectedPanel;

  /** Constructor */
  public InformationPanel() {
    super();

    // Initialize.
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   */
  public InformationPanel(ANALYSIS_PANEL panel) {
    super(panel);

    // Initialize.
    initialize();
  }

  /** Initialize. */
  private void initialize() {

    // Generate a model
    model = new InformationModel();
    // Set the observer.
    model.addObserver(this);

    // Initialize the GUI.
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      this.setLayout(thisLayout);
      setPreferredSize(new Dimension(400, 0));

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
          // edit button
          {
            Icon icon = ResourceUtils.getIcon("edit_info.gif");
            btnEdit = new JButton(icon);
            panelButtons.add(btnEdit);
            btnEdit.setContentAreaFilled(false);
            btnEdit.setBorderPainted(false);
            btnEdit.setPreferredSize(buttonSize);
            btnEdit.setMinimumSize(buttonSize);
            btnEdit.setMaximumSize(buttonSize);
          }
          // Margin setting
          // panelButtons.add(Box.createHorizontalStrut(5));
          {
            Icon icon = ResourceUtils.getIcon("openfile.gif");
            btnOpenFile = new JButton(icon);
            btnOpenFile.setContentAreaFilled(false);
            btnOpenFile.setBorderPainted(false);
            btnOpenFile.setPreferredSize(buttonSize);
            btnOpenFile.setMinimumSize(buttonSize);
            btnOpenFile.setMaximumSize(buttonSize);
            panelButtons.add(btnOpenFile);
          }
          // Margin setting
          // panelButtons.add(Box.createHorizontalStrut(5));
          {
            Icon icon = ResourceUtils.getIcon("save.gif");
            btnExport = new JButton(icon);
            btnExport.setContentAreaFilled(false);
            btnExport.setBorderPainted(false);
            btnExport.setPreferredSize(buttonSize);
            btnExport.setMinimumSize(buttonSize);
            btnExport.setMaximumSize(buttonSize);
            panelButtons.add(btnExport);
          }
          // Margin setting
          // panelButtons.add(Box.createHorizontalStrut(5));
          {
            Icon icon = ResourceUtils.getIcon("unlock.gif");
            btnLock = new JButton(icon);
            btnLock.setContentAreaFilled(false);
            btnLock.setBorderPainted(false);
            btnLock.setPreferredSize(buttonSize);
            btnLock.setMinimumSize(buttonSize);
            btnLock.setMaximumSize(buttonSize);
            panelButtons.add(btnLock);
            btnLock.addActionListener(this);
          }
        }
        // Label placement
        {
          label = new JLabel();
          panelTop.add(label, BorderLayout.CENTER);
          label.setText("");
        }
      }
      {
        {
          // Additional information panel
          contentInfo = Box.createVerticalBox();
          // contentInfo.setBorder(BorderFactory.createLineBorder(Color.RED, 1));
          contentInfo.setOpaque(false);

          // Scroll pine
          scrollInfo = new JScrollPane(contentInfo);
          scrollInfo.getVerticalScrollBar().setUnitIncrement(25);
          scrollInfo.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollInfo.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollInfo.getViewport().setBackground(Color.WHITE);

          add(scrollInfo);
        }
      }

      // Tooltip settings
      btnEdit.setToolTipText(Message.getString("mainmenu.edit")); // edit
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export
      btnOpenFile.setToolTipText(
          Message.getString("informationpanel.tooltip.openblock")); // Open the information section
      btnLock.setToolTipText(Message.getString("informationpanel.tooltip.lock")); // Display lock

      // Button event
      btnEdit.addActionListener(this);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Additional information model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {

    // If the display is locked, do not change the additional information model.
    if (this.viewLock) {
      // Only reflect the editing of selected additional information
      if (selectedInfo == null) return;

      // Update display
      selectedInfo.refresh();
      return;
    }

    // Update the additional information model
    updateModel();

    return;
  }

  /** Update additional information model */
  public void updateModel() {

    // Clear additional information
    clearComponent();

    if (this.model == null) return;

    int count = this.model.getInformationListCount();
    for (int i = 0; i < count; i++) {
      String name = this.model.getInformationNode(i).toString();
      String htmlContent = this.model.getInformationHtmlContent(i);
      IInformation info = this.model.getInformationNode(i);

      // Add additional information
      addInformation(name, htmlContent, info);
    }

    return;
  }

  /**
   * Add additional information
   *
   * @param name Additional information name
   * @param htmlText Additional information HTML body
   * @param info Additional information setting block
   */
  public void addInformation(String name, String htmlText, IInformation info) {

    // Create additional components
    JComponent component = makeRowsPanel(name, htmlText, info);

    // Add a component to the content panel
    addComponent(component);
  }

  /** Clear the content panel */
  public void clearComponent() {
    this.contentInfo.removeAll();

    // redraw
    this.refresh();

    // Clear the selective additional information block
    this.selectedInfo = null;

    // Switching the display lock state
    toggleLockButton(false);
  }

  /**
   * Add components to the content panel
   *
   * @param component Additional component
   */
  private void addComponent(final JComponent component) {
    // Change the size of additional components
    component.setMaximumSize(new Dimension(Short.MAX_VALUE, component.getPreferredSize().height));

    // Place additional components left-justified
    component.setAlignmentX(Component.LEFT_ALIGNMENT);

    // Add component
    this.contentInfo.remove(glue);
    //        this.contentInfo.add(Box.createVerticalStrut(5));
    this.contentInfo.add(component);
    this.contentInfo.add(glue);

    // redraw
    this.refresh();

    EventQueue.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            component.scrollRectToVisible(component.getBounds());
            scrollInfo.getViewport().setViewPosition(new Point(0, 0));
          }
        });

    return;
  }

  /** Redraw */
  private void refresh() {

    // redraw
    this.contentInfo.revalidate();
    this.validate();
    this.repaint();
  }

  /**
   * Addition of additional information panel
   *
   * @param name Additional information: Name
   * @param htmlinfo Additional information: HTML format information
   * @param info Additional information setting block
   * @return Additional information panel
   */
  private JComponent makeRowsPanel(String name, String htmlinfo, IInformation info) {
    // Additional information panel
    NodePanel rows = new NodePanel(info);
    rows.setLayout(new BoxLayout(rows, BoxLayout.Y_AXIS));
    rows.setOpaque(false);
    rows.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
    rows.addMouseListener(this);

    // Name panel
    JPanel panelName = new JPanel();
    panelName.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
    panelName.setOpaque(false);

    // Additional information expansion button: Initial display is expansion button
    java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
    JButton button = new JButton(expand_icon);
    button.setContentAreaFilled(false);
    button.setBorderPainted(false);
    button.setPreferredSize(buttonSize);
    button.setMinimumSize(buttonSize);
    button.setMaximumSize(buttonSize);

    // Name label
    panelName.add(button);
    JLabel label = new JLabel(name);
    label.setOpaque(false);
    panelName.add(label);

    // Additional information body panel
    JPanel panelEditor = new JPanel();
    panelEditor.setLayout(new BorderLayout());
    panelEditor.setOpaque(false);

    // Additional information display text box
    JEditorPane editor =
        new JEditorPane() {
          /** Serial number */
          private static final long serialVersionUID = 1L;

          @Override
          public void updateUI() {
            super.updateUI();
            setEditorHeight();
          }

          @Override
          public void doLayout() {
            super.doLayout();
            setEditorHeight();
          }

          /** Extend the height of the additional information display text box to fit the text */
          private void setEditorHeight() {
            if (this.getDocument() == null) return;
            if (this.getDocument().getLength() <= 0) return;
            if (getUI() == null) return;

            Rectangle r = null;
            try {
              r = this.modelToView(this.getDocument().getLength());
            } catch (BadLocationException e1) {
            }

            if (r != null) {
              Dimension editorSize = this.getPreferredSize();
              editorSize.height = r.y + r.height + EDITPANE_BOTTOM_MERGE;
              this.setPreferredSize(editorSize);
            }
          }
        };
    // The additional information display text box is displayed in HTML.
    Dimension editorSize = new java.awt.Dimension(400, 22);
    editor.setEditable(false);
    editor.setContentType("text/html");
    editor.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
    editor.setOpaque(true);
    editor.setPreferredSize(editorSize);
    editor.setText(htmlinfo);

    // Add event listener
    editor.addHyperlinkListener(this);
    editor.addMouseListener(this);

    editor.validate();
    editor.updateUI();

    // Border setting of additional information display text box
    LineBorder border = new LineBorder(Color.GRAY, 1, false);
    editor.setBorder(border);

    // Additional information Body panel margin settings
    panelEditor.add(Box.createVerticalStrut(5), BorderLayout.NORTH);
    panelEditor.add(Box.createHorizontalStrut(40), BorderLayout.WEST);
    panelEditor.add(Box.createHorizontalStrut(40), BorderLayout.EAST);
    panelEditor.add(Box.createVerticalStrut(5), BorderLayout.SOUTH);
    panelEditor.add(editor, BorderLayout.CENTER);

    // Setting the action listener of the additional information expansion button
    button.addActionListener(new InformationExpandAction(panelEditor));

    // Add name panel and body panel to additional information panel
    rows.add(panelName);
    rows.add(panelEditor);

    // Set the text pane.
    rows.setEditorPane(editor);

    return rows;
  }

  /**
   * Get additional information model
   *
   * @return Additional information model
   */
  public InformationModel getModel() {
    return model;
  }

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    this.addFocusListener(listener);

    // Set focus listener for child components as well
    if (this.contentInfo != null) {
      this.contentInfo.addFocusListener(listener);
      this.btnEdit.addFocusListener(listener);
      this.btnExport.addFocusListener(listener);
    }
  }

  /** Export */
  @Override
  public void export(File file) {
    if (this.model == null) return;

    model.writeFile(file);
  }

  /**
   * Set an action listener on the panel. <br>
   * Assign the created action listener to the menu bar to the panel button.
   *
   * @param menu Menu bar
   */
  @Override
  public void setActionListener(MainMenu menu) {
    // Additional information editing action
    this.actionEdit = menu.getActionEditInformation();

    // Analysis information export action
    this.btnExport.addActionListener(menu.getActionExportAnalysis());

    // Open the relevant part
    this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Clear screen
    clearComponent();

    // Clear the model
    this.model.clearInformation();
  }

  /**
   * Additional information expansion button Action listener
   *
   * @author RIKEN
   */
  private class InformationExpandAction implements ActionListener {
    /** Additional information body panel */
    private JPanel panelEditor;

    /**
     * Constructor
     *
     * @param panel Additional information body panel for switching display
     */
    public InformationExpandAction(JPanel panel) {
      this.panelEditor = panel;
    }

    /**
     * Button click event
     *
     * @param event Event information
     */
    @Override
    public void actionPerformed(ActionEvent event) {
      JButton btn = (JButton) event.getSource();
      // Toggle the display of the additional information body panel
      if (panelEditor.isVisible()) {
        // Hide the additional information body panel
        btn.setIcon(collapse_icon);
        panelEditor.setVisible(false);
      } else {
        // Display the additional information body panel
        btn.setIcon(expand_icon);
        panelEditor.setVisible(true);
      }

      return;
    }
  }

  /**
   * Additional Information Node Panel
   *
   * @author RIKEN
   */
  private class NodePanel extends JPanel {

    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Additional information node */
    private IInformation info;
    /** Additional information display text box */
    private JEditorPane editorPane;

    /**
     * Constructor
     *
     * @param info Additional information block
     */
    public NodePanel(IInformation info) {
      this.info = info;
    }

    /**
     * Get additional information node
     *
     * @return Additional information node
     */
    public IInformation getInfo() {
      return this.info;
    }

    /**
     * Set additional information display text box
     *
     * @param editor Additional information display text box
     */
    public void setEditorPane(JEditorPane editor) {
      this.editorPane = editor;
    }

    /** Redraw the display of additional information */
    public void refresh() {
      if (info == null) return;
      if (info.getInformation() == null) return;
      if (info.getInformation().getContent() == null) return;

      String content = info.getInformation().getContent();
      content = InformationPanel.this.model.createHtmlContent(content);
      this.editorPane.setText(content);

      // redraw
      InformationPanel.this.refresh();
    }
  }

  /**
   * Mouse click event
   *
   * @param event Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {
    // Click check
    if (SwingUtilities.isLeftMouseButton(event)) {
      NodePanel panel = null;
      if (event.getSource() instanceof NodePanel) {
        panel = (NodePanel) event.getSource();
      } else if (event.getSource() instanceof JEditorPane) {
        // Parent parent panel
        Container cont = ((JEditorPane) event.getSource()).getParent().getParent();
        if (cont instanceof NodePanel) {
          panel = (NodePanel) cont;
        }
      }

      // Set the selective additional information block
      this.selectedInfo = panel;

      // Set the background color of the selection panel.
      setSelectedBackgroud(this.selectedInfo);

      // Double click
      if (event.getClickCount() == 2) {
        // Open the relevant part
        this.btnOpenFile.doClick();
      }
    }
  }

  /**
   * Mouse button down event
   *
   * @param e Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent e) {}

  /**
   * Mouse button up event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseReleased(MouseEvent e) {}

  /**
   * Mouseover event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseEntered(MouseEvent e) {}

  /**
   * Mouse out event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseExited(MouseEvent e) {}

  /**
   * Hyperlink action event
   *
   * @param event Event information
   */
  @Override
  public void hyperlinkUpdate(HyperlinkEvent event) {
    // Hyperlink click event
    if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      // Link file
      URL url = event.getURL();

      // Get the startup program (the startup program is set in the class attribute)
      javax.swing.text.Element elem = event.getSourceElement();
      String program = SwingUtils.getAttributeValue(elem, HTML.Tag.A, HTML.Attribute.CLASS);
      String comment = SwingUtils.getAttributeValue(elem, HTML.Tag.A, HTML.Attribute.COMMENT);
      String[] args = null;
      if (comment != null && !comment.isEmpty()) {
        args = new String[] {comment};
      }
      // Executing an external program
      String errMsg = SwingUtils.processOpenProgram(url.toString(), program, args);
      if (errMsg != null && !errMsg.isEmpty()) {
        // Error message
        JFrame frame = (JFrame) SwingUtilities.getWindowAncestor(this);
        JOptionPane.showMessageDialog(
            frame, errMsg, Message.getString("dialog.common.error"), JOptionPane.ERROR_MESSAGE);
      }
    }

    return;
  }

  /** Close the tab */
  @Override
  public void closeTab() {}

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  @Override
  public CodeLine getSelectedCodeLine() {
    IBlock block = getSelectedBlock();
    if (block == null) return null;
    CodeLine line = block.getStartCodeLine();
    return line;
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    if (event.getSource() == this.btnLock) {
      // Switching the display lock state
      toggleLockButton(!this.viewLock);

      // Since it is not locked, the latest additional information is displayed.
      if (!this.viewLock) {
        updateModel();
      }
    } else if (event.getSource() == this.btnEdit) {
      if (this.actionEdit == null) return;
      if (this.selectedInfo == null) return;
      if (this.selectedInfo.getInfo() == null) return;

      // Edit
      this.actionEdit.editInformation(this.selectedInfo.getInfo());
    }
  }

  /**
   * Swap the lock button toggle
   *
   * @param lock true = Locked state
   */
  private void toggleLockButton(boolean lock) {

    // Switching the display lock state
    this.viewLock = lock;

    Icon icon = null;
    if (this.viewLock) {
      // Put the display locked
      icon = ResourceUtils.getIcon("lock.gif");
    } else {
      // Release the display lock state
      icon = ResourceUtils.getIcon("unlock.gif");
    }
    this.btnLock.setIcon(icon);
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  @Override
  public IInformation getSelectedInformation() {
    if (this.selectedInfo == null) return null;
    return selectedInfo.getInfo();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    if (this.selectedInfo == null) return null;
    IInformation info = this.selectedInfo.getInfo();
    if (info instanceof IBlock) {
      IBlock block = (IBlock) info;
      return block;
    }
    return null;
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  @Override
  public void setSourceProperties(SourceProperties properties) {
    // Background color of selection panel
    this.colorSelectedPanel = properties.getBackgoundView2Color();
    // Set the background color of the selection panel.
    setSelectedBackgroud(this.selectedInfo);
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {
    if (this.selectedInfo == null) return;
    if (this.selectedInfo.getInfo() == null) return;
    if (this.selectedInfo.getInfo().getInformation() == null) return;
    IInformation info = this.selectedInfo.getInfo();
    String text = info.getInformation().getContent();

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /** Whether it can be exported */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }

  /** Set the background color of the selection panel. */
  private void setSelectedBackgroud(JPanel panel) {
    // Clear all
    if (this.contentInfo != null) {
      SwingUtils.setBackgroundChildPanel(this.contentInfo, null);
    }
    // Change the background color of the selection panel to the selection color
    if (panel != null) {
      SwingUtils.setBackgroundChildPanel(panel, this.colorSelectedPanel);
    }
  }
}
