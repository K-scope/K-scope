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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Error panel class
 *
 * @author RIKEN
 */
public class ErrorInfoPanel extends AnalisysPanelBase implements Observer, IAnalisysComponent {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Error table */
  private JTable tableError;
  /** Clear button */
  private JButton btnClear;
  /** Export button */
  private JButton btnExport;
  /** Open file */
  private JButton btnOpenFile;
  /** Error label */
  private JLabel label;

  /** Error table model */
  private ErrorInfoModel model;

  /** Constructor */
  public ErrorInfoPanel() {
    super();

    // Initialize.
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   */
  public ErrorInfoPanel(ANALYSIS_PANEL panel) {
    super(panel);

    // Initialize.
    initialize();
  }

  /** Initialize. */
  private void initialize() {

    // Generate a model
    model = new ErrorInfoModel();
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
                    // Model clear
                    clearModel();
                  }
                });
          }
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
          // Error table
          tableError = new JStripeTable();
          tableError.setModel(model.getTableModel());

          tableError.setAutoCreateColumnsFromModel(false);
          tableError.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
          tableError.setColumnSelectionAllowed(false);

          // Table column model
          DefaultTableColumnModel columnModel =
              (DefaultTableColumnModel) tableError.getColumnModel();
          TableColumn column = null;

          // First column is code line information: hidden
          column = columnModel.getColumn(0);
          column.setPreferredWidth(0);
          column.setMinWidth(0);
          column.setMaxWidth(0);
          column.setResizable(false);

          // Error message string
          column = columnModel.getColumn(1);
          column.setPreferredWidth(360);
          column.setMinWidth(160);

          // file name
          column = columnModel.getColumn(2);
          column.setPreferredWidth(240);
          column.setMinWidth(120);

          // line number
          column = columnModel.getColumn(3);
          column.setPreferredWidth(80);
          column.setMinWidth(80);

          // Scroll pine
          JScrollPane scrollTable = new JScrollPane(tableError);
          scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollTable.getViewport().setBackground(Color.WHITE);

          add(scrollTable);
        }
      }

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnOpenFile.setToolTipText(
          Message.getString("errorInfopanel.tooltip.open")); // Open the error part
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Error model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Table model
    ErrorInfoModel observer = (ErrorInfoModel) o;
    tableError.setModel(observer.getTableModel());

    // Panel title
    this.label.setText(observer.getTitle());

    // Error occurrence status
    if (observer.getErrorListCount() > 0) {
      // Since an error has occurred, activate your own tab
      AnalysisView tab = (AnalysisView) this.getParentComponent();
      tab.setSelectedPanel(this.getEnumPanel());
    }
  }

  /**
   * Get the error table model
   *
   * @return error table model
   */
  public ErrorInfoModel getModel() {
    return model;
  }

  /**
   * Set tab focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    this.addFocusListener(listener);
    // Set focus listener on child components as well
    if (this.tableError != null) {
      this.tableError.addFocusListener(listener);
      this.btnClear.addFocusListener(listener);
      this.btnExport.addFocusListener(listener);
      this.btnOpenFile.addFocusListener(listener);
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
    // Analysis information export action
    this.btnExport.addActionListener(menu.getActionExportAnalysis());
    // Open the error part
    this.btnOpenFile.addActionListener((ActionListener) menu.getActionErrorOpenFile());
    this.tableError.addMouseListener((MouseListener) menu.getActionErrorOpenFile());
  }

  /**
   * Get the error message for the selected line. <br>
   * An error message is set in the second column of the table model.
   *
   * @return code information
   */
  public String getSelectedErrorMessage() {

    // Selected line
    int row = this.tableError.getSelectedRow();
    if (row < 0) return null;

    // The second column is the error message
    DefaultTableModel tableModel = (DefaultTableModel) this.tableError.getModel();
    Object obj = tableModel.getValueAt(row, 1);
    if (obj == null) return null;

    return obj.toString();
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearErrorList();
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
    // Selected line
    int row = this.tableError.getSelectedRow();
    if (row < 0) return null;

    // The first column is CodeLine information
    DefaultTableModel tableModel = (DefaultTableModel) this.tableError.getModel();
    Object obj = tableModel.getValueAt(row, 0);
    if (obj == null) return null;
    if (obj instanceof CodeLine) {
      return (CodeLine) obj;
    }

    return null;
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    // Selected line
    int row = this.tableError.getSelectedRow();
    if (row < 0) return null;

    // The first column is CodeLine information
    DefaultTableModel tableModel = (DefaultTableModel) this.tableError.getModel();
    Object obj = tableModel.getValueAt(row, 0);
    if (obj == null) return null;
    if (obj instanceof IBlock) {
      return (IBlock) obj;
    }

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
    if (this.tableError == null) return;
    String text = SwingUtils.toCsvOfSeletedRows(this.tableError);
    if (text == null) return;

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /** Whether it can be exported */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }
}
