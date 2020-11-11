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
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Arithmetic count panel class
 *
 * @author RIKEN
 */
public class OperandTablePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Clear button */
  private JButton btnClear;
  /** Export button */
  private JButton btnExport;
  /** Open the relevant part */
  private JButton btnOpenFile;
  /** Computation count label */
  private JLabel label;
  /** Scroll pine */
  private JScrollPane scrollPane;
  /** Calculation count table */
  private JTable tableOperand;

  /** Arithmetic count table model */
  private OperandTableModel model;

  /** Constructor */
  public OperandTablePanel() {
    super();

    // Generate a model
    model = new OperandTableModel();
    // Set the observer.
    model.addObserver(this);

    // Initialize the GUI.
    initGUI();
  }

  /**
   * Constructor
   *
   * @param proparties Analysis Information Panel Identifier
   */
  public OperandTablePanel(ANALYSIS_PANEL proparties) {
    super(proparties);

    // Generate a model
    model = new OperandTableModel();
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
      //            setPreferredSize(new Dimension(400, 24));

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
            btnClear.setPreferredSize(buttonSize);
            btnClear.setMinimumSize(buttonSize);
            btnClear.setMaximumSize(buttonSize);
            btnClear.setContentAreaFilled(false);
            btnClear.setBorderPainted(false);
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
          // label.setText("");
        }
      }
      {
        {
          // Calculation count table
          tableOperand = new JStripeTable();
          tableOperand.setModel(this.model.getTableModel());
          tableOperand.setAutoCreateColumnsFromModel(false);
          tableOperand.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
          tableOperand.setColumnSelectionAllowed(false);

          // Table column width setting
          DefaultTableColumnModel columnModel =
              (DefaultTableColumnModel) tableOperand.getColumnModel();
          model.setTableColumnWidth(columnModel);

          // Scroll pine
          scrollPane = new JScrollPane();
          scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setViewportView(tableOperand);
          scrollPane.getViewport().setBackground(Color.WHITE);

          add(scrollPane);
        }
      }

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnOpenFile.setToolTipText(
          Message.getString("informationpanel.tooltip.openblock")); // open the selection
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Calculation count model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Arithmetic count model
    OperandTableModel observer = (OperandTableModel) o;

    // Table model
    tableOperand.setModel(observer.getTableModel());

    // Panel title
    this.label.setText(observer.getTitle());
  }

  /**
   * Get the arithmetic count table model
   *
   * @return Variable characteristics list Table model
   */
  public OperandTableModel getModel() {
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
    // Set focus listener on child components as well
    SwingUtils.addChildFocusListener(this, listener);
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
    // Open the relevant part
    this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
    this.tableOperand.addMouseListener((MouseListener) menu.getActionOpenAnalysisLine());
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearOperand();
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
    IBlock block = getSelectedBlock();
    if (block == null) return null;
    return block.getStartCodeLine();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    // Selected line
    int row = this.tableOperand.getSelectedRow();
    if (row < 0) return null;

    // The first column is block information
    DefaultTableModel tableModel = (DefaultTableModel) this.tableOperand.getModel();
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
    // Selected line
    int row = this.tableOperand.getSelectedRow();
    if (row < 0) return null;

    // The first column is block information
    DefaultTableModel tableModel = (DefaultTableModel) this.tableOperand.getModel();
    Object obj = tableModel.getValueAt(row, 0);
    if (obj == null) return null;
    if (obj instanceof IInformation) {
      return (IInformation) obj;
    }
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
    if (this.tableOperand == null) return;
    String text = SwingUtils.toCsvOfSeletedRows(this.tableOperand);
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
