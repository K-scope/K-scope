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
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ScopeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Variable effective area table panel class
 *
 * @author RIKEN
 */
public class ScopePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Clear button */
  private JButton btnClear;
  /** Export button */
  private JButton btnExport;
  /** Variable scope label */
  private JLabel label;
  /** Scroll pine */
  private JScrollPane scrollPane;
  /** Variable effective area table */
  private JTable tableScope;

  /** Variable scope model */
  private ScopeModel model;
  /** Table width */
  private int TABLE_COLUMN_WIDTH = 640;
  /** Constructor */
  public ScopePanel() {
    super();

    // Generate a model
    model = new ScopeModel();
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
  public ScopePanel(ANALYSIS_PANEL proparties) {
    super(proparties);

    // Generate a model
    model = new ScopeModel();
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
          // Variable scope table
          tableScope = new JStripeTable();
          tableScope.setModel(this.model.getTableModel());
          tableScope.setAutoCreateColumnsFromModel(false);
          tableScope.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
          tableScope.setColumnSelectionAllowed(false);

          // Table column model
          DefaultTableColumnModel columnModel =
              (DefaultTableColumnModel) tableScope.getColumnModel();
          TableColumn column = null;
          // Table width
          column = columnModel.getColumn(0);
          column.setPreferredWidth(TABLE_COLUMN_WIDTH);
          column.setMinWidth(100);

          // Scroll pine
          scrollPane = new JScrollPane();
          scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setViewportView(tableScope);
          scrollPane.getViewport().setBackground(Color.WHITE);

          add(scrollPane);
        }
      }

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Variable coverage model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Variable scope model
    ScopeModel observer = (ScopeModel) o;

    // Table model
    tableScope.setModel(observer.getTableModel());

    // Panel title
    this.label.setText(observer.getTitle());
  }

  /**
   * Get the variable scope model
   *
   * @return Variable characteristics list Table model
   */
  public ScopeModel getModel() {
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
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clear();
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
    if (this.tableScope == null) return;
    String text = SwingUtils.toCsvOfSeletedRows(this.tableScope);
    if (text == null) return;

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /** Whether there is information to export */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }
}
