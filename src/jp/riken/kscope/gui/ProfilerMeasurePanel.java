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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableColumnModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ProfilerMeasureModel;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Measurement section information panel class
 *
 * @author RIKEN
 */
public class ProfilerMeasurePanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, ActionListener, MouseListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Clear button */
  private JButton btnClear;
  /** Delete button */
  private JButton btnRemove;
  /** Export button */
  private JButton btnExport;
  /** Open the relevant part */
  private JButton btnOpenFile;
  /** Measurement section information label */
  private JLabel label;
  /** Scroll pine */
  private JScrollPane scrollPane;
  /** Measurement section information table */
  private JTable tableMeasure;
  /** Measurement interval information table model */
  private ProfilerMeasureModel model;
  /** Action to open the relevant part */
  private ViewOpenAnalysisLineAction actionOpenAnalysis;

  /** Constructor */
  public ProfilerMeasurePanel() {
    super();

    // Generate a model
    model = new ProfilerMeasureModel();
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
  public ProfilerMeasurePanel(ANALYSIS_PANEL proparties) {
    super(proparties);

    // Generate a model
    model = new ProfilerMeasureModel();
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
                    // Display a confirmation message.
                    int option =
                        JOptionPane.showConfirmDialog(
                            null,
                            Message.getString(
                                "profilermeasurepanel.confirmdialog.clearall.message"), // Delete
                            // all
                            // registered measurement intervals. \ n Are you sure?
                            Message.getString(
                                "profilermeasurepanel.confirmdialog.clearall.title"), // Clear
                            // measurement
                            // section
                            JOptionPane.OK_CANCEL_OPTION,
                            JOptionPane.WARNING_MESSAGE);
                    if (option != JOptionPane.OK_OPTION) {
                      return;
                    }
                    // Model clear
                    clearModel();
                  }
                });
          }
          // Delete button
          {
            Icon icon = ResourceUtils.getIcon("remove.gif");
            btnRemove = new JButton(icon);
            panelButtons.add(btnRemove);
            btnRemove.setPreferredSize(buttonSize);
            btnRemove.setMinimumSize(buttonSize);
            btnRemove.setMaximumSize(buttonSize);
            btnRemove.setContentAreaFilled(false);
            btnRemove.setBorderPainted(false);
            btnRemove.addActionListener(
                new ActionListener() {
                  @Override
                  public void actionPerformed(ActionEvent e) {
                    // Display a confirmation message.
                    int option =
                        JOptionPane.showConfirmDialog(
                            null,
                            Message.getString(
                                "profilermeasurepanel.confirmdialog.clear.message"), // Delete the
                            // selected
                            // measurement
                            // interval. \
                            // n Are you
                            // sure?
                            Message.getString(
                                "profilermeasurepanel.confirmdialog.clear.title"), // Delete
                            // measurement
                            // section
                            JOptionPane.OK_CANCEL_OPTION,
                            JOptionPane.WARNING_MESSAGE);
                    if (option != JOptionPane.OK_OPTION) {
                      return;
                    }
                    // Delete measurement section
                    removeMeasureData();
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
          label.setText("Operand");
        }
      }
      {
        {
          // Measurement interval information table
          tableMeasure = new JStripeTable();
          tableMeasure.setModel(this.model.getTableModel());
          tableMeasure.setAutoCreateColumnsFromModel(false);
          tableMeasure.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
          tableMeasure.setColumnSelectionAllowed(false);

          // Table column width setting
          DefaultTableColumnModel columnModel =
              (DefaultTableColumnModel) tableMeasure.getColumnModel();
          model.setTableColumnWidth(columnModel);

          // Scroll pine
          scrollPane = new JScrollPane();
          scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setViewportView(tableMeasure);
          scrollPane.getViewport().setBackground(Color.WHITE);

          add(scrollPane);
        }
      }

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnRemove.setToolTipText(
          Message.getString("profilermeasurepanel.tooltip.delete")); // Select delete
      btnOpenFile.setToolTipText(
          Message.getString("profilermeasurepanel.tooltip.open")); // open the selection
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

      // Event listener settings
      btnOpenFile.addActionListener(this);
      tableMeasure.addMouseListener(this);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Measurement interval information model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Measurement interval information model
    ProfilerMeasureModel observer = (ProfilerMeasureModel) o;

    // Table model
    tableMeasure.setModel(observer.getTableModel());

    // Panel title
    this.label.setText(observer.getTitle());
  }

  /**
   * Get the measurement interval information model
   *
   * @return Measurement interval information model
   */
  public ProfilerMeasureModel getModel() {
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
    this.actionOpenAnalysis = (ViewOpenAnalysisLineAction) menu.getActionOpenAnalysisLine();
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearModel();
  }

  /** Delete the selected measurement interval */
  private void removeMeasureData() {
    // Selected line
    ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
    // Clear measurement interval data
    this.model.removeMeasureData(data);
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
    ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
    if (data == null) return null;
    return data.getMeasureArea();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    // Selected line
    ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
    if (data == null) return null;
    IBlock[] blocks = data.getMeasureBlocks();
    if (blocks == null) return null;
    return blocks[0];
  }

  /**
   * Get the measurement interval data of the selected row
   *
   * @return Selective measurement interval data
   */
  private ProfilerMeasureInfo.MeasureData getSelectedMeasureData() {
    int selection = this.tableMeasure.getSelectedRow();
    if (selection < 0) return null;
    // Convert to the number of rows in the table model
    int modelRow = this.tableMeasure.convertRowIndexToModel(selection);
    if (modelRow < 0) return null;
    Object cell = this.tableMeasure.getModel().getValueAt(modelRow, 0);
    if (cell == null) return null;
    if (cell instanceof ProfilerMeasureInfo.MeasureData) {
      return (ProfilerMeasureInfo.MeasureData) cell;
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

  /**
   * Mouse click event
   *
   * @param event Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {

    // Click check
    if (SwingUtilities.isLeftMouseButton(event)) {
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
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Open the corresponding line
    if (event.getSource() == this.btnOpenFile) {
      ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
      if (data == null) return;
      if (data.getMeasureBlocks() != null) {
        this.actionOpenAnalysis.viewSelectedArea(data.getMeasureBlocks());
      } else if (data.getMeasureCodeLine() != null) {
        this.actionOpenAnalysis.clearSourceBlock();
        this.actionOpenAnalysis.viewSelectedSourceBlock(data.getMeasureCodeLine());
      }
    }
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {
    if (this.tableMeasure == null) return;
    String text = SwingUtils.toCsvOfSeletedRows(this.tableMeasure);
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
