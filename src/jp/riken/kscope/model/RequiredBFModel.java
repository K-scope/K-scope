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

package jp.riken.kscope.model;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.Observable;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.properties.RequiredBFProperties.BF_CALC_TYPE;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Request Byte / FLOP table model
 *
 * @author RIKEN
 */
public class RequiredBFModel extends Observable {

  /** Title */
  private String title;
  /** Request Byte / FLOP calculation result list: Group by addition. */
  private List<List<RequiredBFResult>> listResults;
  /** Calculation unit */
  private BF_CALC_TYPE unitType;
  /** Structural tree model. */
  private LanguageTreeModel modelLanguageTree = null;

  /** Request Byte / FLOP table header list: 29 columns The first column is Block information. */
  private String[] HEADER_COLUMNS = {
    "",
    "Block",
    "Load(B)",
    "Store(B)",
    "FLOP",
    "Required B/F",
    "Throughput(GB/s)",
    "Effective B/F",
    "Peak Ratio(%)",
    "Memory Variable",
    "L1 Variable",
    "L2 Variable",
    "Register Variable",
    "Custom Variable",
    "add(F)",
    "sub(F)",
    "mul(F)",
    "div(F)",
    "intrinsic(F)",
    "Performance(GFLOPS)",
    "Store Mode",
    "Memory Band Width(GB/s)",
    "L1 Band Width(GB/s)",
    "L2 Band Width(GB/s)",
    "Register Band Width(GB/s)",
    "Custom Band Width(GB/s)",
    "Memory Coef",
    "L1 Coef",
    "L2 Coef",
    "Register Coef",
    "Custom Coef",
    Message.getString("mainmenu.window.analysis.information") // Additional information
  };
  /** Block operation count table column size: 31 columns -1 = Hide */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH = {
    -1, 240, 100, 100, 100, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120,
    160, 120, 160, 160, 160, 160, 160, 100, 100, 100, 100, 100, 240
  };

  /** Constructor */
  public RequiredBFModel() {
    super();
  }

  /** Notify model changes */
  public void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get the number of groups in the request Byte / FLOP calculation result list
   *
   * @return Request Byte / FLOP calculation result list number
   */
  public int getListResultGroupCount() {
    if (this.listResults == null) {
      return 0;
    }
    return this.listResults.size();
  }

  /**
   * Get the request Byte / FLOP calculation result
   *
   * @param groupId Group index
   * @param index List index
   * @return Request Byte / FLOP calculation result
   */
  public RequiredBFResult getRequiredByteFlopResult(int groupId, int index) {
    if (this.listResults == null) {
      return null;
    }
    if (this.listResults.size() <= groupId) return null;
    List<RequiredBFResult> group = this.listResults.get(groupId);
    if (group == null || group.size() <= index) return null;
    return group.get(index);
  }

  /**
   * Get the request Byte / FLOP calculation result that matches the block from the request Byte /
   * FLOP calculation result group.
   *
   * @param group Group list
   * @param block block
   * @return Request Byte / FLOP calculation result
   */
  public RequiredBFResult getRequiredByteFlopResult(List<RequiredBFResult> group, Object block) {
    if (group == null || block == null) return null;
    for (RequiredBFResult result : group) {
      if (result.getBlock() == block) {
        return result;
      }
    }
    return null;
  }

  /**
   * Get the request Byte / FLOP calculation result table model
   *
   * @return Request Byte / FLOP calculation result table model
   */
  public DefaultTableModel getBlockDefaultTableModel() {
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
    return tableModel;
  }

  /**
   * Set the request Byte / FLOP calculation result table column width. <br>
   * Request Byte / FLOP calculation result table has a fixed column width
   *
   * @param columnModel Table column model
   */
  public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
    for (int i = 0; i < columnModel.getColumnCount(); i++) {
      // Get column
      TableColumn column = columnModel.getColumn(i);
      if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
        if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
          // column.setMinWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
          // column.setMaxWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
          column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
          column.setResizable(true);
        } else {
          column.setMinWidth(0);
          column.setMaxWidth(0);
          column.setPreferredWidth(0);
          column.setResizable(false);
        }
      }
    }
  }

  /**
   * Add request Byte / FLOP calculation result table row
   *
   * @param results Request Byte / FLOP calculation result list
   */
  public void addRequiredByteFlopResults(RequiredBFResult[] results) {
    if (results == null) return;
    if (this.listResults == null) {
      this.listResults = new ArrayList<List<RequiredBFResult>>();
    }
    List<RequiredBFResult> list = new ArrayList<RequiredBFResult>();
    list.addAll(Arrays.asList(results));
    this.listResults.add(list);

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  public void clearModel() {
    // Clear table model
    this.listResults = new ArrayList<List<RequiredBFResult>>();
    // Clear title
    title = null;

    // Notify model changes
    notifyModel();
  }

  /**
   * Get the title
   *
   * @return title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Set the title
   *
   * @param title Title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Output table information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {

    try {
      // Request Byte / FLOP calculation result table
      if (this.listResults == null || this.listResults.size() <= 0) return;

      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      // Output the table
      String buf = SwingUtils.toCsv(getTableModel());
      pw.print(buf);

      pw.close();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Get the table model
   *
   * @return table model
   */
  public DefaultTableModel getTableModel() {
    return createTableModel();
  }

  /**
   * Create a table model
   *
   * @return table model
   */
  private DefaultTableModel createTableModel() {
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel();
    tableModel.setColumnIdentifiers(HEADER_COLUMNS);
    // Create a table model from the arithmetic count list.
    if (this.listResults == null) return tableModel;

    for (List<RequiredBFResult> group : this.listResults) {
      // Create a block list
      List<Object> listObj = new ArrayList<Object>();
      for (RequiredBFResult result : group) {
        listObj.add(result.getBlock());
      }
      // create a tree node
      DefaultMutableTreeNode root =
          SwingUtils.createTreeNode(this.modelLanguageTree.getRootNode(), listObj);

      // List tree nodes in the forward direction
      Enumeration<?> depth = root.preorderEnumeration();
      while (depth.hasMoreElements()) {
        DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();
        if (treeNode == null || treeNode.getUserObject() == null) {
          continue;
        }
        RequiredBFResult result = getRequiredByteFlopResult(group, treeNode.getUserObject());
        if (result == null) continue;

        int depthCount = treeNode.getLevel() - 1;
        if (depthCount < 0) continue;
        String depthText = StringUtils.repeat(" ", depthCount * 4);

        // Create table row array
        Object[] row = new Object[HEADER_COLUMNS.length];
        int col = 0;
        // Request Byte / FLOP calculation result
        row[col++] = result;
        // Block
        row[col++] = depthText + result.getBlock().toString();
        // Load(B)
        row[col++] = result.getLoad();
        // Store(B)
        row[col++] = result.getStore();
        // FLOP
        row[col++] = result.getOperand();
        // Required B/F
        float required = 0.0F;
        if (unitType == BF_CALC_TYPE.FLOP_BYTE) {
          required = result.getRequiredFB();
        } else {
          required = result.getRequiredBF();
        }
        row[col++] = Float.valueOf(String.format("%.2f", required));
        // Throughput(GB/s)
        float throughput = result.getThroughput();
        row[col++] = Float.valueOf(String.format("%.2f", throughput));
        // Effective B/F
        float effective = 0.0F;
        if (unitType == BF_CALC_TYPE.FLOP_BYTE) {
          effective = result.getEffectiveFB();
        } else {
          effective = result.getEffectiveBF();
        }
        row[col++] = Float.valueOf(String.format("%.2f", effective));
        // Peak Ratio(%)
        float peak = 0.0F;
        if (unitType == BF_CALC_TYPE.FLOP_BYTE) {
          peak = result.getPeakFB();
        } else {
          peak = result.getPeakBF();
        }
        peak *= 100.0;
        row[col++] = Float.valueOf(String.format("%.2f", peak));
        // Memory Variable
        row[col++] = result.getMemoryCount();
        // L1 Variable
        row[col++] = result.getL1Count();
        // L2 Variable
        row[col++] = result.getL2Count();
        // Register Variable
        row[col++] = result.getRegisterCount();
        // Custom Variable
        row[col++] = result.getCustomCount();
        // add(F)
        row[col++] = result.getAddCount();
        // sub(F)
        row[col++] = result.getSubCount();
        // mul(F)
        row[col++] = result.getMulCount();
        // div(F)
        row[col++] = result.getDivCount();
        // intrinsic(F)
        row[col++] = result.getIntrinsicCount();
        // Performance(GFLOPS)
        row[col++] = result.getPerformance();
        // Store Mode
        row[col++] = result.getStoreMode();
        // Memory MBW(GB/s)
        row[col++] = result.getMemoryMBW();
        // L1 MBW(GB/s)
        row[col++] = result.getL1MBW();
        // L2 MBW(GB/s)
        row[col++] = result.getL2MBW();
        // Register MBW(GB/s)
        row[col++] = result.getRegisterMBW();
        // Custom MBW(GB/s)
        row[col++] = result.getCustomMBW();
        // Memory Coef
        row[col++] = result.getMemoryCoef();
        // L1 Coef
        row[col++] = result.getL1Coef();
        // L2 Coef
        row[col++] = result.getL2Coef();
        // Register Coef
        row[col++] = result.getRegisterCoef();
        // Custom Coef
        row[col++] = result.getCustomCoef();
        // Additional information
        if (result.getBlock() != null && result.getBlock() instanceof IInformation) {
          TextInfo info = ((IInformation) result.getBlock()).getInformation();
          if (info != null && !StringUtils.isNullOrEmpty(info.getContent())) {
            // row[col++] = info.getAbstract();
            row[col++] = info;
          } else {
            row[col++] = "no info";
          }
        }
        // Add table row
        tableModel.addRow(row);
      }
    }

    return tableModel;
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.listResults == null) return true;
    return (this.listResults.size() < 1);
  }

  /**
   * Get the calculation unit.
   *
   * @return Calculation unit
   */
  public BF_CALC_TYPE getUnitType() {
    return unitType;
  }
  /**
   * Set the calculation unit.
   *
   * @param type Calculation unit
   */
  public void setUnitType(BF_CALC_TYPE type) {
    this.unitType = type;

    // Change the header display unit.
    for (int i = 0; i < this.HEADER_COLUMNS.length; i++) {
      if (type == BF_CALC_TYPE.FLOP_BYTE) {
        this.HEADER_COLUMNS[i] = this.HEADER_COLUMNS[i].replaceAll("B/F", "F/B");
      } else {
        this.HEADER_COLUMNS[i] = this.HEADER_COLUMNS[i].replaceAll("F/B", "B/F");
      }
    }
  }

  /**
   * Get the structure tree model.
   *
   * @return Structural tree model.
   */
  public LanguageTreeModel getModelLanguageTree() {
    return this.modelLanguageTree;
  }

  /**
   * Set the structure tree model.
   *
   * @param model Structural tree model.
   */
  public void setModelLanguageTree(LanguageTreeModel model) {
    this.modelLanguageTree = model;
  }
}
