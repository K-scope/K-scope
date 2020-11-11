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
import java.util.List;
import java.util.Observable;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Variable characteristic information list model
 *
 * @author RIKEN
 */
public class VariableTableModel extends Observable {

  /**
   * Table header list (15 columns). <br>
   * The first column is Variable Definition information.
   */
  private String[] HEADER_COLUMNS = {
    "",
    "type",
    "name",
    "data type",
    "access specifier",
    "parameter",
    "init value",
    "size",
    "intent",
    "optional",
    "pointer/target",
    "save",
    "common",
    "allocatable",
    Message.getString("mainmenu.window.analysis.information")
  }; // Additional information

  /** Table column size -1 = Hide */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH = {
    -1, 40, 120, 80, 120, 80, 80, 80, 80, 80, 60, 60, 80, 80, 160
  };
  /**
   * Minimum table column size. <br>
   * -1 = Hide
   */
  private int[] HEADER_COLUMNS_MINWIDTH = {
    -1, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80
  };

  /** Title */
  private String title;

  /** Procedure variable characteristic information list */
  private List<ProcedureInfo> listProcedureInfo;
  /** Select variable declaration statement */
  private VariableDefinition selectedVariable;

  /**
   * Procedure variable characteristic information class
   *
   * @author RIKEN
   */
  public class ProcedureInfo {
    private IBlock block;
    private List<VariableInfo> listInfo;

    /**
     * Constructor
     *
     * @param block block
     */
    public ProcedureInfo(IBlock block) {
      this.block = block;
      listInfo = null;
    }

    /**
     * Constructor
     *
     * @param block block
     * @param info Variable characteristic information list
     */
    public ProcedureInfo(IBlock block, List<VariableInfo> info) {
      this.block = block;
      this.listInfo = info;
    }

    /**
     * Get a block
     *
     * @return block
     */
    public IBlock getBlock() {
      return this.block;
    }

    /**
     * Get the variable characteristic information list
     *
     * @return Variable characteristic information list
     */
    public List<VariableInfo> getListInfo() {
      return this.listInfo;
    }

    /**
     * Add to variable characteristic information list
     *
     * @param info Variable characteristic information
     */
    public void addVariableInfo(VariableInfo info) {
      if (listInfo == null) {
        listInfo = new ArrayList<VariableInfo>();
      }
      this.listInfo.add(info);
    }

    /**
     * Get the table model
     *
     * @return table model
     */
    public DefaultTableModel getTableModel() {
      // Create a table model
      DefaultTableModel tableModel = getDefaultTableModel();
      if (listInfo == null) return tableModel;

      for (VariableInfo info : listInfo) {
        Object[] rows = new Object[tableModel.getColumnCount()];
        // First column is Variable Definition: Hide
        rows[0] = info.getVariable();
        String[] attrs = info.getAttributes();
        for (int i = 0; i < attrs.length; i++) {
          if (rows.length <= i + 1) break;
          rows[i + 1] = attrs[i];
        }
        tableModel.addRow(rows);
      }

      return tableModel;
    }
  }

  /**
   * Variable characteristic information class
   *
   * @author RIKEN
   */
  public class VariableInfo {
    /** Variable declaration / structure class */
    private VariableDefinition variable;
    /** Variable characteristic information */
    private String[] attributes;

    /**
     * Constructor
     *
     * @param variable Variable declaration / structure class
     * @param attributes Variable property information
     */
    public VariableInfo(VariableDefinition variable, String[] attributes) {
      this.variable = variable;
      this.attributes = attributes;
    }

    /**
     * Get variable declaration statement / structure class
     *
     * @return Variable declaration / structure class
     */
    public VariableDefinition getVariable() {
      return this.variable;
    }

    /**
     * Get variable characteristic information
     *
     * @return Variable characteristic information
     */
    public String[] getAttributes() {
      return this.attributes;
    }
  }

  /** Constructor */
  public VariableTableModel() {
    super();
  }

  /** Notify model changes */
  private void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get the number of procedure variable characteristic information list
   *
   * @return Number of procedure variable characteristic information list
   */
  public int getListProcedureInfoCount() {
    if (this.listProcedureInfo == null) {
      return 0;
    }
    return this.listProcedureInfo.size();
  }

  /**
   * Get procedure variable characteristic information
   *
   * @param index List index
   * @return Procedure variable characteristic information
   */
  public ProcedureInfo getProcedureInfo(int index) {
    if (this.listProcedureInfo == null) {
      return null;
    }
    if (this.listProcedureInfo.size() <= index) return null;

    return this.listProcedureInfo.get(index);
  }

  /**
   * Get the table model
   *
   * @return table model
   */
  public DefaultTableModel getDefaultTableModel() {
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
    //        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
    return tableModel;
  }

  /**
   * Set column width
   *
   * @param columnModel Table column model
   */
  public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
    for (int i = 0; i < columnModel.getColumnCount(); i++) {
      // Get column
      TableColumn column = columnModel.getColumn(i);
      if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
        if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
          column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
        } else {
          column.setMinWidth(0);
          column.setMaxWidth(0);
          column.setPreferredWidth(0);
          column.setResizable(false);
        }
      }
      if (HEADER_COLUMNS_MINWIDTH.length >= i) {
        if (HEADER_COLUMNS_MINWIDTH[i] >= 0) {
          column.setMinWidth(HEADER_COLUMNS_MINWIDTH[i]);
        }
      }
    }
  }

  /**
   * Get the header column list.
   *
   * @return Header column list
   */
  public String[] getHeaderColumns() {
    return HEADER_COLUMNS;
  }

  /**
   * Get the header column width list.
   *
   * @return Header column width
   */
  public int[] getHeaderColumnsWidth() {
    return HEADER_COLUMNS_PREFERREDWIDTH;
  }

  /**
   * Add a table row
   *
   * @param block Procedure block
   * @param variable Variable declaration class
   * @param infos Variable characteristic information
   */
  public void addVariableInfo(IBlock block, VariableDefinition variable, String[] infos) {

    if (infos == null) return;

    // Generate variable characteristic information class
    VariableInfo info = new VariableInfo(variable, infos);

    // Search procedure variable characteristic information
    ProcedureInfo procInfo = getProcedureInfo(block);
    if (procInfo == null) {
      procInfo = new ProcedureInfo(block);
      procInfo.addVariableInfo(info);
      if (this.listProcedureInfo == null) {
        this.listProcedureInfo = new ArrayList<ProcedureInfo>();
      }
      this.listProcedureInfo.add(procInfo);
    } else {
      procInfo.addVariableInfo(info);
    }

    // Notify model changes
    notifyModel();
  }

  /**
   * Search procedure variable characteristic information by key string
   *
   * @param block Procedure variable characteristic information block
   * @return Procedure variable characteristic information
   */
  private ProcedureInfo getProcedureInfo(IBlock block) {
    if (block == null) return null;
    if (this.listProcedureInfo == null) return null;

    for (ProcedureInfo info : this.listProcedureInfo) {
      IBlock infoblock = info.getBlock();
      if (block == infoblock) {
        return info;
      }
    }

    return null;
  }

  /** Clear the table model. */
  public void clearVariable() {
    // Clear table model
    if (this.listProcedureInfo != null) {
      this.listProcedureInfo = new ArrayList<ProcedureInfo>();
    }
    // Clear title
    this.title = null;

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
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      if (this.listProcedureInfo != null) {
        for (ProcedureInfo info : this.listProcedureInfo) {
          IBlock block = info.getBlock();
          if (block != null) {
            pw.println(block.toString());
          }
          // Output the table
          String buf = SwingUtils.toCsv(info.getTableModel());
          pw.print(buf);
          pw.println();
        }
      }
      pw.close();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Set the selection variable declaration statement
   *
   * @param variable Select variable declaration statement
   */
  public void setSelectedVariable(VariableDefinition variable) {
    this.selectedVariable = variable;
  }

  /**
   * Get the selection variable declaration statement
   *
   * @return Selective variable declaration statement
   */
  public VariableDefinition getSelectedVariable() {
    return this.selectedVariable;
  }

  /**
   * Returns whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.listProcedureInfo == null) return true;
    return (this.listProcedureInfo.size() < 1);
  }
}
