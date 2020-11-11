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
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Calculation count model
 *
 * @author RIKEN
 */
public class OperandTableModel extends Observable {

  /** Block operation count table header list The first column is Block information. */
  // private String[] HEADER_COLUMNS = {"", "Block", "(Ld+St)/FLOP", "Load(F)", "Store(F)", "FLOP",
  // "add(F)", "mul(F)", "intrinsic(F)"};
  private String[] HEADER_COLUMNS = {
    "", "Block", "FLOP", "add(F)", "sub(F)", "mul(F)", "div(F)", "intrinsic(F)"
  };

  /** Block operation count table column size -1 = Hide */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH = {-1, 160, 80, 80, 80, 80, 80, 80};

  /** Title */
  private String title;

  /** Block operation count information list */
  private List<OperandBlock> listOperandBlock;

  /**
   * Block operation count information class
   *
   * @author RIKEN
   */
  public class OperandBlock {
    /** Block node */
    private IBlock block;
    /** Calculation count */
    private OperationCount count;

    /** Constructor */
    public OperandBlock() {}

    /**
     * Constructor
     *
     * @param block block
     */
    public OperandBlock(IBlock block) {
      this.block = block;
    }

    /**
     * Constructor
     *
     * @param block block
     * @param count Operation count
     */
    public OperandBlock(IBlock block, OperationCount count) {
      this.block = block;
      this.count = count;
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
     * Get the operation count
     *
     * @return operation count
     */
    public OperationCount getCount() {
      return count;
    }

    /**
     * Set the operation count
     *
     * @param count Operation count
     */
    public void setCount(OperationCount count) {
      this.count = count;
    }

    /**
     * Set block node
     *
     * @param block block node
     */
    public void setBlock(IBlock block) {
      this.block = block;
    }
  }

  /** Constructor */
  public OperandTableModel() {
    super();
  }

  /** Notify model changes */
  private void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get the number of block operation count information list
   *
   * @return Number of block operation count information list
   */
  public int getListOperandBlockCount() {
    if (this.listOperandBlock == null) {
      return 0;
    }
    return this.listOperandBlock.size();
  }

  /**
   * Get block calculation count information
   *
   * @param index List index
   * @return Block operation count information
   */
  public OperandBlock getOperandBlock(int index) {
    if (this.listOperandBlock == null) {
      return null;
    }
    if (this.listOperandBlock.size() <= index) return null;

    return this.listOperandBlock.get(index);
  }

  /**
   * Get block operation count table model
   *
   * @return block operation count table model
   */
  public DefaultTableModel getBlockDefaultTableModel() {
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
    return tableModel;
  }

  /**
   * Set the block operation count table column width. <br>
   * All arithmetic count tables have a fixed column width
   *
   * @param columnModel Table column model
   */
  public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
    for (int i = 0; i < columnModel.getColumnCount(); i++) {
      // Get column
      TableColumn column = columnModel.getColumn(i);
      if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
        if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
          column.setMinWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
          column.setMaxWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
          column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
          column.setResizable(false);
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
   * Add block operation count table row
   *
   * @param block block
   * @param count Block operation count
   */
  public void addOperandBlock(IBlock block, OperationCount count) {

    if (block == null) return;
    if (count == null) return;

    // Generate block operation count information
    OperandBlock info = new OperandBlock(block, count);

    if (this.listOperandBlock == null) {
      this.listOperandBlock = new ArrayList<OperandBlock>();
    }
    this.listOperandBlock.add(info);

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  public void clearOperand() {
    // Clear table model
    this.listOperandBlock = new ArrayList<OperandBlock>();
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

      // Block operation count
      if (this.listOperandBlock == null || this.listOperandBlock.size() <= 0) return;

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

  /** Create a table model */
  private DefaultTableModel createTableModel() {
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel();
    tableModel.setColumnIdentifiers(HEADER_COLUMNS);

    // Create a table model from the arithmetic count list.
    if (listOperandBlock == null) return tableModel;

    for (OperandBlock opblock : this.listOperandBlock) {
      // Create table row array
      Object[] row = new Object[HEADER_COLUMNS.length];

      // Arithmetic count block
      int col = 0;
      row[col++] = opblock.getBlock();
      ;
      // Loop name
      row[col++] = opblock.getCount().getName();
      // F
      row[col++] = opblock.getCount().getF();
      // add
      row[col++] = opblock.getCount().getAdd();
      // sub
      row[col++] = opblock.getCount().getSub();
      // mul
      row[col++] = opblock.getCount().getMul();
      // div
      row[col++] = opblock.getCount().getDiv();
      // Intrinsic
      row[col++] = opblock.getCount().getIntrinsic();

      // Add table row
      tableModel.addRow(row);
    }

    return tableModel;
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.listOperandBlock == null) return true;
    return (this.listOperandBlock.size() < 1);
  }
}
