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
package jp.riken.kscope.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;

/**
 * Block list class
 *
 * @author RIKEN
 */
public class BlockList implements IBlock {
  /** Block list */
  private List<IBlock> blocks;

  /** Constructor */
  public BlockList() {
    this.blocks = new ArrayList<IBlock>();
  }

  /**
   * Constructor
   *
   * @param list Block list
   */
  public BlockList(IBlock[] list) {
    if (list != null && list.length > 0) {
      this.blocks = new ArrayList<IBlock>();
      this.blocks.addAll(Arrays.asList(list));
    }
  }

  /**
   * Get the block list.
   *
   * @return block list
   */
  public List<IBlock> getBlocks() {
    return this.blocks;
  }

  /**
   * Set the block list.
   *
   * @param list Block list
   */
  public void setBlocks(List<IBlock> list) {
    this.blocks = list;
  }

  /**
   * Get the number of block lists.
   *
   * @return Number of block lists
   */
  public int getBlockCount() {
    if (this.blocks == null) return 0;
    return this.blocks.size();
  }

  /**
   * Add a block.
   *
   * @param block Additional block
   */
  public void addBlock(IBlock block) {
    if (this.blocks == null) {
      this.blocks = new ArrayList<IBlock>();
    }
    this.blocks.add(block);
  }

  /**
   * Get the start code line information. Pass the start code line information of the first block in
   * the block list.
   */
  @Override
  public CodeLine getStartCodeLine() {
    if (this.blocks == null || this.blocks.size() <= 0) return null;
    return this.blocks.get(0).getStartCodeLine();
  }

  /**
   * Get exit code line information. Pass the exit code line information of the last block in the
   * block list.
   */
  @Override
  public CodeLine getEndCodeLine() {
    if (this.blocks == null || this.blocks.size() <= 0) return null;
    int index = this.blocks.size() - 1;
    return this.blocks.get(index).getEndCodeLine();
  }

  /** The block type of the block list class is UNKNOWN. */
  @Override
  public BlockType getBlockType() {
    return BlockType.UNKNOWN;
  }

  @Override
  public IBlock getMotherBlock() {
    return null;
  }

  /** Get the variable list from the block list. */
  @Override
  public Set<Variable> getAllVariables() {
    if (this.blocks == null || this.blocks.size() <= 0) return null;
    Set<Variable> list = new HashSet<Variable>();
    for (IBlock block : this.blocks) {
      if (block == null) continue;
      Set<Variable> vars = block.getAllVariables();
      if (vars != null) {
        list.addAll(vars);
      }
    }
    if (list.size() <= 0) return null;
    return list;
  }

  /**
   * Get the string representation of the block list. Get the string representation of the start-end
   * of the block list.
   */
  @Override
  public String toString() {
    if (this.blocks == null || this.blocks.size() <= 0) return null;
    int last = this.blocks.size() - 1;
    String start = this.blocks.get(0).toString();
    String end = this.blocks.get(last).toString();
    if (this.blocks.size() == 1) {
      return start;
    }
    // [start] - [end]
    StringBuffer buf = new StringBuffer();
    buf.append("[");
    buf.append(start);
    buf.append("]");
    buf.append(" - ");
    buf.append("[");
    buf.append(end);
    buf.append("]");
    return buf.toString();
  }

  /**
   * Check if the block is on the list.
   *
   * @param block block
   * @return true = registered
   */
  public boolean contains(IBlock block) {
    if (this.blocks == null || this.blocks.size() <= 0) return false;
    return this.blocks.contains(block);
  }
}
