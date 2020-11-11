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

package jp.riken.kscope.information;

import java.io.Serializable;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Variable;

/**
 * Information block class. <br>
 *
 * @author RIKEN
 */
public class InformationBlock implements IInformation, IBlock, Serializable {
  /** Serial number */
  private static final long serialVersionUID = -6653837061369941499L;

  private InformationBase information = null;
  private IInformation startBlock = null;
  private IInformation endBlock = null;

  /**
   * Constructor.
   *
   * @param info Information to set
   * @param start Start position to set
   * @param end End position to set
   */
  public InformationBlock(InformationBase info, IInformation start, IInformation end) {
    this.information = info;
    this.startBlock = start;
    this.endBlock = end;
  }

  /**
   * Get the start position.
   *
   * @return start position
   */
  public IInformation getStartBlock() {
    return startBlock;
  }

  /**
   * Set the start position.
   *
   * @param start Start position to set
   */
  public void setStartBlock(IInformation start) {
    this.startBlock = start;
  }

  /**
   * Get the end position.
   *
   * @return end position
   */
  public IInformation getEndBlock() {
    return endBlock;
  }

  /**
   * Set the end position.
   *
   * @param end End position to set
   */
  public void setEndBlock(IInformation end) {
    this.endBlock = end;
  }

  /**
   * Get the namespace (module name.routine name).
   *
   * @return namespace (module name.routine name)
   */
  public String getNamespace() {
    if (this.startBlock == null || this.endBlock == null) {
      return "";
    }
    return this.startBlock.getNamespace();
  }

  @Override
  public void setInformation(TextInfo info) {
    this.information = info;
  }

  @Override
  public TextInfo getInformation() {
    // TextInfo is always the current design
    if (this.information instanceof TextInfo) {
      return (TextInfo) this.information;
    }
    return null;
  }

  @Override
  public int getStartPos() {
    return this.startBlock.getStartPos();
  }

  @Override
  public void setStartPos(int pos) {
    this.startBlock.setStartPos(pos);
  }

  @Override
  public int getEndPos() {
    return this.startBlock.getEndPos();
  }

  @Override
  public void setEndPos(int pos) {
    this.startBlock.setEndPos(pos);
  }

  @Override
  public void clearInformation() {
    this.information.setContent("");
  }

  // No ID required
  @Override
  public String getID() {
    return null;
  }

  @Override
  public String toString() {
    if (this.startBlock == this.endBlock) {
      return this.startBlock.toString();
    } else {
      return "[" + this.startBlock.toString() + "]  -  [" + this.endBlock.toString() + "]";
    }
  }

  @Override
  public CodeLine getStartCodeLine() {
    if (this.startBlock instanceof IBlock) {
      IBlock blk = (IBlock) this.startBlock;
      return blk.getStartCodeLine();
    }
    return null;
  }

  @Override
  public CodeLine getEndCodeLine() {
    /**** Provisional code at 2012/03/21 by @hira ****/
    if (this.endBlock != null && this.endBlock instanceof IBlock) {
      IBlock blk = (IBlock) this.endBlock;
      return blk.getEndCodeLine();
    }
    /**** Provisional code at 2012/03/21 by @hira ****/

    if (this.startBlock instanceof IBlock) {
      IBlock blk = (IBlock) this.startBlock;
      return blk.getEndCodeLine();
    }
    return null;
  }

  @Override
  public BlockType getBlockType() {
    if (this.startBlock instanceof IBlock) {
      IBlock blk = (IBlock) this.startBlock;
      return blk.getBlockType();
    }
    return null;
  }

  @Override
  public IBlock getMotherBlock() {
    if (this.startBlock instanceof IBlock) {
      IBlock blk = (IBlock) this.startBlock;
      return blk.getMotherBlock();
    }
    return null;
  }

  /**
   * Get the structure ID. Returns null as no structure ID is needed.
   *
   * @return Structure ID
   */
  @Override
  public String getLayoutID() {
    return null;
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    return null;
  }
}
