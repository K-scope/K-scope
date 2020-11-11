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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import jp.riken.kscope.language.IInformation;

/**
 * Information block collection class
 *
 * @author RIKEN
 */
public class InformationBlocks extends ArrayList<InformationBlock> {

  /** Serial number */
  private static final long serialVersionUID = 4740835139890184660L;

  /**
   * Add an element. However, if the element contents are duplicated, they are not added.
   *
   * @param e Elements to be added
   * @return true: Successful addition. false: Failed to add
   */
  @Override
  public boolean add(InformationBlock e) {
    boolean result = true;
    if (e == null) {
      return false;
    }
    if (!this.contains(e.getInformation(), e.getStartBlock(), e.getEndBlock())) {
      result = super.add(e);
    }
    return result;
  }

  /**
   * Add elements. However, elements with duplicate contents are not added.
   *
   * @param c Elements to be added
   * @return true: Successful addition. false: Failed to add
   */
  @Override
  public boolean addAll(Collection<? extends InformationBlock> c) {
    boolean result = true;
    if (c == null) {
      return false;
    }
    for (InformationBlock e : c) {
      if (!this.add(e)) {
        result = false;
      }
    }
    return result;
  }

  /**
   * Whether the target information block is included.
   *
   * @param info Information
   * @param startBlock Start block
   * @param endBlock End block
   * @return true: The target information block is included. false: The target information block is
   *     not included.
   */
  public boolean contains(InformationBase info, IInformation startBlock, IInformation endBlock) {
    boolean result = false;
    InformationBlock infoBlock = this.findObjectBy(startBlock, endBlock);
    if (infoBlock != null) {
      if (infoBlock.getInformation() == info) {
        result = true;
      }
    }
    return result;
  }

  /**
   * Search for the target information block.
   *
   * @param startBlock Start block
   * @param endBlock End block
   * @return The information block found. Returns null if not found
   */
  public InformationBlock findObjectBy(IInformation startBlock, IInformation endBlock) {
    InformationBlock result = null;

    for (InformationBlock block : this) {
      if (block.getStartBlock() == startBlock && block.getEndBlock() == endBlock) {
        result = block;
        break;
      }
    }

    return result;
  }

  /**
   * Returns a list of Information Blocks starting with the specified block.
   *
   * @param start Start block
   * @return A list of additional information areas. If not, returns an empty list.
   */
  public List<InformationBlock> getStartWith(IInformation start) {
    List<InformationBlock> blocks = new ArrayList<InformationBlock>();
    for (InformationBlock bk : this) {
      if (bk.getStartBlock().equals(start)) {
        blocks.add(bk);
      }
    }
    return blocks;
  }

  /**
   * Delete the target information block.
   *
   * @param startBlock Start block
   * @param endBlock End block
   * @return true: Deleted. false: Delete failed
   */
  public boolean remove(IInformation startBlock, IInformation endBlock) {
    boolean result = true;
    InformationBlock infoBlock = this.findObjectBy(endBlock, endBlock);
    if (infoBlock != null) {
      result = this.remove(infoBlock);
    }
    return result;
  }
}
