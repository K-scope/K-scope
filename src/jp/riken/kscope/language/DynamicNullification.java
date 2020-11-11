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

package jp.riken.kscope.language;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import jp.riken.kscope.information.InformationBlocks;

/**
 * A class that supports the process of dynamically discarding pointer references (setting the
 * pointer variable to null).
 *
 * @author RIKEN
 */
public class DynamicNullification extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -2195173081670708420L;

  private DynamicDeallocation core;

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param trgt Pointer variable to be discarded
   */
  public DynamicNullification(Block parent, List<Variable> trgt) {
    core = new DynamicDeallocation(parent, trgt);
  }

  /** Constructor. */
  public DynamicNullification() {
    super();
  }

  /**
   * Set the target.
   *
   * @param trgt Target
   */
  public void setTarget(List<Variable> trgt) {
    core = new DynamicDeallocation(trgt);
  }
  /**
   * Get block type.
   *
   * @return BlockType.DYNAMIC_NULLIFICATION
   */
  @Override
  public BlockType getBlockType() {
    return BlockType.DYNAMIC_NULLIFICATION;
  }

  /**
   * Get the pointer variable to be discarded.
   *
   * @return pointer variable to be discarded
   */
  public List<Variable> getTarget() {
    return core.getTarget();
  }

  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  @Override
  public InformationBlocks createInformationBlocks() {
    return core.createInformationBlocks();
  }

  /**
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
   */
  @Override
  public IInformation findInformationBlockBy(String id) {
    return core.findInformationBlockBy(id);
  }

  /**
   * Check if they are the same block.
   *
   * @param block block
   * @return true = match
   */
  @Override
  public boolean equalsBlocks(Block block) {
    if (block == null) return false;
    if (!(block instanceof DynamicNullification)) return false;
    if (!super.equalsBlocks(block)) return false;
    if (!core.equalsBlocks(((DynamicNullification) block).core)) {
      return false;
    }
    return true;
  }

  /**
   * Search for the same block
   *
   * @param block IInformation block
   * @return Same block
   */
  @Override
  public IInformation[] searchInformationBlocks(IInformation block) {
    List<IInformation> list = new ArrayList<IInformation>();
    IInformation[] infos = this.core.searchInformationBlocks(block);
    if (infos != null) {
      list.addAll(Arrays.asList(infos));
    }
    if (list.size() <= 0) {
      return null;
    }
    return list.toArray(new IInformation[0]);
  }
}
