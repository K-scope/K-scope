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
 * A class that supports the process of freeing dynamically allocated memory areas.
 *
 * @author RIKEN
 */
public class DynamicDeallocation extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -2983724954300740000L;

  private List<Variable> targets;
  private Variable error;

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param trgt Variables to be released
   */
  public DynamicDeallocation(Block parent, List<Variable> trgt) {
    super(parent);
    targets = trgt;
  }

  /**
   * Constructor.
   *
   * @param trgt Target
   */
  public DynamicDeallocation(List<Variable> trgt) {
    super();
    targets = trgt;
  }

  /** Constructor. */
  public DynamicDeallocation() {
    super();
  }

  /**
   * Set error variables.
   *
   * @param err Error variable
   */
  public void setError(Variable err) {
    error = err;
  }

  /**
   * Get the error variable.
   *
   * @return error Error variable
   */
  public Variable getError() {
    return error;
  }

  /**
   * Set the variable to be solved.
   *
   * @param trgt Target
   */
  public void setTarget(List<Variable> trgt) {
    targets = trgt;
  }
  /**
   * Get block type.
   *
   * @return BlockType.DYNAMIC_DEALLOCATION
   */
  public BlockType getBlockType() {
    return BlockType.DYNAMIC_DEALLOCATION;
  }

  /**
   * Get the variable to be released.
   *
   * @return Variable to be released
   */
  public List<Variable> getTarget() {
    return targets;
  }

  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  @Override
  public InformationBlocks createInformationBlocks() {
    InformationBlocks result = new InformationBlocks();
    result.addAll(super.createInformationBlocks());
    if (this.targets != null) {
      for (Variable variable : this.targets) {
        result.addAll(variable.createInformationBlocks());
      }
    }
    if (this.error != null) {
      result.addAll(this.error.createInformationBlocks());
    }
    return result;
  }

  /**
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
   */
  @Override
  public IInformation findInformationBlockBy(String id) {
    IInformation result = super.findInformationBlockBy(id);

    if (result == null && this.getID().equals(id)) {
      result = this;
    }

    if (result == null && this.targets != null) {
      for (Variable variable : this.targets) {
        result = variable.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
    }
    if (result == null && this.error != null) {
      result = this.error.findInformationBlockBy(id);
    }

    return result;
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
    if (!(block instanceof DynamicDeallocation)) return false;
    if (!super.equalsBlocks(block)) return false;

    if (this.targets != null && ((DynamicDeallocation) block).targets != null) {
      if (this.targets.size() == ((DynamicDeallocation) block).targets.size()) {
        for (int i = 0; i < this.targets.size(); i++) {
          Variable thisVar = this.targets.get(i);
          Variable destVar = ((DynamicDeallocation) block).targets.get(i);
          if (thisVar == destVar) {
            continue;
          } else if (thisVar == null) {
            return false;
          } else if (!thisVar.equalsVariable(destVar)) {
            return false;
          }
        }
      }
    } else if (this.targets != null || ((DynamicDeallocation) block).targets != null) {
      return false;
    }

    if (error != null && ((DynamicDeallocation) block).error != null) {
      if (!error.equalsVariable(((DynamicDeallocation) block).error)) {
        return false;
      }
    } else if (error != null || ((DynamicDeallocation) block).error != null) {
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
    {
      IInformation[] infos = super.searchInformationBlocks(block);
      if (infos != null) {
        list.addAll(Arrays.asList(infos));
      }
    }

    if (this.targets != null) {
      for (Variable variable : this.targets) {
        IInformation[] infos = variable.searchInformationBlocks(block);
        if (infos != null) {
          list.addAll(Arrays.asList(infos));
        }
      }
    }
    if (this.error != null) {
      IInformation[] infos = this.error.searchInformationBlocks(block);
      if (infos != null) {
        list.addAll(Arrays.asList(infos));
      }
    }
    if (list.size() <= 0) {
      return null;
    }
    return list.toArray(new IInformation[0]);
  }
}
