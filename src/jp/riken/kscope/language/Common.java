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
import java.util.Set;
import jp.riken.kscope.information.InformationBlocks;

/**
 * A class that represents variables that share the same storage area in different program units.
 * Represents a Common statement in Fortran.
 *
 * @author RIKEN
 */
public class Common extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -1820258032815507624L;
  /** Common block name */
  private String name = "NO_NAME";
  /** Scalar variables, arrays, records, structs */
  private List<Variable> variables = new ArrayList<Variable>();

  /** Constructor. */
  public Common() {
    super();
  }

  @Override
  public String toString() {
    StringBuilder st = new StringBuilder("common /" + this.name + "/");
    for (Variable var : this.variables) {
      st.append(" " + var.getName() + ",");
    }
    st.deleteCharAt(st.length() - 1);
    return st.toString();
  }
  /** Get block type. @ return BlockType.COMMON */
  public BlockType getBlockType() {
    return BlockType.COMMON;
  }

  /**
   * Set the COMMON name.
   *
   * @param nm COMMON name
   */
  public void setName(String nm) {
    this.name = nm;
  }

  /**
   * Returns the COMMON name.
   *
   * @return COMMON name
   */
  public String getName() {
    return this.name;
  }

  /**
   * Set a list of variables
   *
   * @param vars List of variables
   */
  public void setVariables(List<Variable> vars) {
    this.variables = vars;
  }

  /**
   * Get a list of variables.
   *
   * @return variables List of variables. If not, returns an empty list.
   */
  public List<Variable> getVariables() {
    return variables;
  }
  /**
   * COMMON Returns true if the list contains the specified variable name.
   *
   * @param nm variable name
   * @return True if included
   */
  public boolean contains(String nm) {
    for (Variable var : this.variables) {
      if (var.getName().equalsIgnoreCase(nm)) {
        return true;
      }
    }
    return false;
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
    if (this.variables != null) {
      for (Variable variable : this.variables) {
        result.addAll(variable.createInformationBlocks());
      }
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

    if (result == null && this.variables != null) {
      for (Variable variable : this.variables) {
        result = variable.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
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
    if (!(block instanceof Common)) return false;
    if (!super.equalsBlocks(block)) return false;

    if (this.variables == ((Common) block).getVariables()) {
      return true;
    } else if (this.variables == null) {
      return false;
    } else if (this.variables.size() == ((Common) block).getVariables().size()) {
      for (int i = 0; i < this.variables.size(); i++) {
        Variable thisVar = this.variables.get(i);
        Variable destVar = ((Common) block).getVariables().get(i);
        if (thisVar == destVar) {
          continue;
        } else if (thisVar == null) {
          return false;
        } else if (!thisVar.equalsVariable(destVar)) {
          return false;
        }
      }
      return true;
    }

    return false;
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
    if (this.variables != null) {
      for (Variable variable : this.variables) {
        IInformation[] infos = variable.searchInformationBlocks(block);
        if (infos != null) {
          list.addAll(Arrays.asList(infos));
        }
      }
    }
    if (list.size() <= 0) {
      return null;
    }

    return list.toArray(new IInformation[0]);
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    return null;
  }
}
