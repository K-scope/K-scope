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
 * A class that sets an initial value for a variable. Represents a Data statement in Fortran.
 *
 * @author RIKEN
 */
public class Data extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -3729371399836405032L;

  private List<Variable> variables = new ArrayList<Variable>();
  private List<Expression> values = new ArrayList<Expression>();

  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public Data(Block parent) {
    super(parent);
  }

  /** Constructor. */
  public Data() {
    super();
  }

  /** Get block type. @ return BlockType.DATA */
  public BlockType getBlockType() {
    return BlockType.DATA;
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
   * @return variables List of variables
   */
  public List<Variable> getVariables() {
    return variables;
  }

  /**
   * Set a list of values
   *
   * @param vals List of values
   */
  public void setValues(List<Expression> vals) {
    this.values = vals;
  }

  /**
   * Get a list of values.
   *
   * @return values List of values
   */
  public List<Expression> getValues() {
    return values;
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
    if (this.values != null) {
      for (Expression value : this.values) {
        result.addAll(value.createInformationBlocks());
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
    if (result == null && this.values != null) {
      for (Expression value : this.values) {
        result = value.findInformationBlockBy(id);
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
    if (!(block instanceof Data)) return false;
    if (!super.equalsBlocks(block)) return false;

    if (this.variables != null && ((Data) block).variables != null) {
      if (this.variables.size() == ((Data) block).variables.size()) {
        for (int i = 0; i < this.variables.size(); i++) {
          Variable thisVar = this.variables.get(i);
          Variable destVar = ((Data) block).variables.get(i);
          if (thisVar == destVar) {
            continue;
          } else if (thisVar == null) {
            return false;
          } else if (!thisVar.equalsVariable(destVar)) {
            return false;
          }
        }
      }
    } else if (this.variables != null || ((Data) block).variables != null) {
      return false;
    }

    if (this.values != null && ((Data) block).values != null) {
      if (this.values.size() == ((Data) block).values.size()) {
        for (int i = 0; i < this.values.size(); i++) {
          Expression thisExp = this.values.get(i);
          Expression destExp = ((Data) block).values.get(i);
          if (thisExp == destExp) {
            continue;
          } else if (thisExp == null) {
            return false;
          } else if (!thisExp.equalsExpression(destExp)) {
            return false;
          }
        }
      }
    } else if (this.values != null || ((Data) block).values != null) {
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

    if (this.variables != null) {
      for (Variable variable : this.variables) {
        IInformation[] infos = variable.searchInformationBlocks(block);
        if (infos != null) {
          list.addAll(Arrays.asList(infos));
        }
      }
    }
    if (this.values != null) {
      for (Expression value : this.values) {
        IInformation[] infos = value.searchInformationBlocks(block);
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
