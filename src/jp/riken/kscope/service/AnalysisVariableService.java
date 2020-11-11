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
package jp.riken.kscope.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.IntentAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.PointerAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.utils.StringUtils;

/**
 * Service class that creates a list of variable characteristics
 *
 * @author RIKEN
 */
public class AnalysisVariableService extends AnalysisBaseService {

  /** Variable characteristic list table model */
  private VariableTableModel modelVariable;

  /**
   * Constructor
   *
   * @param fortran Fortran database
   */
  public AnalysisVariableService(Fortran fortran) {
    super(fortran);
  }

  /**
   * Variable characteristic list Set table model
   *
   * @param modelVariable Variable characteristic list table model
   */
  public void setModelVariable(VariableTableModel modelVariable) {
    this.modelVariable = modelVariable;
  }

  /**
   * Create a list of variable characteristics. <br>
   * Create a variable characteristic list from the Fortran database and the block to be analyzed.
   * <br>
   * Set the created variable characteristic list in the variable characteristic list table model.
   *
   * @param blocks Block list
   */
  public void analysisVariable(IBlock[] blocks) {

    if (blocks == null) {
      return;
    }

    for (IBlock block : blocks) {
      // Convert to the program unit to which the block belongs
      while (block instanceof Block) {
        if (block instanceof ExecutableBody) {
          block = ((ExecutableBody) block).getParent();
        } else {
          block = block.getMotherBlock();
        }
      }
      if (block == null) {
        return;
      }
      this.modelVariable.setTitle(block.toString());
      if (block instanceof ProgramUnit) {
        VariableDefinition[] list = ((ProgramUnit) block).get_variables();
        for (VariableDefinition vardef : list) {
          String[] infos = makeVariableDefinitionInfo(vardef);
          this.modelVariable.addVariableInfo(block, vardef, infos);
        }
        // Set of structures
        Set<Type> types = this.getTypeList(list);
        for (Type tp : types) {
          List<VariableDefinition> defs = tp.getDefinitions();
          for (VariableDefinition def : defs) {
            String[] infos = makeVariableDefinitionInfo(def);
            this.modelVariable.addVariableInfo(tp, def, infos);
          }
        }
        // Set of internal subprograms
        Collection<Procedure> children = ((ProgramUnit) block).getChildren();
        for (Procedure child : children) {
          VariableDefinition[] varlist = child.get_variables();
          for (VariableDefinition vardef : varlist) {
            String[] infos = makeVariableDefinitionInfo(vardef);
            this.modelVariable.addVariableInfo(child, vardef, infos);
          }
          // Set of structures
          Set<Type> tps = this.getTypeList(varlist);
          for (Type tp : tps) {
            List<VariableDefinition> defs = tp.getDefinitions();
            for (VariableDefinition def : defs) {
              String[] infos = makeVariableDefinitionInfo(def);
              this.modelVariable.addVariableInfo(tp, def, infos);
            }
          }
        }
      }
    }
  }

  /**
   * Returns the included set of Types from an array of variable declarations.
   *
   * @param list Array of variable declarations
   * @return A set of Type classes. If not, it returns an empty set.
   */
  private Set<Type> getTypeList(VariableDefinition[] list) {
    Set<Type> set = new HashSet<Type>();
    if (list != null) {
      for (int i = 0; i < list.length; i++) {
        if (list[i].getType() instanceof VariableType) {
          VariableType type = (VariableType) list[i].getType();
          if (type.getPrimitiveDataType() == PrimitiveDataType.TYPE) {
            if (type.getType() != null) {
              set.add(type.getType());
            }
          }
        }
      }
    }
    return set;
  }

  /**
   * Sort the array of variable declarations in the order of scalar and array.
   *
   * @param list Array of variable declarations
   * @return A set of sorted variable declarations. If not, it returns an empty set.
   */
  @SuppressWarnings("unused")
  private Set<VariableDefinition> sort(VariableDefinition[] list) {
    if (list == null) {
      return new LinkedHashSet<VariableDefinition>();
    }
    Set<VariableDefinition> set = new LinkedHashSet<VariableDefinition>();
    Set<VariableDefinition> scalars = new LinkedHashSet<VariableDefinition>();
    Set<VariableDefinition> arrays = new LinkedHashSet<VariableDefinition>();

    for (int i = 0; i < list.length; i++) {
      if (list[i].isScalar()) {
        scalars.add(list[i]);
      } else {
        arrays.add(list[i]);
      }
    }
    set.addAll(scalars);
    set.addAll(arrays);
    return set;
  }

  /**
   * Create a list of variable characteristics. <br>
   * Create a variable characteristic list from the Fortran database and the variable declaration to
   * be analyzed. <br>
   * Set the created variable characteristic list in the variable characteristic list table model.
   *
   * @param vars Variable declaration list
   */
  public void analysisVariable(VariableDefinition[] vars) {
    this.modelVariable.setTitle("VariableDefinition List");
    // Set<VariableDefinition> varSet = this.sort(vars);
    for (VariableDefinition vardef : vars) {
      IBlock block = vardef.getMother(); // Procedure to which the variable belongs
      String[] infos = makeVariableDefinitionInfo(vardef);
      this.modelVariable.addVariableInfo(block, vardef, infos);
    }
  }

  private String[] makeVariableDefinitionInfo(VariableDefinition var) {
    List<String> infos = new ArrayList<String>();
    // Scalar or array
    if (var.isScalar()) {
      infos.add("scalar");
    } else {
      infos.add("array");
    }
    // Variable name
    infos.add(var.get_name());
    // Data type
    IVariableType tp = var.getVariableType();
    infos.add(tp.toString());

    // Set of various attributes
    VariableAttribute att = (VariableAttribute) var.getAttribute();

    // Scope attribute
    ScopeAttribute sc;
    if (att == null) {
      sc = ScopeAttribute.NONE;
    } else {
      sc = att.getScope();
    }
    if (sc == ScopeAttribute.PRIVATE) {
      infos.add("private");
    } else if (sc == ScopeAttribute.PUBLIC) {
      infos.add("public");
    } else {
      infos.add("default");
    }

    // Parameter attributes
    if (att != null && att.hasParameter()) {
      infos.add("parameter");
    } else {
      infos.add("no param");
    }

    // initial value
    if (var.getInitValue() == null) {
      infos.add("no value");
    } else {
      infos.add(var.getInitValue());
    }

    if (var.isScalar()) {
      infos.add("1");
    } else {
      VariableDimension dim = var.getVariableDimension();
      infos.add(dim.toString());
    }

    if (att == null) {
      infos.add("no intent");
      infos.add("no opt");
      infos.add("no pointer");
      infos.add("no save");
      // common attribute
      ProgramUnit pu = var.getMother();
      String common = null;
      if (pu != null) {
        common = pu.getCommonName(var.get_name());
      }
      if (common == null) {
        infos.add("no common");
      } else {
        infos.add("com : " + common);
      }
      infos.add("no alloc");
    } else {
      // intent attribute
      IntentAttribute ia = att.getIntent();
      if (ia == IntentAttribute.NONE) {
        infos.add("no intent");
      } else if (ia == IntentAttribute.IN) {
        infos.add("in");
      } else if (ia == IntentAttribute.OUT) {
        infos.add("out");
      } else if (ia == IntentAttribute.INOUT) {
        infos.add("inout");
      }

      // optional attribute
      if (att.hasOptional()) {
        infos.add("optional");
      } else {
        infos.add("no opt");
      }

      // pointer / target attribute
      PointerAttribute pa = att.getPointerOrTarget();
      if (pa == PointerAttribute.NONE) {
        infos.add("no pointer");
      } else if (pa == PointerAttribute.POINTER) {
        infos.add("pointer");
      } else if (pa == PointerAttribute.TARGET) {
        infos.add("target");
      }

      // save attribute
      if (att.hasSave()) {
        infos.add("save");
      } else {
        infos.add("no save");
      }

      // common attribute
      ProgramUnit pu = var.getMother();
      String common = null;
      if (pu != null) {
        common = pu.getCommonName(var.get_name());
      }
      if (common == null) {
        infos.add("no common");
      } else {
        infos.add("com : " + common);
      }

      // allocatable attribute
      if (att.hasAllocatable()) {
        infos.add("allocatable");
      } else {
        infos.add("no alloc");
      }
    }

    // Additional information
    if (var.getInformation() != null
        && !StringUtils.isNullOrEmpty(var.getInformation().getContent())) {
      infos.add(var.getInformation().getContent());
    } else {
      infos.add("no info");
    }
    return infos.toArray(new String[0]);
  }
}
