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

package jp.riken.kscope.language.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.*;

/**
 * Database reset class. Clear definition references for subroutines, functions, and variables.
 *
 * @author RIKEN
 */
public class ClearDefinitions implements ILanguageEntry {
  /** Search history list */
  private List<Object> listVisit;
  /** Fortran database */
  private Fortran language;
  /** Module map to be cleared <old module, new module> */
  private java.util.Map<Module, Module> mapClearModule;

  /**
   * Constructor
   *
   * @param Fortran database
   */
  public ClearDefinitions(Fortran language) {
    this.language = language;
    this.listVisit = new ArrayList<Object>();
  }

  /**
   * Get the Fortran database.
   *
   * @return Fortran database
   */
  public Fortran getLanguage() {
    return language;
  }

  /**
   * Set up a Fortran database.
   *
   * @param language Fortran database
   */
  public void setLanguage(Fortran language) {
    this.language = language;
  }

  /**
   * Set the search list.
   *
   * @param list Search list
   */
  public List<Object> getListVisit() {
    return this.listVisit;
  }

  /**
   * Set the search list.
   *
   * @param list Search list
   */
  public void setListVisit(List<Object> list) {
    this.listVisit = list;
  }

  /** Clear the Variable Definition map. */
  @Override
  public void entry(Module entry) {
    // Clear the VariableDefinition map.
    entry((ProgramUnit) entry);
  }

  /** Clear the ProcedureUsage class list. */
  @Override
  public void entry(Procedure entry) {
    if (entry == null) return;
    if (entry.getCallMember() == null) return;
    if (entry.getCallMember().size() <= 0) return;
    if (this.containsClearProcedure(entry)) {
      // Clear the ProcedureUsage class list.
      entry.setCallMember(null);
    } else if (!this.containsLanguageProcedure(entry)) {
      // Clear the ProcedureUsage class list.
      entry.setCallMember(null);
    }
    // Clear the VariableDefinition map.
    entry((ProgramUnit) entry);
  }

  /**
   * Clear the Variable Definition map.
   *
   * @param entry ProgramUnit
   */
  public void entry(ProgramUnit entry) {
    if (entry == null) return;
    java.util.Map<String, VariableDefinition> map = entry.getVariableMap();
    java.util.Iterator<String> itr = map.keySet().iterator();
    while (itr.hasNext()) {
      String key = itr.next();
      VariableDefinition def = map.get(key);
      if (containsClearVariableDefinition(def)) {
        itr.remove();
      } else if (!containsLanguageVariableDefinition(def)) {
        itr.remove();
      }
    }
  }

  /** Clear the variable definition. */
  @Override
  public void entry(Variable entry) {
    if (entry == null) return;
    VariableDefinition def = entry.getDefinition();
    if (def == null) return;
    if (containsClearVariableDefinition(def)) {
      entry.setDefinition(null);
    } else if (!containsLanguageVariableDefinition(def)) {
      entry.setDefinition(null);
    }
    return;
  }

  /** Clear the CALL statement Procedure definition. */
  @Override
  public void entry(ProcedureUsage entry) {
    if (entry == null) return;
    if (entry.getCallDefinition() == null) return;
    Procedure callDefinition = entry.getCallDefinition();
    if (this.containsClearProcedure(callDefinition)) {
      // Clear the CALL statement Procedure definition.
      entry.setCallDefinition(null);
    } else if (!this.containsLanguageProcedure(callDefinition)) {
      // Clear the CALL statement Procedure definition.
      entry.setCallDefinition(null);
    }
  }

  /** Clear the program that references the variable. */
  @Override
  public void entry(VariableDefinition entry) {
    if (entry == null) return;
    Set<ProgramUnit> refer = entry.getReferMember();
    if (refer == null || refer.size() <= 0) return;
    java.util.Iterator<ProgramUnit> iter = refer.iterator();
    while (iter.hasNext()) {
      ProgramUnit prog = iter.next();
      if (containsClearProgramUnit(prog)) {
        iter.remove();
      } else if (!containsLanguageProgramUnit(prog)) {
        iter.remove();
      }
    }
    return;
  }

  /** Clear the structure definition. */
  @Override
  public void entry(VariableType entry) {
    if (entry == null) return;
    if (entry.getPrimitiveDataType() == PrimitiveDataType.TYPE) {
      Type type = entry.getType();
      if (type != null && type.getMotherBlock() != null) {
        if (containsClearBlock(type.getMotherBlock())) {
          entry.setType(null);
        } else if (!containsLanguageBlock(type.getMotherBlock())) {
          entry.setType(null);
        }
      }
    }
  }

  @Override
  public void entry(Break entry) {}

  @Override
  public void entry(Common entry) {}

  @Override
  public void entry(Condition entry) {}

  @Override
  public void entry(Continue entry) {}

  @Override
  public void entry(Data entry) {}

  @Override
  public void entry(Directive entry) {}

  @Override
  public void entry(DoNothing entry) {}

  @Override
  public void entry(DynamicAllocation entry) {}

  @Override
  public void entry(DynamicDeallocation entry) {}

  @Override
  public void entry(DynamicNullification entry) {}

  @Override
  public void entry(Equivalence entry) {}

  @Override
  public void entry(ExecutableBody entry) {}

  @Override
  public void entry(GoTo entry) {}

  @Override
  public void entry(Pause entry) {}

  @Override
  public void entry(Procedures entry) {}

  @Override
  public void entry(Repetition entry) {}

  @Override
  public void entry(Return entry) {}

  @Override
  public void entry(Selection entry) {}

  @Override
  public void entry(Substitution entry) {}

  @Override
  public void entry(Termination entry) {}

  @Override
  public void entry(UserDefined entry) {}

  @Override
  public void entry(UseState entry) {}

  @Override
  public void entry(ProcedureWithNameOnly entry) {}

  @Override
  public void entry(VariableAttribute entry) {}

  @Override
  public void entry(VariableDimension entry) {}

  @Override
  public void entry(DimensionIndex entry) {}

  @Override
  public void entry(Expression entry) {}

  @Override
  public void entry(ProcedureItem entry) {}

  @Override
  public void entry(Type entry) {}

  @Override
  public void entry(Structure entry) {}

  @Override
  public void entry(Union entry) {}

  /**
   * Set the module to be cleared
   *
   * @param list Module map to be cleared <old module, new module>
   */
  public void setListClearModule(java.util.Map<Module, Module> mapModule) {
    this.mapClearModule = mapModule;
  }

  /**
   * Check if the module is the module to be cleared.
   *
   * @param module Clear check module
   * @return true = clear module
   */
  private boolean containsClearModule(Module module) {
    if (this.mapClearModule == null) return false;
    if (module == null) return false;
    return this.mapClearModule.containsKey(module);
  }

  /**
   * Check if the procedure is included in the module to be cleared.
   *
   * @param definition Procedure
   * @return true = Clear module procedure
   */
  private boolean containsClearProcedure(Procedure definition) {
    Module module = getParentModule(definition);
    if (module == null) return false;
    return containsClearModule(module);
  }

  /**
   * Check if it is a Variable Definition included in the module to be cleared.
   *
   * @param definition VariableDefinition
   * @return true = Clear module VariableDefinition
   */
  private boolean containsClearVariableDefinition(VariableDefinition definition) {
    Module module = getParentModule(definition);
    if (module == null) return false;
    return containsClearModule(module);
  }

  /**
   * Check if it is a Program Unit included in the module to be cleared.
   *
   * @param definition ProgramUnit
   * @return true = Clear module ProgramUnit
   */
  private boolean containsClearProgramUnit(ProgramUnit definition) {
    if (definition == null) return false;
    if (definition instanceof Procedure) {
      return containsClearProcedure((Procedure) definition);
    } else if (definition instanceof Module) {
      return containsClearModule((Module) definition);
    }
    return false;
  }

  /**
   * Check if the block is included in the module to be cleared.
   *
   * @param definition Block
   * @return true = Clear module Block
   */
  private boolean containsClearBlock(IBlock definition) {
    if (definition == null) return false;
    if (definition instanceof Procedure) {
      return containsClearProcedure((Procedure) definition);
    } else if (definition instanceof Module) {
      return containsClearModule((Module) definition);
    } else if (definition instanceof VariableDefinition) {
      return containsClearVariableDefinition((VariableDefinition) definition);
    }
    return false;
  }

  /**
   * Check if the module is a database module.
   *
   * @param module Clear check module
   * @return true = database module
   */
  private boolean containsLanguageModule(Module module) {
    if (module == null) return false;
    Module langModule = this.language.module(module.get_name());
    return (module == langModule);
  }

  /**
   * Check if the procedure is included in the database module.
   *
   * @param definition Procedure
   * @return true = Clear module procedure
   */
  private boolean containsLanguageProcedure(Procedure definition) {
    Module module = getParentModule(definition);
    if (module == null) return false;
    return containsLanguageModule(module);
  }

  /**
   * Check if it is a Variable Definition included in the database module.
   *
   * @param definition VariableDefinition
   * @return true = Clear module VariableDefinition
   */
  private boolean containsLanguageVariableDefinition(VariableDefinition definition) {
    Module module = getParentModule(definition);
    if (module == null) return false;
    return containsLanguageModule(module);
  }

  /**
   * Check if it is a Program Unit included in the database module.
   *
   * @param definition ProgramUnit
   * @return true = Clear module ProgramUnit
   */
  private boolean containsLanguageProgramUnit(ProgramUnit definition) {
    if (definition == null) return false;
    if (definition instanceof Procedure) {
      return containsLanguageProcedure((Procedure) definition);
    } else if (definition instanceof Module) {
      return containsLanguageModule((Module) definition);
    }
    return false;
  }

  /**
   * Check if it is a Block included in the database module.
   *
   * @param definition Block
   * @return true = Clear module Block
   */
  private boolean containsLanguageBlock(IBlock definition) {
    if (definition == null) return false;
    if (definition instanceof Procedure) {
      return containsLanguageProcedure((Procedure) definition);
    } else if (definition instanceof Module) {
      return containsLanguageModule((Module) definition);
    } else if (definition instanceof VariableDefinition) {
      return containsLanguageVariableDefinition((VariableDefinition) definition);
    }
    return false;
  }

  /**
   * Get the parent module of Procedure
   *
   * @param definition Procedure
   * @return module
   */
  private Module getParentModule(Procedure definition) {
    if (definition == null) return null;
    ProgramUnit block = definition.get_mother();
    if (block == null) return null;
    while (true) {
      if (block.get_mother() == null) {
        break;
      }
      block = block.get_mother();
    }

    if (!(block instanceof Module)) {
      return null;
    }

    return (Module) block;
  }

  /**
   * Get the parent module of VariableDefinition
   *
   * @param definition VariableDefinition
   * @return module
   */
  private Module getParentModule(VariableDefinition definition) {
    if (definition == null) return null;
    ProgramUnit block = definition.getMother();
    if (block == null) return null;
    while (true) {
      if (block.get_mother() == null) {
        break;
      }
      block = block.get_mother();
    }

    if (!(block instanceof Module)) {
      return null;
    }

    return (Module) block;
  }
}
