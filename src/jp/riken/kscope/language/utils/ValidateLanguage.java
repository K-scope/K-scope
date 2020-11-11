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
import java.util.Map;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.language.Break;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.Continue;
import jp.riken.kscope.language.Data;
import jp.riken.kscope.language.DimensionIndex;
import jp.riken.kscope.language.Directive;
import jp.riken.kscope.language.DoNothing;
import jp.riken.kscope.language.DynamicAllocation;
import jp.riken.kscope.language.DynamicDeallocation;
import jp.riken.kscope.language.DynamicNullification;
import jp.riken.kscope.language.Equivalence;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.GoTo;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Pause;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Repetition;
import jp.riken.kscope.language.Return;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Termination;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.UserDefined;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Structure;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.fortran.Union;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.ProcedureItem;
import jp.riken.kscope.language.generic.ProcedureWithNameOnly;
import jp.riken.kscope.language.generic.Procedures;

/**
 * Database validation class. Search for undefined structures and set definitions.
 *
 * @author RIKEN
 */
public class ValidateLanguage implements ILanguageEntry {
  /** Search history list */
  private List<Object> listVisit;
  /** List of unverifiable structures */
  private List<ErrorTypeInfo> errorTypes;
  /** Fortran database */
  private Fortran language;
  /** Error message model */
  private List<ErrorInfo> errors;
  /** Searched modules */
  private List<ProgramUnit> searchedPrograms;
  /** Maximum number of loops for structure analysis */
  private final int ANALYSIS_LOOP_MAX = 16;

  /**
   * Error information class
   *
   * @author RIKEN
   */
  class ErrorTypeInfo {
    /** Error structure */
    public Type errorType;
    /** Error location list */
    public List<Object> listLocation;
    /** Error status flag: true = error */
    private boolean error;

    /**
     * Constructor
     *
     * @param errorType Error structure
     * @param listLocation Error location list
     */
    public ErrorTypeInfo(Type errorType, List<Object> listLocation) {
      this.errorType = errorType;
      this.listLocation = listLocation;
      this.error = true;
    }

    /** Check if they are the same error information class object. */
    @Override
    public boolean equals(Object obj) {
      if (!(obj instanceof ErrorTypeInfo)) return false;

      ErrorTypeInfo dest = (ErrorTypeInfo) obj;
      if (this.errorType != dest.errorType) return false;
      // if (!this.listLocation.containsAll(dest.listLocation)) return false;
      return true;
    }

    /**
     * Get error status
     *
     * @return true = error
     */
    public boolean isError() {
      return this.error;
    }

    /**
     * Clear the error condition. <br>
     * Set error to false.
     */
    public void clearError() {
      this.error = false;
    }
  }

  /**
   * Constructor
   *
   * @param language Build Fortran database
   */
  public ValidateLanguage(Fortran language) {
    this.language = language;
    this.errorTypes = new ArrayList<ErrorTypeInfo>();
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
   * Get the search list.
   *
   * @return Search list
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

  /**
   * Add error structure
   *
   * @param error Error structure
   */
  private void addErrorType(Type error) {
    ArrayList<Object> list = new ArrayList<Object>();
    list.addAll(this.listVisit);
    ErrorTypeInfo info = new ErrorTypeInfo(error, list);
    if (this.errorTypes.contains(info)) {
      return;
    }
    this.errorTypes.add(info);
  }

  /**
   * Verify and repair the chain of structures.
   *
   * @return Number of repair errors in the structure chain
   */
  public int analyseTypes() {
    int error = -1;
    int count = ANALYSIS_LOOP_MAX;
    while (true) {
      int error_last = analyseType();
      if (error_last == 0) {
        error = error_last;
        break;
      } else if (error != -1 && error <= error_last) {
        error = error_last;
        break;
      } else {
        error = error_last;
      }
      count--;
      if (count <= 0) break;
    }
    return error;
  }

  /**
   * Verify and repair the chain of structures.
   *
   * @return Number of repair errors in the structure chain
   */
  private int analyseType() {
    int errorCount = 0;
    if (errorTypes == null || errorTypes.size() <= 0) {
      return errorCount;
    }
    for (ErrorTypeInfo info : this.errorTypes) {
      if (!info.isError()) continue;
      ProgramUnit program = getProgramUnit(info.listLocation);
      if (program == null) continue;
      CodeLine errorLine = program.getStartCodeLine();
      String typename = info.errorType.getName();

      boolean success = true;
      List<VariableDefinition> defs = info.errorType.getDefinitions();
      if (defs != null) {
        for (VariableDefinition def : defs) {
          if (!(def.getType() instanceof VariableType)) continue;
          VariableType deftype = (VariableType) def.getType();
          if (deftype.getPrimitiveDataType() != PrimitiveDataType.TYPE) continue;
          if (deftype.getType() == null) continue;
          if (analyseType(program, deftype.getType())) {
            success &= true;
          } else {
            success = false;
            String msg = Message.getString("validatelanguage.error", typename);
            addErrorMsg(errorLine, msg);
            errorCount++;
          }
        }
      }
      if (analyseType(program, info.errorType)) {
        success &= true;
      } else {
        success = false;
        String msg = Message.getString("validatelanguage.error", typename);
        addErrorMsg(errorLine, msg);
        errorCount++;
      }
      if (success) {
        info.clearError();
      }
    }
    return errorCount;
  }

  /**
   * Search for structure definitions.
   *
   * @param program module
   * @param errorType Structure to be repaired
   * @return Structure definition
   */
  private Type searchValidType(ProgramUnit program, Type errorType) {
    if (program == null) return null;
    if (errorType == null) return null;
    String srcname = errorType.getName();
    if (srcname == null || srcname.isEmpty()) return null;

    // Exit if searched module
    if (this.searchedPrograms == null) {
      this.searchedPrograms = new ArrayList<ProgramUnit>();
    }
    if (this.searchedPrograms.contains(program)) {
      return null;
    }
    this.searchedPrograms.add(program);

    List<Type> list = program.getTypeList();
    if (list != null) {
      for (Type dest : list) {
        String destname = dest.getName();
        if (srcname.equalsIgnoreCase(destname)) {
          if (!isValidType(dest)) continue;
          return dest;
        }
      }
    }

    // Search for the same structure from the module of the use statement
    Type target = null;
    for (UseState useEle : program.getUseList()) {
      if (useEle.hasOnlyMember()) {
        if (useEle.hasOnlyMember(srcname)) {
          Module useModule = language.module(useEle.getModuleName());
          target = searchValidType(useModule, errorType);
          if (target != null) {
            return target;
          }
        }
      } else {
        Module useModule = language.module(useEle.getModuleName());
        target = searchValidType(useModule, errorType);
        if (target != null) {
          return target;
        }
      }
    }
    return null;
  }

  /**
   * Check if the structure member contains a null definition.
   *
   * @param type structure
   * @return true = normal struct
   */
  private boolean isValidType(Type type) {

    List<VariableDefinition> varDefs = type.getDefinitions();
    if (varDefs != null && varDefs.size() > 0) {
      for (VariableDefinition vardef : varDefs) {
        IBlock defmother = vardef.getMotherBlock();
        if (defmother != null) {
          if (!isLanguageModule(defmother)) {
            return false;
          }
        }
        if (vardef.getType() == null) {
          return false;
        } else if (vardef.getType() instanceof VariableType) {
          VariableType deftype = (VariableType) vardef.getType();
          if (deftype.getPrimitiveDataType() == PrimitiveDataType.TYPE) {
            if (deftype.getType() == null) {
              return false;
            }
            IBlock typemother = deftype.getType().getMotherBlock();
            if (!isLanguageModule(typemother)) {
              return false;
            }
          }
        }
      }
    }
    return true;
  }

  /**
   * Copy the structure definition
   *
   * @param destType Copy destination structure
   * @param srcType Copy source structure
   */
  private void copyType(Type destType, Type srcType) {
    List<VariableDefinition> srcDefs = srcType.getDefinitions();
    destType.setDefinitions(srcDefs);
    destType.setMotherBlock(srcType.getMotherBlock());
  }

  /**
   * Search for subroutines and modules. Search for subroutines and modules in which a structure is
   * defined from the parent hierarchy of the structure definition.
   *
   * @param listLocation Parent hierarchy of structure definition
   * @return Subroutines, modules
   */
  private ProgramUnit getProgramUnit(List<Object> listLocation) {
    if (listLocation == null || listLocation.size() <= 0) return null;

    // Search for subroutines and modules from the bottom.
    for (int i = listLocation.size() - 1; i >= 0; i--) {
      if (listLocation.get(i) instanceof ProgramUnit) {
        return (ProgramUnit) listLocation.get(i);
      }
    }
    return null;
  }

  @Override
  public void entry(Type entry) {
    if (!isValidType(entry)) {
      addErrorType(entry);
    }
  }

  /**
   * Get the error message list
   *
   * @return errorMessage Error message list
   */
  public ErrorInfo[] getErrorList() {
    if (this.errors == null || this.errors.size() <= 0) return null;
    return this.errors.toArray(new ErrorInfo[0]);
  }

  /** Clear the error message list. */
  public void clearErrorList() {
    this.errors = null;
  }

  /**
   * Add error message to error information list
   *
   * @param lineInfo Error location information
   * @param message Error message
   */
  public void addErrorMsg(CodeLine lineInfo, String message) {
    if (this.errors == null) {
      this.errors = new ArrayList<ErrorInfo>();
    }
    ErrorInfo err = new ErrorInfo(lineInfo, message);
    this.errors.add(err);
  }

  @Override
  public void entry(Module entry) {}

  @Override
  public void entry(Procedure entry) {}

  @Override
  public void entry(Variable entry) {}

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
  public void entry(ProcedureUsage entry) {}

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
  public void entry(VariableDefinition entry) {}

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
  public void entry(Structure entry) {}

  @Override
  public void entry(VariableType entry) {}

  @Override
  public void entry(Union entry) {}

  /**
   * Check if the module block is included in the build Fortran database.
   *
   * @param block Module block
   * @return true = Build included in Fortran database
   */
  private boolean isLanguageModule(IBlock block) {
    if (block == null) return false;
    if (this.language == null) return false;

    Map<String, Module> modules = this.language.getModules();

    // Search for modules
    for (String key : modules.keySet()) {
      Module module = modules.get(key);
      if (module == block) {
        return true;
      }
    }
    return false;
  }

  /**
   * Verify and repair the chain of structures.
   *
   * @param program Search target program
   * @param errorType Check structure
   * @return true = success
   */
  private boolean analyseType(ProgramUnit program, Type errorType) {
    if (program == null) return false;
    if (errorType == null) return false;
    if (!isValidType(errorType)) return true;
    Type target = null;
    this.searchedPrograms = null;

    while (program != null) {
      // Search from your own subroutines and modules
      target = searchValidType(program, errorType);
      if (target != null) {
        break;
      }
      program = program.get_mother();
    }

    if (target == null) {
      // Search for subroutines in NO_MODULE
      Module main = language.module("NO_MODULE");
      target = searchValidType(main, errorType);
    }
    if (target != null) {
      copyType(errorType, target);
    } else {
      return false;
    }
    return true;
  }
}
