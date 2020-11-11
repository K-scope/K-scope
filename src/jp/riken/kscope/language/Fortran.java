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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import jp.riken.kscope.Application;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.generic.IProcedureItem;
import jp.riken.kscope.language.generic.ProcedureWithNameOnly;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.parser.IAnalyseParser;

/**
 * A class that represents a Fortran program. All the information obtained by parsing the source
 * file is generated using the methods of this class (including the super class). Class Fortran
 * consists of ProgramUnit and Block derived classes. Object creation method is always executed for
 * currentUnit and currentBlock.
 */
public final class Fortran extends Program {
  /** Serial number */
  private static final long serialVersionUID = -5141333793433490902L;
  /** Working array for storing module names */
  private String[] moduleName;
  /** Fortran source file list */
  private ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
  /** Working variables for declaration search */
  private transient Map<String, Procedure> knownProcedure = new HashMap<String, Procedure>();
  /** Cancel flag */
  private transient boolean cancel = false;

  /** Constructor. */
  public Fortran() {
    super();
  }

  /**
   * Set a list of source files.
   *
   * @param files List of source files
   */
  public void setSourceFileList(ArrayList<SourceFile> files) {
    sourceFileList = files;
  }

  /**
   * Returns a list of source files. <br>
   * Only source files that correspond to XML files
   *
   * @return List of source files
   */
  public ArrayList<SourceFile> getSourceFileList() {
    return sourceFileList;
  }

  /**
   * Returns a list of source files. <br>
   * All source files, including procedure source files
   *
   * @return List of source files
   */
  public ArrayList<SourceFile> getProcedureFileList() {
    ArrayList<SourceFile> moduleFileList = getSourceFileList();
    ArrayList<SourceFile> list = new ArrayList<SourceFile>();
    list.addAll(moduleFileList);
    Collection<Module> mods = this.getModules().values();
    for (Module mod : mods) {
      Collection<Procedure> procs = mod.getChildren();
      for (Procedure proc : procs) {
        SourceFile procFile = proc.getStartCodeLine().getSourceFile();
        if (procFile != null && !list.contains(procFile)) {
          list.add(procFile);
        }
      }
    }
    if (list.size() <= 0) return null;
    return list;
  }

  // ++++++++++++++++++++++++++++++++++++++++++//
  // interface method //
  // ++++++++++++++++++++++++++++++++++++++++++//

  // ---------------ProgramUnit----------------//
  /**
   * Create a subroutine (Procedure) as a child of the currentUnit. Used when the subroutine has no
   * arguments. You must call endSubroutine before executing a new init method.
   *
   * @param subName Subroutine name.
   */
  public void initSubroutine(String subName) {
    super.init_procedure("subroutine", subName);
  }

  /**
   * Create a subroutine (Procedure) as a child of the currentUnit. Used when the subroutine has
   * arguments. You must call endSubroutine before executing a new init method.
   *
   * @param subName Subroutine name
   * @param args Subroutine arguments
   */
  public void initSubroutine(String subName, String[] args) {
    super.init_procedure("subroutine", subName, args);
  }

  /** Change currentUnit to parent. */
  public void endSubroutine() {
    super.end_procedure();
  }

  /**
   * Start a function declaration.
   *
   * @param sub_name Function name
   */
  public void init_function(String sub_name) {
    super.init_procedure("function", sub_name);
  }

  /**
   * Start a function declaration.
   *
   * @param sub_name Function name
   * @param args Formal argument
   */
  public void init_function(String sub_name, String[] args) {
    super.init_procedure("function", sub_name, args);
  }

  /** Exit the function. */
  public void end_function() {
    super.end_procedure();
  }

  // ++++++++++++++++++++++++++++++++++++++++++//
  // analyse method //
  // ++++++++++++++++++++++++++++++++++++++++++//

  /**
   * Perform parsing on the entire program and associate declarations with calls.
   *
   * @param parser GUI control class
   */
  public void analyseDB(IAnalyseParser parser) {
    moduleName = get_module_name();
    parser.firePropertyChange("status_message", null, "Analyse calls");
    parser.firePropertyChange("status_sub_message", null, "parsing...");
    parser.firePropertyChange("prograss_maxvalue", null, moduleName.length);
    for (int i = 0; i < moduleName.length; i++) {
      Module current_module = module(moduleName[i]);
      Collection<Procedure> subs = current_module.getChildren();
      analyseDBInUnit(subs);
      parser.firePropertyChange("prograss_string", null, String.valueOf(i));
      parser.firePropertyChange("prograss_value", null, i);
    }
    parser.firePropertyChange("status_sub_message", null, "done");
    parser.firePropertyChange("prograss_clear", null, null);
  }

  /** Perform parsing on the entire program and associate declarations with calls. */
  public void analyseDB() {
    Application.status.setMessageStatus("analysys database...");
    moduleName = this.get_module_name();
    for (int i = 0; i < moduleName.length; i++) {
      // Cancel check
      if (isCancel()) break;
      Application.status.setMessageStatus("analysys database..." + moduleName[i]);
      Module current_module = module(moduleName[i]);
      Collection<Procedure> subs = current_module.getChildren();
      analyseDBInUnit(subs);
    }
    Application.status.setMessageStatus("analysys database...done");
  }

  /**
   * Associate a declaration with a call for the passed procedure.
   *
   * @param subs Arrangement of procedures
   */
  private void analyseDBInUnit(Collection<Procedure> subs) {
    if (this.knownProcedure == null) {
      this.knownProcedure = new HashMap<String, Procedure>();
    }
    for (Procedure sub : subs) {
      // Cancel check
      if (isCancel()) break;
      List<ProcedureUsage> calls = sub.getCalls();
      this.knownProcedure.clear();
      for (ProcedureUsage call : calls) {
        if (call.isIntrinsic()) {
          // Processing for the TODO INTRINSIC function. Currently unnecessary, but it seems
          // possible to handle it in some way
        } else {
          this.searchCallDeclaration(sub, call);
        }
      }
      Set<String> defSet = sub.getVariables().keySet();
      Set<String> newSet = new HashSet<String>(sub.getVariableMap().keySet());
      newSet.addAll(defSet);
      for (String varName : newSet) {
        searchVariableDefinition(sub, varName);
      }
      if (sub.getChildren().size() > 0) {
        analyseDBInUnit(sub.getChildren());
      }

      // add at 2013/03/01 by @hira
      // Set the variable definition for the variable
      Set<Variable> vars = sub.getAllVariables();
      if (vars != null) {
        for (Variable var : vars) {
          String varname = var.getName();
          searchVariableDefinition(sub, varname);
        }
      }
      sub.setVariableDefinitions();
    }
  }

  /**
   * Search for and associate function call declarations.
   *
   * @param pu Program unit to which the function call belongs
   * @param call Function call
   */
  private void searchCallDeclaration(ProgramUnit pu, ProcedureUsage call) {
    ProgramUnit me = pu;
    ProgramUnit current = me;
    String callName = call.getCallName().toLowerCase();

    if (callName == null) {
      return;
    }

    // Check if the definition destination is known
    if (this.knownProcedure.containsKey(callName)) {
      call.setCallDefinition(this.knownProcedure.get(callName));
      return;
    }

    // Search while moving the target to the parent program unit
    String changeName = callName;
    while (current != null) {
      // Execute an Interface statement search for current.
      if (callName.equalsIgnoreCase(call.getCallName())) {
        changeName = this.searchCallDeclarationForInterface(current, call);
      }
      if (!(callName.equalsIgnoreCase(changeName))) {
        callName = changeName;
        current =
            me; // The interface statement was found, so search the procedure again with a new name.
      }
      // Find current internal subprogram
      if (current.getChildren().size() > 0) {
        Collection<Procedure> children = current.getChildren();
        for (Procedure child : children) {
          if (child.get_name().equalsIgnoreCase(callName)) {
            knownProcedure.put(callName, child);
            call.setCallDefinition(child);
            return;
          }
        }
      }

      // Execute a search for the Use statement for current.
      changeName = this.searchCallDeclarationForUse(current, call, callName);
      if (call.getCallDefinition() != null) {
        return;
      }
      if (!(callName.equalsIgnoreCase(changeName))) {
        callName = changeName;
        current =
            me; // The interface statement was found, so search the procedure again with a new name.
        continue;
      }
      // Transfer current to mother
      current = current.get_mother();
    }

    // Search for subroutines in NO_MODULE
    Procedure[] subs = module("NO_MODULE").get_procedures();
    for (int i = 0; i < subs.length; i++) {
      if (subs[i].get_name().equalsIgnoreCase(callName)) {
        knownProcedure.put(callName, subs[i]);
        call.setCallDefinition(subs[i]);
        return;
      }
    }
  }

  /**
   * Search for the interface statement for the specified program unit.
   *
   * @param pu Program unit to be searched
   * @param call Procedure call while searching for declaration
   * @return Unique procedure name converted from the generic name. If not, the original name is
   *     returned.
   */
  private String searchCallDeclarationForInterface(ProgramUnit pu, ProcedureUsage call) {
    String callName = call.getCallName();
    List<Procedures> interfaceList = pu.getInterfaceList();
    for (Procedures generic : interfaceList) {
      // Skip anonymous interface
      if (generic.getName() != null) {
        if (generic.getName().equalsIgnoreCase(callName)) {
          Set<IProcedureItem> items = generic.getProcedures();
          // Search for declaration correspondence of module procedure statement
          Procedure declaration = null;
          for (IProcedureItem item : items) {
            if (item instanceof ProcedureWithNameOnly) {
              // add at 2013/02/01 by @hira
              ProcedureWithNameOnly modProc = (ProcedureWithNameOnly) item;
              if (modProc.getDeclaration() == null) {
                String modProcName = modProc.getName();
                modProc.setDeclaration(this.searchModuleProcedureDeclaration(modProcName, pu));
                // Search for declarations only for formal arguments of the corresponding procedure
                Variable[] args = modProc.getDeclaration().get_args();
                for (int i = 0; i < args.length; i++) {
                  this.searchVariableDefinition(modProc.getDeclaration(), args[i].getName());
                }
              }
              // add at 2013/02/01 by @hira
              if (items.size() == 1) {
                declaration = modProc.getDeclaration();
              } else {
                if (declaration == null) {
                  declaration = modProc.getDeclaration();
                }
              }
            }
          }

          // add at 2013/02/01 by @hira
          if (declaration != null) {
            call.setCallDefinition(declaration);
            return callName;
          }
          callName = generic.getActualCallName(call.getArguments());
        }
      }
    }
    return callName;
  }
  /**
   * Search for the procedure declaration corresponding to the module procedure statement.
   *
   * @param name module The name of the procedure
   * @param pu interface Program unit with statement The procedure pointed to by the @return module
   *     procedure. If not, it returns null.
   */
  private Procedure searchModuleProcedureDeclaration(String name, ProgramUnit pu) {
    ProcedureUsage call = new ProcedureUsage(name, null);
    this.searchCallDeclaration(pu, call);
    if (call.getCallDefinition() == null) {
      return null;
    } else {
      return call.getCallDefinition();
    }
  }

  /**
   * Search the procedure declaration for the use statement and interface statement of the specified
   * program unit.
   *
   * @param pu Program unit of declaration retention candidate
   * @param call Procedure call while searching for declaration
   * @param callName The name of the procedure you are searching for
   * @return Unique procedure name converted from the generic name. If not, the original name is
   *     returned.
   */
  private String searchCallDeclarationForUse(ProgramUnit pu, ProcedureUsage call, String callName) {
    String changeName = callName;
    // Look for pu's interface statement if the call name and callName match
    if (call.getCallName().equalsIgnoreCase(callName)) {
      changeName = this.searchCallDeclarationForInterface(pu, call);
      if (!(changeName.equalsIgnoreCase(call.getCallName()))) {
        return changeName; // The search procedure name has been changed, so processing is
        // interrupted.
      }
      // add by @hira at 2013/02/01
      if (call.getCallDefinition() != null) {
        return changeName;
      }
    }

    // Search for Use statements
    for (UseState useEle : pu.getUseList()) {
      if (useEle.hasOnlyMember()) {
        if (useEle.hasOnlyMember(changeName)) {
          Module useModule = module(useEle.getModuleName());
          if (useModule != null) {
            // Check the procedure
            Procedure[] procs = useModule.get_procedures();
            if (procs != null) {
              for (int i = 0; i < procs.length; i++) {
                if (procs[i].get_name().equalsIgnoreCase(changeName)) {
                  call.setCallDefinition(procs[i]);
                  knownProcedure.put(changeName, procs[i]);
                  return changeName;
                }
              }
            }
          }
        }
      } else {
        Module useModule = module(useEle.getModuleName());
        if (useModule != null) {
          // Check the procedure
          Procedure[] procs = useModule.get_procedures();
          if (procs != null) {
            for (int i = 0; i < procs.length; i++) {
              if (procs[i].get_name().equalsIgnoreCase(changeName)) {
                call.setCallDefinition(procs[i]);
                knownProcedure.put(changeName, procs[i]);
                return changeName;
              }
            }
          }
          changeName = searchCallDeclarationForUse(useModule, call, changeName);
          if (!(changeName.equalsIgnoreCase(call.getCallName()))
              || (call.getCallDefinition() != null)) {
            return changeName; // The search procedure name has been changed, or the declaration has
            // been found, so processing is interrupted.
          }
        }
      }
    }
    return changeName;
  }

  /**
   * Search for and associate variable declarations.
   *
   * @param proc Program unit to which the variable belongs
   * @param varName Variable name
   */
  private void searchVariableDefinition(Procedure proc, String varName) {
    ProgramUnit current = proc;
    if (varName == null) {
      return;
    }

    while (current != null) {
      // Find the current declaration
      VariableDefinition varDef = current.get_variable(varName);
      if (varDef != null) {
        proc.putVariableMap(varName, varDef);
        return;
      }

      // Search for current use destination
      if (current.getUseList() != null) {
        if (searchVariableDefinitionForUse(current, varName, proc)) {
          return;
        }
      }
      current = current.get_mother();
    }
    return;
  }

  /**
   * Search the variable declaration for the USE statement in the program unit, and if found,
   * associate it. If there is another USE statement in the USE destination, it is searched
   * recursively.
   *
   * @param pu Program unit
   * @param varName Variable name
   * @param me Procedure to which the variable belongs
   * @return Boolean value. If a declaration is found, it returns true.
   */
  private boolean searchVariableDefinitionForUse(ProgramUnit pu, String varName, Procedure me) {
    for (UseState useEle : pu.getUseList()) {
      Module useModule = module(useEle.getModuleName());
      if (useModule != null) {
        if (useEle.hasOnlyMember()) {
          String transName = useEle.translationReverse(varName);
          VariableDefinition vd = useModule.get_variable(transName);
          if (vd != null) {
            me.putVariableMap(varName, vd);
            vd.addReferMember(me);
            return true;
          }
        } else {
          String nm = useEle.getTranslationName(varName);
          if (nm != null) {
            varName = nm;
          }
          VariableDefinition vd = useModule.get_variable(varName);
          if (vd != null) {
            me.putVariableMap(varName, vd);
            vd.addReferMember(me);
            return true;
          }
          if (useModule.getUseList() != null) {
            if (searchVariableDefinitionForUse(useModule, varName, me)) {
              return true;
            }
          }
        }
      }
    }
    return false;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Search and return the subroutine with the specified name.
   *
   * @param name Subroutine name
   * @return Subroutine with the specified name
   */
  public Procedure search_subroutine(String name) {

    if (name == null) {
      return null;
    }

    for (Module md : this.getModules().values()) {
      Procedure proc = search_sub_rec(md, name);
      if (proc != null) {
        return proc;
      }
    }
    return null;
  }

  /**
   * Get subprograms from programs and modules.
   *
   * @param proc module
   * @param name Subprogram name
   * @return subprogram
   */
  private Procedure search_sub_rec(ProgramUnit proc, String name) {
    if (proc.is_my_procedure(name)) {
      return (Procedure) (proc.get_child(name));
    } else {
      if (proc.getChildren().size() > 0) {
        Procedure[] proc_child = proc.get_children();
        for (int i = 0; i < proc_child.length; i++) {
          // return search_sub_rec(proc_child[i], name) ;
          Procedure sub_proc = search_sub_rec(proc_child[i], name);
          if (sub_proc != null) {
            return sub_proc;
          }
        }
      }
    }
    return null;
  }

  /**
   * Set the CALL statement.
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, pass Statement.NO_LABEL.
   * @param subroutineName CALL subroutine name
   * @param arguments List of arguments
   * @param intrinsic Set true if it is a built-in function. False if not a built-in function.
   */
  public void setCall(
      CodeLine lineInfo,
      String label,
      String subroutineName,
      List<Expression> arguments,
      boolean intrinsic) {
    super.setProcedureUsage(lineInfo, label, subroutineName, arguments, intrinsic);
  }

  /**
   * Set the DO statement start line
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, set Statement.NO_LABEL
   */
  public void start_loop(CodeLine lineInfo, String label) {
    super.start_repetition(lineInfo, label);
  }

  /**
   * Set the DO statement start line.
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, set Statement.NO_LABEL.
   * @param iterator Loop control variable
   * @param initIterator Open price
   * @param endCondition Closing price
   * @param step Step width
   */
  public void start_loop(
      CodeLine lineInfo,
      String label,
      Variable iterator,
      Expression initIterator,
      Expression endCondition,
      Expression step) {
    super.start_repetition(lineInfo, label, iterator, initIterator, endCondition, step);
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   */
  public void end_loop(CodeLine lineInfo) {
    super.end_repetition(lineInfo);
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_loop(CodeLine lineInfo, String label) {
    super.end_repetition(lineInfo, label);
  }

  /**
   * Set the CONTINUE statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  @Override
  public void set_continue(CodeLine lineInfo, String label) {
    super.set_continue(lineInfo, label);
  }

  /**
   * Make a shallow copy.
   *
   * @param fortran Copy source database
   */
  public void copyShallow(Fortran fortran) {
    this.moduleName = fortran.moduleName;
    this.sourceFileList = fortran.sourceFileList;
    super.copyShallow((Program) fortran);
  }

  /**
   * Check if thread execution is cancelled
   *
   * @return true = Cancel
   */
  public boolean isCancel() {
    return this.cancel;
  }

  /**
   * Set the cancel flag.
   *
   * @param flag Cancel flag
   */
  public void setCancel(boolean flag) {
    this.cancel = flag;
  }
}
