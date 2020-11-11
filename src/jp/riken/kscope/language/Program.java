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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.generic.Procedures;

/** Abstract class that represents the program */
public abstract class Program implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = 6765615822590486646L;
  /** The name of the Module object for storing Procedures that are not included in the module. */
  private final String NO_MODULE = "NO_MODULE";
  /** Main program name */
  private String mainName;
  /** Module list */
  private Map<String, Module> modules = new HashMap<String, Module>();
  /** List of programs declared as Common */
  private Map<String, List<ProgramUnit>> commonMap;
  /** Additional information for block specification */
  private InformationBlocks informationBlocks = new InformationBlocks();
  /** Database insert current procedure */
  private transient ProgramUnit currentUnit;

  /** Constructor. */
  public Program() {
    init_module(NO_MODULE);
  }

  /**
   * Get a set of modules.
   *
   * @return modules
   */
  public Map<String, Module> getModules() {
    return modules;
  }

  /**
   * Set the module.
   *
   * @param modules modules
   */
  public void setModules(Map<String, Module> modules) {
    this.modules = modules;
  }

  /**
   * Returns the main name.
   *
   * @return main name
   */
  public String getMainName() {
    return mainName;
  }

  /**
   * Start the main block.
   *
   * @param main_name main name
   */
  public void init_main(String main_name) {
    currentUnit = module(NO_MODULE);
    module(NO_MODULE).set_child("program", main_name);
    mainName = new String(main_name);
    currentUnit = currentUnit.get_child(main_name);
    currentUnit.set_mother(module(NO_MODULE));
  }

  /** Exit the main block. */
  public void end_main() {
    currentUnit = currentUnit.get_mother();
  }

  /**
   * Start the module block.
   *
   * @param module_name Module name
   */
  public void init_module(String module_name) {
    Module module = new Module(module_name);
    modules.put(module_name, module);
    currentUnit = module;
  }

  /** Exit the module block. */
  public void end_module() {
    currentUnit = module(NO_MODULE);
  }

  /**
   * Returns the module with the specified name.
   *
   * @param module_name Module name
   * @return module class
   */
  public Module module(String module_name) {
    return modules.get(module_name);
  }

  /**
   * Returns an array of module names.
   *
   * @return Array of module names
   */
  public String[] get_module_name() {
    return modules.keySet().toArray(new String[modules.size()]);
  }

  /**
   * Start a procedure block.
   *
   * @param type_name Procedure type
   * @param sub_name Procedure name
   */
  protected void init_procedure(String type_name, String sub_name) {
    Procedure sub = new Procedure(type_name, sub_name);
    ProgramUnit mama = currentUnit;
    currentUnit.put_child(sub);
    currentUnit = currentUnit.get_child(sub_name);
    currentUnit.set_mother(mama);
  }

  /**
   * Start a procedure block.
   *
   * @param type_name Procedure type
   * @param sub_name Procedure name
   * @param args Procedure formal parameters
   */
  protected void init_procedure(String type_name, String sub_name, String[] args) {
    Procedure sub = new Procedure(type_name, sub_name, args);
    ProgramUnit mama = currentUnit;
    currentUnit.put_child(sub);
    currentUnit = currentUnit.get_child(sub_name);
    currentUnit.set_mother(mama);
  }

  /** Exit the procedure block. */
  protected void end_procedure() {
    currentUnit = currentUnit.get_mother();
  }

  /**
   * Add attributes.
   *
   * @param attribute_name String representation of the attribute
   */
  public void put_attribute(String attribute_name) {
    currentUnit.put_attribute(attribute_name);
  }

  /**
   * Add a USE statement.
   *
   * @param useline USE statement
   */
  public void setUse(UseState useline) {
    currentUnit.addUse(useline);
  }
  /**
   * Add a USE statement.
   *
   * @param useline USE statement
   * @param lineInfo code Line information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setUse(UseState useline, CodeLine lineInfo, String label) {
    currentUnit.addUse(useline);
    useline.set_block_start(lineInfo);
    useline.set_block_end(lineInfo);
    useline.get_start().set_label(label);
  }

  /**
   * Set the start line of branch processing.
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, set Statement.NO_LABEL.
   * @param type Selection type
   */
  public void startSelection(
      CodeLine lineInfo, String label, jp.riken.kscope.language.Selection.SelectionType type) {
    ((Procedure) currentUnit).start_selection(lineInfo, label, type);
  }

  /**
   * Start a processing block that represents a branch. Corresponds to else if statement, else
   * statement, and case statement.
   *
   * @param cond Conditional expression
   * @param lineInfo Line information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void startCondition(Expression cond, CodeLine lineInfo, String label) {
    if (label == null) {
      label = Statement.NO_LABEL;
    }
    ((Procedure) currentUnit).start_condition(cond, lineInfo, label);
    Block blk = this.getCurrentBlock();
    if (cond != null) {
      if (blk.get_mother() instanceof Selection) {
        Selection selec = (Selection) blk.get_mother();
        if (selec.getConditions().size() == 1) {
          this.currentUnit.addExpressionToRef(selec, cond);
        } else {
          this.currentUnit.addExpressionToRef(blk, cond);
        }
      }
    }
  }

  /**
   * Terminate the processing block that represents the branch. Corresponds to else if statement,
   * else statement, and case statement.
   *
   * @param lineInfo Line information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void endCondition(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).end_condition(lineInfo, label);
  }

  /**
   * Set a conditional expression in the branch processing (select case statement only). It is not
   * set in blocks other than Selection. Not used except in the Select case statement.
   *
   * @param exp conditional expression
   */
  public void setSelectCaseCondition(Expression exp) {
    IBlock blk = this.getCurrentBlock();
    if (blk != null) {
      if (blk instanceof Selection) {
        Selection sel = (Selection) blk;
        if (sel.isSelect()) {
          sel.setCaseCondition(exp);
          this.currentUnit.addExpressionToRef(sel, exp);
        }
      }
    }
  }

  /**
   * Set the end line of the branch.
   *
   * @param lineInfo Code line information
   */
  public void end_selection(CodeLine lineInfo) {
    ((Procedure) currentUnit).end_selection(lineInfo, Statement.NO_LABEL);
  }

  /**
   * Set the end line of the branch statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_selection(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).end_selection(lineInfo, label);
  }

  /**
   * Start a user-defined processing block.
   *
   * @param lineInfo Code line information
   */
  protected void start_user_defined(CodeLine lineInfo) {
    ((Procedure) currentUnit).start_user_defined(lineInfo);
  }

  /**
   * Start a user-defined processing block.
   *
   * @param lineInfo Code line information
   * @param label Code label
   */
  protected void start_user_defined(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).start_user_defined(lineInfo, label);
  }

  /**
   * Exit the user-defined processing block.
   *
   * @param lineInfo Code line information
   */
  protected void end_user_defined(CodeLine lineInfo) {
    ((Procedure) currentUnit).end_user_defined(lineInfo);
  }

  /**
   * Exit the user-defined processing block.
   *
   * @param lineInfo Code line information
   * @param label Code label
   */
  protected void end_user_defined(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).end_user_defined(lineInfo, label);
  }

  /**
   * Set the procedure call.
   *
   * @param lineInfo Code line information
   * @param label Row label
   * @param subroutineName CALL subroutine name
   * @param arguments Argument list
   * @param intrinsic Embedded function flag: true = Embedded function
   */
  protected void setProcedureUsage(
      CodeLine lineInfo,
      String label,
      String subroutineName,
      List<Expression> arguments,
      boolean intrinsic) {
    this.start_procedure_usage(lineInfo, label, subroutineName, arguments);
    IBlock block = this.getCurrentBlock();
    if (block instanceof ProcedureUsage && intrinsic == true) {
      ((ProcedureUsage) block).setIntrinsic();
    }
    this.end_procedure_usage(lineInfo, label);

    // Set references for analytics
    if (arguments != null) {
      for (Expression arg : arguments) {
        Set<Variable> vars = arg.getAllVariables();
        for (Variable var : vars) {
          ((Procedure) this.currentUnit).putRefVariableName(var.getName(), block);
          ((Procedure) this.currentUnit).putVariableMap(var.getName());
        }
      }
    }
  }

  /**
   * Set the start of the procedure call.
   *
   * @param lineInfo Code line information
   * @param label Row label
   * @param subroutineName CALL subroutine name
   * @param arguments Argument list
   */
  protected void start_procedure_usage(
      CodeLine lineInfo, String label, String subroutineName, List<Expression> arguments) {
    ((Procedure) currentUnit).start_procedure_usage(lineInfo, label, subroutineName, arguments);
  }

  /**
   * Set the end of the procedure call.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_procedure_usage(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).end_procedure_usage(lineInfo, label);
  }

  /**
   * Set the start line of the DO statement. (With label)
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void start_repetition(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).start_repetition(lineInfo, label);
  }

  /**
   * Set the start line of the DO statement.
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, set Statement.NO_LABEL.
   * @param iterator Loop control variable. Null if not
   * @param initIterator Open price. Null if not
   * @param endCondition Closing price. Null if not
   * @param step Step width. Null if not
   */
  protected void start_repetition(
      CodeLine lineInfo,
      String label,
      Variable iterator,
      Expression initIterator,
      Expression endCondition,
      Expression step) {

    Procedure aCurrentUnit = (Procedure) this.currentUnit;

    aCurrentUnit.start_repetition(lineInfo, label);
    Repetition aCurrentBlock = (Repetition) aCurrentUnit.getBody().getCurrentBlock();
    aCurrentBlock.setProperty(iterator, initIterator, endCondition, step);

    // Set references for analytics
    if (iterator != null) {
      aCurrentUnit.putVariableMap(iterator.getName());
      aCurrentUnit.putDefVariableName(iterator.getName(), aCurrentBlock);
    }

    if (initIterator != null) {
      aCurrentUnit.addExpressionToRef(aCurrentBlock, initIterator);
    }
    if (endCondition != null) {
      aCurrentUnit.addExpressionToRef(aCurrentBlock, endCondition);
    }
    if (step != null) {
      aCurrentUnit.addExpressionToRef(aCurrentBlock, step);
    }
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   */
  protected void end_repetition(CodeLine lineInfo) {
    ((Procedure) currentUnit).end_repetition(lineInfo);
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_repetition(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).end_repetition(lineInfo, label);
  }

  /**
   * Set the CONTINUE statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void set_continue(CodeLine lineInfo, String label) {
    ((Procedure) currentUnit).set_continue(lineInfo, label);
  }

  /**
   * Set a variable declaration statement.
   *
   * @param var_def Variable declaration statement
   */
  public void set_variable_def(VariableDefinition var_def) {
    currentUnit.set_variable_def(var_def);
  }

  /**
   * Get the block currently stored in the database.
   *
   * @return Currently blocked
   */
  public Block get_current_block() {
    if (this.currentUnit == null) {
      return null;
    }
    if (!(this.currentUnit instanceof Procedure)) {
      return null;
    }
    if (((Procedure) currentUnit).getBody() == null) {
      return null;
    }

    return ((Procedure) currentUnit).getBody().getCurrentBlock();
  }

  /**
   * Get the ProgramUnit currently stored in the database.
   *
   * @return Currently ProgramUnit
   */
  public ProgramUnit get_current_unit() {
    return this.currentUnit;
  }

  /**
   * Get the current block of the current unit.
   *
   * @return Current block. Returns null if the current unit is not a subroutine.
   */
  private Block getCurrentBlock() {
    if (currentUnit instanceof Procedure) {
      return ((Procedure) currentUnit).getCurrentBlock();
    } else {
      return null;
    }
  }

  // A set method group of classes described in one logical line in the class that inherits the
  // Block class

  /**
   * Set an assignment statement.
   *
   * @param left Left side
   * @param right Right side
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setSubstitution(Variable left, Expression right, CodeLine lineInfo, String label) {
    Block block = this.getCurrentBlock();
    Substitution blk = new Substitution(block);
    if (left.isArrayExpression() || right.isArrayExpression()) {
      blk = new ArrayExpression(block);
    }
    blk.setLeft(left);
    blk.set_block_start(lineInfo);
    blk.get_start().set_label(label);
    blk.set_block_end(lineInfo);
    blk.get_end().set_label(label);
    blk.setRight(right);
    block.add_child(blk);

    // Set references for analytics
    ((Procedure) this.currentUnit).putDefVariableName(left.getName(), blk);
    ((Procedure) this.currentUnit).putVariableMap(left.getName());
    this.currentUnit.addExpressionToRef(blk, right);
  }

  /**
   * Set a block that has no child elements in the current block.
   *
   * @param blk Block
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  private void setBlockToCurrent(Block blk, CodeLine lineInfo, String label) {
    Block block = this.getCurrentBlock();
    block.add_child(blk);
    blk.set_block_start(lineInfo);
    blk.get_start().set_label(label);
    blk.set_block_end(lineInfo);
    blk.get_end().set_label(label);
  }

  /**
   * Set the Exit statement.
   *
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setExit(CodeLine lineInfo, String label) {
    Break blk = new Break();
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Return statement.
   *
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setReturn(CodeLine lineInfo, String label) {
    Return blk = new Return();
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Cycle statement.
   *
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setCycle(CodeLine lineInfo, String label) {
    Continue blk = new Continue();
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Stop statement.
   *
   * @param arg Argument
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setStop(String arg, CodeLine lineInfo, String label) {
    Termination blk = new Termination();
    blk.setArgument(arg);
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Continue statement.
   *
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setContinue(CodeLine lineInfo, String label) {
    DoNothing blk = new DoNothing();
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set a Pause statement.
   *
   * @param arg Argument
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setPause(String arg, CodeLine lineInfo, String label) {
    Pause blk = new Pause();
    blk.setArgument(arg);
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set a GoTo statement.
   *
   * @param arg Argument
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setGoTo(String arg, CodeLine lineInfo, String label) {
    GoTo blk = new GoTo();
    blk.setArgument(arg);
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set directives.
   *
   * @param arg argument
   * @param lineInfo Line information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setDirective(String arg, CodeLine lineInfo, String label) {
    Directive blk = new Directive();
    blk.setArgument(arg);
    if (this.getCurrentBlock() != null) {
      setBlockToCurrent(blk, lineInfo, label);
    } else {
      ProgramUnit pu = this.get_current_unit();
      pu.addDirective(blk);
    }
  }

  /**
   * Set the Nullify statement.
   *
   * @param var argument
   * @param lineInfo Line information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setNullify(List<Variable> var, CodeLine lineInfo, String label) {
    DynamicNullification blk = new DynamicNullification();
    blk.setTarget(var);
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Allocate statement.
   *
   * @param trgt Target
   * @param err Error variable expression. If not, set null.
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setAllocate(
      Map<Variable, VariableDimension> trgt, Variable err, CodeLine lineInfo, String label) {
    DynamicAllocation blk = new DynamicAllocation();
    blk.setTarget(trgt);
    blk.setError(err);
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Deallocate statement.
   *
   * @param trgt Target
   * @param err Error variable expression. If not, set null.
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setDeallocate(List<Variable> trgt, Variable err, CodeLine lineInfo, String label) {
    DynamicDeallocation blk = new DynamicDeallocation();
    blk.setTarget(trgt);
    blk.setError(err);
    setBlockToCurrent(blk, lineInfo, label);
  }

  /**
   * Set the Data statement to the current unit.
   *
   * @param blk Data class
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setData(Data blk, CodeLine lineInfo, String label) {
    this.currentUnit.addData(blk);
    blk.set_block_start(lineInfo);
    blk.get_start().set_label(label);
    blk.set_block_end(lineInfo);
    blk.get_end().set_label(label);
  }

  /**
   * Set the Equivalence statement to the current unit.
   *
   * @param blk Equivalence class
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setEquivalence(Equivalence blk, CodeLine lineInfo, String label) {
    this.currentUnit.addEquivalence(blk);
    blk.set_block_start(lineInfo);
    blk.get_start().set_label(label);
    blk.set_block_end(lineInfo);
    blk.get_end().set_label(label);
  }

  /**
   * Set the Common statement to the current unit.
   *
   * @param blk Common class
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setCommon(Common blk, CodeLine lineInfo, String label) {
    this.currentUnit.addCommon(blk);
    blk.set_block_start(lineInfo);
    blk.get_start().set_label(label);
    blk.set_block_end(lineInfo);
    blk.get_end().set_label(label);

    if (this.commonMap == null) {
      this.commonMap = new HashMap<String, List<ProgramUnit>>();
    }
    if (commonMap.containsKey(blk.getName())) {
      List<ProgramUnit> prList = commonMap.get(blk.getName());
      prList.add(this.currentUnit);
    } else {
      List<ProgramUnit> prList = new ArrayList<ProgramUnit>();
      prList.add(this.currentUnit);
      this.commonMap.put(blk.getName(), prList);
    }
  }

  /**
   * Set the Interface statement to the current unit.
   *
   * @param blk Interface class
   * @param lineInfo Row information
   * @param label Label. If not, set Statement.NO_LABEL.
   */
  public void setInterface(Procedures blk, CodeLine lineInfo, String label) {
    this.currentUnit.addInterface(blk);
    blk.set_block_start(lineInfo);
    blk.get_start().set_label(label);
    blk.set_block_end(lineInfo);
    blk.get_end().set_label(label);
  }
  // ----------------------------------------
  /** Set the private attribute to the current unit. */
  public void setPrivateToCurrentUnit() {
    if (!(currentUnit instanceof Procedure)) return;
    ((Procedure) (this.currentUnit)).setPrivate();
  }

  /** Set the public attribute to the current unit. */
  public void setPublicToCurrentUnit() {
    if (!(currentUnit instanceof Procedure)) return;
    ((Procedure) (this.currentUnit)).setPublic();
  }

  // ----------------------------------------
  /**
   * Register the TYPE declaration.
   *
   * @param tp TYPE declaration
   */
  public void addTypeDefinition(Type tp) {
    this.currentUnit.addTypeDefinition(tp);
    List<VariableDefinition> vars = tp.getDefinitions();
    if (vars != null) {
      for (VariableDefinition var : vars) {
        var.setMother(this.currentUnit);
      }
    }
  }

  /**
   * Add an interface block.
   *
   * @param blk interface block
   */
  public void addInterface(Procedures blk) {
    currentUnit.addInterface(blk);
  }

  /**
   * Acquisition of all information blocks specified in the area.
   *
   * @return Area Specified information block collection
   */
  public InformationBlocks getInformationBlocks() {
    return this.informationBlocks;
  }
  /**
   * A set of all information blocks specified in the area.
   *
   * @param blks Area A set of specified information blocks
   */
  public void setInformationBlocks(InformationBlocks blks) {
    this.informationBlocks = blks;
  }

  // ----------------------------------------
  // --------------Information---------------
  // ----------------------------------------
  /**
   * Acquisition of all information blocks including child programs.
   *
   * @return Information block collection
   */
  public InformationBlocks getInformationBlocksAll() {
    InformationBlocks result = new InformationBlocks();
    result.addAll(this.informationBlocks);
    if (this.modules != null) {
      for (Module module : this.modules.values()) {
        result.addAll(module.createInformationBlocks());
      }
    }
    return result;
  }

  /**
   * Set the return type to the current unit.
   *
   * @param tp Return type
   */
  public void setReturnValueType(IVariableType tp) {
    ProgramUnit pu = this.currentUnit;
    if (pu instanceof Procedure) {
      ((Procedure) pu).setReturnValueType(tp);
    }
  }

  /**
   * Set the name of the result in the current unit.
   *
   * @param st Result name
   */
  public void setResult(String st) {
    ProgramUnit pu = this.currentUnit;
    if (pu instanceof Procedure) {
      ((Procedure) pu).setResult(st);
    }
  }

  /**
   * Add the name and data type of the function declared external to the current unit.
   *
   * @param name Function name
   * @param tp data type
   */
  public void addExternalFunction(String name, IVariableType tp) {
    this.currentUnit.addExternalFunctionList(name, tp);
  }

  /**
   * Returns a COMMON map.
   *
   * @return COMMON map
   */
  public Map<String, List<ProgramUnit>> getCommonMap() {
    return commonMap;
  }

  /**
   * Returns a list of program units declaring the specified Common name.
   *
   * @param nm Common name
   * @return A list of program units. If not, returns an empty list.
   */
  public List<ProgramUnit> getCommonUnit(String nm) {
    if (this.commonMap == null) {
      return new ArrayList<ProgramUnit>();
    }
    if (this.commonMap.get(nm) == null) {
      return new ArrayList<ProgramUnit>();
    }
    return this.commonMap.get(nm);
  }

  /**
   * Returns a list of program units contained in the specified file.
   *
   * @param file File
   * @return A list of program units. If not, returns an empty list.
   */
  public List<ProgramUnit> getProgramUnits(SourceFile file) {
    if (file == null) return null;
    ArrayList<ProgramUnit> units = new ArrayList<ProgramUnit>();

    Collection<Module> mods = this.modules.values();
    for (Module mod : mods) {
      Collection<Procedure> procs = mod.getChildren();
      for (Procedure proc : procs) {
        SourceFile procFile = proc.getStartCodeLine().getSourceFile();
        if (file.equals(procFile)) {
          units.add(proc);
        }
      }
    }
    return units;
  }
  /**
   * Searches the block with the specified start and end from the list of multiple block additional
   * information, and returns the corresponding additional information area.
   *
   * @param start Start block
   * @param end End block
   * @return Additional information area. If not, a new additional information area is created and
   *     returned.
   */
  public IInformation getInformation(IInformation start, IInformation end) {
    InformationBlock block = this.informationBlocks.findObjectBy(start, end);
    if (block != null) {
      return block;
    }
    // If not, create a new one and add
    TextInfo newInfo = new TextInfo();
    InformationBlock bk = new InformationBlock(newInfo, start, end);
    this.informationBlocks.add(bk);
    return bk;
  }
  /**
   * Searches the block starting from the specified infoNode from the list of multiple block
   * additional information, and returns the list of the corresponding additional information area.
   * If the specified infoNode is the main, return the entire list.
   *
   * @param infoNode block
   * @return A list of additional information areas. If not, returns an empty list.
   */
  public List<InformationBlock> getInformation(IInformation infoNode) {
    if (infoNode instanceof Procedure) {
      Procedure proc = (Procedure) infoNode;
      if (proc.get_name().equalsIgnoreCase(this.mainName)) {
        return this.informationBlocks;
      }
    }
    return this.informationBlocks.getStartWith(infoNode);
  }

  /**
   * Set the main name
   *
   * @param nm main name
   */
  public void setMainName(String nm) {
    mainName = nm;
  }

  /**
   * Add a module
   *
   * @param pu module
   */
  public void addModule(ProgramUnit pu) {
    if (pu instanceof Module) {
      this.modules.put(pu.get_name(), (Module) pu);
    }
  }

  /**
   * Set the Common statement in commonMap.
   *
   * @param key Common statement block name
   * @param unit Common statement declaration module, subroutine
   */
  public void addCommonMap(String key, ProgramUnit unit) {
    if (this.commonMap == null) {
      this.commonMap = new HashMap<String, List<ProgramUnit>>();
    }
    if (commonMap.containsKey(key)) {
      List<ProgramUnit> prList = commonMap.get(key);
      prList.add(unit);
    } else {
      List<ProgramUnit> prList = new ArrayList<ProgramUnit>();
      prList.add(unit);
      this.commonMap.put(key, prList);
    }
  }

  /**
   * Make a shallow copy.
   *
   * @param program Copy source database
   */
  public void copyShallow(Program program) {
    this.mainName = program.mainName;
    this.modules = program.modules;
    this.commonMap = program.commonMap;
    this.informationBlocks = program.informationBlocks;
  }

  /**
   * Get the ProgramUnit currently stored in the database.
   *
   * @return ProgramUnit currently stored
   */
  public ProgramUnit getCurrentUnit() {
    return this.currentUnit;
  }

  /**
   * Get the main program.
   *
   * @return Main program
   */
  public Procedure getMainProgram() {
    return getProcedureByName(NO_MODULE, this.mainName);
  }

  /**
   * Get the procedure. <br>
   * Search for modules from NO_MODULE.
   *
   * @param procudurename Procedure name
   * @return procedure
   */
  public Procedure getProcedure(String procudurename) {
    return getProcedureByName(null, procudurename);
  }

  /**
   * Get the procedure. <br>
   * If the module name is null, search from NO_MODULE.
   *
   * @param modulename Module name
   * @param procudurename Procedure name
   * @return procedure
   */
  public Procedure getProcedureByName(String modulename, String procudurename) {
    if (procudurename == null) return null;
    Module module = null;
    if (modulename == null) {
      module = this.module(NO_MODULE);
    } else {
      module = this.module(modulename);
    }
    if (module == null) return null;
    ProgramUnit proc = module.get_child(procudurename);
    if (proc instanceof Procedure) {
      return (Procedure) proc;
    }
    return null;
  }

  /**
   * Get the procedure. <br>
   * If the parent module is null, search from NO_MODULE.
   *
   * @param parent parent module
   * @param procudurename Procedure name
   * @return procedure
   */
  public Procedure getProcedure(ProgramUnit parent, String procudurename) {
    ProgramUnit parentUnit = parent;
    if (parentUnit == null) {
      parentUnit = this.module(NO_MODULE);
    }
    ProgramUnit proc = parentUnit.get_child(procudurename);
    if (proc instanceof Procedure) {
      return (Procedure) proc;
    }
    return null;
  }
}
