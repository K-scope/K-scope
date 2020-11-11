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
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;

/**
 * A class that represents a procedure. Corresponds to subroutine, function, entry in Fortran.
 *
 * @author RIKEN
 */
public class Procedure extends ProgramUnit {
  /** Serial number */
  private static final long serialVersionUID = -6409164910117026406L;
  /** Formal argument. */
  private Variable[] arguments;
  /**
   * A list of program units calling this procedure. Member variables for analytical functions.
   *
   * @deprecated unused
   */
  @Deprecated private ArrayList<Procedure> parents = new ArrayList<Procedure>();
  /** A member that represents the data type of the return value. */
  private IVariableType returnValueType = new VariableType(PrimitiveDataType.VOID);
  /** List of CALL statements calling this procedure */
  private transient Set<ProcedureUsage> calls = new LinkedHashSet<ProcedureUsage>();
  /** A member that holds the name of the return value. */
  private String result;
  /** A member variable that represents the execution statement of a procedure. */
  private ExecutableBody body = new ExecutableBody(this);
  /** scope attribute: private, public */
  private ScopeAttribute scope = ScopeAttribute.NONE;

  /** @return returnValueType */
  public IVariableType getReturnValueType() {
    return returnValueType;
  }

  /**
   * Set the data type.
   *
   * @param tp Data type
   */
  public void setReturnValueType(IVariableType tp) {
    this.returnValueType = tp;
  }

  /**
   * Returns the name of the return value.
   *
   * @return result Return name
   */
  public String getResult() {
    return result;
  }

  /**
   * Set the name of the return value.
   *
   * @param res Return name
   */
  public void setResult(String res) {
    this.result = res;
  }

  /**
   * Returns its own processing block.
   *
   * @return processing block
   */
  public ExecutableBody getBody() {
    return this.body;
  }

  /** Set the Public attribute. */
  public void setPublic() {
    scope = ScopeAttribute.PUBLIC;
  }

  /** Set the Private attribute. */
  public void setPrivate() {
    scope = ScopeAttribute.PRIVATE;
  }
  /**
   * Get block type.
   *
   * @return BlockType.PROCEDURE
   */
  @Override
  public BlockType getBlockType() {
    return BlockType.PROCEDURE;
  }

  @Override
  protected String toStringBase() {
    return (this.get_type() + " " + this.get_name());
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Constructor.
   *
   * @param type_name Type of procedure
   * @param sub_name Name of procedure
   */
  public Procedure(String type_name, String sub_name) {
    super(type_name, sub_name);
  }

  /**
   * Constructor.
   *
   * @param type_name Type of procedure
   * @param sub_name Name of procedure
   * @param args Array of formal arguments
   */
  public Procedure(String type_name, String sub_name, String[] args) {
    super(type_name, sub_name);
    arguments = new Variable[args.length];
    for (int i = 0; i < arguments.length; i++) {
      arguments[i] = new Variable(args[i]);
    }
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * List of program units calling this procedure
   *
   * @param parent The program calling this procedure
   * @deprecated unused
   */
  @Deprecated
  protected void add_parent(Procedure parent) {
    parents.add(parent);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Returns the formal parameters in the specified order. Returns null if it does not exist.
   *
   * @param i Argument order
   * @return Formal argument.
   */
  public Variable getArgument(int i) {
    if (arguments.length > i) {
      return arguments[i];
    }
    return null;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Returns an array of formal arguments.
   *
   * @return An array of formal arguments.
   */
  public Variable[] get_args() {
    return arguments;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected ExecutableBody get_body() {
    return (body);
  }

  /**
   * Get a list of program units calling this procedure.
   *
   * @return List of program units calling this procedure
   * @deprecated unused
   */
  @Deprecated
  protected ArrayList<Procedure> get_parents() {
    return (parents);
  }

  // ++++++++++++++++++++++++++++++++++++++++++//
  // interface method //
  // ++++++++++++++++++++++++++++++++++++++++++//

  // --------------Variable_def----------------//

  /*
   * protected void new_variable_def(String var_name) { Variable_def_class
   * var_def = new Variable_def_class(var_name) ; this.put_variable(var_def) ;
   * }
   */

  // ----------------Selection-----------------//
  /**
   * Conditional expression setting.
   *
   * @param cond Conditional expression
   */
  protected void start_condition(Expression cond, CodeLine lineInfo, String label) {
    body.start_condition(cond, lineInfo, label);
  }
  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void end_condition(CodeLine lineInfo, String label) {
    body.end_condition(lineInfo, label);
  }

  // ---------------User_defined---------------//
  protected void start_user_defined(CodeLine lineInfo) {
    body.start_user_defined(lineInfo);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void start_user_defined(CodeLine lineInfo, String label) {
    body.start_user_defined(lineInfo, label);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void end_user_defined(CodeLine lineInfo) {
    body.end_user_defined(lineInfo);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void end_user_defined(CodeLine lineInfo, String label) {
    body.end_user_defined(lineInfo, label);
  }

  // --------------Procedure_usage-------------//
  // ++++++++++++++++++++++++++++++++++++++++++++

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void add_procedure_usage(ProcedureUsage sub_call) {
    body.add_procedure_usage(sub_call);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Returns a list of procedure calls that belong to you.
   *
   * @return A list of procedure calls. If not, returns an empty list.
   */
  public List<ProcedureUsage> getCalls() {
    return body.getCalls();
  }

  // ++++++++++++++++++++++++++++++++++++++++++//
  // output method //
  // ++++++++++++++++++++++++++++++++++++++++++//

  /**
   * Set the start line of the CALL, FUNCTION statement.
   *
   * @param lineInfo Code line information
   */
  protected void start_procedure_usage(CodeLine lineInfo) {
    body.start_procedure_usage(lineInfo);
  }

  /**
   * Set the start line of the CALL, FUNCTION statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void start_procedure_usage(CodeLine lineInfo, String label) {
    body.start_procedure_usage(lineInfo, label);
  }

  /**
   * Set the start line of the CALL, FUNCTION statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   * @param subroutineName CALL subroutine name
   * @param arguments Argument list
   */
  protected void start_procedure_usage(
      CodeLine lineInfo, String label, String subroutineName, List<Expression> arguments) {
    body.start_procedure_usage(lineInfo, label, subroutineName, arguments);
  }

  /**
   * Set the end line of the CALL and FUNCTION statements.
   *
   * @param lineInfo Code line information
   */
  protected void end_procedure_usage(CodeLine lineInfo) {
    body.end_procedure_usage(lineInfo);
  }

  /**
   * Set the end line of the CALL and FUNCTION statements.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_procedure_usage(CodeLine lineInfo, String label) {
    body.end_procedure_usage(lineInfo, label);
  }

  /**
   * Set the start line of the DO statement.
   *
   * @param lineInfo Code line information
   */
  protected void start_repetition(CodeLine lineInfo) {
    body.start_repetition(lineInfo);
  }

  /**
   * Set the start line of the DO statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void start_repetition(CodeLine lineInfo, String label) {
    body.start_repetition(lineInfo, label);
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   */
  protected void end_repetition(CodeLine lineInfo) {
    body.end_repetition(lineInfo);
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_repetition(CodeLine lineInfo, String label) {
    body.end_repetition(lineInfo, label);
  }

  /**
   * Set the CONTINUE statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void set_continue(CodeLine lineInfo, String label) {
    body.set_continue(lineInfo, label);
  }

  /**
   * Set the start line of the SELECT statement. (With label)
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void start_selection(
      CodeLine lineInfo, String label, jp.riken.kscope.language.Selection.SelectionType type) {
    body.start_selection(lineInfo, label, type);
  }

  /**
   * Set the end line of the SELECT statement. (With label)
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_selection(CodeLine lineInfo, String label) {
    body.end_selection(lineInfo, label);
  }

  /**
   * Set a RETURN statement
   *
   * @param lineInfo Code line information
   */
  public void setReturn(CodeLine lineInfo) {
    body.setReturn(lineInfo);
  }
  /**
   * Whether the actual argument list matches. <br>
   * When searching for the corresponding function from the overloaded function group, <br>
   * It is necessary to check the type of formal and actual arguments. <br>
   * "Matching" means that this type check determines that the type is the same. <br>
   *
   * @param actualArguments
   * @return true: Conforms <br>
   *     false: Not compatible
   */
  public boolean matches(List<Expression> actualArguments) {
    if (actualArguments == null || this.arguments == null) {
      return false;
    }
    if (actualArguments.size() > this.arguments.length) {
      return false;
    }

    for (int i = 0; i < this.arguments.length; i++) {
      if (i < actualArguments.size()) {
        VariableDefinition def = this.getVariableMap(this.arguments[i].getName());
        if (def != null) {
          if (!def.matches(actualArguments.get(i))) {
            return false;
          }
        }
      } else {
        if (!this.getVariableMap(this.arguments[i].getName()).getAttribute().contains("optional")) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Returns the current block.
   *
   * @return Current block
   */
  public Block getCurrentBlock() {
    return body.getCurrentBlock();
  }

  /** @param blk */
  public void setCurrentBlock(Block blk) {
    body.setCurrentBlock(blk);
  }

  /**
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
   */
  @Override
  public IInformation findInformationBlockBy(String id) {
    IInformation infoBlock = super.findInformationBlockBy(id);

    if (infoBlock == null) {
      infoBlock = body.findInformationBlockBy(id);
    }

    return infoBlock;
  }

  /**
   * Returns a set of blocks in which a variable is referenced / defined within a program unit.
   *
   * @param name Variable name
   *     <p>A set of @return blocks. If not, it returns an empty set.
   */
  public Set<IBlock> getRefDefBlocks(String name) {
    String nm = name.toLowerCase();
    Set<IBlock> blocks = new LinkedHashSet<IBlock>();
    Set<IBlock> refblocks = this.getRefVariableNames().get(nm);
    Set<IBlock> defblocks = this.getDefVariableNames().get(nm);
    if (refblocks == null) {
      if (defblocks == null) {
        return new LinkedHashSet<IBlock>();
      }
      return defblocks;
    } else {
      if (defblocks == null) {
        return refblocks;
      }
      // When both ref and def have elements
      IBlock[] refArray = refblocks.toArray(new IBlock[0]);
      IBlock[] defArray = defblocks.toArray(new IBlock[0]);
      boolean refFlag = true;
      boolean defFlag = true;
      int refIndex = 0;
      int defIndex = 0;
      IBlock currentRef = refArray[0];
      IBlock currentDef = defArray[0];
      int refLine = currentRef.getStartCodeLine().getStartLine();
      int defLine = currentDef.getStartCodeLine().getStartLine();
      while (refFlag || defFlag) {
        if (refLine < defLine) {
          blocks.add(currentRef);
          if (refIndex == refArray.length - 1) {
            refFlag = false;
            refLine = defArray[defArray.length - 1].getStartCodeLine().getStartLine();
          } else {
            refIndex++;
            currentRef = refArray[refIndex];
            refLine = currentRef.getStartCodeLine().getStartLine();
          }
        } else {
          blocks.add(currentDef);
          if (defIndex == defArray.length - 1) {
            defFlag = false;
            defLine = 1 + refArray[refArray.length - 1].getStartCodeLine().getStartLine();
          } else {
            defIndex++;
            currentDef = defArray[defIndex];
            defLine = currentDef.getStartCodeLine().getStartLine();
          }
        }
      }
    }
    return blocks;
  }

  /**
   * Get the scope attribute.
   *
   * @return scope attribute
   */
  public ScopeAttribute getScope() {
    return this.scope;
  }

  /**
   * If the formal argument of the specified variable name is included, the order is returned.
   *
   * @param dummyArg Variable name
   * @return The order of formal parameters. Returns -1 if it does not contain the specified
   *     variable.
   */
  public int getNumOfDummyArgument(String dummyArg) {
    Variable[] args = this.arguments;
    if (args != null) {
      for (int i = 0; i < args.length; i++) {
        if (args[i].getName().equalsIgnoreCase(dummyArg)) {
          return i;
        }
      }
    }
    return -1;
  }

  /**
   * Returns a list of ProcedureUsage classes calling this procedure. A list of @return Procedure
   * Usage. If not, returns an empty list.
   */
  public Set<ProcedureUsage> getCallMember() {
    if (this.calls == null) {
      return new LinkedHashSet<ProcedureUsage>();
    }
    return this.calls;
  }

  /**
   * Add the ProcedureUsage class that calls this procedure.
   *
   * @param pu ProcedureUsage class
   */
  public void addCallMember(ProcedureUsage pu) {
    if (this.calls == null) {
      this.calls = new LinkedHashSet<ProcedureUsage>();
    }
    this.calls.add(pu);
  }

  /**
   * Set the ProcedureUsage class list that calls this procedure.
   *
   * @param list Procedure Usage list
   */
  public void setCallMember(Set<ProcedureUsage> list) {
    this.calls = list;
  }

  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  @Override
  public InformationBlocks createInformationBlocks() {
    InformationBlocks informationBlocks = new InformationBlocks();
    informationBlocks.addAll(super.createInformationBlocks());
    informationBlocks.addAll(this.body.createInformationBlocks());
    return informationBlocks;
  }

  /**
   * Check if they are the same procedure. Do not check the arguments.
   *
   * @param proc subroutine, function
   * @return true = match
   */
  public boolean equalsBlocks(Procedure proc) {
    if (proc == null) return false;

    if (!super.equalsBlocks(proc)) {
      return false;
    }

    if (this.getBody() == null && proc.getBody() == null) {
      return true;
    } else if (this.getBody() == null) {
      return false;
    }

    if (!this.getBody().equalsBlocks(proc.getBody())) {
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

    // Variable declaration statement, subprogram
    {
      IInformation[] infos = super.searchInformationBlocks(block);
      if (infos != null) {
        list.addAll(Arrays.asList(infos));
      }
    }
    if (this.body != null) {
      IInformation[] infos = body.searchInformationBlocks(block);
      if (infos != null) {
        list.addAll(Arrays.asList(infos));
      }
    }
    if (list.size() <= 0) {
      return null;
    }

    return list.toArray(new IInformation[0]);
  }

  /**
   * Check if they are in the same block hierarchy.
   *
   * @param proc Check target Procedure
   * @return true = match
   */
  @Override
  public boolean equalsLayout(ProgramUnit proc) {
    if (proc == null) return false;
    if (!(proc instanceof Procedure)) return false;

    if (!super.equalsLayout(proc)) {
      return false;
    }

    if (this.getBody() == null && ((Procedure) proc).getBody() == null) {
      return true;
    } else if (this.getBody() == null) {
      return false;
    }

    if (!this.getBody().equalsLayout(((Procedure) proc).getBody())) {
      return false;
    }

    return true;
  }

  /**
   * Search for structural blocks that match the layoutID.
   *
   * @param id layoutID
   * @return Found structural block
   */
  @Override
  public IInformation findInformationLayoutID(String id) {
    if (id == null || id.isEmpty()) return null;
    IInformation infoBlock = super.findInformationLayoutID(id);
    if (infoBlock == null) {
      infoBlock = body.findInformationLayoutID(id);
    }

    return infoBlock;
  }

  /**
   * Search for blocks of line numbers
   *
   * @param line line number
   * @return Line number block
   */
  @Override
  public IBlock[] searchCodeLine(CodeLine line) {
    if (line == null) return null;

    List<IBlock> list = new ArrayList<IBlock>();
    IBlock addblock = null;
    CodeLine thisstart = this.getStartCodeLine();
    CodeLine thisend = this.getEndCodeLine();
    if (line.isOverlap(thisstart, thisend)) {
      addblock = this;
    }
    // Variable declaration statement, subprogram
    {
      IBlock[] blocks = super.searchCodeLine(line);
      if (blocks != null) {
        list.addAll(Arrays.asList(blocks));
      }
    }
    if (this.body != null) {
      IBlock[] blocks = body.searchCodeLine(line);
      if (blocks != null) {
        list.addAll(Arrays.asList(blocks));
      }
    }
    if (list.size() <= 0) {
      if (addblock != null) {
        list.add(addblock);
      } else {
        return null;
      }
    }

    return list.toArray(new IBlock[0]);
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    Set<Variable> list = new HashSet<Variable>();
    // Variable declaration statement, subprogram
    {
      Set<Variable> vars = super.getAllVariables();
      if (vars != null) {
        list.addAll(vars);
      }
    }
    if (this.body != null) {
      Set<Variable> vars = body.getAllVariables();
      if (vars != null) {
        list.addAll(vars);
      }
    }
    if (list.size() <= 0) return null;
    return list;
  }

  /**
   * Check if it is a program statement.
   *
   * @return true = program statement
   */
  public boolean isProgram() {
    if (this.get_type() == null) return false;
    if (this.get_type().equalsIgnoreCase("program")) return true;
    return false;
  }

  /**
   * Check if it is a subroutine statement.
   *
   * @return true = subroutine statement
   */
  public boolean isSubroutine() {
    if (this.get_type() == null) return false;
    if (this.get_type().equalsIgnoreCase("subroutine")) return true;
    return false;
  }

  /**
   * Check if it is a function statement.
   *
   * @return true = function statement
   */
  public boolean isFunction() {
    if (this.get_type() == null) return false;
    if (this.get_type().equalsIgnoreCase("function")) return true;
    return false;
  }
}
