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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.generic.Procedures;

/**
 * A class that represents a program unit.
 *
 * @author RIKEN
 */
public abstract class ProgramUnit implements Serializable, IInformation, IBlock {
  /** Serial number */
  private static final long serialVersionUID = -4778301667477615867L;
  /** Program name, module name, subroutine name, function name */
  private String name;
  /** Program unit type: program, module, subroutine, function */
  private String type;
  /** Parent program */
  private ProgramUnit mother;
  /** Start line information */
  private Statement start;
  /** End line information */
  private Statement end;
  /**
   * Program-based attributes. <br>
   * Currently, only "recursive". {<"recursive", "">}
   */
  private Map<String, Object> attributes = new HashMap<String, Object>();
  /** Subprogram unit. */
  private Map<String, Procedure> children;
  /** INTERFACE statement list */
  private List<Procedures> interfaceList;
  /** USE statement list */
  private List<UseState> useList;
  /** Structure structure definition list */
  private List<Type> typeDefinitions = new ArrayList<Type>();
  /** Equivalence statement list */
  private List<Equivalence> equivalenceList;
  /** COMMON statement list */
  private List<Common> commonList;
  /** DATA statement list */
  private List<Data> dataList;
  /** Directive statement list */
  private List<Directive> directiveList;
  /** External Declared function name and data type map */
  private Map<String, IVariableType> externalFunctionList;
  /**
   * Variable declaration statement list. <br>
   * Map \ <variable name, variable definition \> <br>
   * Change from HashMap to LinkedHashMap: To maintain the insertion order at 2010/03/03 by @hira
   */
  public Map<String, VariableDefinition> variables =
      new LinkedHashMap<String, VariableDefinition>(); // defined
  // variables
  // in
  // this
  // program
  // unit
  /** Additional information */
  private TextInfo information = null;
  /**
   * A map of variable names and their blocks referenced in this program unit. Member variables for
   * analytical functions.
   */
  private Map<String, Set<IBlock>> refVariableNames = new HashMap<String, Set<IBlock>>();
  /**
   * A map of variable names and their blocks defined in this program unit. Member variables for
   * analytical functions.
   */
  private Map<String, Set<IBlock>> defVariableNames = new HashMap<String, Set<IBlock>>();
  /**
   * A map of variable names and declarations (including other program units) used within this
   * program unit. Member variables for analytical functions.
   */
  private transient HashMap<String, VariableDefinition> variableMap =
      new HashMap<String, VariableDefinition>();

  /**
   * Constructor.
   *
   * @param tp Program unit type
   * @param nm name
   */
  public ProgramUnit(String tp, String nm) {
    type = tp;
    name = nm;
  }

  /**
   * Returns a list of structure structure definitions that belong to a program unit.
   *
   * @return Structure List of structure definitions. If not, returns an empty list.
   */
  public List<Type> getTypeList() {
    return this.typeDefinitions;
  }

  /**
   * Get the structure definition
   *
   * @param name Structure name
   * @return Structure definition
   */
  public Type getType(String name) {
    if (this.typeDefinitions == null) return null;
    if (name == null) return null;

    for (Type type : this.typeDefinitions) {
      String typename = type.getName();
      if (name.equalsIgnoreCase(typename)) {
        return type;
      }
    }
    return null;
  }

  /**
   * Add a Data statement.
   *
   * @param dt Data class
   */
  public void addData(Data dt) {
    if (this.dataList == null) {
      this.dataList = new ArrayList<Data>();
    }
    this.dataList.add(dt);
  }

  /**
   * Add a Directive statement.
   *
   * @param di Directive class
   */
  public void addDirective(Directive di) {
    if (this.directiveList == null) {
      this.directiveList = new ArrayList<Directive>();
    }
    this.directiveList.add(di);
  }

  /**
   * Add an Equivalence statement.
   *
   * @param eq Equivalence class
   */
  public void addEquivalence(Equivalence eq) {
    if (this.equivalenceList == null) {
      this.equivalenceList = new ArrayList<Equivalence>();
    }
    this.equivalenceList.add(eq);
  }

  /**
   * Add a Common statement.
   *
   * @param cm Common class
   */
  public void addCommon(Common cm) {
    if (this.commonList == null) {
      this.commonList = new ArrayList<Common>();
    }
    this.commonList.add(cm);
  }

  /**
   * Add an INTERFACE declaration.
   *
   * @param blk interface class
   */
  public void addInterface(Procedures blk) {
    if (this.interfaceList == null) {
      this.interfaceList = new ArrayList<Procedures>();
    }
    this.interfaceList.add(blk);
  }
  /**
   * Returns a list of INTERFACE statements.
   *
   * @return INTERFACE statement list. If not, returns an empty list.
   */
  public List<Procedures> getInterfaceList() {
    if (this.interfaceList == null) {
      return new ArrayList<Procedures>();
    }
    return this.interfaceList;
  }

  @Override
  public String toString() {
    // delete by @hira at 2013/03/01
    //        String info = "";
    //        if (this.getInformation() != null) {
    //            if (!(this.getInformation().getContent().equals(""))) {
    //                info = "[ ! ] ";
    //            }
    //        }
    // return (info + this.toStringBase());
    return this.toStringBase();
  }

  /**
   * Get the program name, module name, subroutine name, and function name.
   *
   * @return Program name, module name, subroutine name, function name
   */
  protected String toStringBase() {
    return name;
  }

  /**
   * Get the file path (file name including the path). <br>
   *
   * @return file path
   */
  public String getFilePath() {
    String result;
    try {
      result = this.start.lineInfo.getSourceFile().getPath();
    } catch (Exception e) {
      result = null;
    }
    return result;
  }

  /**
   * Set the parent program.
   *
   * @param mam parent program
   */
  protected void set_mother(ProgramUnit mam) {
    mother = mam;
  }

  /**
   * Add program attributes.
   *
   * @param attribute_name Attribute name
   */
  protected void put_attribute(String attribute_name) {
    attributes.put(attribute_name, "");
  }

  /**
   * Add program attributes.
   *
   * @param attribute_name Attribute name
   * @param attribute_value Attribute value
   */
  protected void put_attribute(String attribute_name, Object attribute_value) {
    attributes.put(attribute_name, attribute_value);
  }

  /**
   * Add a subprogram.
   *
   * @param child
   */
  protected void put_child(Procedure child) {
    if (this.children == null) {
      this.children = new HashMap<String, Procedure>();
    }
    children.put(child.get_name(), child);
  }

  /**
   * Add a subprogram.
   *
   * @param type_name Program unit type
   * @param proc_name Program name
   */
  protected void set_child(String type_name, String proc_name) {
    Procedure proc = new Procedure(type_name, proc_name);
    put_child(proc);
  }

  /**
   * Add a variable declaration statement.
   *
   * @param vr Variable declaration statement
   */
  protected void put_variable(VariableDefinition vr) {
    vr.setMother(this);
    variables.put(vr.get_name(), vr);
  }

  /**
   * Add a variable declaration statement.
   *
   * @param varName Variable name
   */
  protected void new_variable_def(String varName) {
    VariableDefinition varDef = new VariableDefinition(varName);
    this.put_variable(varDef);
  }

  /**
   * Determine if you have the specified attributes.
   *
   * @param attribute_name The name of the attribute
   * @return Boolean value. True if it has attributes
   */
  public boolean has_attribute(String attribute_name) {
    return attributes.containsKey(attribute_name);
  }

  /**
   * Check if the subprogram name exists.
   *
   * @param name Subprogram name
   * @return true = Exists in the subprogram.
   */
  protected boolean is_my_procedure(String name) {
    if (this.children != null) {
      return children.containsKey(name);
    }
    return false;
  }

  /**
   * Returns the module name, subroutine name, and function name.
   *
   * @return Module name, subroutine name, function name
   */
  public String get_name() {
    return this.name;
  }

  /**
   * Returns the program unit type.
   *
   * @return Program unit type
   */
  public String get_type() {
    return type;
  }

  /**
   * Returns the start statement.
   *
   * @return start statement
   */
  public Statement get_start() {
    return (start);
  }

  /**
   * Returns the end statement.
   *
   * @return end statement
   */
  public Statement get_end() {
    return (end);
  }

  /**
   * Returns the parent program unit.
   *
   * @return Parent program unit. Returns null if not
   */
  public ProgramUnit get_mother() {
    return mother;
  }

  /**
   * Returns an array of subprogram units.
   *
   * @return An array of subprogram units. If not, it returns null.
   */
  public Procedure[] get_children() {
    if (this.children == null) {
      return null;
    }
    return children.values().toArray(new Procedure[0]);
  }
  /**
   * Get subprogram units.
   *
   * @return List of subprogram units. If not, returns an empty list.
   */
  public Collection<Procedure> getChildren() {
    if (this.children == null) {
      return new ArrayList<Procedure>();
    }
    if (this.children.values().size() > 0) {
      return this.children.values();
    }
    return new ArrayList<Procedure>();
  }

  /**
   * Get a subprogram.
   *
   * @param name Subprogram name
   * @return subprogram
   */
  protected ProgramUnit get_child(String name) {
    if (this.children == null) {
      return null;
    }
    return children.get(name);
  }

  /**
   * Get the number of subprograms.
   *
   * @return Number of subprograms
   */
  protected int get_num_of_child() {
    if (this.children == null) {
      return 0;
    }
    return children.size();
  }

  /**
   * Get a list of subprogram names.
   *
   * @return List of subprogram names
   */
  protected String[] get_child_name() {
    if (this.children == null) {
      return null;
    }
    return children.keySet().toArray(new String[children.size()]);
  }

  /**
   * Get the program attribute value.
   *
   * @param attribute_name Attribute name
   * @return attribute value
   */
  protected Object get_attribute(String attribute_name) {
    return attributes.get(attribute_name);
  }

  /**
   * Returns a variable declaration with the specified name.
   *
   * @param var_name Variable name
   * @return Variable declaration. If not, it returns null.
   */
  public VariableDefinition get_variable(String var_name) {
    return variables.get(var_name);
  }

  /**
   * Returns a map of variable declarations related to itself.
   *
   * @return Map of variable declarations.
   */
  public Map<String, VariableDefinition> getVariables() {
    return this.variables;
  }

  /**
   * Set the start code line information.
   *
   * @param lineInfo Start code line information
   */
  public void set_start(CodeLine lineInfo) {
    start = new Statement(lineInfo);
  }

  /**
   * Set the exit code line information.
   *
   * @param lineInfo Exit code line information
   */
  public void set_end(CodeLine lineInfo) {
    end = new Statement(lineInfo);
  }

  /**
   * Set a variable declaration statement.
   *
   * @param varDef Variable declaration statement
   */
  protected void set_variable_def(VariableDefinition varDef) {
    this.put_variable(varDef);
  }

  /**
   * Get the variable declaration statement.
   *
   * @return Variable declaration statement list
   */
  public VariableDefinition[] get_variables() {
    if (variables == null) return null;

    Set<String> keys = variables.keySet();

    Collection<VariableDefinition> values = variables.values();
    return values.toArray(new VariableDefinition[0]);
  }

  @Override
  public void setInformation(TextInfo info) {
    this.information = info;
  }

  /**
   * Get additional information
   *
   * @return Additional information
   */
  @Override
  public TextInfo getInformation() {
    return this.information;
  }

  /**
   * Get start line number information
   *
   * @return Start line number information
   */
  @Override
  public CodeLine getStartCodeLine() {
    if (start == null) return null;
    return start.lineInfo;
  }

  /**
   * Get end line number information
   *
   * @return End line number information
   */
  @Override
  public CodeLine getEndCodeLine() {
    if (end == null) return null;
    return end.lineInfo;
  }

  /**
   * Register the TYPE declaration.
   *
   * @param tp
   */
  protected void addTypeDefinition(Type tp) {
    this.typeDefinitions.add(tp);
    tp.setMotherBlock(this);
  }

  /**
   * Returns a list of USE statements.
   *
   * @return A list of USE statements. If not, returns an empty list.
   */
  public List<UseState> getUseList() {
    if (useList == null) {
      return new ArrayList<UseState>();
    }
    return useList;
  }

  /**
   * Returns a DATA statement list.
   *
   * @return DATA statement list
   */
  public List<Data> getDataList() {
    return dataList;
  }

  /**
   * Returns a Directive statement list.
   *
   * @return Directive statement list
   */
  public List<Directive> getDirectiveList() {
    return directiveList;
  }

  /**
   * Returns a list of EQUIVALENCE statements.
   *
   * @return EQUIVALENCE Statement list
   */
  public List<Equivalence> getEquivalenceList() {
    return equivalenceList;
  }

  /**
   * COMMON Returns a list of statements.
   *
   * @return COMMON statement list. If not, an empty list is returned.
   */
  public List<Common> getCommonList() {
    if (this.commonList == null) {
      return new ArrayList<Common>();
    }
    return commonList;
  }

  /**
   * Returns the name of a COMMON name that contains a variable with the specified name as a member.
   *
   * @param varname Variable name
   * @return COMMON name. If it is an anonymous COMMON block, "NO_NAME" is returned. If there is no
   *     corresponding COMMON statement, null is returned.
   */
  public String getCommonName(String varname) {
    String st = null;
    List<Common> comList = this.getCommonList();
    for (Common com : comList) {
      if (com.contains(varname)) {
        st = com.getName();
      }
    }
    return st;
  }

  /**
   * Add a USE statement.
   *
   * @param useline USE statement
   */
  protected void addUse(UseState useline) {
    if (this.useList == null) {
      this.useList = new ArrayList<UseState>();
    }
    useList.add(useline);
  }

  /**
   * Get the namespace (module name.routine name).
   *
   * @return namespace (module name.routine name)
   */
  @Override
  public String getNamespace() {
    String result = "";
    if (this.mother == null) {
      result = this.name;
    } else {
      result = mother.getNamespace().concat(".");
      result = result.concat(this.name);
    }
    return result;
  }

  /**
   * Get the start position.
   *
   * @return start position
   */
  @Override
  public int getStartPos() {
    return this.getStartCodeLine().getStartLine();
  }
  /**
   * Set the start position.
   *
   * @param pos Starting position
   */
  @Override
  public void setStartPos(int pos) {
    this.getStartCodeLine().setLine(pos);
  }

  /*
   * TODO: Temporary support.
   * Actually, the end of the program is program.getEndCodeLine.getEndLine
   * Get or delete EndCodeLine of program and StartCodeLine
   * Should be renamed to CodeLine. Suspect.
   */

  /**
   * Get the end position.
   *
   * @return end position
   */
  @Override
  public int getEndPos() {
    return this.getStartCodeLine().getEndLine();
  }

  /**
   * Set the end position.
   *
   * @param pos End position
   */
  @Override
  public void setEndPos(int pos) {
    this.getStartCodeLine().setEndLine(pos);
  }

  /**
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
   */
  public IInformation findInformationBlockBy(String id) {
    IInformation result = null;

    if (this.getID().equals(id)) {
      result = this;
    }

    if (result == null && this.variables != null) {
      Collection<VariableDefinition> definitions = this.variables.values();
      for (VariableDefinition definition : definitions) {
        result = definition.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
    }

    if (result == null && this.getChildren() != null) {
      Collection<Procedure> procedures = this.getChildren();
      for (Procedure procedure : procedures) {
        result = procedure.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
    }

    return result;
  }

  /** Delete all additional information. */
  @Override
  public void clearInformation() {
    this.setInformation(null);
    for (Procedure child : this.getChildren()) {
      child.clearInformation();
    }
    for (VariableDefinition variable : this.variables.values()) {
      variable.clearInformation();
    }
  }

  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  public InformationBlocks createInformationBlocks() {
    InformationBlocks result = new InformationBlocks();

    // Add your own additional information
    if (this.information != null) {
      InformationBlock block = new InformationBlock(this.information, this, this);
      result.add(block);
    }
    // Add additional information for each subprogram that you have
    for (Procedure procedure : this.getChildren()) {
      result.addAll(procedure.createInformationBlocks());
    }
    // Add additional information of your own variable declaration
    if (this.variables != null) {
      for (VariableDefinition variable : this.variables.values()) {
        result.addAll(variable.createInformationBlocks());
      }
    }
    return result;
  }

  /**
   * Search ProgramUnit from the namespace (module name + routine name).
   *
   * @param namespace Searched namespace
   * @return The first ProgramUnit object found
   */
  public ProgramUnit findProgramUnitBy(String namespace) {
    ProgramUnit result = null;
    if (this.getNamespace().equalsIgnoreCase(namespace)) {
      result = this;
    }
    if (result == null) {
      for (Procedure proc : this.getChildren()) {
        result = proc.findProgramUnitBy(namespace);
        if (result != null) {
          break;
        }
      }
    }
    return result;
  }

  /**
   * Get a map of the externally declared function name and data type.
   *
   * @return Function name and data type map
   */
  public Map<String, IVariableType> getExternalFunctionList() {
    return externalFunctionList;
  }

  /**
   * external Add a map element for the declared function name and data type.
   *
   * @param funcName Function name
   * @param tp data type
   */
  public void addExternalFunctionList(String funcName, IVariableType tp) {
    if (this.externalFunctionList == null) {
      this.externalFunctionList = new HashMap<String, IVariableType>();
    }
    if (!(this.externalFunctionList.containsKey(funcName))) {
      this.externalFunctionList.put(funcName, tp);
    }
  }
  /**
   * Get an ID.
   *
   * @return ID
   */
  @Override
  public String getID() {
    String result = "";
    if (this.mother != null) {
      result = this.mother.getID() + "." + this.toStringBase();
    } else {
      result = this.toStringBase();
    }
    return result;
  }

  /**
   * Returns a map of variable names and block lists referenced within a program unit.
   *
   * @return Variable name and block list map
   */
  public Map<String, Set<IBlock>> getRefVariableNames() {
    return refVariableNames;
  }

  /**
   * Add variable names referenced and blocks that appear in the program unit.
   *
   * @param refVarName Variable name
   * @param blk Block
   */
  public void putRefVariableName(String refVarName, IBlock blk) {
    String nm = refVarName.toLowerCase();
    if (this.refVariableNames.containsKey(nm)) {
      Set<IBlock> block = this.refVariableNames.get(nm);
      block.add(blk);
    } else {
      Set<IBlock> block = new LinkedHashSet<IBlock>();
      block.add(blk);
      this.refVariableNames.put(nm, block);
    }
  }

  /**
   * Get the set of variable names defined in the program unit.
   *
   * @return A set of variable names
   */
  public Map<String, Set<IBlock>> getDefVariableNames() {
    return defVariableNames;
  }

  /**
   * Add the variable name defined in the program unit and the block that appears.
   *
   * @param defVarName Variable name
   * @param blk Block
   */
  public void putDefVariableName(String defVarName, IBlock blk) {
    String nm = defVarName.toLowerCase();
    if (this.defVariableNames.containsKey(nm)) {
      Set<IBlock> block = this.defVariableNames.get(nm);
      block.add(blk);
    } else {
      Set<IBlock> block = new LinkedHashSet<IBlock>();
      block.add(blk);
      this.defVariableNames.put(nm, block);
    }
  }
  /**
   * Get the variable map used for each program.
   *
   * @return Variable map
   */
  public HashMap<String, VariableDefinition> getVariableMap() {
    if (this.variableMap == null) {
      this.createVariableMap();
    }
    return this.variableMap;
  }

  /**
   * If there is a variable used by the specified name in the program unit, its declaration is
   * returned.
   *
   * @param nm Variable name
   * @return Variable declaration. If not, it returns null.
   */
  public VariableDefinition getVariableMap(String nm) {
    if (nm == null || nm.isEmpty()) return null;
    if (this.variableMap == null) {
      this.createVariableMap();
    }
    VariableDefinition def = null;
    if (this.variableMap.get(nm) == null) {
      return def;
    }
    return this.variableMap.get(nm);
  }

  /** Create a variable declaration list. */
  private void createVariableMap() {
    this.variableMap = new HashMap<String, VariableDefinition>();
  }

  /**
   * Add variable names used for each program.
   *
   * @param nm Variable name
   */
  public void putVariableMap(String nm) {
    if (this.variableMap == null) {
      this.createVariableMap();
    }
    if (!(this.variableMap.containsKey(nm))) {
      VariableDefinition vardef = null;
      this.variableMap.put(nm, vardef);
    }
  }

  /**
   * Add a map of variable names and declarations used on a program-by-program basis.
   *
   * @param varName Variable name
   * @param varDef Variable declaration
   */
  public void putVariableMap(String varName, VariableDefinition varDef) {
    if (this.variableMap == null) {
      this.createVariableMap();
    }
    this.variableMap.put(varName, varDef);
  }
  /**
   * Set all variable names contained in the specified expression and the specified block as
   * references. Set all variable names included in the specified expression and the procedure call
   * included in the expression as references.
   *
   * @param blk block
   * @param exp expression
   */
  public void addExpressionToRef(IBlock blk, Expression exp) {
    Set<Variable> vars = exp.getAllVariables();
    for (Variable var : vars) {
      this.putRefVariableName(var.getName(), blk);
      this.putVariableMap(var.getName());
    }
    Set<ProcedureUsage> rightFunc = exp.getAllFunctions();
    for (ProcedureUsage pu : rightFunc) {
      if (blk instanceof Block) {
        pu.set_mother((Block) blk);
      }
      pu.set_block_start(blk.getStartCodeLine());
      pu.set_block_end(blk.getEndCodeLine());
      List<Expression> args = pu.getArguments();
      for (Expression arg : args) {
        Set<Variable> funcVars = arg.getAllVariables();
        for (Variable var : funcVars) {
          this.putRefVariableName(var.getName(), pu);
        }
      }
    }
  }

  /**
   * Get the parent block
   *
   * @return Parent block
   */
  @Override
  public IBlock getMotherBlock() {
    return this.get_mother();
  }

  /**
   * Check if the same ProgramUnit exists in the child block
   *
   * @param unit Search ProgramUnit
   * @return true = Same Program Unit exists
   */
  public boolean containsChildren(ProgramUnit unit) {
    if (this == unit) return true;
    String thisID = this.getID();
    String unitID = unit.getID();
    if (thisID.equalsIgnoreCase(unitID)) {
      return true;
    }
    if (this.children == null) return false;
    boolean result = false;
    for (String key : this.children.keySet()) {
      ProgramUnit childrenUnit = this.children.get(key);
      if (childrenUnit == null) continue;
      result = childrenUnit.containsChildren(unit);
      if (result) {
        return result;
      }
    }
    return result;
  }

  /**
   * Check if they are the same Program Unit. Check only subprogram units (children) and variable
   * declarations (variables). Since it is for replacement of additional information, check only the
   * acquired block with createInformationBlocks.
   *
   * @param unit module, subroutine
   * @return true = match
   */
  public boolean equalsBlocks(ProgramUnit unit) {
    if (unit == null) return false;

    // Module, subroutine name
    if (this.name != null) {
      if (!this.name.equalsIgnoreCase(unit.name)) {
        return false;
      }
    }
    // Type
    if (this.type != null) {
      if (!this.type.equalsIgnoreCase(unit.type)) {
        return false;
      }
    }
    // Subprogram
    if (this.children != null && unit.children != null) {
      if (this.children.size() != unit.children.size()) {
        return false;
      }
    } else if (!(this.children == null && unit.children == null)) {
      return false;
    }
    if (this.children != null && unit.children != null) {
      for (String key : this.children.keySet()) {
        Procedure srcProc = this.children.get(key);
        Procedure destProc = unit.children.get(key);
        if (srcProc == null || destProc == null) {
          return false;
        }
        if (!srcProc.equalsBlocks(destProc)) {
          return false;
        }
      }
    }

    // Variable declaration statement
    if (this.variables != null && unit.variables != null) {
      if (this.variables.size() != unit.variables.size()) {
        return false;
      }
    } else if (!(this.variables == null && unit.variables == null)) {
      return false;
    }
    if (this.variables != null && unit.variables != null) {
      for (String key : this.variables.keySet()) {
        VariableDefinition srcVar = this.variables.get(key);
        VariableDefinition destVar = unit.variables.get(key);
        if (srcVar == null || destVar == null) {
          return false;
        }
        if (!srcVar.equalsBlocks(destVar)) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Get the same block
   *
   * @param block IInformation block
   * @return Same block
   */
  public IInformation findBlock(IInformation block) {
    if (block == null) return null;
    String id = block.getID();
    return this.findInformationBlockBy(id);
  }

  /**
   * Check if child blocks and variable declarations exist.
   *
   * @return true = empty module, subroutine
   */
  public boolean isEmpty() {
    if (this.children != null && this.children.size() > 0) {
      return false;
    }
    if (this.variables != null && this.variables.size() > 0) {
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
  public IInformation[] searchInformationBlocks(IInformation block) {
    if (block == null) return null;

    List<IInformation> list = new ArrayList<IInformation>();
    if (block instanceof ProgramUnit) {
      if (this.equalsBlocks((ProgramUnit) block)) {
        list.add(this);
      }
    }

    // Variable declaration statement
    if (this.variables != null) {
      Collection<VariableDefinition> definitions = this.variables.values();
      for (VariableDefinition definition : definitions) {
        IInformation[] infos = definition.searchInformationBlocks(block);
        if (infos != null) {
          list.addAll(Arrays.asList(infos));
        }
      }
    }

    // Subroutine
    if (this.getChildren() != null) {
      Collection<Procedure> procedures = this.getChildren();
      for (Procedure procedure : procedures) {
        IInformation[] infos = procedure.searchInformationBlocks(block);
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

  /**
   * Check if they are in the same block hierarchy.
   *
   * @param unit Check target programUnit
   * @return true = match
   */
  public boolean equalsLayout(ProgramUnit unit) {
    // Module, subroutine name
    if (this.name != null) {
      if (!this.name.equalsIgnoreCase(unit.name)) {
        return false;
      }
    }
    // Type
    if (this.type != null) {
      if (!this.type.equalsIgnoreCase(unit.type)) {
        return false;
      }
    }

    // Subprogram
    if (this.children != null && unit.children != null) {
      if (this.children.size() != unit.children.size()) {
        return false;
      }
    } else if (!(this.children == null && unit.children == null)) {
      return false;
    }
    if (this.children != null && unit.children != null) {
      for (String key : this.children.keySet()) {
        Procedure srcProc = this.children.get(key);
        Procedure destProc = unit.children.get(key);
        if (srcProc == null || destProc == null) {
          return false;
        }
        if (!srcProc.equalsLayout(destProc)) {
          return false;
        }
      }
    }

    // In case of block hierarchy check, variable declaration statement is not checked.

    return true;
  }

  /**
   * Get the structure ID.
   *
   * @return Structure ID
   */
  @Override
  public String getLayoutID() {
    return getID();
  }

  /**
   * Search for structural blocks that match the layoutID.
   *
   * @param id layoutID
   * @return Found structural block
   */
  public IInformation findInformationLayoutID(String id) {
    if (id == null || id.isEmpty()) return null;
    IInformation result = null;
    if (this.getLayoutID().equalsIgnoreCase(id)) {
      result = this;
    }

    if (result == null && this.getChildren() != null) {
      Collection<Procedure> procedures = this.getChildren();
      for (Procedure procedure : procedures) {
        result = procedure.findInformationLayoutID(id);
        if (result != null) {
          break;
        }
      }
    }

    return result;
  }

  /**
   * Search for blocks of line numbers
   *
   * @param line line number
   * @return Line number block
   */
  public IBlock[] searchCodeLine(CodeLine line) {
    if (line == null) return null;

    List<IBlock> list = new ArrayList<IBlock>();
    CodeLine thisstart = this.getStartCodeLine();
    CodeLine thisend = this.getEndCodeLine();
    if (line.isOverlap(thisstart, thisend)) {
      ; // nothing
    } else {
      // Since this block is out of range, the child blocks are also out of range
      return null;
    }

    // Variable declaration statement
    if (this.variables != null) {
      Collection<VariableDefinition> definitions = this.variables.values();
      for (VariableDefinition definition : definitions) {
        IBlock[] blocks = definition.searchCodeLine(line);
        if (blocks != null) {
          list.addAll(Arrays.asList(blocks));
        }
      }
    }

    // Subroutine
    if (this.getChildren() != null) {
      Collection<Procedure> procedures = this.getChildren();
      for (Procedure procedure : procedures) {
        IBlock[] blocks = procedure.searchCodeLine(line);
        if (blocks != null) {
          list.addAll(Arrays.asList(blocks));
        }
      }
    }
    if (list.size() <= 0) {
      return null;
    }

    return list.toArray(new IBlock[0]);
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    // Subroutine
    Set<Variable> list = new HashSet<Variable>();
    if (this.getChildren() != null) {
      Collection<Procedure> procedures = this.getChildren();
      for (Procedure procedure : procedures) {
        Set<Variable> vars = procedure.getAllVariables();
        if (vars != null) {
          list.addAll(vars);
        }
      }
    }
    if (list.size() <= 0) return null;
    return list;
  }

  /** Set variable definition for variable */
  public void setVariableDefinitions() {
    Set<Variable> vars = getAllVariables();
    if (vars == null || vars.size() <= 0) return;
    for (Variable var : vars) {
      String name = var.getName();
      VariableDefinition def = getVariableMap(name);
      if (def != null) {
        var.setDefinition(def);
      }
    }
  }
}
