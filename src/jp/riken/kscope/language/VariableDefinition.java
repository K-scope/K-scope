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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;

/** A class that expresses the declaration of variables and structures. */
public class VariableDefinition implements Serializable, IInformation, IBlock {
  /** Serial number */
  private static final long serialVersionUID = 8694203337559301954L;
  /** Variable name. */
  private String name;
  /** Data type. */
  private IVariableType type;
  /** Attribute. */
  private IVariableAttribute attribute;
  /** Array elements. */
  private VariableDimension dimension;
  /** initial value. */
  private String initValue;
  /** Start position information of type declaration. */
  private Statement start;
  /** End position information of type declaration. */
  private Statement end;
  /** Additional information. */
  private TextInfo information = null;
  /** A set of program units that refer to and define themselves by the USE statement. */
  private transient Set<ProgramUnit> referMembers = new HashSet<ProgramUnit>();
  /** Program unit that holds this declaration. */
  private ProgramUnit mother;

  /**
   * Constructor.
   *
   * @param line Row information
   */
  public VariableDefinition(CodeLine line) {
    start = new Statement(line);
    end = new Statement(line);
  }

  /**
   * Constructor
   *
   * <p>VariableDefinition (String nm, VariableType typ, VariableAttribute attrbts) <br>
   * Or <br>
   * VariableDefinition (String nm, VariableType typ, VariableAttribute attrbts, VariableDimension
   * dmnsn) <br>
   * Be sure to use *.
   *
   * @param varnm Variable name
   */
  public VariableDefinition(String varnm) {
    name = varnm;
  }

  /**
   * Constructor.
   *
   * @param nm Variable name
   * @param typ Type
   * @param attrbts Attributes
   */
  public VariableDefinition(String nm, IVariableType typ, IVariableAttribute attrbts) {
    this.name = nm;
    this.type = typ;
    this.attribute = attrbts;
  }

  /**
   * Constructor.
   *
   * @param nm Variable name
   * @param typ Type
   * @param attrbts Attributes
   * @param dmnsn Sequence information
   */
  public VariableDefinition(
      String nm, IVariableType typ, IVariableAttribute attrbts, VariableDimension dmnsn) {
    this(nm, typ, attrbts);
    this.dimension = dmnsn;
  }
  /**
   * Get block type.
   *
   * @return BlockType.VARIABLEDEFINITION
   */
  @Override
  public BlockType getBlockType() {
    return BlockType.VARIABLEDEFINITION;
  }

  /**
   * Set the data type.
   *
   * @param tp Data type
   */
  public void setVariableType(IVariableType tp) {
    type = tp;
  }

  /**
   * Set attributes.
   *
   * @param att Attributes
   */
  public void setVariableAttributes(IVariableAttribute att) {
    attribute = att;
  }

  /**
   * Set the lower and upper limits of array elements.
   *
   * @param i dimension
   * @param startIndex Lower limit
   * @param endIndex Upper limit
   */
  public void setDimensionIndex(int i, Expression startIndex, Expression endIndex) {
    setStartIndex(i, startIndex);
    setEndIndex(i, endIndex);
  }

  /**
   * Set the lower limit of array elements.
   *
   * @param i dimension
   * @param startIndex Lower limit
   */
  public void setStartIndex(int i, Expression startIndex) {
    dimension.getIndex(i).set_start(startIndex);
  }

  /**
   * Set the upper limit of array elements.
   *
   * @param i dimension
   * @param indexEnd Upper limit
   */
  public void setEndIndex(int i, Expression indexEnd) {
    dimension.getIndex(i).set_end(indexEnd);
  }

  /**
   * Add a program unit that references itself. <br>
   *
   * @param proc Procedure
   */
  public void addReferMember(ProgramUnit proc) {
    if (proc == null) {
      return;
    }
    this.getReferMember().add(proc);
  }

  /**
   * Returns a set of program units that reference itself.
   *
   * @return A set of procedures. If it does not exist, it returns an empty set.
   */
  public Set<ProgramUnit> getReferMember() {
    if (this.referMembers == null) {
      return new HashSet<ProgramUnit>();
    }
    return this.referMembers;
  }

  /**
   * Get the variable name.
   *
   * @return variable name
   */
  public String get_name() {
    return (name);
  }

  /**
   * Get the data type.
   *
   * @return data type
   */
  public IVariableType getType() {
    return type;
  }

  /**
   * Get attributes
   *
   * @return attribute
   */
  public IVariableAttribute getAttribute() {
    return attribute;
  }

  /**
   * Get the number of dimensions of an array element
   *
   * @return Number of array element dimensions
   */
  public int get_dimension_size() {
    if (dimension == null || dimension.getIndex() == null) return 0;
    return (dimension.getIndex().length);
  }

  /**
   * Get the lower limit of the specified dimension from the array element
   *
   * @param i Number of dimensions
   * @return lower dimension
   */
  public Expression get_index_start(int i) {
    return (dimension.getIndex(i).get_start());
  }

  /**
   * Get the upper limit of the specified dimension from the array element
   *
   * @param i Number of dimensions
   * @return Dimensional upper limit
   */
  public Expression get_index_end(int i) {
    return (dimension.getIndex(i).get_end());
  }

  /**
   * Set array elements.
   *
   * @param dimension Array elements
   */
  public void setDimension(VariableDimension dimension) {
    this.dimension = dimension;
  }

  /**
   * Set the initial value.
   *
   * @param value initial value
   */
  public void setInitValue(String value) {
    this.initValue = value;
  }

  /**
   * Get the initial value
   *
   * @return Initial value
   */
  public String getInitValue() {
    return this.initValue;
  }

  @Override
  public String toString() {
    String info = "";
    // delete by @hira at 2013/03/01
    //        if (this.getInformation() != null) {
    //            if (!(this.getInformation().getContent().equals(""))) {
    //                info = "[ ! ] ";
    //            }
    //        }
    return (info + this.toStringBase());
  }

  /**
   * Returns a string representation of the variable declaration.
   *
   * @return String representation of variable declaration
   */
  protected String toStringBase() {
    StringBuilder var = new StringBuilder();

    if (type != null) {
      // Data type
      var.append(type.toString());
    }

    if (dimension != null) {
      var.append(",dimension(");
      String dims = "";
      for (int i = 0; i < dimension.get_index_size(); i++) {
        String start = dimension.get_index_start(i).toString();
        String end = dimension.get_index_end(i).toString();
        if (start != null) {
          dims += start;
        }
        dims += ":";
        if (end != null) {
          dims += end;
        }
        if (i + 1 < dimension.get_index_size()) {
          dims += ",";
        }
      }
      var.append(dims);
      var.append(")");
    }

    // Attribute
    if (attribute != null) {
      Iterator<String> itr = attribute.getAttributes().iterator();
      while (itr.hasNext()) {
        String attr = itr.next();
        var.append(",");
        var.append(attr);
      }
    }
    var.append(" ");

    // Variable name
    var.append("::");
    var.append(name);

    // initial value
    if (initValue != null) {
      var.append("=");
      var.append(initValue);
    }
    return var.toString();
  }

  /**
   * Whether the type is compatible. <br>
   * When searching for the corresponding function from the overloaded function group, <br>
   * It is necessary to check the type of formal and actual arguments. <br>
   * "Matching" means that this type check determines that the type is the same.
   *
   * @param actualArgument
   * @return true: Conforms <br>
   *     false: Not compatible
   */
  public boolean matches(Expression actualArgument) {
    if (actualArgument == null) {
      return false;
    }

    // modify by @hira at 2013/02/01
    if (actualArgument.getType() == null) {
      return false;
    }
    if (this.getType() == null) {
      return false;
    }
    return actualArgument.getType().matches(this.getType());
  }
  /**
   * Set additional information
   *
   * @param info Additional information
   */
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
   * Set line information.
   *
   * @param line Row information
   */
  public void setCodeLine(CodeLine line) {
    start = new Statement(line);
    end = new Statement(line);
  }

  /**
   * Get the data type.
   *
   * @return data type
   */
  public IVariableType getVariableType() {
    return type;
  }

  /**
   * Get sequence information.
   *
   * @return Array information
   */
  public VariableDimension getVariableDimension() {
    return this.dimension;
  }

  /**
   * Returns true if it is a scalar.
   *
   * @return Boolean: true if scalar
   */
  public boolean isScalar() {
    if (this.dimension == null) {
      return true;
    }
    return false;
  }

  /**
   * Set the parent program.
   *
   * @param mother parent program
   */
  public void setMother(ProgramUnit mother) {
    this.mother = mother;
  }

  /**
   * Acquire parent program units.
   *
   * @return Parent program unit
   */
  public ProgramUnit getMother() {
    return this.mother;
  }
  /**
   * Get the namespace (module name.routine name).
   *
   * @return namespace (module name.routine name)
   */
  @Override
  public String getNamespace() {
    String result = "";
    if (this.mother != null) {
      result = mother.getNamespace();
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

    return result;
  }

  /** Delete all additional information. */
  @Override
  public void clearInformation() {
    this.setInformation(null);
  }

  /**
   * Generate an additional information container collection.
   *
   * @return Additional information container collection
   */
  public InformationBlocks createInformationBlocks() {
    InformationBlocks result = new InformationBlocks();

    if (this.information != null) {
      InformationBlock cont = new InformationBlock(this.information, this, this);
      result.add(cont);
    }

    return result;
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
      int offset = this.getStartPos() - this.mother.getStartPos();
      result = this.mother.getID() + "$" + offset + ":" + this.toStringBase();
    } else {
      result = this.toStringBase();
    }
    return result;
  }
  /**
   * Get the parent block
   *
   * @return Parent block
   */
  @Override
  public IBlock getMotherBlock() {
    return this.getMother();
  }

  /**
   * Check if they are the same Variable Definition. Check if they are the same in the character
   * string representation of the variable declaration.
   *
   * @param definition Variable / structure declaration
   * @return true = match
   */
  public boolean equalsBlocks(VariableDefinition definition) {
    // Check if they are the same in the character string representation of the variable
    // declaration.
    String thisVar = toStringBase();
    String destVar = definition.toStringBase();
    if (thisVar == null && destVar == null) {
      return true;
    } else if (thisVar == null) {
      return false;
    }
    return thisVar.equalsIgnoreCase(destVar);
  }

  /**
   * Search for the same block
   *
   * @param block IInformation block
   * @return Same block
   */
  public IInformation[] searchInformationBlocks(IInformation block) {
    List<IInformation> list = new ArrayList<IInformation>();
    if (block instanceof VariableDefinition) {
      if (this.equalsBlocks((VariableDefinition) block)) {
        list.addAll(Arrays.asList(this));
      }
    }
    if (list.size() <= 0) {
      return null;
    }
    return list.toArray(new IInformation[0]);
  }

  /**
   * Get the structure ID. Returns null as no structure ID is needed.
   *
   * @return Structure ID
   */
  @Override
  public String getLayoutID() {
    return null;
  }

  /**
   * Search for blocks of line numbers
   *
   * @param line line number
   * @return Line number block
   */
  public IBlock[] searchCodeLine(CodeLine line) {
    if (line == null) return null;
    if (line.getSourceFile() == null) return null;
    if (this.getStartCodeLine() == null) return null;
    if (!line.getSourceFile().equals(this.getStartCodeLine().getSourceFile())) return null;

    List<IBlock> list = new ArrayList<IBlock>();
    CodeLine thisstart = this.getStartCodeLine();
    CodeLine thisend = this.getEndCodeLine();
    if (line.isOverlap(thisstart, thisend)) {
      list.add(this);
    }

    if (list.size() <= 0) {
      return null;
    }

    return list.toArray(new IBlock[0]);
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    return null;
  }
}
