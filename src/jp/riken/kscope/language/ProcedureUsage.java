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
import java.util.List;
import java.util.Set;
import jp.riken.kscope.information.InformationBlocks;

/**
 * A class that represents a procedure call.
 *
 * @author RIKEN
 */
public class ProcedureUsage extends Block {
  /** Serial number */
  private static final long serialVersionUID = 2385929813029019761L;
  /** Subroutine, function call name */
  private String callName;
  /** Subroutine, function definition */
  private transient Procedure callDefinition;
  /** Formal argument */
  private List<Expression> arguments;
  /** Built-in function flag: true = Built-in function */
  private boolean intrinsic = false;
  /** Function call flag: true = Function call */
  private boolean isFunctionCall = false;

  /**
   * Constructor.
   *
   * @param mama Parent block
   */
  ProcedureUsage(Block mama) {
    super(mama);
  }

  /**
   * Constructor.
   *
   * @param mama Parent block
   * @param subroutineName CALL subroutine name
   * @param argmnts Argument list
   */
  public ProcedureUsage(Block mama, String subroutineName, List<Expression> argmnts) {
    super(mama);
    callName = subroutineName;
    arguments = argmnts;
  }

  /**
   * Constructor.
   *
   * @param subroutineName CALL subroutine name
   * @param argmnts Argument list
   */
  public ProcedureUsage(String subroutineName, List<Expression> argmnts) {
    super();
    callName = subroutineName;
    arguments = argmnts;
  }

  /**
   * Get block type.
   *
   * @return BlockType.PROCEDUREUSAGE
   */
  @Override
  public BlockType getBlockType() {
    return BlockType.PROCEDUREUSAGE;
  }

  @Override
  public String toString() {
    String info = "";
    // delete by @hira at 2013/03/01
    // if (this.getInformation() != null) {
    //    if (!(this.getInformation().getContent().equals(""))) {
    //        info = "[ ! ] ";
    //    }
    // }
    return (info + this.toStringBase());
  }

  @Override
  protected String toStringBase() {
    StringBuilder myString = new StringBuilder();
    String call = "call ";
    if (this.isFunctionCall) {
      call = "function call ";
    }
    myString.append(call + this.callName);
    if (this.arguments != null) {
      myString.append("(");
      for (Expression arg : arguments) {
        myString.append(arg.toString());
        myString.append(", ");
      }
      myString.replace(myString.length() - 2, myString.length(), ")");
    }
    return myString.toString();
  }

  /**
   * Set the declaration of the procedure you are calling.
   *
   * @param proc Declaration of procedure
   */
  public void setCallDefinition(Procedure proc) {
    callDefinition = proc;
    if (proc != null) {
      proc.addCallMember(this);
    }
  }

  /** Set to call a built-in function. */
  public void setIntrinsic() {
    this.intrinsic = true;
  }

  /**
   * Get the name of the calling function
   *
   * @return call_name Calling function name
   */
  public String getCallName() {
    return callName;
  }

  /**
   * Returns a declaration of the procedure to call.
   *
   * @return Declaration of procedure
   */
  public Procedure getCallDefinition() {
    return callDefinition;
  }

  /**
   * Returns a list of actual arguments. If not, returns an empty list.
   *
   * @return List of actual arguments
   */
  public List<Expression> getArguments() {
    if (this.arguments == null) {
      return new ArrayList<Expression>();
    }
    return arguments;
  }

  /**
   * Checks if a variable with the specified name is included in the actual argument, and returns a
   * set of the corresponding subscript order.
   *
   * @param varName Variable name
   * @return Set in subscript order (value is 0 or more). If it doesn't have the corresponding
   *     variable, it returns an empty set.
   */
  public Set<Integer> numberOfArg(String varName) {
    Set<Integer> nums = new HashSet<Integer>();
    List<Expression> args = this.getArguments();
    int i = 0;
    for (Expression arg : args) {
      if (arg.hasVariable(varName)) {
        nums.add(i);
      }
      i++;
    }
    return nums;
  }

  /**
   * Get if it is a built-in function
   *
   * @return true = built-in function
   */
  public boolean isIntrinsic() {
    return intrinsic;
  }

  /**
   * Get the string representation of the function definition.
   *
   * @return String representation of function definition
   */
  private String definitionToString() {
    if (this.callDefinition == null) {
      return "No Definition";
    } else {
      return this.callDefinition.toString();
    }
  }
  /**
   * Returns the HTML representation of its own declaration. Highlight the argument corresponding to
   * numArg.
   *
   * @param numArg Argument subscript order
   * @return HTML representation string of its own declaration.
   */
  public String toDefinitionHTMLString(int numArg) {
    StringBuilder html = new StringBuilder();
    html.append("<html>" + this.definitionToString());
    if (this.arguments != null) {
      html.append("(");
      int count = 0;
      for (Expression arg : arguments) {
        if (count == numArg) {
          // TODO Originally, it is better to receive the highlighted String as an argument of this
          // method and have a process to make only that string red.
          html.append("<span style='color:red;'>");
          html.append(arg.toString());
          html.append("</span>");
        } else {
          html.append(arg.toString());
        }
        html.append(", ");
        count++;
      }
      html.replace(html.length() - 2, html.length(), ")");
    }
    html.append("</html>");
    return html.toString();
  }

  /**
   * Returns its own HTML representation. Highlight the argument corresponding to numArg.
   *
   * @param numArg Argument subscript order
   * @param name Argument name
   * @return Your own HTML representation string.
   */
  public String toHTMLString(int numArg, String name) {
    StringBuilder html = new StringBuilder();
    html.append("<html>" + "call " + this.callName);
    if (this.arguments != null) {
      html.append("(");
      int count = 0;
      for (Expression arg : arguments) {
        if (count == numArg) {
          // TODO
          // Originally, it is better to receive the highlighted String as an argument of this
          // method and have a process to make only that string red.
          html.append("<span style='color:red;'>");
          html.append(arg.toString());
          html.append("</span>");
        } else {
          html.append(arg.toString());
        }
        html.append(", ");
        count++;
      }
      html.replace(html.length() - 2, html.length(), ")");
    }
    html.append("</html>");
    return html.toString();
  }

  /**
   * Returns the program unit to which it belongs.
   *
   * @return Program unit. If not obtained, null is returned.
   */
  public Procedure getMyProcedure() {
    Block me = this;
    while (me != null) {
      me = me.get_mother();
      if (me instanceof ExecutableBody) {
        return ((ExecutableBody) me).getParent();
      }
    }
    return null;
  }

  /**
   * Returns a set of variables contained in the actual arguments in the order indicated by the
   * specified number.
   *
   * @param numActualArg Order of actual arguments A set of variable names contained in the @return
   *     argument. If not, it returns an empty set.
   */
  public Set<String> getActualArgument(int numActualArg) {
    Set<String> varNames = new HashSet<String>();
    if (this.arguments != null) {
      Expression arg = this.arguments.get(numActualArg);
      if (arg != null) {
        Set<Variable> vars = arg.getAllVariables();
        for (Variable var : vars) {
          varNames.add(var.getName());
        }
      }
    }
    return varNames;
  }

  /**
   * Returns the specified formal argument name and the order of the actual arguments corresponding
   * to that order.
   *
   * @param dummyArg Formal argument name
   * @param numDummyArg Order of formal parameters
   * @return The order of the actual arguments. Returns -1 if no correspondence is found.
   */
  public int getNumOfActualArgument(String dummyArg, int numDummyArg) {
    if (this.arguments == null) {
      return -1;
    }
    int cnt = 0;
    for (Expression ex : this.arguments) {
      if (ex instanceof KeywordArgument) {
        KeywordArgument keywrd = (KeywordArgument) ex;
        if (keywrd.getKeyword().equalsIgnoreCase(dummyArg)) {
          return cnt;
        }
      } else {
        if (cnt == numDummyArg) {
          return cnt;
        }
      }
      cnt++;
    }
    return -1;
  }

  /** Set to be a function call. */
  public void setTypeIsFunction() {
    this.isFunctionCall = true;
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
    if (this.callDefinition != null) {
      // Remove for self-reference
      if (!this.getNamespace().equals(this.callDefinition.getNamespace())) {
        result.addAll(this.callDefinition.createInformationBlocks());
      }
    }
    if (this.arguments != null) {
      for (Expression argument : this.arguments) {
        result.addAll(argument.createInformationBlocks());
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

    if (result == null && this.callDefinition != null) {
      // Remove for self-reference
      if (!this.getNamespace().equals(this.callDefinition.getNamespace())) {
        result = this.callDefinition.findInformationBlockBy(id);
      }
    }
    if (result == null && this.arguments != null) {
      for (Expression argument : this.arguments) {
        result = argument.findInformationBlockBy(id);
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
    if (!(block instanceof ProcedureUsage)) return false;
    if (!super.equalsBlocks(block)) return false;

    String thisname = this.callName;
    String destname = ((ProcedureUsage) block).callName;
    if (!thisname.equalsIgnoreCase(destname)) {
      return false;
    }
    if (this.arguments != null && ((ProcedureUsage) block).arguments != null) {
      if (this.arguments.size() == ((ProcedureUsage) block).arguments.size()) {
        for (int i = 0; i < this.arguments.size(); i++) {
          Expression thisArg = this.arguments.get(i);
          Expression destArg = ((ProcedureUsage) block).arguments.get(i);
          if (thisArg == destArg) {
            continue;
          } else if (thisArg == null) {
            return false;
          } else if (!thisArg.equalsExpression(destArg)) {
            return false;
          }
        }
      }
    } else if (this.arguments != null || ((ProcedureUsage) block).arguments != null) {
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

    if (this.callDefinition != null) {
      // Remove for self-reference
      if (!this.getNamespace().equals(this.callDefinition.getNamespace())) {
        IInformation[] infos = this.callDefinition.searchInformationBlocks(block);
        if (infos != null) {
          list.addAll(Arrays.asList(infos));
        }
      }
    }
    if (this.arguments != null) {
      for (Expression argument : this.arguments) {
        IInformation[] infos = argument.searchInformationBlocks(block);
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
}
