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
package jp.riken.kscope.xcodeml.language;

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.xcodeml.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * Variable parser class
 *
 * @author RIKEN
 */
public class VariableParser {

  /** typeTable */
  private XcodeMLTypeManager typeManager;

  /**
   * Constructor
   *
   * @param typeManager typeTable
   */
  public VariableParser(XcodeMLTypeManager typeManager) {
    this.typeManager = typeManager;
  }

  /**
   * Parse and generate DB :: Variable class from IXmlNode element.
   *
   * @param node IXmlNode element
   * @return DB :: Variable class
   */
  public Variable getVariable(IXmlNode node) {

    if (node == null) {
      return null;
    }

    Variable var = null;
    if (node instanceof FcharacterRef) {
      var = getVariable((FcharacterRef) node);
    } else if (node instanceof FmemberRef) {
      var = getVariable((FmemberRef) node);
    } else if (node instanceof FcoArrayRef) {
      var = getVariable((FcoArrayRef) node);
    } else if (node instanceof FarrayRef) {
      var = getVariable((FarrayRef) node);
    } else if (node instanceof Var) {
      var = getVariable((Var) node);
    } else if (node instanceof Value) {
      var = getVariable((Value) node);
    } else if (node instanceof VarRef) {
      var = getVariable((VarRef) node);
    }

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from Value element.
   *
   * @param node Value element
   * @return DB :: Variable class
   */
  public Variable getVariable(Value node) {

    if (node == null) {
      return null;
    }

    // Variable name
    IXmlNode value = XmlNodeUtil.getXmlNodeChoice(node);

    // Variable class generation
    Variable var = getVariable(value);

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from Var element.
   *
   * @param node Var element
   * @return DB :: Variable class
   */
  public Variable getVariable(Var node) {

    if (node == null) {
      return null;
    }

    // Variable name
    String value = node.getValue();

    // Variable class generation
    Variable var = new Variable(value);

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from FmemberRef element.
   *
   * @param node FmemberRef element
   * @return DB :: Variable class
   */
  public Variable getVariable(FmemberRef node) {

    if (node == null) {
      return null;
    }

    // Variable
    VarRef varNode = node.getVarRef();
    String member = node.getMember();

    // Variable class generation
    Variable var = getVariable(varNode);

    // Change the structure member name to the added variable name
    String name = var.getName();
    name = name + "%" + member;
    var.setName(name);

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from FarrayRef element.
   *
   * @param node FarrayRef element
   * @return DB :: Variable class
   */
  public Variable getVariable(FarrayRef node) {

    if (node == null) {
      return null;
    }

    // Variable
    VarRef varNode = node.getVarRef();
    List<IXmlNode> indexes = node.getIndexRangeOrArrayIndexOrFarrayConstructor();

    // Variable class generation
    Variable var = getVariable(varNode);
    List<Expression> list = getIndexValues(indexes);
    var.setDimensionIndexValue(list);

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from FcoArrayRef element.
   *
   * @param node FcoArrayRef element
   * @return DB :: Variable class
   */
  public Variable getVariable(FcoArrayRef node) {

    if (node == null) {
      return null;
    }

    // Variable
    VarRef varNode = node.getVarRef();
    List<ArrayIndex> indexes = node.getArrayIndex();

    // Variable class generation
    Variable var = getVariable(varNode);
    List<Expression> list = getArrayIndexValues(indexes);
    var.setDimensionIndexValue(list);

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from FcharacterRef element.
   *
   * @param node FcharacterRef element
   * @return DB :: Variable class
   */
  public Variable getVariable(FcharacterRef node) {

    if (node == null) {
      return null;
    }

    // Variable
    VarRef varNode = node.getVarRef();
    // Variable class generation
    Variable var = getVariable(varNode);

    IndexRange indexRange = node.getIndexRange();
    List<Expression> indexes = getIndexRangeValues(indexRange);
    var.setDimensionIndexValue(indexes);

    return var;
  }

  /**
   * Parse and generate DB :: Variable class from VarRef element.
   *
   * @param node VarRef element
   * @return DB :: Variable class
   */
  public Variable getVariable(VarRef node) {

    if (node == null) {
      return null;
    }

    // Get child elements
    IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
    Variable var = getVariable(child);

    return var;
  }

  /**
   * Get a list of array subscripts
   *
   * @param index IndexRange element
   * @return Array subscript list
   */
  private List<Expression> getIndexRangeValues(IndexRange index) {
    if (index == null) return null;

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(this.typeManager, index);
    Expression expr = exprParser.getExpression();
    if (expr == null) return null;

    List<Expression> list = new ArrayList<Expression>();
    list.add(expr);

    return list;
  }

  /**
   * Get a list of array subscripts
   *
   * @param index IXmlNode element list
   * @return Array subscript list
   */
  private List<Expression> getIndexValues(List<IXmlNode> indexes) {
    if (indexes == null) return null;

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(this.typeManager);

    List<Expression> list = new ArrayList<Expression>();
    if (indexes != null && indexes.size() > 0) {
      for (IXmlNode index : indexes) {
        try {
          Expression expr = exprParser.getExpression(index);
          list.add(expr);
        } catch (XcodeMLException e) {
          e.printStackTrace();
        }
      }
    }
    if (list.size() <= 0) return null;

    return list;
  }

  /**
   * Get a list of array subscripts
   *
   * @param index ArrayIndex Element list
   * @return Array subscript list
   */
  private List<Expression> getArrayIndexValues(List<ArrayIndex> indexes) {
    if (indexes == null) return null;
    List<IXmlNode> list = new ArrayList<IXmlNode>();
    for (ArrayIndex index : indexes) {
      list.add(index);
    }
    return getIndexValues(list);
  }

  /**
   * Parse and generate DB :: Variable class list from VarList element.
   *
   * @param node VarList element
   * @return DB :: Variable class list
   */
  public List<Variable> getVariableList(VarList node) {
    if (node == null) return null;

    List<IXmlNode> nodes = node.getVarRefOrFdoLoop();
    if (nodes == null) return null;

    List<Variable> list = new ArrayList<Variable>();
    for (IXmlNode xmlNode : nodes) {
      if (xmlNode instanceof FdoLoop) {
        List<Value> valList = ((FdoLoop) xmlNode).getValue();
        for (Value value : valList) {
          Variable var = getVariable(value);
          if (var != null) {
            list.add(var);
          }
        }
      } else {
        Variable var = getVariable(xmlNode);
        if (var != null) {
          list.add(var);
        }
      }
    }
    if (list.size() <= 0) return null;

    return list;
  }
}
