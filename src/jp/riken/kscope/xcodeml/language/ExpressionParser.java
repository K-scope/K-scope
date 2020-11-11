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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.KeywordArgument;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.xcodeml.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.FdoStatementSequence;
import jp.riken.kscope.xcodeml.xml.IDefModelExpr;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * Expression parser class
 *
 * @author RIKEN
 */
public class ExpressionParser {

  /** Perth IXmlNode element */
  private IXmlNode parseNode;
  /** typeTable */
  private XcodeMLTypeManager typeManager;
  /** Parent class of IndexRange */
  private Class<?> parentIndexRange;
  /** External procedure list */
  private Map<String, IVariableType> externalFunctionList;

  // Operator string
  /** Comma */
  private final String EXPR_COMMA = ",";
  /** plusExpr +: Addition */
  private final String EXPR_PLUS = "+";
  /** minusExpr-: Subtraction */
  private final String EXPR_MINUS = "-";
  /** mulExpr *: Multiplication */
  private final String EXPR_MUL = "*";
  /** divExpr /: Division */
  private final String EXPR_DIV = "/";
  /** FpowerExpr **: Exponentiation */
  private final String EXPR_POWER = "**";
  /** FconcatExpr //: Concatenation of character expressions */
  private final String EXPR_CONCAT = "//";
  /** logEQExpr == .EQ .: Equivalent */
  private final String EXPR_LOGEQ = "==";
  /** logNEQExpr /= .NE .: Non-equivalent */
  private final String EXPR_LOGNEQ = "/=";
  /** logGEExpr> = .GE .: Greater or equivalent */
  private final String EXPR_LOGGE = ">=";
  /** logGTExpr> .GT .: Greater */
  private final String EXPR_LOGGT = ">";
  /** logLEExpr <= .LE .: Less or equivalent */
  private final String EXPR_LOGLE = "<=";
  /** logLTExpr <.LT .: Lesser */
  private final String EXPR_LOGLT = "<";
  /** logAndExpr .AND .: AND */
  private final String EXPR_LOGAND = ".AND.";
  /** logOrExpr .OR .: OR */
  private final String EXPR_LOGOR = ".OR.";
  /** logEQVExpr .EQV .: Logical equivalent */
  private final String EXPR_LOGEQV = ".EQV.";
  /** logNEQVExpr .NEQV .: Logical unequal */
  private final String EXPR_LOGNEQV = ".NEQV.";
  /** unaryMinusExpr-: Sign inversion */
  private final String EXPR_UNARYMINUS = "-";
  /** logNotExpr .NOT .: Logical negation */
  private final String EXPR_LOGNOT = ".NOT.";
  /** Left parenthesis ( */
  private final String EXPR_PARENLEFT = "(";
  /** Right parenthesis) */
  private final String EXPR_PARENRIGHT = ")";
  /** FmemberRef%: Structure member */
  @SuppressWarnings("unused")
  private final String EXPR_TYPEMEMBER = "%";
  /** Left bracket [ */
  private final String EXPR_COARRAYLEFT = "[";
  /** Right bracket] */
  private final String EXPR_COARRAYRIGHT = "[";
  /** Equal sign = */
  private final String EXPR_EQUAL = "=";
  /** Space */
  private final String EXPR_SPACE = " ";
  /** FarrayConstructor Left initialization parentheses */
  private final String EXPR_ARRAYLEFT = "(/";
  /** FarrayConstructor Right initialization parentheses */
  private final String EXPR_ARRAYRIGHT = "/)";
  /** FarrayRef Array delimiter colon: */
  private final String EXPR_ARRAYCOLON = ":";

  /**
   * Constructor
   *
   * @param typeManager typeTable
   */
  public ExpressionParser(XcodeMLTypeManager typeManager) {
    this.typeManager = typeManager;
  }

  /**
   * Constructor
   *
   * @param typeManager typeTable
   * @param node Perspective exprModel element
   */
  public ExpressionParser(XcodeMLTypeManager typeManager, IXmlNode node) {
    this(typeManager);
    this.parseNode = node;
  }

  /**
   * Get the perspective IXmlNode element
   *
   * @return Perspective IXmlNode element
   */
  public IXmlNode getParseNode() {
    return parseNode;
  }

  /**
   * Set the perspective IXmlNode element
   *
   * @param parseNode parse IXmlNode element
   */
  public void setParseNode(IXmlNode parseNode) {
    this.parseNode = parseNode;
  }

  /**
   * Get typeTable
   *
   * @return typeTable
   */
  public XcodeMLTypeManager getTypeManager() {
    return typeManager;
  }

  /**
   * Set typeTable
   *
   * @param typeManager typeTable
   */
  public void setTypeManager(XcodeMLTypeManager typeManager) {
    this.typeManager = typeManager;
  }

  /**
   * Get an expression class object from an element.
   *
   * @return expression class object
   */
  public Expression getExpression() {
    if (this.parseNode == null) return null;
    Expression expr = null;
    try {
      expr = getExpression(this.parseNode);
    } catch (XcodeMLException e) {
      e.printStackTrace();
    }

    return expr;
  }

  /**
   * Get an expression class object from an element.
   *
   * @param node IXmlNode element
   * @return expression class object
   * @throws XcodeMLException Parsing error
   */
  public Expression getExpression(IXmlNode node) throws XcodeMLException {
    if (node == null) return null;

    Expression expr = null;
    if (node instanceof FunctionCall) expr = getExpression((FunctionCall) node);
    else if (node instanceof FcharacterRef) expr = getExpression((FcharacterRef) node);
    else if (node instanceof FmemberRef) expr = getExpression((FmemberRef) node);
    else if (node instanceof FcoArrayRef) expr = getExpression((FcoArrayRef) node);
    else if (node instanceof FdoLoop) expr = getExpression((FdoLoop) node);
    else if (node instanceof FarrayRef) expr = getExpression((FarrayRef) node);
    else if (node instanceof FcomplexConstant) expr = getExpression((FcomplexConstant) node);
    else if (node instanceof UserUnaryExpr) expr = getExpression((UserUnaryExpr) node);
    /*
     * DefModelBinaryOperation element
     * DivExpr
     * FconcatExpr
     * FpowerExpr
     * LogAndExpr
     * LogEQExpr
     * LogEQVExpr
     * LogGEExpr
     * LogGTExpr
     * LogLEExpr
     * LogLTExpr
     * LogNEQExpr
     * LogNEQVExpr
     * LogNotExpr (unary)
     * LogOrExpr
     * MinusExpr
     * MulExpr
     * PlusExpr
     * UnaryMinusExpr (unary)
     * UserBinaryExpr (unary)
     */
    else if (node instanceof LogNotExpr) expr = getExpression((LogNotExpr) node);
    else if (node instanceof UnaryMinusExpr) expr = getExpression((UnaryMinusExpr) node);
    else if (node instanceof UserBinaryExpr) expr = getExpression((UserBinaryExpr) node);
    else if (node instanceof DefModelBinaryOperation)
      expr = getExpression((DefModelBinaryOperation) node);
    else if (node instanceof FintConstant) expr = getExpression((FintConstant) node);
    else if (node instanceof FrealConstant) expr = getExpression((FrealConstant) node);
    else if (node instanceof FcharacterConstant) expr = getExpression((FcharacterConstant) node);
    else if (node instanceof FlogicalConstant) expr = getExpression((FlogicalConstant) node);
    else if (node instanceof Var) expr = getExpression((Var) node);
    /*
     * DefModelExprList
     * 		FarrayConstructor
     * 		FstructConstructor
     */
    else if (node instanceof DefModelExprList) expr = getExpression((DefModelExprList) node);
    else if (node instanceof VarRef) expr = getExpression((VarRef) node);
    else if (node instanceof IndexRange) expr = getExpression((IndexRange) node);
    else if (node instanceof LowerBound) expr = getExpression((LowerBound) node);
    else if (node instanceof UpperBound) expr = getExpression((UpperBound) node);
    else if (node instanceof Step) expr = getExpression((Step) node);
    else if (node instanceof ArrayIndex) expr = getExpression((ArrayIndex) node);
    else if (node instanceof NamedValue) expr = getExpression((NamedValue) node);
    else if (node instanceof Value) expr = getExpression((Value) node);
    else if (node instanceof Condition) expr = getExpression((Condition) node);
    else if (node instanceof Arguments) expr = getExpression((Arguments) node);
    else if (node instanceof IDefModelExpr) expr = getExpression((IDefModelExpr) node);
    else if (node instanceof FcaseLabel) expr = getExpression((FcaseLabel) node);
    else if (node instanceof Params) expr = getExpression((Params) node);
    else if (node instanceof VarList) expr = getExpression((VarList) node);
    else if (node instanceof ValueList) expr = getExpression((ValueList) node);

    return expr;
  }

  /**
   * Create an expression class from the IDefModelExpr element
   *
   * @param node IDefModelExpr element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(IDefModelExpr node) throws XcodeMLException {
    if (node == null) return null;
    IXmlNode def = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(def);

    return expr;
  }

  /**
   * Create an expression class from the FunctionCall element
   *
   * @param node FunctionCall element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FunctionCall node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    // Subroutine name, function name
    Name name = node.getName();
    String funcName = name.getValue();

    // Data type parser
    VariableTypeParser typePaser = new VariableTypeParser(this.typeManager);
    VariableType funcVarType = null;

    // Attribute
    boolean is_intrinsic = false;
    boolean is_external = false;
    boolean is_recursive = false;
    boolean is_puiblic = false;
    boolean is_private = false;
    if (name.getType() != null) {
      IXmlTypeTableChoice typeChoice = typeManager.findType(name);
      if (typeChoice != null) {
        FfunctionType functionTypeElem = (FfunctionType) typeChoice;
        String returnTypeName = functionTypeElem.getReturnType();
        // EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
        is_intrinsic = XmlNodeUtil.isBoolean(functionTypeElem.isIsIntrinsic());
        is_external = XmlNodeUtil.isBoolean(functionTypeElem.isIsExternal());
        is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
        is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
        is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());
        funcVarType = typePaser.parseVarDefFunctionType(functionTypeElem);
      }
    }
    if (funcVarType == null) {
      // Parse variables
      funcVarType = getVariableType(node.getType());
    }

    boolean intrinsic = XmlNodeUtil.isBoolean(node.isIsIntrinsic());

    // FunctionCall expression
    Expression expr = new Expression();

    // Add buffer: Function name
    buf.append(name.getValue());

    // Add buffer: Left parenthesis
    buf.append(EXPR_PARENLEFT);

    // Get formal arguments
    Arguments args = node.getArguments();
    Expression[] argsExpr = getExpressionArray(args);
    if (argsExpr != null) {
      int count = 0;
      for (Expression arg : argsExpr) {
        if (count > 0) {
          buf.append(EXPR_COMMA);
        }
        buf.append(arg.getLine());
        count++;
      }
      // Generate one Expression
      // Expression exprFuncArg = mergeExpression(argsExpr);
      // expr = mergeExpression(new Expression[]{expr, exprFuncArg});
    }

    // Add buffer: Right parenthesis
    buf.append(EXPR_PARENRIGHT);

    // Add arguments to built-in functions
    List<Expression> argslist = null;
    if (argsExpr != null) {
      argslist = new ArrayList<Expression>(java.util.Arrays.asList(argsExpr));
    }
    ProcedureUsage proc = new ProcedureUsage(name.getValue(), argslist);
    // Built-in function flag set
    if (intrinsic || is_intrinsic) {
      proc.setIntrinsic();
    }

    // add a function
    expr.addFuncCall(proc);
    // Data type
    expr.setVariableType(funcVarType);

    // FunctionCall: expression string
    expr.setLine(buf.toString());

    // Add to external procedure list
    if (is_external && funcVarType != null) {
      addExternalFunction(funcName, funcVarType);
    }
    return expr;
  }

  /**
   * Create an expression class from the Arguments elements
   *
   * @param node Arguments element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(Arguments node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    // Get formal arguments
    Expression[] argsExpr = getExpressionArray(node);
    if (argsExpr == null) return null;

    int count = 0;
    for (Expression arg : argsExpr) {
      if (count > 0) {
        buf.append(EXPR_COMMA);
      }
      buf.append(arg.getLine());
      count++;
    }

    // Generate one Expression
    Expression expr = mergeExpression(argsExpr);

    // Arguments: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the Arguments elements
   *
   * @param node Arguments element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  public Expression[] getExpressionArray(Arguments node) throws XcodeMLException {

    // Get formal arguments
    List<Expression> argsExpr = new ArrayList<Expression>();
    if (node != null && node.getFintConstantOrFrealConstantOrFcomplexConstant() != null) {
      List<IXmlNode> list = node.getFintConstantOrFrealConstantOrFcomplexConstant();
      for (IXmlNode arg : list) {
        Expression argExpr = getExpression(arg);
        argsExpr.add(argExpr);
      }
    }

    if (argsExpr.size() <= 0) return null;

    return argsExpr.toArray(new Expression[0]);
  }

  /**
   * Create an expression class from the UserBinaryExpr (INTERFACE dependent) element
   *
   * @param node UserBinaryExpr (INTERFACE dependent) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(UserBinaryExpr node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    List<IXmlNode> content = node.getContent();

    String name = node.getName();
    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;

    List<Expression> list = new ArrayList<Expression>();
    if (leftExpr != null) {
      Expression var = getExpression(leftExpr);
      if (var != null) {
        list.add(var);

        // Add buffer
        buf.append(var.getLine());
        buf.append(EXPR_SPACE);
      }
    }

    // Add operator name
    buf.append(name);
    buf.append(EXPR_SPACE);

    if (rightExpr != null) {
      Expression var = getExpression(rightExpr);
      if (var != null) {
        list.add(var);
        // Add buffer
        buf.append(var.getLine());
        buf.append(EXPR_SPACE);
      }
    }
    if (list.size() <= 0) return null;

    // Generate one Expression
    Expression expr = mergeExpression(list.toArray(new Expression[0]));
    // Parse variables
    VariableType varType = getVariableType(node.getType());
    expr.setVariableType(varType);

    // UserBinaryExpr: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the DefModelBinaryOperation element
   *
   * @param node DefModelBinaryOperation element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(DefModelBinaryOperation node) throws XcodeMLException {

    // Expression buffer
    StringBuffer buf = new StringBuffer();

    List<IXmlNode> content = node.getContent();

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;

    List<Expression> list = new ArrayList<Expression>();
    if (leftExpr != null) {
      Expression var = getExpression(leftExpr);
      if (var != null) {
        list.add(var);
        // Add buffer
        buf.append(var.getLine());
        buf.append(EXPR_SPACE);
      }
    }

    // Add to buffer: Operator
    String op = getOperation(node);
    // Add buffer
    buf.append(op);
    buf.append(EXPR_SPACE);

    if (rightExpr != null) {
      Expression var = getExpression(rightExpr);
      if (var != null) {
        list.add(var);
        // Add buffer
        buf.append(var.getLine());
      }
    }
    if (list.size() <= 0) return null;

    // Generate one Expression
    Expression expr = mergeExpression(list.toArray(new Expression[0]));

    // (2012/4/18) changed by tomiyama and teraim Changed to target only floating point arithmetic
    String attr = node.getType();
    if (!attr.equals(new String("Fint"))) {
      if (node instanceof PlusExpr) {
        // Addition increment
        expr.incrementAdd();
      } else if (node instanceof MinusExpr) {
        // subtraction increment
        expr.incrementSub();
      } else if (node instanceof MulExpr) {
        // Multiplication increment
        expr.incrementMul();
      } else if (node instanceof DivExpr) {
        // Increment of division
        expr.incrementDiv();
      } else if (node instanceof FpowerExpr) {
        // Increment of power calculation
        expr.incrementPow();
      }
    }

    // Parse variables
    VariableType varType = getVariableType(node.getType());
    expr.setVariableType(varType);

    // DefModelBinaryOperation: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the LogNotExpr (logical negation) element
   *
   * @param node LogNotExpr (logical negation) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(LogNotExpr node) throws XcodeMLException {

    // Expression buffer
    StringBuffer buf = new StringBuffer();

    List<IXmlNode> contents = node.getContent();

    // Add buffer:'.NOT.': Logical negation
    buf.append(EXPR_LOGNOT);
    buf.append(EXPR_SPACE);

    List<Expression> list = new ArrayList<Expression>();
    if (contents != null && contents.size() > 0) {
      for (IXmlNode content : contents) {
        Expression expr = getExpression(content);
        if (expr != null) {
          list.add(expr);

          // Add buffer
          buf.append(expr.getLine());
          buf.append(EXPR_SPACE);
        }
      }
    }
    if (list.size() <= 0) return null;

    // Generate one Expression
    Expression expr = mergeExpression(list.toArray(new Expression[0]));

    // Parse variables
    VariableType varType = getVariableType(node.getType());
    expr.setVariableType(varType);

    // LogNotExpr: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the UnaryMinusExpr (sign inversion) element
   *
   * @param node UnaryMinusExpr (sign inversion) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(UnaryMinusExpr node) throws XcodeMLException {

    // Expression buffer
    StringBuffer buf = new StringBuffer();

    List<IXmlNode> contents = node.getContent();

    // Add buffer:'-': Sign inversion
    buf.append(EXPR_UNARYMINUS); // '-': Sign inversion

    List<Expression> list = new ArrayList<Expression>();
    if (contents != null && contents.size() > 0) {
      for (IXmlNode content : contents) {
        Expression expr = getExpression(content);
        if (expr != null) {
          list.add(expr);

          // Add buffer
          buf.append(expr.getLine());
          buf.append(EXPR_SPACE);
        }
      }
    }
    if (list.size() <= 0) return null;

    // Generate one Expression
    Expression expr = mergeExpression(list.toArray(new Expression[0]));

    // Parse variables
    VariableType varType = getVariableType(node.getType());
    expr.setVariableType(varType);

    // UnaryMinusExpr: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from Condition elements
   *
   * @param node Condition element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(Condition node) throws XcodeMLException {

    IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(child);

    return expr;
  }

  /**
   * Create an expression class from FcharacterRef (see substring) elements
   *
   * @param node FcharacterRef (see substring) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FcharacterRef node) throws XcodeMLException {

    // IndexRange call class: FcharacterRef
    this.parentIndexRange = node.getClass();

    String type = node.getType();

    // Parse variables
    VariableType varType = getVariableType(type);

    // Get child elements
    VarRef varRef = node.getVarRef();
    IndexRange indexRange = node.getIndexRange();

    // Substring variable
    Expression exprVar = getExpression(varRef);

    // index
    Expression exprIndex = getExpression(indexRange);

    // Parse variables
    VariableParser varParser = new VariableParser(this.typeManager);
    Variable var = varParser.getVariable(node);

    // Expression class generation
    Expression expr = mergeExpression(new Expression[] {exprVar, exprIndex});
    expr.setVariableType(varType);
    if (var != null) {
      expr.addVariable(var);
    }

    // FcharacterRef: Expression string
    expr.setLine(var.getVariableString());

    return expr;
  }

  /**
   * Create an expression class from FmemberRef (struct member reference) elements
   *
   * @param node FmemberRef (see structure member) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FmemberRef node) throws XcodeMLException {

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Get child elements
    VarRef varRef = node.getVarRef();

    // Expression class of child element
    Expression exprVar = getExpression(varRef);

    // Expression class generation
    exprVar.setVariableType(varType);

    // Parse variables
    VariableParser varParser = new VariableParser(this.typeManager);
    Variable var = varParser.getVariable(node);

    // Structure reference settings
    if (var != null) {
      exprVar.addVariable(var);
    }

    // FmemberRef: Expression string
    exprVar.setLine(var.getVariableString());

    return exprVar;
  }

  /**
   * Create an expression class from the FcoArrayRef (see coarray) element
   *
   * @param node FcoArrayRef (see coarray) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FcoArrayRef node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Get child elements
    VarRef varRef = node.getVarRef();
    List<ArrayIndex> arrayIndexes = node.getArrayIndex();

    // Expression class of child element
    Expression exprVar = getExpression(varRef);
    // Add buffer
    buf.append(exprVar.getLine());

    // Add buffer: [
    buf.append(EXPR_COARRAYLEFT);

    List<Expression> list = new ArrayList<Expression>();
    Expression exprIndex = null;
    if (arrayIndexes != null && arrayIndexes.size() > 0) {
      int count = 0;
      for (ArrayIndex index : arrayIndexes) {
        if (count > 0) {
          // Add buffer
          buf.append(EXPR_COMMA); // Comma
        }
        Expression expr = getExpression(index);
        // Add buffer
        buf.append(expr.getLine());
        list.add(expr);
        count++;
      }
    }
    if (list.size() > 0) {
      exprIndex = mergeExpression(list.toArray(new Expression[0]));
    }

    // Add buffer:]
    buf.append(EXPR_COARRAYRIGHT);

    // Parse variables
    VariableParser varParser = new VariableParser(this.typeManager);
    Variable var = varParser.getVariable(node);

    // Expression class generation
    Expression expr = mergeExpression(new Expression[] {exprVar, exprIndex});
    expr.setVariableType(varType);
    if (var != null) {
      expr.addVariable(var);
    }

    // FcoArrayRef: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from FdoLoop (DO type iteration) elements
   *
   * @param node FdoLoop (DO type iteration) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FdoLoop node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    // IndexRange call class: FdoLoop
    this.parentIndexRange = node.getClass();

    // Get child elements
    Var var = node.getVar();
    IndexRange indexRange = node.getIndexRange();
    List<Value> values = node.getValue();

    // Expression class of child element
    // Add buffer: Left parenthesis
    buf.append(EXPR_PARENLEFT); // Left parenthesis (

    List<Expression> list = new ArrayList<Expression>();
    Expression exprValue = null;
    if (values != null && values.size() > 0) {
      int count = 0;
      for (Value value : values) {
        if (count > 0) {
          // Add buffer
          buf.append(EXPR_COMMA); // Comma
        }
        Expression expr = getExpression(value);
        // Add buffer
        buf.append(expr.getLine());
        list.add(expr);
        count++;
      }
    }
    if (list.size() > 0) {
      exprValue = mergeExpression(list.toArray(new Expression[0]));
    }

    Expression exprVar = getExpression(var);

    // Add buffer: =
    buf.append(EXPR_EQUAL); // =

    // IndexRange
    Expression exprIndex = getExpression(indexRange);
    // Add buffer
    buf.append(exprIndex.getLine());

    // Add buffer: right parenthesis)
    buf.append(EXPR_PARENRIGHT); // Right parenthesis)

    // Expression class generation
    Expression expr = mergeExpression(new Expression[] {exprVar, exprIndex, exprValue});

    // FdoLoop: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from a FarrayRef (see subarray or array element) element. (Example)
   * a (1: 3) = 5
   *
   * @param node FarrayRef (see subarray or array element) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FarrayRef node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    // IndexRange call class: FarrayRef
    this.parentIndexRange = node.getClass();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Get child elements
    VarRef varref = node.getVarRef();
    List<IXmlNode> indexes = node.getIndexRangeOrArrayIndexOrFarrayConstructor();

    // Variable name
    Expression exprVarRef = getExpression(varref);

    // Parse variables
    VariableParser varParser = new VariableParser(this.typeManager);
    // Variable
    Variable arrayVar = varParser.getVariable(node);

    if (arrayVar == null) {
      if (exprVarRef.getVariables() != null && exprVarRef.getVariables().size() > 0) {
        arrayVar = exprVarRef.getVariables().get(0);
      }
    }

    // Add buffer: Variable name
    buf.append(arrayVar.getName());

    // Add buffer: Left parenthesis
    buf.append(EXPR_PARENLEFT); // Left parenthesis (

    List<Expression> list = new ArrayList<Expression>();
    Expression exprIndex = null;
    if (indexes != null && indexes.size() > 0) {
      int count = 0;
      for (IXmlNode index : indexes) {
        if (count != 0) buf.append(EXPR_COMMA);
        Expression expr = getExpression(index);
        list.add(expr);
        // Add buffer: Variable name
        buf.append(expr.getLine());
        count++;
      }
    }

    // Add buffer: Right parenthesis
    buf.append(EXPR_PARENRIGHT); // Right parenthesis)
    if (list.size() > 0) {
      //            exprIndex = mergeExpression(list.toArray(new Expression[0]));
    }

    // Expression class generation
    //        Expression expr = mergeExpression(new Expression[]{exprVarRef, exprIndex});
    Expression expr = exprVarRef;
    expr.setVariableType(varType);
    if (arrayVar != null) {
      // expr.addVariable(arrayVar);
      List<Variable> vars = new ArrayList<Variable>();
      vars.addAll(Arrays.asList(new Variable[] {arrayVar}));
      expr.setVariables(vars);
    }
    // FarrayRef: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from FcomplexConstant elements
   *
   * @param node FcomplexConstant element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FcomplexConstant node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Get child elements
    List<IXmlNode> contents = node.getContent();

    // Add buffer: Left parenthesis
    buf.append(EXPR_PARENLEFT); // Left parenthesis (

    // Expression class of child element
    List<Expression> list = new ArrayList<Expression>();
    Expression exprContent = null;
    if (contents != null && contents.size() > 0) {
      int count = 0;
      for (IXmlNode content : contents) {
        if (count > 0) {
          // Add buffer
          buf.append(EXPR_COMMA); // Comma
        }
        Expression expr = getExpression(content);
        // Add buffer
        buf.append(expr.getLine());
        list.add(expr);
        count++;
      }
    }
    if (list.size() <= 0) return null;

    // Add buffer: Right parenthesis
    buf.append(EXPR_PARENRIGHT); // Right parenthesis)

    exprContent = mergeExpression(list.toArray(new Expression[0]));

    // Expression class generation
    Expression expr = exprContent;
    expr.setVariableType(varType);

    // FcomplexConstant: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from FrealConstant elements
   *
   * @param node FrealConstant (floating point constant) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FrealConstant node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // constant number
    String value = node.getValue();

    // Add buffer: Value
    buf.append(value);

    // Expression class generation
    Expression expr = new Expression(value);
    expr.setVariableType(varType);

    // FrealConstant: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the FcharacterConstant element
   *
   * @param node FcharacterConstant (string constant) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FcharacterConstant node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // constant number
    String value = node.getValue();

    // Add buffer: Value
    buf.append("\"" + value + "\"");

    // Expression class generation
    Expression expr = new Expression(value);
    expr.setVariableType(varType);

    // FcharacterConstant: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from FintConstant (integer constant) elements
   *
   * @param node FintConstant (integer constant) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FintConstant node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // constant number
    String value = node.getValue();

    // Add buffer: Value
    buf.append(value);

    // Expression class generation
    Expression expr = new Expression(value);
    expr.setVariableType(varType);

    // FintConstant: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from FlogicalConstant elements
   *
   * @param node FlogicalConstant (integer constant) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FlogicalConstant node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // constant number
    String value = node.getValue();

    // Add buffer: Value
    buf.append(value);

    // Expression class generation
    Expression expr = new Expression(value);
    expr.setVariableType(varType);

    // FlogicalConstant: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the UserUnaryExpr (unary expression: INTERFACE dependent)
   * element
   *
   * @param node UserUnaryExpr (unary operation: INTERFACE dependent) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(UserUnaryExpr node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Add buffer: Left parenthesis
    buf.append(EXPR_PARENLEFT); // Left parenthesis (

    // Get child elements
    IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(child);
    // Add buffer
    buf.append(expr.getLine());

    // Add buffer: Right parenthesis
    buf.append(EXPR_PARENRIGHT); // Right parenthesis)

    // Expression class generation
    expr.setVariableType(varType);

    // UserUnaryExpr: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from DefModelExprList (array constructor, struct constructor)
   * elements
   *
   * @param node DefModelExprList (array constructor, structure constructor) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(DefModelExprList node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    String type = null;
    if (node instanceof FarrayConstructor) {
      type = ((FarrayConstructor) node).getType();
      // Add buffer: (/
      buf.append(EXPR_ARRAYLEFT);
    } else if (node instanceof FstructConstructor) {
      type = ((FstructConstructor) node).getType();
      // Add buffer: (
      buf.append(EXPR_PARENLEFT); // Left parenthesis (
    }

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Get child elements
    List<IXmlNode> models = node.getDefModelExpr();

    // Expression class of child element
    List<Expression> list = new ArrayList<Expression>();
    Expression exprContent = null;
    if (models != null && models.size() > 0) {
      int count = 0;
      for (IXmlNode model : models) {
        if (count > 0) {
          // Add buffer: comma
          buf.append(EXPR_COMMA); // Comma
        }
        Expression expr = getExpression(model);
        // Add buffer
        buf.append(expr.getLine());
        list.add(expr);
        count++;
      }
    }
    if (list.size() <= 0) return null;

    exprContent = mergeExpression(list.toArray(new Expression[0]));

    if (node instanceof FarrayConstructor) {
      // Add buffer: /)
      buf.append(EXPR_ARRAYRIGHT);
    } else if (node instanceof FstructConstructor) {
      // Add buffer :)
      buf.append(EXPR_PARENRIGHT); // Right parenthesis)
    }

    // Expression class generation
    Expression expr = exprContent;
    expr.setVariableType(varType);

    // DefModelExprList: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the Var (variable name) element
   *
   * @param node Var (variable name)
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(Var node) throws XcodeMLException {

    // Parse variable data type
    VariableTypeParser typeParser = new VariableTypeParser(this.typeManager);
    VariableType varType = typeParser.parseVariableType(node);

    // Expression class generation
    Expression expr = new Expression();

    // Parse variables
    VariableParser varParser = new VariableParser(this.typeManager);
    Variable var = varParser.getVariable(node);

    // Add variable
    if (var != null) {
      expr.addVariable(var);
    }
    expr.setVariableType(varType);

    // Var: expression string
    expr.setLine(var.getVariableString());

    return expr;
  }

  /**
   * Create an expression class from a VarRef element
   *
   * @param node VarRef (variable reference) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(VarRef node) throws XcodeMLException {
    String type = node.getType();

    // Parse variable data type
    VariableType varType = getVariableType(type);

    // Get child elements
    IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(child);

    // Expression class generation
    expr.setVariableType(varType);

    return expr;
  }

  /**
   * Create an expression class from the IndexRange element
   *
   * @param node Index Range element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(IndexRange node) throws XcodeMLException {

    // Expression buffer
    StringBuffer buf = new StringBuffer();

    // Index Range delimiter
    String delim = EXPR_ARRAYCOLON;
    if (this.parentIndexRange != null) {
      if (this.parentIndexRange.equals(FdoLoop.class)) {
        // Parent node is FdoLoop
        delim = EXPR_COMMA;
      } else if (this.parentIndexRange.equals(FdoStatementSequence.class)) {
        // Parent node is FdoStatementSequence
        delim = EXPR_COMMA;
      }
    }

    // Get child elements
    LowerBound lower = node.getLowerBound();
    Expression exprLower = null;
    if (lower != null) {
      IXmlNode nodeLower = XmlNodeUtil.getXmlNodeChoice(lower);
      exprLower = getExpression(nodeLower);
      // Add buffer
      if (exprLower != null) {
        buf.append(exprLower.getLine());
      }
    }

    // Add buffer: IndexRange delimiter
    buf.append(delim);

    UpperBound upper = node.getUpperBound();
    Expression exprUpper = null;
    if (upper != null) {
      IXmlNode nodeUpper = XmlNodeUtil.getXmlNodeChoice(upper);
      exprUpper = getExpression(nodeUpper);
      // Add buffer
      if (exprUpper != null) {
        buf.append(exprUpper.getLine());
      }
    }
    Step step = node.getStep();
    Expression exprStep = null;
    if (step != null) {
      // Add buffer: IndexRange delimiter
      buf.append(delim);

      IXmlNode nodeStep = XmlNodeUtil.getXmlNodeChoice(step);
      exprStep = getExpression(nodeStep);
      // Add buffer
      if (exprStep != null) {
        buf.append(exprStep.getLine());
      }
    }

    // Expression class generation
    Expression expr = mergeExpression(new Expression[] {exprLower, exprUpper, exprStep});

    // IndexRange: Expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the LowerBound element
   *
   * @param node LowerBound element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(LowerBound node) throws XcodeMLException {

    // Get child elements
    IXmlNode nodeLower = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(nodeLower);

    return expr;
  }

  /**
   * Create an expression class from the UpperBound element
   *
   * @param node UpperBound element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(UpperBound node) throws XcodeMLException {

    // Get child elements
    IXmlNode nodeUpper = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(nodeUpper);

    return expr;
  }

  /**
   * Create an expression class from the Step element
   *
   * @param node Step element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(Step node) throws XcodeMLException {

    // Get child elements
    IXmlNode nodeStep = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(nodeStep);

    return expr;
  }

  /**
   * Create an expression class from the ArrayIndex element
   *
   * @param node ArrayIndex element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(ArrayIndex node) throws XcodeMLException {

    // Get child elements
    IXmlNode nodeStep = XmlNodeUtil.getXmlNodeChoice(node);
    Expression expr = getExpression(nodeStep);

    return expr;
  }

  /**
   * Create an expression class from the NamedValue element
   *
   * @param node ArrayIndex element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(NamedValue node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    // name
    String name = node.getName();
    // value
    String value = node.getValue();

    // Add buffer: Name
    buf.append(name);

    // Add buffer: =
    buf.append(EXPR_EQUAL);

    // Expression class generation
    Expression exprValue = null;
    if (value != null) {
      // Add buffer: Value
      buf.append(value);
    } else {
      // Get child elements
      IXmlNode nodeValue = XmlNodeUtil.getXmlNodeChoice(node);
      exprValue = getExpression(nodeValue);
      // Add buffer: Value
      buf.append(exprValue.getLine());
    }

    if (exprValue == null) return null;

    // Keyword argument
    KeywordArgument expr = new KeywordArgument(name, exprValue);
    // NamedValue: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from a Value (arbitrary value represented by an expression) element
   *
   * @param node Value (arbitrary value represented by an expression) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(Value node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    RepeatCount repeat = node.getRepeatCount();
    Expression exprRepeat = null;
    if (repeat != null) {
      IXmlNode nodeRep = XmlNodeUtil.getXmlNodeChoice(repeat);
      exprRepeat = getExpression(nodeRep);
      // Add buffer
      buf.append(exprRepeat.getLine());
      // Add buffer: *
      buf.append(EXPR_MUL); // *
    }

    IXmlNode nodeModel = XmlNodeUtil.getXmlNodeChoice(node);
    Expression exprModel = null;
    if (nodeModel != null) {
      exprModel = getExpression(nodeModel);
      // Add buffer
      buf.append(exprModel.getLine());
    }

    // Expression class generation
    Expression expr = mergeExpression(new Expression[] {exprRepeat, exprModel});

    // Value: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the FcaseLabel (CASE statement in SELECT CASE syntax) element
   *
   * @param node FcaseLabel (CASE statement in SELECT CASE syntax) element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(FcaseLabel node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    buf.append("CASE");
    buf.append(EXPR_SPACE);

    List<IXmlNode> list = node.getValueOrIndexRange();
    List<Expression> exprs = new ArrayList<Expression>();
    if (list != null && list.size() > 0) {
      for (IXmlNode caseNode : list) {
        Expression exprCase = getExpression(caseNode);
        exprs.add(exprCase);

        // Add buffer
        buf.append(exprCase.getLine());
        buf.append(EXPR_SPACE);
      }
    }

    // Expression class generation
    Expression expr = null;
    if (exprs.size() > 0) {
      expr = mergeExpression(exprs.toArray(new Expression[0]));
    } else {
      expr = new Expression();
      buf.append("DEFAULT");
    }

    // CASE: expression string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from Params elements
   *
   * @param node Params element
   * @return expression class
   * @throws XcodeMLException Parsing error
   */
  private Expression getExpression(Params node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    List<Name> list = node.getName();
    List<Expression> exprs = new ArrayList<Expression>();
    if (list != null && list.size() > 0) {
      int count = 0;
      for (Name nameNode : list) {
        Expression expr = new Expression(nameNode.getValue());
        VariableType type = getVariableType(nameNode.getType());
        expr.setVariableType(type);
        exprs.add(expr);

        if (count > 0) {
          buf.append(EXPR_COMMA);
        }
        // Add buffer
        buf.append(nameNode.getValue());
        count++;
      }
    }

    // Expression class generation
    Expression expr = new Expression();
    if (exprs.size() > 0) {
      expr = mergeExpression(exprs.toArray(new Expression[0]));
    }

    // Parameter string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the VarList element
   *
   * @param node VarList element
   * @return expression class list
   * @throws XcodeMLException Parsing error
   */
  public Expression getExpression(VarList node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    Expression[] list = getExpressionArray(node);
    if (list == null || list.length <= 0) return null;

    for (Expression expr : list) {
      int count = 0;
      if (count > 0) {
        // Add buffer
        buf.append(EXPR_COMMA); // Comma
      }
      // Add buffer
      buf.append(expr.getLine());
      count++;
    }

    // Expression class generation
    Expression expr = mergeExpression(list);
    // Parameter string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the ValueList element
   *
   * @param node ValueList element
   * @return expression class list
   * @throws XcodeMLException Parsing error
   */
  public Expression getExpression(ValueList node) throws XcodeMLException {
    // Expression buffer
    StringBuffer buf = new StringBuffer();

    Expression[] list = getExpressionArray(node);
    if (list == null || list.length <= 0) return null;

    for (Expression expr : list) {
      int count = 0;
      if (count > 0) {
        // Add buffer
        buf.append(EXPR_COMMA); // Comma
      }
      // Add buffer
      buf.append(expr.getLine());
      count++;
    }

    // Expression class generation
    Expression expr = mergeExpression(list);
    // Parameter string
    expr.setLine(buf.toString());

    return expr;
  }

  /**
   * Create an expression class from the VarList element
   *
   * @param node VarList element
   * @return expression class list
   * @throws XcodeMLException Parsing error
   */
  public Expression[] getExpressionArray(VarList node) throws XcodeMLException {
    // IXmlNode list
    List<IXmlNode> vars = node.getVarRefOrFdoLoop();

    // Expression class of child element
    List<Expression> list = new ArrayList<Expression>();
    if (vars != null && vars.size() > 0) {
      for (IXmlNode var : vars) {
        Expression expr = getExpression(var);
        list.add(expr);
      }
    }

    if (list.size() <= 0) return null;

    return list.toArray(new Expression[0]);
  }

  /**
   * Create an expression class from the ValueList element
   *
   * @param node ValueList element
   * @return expression class list
   * @throws XcodeMLException Parsing error
   */
  public Expression[] getExpressionArray(ValueList node) throws XcodeMLException {

    // Value list
    List<Value> values = node.getValue();

    // Expression class of child element
    List<Expression> list = new ArrayList<Expression>();
    if (values != null && values.size() > 0) {
      for (Value value : values) {
        Expression expr = getExpression(value);
        list.add(expr);
      }
    }

    if (list.size() <= 0) return null;

    return list.toArray(new Expression[0]);
  }

  /**
   * Aggregate Expression
   *
   * @param exprs Expression list
   * @return Total Expression
   */
  private Expression mergeExpression(Expression[] exprs) {
    if (exprs == null) return null;

    Expression exprMerge = new Expression();
    List<Variable> listVar = new ArrayList<Variable>();
    List<ProcedureUsage> listFunc = new ArrayList<ProcedureUsage>();
    int addCount = 0;
    int subCount = 0;
    int mulCount = 0;
    int divCount = 0;
    int powCount = 0;
    IVariableType type = null;

    for (Expression expr : exprs) {
      if (expr == null) continue;

      listVar.addAll(expr.getVariables());
      listFunc.addAll(expr.getFuncCalls());
      addCount += expr.getAddCount();
      subCount += expr.getSubCount();
      mulCount += expr.getMulCount();
      divCount += expr.getDivCount();
      powCount += expr.getPowCount();
      if (type == null && expr.getType() != null) {
        type = expr.getType();
      }
    }
    exprMerge.setVariables(listVar);
    exprMerge.setFuncCalls(listFunc);
    exprMerge.setAddCount(addCount);
    exprMerge.setSubCount(subCount);
    exprMerge.setMulCount(mulCount);
    exprMerge.setDivCount(divCount);
    exprMerge.setPowCount(powCount);
    exprMerge.setVariableType(type);

    return exprMerge;
  }

  /**
   * Get variable data type
   *
   * @param typename Data type name
   * @return variable data type
   */
  private VariableType getVariableType(String typename) {
    if (typename == null || typename.isEmpty()) return null;

    // Parse variables
    VariableTypeParser typeParser = new VariableTypeParser(this.typeManager);
    VariableType varType = typeParser.parseVariableType(typename);

    return varType;
  }

  /**
   * Get the operator
   *
   * @param node XML node
   * @return operator string
   */
  private String getOperation(IXmlNode node) {

    if (node instanceof PlusExpr) {
      // Addition
      return (EXPR_PLUS);
    } else if (node instanceof MinusExpr) {
      // Subtraction
      return (EXPR_MINUS);
    } else if (node instanceof MulExpr) {
      // Multiply
      return (EXPR_MUL);
    } else if (node instanceof DivExpr) {
      // Division
      return (EXPR_DIV);
    } else if (node instanceof FpowerExpr) {
      // Power calculation
      return (EXPR_POWER);
    } else if (node instanceof LogLEExpr) {
      // logLEExpr <= .LE .: less or equivalent
      return (EXPR_LOGLE);
    } else if (node instanceof FconcatExpr) {
      // FconcatExpr //: Concatenation of character expressions
      return (EXPR_CONCAT);
    } else if (node instanceof LogEQExpr) {
      // logEQExpr == .EQ .: Equivalent
      return (EXPR_LOGEQ);
    } else if (node instanceof LogNEQExpr) {
      // logNEQExpr / = .NE .: Non-equivalent
      return (EXPR_LOGNEQ);
    } else if (node instanceof LogGEExpr) {
      // logGEExpr> = .GE .: Greater or equivalent
      return (EXPR_LOGGE);
    } else if (node instanceof LogGTExpr) {
      // logGTExpr> .GT .: Greater
      return (EXPR_LOGGT);
    } else if (node instanceof LogLTExpr) {
      // logLTExpr <.LT .: less
      return (EXPR_LOGLT);
    } else if (node instanceof LogAndExpr) {
      // logAndExpr .AND .: AND
      return (EXPR_LOGAND);
    } else if (node instanceof LogOrExpr) {
      // logOrExpr .OR .: OR
      return (EXPR_LOGOR);
    } else if (node instanceof LogEQVExpr) {
      // logEQVExpr .EQV .: Logical equivalent
      return (EXPR_LOGEQV);
    } else if (node instanceof LogNEQVExpr) {
      // logNEQVExpr .NEQV .: Logical non-equivalent
      return (EXPR_LOGNEQV);
    }

    return "";
  }

  /**
   * Add to external procedure list
   *
   * @param funcName External procedure name
   * @param varType data type
   */
  private void addExternalFunction(String funcName, IVariableType varType) {
    if (this.externalFunctionList == null) {
      this.externalFunctionList = new HashMap<String, IVariableType>();
    }
    if (externalFunctionList.containsKey(funcName)) {
      externalFunctionList.put(funcName, varType);
    }

    return;
  }

  /**
   * Get a list of external procedures
   *
   * @return External procedure list
   */
  public Map<String, IVariableType> getExternalFunction() {
    return this.externalFunctionList;
  }
}
