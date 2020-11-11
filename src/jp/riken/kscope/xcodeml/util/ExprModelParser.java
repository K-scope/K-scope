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
package jp.riken.kscope.xcodeml.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import jp.riken.kscope.Message;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.xml.*;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * Perspective class of exprModel element (model commonly used from elements that refer to
 * expression representation)
 */
public class ExprModelParser {
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
  /** logLEExpr <= .LE .: less or equivalent */
  private final String EXPR_LOGLE = "<=";
  /** logLTExpr <.LT .: less */
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
  /** ( */
  private final String EXPR_PARENLEFT = "(";
  /** ) */
  private final String EXPR_PARENRIGHT = ")";
  /** FmemberRef%: Structure member */
  private final String EXPR_TYPEMEMBER = "%";
  /** [ */
  private final String EXPR_COARRAYLEFT = "[";
  /** ] */
  private final String EXPR_COARRAYRIGHT = "[";
  /** Equal sign */
  private final String EXPR_EQUAL = "=";
  /** Space */
  private final String EXPR_SPACE = " ";
  /** FarrayConstructor */
  private final String EXPR_ARRAYLEFT = "(/";
  /** FarrayConstructor */
  private final String EXPR_ARRAYRIGHT = "/)";
  /** : */
  private final String EXPR_ARRAYCOLON = ":";

  // Perspective mode of exprModel element
  /**
   * Perspers only variables. Only variables such as integers, real numbers, and strings are
   * returned.
   */
  public static final int PARSE_VARIABLE = 0x000001;
  /**
   * Do not parse subroutines and function arguments. Returns only subroutine and function names.
   */
  public static final int PARSE_FUNCTION = 0x000010;
  /** Add operator. */
  public static final int PARSE_OPERATOR = 0x000100;
  /** Arguments are separated by commas and are character strings for each argument. */
  public static final int PARSE_ARGUMENTS = 0x001000;
  /** Built-in functions do not parse. */
  public static final int PARSE_NONEINTRINSIC = 0x010000;
  /**
   * Add operator. + Arguments are separated by commas and are character strings for each argument.
   */
  public static final int PARSE_DETAIL = PARSE_OPERATOR | PARSE_ARGUMENTS;

  /** exprModel perspective mode */
  private int _mode;

  /** Perth exprModel element */
  private IXmlNode _parseNode;

  /** typeTable */
  private XcodeMLTypeManager _typeManager;

  /**
   * Constructor
   *
   * @param mode Perspective mode
   * @param typeManager typeTable
   */
  public ExprModelParser(int mode, XcodeMLTypeManager typeManager) {
    this._mode = mode;
    _typeManager = typeManager;
  }

  /**
   * Constructor
   *
   * @param mode Perspective mode
   * @param typeManager typeTable
   * @param node Perspective exprModel element
   */
  public ExprModelParser(int mode, XcodeMLTypeManager typeManager, IXmlNode node) {
    this._mode = mode;
    this._parseNode = node;
    _typeManager = typeManager;
  }

  /** @param parseNode Set _parseNode */
  public void setParseNode(IXmlNode parseNode) {
    _parseNode = parseNode;
  }

  /**
   * Get the variable list from the perspective element.
   *
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList() throws XcodeMLException {
    if (_parseNode == null) return null;

    return getVariableList(_parseNode, false);
  }

  /**
   * Make the perspective node a string.
   *
   * @return expression expression string
   */
  @Override
  public String toString() {
    try {
      String[] vars = null;
      if (_parseNode instanceof IDefModelExpr) {
        vars = getVariableList((IDefModelExpr) _parseNode, false);
      } else {
        vars = getVariableList(_parseNode, false);
      }
      if (vars == null) return null;

      StringBuilder expr = new StringBuilder();
      for (String var : vars) {
        expr.append(var);
      }
      if (expr.length() <= 0) return null;

      return expr.toString().trim();

    } catch (XcodeMLException e) {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Convert variables and operator lists to strings.
   *
   * @param vars Variable, operator list
   * @return expression expression string
   */
  private String toString(String[] vars) {
    StringBuilder expr = new StringBuilder();
    for (String var : vars) {
      expr.append(var);
    }
    if (expr.length() <= 0) return null;

    return expr.toString().trim();
  }

  /**
   * Get the variable list from the Arguments element.
   *
   * @param args Arguments element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(Arguments args, boolean grouping) throws XcodeMLException {
    List<IXmlNode> list = args.getFintConstantOrFrealConstantOrFcomplexConstant();
    if (list == null || list.size() <= 0) return null;

    ArrayList<String> var_list = new ArrayList<String>();
    for (IXmlNode node : list) {
      String[] vars = getVariableList(node, grouping);
      if (vars == null) {
        // exprmodelparser.variablelist.parse.error = Parsing error: Could not parse% s.
        throw new XcodeMLException(
            Message.getString(
                "exprmodelparser.variablelist.parse.error", node.getClass().getName()));
      }

      if ((this._mode & PARSE_ARGUMENTS) != 0x00) {
        // Arguments are separated by commas and are character strings for each argument.
        if (var_list.size() > 0) var_list.add(EXPR_COMMA);
        var_list.add(toString(vars));
      } else {
        addOperatorString(var_list, EXPR_COMMA); // Add comma
        var_list.addAll(Arrays.asList(vars));
      }
    }
    if (var_list.size() <= 0) return null;
    return (String[]) var_list.toArray(new String[var_list.size()]);
  }

  /**
   * Get the variable list from the element list.
   *
   * @param list Element list
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(List<IXmlNode> list, boolean grouping) throws XcodeMLException {
    if (list == null) return null;
    ArrayList<String> var_list = new ArrayList<String>();
    for (IXmlNode node : list) {
      String[] vars = getVariableList(node, grouping);
      addOperatorString(var_list, EXPR_COMMA); // Add comma
      var_list.addAll(Arrays.asList(vars));
    }
    if (var_list.size() <= 0) return null;
    return (String[]) var_list.toArray(new String[var_list.size()]);
  }

  /**
   * Get the variable list from the element.
   *
   * @param expr exprModel element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(IXmlNode expr, boolean grouping) throws XcodeMLException {
    String[] vars = null;
    if (expr instanceof FunctionCall) vars = getVariableList((FunctionCall) expr, grouping);
    else if (expr instanceof UserBinaryExpr)
      vars = getVariableList((UserBinaryExpr) expr, grouping);
    else if (expr instanceof FcharacterRef) vars = getVariableList((FcharacterRef) expr, grouping);
    else if (expr instanceof FmemberRef) vars = getVariableList((FmemberRef) expr, grouping);
    else if (expr instanceof FcoArrayRef) vars = getVariableList((FcoArrayRef) expr, grouping);
    else if (expr instanceof FdoLoop) vars = getVariableList((FdoLoop) expr, grouping);
    else if (expr instanceof FarrayRef) vars = getVariableList((FarrayRef) expr, grouping);
    else if (expr instanceof FcomplexConstant)
      vars = getVariableList((FcomplexConstant) expr, grouping);
    else if (expr instanceof UserUnaryExpr) vars = getVariableList((UserUnaryExpr) expr, grouping);
    else if (expr instanceof FpowerExpr) vars = getVariableList((FpowerExpr) expr, grouping);
    else if (expr instanceof LogLEExpr) vars = getVariableList((LogLEExpr) expr, grouping);
    else if (expr instanceof FintConstant) vars = getVariableList((FintConstant) expr, grouping);
    else if (expr instanceof FrealConstant) vars = getVariableList((FrealConstant) expr, grouping);
    else if (expr instanceof FcharacterConstant)
      vars = getVariableList((FcharacterConstant) expr, grouping);
    else if (expr instanceof FlogicalConstant)
      vars = getVariableList((FlogicalConstant) expr, grouping);
    else if (expr instanceof Var) vars = getVariableList((Var) expr, grouping);
    else if (expr instanceof FarrayConstructor)
      vars = getVariableList((FarrayConstructor) expr, grouping);
    else if (expr instanceof FstructConstructor)
      vars = getVariableList((FstructConstructor) expr, grouping);
    else if (expr instanceof UnaryMinusExpr)
      vars = getVariableList((UnaryMinusExpr) expr, grouping);
    else if (expr instanceof LogNotExpr) vars = getVariableList((LogNotExpr) expr, grouping);
    else if (expr instanceof VarRef) vars = getVariableList((VarRef) expr, grouping);
    else if (expr instanceof PlusExpr) vars = getVariableList((PlusExpr) expr, grouping);
    else if (expr instanceof MinusExpr) vars = getVariableList((MinusExpr) expr, grouping);
    else if (expr instanceof MulExpr) vars = getVariableList((MulExpr) expr, grouping);
    else if (expr instanceof DivExpr) vars = getVariableList((DivExpr) expr, grouping);
    else if (expr instanceof FconcatExpr) vars = getVariableList((FconcatExpr) expr, grouping);
    else if (expr instanceof LogEQExpr) vars = getVariableList((LogEQExpr) expr, grouping);
    else if (expr instanceof LogNEQExpr) vars = getVariableList((LogNEQExpr) expr, grouping);
    else if (expr instanceof LogGEExpr) vars = getVariableList((LogGEExpr) expr, grouping);
    else if (expr instanceof LogGTExpr) vars = getVariableList((LogGTExpr) expr, grouping);
    else if (expr instanceof LogLTExpr) vars = getVariableList((LogLTExpr) expr, grouping);
    else if (expr instanceof LogAndExpr) vars = getVariableList((LogAndExpr) expr, grouping);
    else if (expr instanceof LogOrExpr) vars = getVariableList((LogOrExpr) expr, grouping);
    else if (expr instanceof LogEQVExpr) vars = getVariableList((LogEQVExpr) expr, grouping);
    else if (expr instanceof LogNEQVExpr) vars = getVariableList((LogNEQVExpr) expr, grouping);
    else if (expr instanceof IndexRange) vars = getVariableList((IndexRange) expr, grouping);
    else if (expr instanceof LowerBound) vars = getVariableList((LowerBound) expr, grouping);
    else if (expr instanceof UpperBound) vars = getVariableList((UpperBound) expr, grouping);
    else if (expr instanceof Step) vars = getVariableList((Step) expr, grouping);
    else if (expr instanceof ArrayIndex) vars = getVariableList((ArrayIndex) expr, grouping);
    else if (expr instanceof NamedValue) vars = getVariableList((NamedValue) expr, grouping);
    else if (expr instanceof Value) vars = getVariableList((Value) expr, grouping);
    else if (expr instanceof Condition) vars = getVariableList((Condition) expr, grouping);

    return vars;
  }

  /**
   * Get the variable list from the FunctionCall element
   *
   * @param node FunctionCall element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FunctionCall node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();

    Name name = node.getName();
    boolean isIntrinsic = XmlNodeUtil.isBoolean(node.isIsIntrinsic());

    // Not a built-in function. Or the built-in function removal flag is not set.
    if (!isIntrinsic || ((this._mode & PARSE_NONEINTRINSIC) == 0x00)) {
      list.add(name.getValue());
    }

    // For built-in functions, parse the arguments.
    // PARSE_FUNCTION = Subroutines and function arguments are not parsed. Returns only subroutine
    // and function names.
    if (isIntrinsic || (this._mode & PARSE_FUNCTION) == 0x00) {
      addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis
      // Argument parsing
      String[] args = getVariableList(node.getArguments(), grouping);
      list.addAll(Arrays.asList(args));
      addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis
    }

    if (list.size() <= 0) return null;

    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * INTERFACE dependent element
   *
   * @param node INTERFACE dependent
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(UserBinaryExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();

    String name = node.getName();

    List<IXmlNode> content = node.getContent();

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;

    if (leftExpr != null) {
      String[] vars = getVariableList(leftExpr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }
    addOperatorString(list, EXPR_SPACE); // Space

    list.add(name);

    addOperatorString(list, EXPR_SPACE); // Space

    if (rightExpr != null) {
      String[] vars = getVariableList(rightExpr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FcharacterRef (see substring) element
   *
   * @param node FcharacterRef (see substring) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FcharacterRef node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();

    // VarRef
    VarRef var = node.getVarRef();
    if (var != null) {
      String[] vars = getVariableList(var, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    // Left parenthesis
    addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis (

    // IndexRange
    IndexRange range = node.getIndexRange();
    if (range != null) {
      String[] vars = getVariableList(range, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    // Right parenthesis
    addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis)

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from a structure member (FmemberRef) element
   *
   * @param node Structure member (FmemberRef)
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FmemberRef node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    VarRef ref = node.getVarRef();
    if (ref != null) {
      String[] vars = getVariableList(ref, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    // member reference
    addOperatorString(list, EXPR_TYPEMEMBER); // %

    String member = node.getMember();
    if (member != null) {
      list.add(member);
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FcoArrayRef (see coarray) element
   *
   * @param node FcoArrayRef (see coarray)
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FcoArrayRef node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    VarRef ref = node.getVarRef();
    if (ref != null) {
      String[] vars = getVariableList(ref, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    addOperatorString(list, EXPR_COARRAYLEFT); // [

    List<ArrayIndex> idxs = node.getArrayIndex();
    for (ArrayIndex expr : idxs) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    addOperatorString(list, EXPR_COARRAYRIGHT); // ]

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the ArrayIndex element
   *
   * @param node ArrayIndex
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(ArrayIndex node, boolean grouping) throws XcodeMLException {
    IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
    String[] vars = getVariableList(expr, grouping);
    return vars;
  }

  /**
   * Get a list of variables from the VarRef element
   *
   * @param node VarRef (variable reference) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(VarRef node, boolean grouping) throws XcodeMLException {
    IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
    String[] vars = getVariableList(expr, grouping);
    return vars;
  }

  /**
   * Get a list of variables from an FdoLoop element
   *
   * @param node FdoLoop (DO type iteration) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FdoLoop node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();

    // Left parenthesis
    addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis (

    List<Value> values = node.getValue();
    for (Value expr : values) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_COMMA); // Comma
      }
    }

    Var var = node.getVar();
    if (var != null) {
      String[] vars = getVariableList(var, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    addOperatorString(list, EXPR_EQUAL); // =

    IndexRange idx = node.getIndexRange();
    if (idx != null) {
      String[] vars = getVariableList(idx, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    // Right parenthesis
    addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis)

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the Var (variable name) element
   *
   * @param node Var (variable name)
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   */
  public String[] getVariableList(Var node, boolean grouping) {
    String value = node.getValue();
    if (value == null) return null;
    return new String[] {value};
  }

  /**
   * Get a list of variables from the IndexRange element
   *
   * @param node IndexRange element
   * @param grouping true = Group variable list
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(IndexRange node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    LowerBound lower = node.getLowerBound();
    if (lower != null) {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(lower);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_ARRAYCOLON); // colon
      }
    }
    UpperBound upper = node.getUpperBound();
    if (upper != null) {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(upper);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        addOperatorString(list, EXPR_ARRAYCOLON); // colon
        list.addAll(Arrays.asList(vars));
      }
    }
    Step step = node.getStep();
    if (step != null) {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(step);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        addOperatorString(list, EXPR_ARRAYCOLON); // colon
        list.addAll(Arrays.asList(vars));
      }
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the LowerBound element
   *
   * @param node LowerBound element
   * @param grouping true = Group variable list
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LowerBound node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    if (node != null) {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }
    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the UpperBound element
   *
   * @param node UpperBound (upper limit of index range) element
   * @param grouping true = Group variable list
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(UpperBound node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    if (node != null) {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }
    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the Step element
   *
   * @param node Step element
   * @param grouping true = Group variable list
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(Step node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    if (node != null) {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }
    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the Value (arbitrary value represented by an expression) element
   *
   * @param node Value (arbitrary value represented by an expression) element
   * @param grouping true = Group variable list
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(Value node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    RepeatCount repeat = node.getRepeatCount();
    if (repeat != null) {
      IXmlNode expr_rep = XmlNodeUtil.getXmlNodeChoice(repeat);
      String[] vars = getVariableList(expr_rep, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_MUL); // *
      }
    }

    IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
    if (expr != null) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from a FarrayRef (see subarray or array element) element
   *
   * @param node FarrayRef (see subarray or array element) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FarrayRef node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();

    VarRef varRef = node.getVarRef();
    String[] varRefs = getVariableList(varRef, grouping);
    if (varRefs != null) {
      list.addAll(Arrays.asList(varRefs));
    }

    List<IXmlNode> values = node.getIndexRangeOrArrayIndexOrFarrayConstructor();
    // Left parenthesis
    addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis (

    for (IXmlNode expr : values) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_COMMA); // colon:
      }
    }
    if (list.get(list.size() - 1) == EXPR_COMMA) list.remove(list.size() - 1);

    // Right parenthesis
    addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis)

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FcomplexConstant (complex type constant) element
   *
   * @param node FcomplexConstant (COMPLEX type constant) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FcomplexConstant node, boolean grouping) throws XcodeMLException {

    // Do not return constants as variables
    if ((this._mode & PARSE_VARIABLE) != 0x00) {
      return null;
    }

    ArrayList<String> list = new ArrayList<String>();
    // Left parenthesis
    addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis (

    List<IXmlNode> content = node.getContent();
    if (content == null) return null;
    for (IXmlNode expr : content) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_COMMA); // Comma
      }
    }
    if (list.get(list.size() - 1) == EXPR_COMMA) list.remove(list.size() - 1);

    // Right parenthesis
    addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis)

    if (list.size() <= 0) return null;

    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the UserUnaryExpr element
   *
   * @param node UserUnaryExpr
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(UserUnaryExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    String name = node.getName();
    list.add(name);

    // Left parenthesis
    addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis (

    IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
    if (expr == null) return null;
    String[] vars = getVariableList(expr, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    // Right parenthesis
    addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis)

    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FpowerExpr element
   *
   * <p>EXPR_POWER = "**"; /// FpowerExpr **: Exponentiation
   *
   * @param node FpowerExpr element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FpowerExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_POWER, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the LogLEExpr (less or equivalent) element
   *
   * <p>EXPR_LOGLE = ".LE."; /// logLEExpr <= .LE .: less or equivalent)
   *
   * @param node LogLEExpr (less or equivalent) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogLEExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGLE, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FintConstant (constant with integer value) element
   *
   * @param node FintConstant (constant with integer value) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   */
  public String[] getVariableList(FintConstant node, boolean grouping) {
    // Do not return integers as variables
    if ((this._mode & PARSE_VARIABLE) != 0x00) {
      return null;
    }

    ArrayList<String> list = new ArrayList<String>();
    String content = node.getValue();
    String kind = node.getKind();
    if (StringUtils.isNullOrEmpty(kind) == false) {
      list.add(content + "_" + kind);
    } else {
      list.add(content);
    }
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from a FrealConstant element
   *
   * @param node FrealConstant (floating point) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   */
  public String[] getVariableList(FrealConstant node, boolean grouping) {
    // Do not return constants as variables
    if ((this._mode & PARSE_VARIABLE) != 0x00) {
      return null;
    }

    ArrayList<String> list = new ArrayList<String>();
    String content = node.getValue();
    String kind = node.getKind();

    if (StringUtils.isNullOrEmpty(kind) == false && content.toLowerCase().indexOf("d") < 0) {
      list.add(content + "_" + kind);
    } else {
      list.add(content);
    }
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FcharacterConstant element
   *
   * @param node FcharacterConstant element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   */
  public String[] getVariableList(FcharacterConstant node, boolean grouping) {
    // Do not return constants as variables
    if ((this._mode & PARSE_VARIABLE) != 0x00) {
      return null;
    }

    ArrayList<String> list = new ArrayList<String>();
    String content = node.getValue();
    if (content == null) return null;
    String kind = node.getKind();
    String value = "";
    if (StringUtils.isNullOrEmpty(kind) == false) {
      value = kind + "_";
    }

    value += "\"" + content + "\"";
    list.add(value);

    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the FlogicalConstant element
   *
   * @param node FlogicalConstant (logical value) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   */
  public String[] getVariableList(FlogicalConstant node, boolean grouping) {
    // Do not return constants as variables
    if ((this._mode & PARSE_VARIABLE) != 0x00) {
      return null;
    }

    ArrayList<String> list = new ArrayList<String>();
    String content = node.getValue();
    String kind = node.getKind();
    if (StringUtils.isNullOrEmpty(kind) == false) {
      list.add(content + "_" + kind);
    } else {
      list.add(content);
    }
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the Farray Constructor element
   *
   * @param node FarrayConstructor element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FarrayConstructor node, boolean grouping)
      throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getDefModelExpr();
    if (content == null) return null;

    // (
    addOperatorString(list, EXPR_ARRAYLEFT);
    for (IXmlNode expr : content) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_COMMA); // Comma
      }
    }
    if (list.get(list.size() - 1) == EXPR_COMMA) list.remove(list.size() - 1);

    // )
    addOperatorString(list, EXPR_ARRAYRIGHT);

    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the Fstruct Constructor element
   *
   * @param node FstructConstructor element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FstructConstructor node, boolean grouping)
      throws XcodeMLException {
    FstructType structTypeElem = (FstructType) _typeManager.findType(node.getType());
    String aliasStructTypeName = _typeManager.getAliasTypeName(structTypeElem.getType());

    ArrayList<String> list = new ArrayList<String>();
    list.add(aliasStructTypeName);

    // Left parenthesis
    addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis (

    List<IXmlNode> content = node.getDefModelExpr();
    if (content == null) return null;
    for (IXmlNode expr : content) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
        addOperatorString(list, EXPR_COMMA); // Comma
      }
    }
    if (list.get(list.size() - 1) == EXPR_COMMA) list.remove(list.size() - 1);

    // Right parenthesis
    addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis)

    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the UnaryMinusExpr (sign inversion) element
   *
   * <p>EXPR_UNARYMINUS = "-"; /// unaryMinusExpr-: Sign inversion
   *
   * @param node UnaryMinusExpr (sign inversion) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(UnaryMinusExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    addOperatorString(list, EXPR_UNARYMINUS); // '-': Sign inversion

    for (IXmlNode expr : content) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the LogNotExpr (logical negation) element
   *
   * <p>EXPR_LOGNOT = ".NOT."; /// logNotExpr .NOT .: Logical negation
   *
   * @param node LogNotExpr (logical negation) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogNotExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    addOperatorString(list, EXPR_LOGNOT); // '.NOT.': Logical negation
    for (IXmlNode expr : content) {
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the PlusExpr (addition) element EXPR_PLUS = "+"; /// plusExpr +:
   * Addition
   *
   * @param node PlusExpr (addition) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(PlusExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_PLUS, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the MinusExpr (subtraction) element EXPR_MINUS = "-"; ///
   * minusExpr-: Subtraction
   *
   * @param node MinusExpr (subtraction) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(MinusExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_MINUS, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the MulExpr (multiplication) element EXPR_MUL = "*"; /// mulExpr *:
   * Multiply
   *
   * @param node MulExpr (multiplication) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(MulExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_MUL, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the DivExpr element EXPR_DIV = "/"; /// divExpr /: Divide
   *
   * @param node DivExpr (division) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(DivExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_DIV, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the FconcatExpr (concatenation of character expressions) element
   * EXPR_CONCAT = "//"; /// FconcatExpr // : Concatenation of character expressions
   *
   * @param node FconcatExpr (concatenation of character expressions) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(FconcatExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_CONCAT, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogEQExpr (equivalent) element EXPR_LOGEQ = ".EQ."; ///
   * logEQExpr == .EQ.: Equivalent
   *
   * @param node LogEQExpr (equivalent) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogEQExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGEQ, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogNEQExpr (non-equivalent) element EXPR_LOGNEQ = ".NE."; ///
   * logNEQExpr / = .NE .: Non-equivalent
   *
   * @param node LogNEQExpr (non-equivalent) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogNEQExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGNEQ, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get a list of variables from the LogGEExpr (greater than or equivalent) element
   *
   * <p>EXPR_LOGGE = ".GE."; /// logGEExpr> = .GE .: Greater or equivalent
   *
   * @param node LogGEExpr (greater than or equivalent) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogGEExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGGE, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogGTExpr (greater than) element EXPR_LOGGT = ".GT."; ///
   * logGTExpr> .GT.: Greater
   *
   * @param node LogGTExpr (greater than) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogGTExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGGT, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogLTExpr (less) element EXPR_LOGLT = ".LT."; /// logLTExpr
   * <.LT.: Small
   *
   * @param node LogLTExpr (less than) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogLTExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGLT, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogAndExpr element EXPR_LOGAND = ".AND."; /// logAndExpr .AND. :
   * Logical AND
   *
   * @param node LogAndExpr (logical product) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogAndExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGAND, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogOrExpr (OR) element EXPR_LOGOR = ".OR."; /// logOrExpr .OR.:
   * OR
   *
   * @param node LogOrExpr (OR) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogOrExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGOR, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogEQVExpr (logical equivalent) element EXPR_LOGEQV = ".EQV.";
   * /// logEQVExpr .EQV .: Logical equivalent
   *
   * @param node LogEQVExpr (logical equivalent) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogEQVExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGEQV, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get the variable list from the LogNEQVExpr (logical non-equivalent) element EXPR_LOGNEQV =
   * ".NEQV."; /// logNEQVExpr .NEQV .: Logical non-equivalent
   *
   * @param node LogNEQVExpr (logical non-equivalent) element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(LogNEQVExpr node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    List<IXmlNode> content = node.getContent();
    if (content == null) return null;

    IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
    IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
    String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGNEQV, grouping);
    if (vars != null) {
      list.addAll(Arrays.asList(vars));
    }

    if (list.size() <= 0) return null;
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * exprModel Get a list of variables from a model element
   *
   * @param node exprModel Model element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(IDefModelExpr node, boolean grouping) throws XcodeMLException {
    if (node == null) return null;
    IXmlNode def = XmlNodeUtil.getXmlNodeChoice(node);
    String[] vars = getVariableList(def, grouping);

    return vars;
  }

  /**
   * Add the operator to the list. Check PARSE_OPERATOR in parsing mode to determine the addition of
   * operators.
   *
   * @param list Variable list
   * @param oper Additional operators
   */
  private void addOperatorString(List<String> list, String oper) {
    if ((this._mode & PARSE_OPERATOR) != 0x00) {
      list.add(oper);
    }
    return;
  }

  /**
   * Parse the variable list from the left and right sides of the operator.
   *
   * @param leftExpr Left side
   * @param rightExpr Right side
   * @param operation operator
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return Variable list
   * @throws XcodeMLException Parsing error
   */
  private String[] getVariableList(
      IXmlNode leftExpr, IXmlNode rightExpr, String operation, boolean grouping)
      throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();

    if (grouping) {
      // Left parenthesis
      addOperatorString(list, EXPR_PARENLEFT); // Left parenthesis
    }

    // Left side
    if (leftExpr != null) {
      String[] vars = getVariableList(leftExpr, true);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    addOperatorString(list, operation); // operator

    // Right side
    if (rightExpr != null) {
      String[] vars = getVariableList(rightExpr, true);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }

    if (grouping) {
      addOperatorString(list, EXPR_PARENRIGHT); // Right parenthesis
    }
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get attributes from a NamedValue element
   *
   * @param node NamedValue element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return attribute list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(NamedValue node, boolean grouping) throws XcodeMLException {
    ArrayList<String> list = new ArrayList<String>();
    // name
    String name = node.getName();
    list.add(name);
    if (name == null) return null;

    // add'='
    list.add("=");

    // value
    String value = node.getValue();
    if (value != null) {
      list.add(value);
    } else {
      IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
      String[] vars = getVariableList(expr, grouping);
      if (vars != null) {
        list.addAll(Arrays.asList(vars));
      }
    }
    return (String[]) list.toArray(new String[list.size()]);
  }

  /**
   * Get attributes from Conditional elements
   *
   * @param node Condition element
   * @param grouping Grouping'(..)' flag when assembling expressions
   * @return attribute list
   * @throws XcodeMLException Parsing error
   */
  public String[] getVariableList(Condition node, boolean grouping) throws XcodeMLException {

    IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
    String[] vars = getVariableList(expr, grouping);

    return vars;
  }
}
