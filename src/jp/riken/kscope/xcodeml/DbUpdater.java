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

package jp.riken.kscope.xcodeml;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Data;
import jp.riken.kscope.language.Equivalence;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.Argument;
import jp.riken.kscope.language.generic.ProcedureItem;
import jp.riken.kscope.language.generic.ProcedureWithNameOnly;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.language.ExpressionParser;
import jp.riken.kscope.xcodeml.language.VariableDefinitionParser;
import jp.riken.kscope.xcodeml.language.VariableDimensionParser;
import jp.riken.kscope.xcodeml.language.VariableParser;
import jp.riken.kscope.xcodeml.language.VariableTypeParser;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.EnumType;
import jp.riken.kscope.xcodeml.xml.FdataDeclSequence;
import jp.riken.kscope.xcodeml.xml.GotoStatementSequence;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.xml.gen.Alloc;
import jp.riken.kscope.xcodeml.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.xml.gen.ContinueStatement;
import jp.riken.kscope.xcodeml.xml.gen.Else;
import jp.riken.kscope.xcodeml.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.xml.gen.FallocateStatement;
import jp.riken.kscope.xcodeml.xml.gen.FassignStatement;
import jp.riken.kscope.xcodeml.xml.gen.FcaseLabel;
import jp.riken.kscope.xcodeml.xml.gen.FcommonDecl;
import jp.riken.kscope.xcodeml.xml.gen.FcycleStatement;
import jp.riken.kscope.xcodeml.xml.gen.FdataDecl;
import jp.riken.kscope.xcodeml.xml.gen.FdeallocateStatement;
import jp.riken.kscope.xcodeml.xml.gen.FdoStatement;
import jp.riken.kscope.xcodeml.xml.gen.FdoWhileStatement;
import jp.riken.kscope.xcodeml.xml.gen.FequivalenceDecl;
import jp.riken.kscope.xcodeml.xml.gen.FexitStatement;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionDecl;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionDefinition;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.xml.gen.FifStatement;
import jp.riken.kscope.xcodeml.xml.gen.FinterfaceDecl;
import jp.riken.kscope.xcodeml.xml.gen.FmoduleDefinition;
import jp.riken.kscope.xcodeml.xml.gen.FmoduleProcedureDecl;
import jp.riken.kscope.xcodeml.xml.gen.FnullifyStatement;
import jp.riken.kscope.xcodeml.xml.gen.FpauseStatement;
import jp.riken.kscope.xcodeml.xml.gen.FpointerAssignStatement;
import jp.riken.kscope.xcodeml.xml.gen.FpragmaStatement;
import jp.riken.kscope.xcodeml.xml.gen.FreturnStatement;
import jp.riken.kscope.xcodeml.xml.gen.FselectCaseStatement;
import jp.riken.kscope.xcodeml.xml.gen.FstopStatement;
import jp.riken.kscope.xcodeml.xml.gen.FstructDecl;
import jp.riken.kscope.xcodeml.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.xml.gen.FuseDecl;
import jp.riken.kscope.xcodeml.xml.gen.FuseOnlyDecl;
import jp.riken.kscope.xcodeml.xml.gen.FwhereStatement;
import jp.riken.kscope.xcodeml.xml.gen.GotoStatement;
import jp.riken.kscope.xcodeml.xml.gen.Name;
import jp.riken.kscope.xcodeml.xml.gen.Params;
import jp.riken.kscope.xcodeml.xml.gen.Renamable;
import jp.riken.kscope.xcodeml.xml.gen.Rename;
import jp.riken.kscope.xcodeml.xml.gen.StatementLabel;
import jp.riken.kscope.xcodeml.xml.gen.Then;
import jp.riken.kscope.xcodeml.xml.gen.Value;
import jp.riken.kscope.xcodeml.xml.gen.ValueList;
import jp.riken.kscope.xcodeml.xml.gen.Var;
import jp.riken.kscope.xcodeml.xml.gen.VarDecl;
import jp.riken.kscope.xcodeml.xml.gen.VarList;
import jp.riken.kscope.xcodeml.xml.gen.VarRef;

/**
 * Store parse results in Fortran database
 *
 * @author RIKEN
 */
public class DbUpdater extends XcodeMLVisitorImpl {
  /** Fortran database */
  private Fortran m_database = null;
  /** XcodeML parser execution status class */
  private XcodeMLContext m_context;
  /** Statement number node of the previous node */
  @SuppressWarnings("unused")
  private StatementLabel prevLabel = null;
  /** Error information list */
  private List<ErrorInfo> listErrorInfo;

  /**
   * Constructor
   *
   * @param db Fortran database
   * @param context XcodeML parser execution status class
   */
  public DbUpdater(Fortran db, XcodeMLContext context) {
    m_database = db;
    m_context = context;
  }

  /**
   * Register the start of the FfunctionDefinition element (MAIN PROGRAM / function / subroutine
   * definition).
   *
   * @param visitable FfunctionDefinition element
   * @return boolean Success or failure
   */
  @Override
  public boolean enter(FfunctionDefinition visitable) {

    // Code output class
    XcodeMLTypeManager typeManager = m_context.getTypeManager();
    VariableTypeParser typePaser = new VariableTypeParser(typeManager);

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // PROGRAM, SUBROUTINE, FUNCTION statement information
    // Program name, subroutine name, function name
    Name functionNameElem = visitable.getName();
    String functionname = functionNameElem.getValue();

    // Data type
    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    VariableType funcType = typePaser.parseVarDefFunctionType(functionTypeElem);

    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);

    // result
    String result = functionTypeElem.getResultName();

    // Attribute
    boolean is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
    boolean is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
    boolean is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());

    // Formal argument
    Params params = functionTypeElem.getParams();
    ArrayList<String> args = new ArrayList<String>();
    if (params != null) {
      for (Name nameElem : params.getName()) {
        args.add(nameElem.getValue());
      }
    }

    // Database registration
    if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
      // Check if an existing program exists.
      Procedure errorProc = m_database.getMainProgram();
      if (errorProc != null) {
        addDuplicateError(functionname, lineInfo, errorProc);
      }
      // PROGRAM
      m_database.init_main(functionname);
    } else {
      if (typeId == EnumType.VOID) {
        // Check if an existing subroutine exists.
        Procedure errorProc = m_database.getProcedure(m_database.getCurrentUnit(), functionname);
        if (errorProc != null) {
          addDuplicateError(functionname, lineInfo, errorProc);
        }
        // SUBROUTINE
        if (args.size() <= 0) {
          m_database.initSubroutine(functionname);
        } else {
          m_database.initSubroutine(functionname, args.toArray(new String[0]));
        }
        funcType = new VariableType(PrimitiveDataType.VOID);
      } else {
        // Check if an existing function exists.
        Procedure errorProc = m_database.getProcedure(m_database.getCurrentUnit(), functionname);
        if (errorProc != null) {
          addDuplicateError(functionname, lineInfo, errorProc);
        }
        // FUNCTION
        if (args.size() <= 0) {
          m_database.init_function(functionname);
        } else {
          m_database.init_function(functionname, args.toArray(new String[0]));
        }
      }
    }
    m_database.get_current_unit().set_start(lineInfo);

    // Function data type
    m_database.setReturnValueType(funcType);
    // result
    m_database.setResult(result);
    // recursive
    if (is_recursive) m_database.put_attribute("recursive");
    // public
    if (is_puiblic) m_database.setPublicToCurrentUnit();
    // private
    if (is_private) m_database.setPrivateToCurrentUnit();

    return true;
  }

  /**
   * Register the end of the FfunctionDefinition element (MAIN PROGRAM / function / subroutine
   * definition).
   *
   * @param visitable FfunctionDefinition element
   */
  @Override
  public void leave(FfunctionDefinition visitable) {

    // Code output class
    XcodeMLTypeManager typeManager = m_context.getTypeManager();

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // PROGRAM, SUBROUTINE, FUNCTION statement information
    // Program name, subroutine name, function name
    Name functionNameElem = visitable.getName();
    String functionname = functionNameElem.getValue();

    // Declaration
    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);

    // Database registration
    m_database.get_current_unit().set_end(lineInfo);
    if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
      // PROGRAM
      m_database.end_main();
    } else if (typeId == EnumType.VOID) {
      // SUBROUTINE
      m_database.endSubroutine();
    } else {
      // FUNCTION
      m_database.end_function();
    }

    return;
  }

  /**
   * Register the FunctionCall element (function / subroutine call).
   *
   * @param visitable FunctionCall
   * @param call_line Subroutine call statement or substring of function name (argument)
   * @return Success or failure
   */
  public boolean enterFunctionCall(FunctionCall visitable, String call_line) {

    String label = Statement.NO_LABEL;

    // Is the next higher node ExprStatement?
    if (!m_context.isInvokeNodeOf(ExprStatement.class, 1)) {
      return true;
    }

    // Is the database able to register functionCall (is the block currently a ProgramUnit?)
    if (!isCurrentProgramUnit()) {
      return true;
    }

    // Data type parser
    VariableTypeParser typePaser = new VariableTypeParser(this.m_context.getTypeManager());
    VariableType funcVarType = null;

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().createCodeLine(visitable, call_line);
    int lineno = lineInfo.getStartLine();

    // argument of set_call
    List<Expression> list = null;
    // Subroutine name, function name
    Name functionNameElem = visitable.getName();
    String functionname = functionNameElem.getValue();

    // Attribute
    boolean is_intrinsic = false;
    boolean is_external = false;
    boolean is_recursive = false;
    boolean is_puiblic = false;
    boolean is_private = false;
    if (functionNameElem.getType() != null) {
      IXmlTypeTableChoice typeChoice = this.m_context.getTypeManager().findType(functionNameElem);
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
    boolean intrinsic = XmlNodeUtil.isBoolean(visitable.isIsIntrinsic());

    // Argument list
    try {
      Arguments arguments = visitable.getArguments();
      ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());
      Expression[] argsExpr = exprParser.getExpressionArray(arguments);
      if (argsExpr != null) {
        list = new ArrayList<Expression>(java.util.Arrays.asList(argsExpr));
      }

      // Registration of external procedures
      addExternalFunction(exprParser.getExternalFunction());

    } catch (XcodeMLException ex) {
      ex.printStackTrace();
    }
    // CALL statement, function call registration
    m_database.setCall(lineInfo, label, functionname, list, intrinsic);
    if (is_external) {
      // Registration of external procedures
      addExternalFunction(functionname, funcVarType);
    }

    return true;
  }

  /**
   * Check if the currentUnit of the Fortran class is ProgramUnit.
   *
   * @return true: currentUnit is a ProgramUnit.
   */
  private boolean isCurrentProgramUnit() {
    ProgramUnit unit = m_database.get_current_unit();
    if (unit instanceof ProgramUnit) {
      return true;
    }

    return false;
  }

  /**
   * Parse the statement number.
   *
   * @param visitable StatementLabel
   * @param nextNode Statement number Target element
   * @return Success or failure
   */
  public boolean enterStatementLabel(StatementLabel visitable, IXmlNode nextNode) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    if ((nextNode != null) && (nextNode instanceof StatementLabel)) {
      // Line with statement number only
      // Register as a CONTINUE statement.
      if (!StringUtils.isNullOrEmpty(visitable.getLabelName())) {
        label = visitable.getLabelName();
      }
      // Register the Statement Label statement.
      m_database.setContinue(lineInfo, label);
    } else {
      // do nothing.
      // Register with the sentence number on the next line.
      prevLabel = visitable;
    }

    return true;
  }

  /**
   * Parse the argument list. <br>
   * Get the argument string list from the code builder class. Set to the argument list member.
   *
   * @param visitable Arguments
   * @return Success or failure
   */
  @Override
  public boolean enter(Arguments visitable) {

    // Get the argument list.
    // m_argumentList =
    // m_context.getCodeBuilder().getArgumentList(visitable);

    return true;
  }

  /**
   * Register the FassignStatement element (assignment statement).
   *
   * @param visitable FassignStatement
   * @return Success or failure
   */
  @Override
  public boolean enter(FassignStatement visitable) {
    String label = Statement.NO_LABEL;

    //        try {
    // Is the database able to register functionCall (is the block currently a ProgramUnit?)
    if (!isCurrentProgramUnit()) {
      return true;
    }

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
    int lineno = lineInfo.getStartLine();

    List<IXmlNode> content = visitable.getContent();
    // Left side
    IXmlNode leftExpr = (content != null && content.size() >= 1) ? content.get(0) : null;
    // Right side
    IXmlNode rightExpr = (content != null && content.size() >= 2) ? content.get(1) : null;

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    // Left side
    Variable leftVar = varParser.getVariable(leftExpr);

    // Right side
    exprParser.setParseNode(rightExpr);
    Expression rightVar = exprParser.getExpression();

    m_database.setSubstitution(leftVar, rightVar, lineInfo, label);

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the starting element of Fdo Statement (DO statement).
   *
   * @param visitable Fdo Statement element
   * @return Success or failure
   */
  @Override
  public boolean enter(FdoStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // DO syntax name (SSSS: DO)
    String construct_name = visitable.getConstructName();

    // Variable name
    Var var = visitable.getVar();
    if (var == null) {
      // DO statement with no conditional statement (infinite loop)
      if (construct_name != null) {
        m_database.start_loop(lineInfo, construct_name);
      } else {
        m_database.start_loop(lineInfo, Statement.NO_LABEL);
      }
      return true;
    }

    // Get variable data type
    VariableDefinitionParser varParser =
        new VariableDefinitionParser(this.m_context.getTypeManager());
    VariableDefinition varDef = varParser.parseVariableDefinition(var);
    Variable variable = new Variable(var.getValue());
    variable.setDefinition(varDef);

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // Parse the loop expression.
    // Get the lower bound of the range
    Expression lowerExp = null;
    if (visitable.getIndexRange() != null && visitable.getIndexRange().getLowerBound() != null) {
      exprParser.setParseNode(visitable.getIndexRange().getLowerBound());
      // Get the lower bound of the range
      lowerExp = exprParser.getExpression();
    }
    // Get the upper limit of the range
    Expression upperExp = null;
    if (visitable.getIndexRange() != null && visitable.getIndexRange().getUpperBound() != null) {
      exprParser.setParseNode(visitable.getIndexRange().getUpperBound());
      // Get the upper limit of the range
      upperExp = exprParser.getExpression();
    }
    // Get the steps in the range
    Expression stepExp = null;
    if (visitable.getIndexRange() != null && visitable.getIndexRange().getUpperBound() != null) {
      exprParser.setParseNode(visitable.getIndexRange().getStep());
      // Get the steps in the range
      stepExp = exprParser.getExpression();
    }

    // Start blocking DO statement
    if (construct_name != null) {
      m_database.start_loop(lineInfo, construct_name, variable, lowerExp, upperExp, stepExp);
    } else {
      m_database.start_loop(lineInfo, Statement.NO_LABEL, variable, lowerExp, upperExp, stepExp);
    }

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the end element of Fdo Statement (DO statement).
   *
   * @param visitable Fdo Statement element
   */
  @Override
  public void leave(FdoStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // End of DO statement block
    m_database.end_loop(lineInfo);
  }

  /**
   * Register the start element of FmoduleDefinition (MODULE statement).
   *
   * @param visitable FmoduleDefinition (MODULE statement) element
   * @return Success or failure
   */
  @Override
  public boolean enter(FmoduleDefinition visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    String name = visitable.getName();
    // Check if an existing module exists.
    Module module = m_database.module(name);
    if (module != null && module.get_start() != null && module.get_start().getLineInfo() != null) {
      addDuplicateError(name, lineInfo, module);
    }
    m_database.init_module(name);
    m_database.get_current_unit().set_start(lineInfo);

    return true;
  }

  /**
   * Register the end element of FmoduleDefinition (MODULE statement).
   *
   * @param visitable FmoduleDefinition (MODULE statement) element
   */
  @Override
  public void leave(FmoduleDefinition visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    m_database.get_current_unit().set_end(lineInfo);
    m_database.end_module();
  }

  /**
   * Register the element of ContinueStatement (CONTAINUE statement)
   *
   * @param visitable ContinueStatement (CONTAINUE statement) element
   */
  @Override
  public boolean enter(ContinueStatement visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    if (!StringUtils.isNullOrEmpty(m_context.getStatementLabel())) {
      label = m_context.getStatementLabel();
    }
    // Register the Statement Label statement.
    m_database.setContinue(lineInfo, label);

    return true;
  }

  /**
   * Register the starting element of the Fdo While Statement (DO WHILE statement).
   *
   * @param visitable Fdo Statement element
   * @return Success or failure
   */
  @Override
  public boolean enter(FdoWhileStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // DO syntax name (SSSS: DO)
    String construct_name = visitable.getConstructName();

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // Parse the loop conditional expression.
    // Get the loop conditional expression
    Expression conditionExp = null;
    if (visitable.getCondition() != null) {
      exprParser.setParseNode(visitable.getCondition());
      // Get the lower bound of the range
      conditionExp = exprParser.getExpression();
    }

    // Start blocking DO statement
    if (construct_name != null) {
      m_database.start_loop(lineInfo, construct_name, null, null, conditionExp, null);
    } else {
      m_database.start_loop(lineInfo, Statement.NO_LABEL, null, null, conditionExp, null);
    }

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the end element of FdoWhileStatement (DO WHILE statement).
   *
   * @param visitable Fdo Statement element
   */
  @Override
  public void leave(FdoWhileStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // End of DO statement block
    m_database.end_loop(lineInfo);
  }

  /**
   * Register the start element of FselectCaseStatement (SELECT-CASE statement).
   *
   * @param visitable FselectCaseStatement (SELECT-CASE statement) element
   * @return Success or failure
   */
  @Override
  public boolean enter(FselectCaseStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // SELECT syntax name (SSSS: SELECT)
    String construct_name = visitable.getConstructName();

    if (construct_name != null) {
      m_database.startSelection(
          lineInfo, construct_name, jp.riken.kscope.language.Selection.SelectionType.SELECT);
    } else {
      m_database.startSelection(
          lineInfo, Statement.NO_LABEL, jp.riken.kscope.language.Selection.SelectionType.SELECT);
    }

    // Parse the conditional expression.
    // Expression Parser Model Parser
    ExpressionParser exprParser =
        new ExpressionParser(m_context.getTypeManager(), visitable.getValue());

    // Get the conditional expression
    Expression condExpr = exprParser.getExpression();

    // Conditional expression
    m_database.setSelectCaseCondition(condExpr);

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the end element of FselectCaseStatement (SELECT-CASE statement).
   *
   * @param visitable FselectCaseStatement (SELECT-CASE statement) element
   */
  @Override
  public void leave(FselectCaseStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // End block of SELECT statement
    // m_database.endCondition(lineInfo, Statement.NO_LABEL);
    m_database.end_selection(lineInfo);

    return;
  }

  /**
   * Register the start element of FcaseLabel (CASE statement).
   *
   * @param visitable FcaseLabel (CASE statement) element
   * @return Success or failure
   */
  @Override
  public boolean enter(FcaseLabel visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    //        if (((Procedure) (m_database.currentUnit)).body.getCurrentBlock() instanceof
    // Condition) {
    //            m_database.endCondition(lineInfo, Statement.NO_LABEL);
    //        }

    // Row label
    String construct_name = visitable.getConstructName();

    // Parse the conditional expression.
    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager(), visitable);

    // Get the conditional expression
    Expression condExpr = exprParser.getExpression();

    if (construct_name != null) {
      m_database.startCondition(condExpr, lineInfo, construct_name);
    } else {
      m_database.startCondition(condExpr, lineInfo, Statement.NO_LABEL);
    }

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the end element of FcaseLabel (CASE statement).
   *
   * @param visitable FcaseLabel (CASE statement) element
   */
  @Override
  public void leave(FcaseLabel visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
    m_database.endCondition(lineInfo, Statement.NO_LABEL);
  }

  /**
   * Register the start element of FifStatement (IF statement).
   *
   * @param visitable FifStatement element
   * @return Success or failure
   */
  @Override
  public boolean enter(FifStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // IF syntax name (SSSS: IF)
    String construct_name = visitable.getConstructName();

    // Parse the conditional expression.
    // Expression Parser Model Parser
    ExpressionParser exprParser =
        new ExpressionParser(m_context.getTypeManager(), visitable.getCondition());

    // Get the conditional expression
    Expression condExpr = exprParser.getExpression();

    // Check if it is an Else IF statement.
    boolean isElseIf = false;
    // Is the next higher node Else?
    if (m_context.isInvokeNodeOf(Else.class, 2)) {
      Else elseNode = (Else) m_context.getInvokeNode(2);
      // Is the start line number set?
      if (StringUtils.isNullOrEmpty(elseNode.getLineno())) {
        // This is an Else IF statement because the start line number of the Else element two above
        // is not set.
        isElseIf = true;
      }
    }

    //
    if (!isElseIf) {
      // IF statement
      if (construct_name != null) {
        m_database.startSelection(
            lineInfo, construct_name, jp.riken.kscope.language.Selection.SelectionType.IF);
      } else {
        m_database.startSelection(
            lineInfo, Statement.NO_LABEL, jp.riken.kscope.language.Selection.SelectionType.IF);
      }

      // Conditional expression
      m_database.startCondition(condExpr, lineInfo, visitable.getConstructName());
    } else {
      // ELSE-IF statement
      // String line = "ELSE IF " + cond_str;
      // Conditional expression
      m_database.startCondition(condExpr, lineInfo, visitable.getConstructName());
    }

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the end element of FifStatement (IF statement).
   *
   * @param visitable FifStatement element
   */
  @Override
  public void leave(FifStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Check if there is an Else IF statement.
    boolean isElseIf = false;
    // Is the next higher node Else?
    if (m_context.isInvokeNodeOf(Else.class, 2)) {
      Else elseNode = (Else) m_context.getInvokeNode(2);
      // Is the start line number set?
      if (StringUtils.isNullOrEmpty(elseNode.getLineno())) {
        // This is an Else IF statement because the start line number of the Else element two above
        // is not set.
        isElseIf = true;
      }
    }

    // In case of ELSE-IF, do not call end_selection.
    if (!isElseIf) {
      m_database.end_selection(lineInfo);
    }

    return;
  }

  /**
   * Register the end element of Then (true sentence block).
   *
   * @param visitable Then (true sentence block)
   */
  @Override
  public void leave(Then visitable) {

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
    m_database.endCondition(lineInfo, Statement.NO_LABEL);

    return;
  }

  /**
   * Register the starting element of Else (fake sentence block).
   *
   * @param visitable Else (fake sentence block)
   * @return Success or failure
   */
  @Override
  public boolean enter(Else visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Check if it is Else.
    boolean isElseIf = false;
    // Is the start line number set?
    if (StringUtils.isNullOrEmpty(visitable.getLineno())) {
      // This is an Else IF statement because the start line number is not set.
      isElseIf = true;
    }

    // In the case of ELSE-IF statement, register it in the DB with the following IF element.
    if (!isElseIf) {
      m_database.startCondition(null, lineInfo, Statement.NO_LABEL);
    }
    return true;
  }

  /**
   * Register the end element of Else (fake sentence block).
   *
   * @param visitable Else (fake sentence block)
   */
  @Override
  public void leave(Else visitable) {

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Check if there is an Else IF statement.
    boolean isElseIf = false;
    // Is the start line number set?
    if (StringUtils.isNullOrEmpty(visitable.getLineno())) {
      // This is an Else IF statement because the start line number is not set.
      isElseIf = true;
    }

    // Do not call end_condition for ELSE-IF statements.
    if (!isElseIf) {
      m_database.endCondition(lineInfo, Statement.NO_LABEL);
    }
    return;
  }

  /**
   * Register the start element of FwhereStatement (WHERE statement).
   *
   * @param visitable FwhereStatement (WHERE statement) element
   * @return Success or failure
   */
  @Override
  public boolean enter(FwhereStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Parse the conditional expression.
    // Expression Parser Model Parser
    ExpressionParser exprParser =
        new ExpressionParser(m_context.getTypeManager(), visitable.getCondition());

    // Get the conditional expression
    Expression condExpr = exprParser.getExpression();

    m_database.startSelection(
        lineInfo, Statement.NO_LABEL, jp.riken.kscope.language.Selection.SelectionType.WHERE);

    // Conditional expression
    m_database.startCondition(condExpr, lineInfo, Statement.NO_LABEL);

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the end element of FwhereStatement (WHERE statement).
   *
   * @param visitable FwhereStatement (WHERE statement) element
   */
  @Override
  public void leave(FwhereStatement visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    m_database.end_selection(lineInfo);

    return;
  }

  /**
   * Register the start element of Return (RETURN statement).
   *
   * @param visitable RETURN statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FreturnStatement visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // RETURN statement
    m_database.setReturn(lineInfo, label);

    return true;
  }

  /**
   * Register the starting element of VarDecl (variable declaration statement).
   *
   * @param visitable VarDecl (variable declaration statement)
   * @return Success or failure
   */
  @Override
  public boolean enter(VarDecl visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Variable declaration statement
    VariableDefinitionParser varParser =
        new VariableDefinitionParser(this.m_context.getTypeManager());
    VariableDefinition varDef = varParser.parseVariableDefinition(visitable.getName());
    if (varDef == null) {
      // Null for built-in function type declarations
      //            assert (varDef != null);
      return true;
    }

    // Set the source code line
    varDef.setCodeLine(lineInfo);

    // Parse the initial value.
    if (visitable.getValue() != null) {
      // Expression Parser Model Parser
      ExpressionParser exprParser =
          new ExpressionParser(m_context.getTypeManager(), visitable.getValue());
      Expression init_value = exprParser.getExpression();
      if (init_value != null) {
        varDef.setInitValue(init_value.getLine()); // initial value
      }
      // Registration of external procedures
      addExternalFunction(exprParser.getExternalFunction());
    }

    // Register the variable declaration statement.
    m_database.set_variable_def(varDef);

    return true;
  }

  /**
   * Register the starting element of FstructDecl (variable declaration statement).
   *
   * @param visitable FstructDecl (definition of structure)
   * @return Success or failure
   */
  @Override
  public boolean enter(FstructDecl visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Variable declaration statement
    VariableDefinitionParser parserDef =
        new VariableDefinitionParser(this.m_context.getTypeManager());
    VariableDefinition def = parserDef.parseVariableDefinition(visitable);
    if (def == null) return false;
    if (def.getVariableType() == null) return false;
    if (((VariableType) def.getVariableType()).getPrimitiveDataType() != PrimitiveDataType.TYPE
        && ((VariableType) def.getVariableType()).getPrimitiveDataType()
            != PrimitiveDataType.STRUCTURE) return false;

    // Structure definition
    jp.riken.kscope.language.fortran.Type type = ((VariableType) def.getVariableType()).getType();
    type.setStartCodeLine(lineInfo);

    // Register the structure definition.
    m_database.addTypeDefinition(type);

    return true;
  }

  /**
   * Register the start element of FuseDecl (USE declaration statement without ONLY specifier).
   *
   * @param visitable FuseDecl (USE declaration without ONLY specifier)
   * @return Success or failure
   */
  @Override
  public boolean enter(FuseDecl visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Module name
    String mod_name = visitable.getName();

    // USE statement class
    UseState use = new UseState();
    use.setModuleName(mod_name);

    // Local name and reference name
    List<Rename> renames = visitable.getRename();
    for (Rename rename : renames) {
      if (StringUtils.isNullOrEmpty(rename.getLocalName())) {
        use.addOnlyMember(rename.getUseName());
      } else {
        use.addTranslationName(rename.getLocalName(), rename.getUseName());
      }
    }

    // Register the USE declaration statement.
    m_database.setUse(use, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FuseOnlyDecl (USE declaration statement of ONLY specifier).
   *
   * @param visitable FuseOnlyDecl (USE declaration statement of ONLY specifier)
   * @return Success or failure
   */
  @Override
  public boolean enter(FuseOnlyDecl visitable) {
    String label = Statement.NO_LABEL;

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Module name
    String mod_name = visitable.getName();

    // USE statement class
    UseState use = new UseState();
    use.setModuleName(mod_name);

    // Local name and reference name
    List<Renamable> renames = visitable.getRenamable();
    for (Renamable rename : renames) {
      if (StringUtils.isNullOrEmpty(rename.getLocalName())) {
        use.addOnlyMember(rename.getUseName());
      } else {
        use.addTranslationName(rename.getLocalName(), rename.getUseName());
      }
    }

    // Register the USE declaration statement.
    m_database.setUse(use, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FpointerAssignStatement.
   *
   * @param visitable FpointerAssignStatement (pointer assignment statement)
   * @return Success or failure
   */
  @Override
  public boolean enter(FpointerAssignStatement visitable) {
    String label = Statement.NO_LABEL;

    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    List<IXmlNode> content = visitable.getContent();
    // Left side
    IXmlNode leftExpr = (content != null && content.size() >= 1) ? content.get(0) : null;
    // Right side
    IXmlNode rightExpr = (content != null && content.size() >= 2) ? content.get(1) : null;

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    // Left side
    Variable leftVar = varParser.getVariable(leftExpr);

    // Right side
    exprParser.setParseNode(rightExpr);
    Expression rightVar = exprParser.getExpression();

    m_database.setSubstitution(leftVar, rightVar, lineInfo, label);

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the start element of FpragmaStatement (OpenMP3.0 directive).
   *
   * @param visitable FpragmaStatement (OpenMP3.0 directive)
   * @return Success or failure
   */
  @Override
  public boolean enter(FpragmaStatement visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Instructions
    String value = visitable.getValue();

    // Register the directive statement.
    m_database.setDirective(value, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FexitStatement (EXIT statement).
   *
   * @param visitable EXIT statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FexitStatement visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // EXIT statement
    if (!StringUtils.isNullOrEmpty(visitable.getConstructName())) {
      label = visitable.getConstructName();
    }
    // Register the EXIT statement.
    m_database.setExit(lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FcycleStatement (CYCLE statement).
   *
   * @param visitable CYCLE statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FcycleStatement visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // CYCLE statement
    if (!StringUtils.isNullOrEmpty(visitable.getConstructName())) {
      label = visitable.getConstructName();
    }
    // Register the CYCLE statement.
    m_database.setCycle(lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FstopStatement (STOP statement).
   *
   * @param visitable STOP statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FstopStatement visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // STOP statement
    String arg = null;
    String code = visitable.getCode();
    String msg = visitable.getMessage();
    if (!StringUtils.isNullOrEmpty(code)) {
      arg = code;
    } else if (!StringUtils.isNullOrEmpty(msg)) {
      arg = msg;
    }

    // Register the STOP statement.
    m_database.setStop(arg, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FpauseStatement (PAUSE statement).
   *
   * @param visitable PAUSE statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FpauseStatement visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // PAUSE statement
    String arg = null;
    String code = visitable.getCode();
    String msg = visitable.getMessage();
    if (!StringUtils.isNullOrEmpty(code)) {
      arg = code;
    } else if (!StringUtils.isNullOrEmpty(msg)) {
      arg = msg;
    }

    // Register the PAUSE statement.
    m_database.setPause(arg, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of GotoStatement (GOTO statement).
   *
   * @param visitable GOTO statement
   * @return Success or failure
   */
  @Override
  public boolean enter(GotoStatement visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // GOTO statement
    String arg = visitable.getLabelName();
    if (StringUtils.isNullOrEmpty(arg)) {
      GotoStatementSequence seq =
          new GotoStatementSequence(visitable.getParams(), visitable.getValue());
      return enter(seq);
    }

    // Register the GOTO statement.
    // If it is a line number, use a numeric character (delete the first 0)
    if (StringUtils.isNumeric(arg)) {
      arg = Integer.valueOf(arg).toString();
    }
    m_database.setGoTo(arg, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of GotoStatementSequence (GOTO statement).
   *
   * @param visitable GOTO statement
   * @return Success or failure
   */
  @Override
  public boolean enter(GotoStatementSequence visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // GOTO statement
    StringBuffer arg = new StringBuffer();
    {
      Params params = visitable.getParams();
      exprParser.setParseNode(params);
      Expression expr = exprParser.getExpression();
      if (expr != null && !StringUtils.isNullOrEmpty(expr.getLine())) {
        arg.append("(");
        arg.append(expr.getLine());
        arg.append("), ");
      }
    }
    {
      Value value = visitable.getValue();
      exprParser.setParseNode(value);
      Expression expr = exprParser.getExpression();
      if (expr != null && !StringUtils.isNullOrEmpty(expr.getLine())) {
        arg.append(expr.getLine());
      }
    }

    // Register the GOTO statement.
    m_database.setGoTo(arg.toString(), lineInfo, label);

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the start element of FnullifyStatement (NULLIFY statement).
   *
   * @param visitable NULLIFY statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FnullifyStatement visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    List<Alloc> list = visitable.getAlloc();
    List<Variable> varlist = new ArrayList<Variable>();
    if (list != null) {
      for (Alloc nodeAlloc : list) {
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(nodeAlloc);

        // Variable
        Variable var = varParser.getVariable(node);
        // add a variable
        if (var != null) {
          varlist.add(var);
        }
      }
    }

    // Register the NULLIFY statement.
    m_database.setNullify(varlist, lineInfo, label);

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the start element of FallocateStatement (ALLOCATE statement).
   *
   * @param visitable ALLOCATE statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FallocateStatement visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    List<Alloc> list = visitable.getAlloc();
    String stat = visitable.getStatName();

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    // VariableDimension parser
    VariableDimensionParser dimsParser = new VariableDimensionParser(m_context.getTypeManager());

    Map<Variable, VariableDimension> targets = new LinkedHashMap<Variable, VariableDimension>();
    if (list != null) {
      for (Alloc nodeAlloc : list) {
        Variable var = null;
        VariableDimension dims = null;

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(nodeAlloc);
        var = varParser.getVariable(node);

        // Array declaration
        List<IXmlNode> arrayElems = nodeAlloc.getIndexRangeOrArrayIndex();
        if (arrayElems != null && arrayElems.size() > 0) {
          dims = dimsParser.parseVariableDimension(arrayElems);
        }

        if (var != null) {
          targets.put(var, dims);
        }
      }
    }

    // Register the ALLOCATE statement.
    Variable err = null;
    if (stat != null) {
      err = new Variable(stat);
    }
    m_database.setAllocate(targets, err, lineInfo, label);
    return true;
  }

  /**
   * Register the start element of FdeallocateStatement (DEALLOCATE statement).
   *
   * @param visitable DEALLOCATE statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FdeallocateStatement visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    List<Alloc> list = visitable.getAlloc();
    String stat = visitable.getStatName();

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());
    List<Variable> targets = new ArrayList<Variable>();
    if (list != null) {
      for (Alloc nodeAlloc : list) {
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(nodeAlloc);
        Variable var = varParser.getVariable(node);
        if (var != null) {
          targets.add(var);
        }
      }
    }

    // Register the ALLOCATE statement.
    Variable err = null;
    if (stat != null) {
      err = new Variable(stat);
    }
    m_database.setDeallocate(targets, err, lineInfo, label);
    return true;
  }

  /**
   * Register the start element of FdataDecl (DATA statement).
   *
   * @param visitable DATA statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FdataDecl visitable) {

    List<IXmlNode> list = visitable.getVarListAndValueList();
    if (list == null) {
      return false;
    }

    // ((varList, valueList)+)
    for (int i = 0; i < list.size(); i = i + 2) {
      if (i + 1 >= list.size()) {
        break;
      }
      if (!(list.get(i) instanceof VarList)) {
        continue;
      }
      if (!(list.get(i + 1) instanceof ValueList)) {
        continue;
      }
      FdataDeclSequence dataSeq =
          new FdataDeclSequence((VarList) list.get(i), (ValueList) list.get(i + 1));
      boolean result = enter(dataSeq);
      if (!result) return false;
    }

    return true;
  }

  /**
   * Register the start element of FdataDeclSequence (DATA statement).
   *
   * @param visitable DATA statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FdataDeclSequence visitable) {
    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Expression Parser Model Parser
    ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    // ((varList, valueList)+)
    try {
      // VarList
      List<Variable> vars = varParser.getVariableList(visitable.getVarList());

      // ValueList
      Expression[] valueList = exprParser.getExpressionArray(visitable.getValueList());

      if (vars != null && valueList != null) {
        Data block = new Data();
        block.setVariables(vars);
        if (valueList != null) {
          List<Expression> values = new ArrayList<Expression>();
          values.addAll(java.util.Arrays.asList(valueList));
          block.setValues(values);
        }

        // Register the Data statement.
        m_database.setData(block, lineInfo, label);
      }

    } catch (XcodeMLException e) {
      e.printStackTrace();
      return false;
    }

    // Registration of external procedures
    addExternalFunction(exprParser.getExternalFunction());

    return true;
  }

  /**
   * Register the starting element of FequivalenceDecl (EQUIVALENCE statement).
   *
   * @param visitable EQUIVALENCE statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FequivalenceDecl visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    List<IXmlNode> list = visitable.getVarRefAndVarList();
    List<Variable> variables = new ArrayList<Variable>();
    for (int i = 0; i < list.size(); i++) {
      if (list.get(i) instanceof VarRef) {
        // VarRef
        Variable var = varParser.getVariable(list.get(i));
        variables.add(var);
      } else if (list.get(i) instanceof VarList) {
        // VarList
        List<IXmlNode> varRefOrFdoLoop = ((VarList) list.get(i)).getVarRefOrFdoLoop();
        if (varRefOrFdoLoop != null) {
          for (IXmlNode varRef : varRefOrFdoLoop) {
            Variable var = varParser.getVariable(varRef);
            variables.add(var);
          }
        }
      }
    }

    // Register the EQUIVALENCE statement.
    Equivalence block = new Equivalence();
    block.setVariables(variables);
    m_database.setEquivalence(block, lineInfo, label);

    return true;
  }

  /**
   * Register the start element of FcommonDecl (COMMON statement).
   *
   * @param visitable COMMON statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FcommonDecl visitable) {

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Parse variables
    VariableParser varParser = new VariableParser(m_context.getTypeManager());

    List<VarList> list = visitable.getVarList();
    for (int i = 0; i < list.size(); i++) {
      // name
      String name = list.get(i).getName();
      // VarList
      List<IXmlNode> varRefOrFdoLoop = list.get(i).getVarRefOrFdoLoop();
      List<Variable> variables = new ArrayList<Variable>();
      if (varRefOrFdoLoop != null) {
        for (IXmlNode varRef : varRefOrFdoLoop) {
          Variable var = varParser.getVariable(varRef);
          variables.add(var);
        }
      }

      if (variables != null && variables.size() > 0) {
        // Register the COMMON statement.
        Common block = new Common();
        block.setName(name);
        block.setVariables(variables);
        m_database.setCommon(block, lineInfo, label);
      }
    }

    return true;
  }

  /**
   * Register the start element of FinterfaceDecl (INTERFACE statement).
   *
   * @param visitable INTERFACE statement
   * @return Success or failure
   */
  @Override
  public boolean enter(FinterfaceDecl visitable) {

    // Code output class
    XcodeMLTypeManager typeManager = m_context.getTypeManager();

    String label = Statement.NO_LABEL;
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // interface name
    String infName = visitable.getName();
    List<IXmlNode> list = visitable.getFfunctionDeclOrFmoduleProcedureDecl();

    VariableTypeParser typePaser = new VariableTypeParser(this.m_context.getTypeManager());
    VariableDefinitionParser defPaser =
        new VariableDefinitionParser(this.m_context.getTypeManager());

    // Generate interface name
    if (XmlNodeUtil.isBoolean(visitable.isIsOperator())) {
      infName = "OPERATOR(" + infName + ")";
    } else if (XmlNodeUtil.isBoolean(visitable.isIsAssignment())) {
      infName = "ASSIGNMENT(" + infName + ")";
    }
    // INTERFACE statement class generation
    Procedures interProc = new Procedures(infName);
    for (IXmlNode node : list) {
      if (node instanceof FfunctionDecl) {
        Name itemName = ((FfunctionDecl) node).getName();
        ;

        IXmlTypeTableChoice typeChoice = typeManager.findType(itemName);
        FfunctionType functionTypeElem = (FfunctionType) typeChoice;
        String returnTypeName = functionTypeElem.getReturnType();
        // EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
        // boolean is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
        // boolean is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
        // boolean is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());
        // result
        String result = functionTypeElem.getResultName();

        // Generic definition function data type
        VariableType funcType = typePaser.parseVariableType(((FfunctionDecl) node).getName());

        // Formal argument
        Params params = functionTypeElem.getParams();
        jp.riken.kscope.language.generic.Arguments args =
            new jp.riken.kscope.language.generic.Arguments();
        if (params != null) {
          for (Name nameElem : params.getName()) {
            Argument arg = new Argument(nameElem.getValue());
            // modify by @hira at 2013/01/10
            // VariableDefinition argDef = defPaser.parseVariableDefinition(nameElem);
            VariableDefinition argDef = defPaser.parseVariableDefinitionWithFfunctionType(nameElem);
            arg.setType(argDef.getVariableType());
            arg.setVariableAttributes(argDef.getAttribute());

            args.add(arg);
          }
        }
        ProcedureItem func = new ProcedureItem(itemName.getValue(), funcType, args);
        func.setResult(result);
        interProc.add(func);
      } else if (node instanceof FmoduleProcedureDecl) {
        List<Name> names = ((FmoduleProcedureDecl) node).getName();
        for (Name modName : names) {
          ProcedureWithNameOnly modProc = new ProcedureWithNameOnly(modName.getValue());
          interProc.add(modProc);
        }
      }
    }

    // m_database.addInterface(interProc);
    m_database.setInterface(interProc, lineInfo, label);

    return true;
  }

  /**
   * Add to external procedure list
   *
   * @param funcName External procedure name
   * @param varType data type
   */
  private void addExternalFunction(String funcName, IVariableType varType) {
    if (funcName == null || varType == null) return;

    // Add to external procedure list
    m_database.addExternalFunction(funcName, varType);
  }

  /**
   * Add to external procedure list
   *
   * @param list External procedure list
   */
  private void addExternalFunction(Map<String, IVariableType> list) {
    if (list == null || list.size() <= 0) return;

    // Add to external procedure list
    for (Iterator<Entry<String, IVariableType>> itr = list.entrySet().iterator(); itr.hasNext(); ) {
      Map.Entry<String, IVariableType> entry = itr.next();
      String funcName = entry.getKey();
      IVariableType varType = entry.getValue();
      m_database.addExternalFunction(funcName, varType);
    }
  }

  @Override
  public void leave(FstructDecl visitable) {
    // Source file, source code line
    CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

    // Variable name
    Name nameStruct = visitable.getName();
    String name = nameStruct.getValue();
    if (this.m_database.getCurrentUnit() == null) return;
    jp.riken.kscope.language.fortran.Type type = this.m_database.getCurrentUnit().getType(name);
    if (type == null) return;
    type.setEndCodeLine(lineInfo);
  }

  /**
   * Register error information.
   *
   * @param line Code line information
   * @param message Error message
   */
  private void addErrorInfo(CodeLine line, String message) {
    if (this.listErrorInfo == null) {
      this.listErrorInfo = new ArrayList<ErrorInfo>();
    }
    this.listErrorInfo.add(new ErrorInfo(line, message));
  }

  /**
   * Get the error list.
   *
   * @return error list
   */
  public ErrorInfo[] getListErrorInfo() {
    if (this.listErrorInfo == null || this.listErrorInfo.size() <= 0) {
      return null;
    }
    return this.listErrorInfo.toArray(new ErrorInfo[0]);
  }

  /**
   * Get duplicate procedure error messages.
   *
   * @param overridename Duplicate procedure name
   * @param lineInfo Error code information
   * @param duplicateUnit Duplicate block
   */
  private void addDuplicateError(
      String overridename, CodeLine lineInfo, ProgramUnit duplicateUnit) {
    if (overridename == null) return;
    if (lineInfo == null) return;
    if (duplicateUnit == null) return;

    String errorname = overridename;
    if (!overridename.equalsIgnoreCase(duplicateUnit.get_name())) {
      errorname += "," + duplicateUnit.get_name();
    }
    String dupInfo = getErrorLineInfo(duplicateUnit);
    if (dupInfo == null) return;
    String key = null;
    // dbupdate.error.duplicate.module = [Warning] MODULE [% s] is duplicated. override =% s.
    if (duplicateUnit instanceof Module) {
      key = "dbupdate.error.duplicate.module";
    } else if (duplicateUnit instanceof Procedure) {
      // dbupdate.error.duplicate.program = [Warning] PROGRAM [% s,% s] is duplicated. override =%
      // s.
      if (((Procedure) duplicateUnit).isProgram()) {
        key = "dbupdate.error.duplicate.program";
      }
      // dbupdate.error.duplicate.subroutine = [Warning] SUBROUTINE [% s] is duplicated. override =%
      // s.
      else if (((Procedure) duplicateUnit).isSubroutine()) {
        key = "dbupdate.error.duplicate.subroutine";
      }
      // dbupdate.error.duplicate.function = [Warning] Duplicate FUNCTION [% s]. override =% s.
      else if (((Procedure) duplicateUnit).isFunction()) {
        key = "dbupdate.error.duplicate.function";
      }
    }
    if (key == null) return;
    String msg = Message.getString(key, errorname, dupInfo);
    addErrorInfo(lineInfo, msg);
  }

  /**
   * Create an error display string for code line information.
   *
   * @param block Error block
   * @return Error display string
   */
  private String getErrorLineInfo(ProgramUnit block) {
    if (block == null) return null;
    if (block.get_start() == null) return null;
    if (block.get_start().getLineInfo() == null) return null;
    CodeLine line = block.get_start().getLineInfo();
    if (line.getSourceFile() == null || line.getSourceFile().getFile() == null) return null;
    // File baseFolder = m_context.getBaseFolder();
    // if (baseFolder == null) return null;
    String relative = line.getSourceFile().getFile().getPath();
    if (relative == null) return null;

    StringBuffer buf = new StringBuffer();
    buf.append("[");
    buf.append(relative);
    buf.append("] ");
    buf.append(line.getStartLine());
    buf.append(":");
    buf.append(block.toString());

    return buf.toString();
  }
}
