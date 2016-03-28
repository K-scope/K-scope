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

package jp.riken.kscope.xcodeml.fortran;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
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
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.fortran.parser.ExpressionParser;
import jp.riken.kscope.xcodeml.fortran.parser.VariableDefinitionParser;
import jp.riken.kscope.xcodeml.fortran.parser.VariableDimensionParser;
import jp.riken.kscope.xcodeml.fortran.parser.VariableParser;
import jp.riken.kscope.xcodeml.fortran.parser.VariableTypeParser;
import jp.riken.kscope.xcodeml.fortran.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.fortran.xml.EnumType;
import jp.riken.kscope.xcodeml.fortran.xml.FdataDeclSequence;
import jp.riken.kscope.xcodeml.fortran.xml.GotoStatementSequence;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Alloc;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Body;
import jp.riken.kscope.xcodeml.fortran.xml.gen.ContinueStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Else;
import jp.riken.kscope.xcodeml.fortran.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FallocateStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FassignStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcaseLabel;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcommonDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcycleStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdataDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdeallocateStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdoStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdoWhileStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FequivalenceDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FexitStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FfunctionDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FfunctionDefinition;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FifStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FinterfaceDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FmoduleDefinition;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FmoduleProcedureDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FnullifyStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FpauseStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FpointerAssignStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FpragmaStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FreturnStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FselectCaseStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FstopStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FstructDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FuseDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FuseOnlyDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FwhereStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.GotoStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Name;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Params;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Renamable;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Rename;
import jp.riken.kscope.xcodeml.fortran.xml.gen.StatementLabel;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Then;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Value;
import jp.riken.kscope.xcodeml.fortran.xml.gen.ValueList;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Var;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarList;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarRef;

/**
 * フォートランデータベースにパース結果を格納する
 * @author RIKEN
 *
 */
public class DbUpdater extends XcodeMLVisitorImpl {
    /** フォートランデータベース */
    private Fortran m_database = null;
    /** XcodeMLパーサ実行状況クラス */
    private XcodeMLContext m_context;
    /** 前ノードの文番号ノード */
    @SuppressWarnings("unused")
    private StatementLabel prevLabel = null;
    /** エラー情報リスト */
    private List<ErrorInfo> listErrorInfo;

    /**
     * コンストラクタ
     *
     * @param db
     *            Fortranデータベース
     * @param context
     *            XcodeMLパーサ実行状況クラス
     */
    public DbUpdater(Fortran db, XcodeMLContext context) {
        m_database = db;
        m_context = context;
    }

    /**
     * FfunctionDefinition要素（MAIN PROGRAM・関数・サブルーチン定義）の開始を登録する。
     *
     * @param visitable
     *            FfunctionDefinition要素
     * @return boolean 成否
     */
    @Override
    public boolean enter(FfunctionDefinition visitable) {

        // コード出力クラス
        XcodeMLTypeManager typeManager = m_context.getTypeManager();
        VariableTypeParser typePaser = new VariableTypeParser(typeManager);

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // PROGRAM, SUBROUTINE, FUNCTION文の情報
        // プログラム名、サブルーチン名、関数名
        Name functionNameElem = visitable.getName();
        String functionname = functionNameElem.getValue();

        // データ型
        IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
        FfunctionType functionTypeElem = (FfunctionType) typeChoice;
        VariableType funcType = typePaser.parseVarDefFunctionType(functionTypeElem);

        String returnTypeName = functionTypeElem.getReturnType();
        EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);

        // result
        String result = functionTypeElem.getResultName();

        // 属性
        boolean is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
        boolean is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
        boolean is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());

        // 仮引数
        Params params = functionTypeElem.getParams();
        ArrayList<String> args = new ArrayList<String>();
        if (params != null) {
            for (Name nameElem : params.getName()) {
                args.add(nameElem.getValue());
            }
        }

        // データベース登録
        if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
            // 既存のプログラムが存在するかチェックする.
            Procedure errorProc = m_database.getMainProgram();
            if (errorProc != null) {
                addDuplicateError(functionname, lineInfo, errorProc);
            }
            // PROGRAM
            m_database.init_main(functionname);
        }
        else {
            if (typeId == EnumType.VOID) {
                // 既存のサブルーチンが存在するかチェックする.
                Procedure errorProc = m_database.getProcedure(m_database.get_current_unit(), functionname);
                if (errorProc != null) {
                    addDuplicateError(functionname, lineInfo, errorProc);
                }
                // SUBROUTINE
                if (args.size() <= 0) {
                    m_database.initSubroutine(functionname);
                } else {
                    m_database.initSubroutine(functionname,
                            args.toArray(new String[0]));
                }
                funcType = new VariableType(PrimitiveDataType.VOID);
            }
            else {
                // 既存の関数が存在するかチェックする.
                Procedure errorProc = m_database.getProcedure(m_database.get_current_unit(), functionname);
                if (errorProc != null) {
                    addDuplicateError(functionname, lineInfo, errorProc);
                }
                // FUNCTION
                if (args.size() <= 0) {
                    m_database.init_function(functionname);
                } else {
                    m_database.init_function(functionname,
                            args.toArray(new String[0]));
                }
            }
        }
        m_database.get_current_unit().set_start(lineInfo);

        // 関数データ型
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
     * FfunctionDefinition要素（MAIN PROGRAM・関数・サブルーチン定義）の終了を登録する。
     *
     * @param visitable
     *            FfunctionDefinition要素
     */
    @Override
    public void leave(FfunctionDefinition visitable) {

        // コード出力クラス
        XcodeMLTypeManager typeManager = m_context.getTypeManager();

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // PROGRAM, SUBROUTINE, FUNCTION文の情報
        // プログラム名、サブルーチン名、関数名
        Name functionNameElem = visitable.getName();
        String functionname = functionNameElem.getValue();

        // 宣言文
        IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
        FfunctionType functionTypeElem = (FfunctionType) typeChoice;
        String returnTypeName = functionTypeElem.getReturnType();
        EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);

        // データベース登録
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
     * FunctionCall要素(関数・サブルーチン呼び出し)を登録する。
     *
     * @param visitable
     *            FunctionCall
     * @param call_line
     *            サブルーチン呼び出し文、又は関数名(引数)の部分文字列
     * @return 成否
     */
    public boolean enterFunctionCall(FunctionCall visitable, String call_line) {

        String label = Statement.NO_LABEL;

        // 1つ上のノードがExprStatementであるか？
        if (!m_context.isInvokeNodeOf(ExprStatement.class, 1)) {
            return true;
        }

        // データベースがfunctionCallを登録可能であるか(現在ブロックがProgramUnitであるか?)
        if (!isCurrentProgramUnit()) {
            return true;
        }

        // データ型パーサ
        VariableTypeParser typePaser = new VariableTypeParser(this.m_context.getTypeManager());
        VariableType funcVarType = null;

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().createCodeLine(
                visitable, call_line);
        int lineno = lineInfo.getStartLine();

        // set_callの引数
        List<Expression> list = null;
        // サブルーチン名、関数名
        Name functionNameElem = visitable.getName();
        String functionname = functionNameElem.getValue();

        // 属性
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
                //EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
                is_intrinsic = XmlNodeUtil.isBoolean(functionTypeElem.isIsIntrinsic());
                is_external = XmlNodeUtil.isBoolean(functionTypeElem.isIsExternal());
                is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
                is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
                is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());
                funcVarType = typePaser.parseVarDefFunctionType(functionTypeElem);
            }
        }
        boolean intrinsic = XmlNodeUtil.isBoolean(visitable.isIsIntrinsic());

        // 引数リスト
        try {
            Arguments arguments = visitable.getArguments();
            ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());
            Expression[] argsExpr = exprParser.getExpressionArray(arguments);
            if (argsExpr != null) {
                list = new ArrayList<Expression>(java.util.Arrays.asList(argsExpr));
            }

            // 外部手続きの登録
            addExternalFunction(exprParser.getExternalFunction());

        } catch (XcodeMLException ex) {
            ex.printStackTrace();
        }
        // CALL文、関数呼出登録
        m_database.setCall(lineInfo, label, functionname, list, intrinsic);
        if (is_external) {
            // 外部手続きの登録
            addExternalFunction(functionname, funcVarType);
        }

        return true;
    }

    /**
     * FortranクラスのcurrentUnitがProgramUnitであるかチェックする。
     *
     * @return true:currentUnitはProgramUnitである。
     */
    private boolean isCurrentProgramUnit() {
        ProgramUnit unit = m_database.get_current_unit();
        if (unit instanceof ProgramUnit) {
            return true;
        }

        return false;
    }

    /**
     * 文番号をパースする。
     * @param visitable        StatementLabel
     * @param nextNode        文番号対象要素
     * @return 成否
     */
    public boolean enterStatementLabel(StatementLabel visitable,
            IXmlNode nextNode) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        if ((nextNode != null) && (nextNode instanceof StatementLabel)) {
            // 文番号のみの行
            // CONTINUE文として登録する。
            if (!StringUtils.isNullOrEmpty(visitable.getLabelName())) {
                label = visitable.getLabelName();
            }
            // StatementLabel文を登録する。
            m_database.setContinue(lineInfo, label);
        } else {
            // 何もしない。
            // 次行で文番号と共に登録する。
            prevLabel = visitable;
        }

        return true;
    }

    /**
     * 引数リストをパースする.<br/>
     *
     * コードビルダクラスから引数文字列リストを取得する。 引数リストメンバにセットする。
     *
     * @param visitable
     *            Arguments
     * @return 成否
     */
    @Override
    public boolean enter(Arguments visitable) {

        // 引数リストを取得する。
        // m_argumentList =
        // m_context.getCodeBuilder().getArgumentList(visitable);

        return true;
    }

    /**
     * FassignStatement要素(代入文)を登録する。
     *
     * @param visitable
     *            FassignStatement
     * @return 成否
     */
    @Override
    public boolean enter(FassignStatement visitable) {
        String label = Statement.NO_LABEL;

//        try {
            // データベースがfunctionCallを登録可能であるか(現在ブロックがProgramUnitであるか?)
            if (!isCurrentProgramUnit()) {
                return true;
            }

            // ソースファイル、ソースコード行
            CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
            int lineno = lineInfo.getStartLine();

            List<IXmlNode> content = visitable.getContent();
            // 左辺
            IXmlNode leftExpr = (content != null && content.size() >= 1) ? content
                    .get(0) : null;
            // 右辺
            IXmlNode rightExpr = (content != null && content.size() >= 2) ? content
                    .get(1) : null;

            // ExpressionParserモデルパーサ
            ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

            // 変数のパース
            VariableParser varParser = new VariableParser(m_context.getTypeManager());

            // 左辺
            // Variable leftVar = varParser.getVariable(leftExpr);
            exprParser.setParseNode(leftExpr);
            Expression leftVar = exprParser.getExpression();

            // 右辺
            exprParser.setParseNode(rightExpr);
            Expression rightVar = exprParser.getExpression();

            m_database.setSubstitution(leftVar, rightVar, lineInfo, label);

            // 外部手続きの登録
            addExternalFunction(exprParser.getExternalFunction());

            return true;

    }

    /**
     * FdoStatement(DO文)の開始要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FdoStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // DO構文名 (SSSS : DO)
        String construct_name = visitable.getConstructName();

        // 変数名
        Var var = visitable.getVar();
        if (var == null) {
            // 条件文なし（無限ループ）のDO文
            if (construct_name != null) {
                m_database.start_loop(lineInfo, construct_name);
            } else {
                m_database.start_loop(lineInfo, Statement.NO_LABEL);
            }
            return true;
        }

        // 変数データ型の取得
        VariableDefinitionParser varParser = new VariableDefinitionParser(this.m_context.getTypeManager());
        VariableDefinition varDef = varParser.parseVariableDefinition(var);
        Variable variable = new Variable(var.getValue());
        variable.setDefinition(varDef);

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // ループ式をパースする。
        // 範囲の下限を取得する
        Expression lowerExp = null;
        if (visitable.getIndexRange() != null && visitable.getIndexRange().getLowerBound() != null) {
            exprParser.setParseNode(visitable.getIndexRange().getLowerBound());
            // 範囲の下限を取得する
            lowerExp = exprParser.getExpression();
        }
        // 範囲の上限を取得する
        Expression upperExp = null;
        if (visitable.getIndexRange() != null && visitable.getIndexRange().getUpperBound() != null) {
            exprParser.setParseNode(visitable.getIndexRange().getUpperBound());
            // 範囲の上限を取得する
            upperExp = exprParser.getExpression();
        }
        // 範囲のステップを取得する
        Expression stepExp = null;
        if (visitable.getIndexRange() != null && visitable.getIndexRange().getUpperBound() != null) {
            exprParser.setParseNode(visitable.getIndexRange().getStep());
            // 範囲のステップを取得する
            stepExp = exprParser.getExpression();
        }

        // DO文のブロック開始
        if (construct_name != null) {
            m_database.start_loop(lineInfo, construct_name, variable, lowerExp, upperExp, stepExp);
        } else {
            m_database.start_loop(lineInfo, Statement.NO_LABEL, variable, lowerExp, upperExp, stepExp);
        }

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FdoStatement(DO文)の終了要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     */
    @Override
    public void leave(FdoStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // DO文のブロック終了
        m_database.end_loop(lineInfo);
    }

    /**
     * FmoduleDefinition(MODULE文)の開始要素を登録する。
     *
     * @param visitable
     *            FmoduleDefinition(MODULE文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FmoduleDefinition visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        String name = visitable.getName();
        // 既存のモジュールが存在するかチェックする.
        Module module = m_database.module(name);
        if (module != null && module.get_start() != null && module.get_start().getLineInfo() != null) {
            addDuplicateError(name, lineInfo, module);
        }
        m_database.init_module(name);
        m_database.get_current_unit().set_start(lineInfo);

        return true;
    }

    /**
     * FmoduleDefinition(MODULE文)の終了要素を登録する。
     *
     * @param visitable
     *            FmoduleDefinition(MODULE文)要素
     */
    @Override
    public void leave(FmoduleDefinition visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        m_database.get_current_unit().set_end(lineInfo);
        m_database.end_module();
    }

    /**
     * ContinueStatement(CONTAINUE文)の要素を登録する
     *
     * @param visitable
     *            ContinueStatement(CONTAINUE文)要素
     */
    @Override
    public boolean enter(ContinueStatement visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        if (!StringUtils.isNullOrEmpty(m_context.getStatementLabel())) {
            label = m_context.getStatementLabel();
        }
        // StatementLabel文を登録する。
        m_database.setContinue(lineInfo, label);

        return true;
    }

    /**
     * FdoWhileStatement(DO WHILE文)の開始要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FdoWhileStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // DO構文名 (SSSS : DO)
        String construct_name = visitable.getConstructName();

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // ループ条件式をパースする。
        // ループ条件式を取得する
        Expression conditionExp = null;
        if (visitable.getCondition() != null) {
            exprParser.setParseNode(visitable.getCondition());
            // 範囲の下限を取得する
            conditionExp = exprParser.getExpression();
        }

        // DO文のブロック開始
        if (construct_name != null) {
            m_database.start_loop(lineInfo, construct_name, null, null, conditionExp, null);
        } else {
            m_database.start_loop(lineInfo, Statement.NO_LABEL, null, null, conditionExp, null);
        }

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FdoWhileStatement(DO WHILE文)の終了要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     */
    @Override
    public void leave(FdoWhileStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // DO文のブロック終了
        m_database.end_loop(lineInfo);
    }

    /**
     * FselectCaseStatement(SELECT-CASE文)の開始要素を登録する。
     *
     * @param visitable
     *            FselectCaseStatement(SELECT-CASE文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FselectCaseStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // SELECT構文名 (SSSS : SELECT)
        String construct_name = visitable.getConstructName();

        if (construct_name != null) {
            m_database.startSelection(lineInfo, construct_name, jp.riken.kscope.language.Selection.SelectionType.SELECT);
        } else {
            m_database.startSelection(lineInfo, Statement.NO_LABEL, jp.riken.kscope.language.Selection.SelectionType.SELECT);
        }

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable.getValue());

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        // 条件式
        m_database.setSelectCaseCondition(condExpr);

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FselectCaseStatement(SELECT-CASE文)の終了要素を登録する。
     *
     * @param visitable
     *            FselectCaseStatement(SELECT-CASE文)要素
     */
    @Override
    public void leave(FselectCaseStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // SELECT文のブロック終了
        // m_database.endCondition(lineInfo, Statement.NO_LABEL);
        m_database.end_selection(lineInfo);

        return;
    }

    /**
     * FcaseLabel(CASE文)の開始要素を登録する。
     *
     * @param visitable
     *            FcaseLabel(CASE文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FcaseLabel visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

//        if (((Procedure) (m_database.currentUnit)).body.getCurrentBlock() instanceof Condition) {
//            m_database.endCondition(lineInfo, Statement.NO_LABEL);
//        }

        // 行ラベル
        String construct_name = visitable.getConstructName();

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable);

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        if (construct_name != null) {
            m_database.startCondition(condExpr, lineInfo, construct_name);
        }
        else {
            m_database.startCondition(condExpr, lineInfo, Statement.NO_LABEL);
        }

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FcaseLabel(CASE文)の終了要素を登録する。
     *
     * @param visitable
     *            FcaseLabel(CASE文)要素
     */
    @Override
    public void leave(FcaseLabel visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
        m_database.endCondition(lineInfo, Statement.NO_LABEL);
    }

    /**
     * FifStatement(IF文)の開始要素を登録する。
     *
     * @param visitable
     *            FifStatement(IF文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FifStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // IF構文名 (SSSS : IF)
        String construct_name = visitable.getConstructName();

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable.getCondition());

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        // ElseIF文であるかチェックする。
        boolean isElseIf = false;
        // 2つ上のノードがElseであるか？
        if (m_context.isInvokeNodeOf(Else.class, 2)) {
            Else elseNode = (Else) m_context.getInvokeNode(2);
            // 開始行番号が設定されているか？
            if (StringUtils.isNullOrEmpty(elseNode.getLineno())) {
                // 2つ上のElse要素の開始行番号が設定されていないのでElseIF文である。
                isElseIf = true;
            }
        }

        //
        if (!isElseIf) {
            // IF文
            if (construct_name != null) {
                m_database.startSelection(lineInfo, construct_name,
                        jp.riken.kscope.language.Selection.SelectionType.IF);
            } else {
                m_database.startSelection(lineInfo, Statement.NO_LABEL, jp.riken.kscope.language.Selection.SelectionType.IF);
            }

            // 条件式
            m_database.startCondition(condExpr, lineInfo, visitable.getConstructName());
        } else {
            // ELSE-IF文
            // String line = "ELSE IF " + cond_str;
            // 条件式
            m_database.startCondition(condExpr, lineInfo, visitable.getConstructName());
        }

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FifStatement(IF文)の終了要素を登録する。
     *
     * @param visitable
     *            FifStatement(IF文)要素
     */
    @Override
    public void leave(FifStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ElseIF文デあるかチェックする。
        boolean isElseIf = false;
        // 2つ上のノードがElseであるか？
        if (m_context.isInvokeNodeOf(Else.class, 2)) {
            Else elseNode = (Else) m_context.getInvokeNode(2);
            // 開始行番号が設定されているか？
            if (StringUtils.isNullOrEmpty(elseNode.getLineno())) {
                // 2つ上のElse要素の開始行番号が設定されていないのでElseIF文である。
                isElseIf = true;
            }
        }

        // ELSE-IFの場合はend_selectionを呼び出さない。
        if (!isElseIf) {
            m_database.end_selection(lineInfo);
        }

        return;
    }

    /**
     * Then(真の文ブロック)の終了要素を登録する。
     *
     * @param visitable
     *            Then(真の文ブロック)
     */
    @Override
    public void leave(Then visitable) {

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
        m_database.endCondition(lineInfo, Statement.NO_LABEL);

        return;
    }

    /**
     * Else(偽の文ブロック)の開始要素を登録する。
     *
     * @param visitable
     *            Else(偽の文ブロック)
     * @return 成否
     */
    @Override
    public boolean enter(Else visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // Elseであるかチェックする。
        boolean isElseIf = false;
        // 開始行番号が設定されているか？
        if (StringUtils.isNullOrEmpty(visitable.getLineno())) {
            // 開始行番号が設定されていないのでElseIF文である。
            isElseIf = true;
        }

        // ELSE-IF文の場合は次のIF要素でDBに登録する。
        if (!isElseIf) {
            m_database.startCondition(null, lineInfo, Statement.NO_LABEL);
        }
        return true;
    }

    /**
     * Else(偽の文ブロック)の終了要素を登録する。
     *
     * @param visitable
     *            Else(偽の文ブロック)
     */
    @Override
    public void leave(Else visitable) {

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ElseIF文デあるかチェックする。
        boolean isElseIf = false;
        // 開始行番号が設定されているか？
        if (StringUtils.isNullOrEmpty(visitable.getLineno())) {
            // 開始行番号が設定されていないのでElseIF文である。
            isElseIf = true;
        }

        // ELSE-IF文の場合はend_conditionを呼び出さない。
        if (!isElseIf) {
            m_database.endCondition(lineInfo, Statement.NO_LABEL);
        }
        return;
    }

    /**
     * FwhereStatement(WHERE文)の開始要素を登録する。
     *
     * @param visitable
     *            FwhereStatement(WHERE文)要素
     * @return 成否
     */
    @Override
    public boolean enter(FwhereStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable.getCondition());

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        m_database.startSelection(lineInfo, Statement.NO_LABEL, jp.riken.kscope.language.Selection.SelectionType.WHERE);

        // 条件式
        m_database.startCondition(condExpr, lineInfo, Statement.NO_LABEL);

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FwhereStatement(WHERE文)の終了要素を登録する。
     *
     * @param visitable
     *            FwhereStatement(WHERE文)要素
     */
    @Override
    public void leave(FwhereStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        m_database.end_selection(lineInfo);

        return;
    }


    /**
     * Return(RETURN文)の開始要素を登録する。
     *
     * @param visitable
     *            RETURN文
     * @return 成否
     */
    @Override
    public boolean enter(FreturnStatement visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // RETURN文
        m_database.setReturn(lineInfo, label);

        return true;

    }

    /**
     * VarDecl(変数宣言文)の開始要素を登録する。
     *
     * @param visitable
     *            VarDecl(変数宣言文)
     * @return 成否
     */
    @Override
    public boolean enter(VarDecl visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 変数宣言文
        VariableDefinitionParser varParser = new VariableDefinitionParser(this.m_context.getTypeManager());
        VariableDefinition varDef = varParser.parseVariableDefinition(visitable.getName());
        if (varDef == null) {
            // 組込関数の型宣言の場合はnull
//            assert (varDef != null);
            return true;
        }

        // ソースコード行を設定する
        varDef.setCodeLine(lineInfo);

        // 初期値をパースする。
        if (visitable.getValue() != null) {
            // ExpressionParserモデルパーサ
            ExpressionParser exprParser = new ExpressionParser(
                                                    m_context.getTypeManager(),
                                                    visitable.getValue());
            Expression init_value = exprParser.getExpression();
            if (init_value != null) {
                varDef.setInitValue(init_value); // 初期値
            }
            // 外部手続きの登録
            addExternalFunction(exprParser.getExternalFunction());
        }

        // 変数宣言文を登録する。
        m_database.set_variable_def(varDef);

        return true;
    }

    /**
     * FstructDecl(変数宣言文)の開始要素を登録する。
     *
     * @param visitable
     *            FstructDecl(構造体の定義)
     * @return 成否
     */
    @Override
    public boolean enter(FstructDecl visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 変数宣言文
        VariableDefinitionParser parserDef = new VariableDefinitionParser(this.m_context.getTypeManager());
        VariableDefinition def = parserDef.parseVariableDefinition(visitable);
        if (def == null) return false;
        if (def.getVariableType() == null)  return false;
        if (((VariableType)def.getVariableType()).getPrimitiveDataType() != PrimitiveDataType.TYPE
            && ((VariableType)def.getVariableType()).getPrimitiveDataType() != PrimitiveDataType.STRUCTURE)  return false;

        // 構造体定義
        jp.riken.kscope.language.fortran.Type type = ((VariableType)def.getVariableType()).getType();
        type.setStartCodeLine(lineInfo);

        // 構造体定義を登録する。
        m_database.addTypeDefinition(type);

        return true;
    }

    /**
     * FuseDecl(ONLY指定子なしのUSE宣言文)の開始要素を登録する。
     *
     * @param visitable
     *            FuseDecl(ONLY指定子なしのUSE宣言文)
     * @return 成否
     */
    @Override
    public boolean enter(FuseDecl visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // モジュール名
        String mod_name = visitable.getName();

        // USE文クラス
        UseState use = new UseState();
        use.setModuleName(mod_name);

        // 局所名と参照対象名
        List<Rename> renames = visitable.getRename();
        for (Rename rename : renames) {
            if (StringUtils.isNullOrEmpty(rename.getLocalName())) {
                use.addOnlyMember(rename.getUseName());
            }
            else {
                use.addTranslationName(rename.getLocalName(), rename.getUseName());
            }
        }

        // USE宣言文を登録する。
        m_database.setUse(use, lineInfo, label);

        return true;
    }

    /**
     * FuseOnlyDecl(ONLY指定子のUSE宣言文)の開始要素を登録する。
     *
     * @param visitable
     *            FuseOnlyDecl(ONLY指定子のUSE宣言文)
     * @return 成否
     */
    @Override
    public boolean enter(FuseOnlyDecl visitable) {
        String label = Statement.NO_LABEL;

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // モジュール名
        String mod_name = visitable.getName();

        // USE文クラス
        UseState use = new UseState();
        use.setModuleName(mod_name);

        // 局所名と参照対象名
        List<Renamable> renames = visitable.getRenamable();
        for (Renamable rename : renames) {
            if (StringUtils.isNullOrEmpty(rename.getLocalName())) {
                use.addOnlyMember(rename.getUseName());
            }
            else {
                use.addTranslationName(rename.getLocalName(), rename.getUseName());
            }
        }

        // USE宣言文を登録する。
        m_database.setUse(use, lineInfo, label);

        return true;
    }

    /**
     * FpointerAssignStatement(ポインタ代入文)の開始要素を登録する。
     *
     * @param visitable        FpointerAssignStatement(ポインタ代入文)
     * @return 成否
     */
    @Override
    public boolean enter(FpointerAssignStatement visitable) {
        String label = Statement.NO_LABEL;

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        List<IXmlNode> content = visitable.getContent();
        // 左辺
        IXmlNode leftExpr = (content != null && content.size() >= 1) ? content.get(0) : null;
        // 右辺
        IXmlNode rightExpr = (content != null && content.size() >= 2) ? content.get(1) : null;

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());

        // 左辺
        // Variable leftVar = varParser.getVariable(leftExpr);
        exprParser.setParseNode(leftExpr);
        Expression leftVar = exprParser.getExpression();

        // 右辺
        exprParser.setParseNode(rightExpr);
        Expression rightVar = exprParser.getExpression();

        m_database.setSubstitution(leftVar, rightVar, lineInfo, label);

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;

    }

    /**
     * FpragmaStatement(OpenMP3.0の指示文)の開始要素を登録する。
     *
     * @param visitable        FpragmaStatement(OpenMP3.0の指示文)
     * @return 成否
     */
    @Override
    public boolean enter(FpragmaStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 指示文
        String value = visitable.getValue();

        // ディレクティブ文を登録する。
        m_database.setDirective(value, lineInfo, label);

        return true;
    }


    /**
     * FexitStatement(EXIT文)の開始要素を登録する。
     *
     * @param visitable   EXIT文
     * @return 成否
     */
    @Override
    public boolean enter(FexitStatement visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // EXIT文
        if (!StringUtils.isNullOrEmpty(visitable.getConstructName())) {
            label = visitable.getConstructName();
        }
        // EXIT文を登録する。
        m_database.setExit(lineInfo, label);

        return true;
    }

    /**
     * FcycleStatement(CYCLE文)の開始要素を登録する。
     *
     * @param visitable   CYCLE文
     * @return 成否
     */
    @Override
    public boolean enter(FcycleStatement visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // CYCLE文
        if (!StringUtils.isNullOrEmpty(visitable.getConstructName())) {
            label = visitable.getConstructName();
        }
        // CYCLE文を登録する。
        m_database.setCycle(lineInfo, label);

        return true;
    }

    /**
     * FstopStatement(STOP文)の開始要素を登録する。
     *
     * @param visitable   STOP文
     * @return 成否
     */
    @Override
    public boolean enter(FstopStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // STOP文
        String arg = null;
        String code = visitable.getCode();
        String msg = visitable.getMessage();
        if (!StringUtils.isNullOrEmpty(code)) {
            arg = code;
        }
        else if (!StringUtils.isNullOrEmpty(msg)) {
            arg = msg;
        }

        // STOP文を登録する。
        m_database.setStop(arg, lineInfo, label);

        return true;
    }

    /**
     * FpauseStatement(PAUSE文)の開始要素を登録する。
     *
     * @param visitable   PAUSE文
     * @return 成否
     */
    @Override
    public boolean enter(FpauseStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // PAUSE文
        String arg = null;
        String code = visitable.getCode();
        String msg = visitable.getMessage();
        if (!StringUtils.isNullOrEmpty(code)) {
            arg = code;
        }
        else if (!StringUtils.isNullOrEmpty(msg)) {
            arg = msg;
        }

        // PAUSE文を登録する。
        m_database.setPause(arg, lineInfo, label);

        return true;
    }

    /**
     * GotoStatement(GOTO文)の開始要素を登録する。
     *
     * @param visitable   GOTO文
     * @return 成否
     */
    @Override
    public boolean enter(GotoStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // GOTO文
        String arg = visitable.getLabelName();
        if (StringUtils.isNullOrEmpty(arg)) {
            GotoStatementSequence seq = new GotoStatementSequence(visitable.getParams(), visitable.getValue());
            return enter(seq);
        }

        // GOTO文を登録する。
        // 行番号の場合は、数値文字にする(先頭０削除）
        if (StringUtils.isNumeric(arg)) {
            arg = Integer.valueOf(arg).toString();
        }
        m_database.setGoTo(arg, lineInfo, label);

        return true;
    }

    /**
     * GotoStatementSequence(GOTO文)の開始要素を登録する。
     *
     * @param visitable   GOTO文
     * @return 成否
     */
    @Override
    public boolean enter(GotoStatementSequence visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // GOTO文
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

        // GOTO文を登録する。
        m_database.setGoTo(arg.toString(), lineInfo, label);

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FnullifyStatement(NULLIFY文)の開始要素を登録する。
     *
     * @param visitable   NULLIFY文
     * @return 成否
     */
    @Override
    public boolean enter(FnullifyStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());

        List<Alloc> list = visitable.getAlloc();
        List<Variable> varlist = new ArrayList<Variable>();
        if (list != null) {
            for (Alloc  nodeAlloc : list) {
                IXmlNode node = XmlNodeUtil.getXmlNodeChoice(nodeAlloc);

                // 変数
                Variable var = varParser.getVariable(node);
                // 変数を追加する
                if (var != null) {
                    varlist.add(var);
                }
            }
        }

        // NULLIFY文を登録する。
        m_database.setNullify(varlist, lineInfo, label);

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;

    }

    /**
     * FallocateStatement(ALLOCATE文)の開始要素を登録する。
     *
     * @param visitable   ALLOCATE文
     * @return 成否
     */
    @Override
    public boolean enter(FallocateStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        List<Alloc> list = visitable.getAlloc();
        String stat = visitable.getStatName();

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());

        // VariableDimensionパーサ
        VariableDimensionParser dimsParser = new VariableDimensionParser(m_context.getTypeManager());

        Map<Variable, VariableDimension> targets = new LinkedHashMap<Variable, VariableDimension>();
        if (list != null) {
            for (Alloc  nodeAlloc : list) {
                Variable var = null;
                VariableDimension dims = null;

                IXmlNode node = XmlNodeUtil.getXmlNodeChoice(nodeAlloc);
                var = varParser.getVariable(node);

                // 配列宣言
                List<IXmlNode> arrayElems = nodeAlloc.getDefModelArraySubscript();
                if (arrayElems != null && arrayElems.size() > 0) {
                    dims = dimsParser.parseVariableDimension(arrayElems);
                }

                if (var != null) {
                    targets.put(var,  dims);
                }
            }
        }

        // ALLOCATE文を登録する。
        Variable err = null;
        if (stat != null) {
            err = new Variable(stat);
        }
        m_database.setAllocate(targets, err, lineInfo, label);
        return true;

    }

    /**
     * FdeallocateStatement(DEALLOCATE文)の開始要素を登録する。
     *
     * @param visitable   DEALLOCATE文
     * @return 成否
     */
    @Override
    public boolean enter(FdeallocateStatement visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        List<Alloc> list = visitable.getAlloc();
        String stat = visitable.getStatName();

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());
        List<Variable> targets = new ArrayList<Variable>();
        if (list != null) {
            for (Alloc  nodeAlloc : list) {
                IXmlNode node = XmlNodeUtil.getXmlNodeChoice(nodeAlloc);
                Variable var = varParser.getVariable(node);
                if (var != null) {
                    targets.add(var);
                }
            }
        }

        // ALLOCATE文を登録する。
        Variable err = null;
        if (stat != null) {
            err = new Variable(stat);
        }
        m_database.setDeallocate(targets, err, lineInfo, label);
        return true;

    }


    /**
     * FdataDecl(DATA文)の開始要素を登録する。
     *
     * @param visitable   DATA文
     * @return 成否
     */
    @Override
    public boolean enter(FdataDecl visitable) {

        List<IXmlNode> list = visitable.getVarListAndValueList();
        if (list == null) {
            return false;
        }

        // ((varList, valueList)+)
        for (int i=0; i<list.size(); i=i+2) {
            if (i+1 >= list.size()) {
                break;
            }
            if (!(list.get(i) instanceof VarList)) {
                continue;
            }
            if (!(list.get(i+1) instanceof ValueList)) {
                continue;
            }
            FdataDeclSequence dataSeq = new FdataDeclSequence(
                                                (VarList)list.get(i),
                                                (ValueList)list.get(i+1));
            boolean result = enter(dataSeq);
            if (!result) return false;
        }

        return true;
    }

    /**
     * FdataDeclSequence(DATA文)の開始要素を登録する。
     *
     * @param visitable   DATA文
     * @return 成否
     */
    @Override
    public boolean enter(FdataDeclSequence visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // 変数のパース
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

                // Data文を登録する。
                m_database.setData(block, lineInfo, label);
            }

        } catch (XcodeMLException e) {
            e.printStackTrace();
            return false;
        }

        // 外部手続きの登録
        addExternalFunction(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FequivalenceDecl(EQUIVALENCE文)の開始要素を登録する。
     *
     * @param visitable   EQUIVALENCE文
     * @return 成否
     */
    @Override
    public boolean enter(FequivalenceDecl visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());

        List<IXmlNode> list = visitable.getVarRefAndVarList();
        List<Variable> variables = new ArrayList<Variable>();
        for (int i=0; i<list.size(); i++) {
            if (list.get(i) instanceof VarRef) {
                // VarRef
                Variable var = varParser.getVariable(list.get(i));
                variables.add(var);
            }
            else if (list.get(i) instanceof VarList) {
                // VarList
                List<IXmlNode> varRefOrFdoLoop = ((VarList)list.get(i)).getVarRefOrFdoLoop();
                if (varRefOrFdoLoop != null) {
                    for (IXmlNode varRef : varRefOrFdoLoop) {
                        Variable var = varParser.getVariable(varRef);
                        variables.add(var);
                    }
                }
            }
        }

        // EQUIVALENCE文を登録する。
        Equivalence block = new Equivalence();
        block.setVariables(variables);
        m_database.setEquivalence(block, lineInfo, label);

        return true;
    }

    /**
     * FcommonDecl(COMMON文)の開始要素を登録する。
     *
     * @param visitable   COMMON文
     * @return 成否
     */
    @Override
    public boolean enter(FcommonDecl visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());

        List<VarList> list = visitable.getVarList();
        for (int i=0; i<list.size(); i++) {
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
                // COMMON文を登録する。
                Common block = new Common();
                block.setName(name);
                block.setVariables(variables);
                m_database.setCommon(block, lineInfo, label);
            }
        }

        return true;
    }

    /**
     * FinterfaceDecl(INTERFACE文)の開始要素を登録する。
     *
     * @param visitable   INTERFACE文
     * @return 成否
     */
    @Override
    public boolean enter(FinterfaceDecl visitable) {

        // コード出力クラス
        XcodeMLTypeManager typeManager = m_context.getTypeManager();

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // インターフェイス名
        String infName = visitable.getName();
        List<IXmlNode> list = visitable.getFfunctionDeclOrFmoduleProcedureDecl();

        VariableTypeParser typePaser = new VariableTypeParser(this.m_context.getTypeManager());
        VariableDefinitionParser defPaser = new VariableDefinitionParser(this.m_context.getTypeManager());

        // インターフェイス名の生成
        if (XmlNodeUtil.isBoolean(visitable.isIsOperator())) {
            infName = "OPERATOR(" + infName + ")";
        }
        else if (XmlNodeUtil.isBoolean(visitable.isIsAssignment())) {
            infName = "ASSIGNMENT(" + infName + ")";
        }
        // INTERFACE文クラス生成
        Procedures interProc = new Procedures(infName);
        for (IXmlNode node : list) {
            if (node instanceof FfunctionDecl) {
                Name itemName = ((FfunctionDecl)node).getName();;

                IXmlTypeTableChoice typeChoice = typeManager.findType(itemName);
                FfunctionType functionTypeElem = (FfunctionType) typeChoice;
                String returnTypeName = functionTypeElem.getReturnType();
                //EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
                //boolean is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
                //boolean is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
                //boolean is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());
                // result
                String result = functionTypeElem.getResultName();

                // 総称定義関数データ型
                VariableType funcType = typePaser.parseVariableType(((FfunctionDecl) node).getName());

                // 仮引数
                Params params = functionTypeElem.getParams();
                jp.riken.kscope.language.generic.Arguments args = new jp.riken.kscope.language.generic.Arguments();
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
            }
            else if (node instanceof FmoduleProcedureDecl) {
                List<Name> names = ((FmoduleProcedureDecl)node).getName();
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
     * 外部手続きリストに追加する
     * @param funcName        外部手続き名
     * @param varType        データ型
     */
    private void addExternalFunction(String funcName, IVariableType varType) {
        if (funcName == null || varType == null) return;

        // 外部手続きリストに追加する
        m_database.addExternalFunction(funcName, varType);
    }

    /**
     * 外部手続きリストに追加する
     * @param list        外部手続きリスト
     */
    private void addExternalFunction(Map<String, IVariableType> list) {
        if (list == null || list.size() <= 0) return;

        // 外部手続きリストに追加する
        for (Iterator<Entry<String, IVariableType>> itr = list.entrySet().iterator(); itr.hasNext();) {
            Map.Entry<String, IVariableType> entry = itr.next();
            String funcName = entry.getKey();
            IVariableType varType = entry.getValue();
            m_database.addExternalFunction(funcName, varType);
        }
    }

    @Override
    public void leave(FstructDecl visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 変数名
        Name nameStruct = visitable.getName();
        String name = nameStruct.getValue();
        if (this.m_database.get_current_unit() == null) return;
        jp.riken.kscope.language.fortran.Type type = this.m_database.get_current_unit().getType(name);
        if (type == null) return;
        type.setEndCodeLine(lineInfo);

    }

    /**
     * エラー情報を登録する.
     * @param line        コード行情報
     * @param message    エラーメッセージ
     */
    private void addErrorInfo(CodeLine line, String message) {
        if (this.listErrorInfo == null) {
            this.listErrorInfo = new ArrayList<ErrorInfo>();
        }
        this.listErrorInfo.add(new ErrorInfo(line, message));
    }

    /**
     * エラーリストを取得する.
     * @return    エラーリスト
     */
    public ErrorInfo[] getListErrorInfo() {
        if (this.listErrorInfo == null || this.listErrorInfo.size() <= 0) {
            return null;
        }
        return this.listErrorInfo.toArray(new ErrorInfo[0]);
    }

    /**
     * プロシージャの重複エラーメッセージを取得する.
     * @param overridename        重複プロシージャ名
     * @param lineInfo        エラーコード情報
     * @param duplicateUnit        重複ブロック
     */
    private void addDuplicateError(String overridename, CodeLine lineInfo, ProgramUnit duplicateUnit) {
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
        // dbupdate.error.duplicate.module=[警告] MODULE[%s]が重複しています。override=%s.
        if (duplicateUnit instanceof Module) {
            key = "dbupdate.error.duplicate.module";
        }
        else if (duplicateUnit instanceof Procedure) {
            // dbupdate.error.duplicate.program=[警告] PROGRAM[%s, %s]が重複しています。override=%s.
            if (((Procedure) duplicateUnit).isProgram()) {
                key = "dbupdate.error.duplicate.program";
            }
            // dbupdate.error.duplicate.subroutine=[警告] SUBROUTINE[%s]が重複しています。override=%s.
            else if (((Procedure) duplicateUnit).isSubroutine()) {
                key = "dbupdate.error.duplicate.subroutine";
            }
            // dbupdate.error.duplicate.function=[警告] FUNCTION[%s]が重複しています。override=%s.
            else if (((Procedure) duplicateUnit).isFunction()) {
                key = "dbupdate.error.duplicate.function";
            }
        }
        if (key == null) return;
        String msg = Message.getString(key, errorname, dupInfo);
        addErrorInfo(lineInfo, msg);
    }

    /**
     * コード行情報のエラー表示文字列を作成する.
     * @param block        エラーブロック
     * @return            エラー表示文字列
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

    /**
     * body要素の開始要素を登録する。
     * @param visitable         Body要素
     * @return 成否
     */
    @Override
    public boolean enter(Body visitable) {
        // body開始
        m_database.startBody();
        return true;
    }


}


