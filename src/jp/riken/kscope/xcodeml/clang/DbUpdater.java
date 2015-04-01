/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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
package jp.riken.kscope.xcodeml.clang;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.CompoundBlock;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.clang.XcodeMLVisitorImpl;
import jp.riken.kscope.xcodeml.clang.XcodeMLContext;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgDivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgLshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgMulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgPlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgRshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BinaryExpression;
import jp.riken.kscope.xcodeml.clang.xml.gen.BreakStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.BuiltinOp;
import jp.riken.kscope.xcodeml.clang.xml.gen.CaseLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.ContinueStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.DefaultLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.DoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.ForStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Function;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionDefinition;
import jp.riken.kscope.xcodeml.clang.xml.gen.GotoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
import jp.riken.kscope.xcodeml.clang.xml.gen.IfStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.Params;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Pragma;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ReturnStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.StatementLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.SwitchStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnaryExpression;
import jp.riken.kscope.xcodeml.clang.xml.gen.WhileStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.XcodeProgram;
import jp.riken.kscope.xcodeml.clang.parser.ExpressionParser;
import jp.riken.kscope.xcodeml.clang.parser.VariableDefinitionParser;
import jp.riken.kscope.xcodeml.clang.parser.VariableParser;
import jp.riken.kscope.xcodeml.clang.parser.VariableTypeParser;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarDecl;
import jp.riken.kscope.xcodeml.clang.xml.gen.Else;
import jp.riken.kscope.xcodeml.clang.xml.gen.DoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.clang.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.clang.xml.gen.Then;

/**
 * フォートランデータベースにパース結果を格納する
 * @author RIKEN
 * @version    2015/04/01     エラーメッセージにファイル名追加 : addDuplicateError
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
        this.listErrorInfo = new ArrayList<ErrorInfo>();
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
     * エラー情報を登録する.
     * @param line		コード行情報
     * @param message	エラーメッセージ
     */
    private void addErrorInfo(CodeLine line, String message) {
        if (this.listErrorInfo == null) {
            this.listErrorInfo = new ArrayList<ErrorInfo>();
        }
        this.listErrorInfo.add(new ErrorInfo(line, message));
    }

    /**
     * プロシージャの重複エラーメッセージを取得する.
     * @param overridename		重複プロシージャ名
     * @param lineInfo		エラーコード情報
     * @param duplicateUnit		重複ブロック
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
        String filename = null;
        if (this.m_context.getSourceXmlFile() != null) {
        	filename = this.m_context.getSourceXmlFile().getPath();
        }
        if (filename != null) {
        	dupInfo += " filename=" + filename;
        }
        
        String key = null;
        // dbupdate.error.duplicate.sourcefile=[警告] ソースファイル[%s]が重複しています。override=%s.
        if (duplicateUnit instanceof Module) {
            key = "dbupdate.error.duplicate.sourcefile";
        }
        else if (duplicateUnit instanceof Procedure) {
            // dbupdate.error.duplicate.procedure=[警告] 関数[%s]が重複しています。override=%s.
            key = "dbupdate.error.duplicate.procedure";
        }
        if (key == null) return;
        String msg = Message.getString(key, errorname, dupInfo);
        addErrorInfo(lineInfo, msg);
    }

    /**
     * コード行情報のエラー表示文字列を作成する.
     * @param block		エラーブロック
     * @return			エラー表示文字列
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
     * Fortranクラスのcurrent_blockがCompoundBlockであるかチェックする。
     *
     * @return true=current_blockはCompoundBlockである。
     */
    private boolean isCurrentCompoundBlock() {
        Block block = m_database.getCurrentBlock();
        if (block instanceof CompoundBlock) {
            return true;
        }

        return false;
    }

    /**
     * XcodeProgram(プログラムルート)の開始要素を登録する。
     * @param visitable          XcodeProgram(プログラムルート)
     * @return 成否
     */
    @Override
    public boolean enter(XcodeProgram visitable) {

        // ソースファイル名
        String source_file = visitable.getSource();
        String language = visitable.getLanguage();
        FILE_TYPE filetype = FILE_TYPE.UNKNOWN;
        if ("Fortran".equalsIgnoreCase(language)) {
            // Fortran
            filetype = FILE_TYPE.FORTRANLANG;
        }
        else if ("C".equalsIgnoreCase(language)) {
            // C言語
            filetype = FILE_TYPE.CLANG;
        }

        SourceFile file = new SourceFile(source_file);
        file.setFileType(filetype);
        CodeLine lineInfo = new CodeLine(file, source_file);

        // 既存のモジュールが存在するかチェックする.
        Module module = m_database.module(source_file);
        if (module != null && module.get_start() != null && module.get_start().getLineInfo() != null) {
            addDuplicateError(source_file, lineInfo, module);
        }

        // ソースファイル名をモジュール名として登録する
        m_database.init_module(source_file);
        m_database.get_current_unit().set_start(lineInfo);

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
        VariableDefinitionParser varParser = new VariableDefinitionParser(this.m_context.getTypeManager(), null);
        VariableDefinition varDef = varParser.parseVariableDefinition(visitable);
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
                varDef.setInitValue(init_value.getLine()); // 初期値
            }
            // 外部手続きの登録
            addExternalFunctions(exprParser.getExternalFunction());
        }

        // 変数宣言文を登録する。
        m_database.set_variable_def(varDef);

        return true;
    }


    /**
     * 外部手続きリストに追加する
     * @param list		外部手続きリスト
     */
    private void addExternalFunctions(Map<String, IVariableType> list) {
        if (list == null || list.size() <= 0) return;

        // 外部手続きリストに追加する
        for (Iterator<Entry<String, IVariableType>> itr = list.entrySet().iterator(); itr.hasNext();) {
            Map.Entry<String, IVariableType> entry = itr.next();
            String funcName = entry.getKey();
            IVariableType varType = entry.getValue();
            this.addExternalFunction(funcName, varType);
        }
    }

    /**
     * 外部手続きリストに追加する
     * @param funcName		外部手続き関数名
     * @param varType		外部手続き関数データ型
     */
    private void addExternalFunction(String funcName, IVariableType varType) {
        if (funcName == null ) return;
        if (varType == null ) return;
        // 外部手続きリストに追加する
        m_database.addExternalFunction(funcName, varType);
    }

    /**
     * FunctionDefinition要素（関数定義）の開始を登録する。
     *
     * @param visitable         FunctionDefinition要素
     * @return boolean 成否
     */
    @Override
    public boolean enter(FunctionDefinition visitable) {
        if (visitable == null) return false;
        if (visitable.getName() == null) return false;

        // コード出力クラス
        XcodeMLTypeManager typeManager = m_context.getTypeManager();
        VariableTypeParser typePaser = new VariableTypeParser(typeManager, null);

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // main,関数文の情報
        // 関数名
        String func_name = visitable.getName().getValue();

        // データ型
        Id func_id = typeManager.findSymbol(func_name);
        VariableType funcType = typePaser.parseVariableType(func_id);

        // 仮引数

        // 変数宣言文
        VariableDefinitionParser varParser = new VariableDefinitionParser(this.m_context.getTypeManager(), null);
        Params params = visitable.getParams();
        ArrayList<Variable> args = new ArrayList<Variable>();
        if (params != null) {
            for (Name nameElem : params.getName()) {
                String name = nameElem.getValue();
                String type = nameElem.getType();

                VariableDefinition varDef = varParser.parseVariableDefinition(nameElem);
                Variable var = new Variable(name);
                var.setDefinition(varDef);
                args.add(var);
            }
        }

        // データベース登録
        // main, 関数
        // 既存の関数が存在するかチェックする.
        Procedure errorProc = m_database.getProcedure(m_database.getCurrentUnit(), func_name);
        if (errorProc != null) {
            addDuplicateError(func_name, lineInfo, errorProc);
        }
        // FUNCTION
        if (args.size() <= 0) {
            m_database.init_function(func_name);
        } else {
            m_database.init_function(func_name, args.toArray(new Variable[0]));
        }
        if ("main".equals(func_name)) {
            // mainプログラム
            m_database.setMainName(func_name);
        }

        m_database.get_current_unit().set_start(lineInfo);

        // 関数データ型
        m_database.setReturnValueType(funcType);

        return true;
    }

    /**
     * FfunctionDefinition要素（MAIN PROGRAM・関数・サブルーチン定義）の終了を登録する。
     *
     * @param visitable
     *            FfunctionDefinition要素
     */
    @Override
    public void leave(FunctionDefinition visitable) {
        if (visitable == null) return;
        if (visitable.getName() == null) return;

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // main,関数文の情報
        // 関数名
        String func_name = visitable.getName().getValue();

        // データベース登録
        m_database.get_current_unit().set_end(lineInfo);
        // FUNCTION
        m_database.end_function();

        return;
    }

    /**
     * DoStatement(DO文)の開始要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     * @return 成否
     */
    @Override
    public boolean enter(DoStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

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
        m_database.start_loop(lineInfo, Statement.NO_LABEL, null, null, conditionExp, null);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * DoStatement(DO文)の終了要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     */
    @Override
    public void leave(DoStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // DO文のブロック終了
        m_database.end_loop(lineInfo);
    }


    /**
     * ForStatement(FOR文)の開始要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     * @return 成否
     */
    @Override
    public boolean enter(ForStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // ループ式をパースする。
        // 範囲の初期値を取得する
        Expression initExp = null;
        if (visitable.getInit() != null) {
            exprParser.setParseNode(visitable.getInit());
            // 範囲の初期値を取得する
            initExp = exprParser.getExpression();
        }

        // 範囲の条件式を取得する
        Expression condExp = null;
        if (visitable.getCondition() != null) {
            exprParser.setParseNode(visitable.getCondition());
            // 範囲の条件式を取得する
            condExp = exprParser.getExpression();
        }

        // 範囲のステップを取得する
        Expression stepExp = null;
        if (visitable.getIter() != null) {
            exprParser.setParseNode(visitable.getIter());
            // 範囲のステップを取得する
            stepExp = exprParser.getExpression();
        }

        // DO文のブロック開始
        m_database.start_loop(lineInfo, Statement.NO_LABEL, null, initExp, condExp, stepExp);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FdoStatement(DO文)の終了要素を登録する。
     *
     * @param visitable
     *            FdoStatement(DO文)要素
     */
    @Override
    public void leave(ForStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // DO文のブロック終了
        m_database.end_loop(lineInfo);
    }

    /**
     * IfStatement(IF文)の開始要素を登録する。
     *
     * @param visitable           IfStatement(IF文)要素
     * @return 成否
     */
    @Override
    public boolean enter(IfStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable.getCondition());

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        m_database.startSelection(lineInfo, null,
                    jp.riken.kscope.language.Selection.SelectionType.IF);

        // 条件式
        m_database.startCondition(condExpr, lineInfo, null);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * FifStatement(IF文)の終了要素を登録する。
     *
     * @param visitable
     *            FifStatement(IF文)要素
     */
    @Override
    public void leave(IfStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ElseIF文デあるかチェックする。
        boolean isElseIf = false;

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

        m_database.startCondition(null, lineInfo, Statement.NO_LABEL);

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

        m_database.endCondition(lineInfo, Statement.NO_LABEL);

        return;
    }

    /**
     * FunctionCall要素(関数・サブルーチン呼び出し)を登録する。
     *
     * @param visitable
     *            FunctionCall
     * @return 成否
     */
    @Override
    public boolean enter(FunctionCall visitable) {

        String label = Statement.NO_LABEL;

        // データベースがfunctionCallを登録可能であるか(現在ブロックがProgramUnitであるか?)
        if (!isCurrentProgramUnit()) {
            return true;
        }

        // データ型パーサ
        VariableTypeParser typePaser = new VariableTypeParser(this.m_context.getTypeManager(), null);

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
        int lineno = lineInfo.getStartLine();

        // set_callの引数
        List<Expression> list = null;
        // サブルーチン名、関数名
        String functionName = null;
        Function function = visitable.getFunction();
        if (function.getFuncAddr() != null) {
            functionName = function.getFuncAddr().getValue();
        }
        else {
            IXmlNode node = XmlNodeUtil.getXmlNodeChoice(function);
            // ExpressionParserモデルパーサ
            ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());
            // 変数のパース
            VariableParser varParser = new VariableParser(m_context.getTypeManager());
            Variable nodeVar = varParser.getVariable(node);
            // 式パーサー
            exprParser.setParseNode(node);
            Expression nodeExpr = exprParser.getExpression();
            functionName = nodeExpr.getLine();
        }
        if (functionName == null) return false;

        // 属性
        VariableType funcVarType = null;
        boolean is_inline = false;
        boolean is_static = false;
        boolean is_external = false;
        boolean is_volatile = false;
        boolean is_const = false;
        boolean is_restrict = false;
        boolean is_intrinsic = false;
        if (function.getFuncAddr() != null && function.getFuncAddr().getType() != null) {
            String func_type = function.getFuncAddr().getType();
            is_inline = this.m_context.getTypeManager().isInline(func_type);
            is_static = this.m_context.getTypeManager().isStatic(func_type);
            is_external = this.m_context.getTypeManager().isSclassExtern(func_type);
            is_volatile = this.m_context.getTypeManager().isVolatile(func_type);
            is_const = this.m_context.getTypeManager().isConst(func_type);
            is_restrict = this.m_context.getTypeManager().IsRestrict(func_type);


            IXmlTypeTableChoice typeChoice = this.m_context.getTypeManager().findType(func_type, FunctionType.class);
            if (typeChoice != null && typeChoice instanceof FunctionType) {
                FunctionType functionTypeElem = (FunctionType) typeChoice;
                String returnTypeName = functionTypeElem.getReturnType();
                funcVarType = typePaser.parseVarFunctionType(functionTypeElem);
            }
        }

        // 引数リスト
        try {
            Arguments arguments = visitable.getArguments();
            ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());
            Expression[] argsExpr = exprParser.getExpressions(arguments.getExpressions());
            if (argsExpr != null) {
                list = new ArrayList<Expression>(java.util.Arrays.asList(argsExpr));
            }

            // 外部手続きの登録
            addExternalFunctions(exprParser.getExternalFunction());

        } catch (XcodeMLException ex) {
            ex.printStackTrace();
        }
        // CALL文、関数呼出登録
        m_database.setCall(lineInfo, label, functionName, list, is_intrinsic);
        if (is_external) {
            // 外部手続きの登録
            addExternalFunction(functionName, funcVarType);
        }

        return true;
    }


    /**
     * BuiltinOp要素(組込関数)を登録する。
     *
     * @param visitable
     *            BuiltinOp
     * @return 成否
     */
    @Override
    public boolean enter(BuiltinOp visitable) {

        String label = Statement.NO_LABEL;

        // データベースがBuiltinOpを登録可能であるか(現在ブロックがProgramUnitであるか?)
        if (!isCurrentProgramUnit()) {
            return true;
        }

        // データ型パーサ
        VariableTypeParser typePaser = new VariableTypeParser(this.m_context.getTypeManager(), null);

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
        int lineno = lineInfo.getStartLine();

        // set_callの引数
        List<Expression> list = null;
        // 組込関数名
        String functionName = visitable.getName();
        String func_type = visitable.getType();

        // 属性
        VariableType funcVarType = null;
        boolean is_inline = false;
        boolean is_static = false;
        boolean is_external = false;
        boolean is_volatile = false;
        boolean is_const = false;
        boolean is_restrict = false;
        boolean is_intrinsic = true;		// 組込関数であるのでtrueとする.
        if (func_type != null) {
            is_inline = this.m_context.getTypeManager().isInline(func_type);
            is_static = this.m_context.getTypeManager().isStatic(func_type);
            is_external = this.m_context.getTypeManager().isSclassExtern(func_type);
            is_volatile = this.m_context.getTypeManager().isVolatile(func_type);
            is_const = this.m_context.getTypeManager().isConst(func_type);
            is_restrict = this.m_context.getTypeManager().IsRestrict(func_type);

            IXmlTypeTableChoice typeChoice = this.m_context.getTypeManager().findType(func_type, FunctionType.class);
            if (typeChoice != null && typeChoice instanceof FunctionType) {
                FunctionType functionTypeElem = (FunctionType) typeChoice;
                String returnTypeName = functionTypeElem.getReturnType();
                funcVarType = typePaser.parseVarFunctionType(functionTypeElem);
            }
        }

        // 引数リスト
        try {
            List<IXmlNode> arguments = visitable.getIntConstantOrFloatConstantOrLonglongConstant();
            ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());
            Expression[] argsExpr = exprParser.getExpressions(arguments);
            if (argsExpr != null) {
                list = new ArrayList<Expression>(java.util.Arrays.asList(argsExpr));
            }

            // 外部手続きの登録
            addExternalFunctions(exprParser.getExternalFunction());

        } catch (XcodeMLException ex) {
            ex.printStackTrace();
        }
        // CALL文、関数呼出登録
        m_database.setCall(lineInfo, label, functionName, list, is_intrinsic);
        if (is_external) {
            // 外部手続きの登録
            addExternalFunction(functionName, funcVarType);
        }

        return true;
    }

    /**
     * AssignExpr要素(代入文)を登録する。
     *
     * @param visitable       AssignExpr
     * @return 成否
     */
    @Override
    public boolean enter(AssignExpr visitable) {
        return this.enterBinaryExpression(visitable);
    }

    /**
     * PostIncrExpr要素(後置インクリメント文)を登録する。
     *
     * @param visitable       PostIncrExpr
     * @return 成否
     */
    @Override
    public boolean enter(PostIncrExpr visitable) {
        return enterUnaryExpression(visitable);
    }

    /**
     * PostIncrExpr要素(後置デクリメント文)を登録する。
     *
     * @param visitable       PostDecrExpr
     * @return 成否
     */
    @Override
    public boolean enter(PostDecrExpr visitable) {
        return enterUnaryExpression(visitable);
    }

    /**
     * PreIncrExpr要素(前置インクリメント文)を登録する。
     *
     * @param visitable       PostIncrExpr
     * @return 成否
     */
    @Override
    public boolean enter(PreIncrExpr visitable) {
        return enterUnaryExpression(visitable);
    }

    /**
     * PreDecrExpr要素(前置デクリメント文)を登録する。
     *
     * @param visitable       PostDecrExpr
     * @return 成否
     */
    @Override
    public boolean enter(PreDecrExpr visitable) {
        return enterUnaryExpression(visitable);
    }


    /**
     * UnaryExpression要素(単項式文)を登録する。
     *
     * @param visitable       UnaryExpression
     * @return 成否
     */
    private boolean enterUnaryExpression(UnaryExpression visitable) {
        String label = Statement.NO_LABEL;

        // データベースがfunctionCallを登録可能であるか(現在ブロックがProgramUnitであるか?)
        if (!isCurrentProgramUnit()) {
            return true;
        }

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
        int lineno = lineInfo.getStartLine();

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // 変数のパース
        VariableParser varParser = new VariableParser(m_context.getTypeManager());
        Variable nodeVar = varParser.getVariable(node);

        // 式パーサー
        exprParser.setParseNode(node);
        Expression nodeExpr = exprParser.getExpression();

        m_database.setSubstitution(nodeVar, nodeExpr, lineInfo, label);

        return true;
    }

    /**
     * SwitchStatement(switch文)の開始要素を登録する。
     *
     * @param visitable          SwitchStatement(switch文)要素
     * @return 成否
     */
    @Override
    public boolean enter(SwitchStatement visitable) {

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable.getValue());

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        m_database.startSelection(lineInfo, null,
                    jp.riken.kscope.language.Selection.SelectionType.SELECT);

        // 条件式
        m_database.setSelectCaseCondition(condExpr);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * SwitchStatement(switch文)の終了要素を登録する。
     *
     * @param visitable          SwitchStatement(switch文)要素
     * @return 成否
     */
    @Override
    public void leave(SwitchStatement visitable) {

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        if (m_database.get_current_block() instanceof Condition) {
            m_database.endCondition(null, Statement.NO_LABEL);
        }

        // SELECT文のブロック終了
        m_database.end_selection(lineInfo);

        return;
    }

    /**
     * CaseLabel(case文)の開始要素を登録する。
     *
     * @param visitable          CaseLabel(case文)要素
     * @return 成否
     */
    @Override
    public boolean enter(CaseLabel visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        if (m_database.get_current_block() instanceof Condition) {
            m_database.endCondition(null, Statement.NO_LABEL);
        }

        // 条件式をパースする。
        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(
                                                m_context.getTypeManager(),
                                                visitable);

        // 条件式を取得する
        Expression condExpr = exprParser.getExpression();

        m_database.startCondition(condExpr, lineInfo, Statement.NO_LABEL);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * DefaultLabel(default文)の開始要素を登録する。
     *
     * @param visitable          DefaultLabel(default文)要素
     * @return 成否
     */
    @Override
    public boolean enter(DefaultLabel visitable) {

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        if (m_database.get_current_block() instanceof Condition) {
            m_database.endCondition(null, Statement.NO_LABEL);
        }

        m_database.startCondition(null, lineInfo, Statement.NO_LABEL);

        return true;
    }

    /**
     * BreakStatement(break文)の開始要素を登録する。
     *
     * @param visitable          BreakStatement(break文)要素
     * @return 成否
     */
    @Override
    public boolean enter(BreakStatement visitable) {

        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // break文を登録する。
        m_database.setExit(lineInfo, label);

        return true;
    }

    /**
     * ContinueStatement(continue文)の開始要素を登録する。
     *
     * @param visitable          ContinueStatement(continue文)要素
     * @return 成否
     */
    @Override
    public boolean enter(ContinueStatement visitable) {
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // continue文を登録する。
        m_database.setContinue(lineInfo, label);

        return true;
    }

    /**
     * WhileStatement(while文)の開始要素を登録する。
     *
     * @param visitable          WhileStatement(while文)要素
     * @return 成否
     */
    @Override
    public boolean enter(WhileStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

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
        m_database.start_loop(lineInfo, Statement.NO_LABEL, null, null, conditionExp, null);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * WhileStatement(while文)の終了要素を登録する。
     *
     * @param visitable          WhileStatement(while文)要素
     * @return 成否
     */
    @Override
    public void leave(WhileStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // while文のブロック終了
        m_database.end_loop(lineInfo);
    }

    /**
     * CompoundStatement(複文:空文)の開始要素を登録する。
     *
     * @param visitable          CompoundStatement(複文:空文)要素
     * @return 成否
     */
    @Override
    public boolean enter(CompoundStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 1つ上のノードがIBaseStatementであるか？
        IXmlNode parent_node = m_context.getInvokeNode(1);
        if (!(parent_node instanceof CompoundStatement)) {
            if (parent_node instanceof jp.riken.kscope.xcodeml.clang.xml.IBaseStatement) {
                // 1つ上も文であるので登録の必要はない。
                return true;
            }
        }
        parent_node = m_context.getInvokeNode(2);
        if (parent_node instanceof FunctionDefinition) {
            // 2つ上がFunctionDefinition（関数定義）であるので登録の必要はない
            return true;
        }
        if (parent_node instanceof IfStatement) {
            // 2つ上がIfStatement（IF文）であるので登録の必要はない
            return true;
        }


        // 複文のブロック開始
        m_database.startCompoundBlock(lineInfo);

        return true;
    }

    /**
     * CompoundStatement(複文:空文)の終了要素を登録する。
     *
     * @param visitable          CompoundStatement(複文:空文)要素
     * @return 成否
     */
    @Override
    public void leave(CompoundStatement visitable) {

        // データベースのカレントブロックがCompoundBlockであるか
        if (!this.isCurrentCompoundBlock()) return;

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 複文のブロック終了
        m_database.endCompoundBlock(lineInfo);
    }


    /**
     * Decompile "exprStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ExprStatement visitable) {
        // DONE: ExprStatement
        String label = Statement.NO_LABEL;

        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();
        int lineno = lineInfo.getStartLine();

        // 子ノード選択
        boolean result = true;
        if (visitable.getPostDecrExpr() != null) {
            result = this.enter((PostDecrExpr)visitable.getPostDecrExpr());
        }
        else if (visitable.getPostIncrExpr() != null) {
            result = this.enter((PostIncrExpr)visitable.getPostIncrExpr());
        }
        else if (visitable.getPreDecrExpr() != null) {
            result = this.enter((PreDecrExpr)visitable.getPreDecrExpr());
        }
        else if (visitable.getPreIncrExpr() != null) {
            result = this.enter((PreIncrExpr)visitable.getPreIncrExpr());
        }
        else if (visitable.getAssignExpr() != null) {
            result = this.enter((AssignExpr)visitable.getAssignExpr());
        }
        else if (visitable.getFunctionCall() != null) {
            result = this.enter((FunctionCall)visitable.getFunctionCall());
        }
        else if (visitable.getAsgPlusExpr() != null) {
            result = this.enter((AsgPlusExpr)visitable.getAsgPlusExpr());
        }
        else if (visitable.getAsgBitAndExpr() != null) {
            result = this.enter((AsgBitAndExpr)visitable.getAsgBitAndExpr());
        }
        else if (visitable.getAsgBitOrExpr() != null) {
            result = this.enter((AsgBitOrExpr)visitable.getAsgBitOrExpr());
        }
        else if (visitable.getAsgBitXorExpr() != null) {
            result = this.enter((AsgBitXorExpr)visitable.getAsgBitXorExpr());
        }
        else if (visitable.getAsgModExpr() != null) {
            result = this.enter((AsgModExpr)visitable.getAsgModExpr());
        }
        else if (visitable.getAsgMulExpr() != null) {
            result = this.enter((AsgMulExpr)visitable.getAsgMulExpr());
        }
        else if (visitable.getAsgDivExpr() != null) {
            result = this.enter((AsgDivExpr)visitable.getAsgDivExpr());
        }
        else if (visitable.getAsgMinusExpr() != null) {
            result = this.enter((AsgMinusExpr)visitable.getAsgMinusExpr());
        }
        else if (visitable.getAsgRshiftExpr() != null) {
            result = this.enter((AsgRshiftExpr)visitable.getAsgRshiftExpr());
        }
        else if (visitable.getAsgLshiftExpr() != null) {
            result = this.enter((AsgLshiftExpr)visitable.getAsgLshiftExpr());
        }


        return result;
    }

    /**
     * Decompile "AsgPlusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgPlusExpr visitable) {
        // DONE: AsgPlusExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgModExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgModExpr visitable) {
        // DONE: AsgModExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgBitOrExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgBitOrExpr visitable) {
        // DONE: AsgBitOrExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgBitXorExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgBitXorExpr visitable) {
        // DONE: AsgBitXorExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgMulExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgMulExpr visitable) {
        // DONE: AsgMulExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgRshiftExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgRshiftExpr visitable) {
        // DONE: AsgRshiftExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgBitAndExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgBitAndExpr visitable) {
        // DONE: AsgBitAndExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgDivExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgDivExpr visitable) {
        // DONE: AsgDivExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgMinusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgMinusExpr visitable) {
        // DONE: AsgMinusExpr
        return this.enterBinaryExpression(visitable);
    }

    /**
     * Decompile "AsgLshiftExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgLshiftExpr visitable) {
        // DONE: AsgLshiftExpr
        return this.enterBinaryExpression(visitable);
    }



    /**
     * BinaryExpression要素(２項式文)を登録する。
     *
     * @param visitable       BinaryExpression
     * @return 成否
     */
    private boolean enterBinaryExpression(BinaryExpression visitable) {
        String label = Statement.NO_LABEL;

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
        Variable leftVar = varParser.getVariable(leftExpr);
        exprParser.setParseNode(leftExpr);
        Expression left = exprParser.getExpression();

        // 右辺
        exprParser.setParseNode(rightExpr);
        Expression rightVar = exprParser.getExpression();

        m_database.setSubstitution(leftVar, rightVar, lineInfo, label);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * Decompile "ReturnStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ReturnStatement visitable) {
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

        // リターン式をパースする。
        Expression exp = null;
        // リターン式
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (node != null) {
            exprParser.setParseNode(node);
            // 範囲の初期値を取得する
            exp = exprParser.getExpression();
        }

        // RETURN文
        String label = Statement.NO_LABEL;
        m_database.setReturn(lineInfo, exp, label);

        // 外部手続きの登録
        addExternalFunctions(exprParser.getExternalFunction());

        return true;
    }

    /**
     * Decompile "GotoStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(GotoStatement visitable) {
        // DONE: GotoStatement
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        String name = null;
        if (visitable.getName() == null) {
            // ExpressionParserモデルパーサ
            ExpressionParser exprParser = new ExpressionParser(m_context.getTypeManager());

            // goto式をパースする。
            Expression exp = null;
            // goto式
            IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
            if (node != null) {
                exprParser.setParseNode(node);
                exp = exprParser.getExpression();
                name = exp.getLine();
            }
        }
        else {
            name = visitable.getName().getValue();
        }

        // RETURN文
        String label = Statement.NO_LABEL;
        m_database.setGoTo(name, lineInfo, label);

        return true;
    }


    /**
     * Decompile "Pragma" element in XcodeML/C.
     */
    @Override
    public boolean enter(Pragma visitable) {
        // DONE: Pragma
        String label = Statement.NO_LABEL;
        // ソースファイル、ソースコード行
        CodeLine lineInfo = m_context.getCodeBuilder().getLastCodeLine();

        // 指示文
        String value = visitable.getValue();

        // ディレクティブ文を登録する。
        m_database.setDirective(value, lineInfo, label);

        return true;
    }

}

