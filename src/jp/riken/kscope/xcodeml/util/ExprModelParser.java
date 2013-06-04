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
 * exprModel要素(式表現を参照する要素から共通で利用されるモデル)のパースクラス
 *
 */
public class ExprModelParser {
    // 演算子文字列
	/** カンマ */
    private final String EXPR_COMMA = ",";
    /** plusExpr + : 加算 */
    private final String EXPR_PLUS = "+";
    /** minusExpr - : 減算 */
    private final String EXPR_MINUS = "-";
    /** mulExpr * : 乗算 */
    private final String EXPR_MUL = "*";
    /** divExpr / : 除算 */
    private final String EXPR_DIV = "/";
    /** FpowerExpr ** : べき乗 */
    private final String EXPR_POWER = "**";
    /** FconcatExpr // : 文字式の連結 */
    private final String EXPR_CONCAT = "//";
    /** logEQExpr == .EQ. : 等価 */
    private final String EXPR_LOGEQ = "==";
    /** logNEQExpr /= .NE. : 非等価 */
    private final String EXPR_LOGNEQ = "/=";
    /** logGEExpr >= .GE. : 大なり、または同値 */
    private final String EXPR_LOGGE = ">=";
    /** logGTExpr > .GT. : 大なり */
    private final String EXPR_LOGGT = ">";
    /** logLEExpr <= .LE. : 小なり、または等価 */
    private final String EXPR_LOGLE = "<=";
    /** logLTExpr < .LT. : 小なり */
    private final String EXPR_LOGLT = "<";
    /** logAndExpr .AND. : 論理積 */
    private final String EXPR_LOGAND = ".AND.";
    /** logOrExpr .OR. : 論理和 */
    private final String EXPR_LOGOR = ".OR.";
    /** logEQVExpr .EQV. : 論理等価 */
    private final String EXPR_LOGEQV = ".EQV.";
    /** logNEQVExpr .NEQV. : 論理非等価 */
    private final String EXPR_LOGNEQV = ".NEQV.";
    /** unaryMinusExpr - : 符号反転 */
    private final String EXPR_UNARYMINUS = "-";
    /** logNotExpr .NOT. : 論理否定 */
    private final String EXPR_LOGNOT = ".NOT.";
    /** ( */
    private final String EXPR_PARENLEFT = "(";
    /** ) */
    private final String EXPR_PARENRIGHT = ")";
    /** FmemberRef % : 構造体メンバ */
    private final String EXPR_TYPEMEMBER = "%";
    /** [ */
    private final String EXPR_COARRAYLEFT = "[";
    /** ] */
    private final String EXPR_COARRAYRIGHT = "[";
    /** 等号記号 */
    private final String EXPR_EQUAL = "=";
    /** スペース */
    private final String EXPR_SPACE = " ";
    /** FarrayConstructor */
    private final String EXPR_ARRAYLEFT = "(/";
    /** FarrayConstructor */
    private final String EXPR_ARRAYRIGHT = "/)";
    /** : */
    private final String EXPR_ARRAYCOLON = ":";

    // exprModel要素のパースモード
    /** 変数のみパースパースする。整数、実数、文字列等の変数以外は返さない。 */
    public static final int PARSE_VARIABLE = 0x000001;
    /** サブルーチン、関数の引数はパースしない。サブルーチン、関数名のみを返す。 */
    public static final int PARSE_FUNCTION = 0x000010;
    /** 演算子を付加する。 */
    public static final int PARSE_OPERATOR = 0x000100;
    /** 引数はカンマ区切りで引数毎に文字列とする。 */
    public static final int PARSE_ARGUMENTS = 0x001000;
    /** 組込関数はパースしない。 */
    public static final int PARSE_NONEINTRINSIC = 0x010000;
    /** 演算子を付加する。 + 引数はカンマ区切りで引数毎に文字列とする。 */
    public static final int PARSE_DETAIL = PARSE_OPERATOR | PARSE_ARGUMENTS;

    /** exprModelのパースモード */
    private int _mode;

    /** パースexprModel要素 */
    private IXmlNode _parseNode;

    /** typeTable */
    private XcodeMLTypeManager _typeManager;

    /**
     * コンストラクタ
     *
     * @param mode            パースモード
     * @param typeManager     typeTable
     */
    public ExprModelParser(int mode, XcodeMLTypeManager typeManager) {
        this._mode = mode;
        _typeManager = typeManager;
    }

    /**
     * コンストラクタ
     *
     * @param mode
     *            パースモード
     * @param typeManager
     *            typeTable
     * @param node
     *            パースexprModel要素
     */
    public ExprModelParser(int mode, XcodeMLTypeManager typeManager,
            IXmlNode node) {
        this._mode = mode;
        this._parseNode = node;
        _typeManager = typeManager;
    }

    /**
     * @param parseNode
     *            セットする _parseNode
     */
    public void setParseNode(IXmlNode parseNode) {
        _parseNode = parseNode;
    }

    /**
     * パース要素から変数リストを取得する。
     *
     * @return 変数リスト
     * @throws XcodeMLException パースエラー
     */
    public String[] getVariableList() throws XcodeMLException {
        if (_parseNode == null)
            return null;

        return getVariableList(_parseNode, false);
    }

    /**
     * パースノードを文字列にする。
     *
     * @return 式表現文字列
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
            if (vars == null)
                return null;

            StringBuilder expr = new StringBuilder();
            for (String var : vars) {
                expr.append(var);
            }
            if (expr.length() <= 0)
                return null;

            return expr.toString().trim();

        } catch (XcodeMLException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * 変数、演算子リストを文字列にする。
     *
     * @param vars
     *            変数、演算子リスト
     * @return 式表現文字列
     */
    private String toString(String[] vars) {
        StringBuilder expr = new StringBuilder();
        for (String var : vars) {
            expr.append(var);
        }
        if (expr.length() <= 0)
            return null;

        return expr.toString().trim();
    }

    /**
     * Arguments要素から変数リストを取得する。
     *
     * @param args            Arguments要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException   パースエラー
     */
    public String[] getVariableList(Arguments args, boolean grouping) throws XcodeMLException {
        List<IXmlNode> list = args
                .getFintConstantOrFrealConstantOrFcomplexConstant();
        if (list == null || list.size() <= 0)
            return null;

        ArrayList<String> var_list = new ArrayList<String>();
        for (IXmlNode node : list) {
            String[] vars = getVariableList(node, grouping);
            if (vars == null) {
        		// exprmodelparser.variablelist.parse.error=パースエラー:%sをパースできませんでした。
                throw new XcodeMLException(
                		Message.getString("exprmodelparser.variablelist.parse.error", node.getClass().getName()));
            }

            if ((this._mode & PARSE_ARGUMENTS) != 0x00) {
                // 引数はカンマ区切りで引数毎に文字列とする。
                if (var_list.size() > 0)
                    var_list.add(EXPR_COMMA);
                var_list.add(toString(vars));
            } else {
                addOperatorString(var_list, EXPR_COMMA); // カンマ追加
                var_list.addAll(Arrays.asList(vars));
            }
        }
        if (var_list.size() <= 0)
            return null;
        return (String[]) var_list.toArray(new String[var_list.size()]);
    }

    /**
     * 要素リストから変数リストを取得する。
     *
     * @param list            要素リスト
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException   パースエラー
     */
    public String[] getVariableList(List<IXmlNode> list, boolean grouping) throws XcodeMLException {
        if (list == null)
            return null;
        ArrayList<String> var_list = new ArrayList<String>();
        for (IXmlNode node : list) {
            String[] vars = getVariableList(node, grouping);
            addOperatorString(var_list, EXPR_COMMA); // カンマ追加
            var_list.addAll(Arrays.asList(vars));
        }
        if (var_list.size() <= 0)
            return null;
        return (String[]) var_list.toArray(new String[var_list.size()]);
    }

    /**
     * 要素から変数リストを取得する。
     *
     * @param expr            exprModel要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException   パースエラー
     */
    public String[] getVariableList(IXmlNode expr, boolean grouping) throws XcodeMLException {
        String[] vars = null;
        if (expr instanceof FunctionCall)
            vars = getVariableList((FunctionCall) expr, grouping);
        else if (expr instanceof UserBinaryExpr)
            vars = getVariableList((UserBinaryExpr) expr, grouping);
        else if (expr instanceof FcharacterRef)
            vars = getVariableList((FcharacterRef) expr, grouping);
        else if (expr instanceof FmemberRef)
            vars = getVariableList((FmemberRef) expr, grouping);
        else if (expr instanceof FcoArrayRef)
            vars = getVariableList((FcoArrayRef) expr, grouping);
        else if (expr instanceof FdoLoop)
            vars = getVariableList((FdoLoop) expr, grouping);
        else if (expr instanceof FarrayRef)
            vars = getVariableList((FarrayRef) expr, grouping);
        else if (expr instanceof FcomplexConstant)
            vars = getVariableList((FcomplexConstant) expr, grouping);
        else if (expr instanceof UserUnaryExpr)
            vars = getVariableList((UserUnaryExpr) expr, grouping);
        else if (expr instanceof FpowerExpr)
            vars = getVariableList((FpowerExpr) expr, grouping);
        else if (expr instanceof LogLEExpr)
            vars = getVariableList((LogLEExpr) expr, grouping);
        else if (expr instanceof FintConstant)
            vars = getVariableList((FintConstant) expr, grouping);
        else if (expr instanceof FrealConstant)
            vars = getVariableList((FrealConstant) expr, grouping);
        else if (expr instanceof FcharacterConstant)
            vars = getVariableList((FcharacterConstant) expr, grouping);
        else if (expr instanceof FlogicalConstant)
            vars = getVariableList((FlogicalConstant) expr, grouping);
        else if (expr instanceof Var)
            vars = getVariableList((Var) expr, grouping);
        else if (expr instanceof FarrayConstructor)
            vars = getVariableList((FarrayConstructor) expr, grouping);
        else if (expr instanceof FstructConstructor)
            vars = getVariableList((FstructConstructor) expr, grouping);
        else if (expr instanceof UnaryMinusExpr)
            vars = getVariableList((UnaryMinusExpr) expr, grouping);
        else if (expr instanceof LogNotExpr)
            vars = getVariableList((LogNotExpr) expr, grouping);
        else if (expr instanceof VarRef)
            vars = getVariableList((VarRef) expr, grouping);
        else if (expr instanceof PlusExpr)
            vars = getVariableList((PlusExpr) expr, grouping);
        else if (expr instanceof MinusExpr)
            vars = getVariableList((MinusExpr) expr, grouping);
        else if (expr instanceof MulExpr)
            vars = getVariableList((MulExpr) expr, grouping);
        else if (expr instanceof DivExpr)
            vars = getVariableList((DivExpr) expr, grouping);
        else if (expr instanceof FconcatExpr)
            vars = getVariableList((FconcatExpr) expr, grouping);
        else if (expr instanceof LogEQExpr)
            vars = getVariableList((LogEQExpr) expr, grouping);
        else if (expr instanceof LogNEQExpr)
            vars = getVariableList((LogNEQExpr) expr, grouping);
        else if (expr instanceof LogGEExpr)
            vars = getVariableList((LogGEExpr) expr, grouping);
        else if (expr instanceof LogGTExpr)
            vars = getVariableList((LogGTExpr) expr, grouping);
        else if (expr instanceof LogLTExpr)
            vars = getVariableList((LogLTExpr) expr, grouping);
        else if (expr instanceof LogAndExpr)
            vars = getVariableList((LogAndExpr) expr, grouping);
        else if (expr instanceof LogOrExpr)
            vars = getVariableList((LogOrExpr) expr, grouping);
        else if (expr instanceof LogEQVExpr)
            vars = getVariableList((LogEQVExpr) expr, grouping);
        else if (expr instanceof LogNEQVExpr)
            vars = getVariableList((LogNEQVExpr) expr, grouping);
        else if (expr instanceof IndexRange)
            vars = getVariableList((IndexRange) expr, grouping);
        else if (expr instanceof LowerBound)
            vars = getVariableList((LowerBound) expr, grouping);
        else if (expr instanceof UpperBound)
            vars = getVariableList((UpperBound) expr, grouping);
        else if (expr instanceof Step)
            vars = getVariableList((Step) expr, grouping);
        else if (expr instanceof ArrayIndex)
            vars = getVariableList((ArrayIndex) expr, grouping);
        else if (expr instanceof NamedValue)
            vars = getVariableList((NamedValue) expr, grouping);
        else if (expr instanceof Value)
            vars = getVariableList((Value) expr, grouping);
        else if (expr instanceof Condition)
            vars = getVariableList((Condition) expr, grouping);

        return vars;
    }

    /**
     * FunctionCall要素から変数リストを取得する
     *
     * @param node            FunctionCall要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FunctionCall node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();

        Name name = node.getName();
        boolean isIntrinsic = XmlNodeUtil.isBoolean(node.isIsIntrinsic());

        // 組込関数ではない。又は組込関数除去フラグがセットされていない。
        if (!isIntrinsic || ((this._mode & PARSE_NONEINTRINSIC) == 0x00)) {
            list.add(name.getValue());
        }

        // 組込関数の場合は、引数をパースする。
        // PARSE_FUNCTION = サブルーチン、関数の引数はパースしない。サブルーチン、関数名のみを返す。
        if (isIntrinsic || (this._mode & PARSE_FUNCTION) == 0x00) {
            addOperatorString(list, EXPR_PARENLEFT); // 左括弧
            // 引数パース
            String[] args = getVariableList(node.getArguments(), grouping);
            list.addAll(Arrays.asList(args));
            addOperatorString(list, EXPR_PARENRIGHT); // 右括弧
        }

        if (list.size() <= 0)
            return null;

        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * INTERFACE依存要素
     *
     * @param node            INTERFACE依存
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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
        addOperatorString(list, EXPR_SPACE); // スペース

        list.add(name);

        addOperatorString(list, EXPR_SPACE); // スペース

        if (rightExpr != null) {
            String[] vars = getVariableList(rightExpr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FcharacterRef(部分文字列参照)要素から変数リストを取得する
     *
     * @param node            FcharacterRef(部分文字列参照)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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

        // 左括弧
        addOperatorString(list, EXPR_PARENLEFT); // 左括弧 (

        // IndexRange
        IndexRange range = node.getIndexRange();
        if (range != null) {
            String[] vars = getVariableList(range, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }

        // 右括弧
        addOperatorString(list, EXPR_PARENRIGHT); // 右括弧 )

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * 構造体メンバ(FmemberRef)要素から変数リストを取得する
     *
     * @param node            構造体メンバ(FmemberRef)
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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

        // メンバ参照
        addOperatorString(list, EXPR_TYPEMEMBER); // %

        String member = node.getMember();
        if (member != null) {
            list.add(member);
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FcoArrayRef(coarrayの参照)要素から変数リストを取得する
     *
     * @param node            FcoArrayRef(coarrayの参照)
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * ArrayIndex(インデックス値)要素から変数リストを取得する
     *
     * @param node            ArrayIndex(インデックス値)
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(ArrayIndex node, boolean grouping) throws XcodeMLException {
        IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
        String[] vars = getVariableList(expr, grouping);
        return vars;
    }

    /**
     * VarRef(変数参照)要素から変数リストを取得する
     *
     * @param node           VarRef(変数参照)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(VarRef node, boolean grouping) throws XcodeMLException {
        IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
        String[] vars = getVariableList(expr, grouping);
        return vars;
    }

    /**
     * FdoLoop(DO型反復)要素から変数リストを取得する
     *
     * @param node           FdoLoop(DO型反復)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FdoLoop node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();

        // 左括弧
        addOperatorString(list, EXPR_PARENLEFT); // 左括弧 (

        List<Value> values = node.getValue();
        for (Value expr : values) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
                addOperatorString(list, EXPR_COMMA); // カンマ
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

        // 右括弧
        addOperatorString(list, EXPR_PARENRIGHT); // 右括弧 )

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * Var(変数名)要素から変数リストを取得する
     *
     * @param node            Var(変数名)
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     */
    public String[] getVariableList(Var node, boolean grouping) {
        String value = node.getValue();
        if (value == null)
            return null;
        return new String[] { value };
    }

    /**
     * IndexRange(インデックス範囲)要素から変数リストを取得する
     *
     * @param node        IndexRange(インデックス範囲)要素
     * @param grouping		true=変数リストをグルーピングを行う
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(IndexRange node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        LowerBound lower = node.getLowerBound();
        if (lower != null) {
            IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(lower);
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
                addOperatorString(list, EXPR_ARRAYCOLON); // コロン
            }
        }
        UpperBound upper = node.getUpperBound();
        if (upper != null) {
            IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(upper);
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                addOperatorString(list, EXPR_ARRAYCOLON); // コロン
                list.addAll(Arrays.asList(vars));
            }
        }
        Step step = node.getStep();
        if (step != null) {
            IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(step);
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                addOperatorString(list, EXPR_ARRAYCOLON); // コロン
                list.addAll(Arrays.asList(vars));
            }
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }


    /**
     * LowerBound(インデックス範囲の下限)要素から変数リストを取得する
     *
     * @param node        LowerBound(インデックス範囲の下限)要素
     * @param grouping		true=変数リストをグルーピングを行う
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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
        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * UpperBound(インデックス範囲の上限)要素から変数リストを取得する
     *
     * @param node        UpperBound(インデックス範囲の上限)要素
     * @param grouping		true=変数リストをグルーピングを行う
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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
        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * Step(インデックス範囲のステップ)要素から変数リストを取得する
     *
     * @param node        Step(インデックス範囲のステップ)要素
     * @param grouping		true=変数リストをグルーピングを行う
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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
        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * Value(式で表わされる任意の値)要素から変数リストを取得する
     *
     * @param node            Value(式で表わされる任意の値)要素
     * @param grouping		true=変数リストをグルーピングを行う
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
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

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FarrayRef(部分配列、または配列要素参照)要素から変数リストを取得する
     *
     * @param node            FarrayRef(部分配列、または配列要素参照)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FarrayRef node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();

        VarRef varRef = node.getVarRef();
        String[] varRefs = getVariableList(varRef, grouping);
        if (varRefs != null) {
            list.addAll(Arrays.asList(varRefs));
        }

        List<IXmlNode> values = node
                .getIndexRangeOrArrayIndexOrFarrayConstructor();
        // 左括弧
        addOperatorString(list, EXPR_PARENLEFT); // 左括弧 (

        for (IXmlNode expr : values) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
                addOperatorString(list, EXPR_COMMA); // コロン :
            }
        }
        if (list.get(list.size() - 1) == EXPR_COMMA)
            list.remove(list.size() - 1);

        // 右括弧
        addOperatorString(list, EXPR_PARENRIGHT); // 右括弧 )

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FcomplexConstant(COMPLEX型の定数)要素から変数リストを取得する
     *
     * @param node            FcomplexConstant(COMPLEX型の定数)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FcomplexConstant node, boolean grouping) throws XcodeMLException {

        // 定数は変数として返さない
        if ((this._mode & PARSE_VARIABLE) != 0x00) {
            return null;
        }

        ArrayList<String> list = new ArrayList<String>();
        // 左括弧
        addOperatorString(list, EXPR_PARENLEFT); // 左括弧 (

        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;
        for (IXmlNode expr : content) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
                addOperatorString(list, EXPR_COMMA); // カンマ
            }
        }
        if (list.get(list.size() - 1) == EXPR_COMMA)
            list.remove(list.size() - 1);

        // 右括弧
        addOperatorString(list, EXPR_PARENRIGHT); // 右括弧 )

        if (list.size() <= 0)
            return null;

        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * UserUnaryExpr要素から変数リストを取得する
     *
     * @param node           UserUnaryExpr
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(UserUnaryExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        String name = node.getName();
        list.add(name);

        // 左括弧
        addOperatorString(list, EXPR_PARENLEFT); // 左括弧 (

        IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
        if (expr == null)
            return null;
        String[] vars = getVariableList(expr, grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        // 右括弧
        addOperatorString(list, EXPR_PARENRIGHT); // 右括弧 )

        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FpowerExpr(べき乗)要素から変数リストを取得する
     *
     * EXPR_POWER = "**"; /// FpowerExpr ** : べき乗
     *
     * @param node           FpowerExpr(べき乗)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FpowerExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_POWER,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogLEExpr(小なり、または等価)要素から変数リストを取得する
     *
     * EXPR_LOGLE = ".LE."; /// logLEExpr <= .LE. : 小なり、または等価)
     *
     * @param node           LogLEExpr(小なり、または等価)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogLEExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGLE,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FintConstant(整数の値を持つ定数)要素から変数リストを取得する
     *
     * @param node            FintConstant(整数の値を持つ定数)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     */
    public String[] getVariableList(FintConstant node, boolean grouping) {
        // 整数は変数として返さない
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
     * FrealConstant(浮動小数点数)要素から変数リストを取得する
     *
     * @param node            FrealConstant(浮動小数点数)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     */
    public String[] getVariableList(FrealConstant node, boolean grouping) {
        // 定数は変数として返さない
        if ((this._mode & PARSE_VARIABLE) != 0x00) {
            return null;
        }

        ArrayList<String> list = new ArrayList<String>();
        String content = node.getValue();
        String kind = node.getKind();

        if (StringUtils.isNullOrEmpty(kind) == false
                && content.toLowerCase().indexOf("d") < 0) {
            list.add(content + "_" + kind);
        } else {
            list.add(content);
        }
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FcharacterConstant(文字列)要素から変数リストを取得する
     *
     * @param node           FcharacterConstant(文字列)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     */
    public String[] getVariableList(FcharacterConstant node, boolean grouping) {
        // 定数は変数として返さない
        if ((this._mode & PARSE_VARIABLE) != 0x00) {
            return null;
        }

        ArrayList<String> list = new ArrayList<String>();
        String content = node.getValue();
        if (content == null)
            return null;
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
     * FlogicalConstant(論理値)要素から変数リストを取得する
     *
     * @param node           FlogicalConstant(論理値)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     */
    public String[] getVariableList(FlogicalConstant node, boolean grouping) {
        // 定数は変数として返さない
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
     * FarrayConstructor(配列構成子)要素から変数リストを取得する
     *
     * @param node            FarrayConstructor(配列構成子)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FarrayConstructor node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getDefModelExpr();
        if (content == null)
            return null;

        // (
        addOperatorString(list, EXPR_ARRAYLEFT);
        for (IXmlNode expr : content) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
                addOperatorString(list, EXPR_COMMA); // カンマ
            }
        }
        if (list.get(list.size() - 1) == EXPR_COMMA)
            list.remove(list.size() - 1);

        // )
        addOperatorString(list, EXPR_ARRAYRIGHT);

        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FstructConstructor(構造体構成子)要素から変数リストを取得する
     *
     * @param node            FstructConstructor(構造体構成子)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FstructConstructor node, boolean grouping) throws XcodeMLException {
        FstructType structTypeElem = (FstructType) _typeManager.findType(node
                .getType());
        String aliasStructTypeName = _typeManager
                .getAliasTypeName(structTypeElem.getType());

        ArrayList<String> list = new ArrayList<String>();
        list.add(aliasStructTypeName);

        // 左括弧
        addOperatorString(list, EXPR_PARENLEFT); // 左括弧 (

        List<IXmlNode> content = node.getDefModelExpr();
        if (content == null)
            return null;
        for (IXmlNode expr : content) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
                addOperatorString(list, EXPR_COMMA); // カンマ
            }
        }
        if (list.get(list.size() - 1) == EXPR_COMMA)
            list.remove(list.size() - 1);

        // 右括弧
        addOperatorString(list, EXPR_PARENRIGHT); // 右括弧 )

        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * UnaryMinusExpr(符号反転)要素から変数リストを取得する
     *
     * EXPR_UNARYMINUS = "-"; /// unaryMinusExpr - : 符号反転
     *
     * @param node           UnaryMinusExpr(符号反転)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(UnaryMinusExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        addOperatorString(list, EXPR_UNARYMINUS); // '-' : 符号反転

        for (IXmlNode expr : content) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogNotExpr(論理否定)要素から変数リストを取得する
     *
     * EXPR_LOGNOT = ".NOT."; /// logNotExpr .NOT. : 論理否定
     *
     * @param node           LogNotExpr(論理否定)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogNotExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        addOperatorString(list, EXPR_LOGNOT); // '.NOT.' : 論理否定
        for (IXmlNode expr : content) {
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * PlusExpr(加算)要素から変数リストを取得する EXPR_PLUS = "+"; /// plusExpr + : 加算
     *
     * @param node            PlusExpr(加算)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(PlusExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_PLUS,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * MinusExpr(減算)要素から変数リストを取得する EXPR_MINUS = "-"; /// minusExpr - : 減算
     *
     * @param node            MinusExpr(減算)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(MinusExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_MINUS,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * MulExpr(乗算)要素から変数リストを取得する EXPR_MUL = "*"; /// mulExpr * : 乗算
     *
     * @param node            MulExpr(乗算)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(MulExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_MUL, grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * DivExpr(除算)要素から変数リストを取得する EXPR_DIV = "/"; /// divExpr / : 除算
     *
     * @param node            DivExpr(除算)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(DivExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_DIV, grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * FconcatExpr(文字式の連結)要素から変数リストを取得する EXPR_CONCAT = "//"; /// FconcatExpr //
     * : 文字式の連結
     *
     * @param node           FconcatExpr(文字式の連結)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(FconcatExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_CONCAT,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogEQExpr(等価)要素から変数リストを取得する EXPR_LOGEQ = ".EQ."; /// logEQExpr == .EQ. :
     * 等価
     *
     * @param node            LogEQExpr(等価)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogEQExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGEQ,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogNEQExpr(非等価)要素から変数リストを取得する EXPR_LOGNEQ = ".NE."; /// logNEQExpr /=
     * .NE. : 非等価
     *
     * @param node           LogNEQExpr(非等価)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogNEQExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGNEQ,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogGEExpr(大なり、または同値)要素から変数リストを取得する
     *
     * EXPR_LOGGE = ".GE."; /// logGEExpr >= .GE. : 大なり、または同値
     *
     * @param node           LogGEExpr(大なり、または同値)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogGEExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGGE,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogGTExpr(大なり)要素から変数リストを取得する EXPR_LOGGT = ".GT."; /// logGTExpr > .GT. :
     * 大なり
     *
     * @param node            LogGTExpr(大なり)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogGTExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGGT,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogLTExpr(小なり)要素から変数リストを取得する EXPR_LOGLT = ".LT."; /// logLTExpr < .LT. :
     * 小なり
     *
     * @param node            LogLTExpr(小なり)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogLTExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGLT,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogAndExpr(論理積)要素から変数リストを取得する EXPR_LOGAND = ".AND."; /// logAndExpr .AND.
     * : 論理積
     *
     * @param node            LogAndExpr(論理積)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogAndExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGAND,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogOrExpr(論理和)要素から変数リストを取得する EXPR_LOGOR = ".OR."; /// logOrExpr .OR. :
     * 論理和
     *
     * @param node           LogOrExpr(論理和)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogOrExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGOR,
                grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogEQVExpr(論理等価)要素から変数リストを取得する EXPR_LOGEQV = ".EQV."; /// logEQVExpr
     * .EQV. : 論理等価
     *
     * @param node            LogEQVExpr(論理等価)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogEQVExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGEQV, grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * LogNEQVExpr(論理非等価)要素から変数リストを取得する EXPR_LOGNEQV = ".NEQV."; /// logNEQVExpr
     * .NEQV. : 論理非等価
     *
     * @param node            LogNEQVExpr(論理非等価)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(LogNEQVExpr node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        List<IXmlNode> content = node.getContent();
        if (content == null)
            return null;

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;
        String[] vars = getVariableList(leftExpr, rightExpr, EXPR_LOGNEQV, grouping);
        if (vars != null) {
            list.addAll(Arrays.asList(vars));
        }

        if (list.size() <= 0)
            return null;
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * exprModelモデル要素から変数リストを取得する
     *
     * @param node           exprModelモデル要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(IDefModelExpr node, boolean grouping) throws XcodeMLException {
        if (node == null)
            return null;
        IXmlNode def = XmlNodeUtil.getXmlNodeChoice(node);
        String[] vars = getVariableList(def, grouping);

        return vars;
    }

    /**
     * 演算子をリストに追加する。 パースモードのPARSE_OPERATORをチェックして演算子の追加を判断する。
     *
     * @param list            変数リスト
     * @param oper
     *            追加演算子
     */
    private void addOperatorString(List<String> list, String oper) {
        if ((this._mode & PARSE_OPERATOR) != 0x00) {
            list.add(oper);
        }
        return;
    }

    /**
     * 演算子の左辺、右辺から変数リストをパースする。
     *
     * @param leftExpr            左辺
     * @param rightExpr            右辺
     * @param operation            演算子
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 変数リスト
     * @throws XcodeMLException  パースエラー
     */
    private String[] getVariableList(IXmlNode leftExpr, IXmlNode rightExpr,
            String operation, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();

        if (grouping) {
            // 左括弧
            addOperatorString(list, EXPR_PARENLEFT); // 左括弧
        }

        // 左辺
        if (leftExpr != null) {
            String[] vars = getVariableList(leftExpr, true);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }

        addOperatorString(list, operation); // 演算子

        // 右辺
        if (rightExpr != null) {
            String[] vars = getVariableList(rightExpr, true);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }

        if (grouping) {
            addOperatorString(list, EXPR_PARENRIGHT); // 右括弧
        }
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * NamedValue(名前付きの値)要素から属性を取得する
     * @param node            NamedValue(名前付きの値)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 属性リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(NamedValue node, boolean grouping) throws XcodeMLException {
        ArrayList<String> list = new ArrayList<String>();
        // 名前
        String name = node.getName();
        list.add(name);
        if (name == null) return null;

        // '='を追加する
        list.add("=");

        // 値
        String value = node.getValue();
        if (value != null) {
            list.add(value);
        }
        else {
            IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
            String[] vars = getVariableList(expr, grouping);
            if (vars != null) {
                list.addAll(Arrays.asList(vars));
            }
        }
        return (String[]) list.toArray(new String[list.size()]);
    }


    /**
     * Condition(条件式)要素から属性を取得する
     * @param node            Condition(条件式)要素
     * @param grouping        式組立時のグルーピング'(..)'のフラグ
     * @return 属性リスト
     * @throws XcodeMLException  パースエラー
     */
    public String[] getVariableList(Condition node, boolean grouping) throws XcodeMLException {

        IXmlNode expr = XmlNodeUtil.getXmlNodeChoice(node);
        String[] vars = getVariableList(expr, grouping);

        return vars;
    }

}
