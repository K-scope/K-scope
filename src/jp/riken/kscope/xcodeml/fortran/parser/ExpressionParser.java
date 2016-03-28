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
package jp.riken.kscope.xcodeml.fortran.parser;

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
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.fortran.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.fortran.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.fortran.xml.EnumType;
import jp.riken.kscope.xcodeml.fortran.xml.FdoStatementSequence;
import jp.riken.kscope.xcodeml.fortran.xml.IDefModelExpr;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.fortran.xml.gen.ArrayIndex;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Condition;
import jp.riken.kscope.xcodeml.fortran.xml.gen.DefModelBinaryOperation;
import jp.riken.kscope.xcodeml.fortran.xml.gen.DefModelExprList;
import jp.riken.kscope.xcodeml.fortran.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FarrayConstructor;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FarrayRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcaseLabel;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcharacterConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcharacterRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcoArrayRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcomplexConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FconcatExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdoLoop;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FintConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FlogicalConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FmemberRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FpowerExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FrealConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FstructConstructor;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.fortran.xml.gen.IndexRange;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogAndExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogEQExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogEQVExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogGEExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogGTExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogLEExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogLTExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogNEQExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogNEQVExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogNotExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LogOrExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.LowerBound;
import jp.riken.kscope.xcodeml.fortran.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Name;
import jp.riken.kscope.xcodeml.fortran.xml.gen.NamedValue;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Params;
import jp.riken.kscope.xcodeml.fortran.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.RepeatCount;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Step;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UnaryMinusExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UpperBound;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UserBinaryExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UserUnaryExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Value;
import jp.riken.kscope.xcodeml.fortran.xml.gen.ValueList;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Var;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarList;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarRef;



/**
 * Expressionパーサクラス
 * @author RIKEN
 */
public class ExpressionParser {

    /** パースIXmlNode要素 */
    private IXmlNode parseNode;
    /** typeTable */
    private XcodeMLTypeManager typeManager;
    /** IndexRangeの親クラス */
    private Class<?> parentIndexRange;
    /** 外部手続きリスト */
    private Map<String, IVariableType> externalFunctionList;

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
    /** 左小括弧 ( */
    private final String EXPR_PARENLEFT = "(";
    /** 右小括弧 ) */
    private final String EXPR_PARENRIGHT = ")";
    /** FmemberRef % : 構造体メンバ */
    @SuppressWarnings("unused")
    private final String EXPR_TYPEMEMBER = "%";
    /** 左大括弧 [ */
    private final String EXPR_COARRAYLEFT = "[";
    /** 右大括弧 ] */
    private final String EXPR_COARRAYRIGHT = "[";
    /** 等号記号 = */
    private final String EXPR_EQUAL = "=";
    /** スペース */
    private final String EXPR_SPACE = " ";
    /** FarrayConstructor 左初期化小括弧 */
    private final String EXPR_ARRAYLEFT = "(/";
    /** FarrayConstructor 右初期化小括弧 */
    private final String EXPR_ARRAYRIGHT = "/)";
    /** FarrayRef 配列区切りコロン : */
    private final String EXPR_ARRAYCOLON = ":";


    /**
     * コンストラクタ
     *
     * @param typeManager    typeTable
     */
    public ExpressionParser(XcodeMLTypeManager typeManager) {
        this.typeManager = typeManager;
    }

    /**
     * コンストラクタ
     *
     * @param typeManager    typeTable
     * @param node           パースexprModel要素
     */
    public ExpressionParser(XcodeMLTypeManager typeManager, IXmlNode node) {
        this(typeManager);
        this.parseNode = node;
    }

    /**
     * パースIXmlNode要素を取得する
     * @return        パースIXmlNode要素
     */
    public IXmlNode getParseNode() {
        return parseNode;
    }

    /**
     * パースIXmlNode要素を設定する
     * @param parseNode        パースIXmlNode要素
     */
    public void setParseNode(IXmlNode parseNode) {
        this.parseNode = parseNode;
    }

    /**
     * typeTableを取得する
     * @return        typeTable
     */
    public XcodeMLTypeManager getTypeManager() {
        return typeManager;
    }

    /**
     * typeTableを設定する
     * @param typeManager        typeTable
     */
    public void setTypeManager(XcodeMLTypeManager typeManager) {
        this.typeManager = typeManager;
    }

    /**
     * 要素から式クラスオブジェクトを取得する。
     *
     * @return 式クラスオブジェクト
     */
    public Expression getExpression()  {
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
     * 要素から式クラスオブジェクトを取得する。
     *
     * @param node            IXmlNode要素
     * @return 式クラスオブジェクト
     * @throws XcodeMLException   パースエラー
     */
    public Expression getExpression(IXmlNode node) throws XcodeMLException {
        if (node == null) return null;

        Expression expr = null;
        if (node instanceof FunctionCall)
            expr = getExpression((FunctionCall) node);
        else if (node instanceof FcharacterRef)
            expr = getExpression((FcharacterRef) node);
        else if (node instanceof FmemberRef)
            expr = getExpression((FmemberRef) node);
        else if (node instanceof FcoArrayRef)
            expr = getExpression((FcoArrayRef) node);
        else if (node instanceof FdoLoop)
            expr = getExpression((FdoLoop) node);
        else if (node instanceof FarrayRef)
            expr = getExpression((FarrayRef) node);
        else if (node instanceof FcomplexConstant)
            expr = getExpression((FcomplexConstant) node);
        else if (node instanceof UserUnaryExpr)
            expr = getExpression((UserUnaryExpr) node);
        /*
         * DefModelBinaryOperation要素
         *         DivExpr
         *        FconcatExpr
         *         FpowerExpr
         *         LogAndExpr
         *         LogEQExpr
         *         LogEQVExpr
         *         LogGEExpr
         *         LogGTExpr
         *         LogLEExpr
         *         LogLTExpr
         *         LogNEQExpr
         *         LogNEQVExpr
         *         LogNotExpr (単項)
         *         LogOrExpr
         *         MinusExpr
         *         MulExpr
         *         PlusExpr
         *         UnaryMinusExpr (単項)
         *         UserBinaryExpr (単項)
         */
        else if (node instanceof LogNotExpr)
            expr = getExpression((LogNotExpr) node);
        else if (node instanceof UnaryMinusExpr)
            expr = getExpression((UnaryMinusExpr) node);
        else if (node instanceof UserBinaryExpr)
            expr = getExpression((UserBinaryExpr) node);
        else if (node instanceof DefModelBinaryOperation)
            expr = getExpression((DefModelBinaryOperation) node);

        else if (node instanceof FintConstant)
            expr = getExpression((FintConstant) node);
        else if (node instanceof FrealConstant)
            expr = getExpression((FrealConstant) node);
        else if (node instanceof FcharacterConstant)
            expr = getExpression((FcharacterConstant) node);
        else if (node instanceof FlogicalConstant)
            expr = getExpression((FlogicalConstant) node);
        else if (node instanceof Var)
            expr = getExpression((Var) node);
        /*
         * DefModelExprList
         *         FarrayConstructor
         *         FstructConstructor
         */
        else if (node instanceof DefModelExprList)
            expr = getExpression((DefModelExprList) node);
        else if (node instanceof VarRef)
            expr = getExpression((VarRef) node);
        else if (node instanceof IndexRange)
            expr = getExpression((IndexRange) node);
        else if (node instanceof LowerBound)
            expr = getExpression((LowerBound) node);
        else if (node instanceof UpperBound)
            expr = getExpression((UpperBound) node);
        else if (node instanceof Step)
            expr = getExpression((Step) node);
        else if (node instanceof ArrayIndex)
            expr = getExpression((ArrayIndex) node);
        else if (node instanceof NamedValue)
            expr = getExpression((NamedValue) node);
        else if (node instanceof Value)
            expr = getExpression((Value) node);
        else if (node instanceof Condition)
            expr = getExpression((Condition) node);
        else if (node instanceof Arguments)
            expr = getExpression((Arguments) node);
        else if (node instanceof IDefModelExpr)
            expr = getExpression((IDefModelExpr) node);
        else if (node instanceof FcaseLabel)
            expr = getExpression((FcaseLabel) node);
        else if (node instanceof Params)
            expr = getExpression((Params) node);
        else if (node instanceof VarList)
            expr = getExpression((VarList) node);
        else if (node instanceof ValueList)
            expr = getExpression((ValueList) node);

        return expr;
    }


    /**
     * IDefModelExpr要素から式クラスを作成する
     * @param node            IDefModelExpr要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(IDefModelExpr node) throws XcodeMLException {
        if (node == null) return null;
        IXmlNode def = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(def);

        return expr;
    }

    /**
     * FunctionCall(関数呼出)要素から式クラスを作成する
     * @param node            FunctionCall(関数呼出)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FunctionCall node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // サブルーチン名、関数名
        Name name = node.getName();
        String funcName = name.getValue();

        // データ型パーサ
        VariableTypeParser typePaser = new VariableTypeParser(this.typeManager);
        VariableType funcVarType = null;

        // 属性
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
                //EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
                is_intrinsic = XmlNodeUtil.isBoolean(functionTypeElem.isIsIntrinsic());
                is_external = XmlNodeUtil.isBoolean(functionTypeElem.isIsExternal());
                is_recursive = XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive());
                is_puiblic = XmlNodeUtil.isBoolean(functionTypeElem.isIsPublic());
                is_private = XmlNodeUtil.isBoolean(functionTypeElem.isIsPrivate());
                funcVarType = typePaser.parseVarDefFunctionType(functionTypeElem);
            }
        }
        if (funcVarType == null) {
            // 変数のパース
            funcVarType = getVariableType(node.getType());
        }

        boolean intrinsic = XmlNodeUtil.isBoolean(node.isIsIntrinsic());

        // FunctionCall式
        Expression expr = new Expression();

        // バッファ追加:関数名
        buf.append(name.getValue());

        // バッファ追加:左括弧
        buf.append(EXPR_PARENLEFT);

        // 仮引数の取得を行う
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
            // 1つのExpressionの生成
            // Expression exprFuncArg = mergeExpression(argsExpr);
            // expr = mergeExpression(new Expression[]{expr, exprFuncArg});
        }

        // バッファ追加:右括弧
        buf.append(EXPR_PARENRIGHT);

        // 組込関数に引数の追加
        List<Expression> argslist = null;
        if (argsExpr != null) {
            argslist = new ArrayList<Expression>(java.util.Arrays.asList(argsExpr));
        }
        ProcedureUsage proc = new ProcedureUsage(name.getValue(), argslist);
        // 組込関数フラグセット
        if (intrinsic || is_intrinsic) {
            proc.setIntrinsic();
        }

        // 関数を追加する
        expr.addFuncCall(proc);
        // データ型
        expr.setVariableType(funcVarType);

        // FunctionCall:式文字列
        expr.setLine(buf.toString());

        // 外部手続きリストに追加する
        if (is_external && funcVarType != null) {
            addExternalFunction(funcName, funcVarType);
        }
        return expr;
    }

    /**
     * Arguments(引数)要素から式クラスを作成する
     * @param node            Arguments(引数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Arguments node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // 仮引数の取得を行う
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

        // 1つのExpressionの生成
        Expression expr = mergeExpression(argsExpr);

        // Arguments:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * Arguments(引数)要素から式クラスを作成する
     * @param node            Arguments(引数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    public Expression[] getExpressionArray(Arguments node) throws XcodeMLException {

        // 仮引数の取得を行う
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
     * UserBinaryExpr(INTERFACE依存)要素から式クラスを作成する
     * @param node            UserBinaryExpr(INTERFACE依存)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(UserBinaryExpr node) throws XcodeMLException {
        // 式バッファ
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

                // バッファ追加
                buf.append(var.getLine());
                buf.append(EXPR_SPACE);

            }
        }

        // 演算子名追加
        buf.append(name);
        buf.append(EXPR_SPACE);

        if (rightExpr != null) {
            Expression var = getExpression(rightExpr);
            if (var != null) {
                list.add(var);
                // バッファ追加
                buf.append(var.getLine());
                buf.append(EXPR_SPACE);
            }
        }
        if (list.size() <= 0) return null;

        // 1つのExpressionの生成
        Expression expr = mergeExpression(list.toArray(new Expression[0]));
        // 変数のパース
        VariableType varType = getVariableType(node.getType());
        expr.setVariableType(varType);

        // UserBinaryExpr:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * DefModelBinaryOperation(二項演算式)要素から式クラスを作成する
     * @param node            DefModelBinaryOperation(二項演算式)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(DefModelBinaryOperation node) throws XcodeMLException {

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        List<IXmlNode> content = node.getContent();

        IXmlNode leftExpr = content.size() >= 1 ? content.get(0) : null;
        IXmlNode rightExpr = content.size() >= 2 ? content.get(1) : null;

        List<Expression> list = new ArrayList<Expression>();
        if (leftExpr != null) {
            Expression var = getExpression(leftExpr);
            if (var != null) {
                list.add(var);
                // バッファ追加
                buf.append(var.getLine());
                buf.append(EXPR_SPACE);
            }
        }

        // バッファに追加:演算子
        String op = getOperation(node);
        // バッファ追加
        buf.append(op);
        buf.append(EXPR_SPACE);

        if (rightExpr != null) {
            Expression var = getExpression(rightExpr);
            if (var != null) {
                list.add(var);
                // バッファ追加
                buf.append(var.getLine());
            }
        }
        if (list.size() <= 0) return null;

        // 1つのExpressionの生成
        Expression expr = mergeExpression(list.toArray(new Expression[0]));

        //(2012/4/18) changed by tomiyama and teraim 浮動小数点演算のみ対象とするように変更
        String attr = node.getType();
        if (!attr.equals(new String("Fint"))) {
            if (node instanceof PlusExpr) {
                // 加算のインクリメント
                expr.incrementAdd();
            } else if (node instanceof MinusExpr) {
                // 減算のインクリメント
                expr.incrementSub();
            } else if (node instanceof MulExpr) {
                // 乗算のインクリメント
                expr.incrementMul();
            } else if (node instanceof DivExpr) {
                // 除算のインクリメント
                expr.incrementDiv();
            } else if (node instanceof FpowerExpr) {
                // べき算のインクリメント
                expr.incrementPow();
            }
        }

        // 変数のパース
        VariableType varType = getVariableType(node.getType());
        expr.setVariableType(varType);

        // DefModelBinaryOperation:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * LogNotExpr(論理否定)要素から式クラスを作成する
     * @param node            LogNotExpr(論理否定)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(LogNotExpr node) throws XcodeMLException {

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // バッファ追加 : '.NOT.' : 論理否定
        buf.append(EXPR_LOGNOT);
        buf.append(EXPR_SPACE);

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);
        if (expr == null) return null;

        // バッファ追加
        buf.append(expr.getLine());
        buf.append(EXPR_SPACE);

        // 変数のパース
        VariableType varType = getVariableType(node.getType());
        expr.setVariableType(varType);

        // LogNotExpr:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * UnaryMinusExpr(符号反転)要素から式クラスを作成する
     * @param node            UnaryMinusExpr(符号反転)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(UnaryMinusExpr node) throws XcodeMLException {

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // バッファ追加 : '-' : 符号反転
        buf.append(EXPR_UNARYMINUS); // '-' : 符号反転

        buf.append(EXPR_UNARYMINUS);

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);
        if (expr == null) return null;

        // バッファ追加
        buf.append(expr.getLine());
        buf.append(EXPR_SPACE);
        // 変数のパース
        VariableType varType = getVariableType(node.getType());
        expr.setVariableType(varType);

        // UnaryMinusExpr:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * Condition(条件式)要素から式クラスを作成する
     * @param node            Condition(条件式)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Condition node) throws XcodeMLException {

        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);

        return expr;
    }


    /**
     * FcharacterRef(部分文字列参照)要素から式クラスを作成する
     *
     * @param node            FcharacterRef(部分文字列参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FcharacterRef node) throws XcodeMLException {

        // IndexRange呼出クラス:FcharacterRef
        this.parentIndexRange = node.getClass();

        String type = node.getType();

        // 変数のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        VarRef varRef = node.getVarRef();
        IndexRange indexRange = node.getIndexRange();

        // 部分文字列変数
        Expression exprVar = getExpression(varRef);

        // インデックス
        Expression exprIndex = getExpression(indexRange);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // Expressionクラス生成
        Expression expr = mergeExpression(new Expression[]{exprVar, exprIndex});
        expr.setVariableType(varType);
        if (var != null) {
            expr.addVariable(var);
        }

        // FcharacterRef:式文字列
        expr.setLine(var.getVariableString());

        return expr;
    }


    /**
     * FmemberRef(構造体メンバ参照)要素から式クラスを作成する
     *
     * @param node            FmemberRef(構造体メンバ参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FmemberRef node) throws XcodeMLException {

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        VarRef varRef = node.getVarRef();

        // 子要素のExpressionクラス
        Expression exprVar = getExpression(varRef);

        // Expressionクラス生成
        exprVar.setVariableType(varType);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // 構造体参照の設定
        if (var != null) {
            exprVar.addVariable(var);
        }

        // FmemberRef:式文字列
        exprVar.setLine(var.getVariableString());

        return exprVar;
    }


    /**
     * FcoArrayRef(coarrayの参照)要素から式クラスを作成する
     *
     * @param node            FcoArrayRef(coarrayの参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FcoArrayRef node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        VarRef varRef = node.getVarRef();
        List<ArrayIndex> arrayIndexes = node.getArrayIndex();

        // 子要素のExpressionクラス
        Expression exprVar = getExpression(varRef);
        // バッファ追加
        buf.append(exprVar.getLine());

        // バッファ追加: [
        buf.append(EXPR_COARRAYLEFT);

        List<Expression> list = new ArrayList<Expression>();
        Expression exprIndex = null;
        if (arrayIndexes != null && arrayIndexes.size() > 0) {
            int count = 0;
            for (ArrayIndex index : arrayIndexes) {
                if (count > 0) {
                    // バッファ追加
                    buf.append(EXPR_COMMA);       // カンマ
                }
                Expression expr = getExpression(index);
                // バッファ追加
                buf.append(expr.getLine());
                list.add(expr);
                count++;
            }
        }
        if (list.size() > 0) {
            exprIndex = mergeExpression(list.toArray(new Expression[0]));
        }

        // バッファ追加: ]
        buf.append(EXPR_COARRAYRIGHT);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // Expressionクラス生成
        Expression expr = mergeExpression(new Expression[]{exprVar, exprIndex});
        expr.setVariableType(varType);
        if (var != null) {
            expr.addVariable(var);
        }

        // FcoArrayRef:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * FdoLoop(DO型反復)要素から式クラスを作成する
     *
     * @param node            FdoLoop(DO型反復)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FdoLoop node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // IndexRange呼出クラス:FdoLoop
        this.parentIndexRange = node.getClass();

        // 子要素の取得
        Var var = node.getVar();
        IndexRange indexRange = node.getIndexRange();
        List<Value> values = node.getValue();

        // 子要素のExpressionクラス
        // バッファ追加:左括弧
        buf.append(EXPR_PARENLEFT); // 左括弧 (

        List<Expression> list = new ArrayList<Expression>();
        Expression exprValue = null;
        if (values != null && values.size() > 0) {
            int count = 0;
            for (Value value : values) {
                if (count > 0) {
                    // バッファ追加
                    buf.append(EXPR_COMMA);        // カンマ
                }
                Expression expr = getExpression(value);
                // バッファ追加
                buf.append(expr.getLine());
                list.add(expr);
                count++;
            }
            buf.append(EXPR_COMMA);        // カンマ
        }
        if (list.size() > 0) {
            exprValue = mergeExpression(list.toArray(new Expression[0]));
        }

        Expression exprVar = getExpression(var);
        // バッファ追加
        buf.append(exprVar.getLine());

        // バッファ追加:=
        buf.append(EXPR_EQUAL); // =

        // IndexRange
        Expression exprIndex = getExpression(indexRange);
        // バッファ追加
        buf.append(exprIndex.getLine());

        // バッファ追加:右括弧 )
        buf.append(EXPR_PARENRIGHT); // 右括弧 )

        // Expressionクラス生成
        Expression expr = mergeExpression(new Expression[]{exprVar, exprIndex, exprValue});

        // FdoLoop:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * FarrayRef(部分配列、または配列要素参照)要素から式クラスを作成する.
     * (例) a(1:3) = 5
     * @param node            FarrayRef(部分配列、または配列要素参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FarrayRef node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // IndexRange呼出クラス:FarrayRef
        this.parentIndexRange = node.getClass();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        VarRef varref = node.getVarRef();
        List<IXmlNode> indexes = node.getIndexRangeOrArrayIndexOrFarrayConstructor();

        // 変数名
        Expression exprVarRef = getExpression(varref);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        // 変数
        Variable arrayVar = varParser.getVariable(node);

        if (arrayVar == null) {
            if (exprVarRef.getVariables() != null && exprVarRef.getVariables().size() > 0) {
                arrayVar = exprVarRef.getVariables().get(0);
            }
        }

        // バッファ追加:変数名
        buf.append(arrayVar.getName());

        // バッファ追加:左括弧
        buf.append(EXPR_PARENLEFT); // 左括弧 (

        List<Expression> list = new ArrayList<Expression>();
        Expression exprIndex = null;
        if (indexes != null && indexes.size() > 0) {
            int count = 0;
            for (IXmlNode index : indexes) {
                if (count != 0) buf.append(EXPR_COMMA);
                Expression expr = getExpression(index);
                list.add(expr);
                // バッファ追加:変数名
                buf.append(expr.getLine());
                count++;
            }
        }

        // バッファ追加:右括弧
        buf.append(EXPR_PARENRIGHT);    // 右括弧 )
        if (list.size() > 0) {
//            exprIndex = mergeExpression(list.toArray(new Expression[0]));
        }

        // Expressionクラス生成
//        Expression expr = mergeExpression(new Expression[]{exprVarRef, exprIndex});
        Expression expr = exprVarRef;
        expr.setVariableType(varType);
        if (arrayVar != null) {
            // expr.addVariable(arrayVar);
            List<Variable> vars = new ArrayList<Variable>();
            vars.addAll(Arrays.asList(new Variable[]{arrayVar}));
            expr.setVariables(vars);
        }
        // FarrayRef:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * FcomplexConstant(複素数型定数)要素から式クラスを作成する
     *
     * @param node            FcomplexConstant(複素数型定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FcomplexConstant node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        List<IXmlNode> contents = node.getContent();

        // バッファ追加:左括弧
        buf.append(EXPR_PARENLEFT); // 左括弧 (

        // 子要素のExpressionクラス
        List<Expression> list = new ArrayList<Expression>();
        Expression exprContent = null;
        if (contents != null && contents.size() > 0) {
            int count = 0;
            for (IXmlNode content : contents) {
                if (count > 0) {
                    // バッファ追加
                    buf.append(EXPR_COMMA);        // カンマ
                }
                Expression expr = getExpression(content);
                // バッファ追加
                buf.append(expr.getLine());
                list.add(expr);
                count++;
            }
        }
        if (list.size() <= 0 ) return null;

        // バッファ追加:右括弧
        buf.append(EXPR_PARENRIGHT); // 右括弧 )

        exprContent = mergeExpression(list.toArray(new Expression[0]));

        // Expressionクラス生成
        Expression expr = exprContent;
        expr.setVariableType(varType);

        // FcomplexConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * FrealConstant(浮動小数定数)要素から式クラスを作成する
     *
     * @param node           FrealConstant(浮動小数定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FrealConstant node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 定数値
        String value = node.getValue();

        // バッファ追加：値
        buf.append(value);

        // Expressionクラス生成
        Expression expr = new Expression(value);
        expr.setVariableType(varType);

        // Kind
        String kind = node.getKind();
        if (kind != null && !kind.isEmpty()
            && !StringUtils.isNumeric(kind) ) {
            buf.append("_" + kind);
            if (!StringUtils.isNumeric(kind)) {
                Variable kind_var = new Variable(kind);
                expr.addVariable(kind_var);
            }
        }

        // FrealConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * FcharacterConstant(文字列定数)要素から式クラスを作成する
     *
     * @param node           FcharacterConstant(文字列定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FcharacterConstant node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 定数値
        String value = node.getValue();

        // バッファ追加：値
        buf.append("\"" + value + "\"");

        // Expressionクラス生成
        Expression expr = new Expression(value);
        expr.setVariableType(varType);

        // FcharacterConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * FintConstant(整数型定数)要素から式クラスを作成する
     *
     * @param node            FintConstant(整数型定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FintConstant node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 定数値
        String value = node.getValue();

        // バッファ追加：値
        buf.append(value);

        // Expressionクラス生成
        Expression expr = new Expression(value);
        expr.setVariableType(varType);

        // FintConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }



    /**
     * FlogicalConstant(論理値定数)要素から式クラスを作成する
     *
     * @param node            FlogicalConstant(整数型定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FlogicalConstant node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 定数値
        String value = node.getValue();

        // バッファ追加：値
        buf.append(value);

        // Expressionクラス生成
        Expression expr = new Expression(value);
        expr.setVariableType(varType);

        // FlogicalConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * UserUnaryExpr(単項演算式:INTERFACE依存)要素から式クラスを作成する
     *
     * @param node            UserUnaryExpr(単項演算式:INTERFACE依存)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(UserUnaryExpr node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // バッファ追加:左括弧
        buf.append(EXPR_PARENLEFT); // 左括弧 (

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);
        // バッファ追加
        buf.append(expr.getLine());

        // バッファ追加:右括弧
        buf.append(EXPR_PARENRIGHT); // 右括弧 )

        // Expressionクラス生成
        expr.setVariableType(varType);

        // UserUnaryExpr:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * DefModelExprList(配列構成子,構造体構成子)要素から式クラスを作成する
     *
     * @param node            DefModelExprList(配列構成子,構造体構成子)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(DefModelExprList node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = null;
        if (node instanceof FarrayConstructor) {
            type = ((FarrayConstructor)node).getType();
            // バッファ追加 : (/
            buf.append( EXPR_ARRAYLEFT);
        }
        else if (node instanceof FstructConstructor) {
            type = ((FstructConstructor)node).getType();
            // バッファ追加 : (
            buf.append( EXPR_PARENLEFT); // 左括弧 (
        }

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        List<IXmlNode> models = node.getDefModelExpr();

        // 子要素のExpressionクラス
        List<Expression> list = new ArrayList<Expression>();
        Expression exprContent = null;
        if (models != null && models.size() > 0) {
            int count = 0;
            for (IXmlNode model : models) {
                if (count > 0) {
                    // バッファ追加:カンマ
                    buf.append(EXPR_COMMA); // カンマ
                }
                Expression expr = getExpression(model);
                // バッファ追加
                buf.append(expr.getLine());
                list.add(expr);
                count++;
            }
        }
        if (list.size() <= 0) return null;

        exprContent = mergeExpression(list.toArray(new Expression[0]));

        if (node instanceof FarrayConstructor) {
            // バッファ追加 : /)
            buf.append( EXPR_ARRAYRIGHT);
        }
        else if (node instanceof FstructConstructor) {
            // バッファ追加 : )
            buf.append( EXPR_PARENRIGHT); // 右括弧 )
        }

        // Expressionクラス生成
        Expression expr = exprContent;
        expr.setVariableType(varType);

        // DefModelExprList:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * Var(変数名)要素から式クラスを作成する
     *
     * @param node            Var(変数名)
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Var node) throws XcodeMLException {

        // 変数データ型のパース
        VariableTypeParser typeParser = new VariableTypeParser(this.typeManager);
        VariableType varType = typeParser.parseVariableType(node);

        // Expressionクラス生成
        Expression expr = new Expression();

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // 変数の追加
        if (var != null) {
            expr.addVariable(var);
        }
        expr.setVariableType(varType);

        // Var:式文字列
        expr.setLine(var.getVariableString());

        return expr;
    }

    /**
     * VarRef(変数参照)要素から式クラスを作成する
     *
     * @param node            VarRef(変数参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(VarRef node) throws XcodeMLException {
        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);

        // Expressionクラス生成
        expr.setVariableType(varType);

        return expr;
    }


    /**
     * IndexRange(インデックス範囲)要素から式クラスを作成する
     *
     * @param node            IndexRange(インデックス範囲)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(IndexRange node) throws XcodeMLException {

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // IndexRangeの区切文字
        String delim = EXPR_ARRAYCOLON;
        if (this.parentIndexRange != null) {
            if (this.parentIndexRange.equals(FdoLoop.class)) {
                // Parent node is FdoLoop
                delim = EXPR_COMMA;
            }
            else if (this.parentIndexRange.equals(FdoStatementSequence.class)) {
                // Parent node is FdoStatementSequence
                delim = EXPR_COMMA;
            }
        }

        // 子要素の取得
        LowerBound lower = node.getLowerBound();
        Expression exprLower = null;
        if (lower != null) {
            IXmlNode nodeLower = XmlNodeUtil.getXmlNodeChoice(lower);
            exprLower = getExpression(nodeLower);
            // バッファ追加
            if (exprLower != null) {
                buf.append(exprLower.getLine());
            }
        }

        // バッファ追加:IndexRangeの区切文字
        buf.append(delim);

        UpperBound upper = node.getUpperBound();
        Expression exprUpper = null;
        if (upper != null) {
            IXmlNode nodeUpper = XmlNodeUtil.getXmlNodeChoice(upper);
            exprUpper = getExpression(nodeUpper);
            // バッファ追加
            if (exprUpper != null) {
                buf.append(exprUpper.getLine());
            }
        }
        Step step = node.getStep();
        Expression exprStep = null;
        if (step != null) {
            // バッファ追加:IndexRangeの区切文字
            buf.append(delim);

            IXmlNode nodeStep = XmlNodeUtil.getXmlNodeChoice(step);
            exprStep = getExpression(nodeStep);
            // バッファ追加
            if (exprStep != null) {
                buf.append(exprStep.getLine());
            }
        }

        // Expressionクラス生成
        Expression expr = mergeExpression(new Expression[]{exprLower, exprUpper, exprStep});

        // IndexRange:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * LowerBound(範囲の下限)要素から式クラスを作成する
     *
     * @param node            LowerBound(範囲の下限)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(LowerBound node) throws XcodeMLException {

        // 子要素の取得
        IXmlNode nodeLower = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(nodeLower);

        return expr;
    }

    /**
     * UpperBound(範囲の上限)要素から式クラスを作成する
     *
     * @param node            UpperBound(範囲の上限)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(UpperBound node) throws XcodeMLException {

        // 子要素の取得
        IXmlNode nodeUpper = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(nodeUpper);

        return expr;
    }

    /**
     * Step(範囲のステップ)要素から式クラスを作成する
     *
     * @param node            Step(範囲のステップ)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Step node) throws XcodeMLException {

        // 子要素の取得
        IXmlNode nodeStep = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(nodeStep);

        return expr;
    }


    /**
     * ArrayIndex(インデックス値)要素から式クラスを作成する
     *
     * @param node            ArrayIndex(インデックス値)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(ArrayIndex node) throws XcodeMLException {

        // 子要素の取得
        IXmlNode nodeStep = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(nodeStep);

        return expr;
    }

    /**
     * NamedValue(インデックス値)要素から式クラスを作成する
     *
     * @param node            ArrayIndex(インデックス値)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(NamedValue node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // 名前
        String name = node.getName();
        // 値
        String value = node.getValue();

        // バッファ追加:名前
        buf.append(name);

        // バッファ追加:=
        buf.append(EXPR_EQUAL);

        // Expressionクラス生成
        Expression exprValue = null;
        if (value != null) {
            // バッファ追加:値
            buf.append(value);
        }
        else {
            // 子要素の取得
            IXmlNode nodeValue = XmlNodeUtil.getXmlNodeChoice(node);
            exprValue = getExpression(nodeValue);
            // バッファ追加:値
            buf.append(exprValue.getLine());
        }

        if (exprValue == null) return null;

        // キーワード引数
        KeywordArgument expr = new KeywordArgument(name, exprValue);
        // NamedValue:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * Value(式で表わされる任意の値)要素から式クラスを作成する
     *
     * @param node            Value(式で表わされる任意の値)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Value node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        RepeatCount repeat = node.getRepeatCount();
        Expression exprRepeat = null;
        if (repeat != null) {
            IXmlNode nodeRep = XmlNodeUtil.getXmlNodeChoice(repeat);
            exprRepeat = getExpression(nodeRep);
            // バッファ追加
            buf.append(exprRepeat.getLine());
            // バッファ追加 : *
            buf.append(EXPR_MUL); // *
        }

        IXmlNode nodeModel = XmlNodeUtil.getXmlNodeChoice(node);
        Expression exprModel = null;
        if (nodeModel != null) {
            exprModel = getExpression(nodeModel);
            // バッファ追加
            buf.append(exprModel.getLine());
        }

        // Expressionクラス生成
        Expression expr = mergeExpression(new Expression[]{exprRepeat, exprModel});

        // Value:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * FcaseLabel(SELECT CASE構文のCASE文)要素から式クラスを作成する
     *
     * @param node            FcaseLabel(SELECT CASE構文のCASE文)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FcaseLabel node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        buf.append("CASE");
        buf.append(EXPR_SPACE);

        List<IXmlNode> list = node.getValueOrIndexRange();
        List<Expression> exprs = new ArrayList<Expression>();
        if (list != null && list.size() > 0) {
            for (IXmlNode caseNode : list) {
                Expression exprCase = getExpression(caseNode);
                exprs.add(exprCase);

                // バッファ追加
                buf.append(exprCase.getLine());
                buf.append(EXPR_SPACE);
            }
        }

        // Expressionクラス生成
        Expression expr = null;
        if (exprs.size() > 0) {
            expr = mergeExpression(exprs.toArray(new Expression[0]));
        }
        else {
            expr = new Expression();
            buf.append("DEFAULT");
        }

        // CASE:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * Params要素から式クラスを作成する
     *
     * @param node            Params要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Params node) throws XcodeMLException {
        // 式バッファ
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
                // バッファ追加
                buf.append(nameNode.getValue());
                count++;
            }
        }

        // Expressionクラス生成
        Expression expr = new Expression();
        if (exprs.size() > 0) {
            expr = mergeExpression(exprs.toArray(new Expression[0]));
        }

        // パラメータ文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * VarList要素から式クラスを作成する
     *
     * @param node            VarList要素
     * @return 式クラスリスト
     * @throws XcodeMLException  パースエラー
     */
    public Expression getExpression(VarList node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        Expression[] list = getExpressionArray(node);
        if (list == null || list.length <= 0) return null;

        for (Expression expr : list) {
            int count = 0;
            if (count > 0) {
                // バッファ追加
                buf.append(EXPR_COMMA);        // カンマ
            }
            // バッファ追加
            buf.append(expr.getLine());
            count++;
        }

        // Expressionクラス生成
        Expression expr = mergeExpression(list);
        // パラメータ文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * ValueList要素から式クラスを作成する
     *
     * @param node            ValueList要素
     * @return 式クラスリスト
     * @throws XcodeMLException  パースエラー
     */
    public Expression getExpression(ValueList node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        Expression[] list = getExpressionArray(node);
        if (list == null || list.length <= 0) return null;

        for (Expression expr : list) {
            int count = 0;
            if (count > 0) {
                // バッファ追加
                buf.append(EXPR_COMMA);        // カンマ
            }
            // バッファ追加
            buf.append(expr.getLine());
            count++;
        }

        // Expressionクラス生成
        Expression expr = mergeExpression(list);
        // パラメータ文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * VarList要素から式クラスを作成する
     *
     * @param node            VarList要素
     * @return 式クラスリスト
     * @throws XcodeMLException  パースエラー
     */
    public Expression[] getExpressionArray(VarList node) throws XcodeMLException {
        // IXmlNodeリスト
        List<IXmlNode> vars = node.getVarRefOrFdoLoop();

        // 子要素のExpressionクラス
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
     * ValueList要素から式クラスを作成する
     *
     * @param node            ValueList要素
     * @return 式クラスリスト
     * @throws XcodeMLException  パースエラー
     */
    public Expression[] getExpressionArray(ValueList node) throws XcodeMLException {

        // Valueリスト
        List<Value> values = node.getValue();

        // 子要素のExpressionクラス
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
     * Expressionの集計を行う
     * @param exprs        Expressionリスト
     * @return                合計Expression
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
     *  変数データ型を取得する
     * @param typename        データタイプ名
     * @return            変数データ型
     */
    private VariableType getVariableType(String typename) {
        if (typename == null || typename.isEmpty()) return null;

        // 変数のパース
        VariableTypeParser typeParser = new VariableTypeParser(this.typeManager);
        VariableType varType = typeParser.parseVariableType(typename);

        return varType;
    }

    /**
     * 演算子を取得する
     * @param node        XMLノード
     * @return   演算子文字列
     */
    private String getOperation(IXmlNode node) {

        if (node instanceof PlusExpr) {
            // 加算
            return (EXPR_PLUS);
        }
        else if (node instanceof MinusExpr) {
            // 減算
            return (EXPR_MINUS);
        }
        else if (node instanceof MulExpr) {
            // 乗算
            return (EXPR_MUL);
        }
        else if (node instanceof DivExpr) {
            // 除算
            return (EXPR_DIV);
        }
        else if (node instanceof FpowerExpr) {
            // べき算
            return (EXPR_POWER);
        }
        else if (node instanceof LogLEExpr) {
            // logLEExpr <= .LE. : 小なり、または等価
            return (EXPR_LOGLE);
        }
        else if (node instanceof FconcatExpr) {
            // FconcatExpr // : 文字式の連結
            return (EXPR_CONCAT);
        }
        else if (node instanceof LogEQExpr) {
            // logEQExpr == .EQ. : 等価
            return (EXPR_LOGEQ);
        }
        else if (node instanceof LogNEQExpr) {
            // logNEQExpr /= .NE. : 非等価
            return (EXPR_LOGNEQ);
        }
        else if (node instanceof LogGEExpr) {
            // logGEExpr >= .GE. : 大なり、または同値
            return (EXPR_LOGGE);
        }
        else if (node instanceof LogGTExpr) {
            // logGTExpr > .GT. : 大なり
            return (EXPR_LOGGT);
        }
        else if (node instanceof LogLTExpr) {
            // logLTExpr < .LT. : 小なり
            return (EXPR_LOGLT);
        }
        else if (node instanceof LogAndExpr) {
            // logAndExpr .AND. : 論理積
            return (EXPR_LOGAND);
        }
        else if (node instanceof LogOrExpr) {
            // logOrExpr .OR. : 論理和
            return (EXPR_LOGOR);
        }
        else if (node instanceof LogEQVExpr) {
            // logEQVExpr .EQV. : 論理等価
            return (EXPR_LOGEQV);
        }
        else if (node instanceof LogNEQVExpr) {
            // logNEQVExpr .NEQV. : 論理非等価
            return (EXPR_LOGNEQV);
        }

        return "";
    }

    /**
     * 外部手続きリストに追加する
     * @param funcName        外部手続き名
     * @param varType        データ型
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
     * 外部手続きリストを取得する
     * @return        外部手続きリスト
     */
    public Map<String, IVariableType> getExternalFunction() {
        return this.externalFunctionList;
    }
}


