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
package jp.riken.kscope.xcodeml.clang.parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlElement;

import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.KeywordArgument;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.fortran.Structure;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.SclassAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.clang.EnumPrimitiveType;
import jp.riken.kscope.xcodeml.clang.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.IExpressions;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.clang.xml.gen.AddrOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArraySize;
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
import jp.riken.kscope.xcodeml.clang.xml.gen.BitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitField;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BuiltinOp;
import jp.riken.kscope.xcodeml.clang.xml.gen.CaseLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.CastExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayAssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.CommaExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundLiteral;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValue;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValueAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValueExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CondExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Condition;
import jp.riken.kscope.xcodeml.clang.xml.gen.DesignatedValue;
import jp.riken.kscope.xcodeml.clang.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.FloatConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.FuncAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Function;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccAlignOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccAsmOperand;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccAttribute;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccCompoundExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccLabelAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccMemberDesignator;
import jp.riken.kscope.xcodeml.clang.xml.gen.GotoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.IndexRange;
import jp.riken.kscope.xcodeml.clang.xml.gen.Init;
import jp.riken.kscope.xcodeml.clang.xml.gen.IntConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.Iter;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogEQExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogGEExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogGTExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogLEExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogLTExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogNEQExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LonglongConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.LowerBound;
import jp.riken.kscope.xcodeml.clang.xml.gen.LshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MoeConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.Params;
import jp.riken.kscope.xcodeml.clang.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ReturnStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.RshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.SizeOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Step;
import jp.riken.kscope.xcodeml.clang.xml.gen.StringConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.SubArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.TypeName;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnaryExpression;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnaryMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.UpperBound;
import jp.riken.kscope.xcodeml.clang.xml.gen.Value;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.XmpDescOf;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;



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
    /** modExpr % : 除算 */
    private final String EXPR_MOD = "%";
    /** lshiftExpr << : 左シフト */
    private final String EXPR_LSHIFT =  "<<";
    /** rshiftExpr >> : 右シフト */
    private final String EXPR_RSHIFT =  ">>";
    /** bitAndExpr & : ビット論理積 */
    private final String EXPR_BITAND = "&";
    /** bitOrExpr & : ビット論理和 */
    private final String EXPR_BITOR = "|";
    /** bitXorExpr & : ビット論理排他和 */
    private final String EXPR_BITXOR = "^";
    /** bitXorExpr & : ビット反転 */
    private final String EXPR_BITNOT = "~";
    /** logEQExpr == .EQ. : 等価 */
    private final String EXPR_LOGEQ = "==";
    /** logNEQExpr /= .NE. : 非等価 */
    private final String EXPR_LOGNEQ = "!=";
    /** logGEExpr >= .GE. : 大なり、または同値 */
    private final String EXPR_LOGGE = ">=";
    /** logGTExpr > .GT. : 大なり */
    private final String EXPR_LOGGT = ">";
    /** logLEExpr <= .LE. : 小なり、または等価 */
    private final String EXPR_LOGLE = "<=";
    /** logLTExpr < .LT. : 小なり */
    private final String EXPR_LOGLT = "<";
    /** logAndExpr .AND. : 論理積 */
    private final String EXPR_LOGAND = "&&";
    /** logOrExpr .OR. : 論理和 */
    private final String EXPR_LOGOR = "||";
    /** unaryMinusExpr - : 符号反転 */
    private final String EXPR_UNARYMINUS = "-";
    /** logNotExpr .NOT. : 論理否定 */
    private final String EXPR_LOGNOT = "!";
    /** postIncrExpr,preIncrExpr ++ : インクリメント */
    private final String EXPR_INCREMENT = "++";
    /** postDecrExpr,preDecrExpr ++ : デクリメント */
    private final String EXPR_DECREMENT = "--";
    /** 左小括弧 ( : 関数、文*/
    private final String EXPR_PARENLEFT = "(";
    /** 右小括弧 ) : 関数、文*/
    private final String EXPR_PARENRIGHT = ")";
    /** MemberRef % : 構造体メンバ */
    @SuppressWarnings("unused")
    private final String EXPR_STRUCTURE_REF = "->";
    private final String EXPR_STRUCTURE_MEM = ".";
    /** 左大括弧 [ */
    private final String EXPR_ARRAYLEFT = "[";
    /** 右大括弧 ] */
    private final String EXPR_ARRAYRIGHT = "]";
    /** 等号記号 = */
    private final String EXPR_EQUAL = "=";
    /** スペース */
    private final String EXPR_SPACE = " ";
    /** ArrayRef 配列区切りコロン : */
    private final String EXPR_ARRAYCOLON = ":";
    /** ポインタ * */
    private final String EXPR_POINTER = "*";
    /** 左中括弧 [ */
    private final String EXPR_COMPOUNDLEFT = "{";
    /** 右中括弧 ] */
    private final String EXPR_COMPOUNDRIGHT = "}";
    /** NULL */
    private final String EXPR_NULL = "NULL";


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
     * @return		パースIXmlNode要素
     */
    public IXmlNode getParseNode() {
        return parseNode;
    }

    /**
     * パースIXmlNode要素を設定する
     * @param parseNode		パースIXmlNode要素
     */
    public void setParseNode(IXmlNode parseNode) {
        this.parseNode = parseNode;
    }

    /**
     * typeTableを取得する
     * @return		typeTable
     */
    public XcodeMLTypeManager getTypeManager() {
        return typeManager;
    }

    /**
     * typeTableを設定する
     * @param typeManager		typeTable
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
     * @param node            IXmlNode要素 : IExpression要素を子要素に持つ要素
     * @return 式クラスオブジェクト
     * @throws XcodeMLException   パースエラー
     */
    public Expression getExpression(IXmlNode node) throws XcodeMLException {
        if (node == null) return null;

        Expression expr = null;
        if (node instanceof Arguments)   expr = getExpression((Arguments) node);
        else if (node instanceof TypeName) expr = getExpression((TypeName) node);
        else if (node instanceof GccMemberDesignator) expr = getExpression((GccMemberDesignator) node);
        else if (node instanceof Value)  expr = getExpression((Value) node);
        else if (node instanceof CompoundLiteral)  expr = getExpression((CompoundLiteral) node);

        /*
         * IExpression要素
         * intConstant
         * floatConstant
         * longlongConstant
         * stringConstant
         * moeConstant
         * funcAddr
         * pointerRef
         * Var
         * varAddr
         * arrayRef
         * arrayAddr
         * memberAddr
         * memberRef
         * memberArrayRef
         * memberArrayAddr
         * assignExpr
         * plusExpr
         * minusExpr
         * mulExpr
         * divExpr
         * modExpr
         * LshiftExpr
         * RshiftExpr
         * bitAndExpr
         * bitOrExpr
         * bitXorExpr
         * asgPlusExpr
         * asgMinusExpr
         * asgMulExpr
         * asgDivExpr
         * asgModExpr
         * asgLshiftExpr
         * asgRshiftExpr
         * asgBitAndExpr
         * asgBitOrExpr
         * asgBitXorExpr
         * logEQExpr
         * logNEQExpr
         * logGEExpr
         * logGTExpr
         * logLEExpr
         * logLTExpr
         * logAndExpr
         * logOrExpr
         * unaryMinusExpr
         * bitNotExpr
         * logNotExpr
         * functionCall
         * commaExpr
         * postIncrExpr
         * postDecrExpr
         * preIncrExpr
         * preDecrExpr
         * castExpr
         * condExpr
         * sizeOfExpr
         * addrOfExpr
         * xmpDescOf
         * compoundValueExpr
         * compoundValueAddrExpr
         * // gcc extension
         * gccAlignOfExpr
         * gccLabelAddr
         * gccCompoundExpr
         * builtin_op
         * // XcarableMP extension
         * subArrayRef
         * coArrayRef
         * coArrayAssignExpr
         */
        else if (node instanceof IntConstant)  expr = getExpression((IntConstant) node);
        else if (node instanceof FloatConstant)  expr = getExpression((FloatConstant) node);
        else if (node instanceof LonglongConstant)  expr = getExpression((LonglongConstant) node);
        else if (node instanceof StringConstant)  expr = getExpression((StringConstant) node);
        else if (node instanceof MoeConstant)  expr = getExpression((MoeConstant) node);
        else if (node instanceof FuncAddr)  expr = getExpression((FuncAddr) node);
        else if (node instanceof PointerRef)  expr = getExpression((PointerRef) node);
        else if (node instanceof Var)  expr = getExpression((Var) node);
        else if (node instanceof VarAddr)  expr = getExpression((VarAddr) node);
        else if (node instanceof ArrayRef)  expr = getExpression((ArrayRef) node);
        else if (node instanceof ArrayAddr)  expr = getExpression((ArrayAddr) node);
        else if (node instanceof MemberAddr)  expr = getExpression((MemberAddr) node);
        else if (node instanceof MemberRef)  expr = getExpression((MemberRef) node);
        else if (node instanceof MemberArrayRef)  expr = getExpression((MemberArrayRef) node);
        else if (node instanceof MemberArrayAddr)  expr = getExpression((MemberArrayAddr) node);
        else if (node instanceof AssignExpr)  expr = getExpression((AssignExpr) node);
        else if (node instanceof PlusExpr)  expr = getExpression((PlusExpr) node);
        else if (node instanceof MinusExpr)  expr = getExpression((MinusExpr) node);
        else if (node instanceof MulExpr)  expr = getExpression((MulExpr) node);
        else if (node instanceof DivExpr)  expr = getExpression((DivExpr) node);
        else if (node instanceof ModExpr)  expr = getExpression((ModExpr) node);
        else if (node instanceof LshiftExpr)  expr = getExpression((LshiftExpr) node);
        else if (node instanceof RshiftExpr)  expr = getExpression((RshiftExpr) node);
        else if (node instanceof BitAndExpr)  expr = getExpression((BitAndExpr) node);
        else if (node instanceof BitOrExpr)  expr = getExpression((BitOrExpr) node);
        else if (node instanceof BitXorExpr)  expr = getExpression((BitXorExpr) node);
        else if (node instanceof AsgPlusExpr)  expr = getExpression((AsgPlusExpr) node);
        else if (node instanceof AsgMinusExpr)  expr = getExpression((AsgMinusExpr) node);
        else if (node instanceof AsgMulExpr)  expr = getExpression((AsgMulExpr) node);
        else if (node instanceof AsgDivExpr)  expr = getExpression((AsgDivExpr) node);
        else if (node instanceof AsgModExpr)  expr = getExpression((AsgModExpr) node);
        else if (node instanceof AsgLshiftExpr)  expr = getExpression((AsgLshiftExpr) node);
        else if (node instanceof AsgRshiftExpr)  expr = getExpression((AsgRshiftExpr) node);
        else if (node instanceof AsgBitAndExpr)  expr = getExpression((AsgBitAndExpr) node);
        else if (node instanceof AsgBitOrExpr)  expr = getExpression((AsgBitOrExpr) node);
        else if (node instanceof AsgBitXorExpr)  expr = getExpression((AsgBitXorExpr) node);
        else if (node instanceof LogEQExpr)  expr = getExpression((LogEQExpr) node);
        else if (node instanceof LogNEQExpr)  expr = getExpression((LogNEQExpr) node);
        else if (node instanceof LogGEExpr)  expr = getExpression((LogGEExpr) node);
        else if (node instanceof LogGTExpr)  expr = getExpression((LogGTExpr) node);
        else if (node instanceof LogLEExpr)  expr = getExpression((LogLEExpr) node);
        else if (node instanceof LogLTExpr)  expr = getExpression((LogLTExpr) node);
        else if (node instanceof LogAndExpr)  expr = getExpression((LogAndExpr) node);
        else if (node instanceof LogOrExpr)  expr = getExpression((LogOrExpr) node);
        else if (node instanceof UnaryMinusExpr)  expr = getExpression((UnaryMinusExpr) node);
        else if (node instanceof BitNotExpr)  expr = getExpression((BitNotExpr) node);
        else if (node instanceof LogNotExpr)  expr = getExpression((LogNotExpr) node);
        else if (node instanceof FunctionCall)  expr = getExpression((FunctionCall) node);
        else if (node instanceof CommaExpr)  expr = getExpression((CommaExpr) node);
        else if (node instanceof PostIncrExpr)  expr = getExpression((PostIncrExpr) node);
        else if (node instanceof PostDecrExpr)  expr = getExpression((PostDecrExpr) node);
        else if (node instanceof PreIncrExpr)  expr = getExpression((PreIncrExpr) node);
        else if (node instanceof PreDecrExpr)  expr = getExpression((PreDecrExpr) node);
        else if (node instanceof CastExpr)  expr = getExpression((CastExpr) node);
        else if (node instanceof CondExpr)  expr = getExpression((CondExpr) node);
        else if (node instanceof SizeOfExpr)  expr = getExpression((SizeOfExpr) node);
        else if (node instanceof AddrOfExpr)  expr = getExpression((AddrOfExpr) node);
        else if (node instanceof XmpDescOf)  expr = getExpression((XmpDescOf) node);
        else if (node instanceof CompoundValueExpr)  expr = getExpression((CompoundValueExpr) node);
        else if (node instanceof CompoundValueAddr)  expr = getExpression((CompoundValueAddr) node);
        else if (node instanceof GccAlignOfExpr)  expr = getExpression((GccAlignOfExpr) node);
        else if (node instanceof GccLabelAddr)  expr = getExpression((GccLabelAddr) node);
        else if (node instanceof BuiltinOp)  expr = getExpression((BuiltinOp) node);
        else if (node instanceof SubArrayRef)  expr = getExpression((SubArrayRef) node);
        else if (node instanceof CoArrayRef)  expr = getExpression((CoArrayRef) node);
        else if (node instanceof CoArrayAssignExpr)  expr = getExpression((CoArrayAssignExpr) node);
        else if (node instanceof DesignatedValue)  expr = getExpression((DesignatedValue) node);
        else if (node instanceof CompoundValue)  expr = getExpression((CompoundValue) node);
        else if (node instanceof IndexRange)  expr = getExpression((IndexRange) node);
        // IExpressions
        else if (node instanceof IExpressions)  expr = getExpression((IExpressions) node);


        return expr;
    }

    /**
     * FunctionCall(関数呼出)要素から式クラスを作成する
     * @param node            FunctionCall(関数呼出)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FunctionCall node) throws XcodeMLException {
        if (node == null) return null;
        if (node.getFunction() == null) return null;

        // データ型パーサ
        VariableTypeParser typePaser = new VariableTypeParser(this.typeManager, null);

        // 関数呼出
        IXmlNode def = XmlNodeUtil.getXmlNodeChoice(node.getFunction());
        Expression func_expr = this.getExpression(def);
        if (func_expr == null) {
            throw new XcodeMLException("can not get function call node[null].");
        }
        if (func_expr.getFuncCalls() == null || func_expr.getFuncCalls().size() <= 0) {
            VariableType type = (VariableType)func_expr.getType();
            if (type.isFunction() && !StringUtils.isNullOrEmpty(func_expr.getLine())) {
                // 関数の参照呼出
                ProcedureUsage proc = new ProcedureUsage(func_expr.getLine(), null);
                func_expr.addFuncCall(proc);
            }
            else {
                throw new XcodeMLException("can not get function call node.");
            }
        }

        if (func_expr.getFuncCalls().size() > 1) {
            throw new XcodeMLException("multiple function calls.[function name=" + func_expr.getLine() + "]");
        }
        ProcedureUsage func_proc = func_expr.getFuncCalls().get(0);
        String func_name = func_proc.getCallName();
        VariableType func_vartype = null;
        SclassAttribute func_sclass = null;
        if (func_proc.getCallDefinition() != null) {
            func_vartype = (VariableType)func_proc.getCallDefinition().getReturnValueType();
            func_sclass = func_proc.getCallDefinition().getSclass();
        }

        // 実引数
        Expression[] arg_exprs = null;
        if (node.getArguments() != null) {
            arg_exprs = this.getExpressions(node.getArguments().getExpressions());
        }
        if (arg_exprs != null) {
            // 実引数の追加
            List<Expression> argslist = null;
            if (arg_exprs != null) {
                argslist = new ArrayList<Expression>(java.util.Arrays.asList(arg_exprs));
            }
            func_proc.setArguments(argslist);
        }

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // バッファ追加:関数名
        buf.append(func_expr.getLine());

        // バッファ追加:左括弧
        buf.append(EXPR_PARENLEFT);

        // 実引数の組み立てを行う
        if (arg_exprs != null) {
            for (int i=0; i<arg_exprs.length; i++) {
                Expression arg = arg_exprs[i];
                if (i > 0) buf.append(EXPR_COMMA);
                buf.append(arg.getLine());
            }
        }

        // バッファ追加:右括弧
        buf.append(EXPR_PARENRIGHT);

        // FunctionCall:式文字列
        func_expr.setLine(buf.toString());

        // 外部手続きリストに追加する
        if (func_name != null && func_vartype != null) {
            if (func_sclass != null && func_sclass == SclassAttribute.EXTERN) {
                addExternalFunction(func_name, func_vartype);
            }
        }
        return func_expr;
    }


    /**
     * Arguments(引数)要素から式クラスを作成する
     * @param node            Arguments(引数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(Arguments node) throws XcodeMLException {
        if (node == null) return null;
        if (node.getExpressions() == null) return null;

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // 仮引数の取得を行う
        Expression[] argsExpr = getExpressions(node.getExpressions());
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
     * コンパイラ組み込みの関数の引数:構造体・共用体のメンバ要素から式クラスを作成する.
     * ref属性:構造体参照、member属性は演算式には関係ないのでパースしない。
     * 配列式のみから式クラスを作成する.
     *
     * @param node			組込関数の引数:構造体・共用体のメンバ要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(GccMemberDesignator node) throws XcodeMLException {
        if (node == null) return null;

        Expression expr = new Expression();
        // ref属性 : 構造体
        String ref = node.getRef();
        VariableType ref_type = this.getVariableType(ref);
        expr.setVariableType(ref_type);

        // member属性
        String member = node.getMember();

        // 配列式
        IXmlNode expr_node = XmlNodeUtil.getXmlNodeChoice(node);
        Expression array_expr = this.getExpression(expr_node);
        if (array_expr != null) {
            expr.addVariables(array_expr.getVariables());
        }

        // gccMemberDesignator要素
        GccMemberDesignator gcc_node = node.getGccMemberDesignator();
        Expression gcc_expr = this.getExpression(gcc_node);
        if (gcc_expr != null) {
            expr.addVariables(gcc_expr.getVariables());
        }

        return expr;
    }

    /**
     * CompoundLiteral要素から式クラスを作成する
     * @param node            CompoundLiteral要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CompoundLiteral node) throws XcodeMLException {
        if (node == null) return null;

        // 実引数
        Expression[] exprs = null;
        if (node.getIntConstantOrFloatConstantOrLonglongConstant() != null) {
            exprs = this.getExpressions(node.getIntConstantOrFloatConstantOrLonglongConstant());
        }

        // 1つのExpressionの生成
        Expression expr = mergeExpression(exprs);

        return expr;
    }

    /**
     * 要素リストから式クラスを作成する
     * @param node            要素リスト
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    public Expression[] getExpressions(List<IXmlNode> nodes) throws XcodeMLException {
        if (nodes == null) return null;

        // 仮引数の取得を行う
        List<Expression> exprs = new ArrayList<Expression>();
        for (IXmlNode node : nodes) {
            Expression expr = getExpression(node);
            exprs.add(expr);
        }

        if (exprs.size() <= 0) return null;

        return exprs.toArray(new Expression[0]);
    }

    /**
     * BinaryExpression(二項演算式)要素から式クラスを作成する
     * 二項演算式要素
     *     assignExpr
     *     plusExpr
     *     minusExpr
     *     mulExpr
     *     divExpr
     *     modExpr
     *     LshiftExpr
     *     RshiftExpr
     *     bitAndExpr
     *     bitOrExpr
     *     bitXorExpr
     *     asgPlusExpr
     *     asgMinusExpr
     *     asgMulExpr
     *     asgDivExpr
     *     asgModExpr
     *     asgLshiftExpr
     *     asgRshiftExpr
     *     asgBitAndExpr
     *     asgBitOrExpr
     *     asgBitXorExpr
     *     logEQExpr
     *     logNEQExpr
     *     logGEExpr
     *     logGTExpr
     *     logLEExpr
     *     logLTExpr
     *     logAndExpr
     *     logOrExpr
     * @param node            BinaryExpression(二項演算式)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(BinaryExpression node) throws XcodeMLException {

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

        // 浮動小数点演算のみ対象とするように変更
        String attr = node.getType();
        // 変数のパース
        VariableType varType = getVariableType(node.getType());
        PrimitiveDataType data_type = varType.getPrimitiveDataType();
        if (data_type == PrimitiveDataType.FLOAT || data_type == PrimitiveDataType.DOUBLE) {
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
            }
        }
        expr.setVariableType(varType);

        // BinaryExpression:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * UnaryExpression(単項演算子)要素から式クラスを作成する
     * 単項演算子要素
     *     unaryMinusExpr
     *     bitNotExpr
     *     logNotExpr
     *     postIncrExpr
     *     postDecrExpr
     *     preIncrExpr
     *     preDecrExpr
     * @param node            UnaryExpression(単項演算子)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(UnaryExpression node) throws XcodeMLException {

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // バッファに追加:演算子
        String op = getOperation(node);
        if (!(node instanceof PostIncrExpr || node instanceof PostDecrExpr)) {
            buf.append(op);
        }

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);

        // バッファ追加
        buf.append(expr.getLine());
        // バッファに追加:演算子
        if (node instanceof PostIncrExpr || node instanceof PostDecrExpr) {
            buf.append(op);
        }

        // 変数のパース
        VariableType varType = getVariableType(node.getType());
        expr.setVariableType(varType);

        // UnaryMinusExpr:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * CastExpr(キャスト)要素から式クラスを作成する
     * @param node            CastExpr(キャスト)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CastExpr node) throws XcodeMLException {

        String type = node.getType();
        VariableType cast_type = this.getVariableType(type);

        // 式バッファ : キャストデータ型
        StringBuffer buf = new StringBuffer();
        buf.append(EXPR_PARENLEFT);
        buf.append(cast_type.getName());
        buf.append(EXPR_PARENRIGHT);

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);

        // バッファ追加
        buf.append(expr.getLine());

        // キャストデータ型の設定
        expr.setVariableType(cast_type);

        // NULLであるかチェックする
        boolean is_null = false;
        if (cast_type != null && cast_type.isVoid() && cast_type.isPointer()) {
            if (child instanceof IntConstant) {
                String value = ((IntConstant)child).getValue();
                if ("0".equals(value)) {
                    is_null = true;
                }
            }
        }
        // CastExpr:式文字列
        if (is_null) {
            expr.setLine(EXPR_NULL);
        }
        else {
            expr.setLine(buf.toString());
        }

        return expr;
    }

    /**
     * CondExpr(３項演算子)要素から式クラスを作成する
     * @param node            CondExpr(３項演算子)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CondExpr node) throws XcodeMLException {

        String type = node.getType();
        VariableType cond_type = this.getVariableType(type);

        // 3項演算子
        List<IXmlNode> contents = node.getContent();
        if (contents == null || contents.size() < 3) return null;

        IXmlNode _1st_node = contents.get(0);
        IXmlNode _2nd_node = contents.get(1);
        IXmlNode _3rd_node = contents.get(2);
        Expression _1st_expr = getExpression(_1st_node);
        Expression _2nd_expr = getExpression(_2nd_node);
        Expression _3rd_expr = getExpression(_3rd_node);

        // 式バッファ : キャストデータ型
        StringBuffer buf = new StringBuffer();
        buf.append(_1st_expr.getLine());
        buf.append(" ? ");
        buf.append(_2nd_expr.getLine());
        buf.append(" : ");
        buf.append(_3rd_expr.getLine());

        Expression exprs[] = {_1st_expr, _2nd_expr, _3rd_expr};
        Expression expr = mergeExpression(exprs);

        // キャストデータ型の設定
        expr.setVariableType(cond_type);

        // CastExpr:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * SizeOfExpr(sizeof演算子)要素から式クラスを作成する
     * @param node            SizeOfExpr(sizeof演算子)
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(SizeOfExpr node) throws XcodeMLException {

        String type = node.getType();

        Expression expr = null;
        StringBuffer buf = new StringBuffer();
        buf.append("sizeof");
        buf.append(EXPR_PARENLEFT);

        // sizeof
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        if (child != null) {
            expr = getExpression(child);
            buf.append(expr.getLine());
        }
        else if (node.getTypeName() != null) {
            TypeName type_node = node.getTypeName();
            String type_name = type_node.getType();
            VariableType size_type = this.getVariableType(type_name);
            if (size_type != null) {
                expr = new Expression();
                buf.append(size_type.toStringFull());
            }
        }
        if (expr == null) return null;

        // 式バッファ
        buf.append(EXPR_PARENRIGHT);

        // sizeofデータ型の設定
        VariableType sizeof_type = this.getVariableType(type);
        expr.setVariableType(sizeof_type);

        // sizeof:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * AddrOfExpr(アドレス演算子)要素から式クラスを作成する
     * @param node            AddrOfExpr(アドレス演算子)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(AddrOfExpr node) throws XcodeMLException {

        String type = node.getType();

        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        if (child == null) return null;
        Expression expr = getExpression(child);
        if (expr == null) return null;

        StringBuffer buf = new StringBuffer();
        buf.append("&");
        buf.append(expr.getLine());
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * GccAlignOfExpr(GCC:__alignof__演算子)要素から式クラスを作成する
     * @param node            GccAlignOfExpr(GCC:__alignof__演算子)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(GccAlignOfExpr node) throws XcodeMLException {

        String type = node.getType();

        Expression expr = null;
        StringBuffer buf = new StringBuffer();
        buf.append("__alignof__");
        buf.append(EXPR_PARENLEFT);

        // __alignof__
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        if (child != null) {
            expr = getExpression(child);
            buf.append(expr.getLine());
        }
        else if (node.getTypeName() != null) {
            TypeName type_node = node.getTypeName();
            String type_name = type_node.getType();
            VariableType size_type = this.getVariableType(type_name);
            expr = new Expression();
            buf.append(type_name);
        }
        if (expr == null) return null;

        // 式バッファ
        buf.append(EXPR_PARENRIGHT);

        // sizeofデータ型の設定
        VariableType sizeof_type = this.getVariableType(type);
        expr.setVariableType(sizeof_type);

        // sizeof:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * GccLabelAddr(GCC:&&単項演算子)要素から式クラスを作成する
     * @param node            GccLabelAddr(GCC:&&単項演算子)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(GccLabelAddr node) throws XcodeMLException {

        String value = node.getValue();
        String type = node.getType();

        StringBuffer buf = new StringBuffer();
        buf.append("&&");
        buf.append(value);

        Expression expr = new Expression(buf.toString());

        return expr;
    }

    /**
     * XmpDescOf要素から式クラスを作成する
     * @param node            XmpDescOf要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(XmpDescOf node) throws XcodeMLException {
        if (node == null) return null;

        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);

        return expr;
    }


    /**
     * IExpressions(式)要素から式クラスを作成する
     * @param node            IExpressions(式)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(IExpressions node) throws XcodeMLException {
        if (node == null) return null;

        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(child);

        return expr;
    }

    /**
     * memberRef(構造体メンバ参照)要素から式クラスを作成する
     *
     * @param node            memberRef(構造体メンバ参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(MemberRef node) throws XcodeMLException {

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Expression structure_expr = getExpression(structure_node);

        // 構造体メンバ
        String member = node.getMember();

        // データ型
        String type = node.getType();
        VariableType varType = getVariableType(type);

        // Expressionクラス生成
        structure_expr.setVariableType(varType);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // 式バッファ
        StringBuffer buf = new StringBuffer();
        buf.append(structure_expr.getLine());
        buf.append(EXPR_STRUCTURE_MEM);
        buf.append(member);

        // 構造体参照の設定
        if (var != null) {
            structure_expr.addVariable(var);
        }

        // MemberRef:式文字列
        structure_expr.setLine(buf.toString());

        return structure_expr;
    }

    /**
     * MemberArrayAddr(構造体配列メンバ参照)要素から式クラスを作成する
     *
     * @param node            MemberArrayAddr(構造体配列メンバ参照)
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(MemberArrayAddr node) throws XcodeMLException {

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Expression structure_expr = getExpression(structure_node);

        // 構造体メンバ
        String member = node.getMember();

        // データ型
        String type = node.getType();
        VariableType varType = getVariableType(type);

        // Expressionクラス生成
        structure_expr.setVariableType(varType);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // 式バッファ
        StringBuffer buf = new StringBuffer();
        buf.append(structure_expr.getLine());
        buf.append(EXPR_STRUCTURE_MEM);
        buf.append(member);

        // 構造体参照の設定
        if (var != null) {
            structure_expr.addVariable(var);
        }

        // MemberArrayAddr:式文字列
        structure_expr.setLine(buf.toString());

        return structure_expr;
    }


    /**
     * MemberArrayRef(構造体配列メンバ参照)要素から式クラスを作成する
     *
     * @param node            MemberArrayRef(構造体配列メンバ参照)
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(MemberArrayRef node) throws XcodeMLException {

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Expression structure_expr = getExpression(structure_node);

        // 構造体メンバ
        String member = node.getMember();

        // データ型
        String type = node.getType();
        VariableType varType = getVariableType(type);

        // Expressionクラス生成
        structure_expr.setVariableType(varType);

        // 変数のパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // 式バッファ
        StringBuffer buf = new StringBuffer();
        buf.append(structure_expr.getLine());
        buf.append(EXPR_STRUCTURE_MEM);
        buf.append(member);

        // 構造体参照の設定
        if (var != null) {
            structure_expr.addVariable(var);
        }

        // MemberArrayRef:式文字列
        structure_expr.setLine(buf.toString());

        return structure_expr;
    }


    /**
     * BuiltinOp(コンパイラ組込関数呼出)要素から式クラスを作成する
     *
     * @param node           BuiltinOp(コンパイラ組込関数呼出)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(BuiltinOp node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String op_name = node.getName();
        String type = node.getType();
        buf.append(op_name);
        buf.append(EXPR_PARENLEFT);

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 呼出関数実引数の取得
        List<IXmlNode> arg_nodes = node.getIntConstantOrFloatConstantOrLonglongConstant();
        List<Expression> list = new ArrayList<Expression>();
        if (arg_nodes != null && arg_nodes.size() > 0) {
            for (int i=0; i<arg_nodes.size(); i++) {
                // 式 : 呼び出す関数の引数として、式を指定する。
                IXmlNode arg = arg_nodes.get(i);
                if (i > 0) {
                    // バッファ追加
                    buf.append(EXPR_COMMA);       // カンマ
                }
                Expression expr = getExpression(arg);
                list.add(expr);

                // typeName － 呼び出す関数の引数として、型名を指定する。
                if (arg_nodes.get(i) instanceof TypeName) {
                    TypeName arg_type = (TypeName)arg_nodes.get(i);
                    String arg_typename = arg_type.getType();
                    VariableType arg_vartype = getVariableType(arg_typename);
                    expr.setVariableType(arg_vartype);
                }
                // gccMemberDesignator － 呼び出す関数の引数として、構造体・共用体のメンバ指示子を指定する
                if (arg_nodes.get(i) instanceof GccMemberDesignator) {
                    GccMemberDesignator member = (GccMemberDesignator)arg_nodes.get(i);
                    Expression mem_expr = getExpression(member);
                    expr.setVariableType(mem_expr.getType());
                    expr.addVariables(mem_expr.getVariables());
                }

                // バッファ追加
                buf.append(expr.getLine());       // 引数
            }
        }
        // バッファ追加: )
        buf.append(EXPR_PARENRIGHT);

        Expression expr = null;
        if (list.size() > 0) {
            expr = mergeExpression(list.toArray(new Expression[0]));
        }
        if (expr != null) {
            expr.setVariableType(varType);
            // BuiltinOp:式文字列
            expr.setLine(buf.toString());
        }

        return expr;
    }

    /**
     * SubArrayRef(部分配列の参照)要素から式クラスを作成する
     *
     * @param node            SubArrayRef(部分配列の参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(SubArrayRef node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // arrayAddr
        ArrayAddr addr_node = node.getArrayAddrInSubArrayRef();
        Expression addr_expr = getExpression(addr_node);

        // インデックス
        List<IXmlNode> list = node.getIndexRangeOrIntConstantOrFloatConstant();
        Expression[] index_exprs = getExpressions(list);

        buf.append(addr_expr.getLine());
        buf.append(EXPR_ARRAYLEFT);
        if (index_exprs != null) {
            int count = 0;
            for (Expression expr : index_exprs) {
                if (count > 0) {
                    buf.append(EXPR_ARRAYCOLON);
                }
                List<Variable> vars = expr.getVariables();
                addr_expr.addVariables(vars);
                buf.append(addr_expr.getLine());
                count++;
            }
        }
        buf.append(EXPR_ARRAYRIGHT);

        return addr_expr;
    }


    /**
     * CoArrayRef(coarrayの参照)要素から式クラスを作成する
     *
     * @param node            CoArrayRef(coarrayの参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CoArrayRef node) throws XcodeMLException {
        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        // 変数の取得
        Var var_node = node.getVarInCoArrayRef();
        ArrayRef array_node = node.getArrayRefInCoArrayRef();
        SubArrayRef sub_node = node.getSubArrayRefInCoArrayRef();
        MemberRef member_node = node.getMemberRefInCoArrayRef();
        Expression expr = null;
        if (var_node != null) expr = this.getExpression(var_node);
        else if (array_node != null) expr = this.getExpression(array_node);
        else if (sub_node != null) expr = this.getExpression(sub_node);
        else if (member_node != null) expr = this.getExpression(member_node);
        if (expr == null) return null;

        // 配列式
        List<IXmlNode> index_nodes = node.getExpressions();
        Expression[] array_exprs = this.getExpressions(index_nodes);

        // バッファ追加
        buf.append(expr.getLine());
        if (array_exprs != null) {
            for (Expression array : array_exprs) {
                // バッファ追加: [  配列  ]
                buf.append(EXPR_ARRAYLEFT);
                buf.append(array.getLine());
                buf.append(EXPR_ARRAYRIGHT);
            }
            Expression exprs = mergeExpression(array_exprs);
            expr = mergeExpression(new Expression[]{expr, exprs});
        }
        expr.setVariableType(varType);

        // FcoArrayRef:式文字列
        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * CoArrayAssignExpr(coarrayの参照)要素から式クラスを作成する
     *
     * @param node            CoArrayAssignExpr(coarrayの参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CoArrayAssignExpr node) throws XcodeMLException {

        String type = node.getType();
        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        List<IXmlNode> content_nodes = node.getContent();

        List<IXmlNode> contents = node.getContent();
        if (contents == null || contents.size() < 2) return null;

        IXmlNode _1st_node = contents.get(0);
        IXmlNode _2nd_node = contents.get(1);
        Expression _1st_expr = getExpression(_1st_node);
        Expression _2nd_expr = getExpression(_2nd_node);

        Expression exprs[] = {_1st_expr, _2nd_expr};
        Expression expr = mergeExpression(exprs);

        // キャストデータ型の設定
        expr.setVariableType(varType);

        // 式バッファ
        StringBuffer buf = new StringBuffer();
        buf.append(_1st_expr.getLine());
        buf.append(EXPR_EQUAL);
        buf.append(_2nd_expr.getLine());

        expr.setLine(buf.toString());

        return expr;
    }


    /**
     * ArrayRef(配列の先頭要素)要素から式クラスを作成する.
     * @param node            ArrayRef(配列の先頭要素)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(ArrayRef node) throws XcodeMLException {
        if (node == null) return null;

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        // 変数名
        ArrayAddr array_var = node.getArrayAddrInArrayRef();
        Expression var_expr = this.getExpression(array_var);

        // 配列式
        List<IXmlNode> array_dims = node.getExpressions();
        Expression[] array_expr = this.getExpressions(array_dims);

        // 変数データ型のパース
        if (var_expr.getType() == null) {
            String type = node.getType();
            VariableType varType = getVariableType(type);
            if (varType != null) {
                var_expr.setVariableType(varType);
            }
        }

        // バッファ追加:変数名
        buf.append(array_var.getValue());

        if (array_expr != null) {
            for (Expression expr : array_expr) {
                // バッファ追加:左括弧
                buf.append(EXPR_ARRAYLEFT); // 左括弧 [
                // バッファ追加:配列
                buf.append(expr.getLine());
                // バッファ追加:右括弧
                buf.append(EXPR_ARRAYRIGHT);    // 右括弧 ]
            }
        }
        // ArrayRef:式文字列
        var_expr.setLine(buf.toString());

        // Expressionクラス生成
        if (array_expr != null) {
            for (Expression expr : array_expr) {
                List<Variable> vars = expr.getVariables();
                var_expr.addVariables(vars);
            }
        }

        return var_expr;
    }


    /**
     * ArrayAddr(配列アドレス参照)要素から式クラスを作成する.
     * @param node            ArrayAddr(配列アドレス参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(ArrayAddr node) throws XcodeMLException {
        if (node == null) return null;

        // 変数名
        String name = node.getValue();
        // データ型
        String type = node.getType();
        VariableType varType = getVariableType(type);

        // 式クラス
        Expression expr = new Expression(name);
        expr.setVariableType(varType);

        return expr;
    }


    /**
     * MemberAddr(構造体メンバアドレス参照)要素から式クラスを作成する.
     * @param node            MemberAddr(構造体メンバアドレス参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(MemberAddr node) throws XcodeMLException {
        if (node == null) return null;

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Expression structure_expr = getExpression(structure_node);

        // 構造体メンバ
        String member = node.getMember();

        // データ型
        String type = node.getType();
        VariableType varType = getVariableType(type);

        // 構造体メンバのパース
        VariableParser varParser = new VariableParser(this.typeManager);
        Variable var = varParser.getVariable(node);

        // 式バッファ
        StringBuffer buf = new StringBuffer();
        buf.append(structure_expr.getLine());
        buf.append(EXPR_STRUCTURE_MEM);
        buf.append(member);

        // 構造体参照の設定
        if (var != null) {
            structure_expr.addVariable(var);
        }

        // MemberAddr:式文字列
        structure_expr.setLine(buf.toString());

        return structure_expr;
    }


    /**
     * LonglongConstant(32ビット16進数)要素から式クラスを作成する
     *
     * @param node           LonglongConstant(32ビット16進数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(LonglongConstant node) throws XcodeMLException {
        if (node == null) return null;

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

        // LonglongConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * StringConstant(文字列定数)要素から式クラスを作成する
     *
     * @param node           StringConstant(文字列定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(StringConstant node) throws XcodeMLException {
        if (node == null) return null;

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

        // StringConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * IntConstant(整数型定数)要素から式クラスを作成する
     *
     * @param node            FintConstant(整数型定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(IntConstant node) throws XcodeMLException {
        if (node == null) return null;

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

        // IntConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }



    /**
     * FloatConstant(浮動小数点数)要素から式クラスを作成する
     *
     * @param node            FloatConstant(浮動小数点数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FloatConstant node) throws XcodeMLException {
        if (node == null) return null;

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

        // FloatConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * MoeConstant(enum 定数)要素から式クラスを作成する
     *
     * @param node            MoeConstant(enum 定数)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(MoeConstant node) throws XcodeMLException {
        if (node == null) return null;

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

        // FloatConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * FuncAddr(関数アドレス)要素から式クラスを作成する
     *
     * @param node            FuncAddr(関数アドレス)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(FuncAddr node) throws XcodeMLException {
        if (node == null) return null;

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 関数データ型のパース
        VariableType varType = getVariableType(type);

        // 関数名
        String func_name = node.getValue();

        // バッファ追加：値
        buf.append(func_name);

        // Expressionクラス生成
        Expression expr = new Expression(func_name);
        expr.setVariableType(varType);
        // 関数呼出クラス：仮引数はnull(後で設定する)
        ProcedureUsage proc = new ProcedureUsage(func_name, null);
        expr.addFuncCall(proc);

        // FloatConstant:式文字列
        expr.setLine(buf.toString());

        return expr;
    }

    /**
     * PointerRef(メモリ参照)要素から式クラスを作成する
     *
     * @param node           PointerRef(メモリ参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(PointerRef node) throws XcodeMLException {
        if (node == null) return null;

        // 式バッファ
        StringBuffer buf = new StringBuffer();

        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        IXmlNode unary_expr = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = getExpression(unary_expr);

        boolean is_group = true;
        if (unary_expr instanceof Var) {
            is_group = false;
        }
        buf.append(EXPR_POINTER);
        if (is_group) buf.append(EXPR_PARENLEFT);
        buf.append(expr.getLine());
        if (is_group) buf.append(EXPR_PARENRIGHT);

        // Expressionクラス生成
        expr.setVariableType(varType);

        // FloatConstant:式文字列
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
        VariableTypeParser typeParser = new VariableTypeParser(this.typeManager, null);
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
        expr.setLine(var.getName());

        return expr;
    }

    /**
     * TypeName(データ型名)要素から式クラスを作成する
     *
     * @param node            TypeName(変数名)
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(TypeName node) throws XcodeMLException {

        // 変数データ型のパース
        VariableTypeParser typeParser = new VariableTypeParser(this.typeManager, null);
        VariableType varType = typeParser.parseVariableType(node.getType());

        // Expressionクラス生成
        Expression expr = new Expression();

        expr.setVariableType(varType);

        // Var:式文字列
        expr.setLine(varType.toStringClang());

        return expr;
    }

    /**
     * VarAddr(変数参照)要素から式クラスを作成する
     *
     * @param node            VarAddr(変数参照)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(VarAddr node) throws XcodeMLException {
        if (node == null) return null;

        // 変数名
        String name = node.getValue();

        // 変数データ型
        String type = node.getType();

        // 変数データ型のパース
        VariableType varType = getVariableType(type);

        Expression expr = new Expression(name);

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

        // IndexRangeの範囲
        LowerBound lowerBound = node.getLowerBound();
        UpperBound upperBound = node.getUpperBound();
        Step step = node.getStep();

        Expression lower_expr = this.getExpression((IExpressions)lowerBound);
        Expression upper_expr = this.getExpression((IExpressions)upperBound);
        Expression step_expr = this.getExpression((IExpressions)step);

        // 式バッファ
        StringBuffer buf = new StringBuffer();
        if (lower_expr != null) {
            buf.append(lower_expr.getLine());
        }
        if (upper_expr != null) {
            if (buf.length() > 0) buf.append(EXPR_ARRAYCOLON);
            buf.append(upper_expr.getLine());
        }
        if (step_expr != null) {
            if (buf.length() > 0) buf.append(EXPR_ARRAYCOLON);
            buf.append(step_expr.getLine());
        }

        // Expressionクラス生成
        Expression expr = mergeExpression(new Expression[]{lower_expr, upper_expr, step_expr});

        // IndexRange:式文字列
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
        // IXmlNode nodeModel = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = null;
        if (node.getExpressionsOrValues() != null) {
            List<IXmlNode> list = node.getExpressionsOrValues();
            StringBuffer buf = new StringBuffer();
            Expression exprs[] = getExpressions(list);
            if (exprs != null) {
                if (list.size() > 1) {
                    buf.append(EXPR_COMPOUNDLEFT);
                }
                for (int i=0; i<exprs.length; i++) {
                    if (i != 0) buf.append(EXPR_COMMA);
                    buf.append(exprs[i].getLine());
                }
                if (list.size() > 1) {
                    buf.append(EXPR_COMPOUNDRIGHT);
                }
                // 1つのExpressionの生成
                expr = mergeExpression(exprs);
                expr.setLine(buf.toString());
            }
        }
        else if (node.getDesignatedValue() != null) {
            expr = getExpression(node.getDesignatedValue());
        }

        return expr;
    }

    /**
     * DesignatedValue要素から式クラスを作成する
     *
     * @param node            DesignatedValue要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(DesignatedValue node) throws XcodeMLException {

        IXmlNode nodeModel = XmlNodeUtil.getXmlNodeChoice(node);
        Expression expr = null;
        if (nodeModel != null) {
            expr = getExpression(nodeModel);
        }
        else if (node.getDesignatedValue() != null) {
            expr = getExpression(node.getDesignatedValue());
        }
        else if (node.getValue() != null) {
            expr = getExpression(node.getValue() );
        }

        return expr;
    }

    /**
     * CompoundValue要素から式クラスを作成する
     *
     * @param node            CompoundValue要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CompoundValue node) throws XcodeMLException {

        CompoundValue.Value value = node.getValue();
        Expression expr = this.getExpression((CompoundLiteral)value);

        return expr;
    }


    /**
     * CommaExpr(コンマ式)要素から式クラスを作成する
     *
     * @param node            CommaExpr(コンマ式)要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CommaExpr node) throws XcodeMLException {

        Expression[] exprs = getExpressions(node.getExpressions());

        // 1つのExpressionの生成
        Expression expr = mergeExpression(exprs);

        return expr;
    }


    /**
     * CompoundValueExpr要素から式クラスを作成する
     *
     * @param node            CompoundValueExpr要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CompoundValueExpr node) throws XcodeMLException {

        Value value = node.getValue();
        Expression expr = getExpression(value);

        return expr;
    }


    /**
     * CompoundValueAddr要素から式クラスを作成する
     *
     * @param node            CompoundValueAddr要素
     * @return 式クラス
     * @throws XcodeMLException  パースエラー
     */
    private Expression getExpression(CompoundValueAddr node) throws XcodeMLException {

        Value value = node.getValue();
        Expression expr = getExpression(value);

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
     * Expressionの集計を行う
     * @param exprs		Expressionリスト
     * @return				合計Expression
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
     * @param typename		データタイプ名
     * @return			変数データ型
     */
    private VariableType getVariableType(String typename) {
        if (typename == null || typename.isEmpty()) return null;

        // 変数のパース
        VariableTypeParser typeParser = new VariableTypeParser(this.typeManager, null);
        VariableType varType = typeParser.parseVariableType(typename);

        return varType;
    }

    /**
     *  シンボル参照名:refからデータ型を取得する
     * @param ref		シンボル参照名
     * @return			データ型
     */
    private VariableType getVariableRef(String ref) {
        if (ref == null || ref.isEmpty()) return null;

        Id id = this.typeManager.findSymbol(ref);
        if (id == null) return null;

        // 変数のパース
        VariableTypeParser typeParser = new VariableTypeParser(this.typeManager, null);

        VariableType varType = typeParser.parseVariableType(id);

        return varType;
    }

    /**
     * 演算子を取得する
     * @param node		XMLノード
     * @return   演算子文字列
     */
    private String getOperation(IXmlNode node) {

        // ２項演算子
        if (node instanceof AssignExpr) {
            // 代入
            return (EXPR_EQUAL);
        }
        else if (node instanceof PlusExpr) {
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
        else if (node instanceof ModExpr) {
            // 剰余
            return (EXPR_MOD);
        }
        else if (node instanceof LshiftExpr) {
            // 左シフト
            return (EXPR_LSHIFT);
        }
        else if (node instanceof RshiftExpr) {
            // 右シフト
            return (EXPR_RSHIFT);
        }
        else if (node instanceof BitAndExpr) {
            // ビット論理積
            return (EXPR_BITAND);
        }
        else if (node instanceof BitOrExpr) {
            // ビット論理和
            return (EXPR_BITOR);
        }
        else if (node instanceof BitXorExpr) {
            // ビット論理排他和
            return (EXPR_BITXOR);
        }
        else if (node instanceof AsgPlusExpr) {
            // 加算代入演算子
            return (EXPR_PLUS + EXPR_EQUAL);
        }
        else if (node instanceof AsgMinusExpr) {
            // 減算代入演算子
            return (EXPR_MINUS + EXPR_EQUAL);
        }
        else if (node instanceof AsgMulExpr) {
            // 乗算代入演算子
            return (EXPR_MUL + EXPR_EQUAL);
        }
        else if (node instanceof AsgDivExpr) {
            // 除算代入演算子
            return (EXPR_DIV + EXPR_EQUAL);
        }
        else if (node instanceof AsgModExpr) {
            // 剰余代入演算子
            return (EXPR_MOD + EXPR_EQUAL);
        }
        else if (node instanceof AsgLshiftExpr) {
            // 左シフト代入演算子
            return (EXPR_LSHIFT + EXPR_EQUAL);
        }
        else if (node instanceof AsgRshiftExpr) {
            // 右シフト代入演算子
            return (EXPR_RSHIFT + EXPR_EQUAL);
        }
        else if (node instanceof AsgBitAndExpr) {
            // ビット単位のAND代入演算子
            return (EXPR_BITAND + EXPR_EQUAL);
        }
        else if (node instanceof AsgBitOrExpr) {
            // ビット単位のOR代入演算子
            return (EXPR_BITOR + EXPR_EQUAL);
        }
        else if (node instanceof AsgBitXorExpr) {
            // ビット単位のXOR代入演算子
            return (EXPR_BITXOR + EXPR_EQUAL);
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
        else if (node instanceof LogLEExpr) {
            // logLEExpr <= .LE. : 小なり、または等価
            return (EXPR_LOGLE);
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
        // 単項演算子
        else if (node instanceof UnaryMinusExpr) {
            // unaryMinusExpr .OR. : 符号反転
            return (EXPR_UNARYMINUS);
        }
        else if (node instanceof UnaryMinusExpr) {
            // unaryMinusExpr - : 符号反転
            return (EXPR_UNARYMINUS);
        }
        else if (node instanceof BitNotExpr) {
            // BitNotExpr ~ : ビット反転
            return (EXPR_BITNOT);
        }
        else if (node instanceof LogNotExpr) {
            // logNotExpr ~ : 論理否定
            return (EXPR_LOGNOT);
        }
        else if (node instanceof PostIncrExpr) {
            // postIncrExpr ++ : 後置インクリメント
            return (EXPR_INCREMENT);
        }
        else if (node instanceof PreIncrExpr) {
            // postIncrExpr ++ : 前置インクリメント
            return (EXPR_INCREMENT);
        }
        else if (node instanceof PostDecrExpr) {
            // postIncrExpr ++ : 後置デクリメント
            return (EXPR_DECREMENT);
        }
        else if (node instanceof PreDecrExpr) {
            // postIncrExpr ++ : 前置デクリメント
            return (EXPR_DECREMENT);
        }

        return "";
    }

    /**
     * 外部手続きリストに追加する
     * @param funcName		外部手続き名
     * @param varType		データ型
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
     * @return		外部手続きリスト
     */
    public Map<String, IVariableType> getExternalFunction() {
        return this.externalFunctionList;
    }



}



