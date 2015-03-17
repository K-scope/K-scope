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

package jp.riken.kscope.xcodeml.clang.utils;

import java.util.List;

import jp.riken.kscope.xcodeml.EnumError;
import jp.riken.kscope.xcodeml.clang.xml.IBaseStatement;
import jp.riken.kscope.xcodeml.clang.xml.IExpressions;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.gen.AddrOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayRef;
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
import jp.riken.kscope.xcodeml.clang.xml.gen.BitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BuiltinOp;
import jp.riken.kscope.xcodeml.clang.xml.gen.CastExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayAssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.CommaExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValueAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValueExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CondExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Declarations;
import jp.riken.kscope.xcodeml.clang.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Else;
import jp.riken.kscope.xcodeml.clang.xml.gen.FloatConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.FuncAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccAlignOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccCompoundExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccLabelAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GlobalDeclarations;
import jp.riken.kscope.xcodeml.clang.xml.gen.IntConstant;
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
import jp.riken.kscope.xcodeml.clang.xml.gen.LshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MoeConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.RshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.SizeOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.StringConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.SubArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.Then;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnaryMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.XcodeProgram;
import jp.riken.kscope.xcodeml.clang.xml.gen.XmpDescOf;


/**
 * XML要素ユーティリティクラス
 * @author RIKEN
 */
public class XmlNodeUtil {

    /**
     * XMLノードから要素名を取得する.
     *
     * @param node		XMLノード
     * @return     要素名
     */
    public static String getElementName(IXmlNode node) {
        String nodeName = node.getClass().getSimpleName();
        return nodeName;
    }

    /**
     * エラーメッセージを作成する
     *
     * @param errorNode		エラーノード
     * @param errorCode		エラー番号
     * @param args			エラー付加情報
     * @return			エラーメッセージ
     */
    public static String formatError(IXmlNode errorNode, EnumError errorCode, Object... args) {
        StringBuilder sb = new StringBuilder();
        sb.append("Error(");
        sb.append(errorCode.ordinal());
        sb.append("): ");
        sb.append(errorCode.format(args));

        if (errorNode == null) {
            return sb.toString();
        }

        sb.append("\n<Element>\n");

        String nodeName = errorNode.getClass().getSimpleName();
        sb.append(nodeName);

        return sb.toString();
    }

    /**
     * Boolean値をbooleanに変換する.<br/>
     * nullの場合は、falseを返す
     *
     * @param value		Boolean値
     * @return			boolean値
     */
    public static boolean isBoolean(Boolean value) {
        if (value == null)
            return false;
        return value.booleanValue();
    }

    /**
     * String値をbooleanに変換する.<br/>
     * null, 不明の場合は、falseを返す
     *
     * @param value		Boolean値
     * @return			boolean値
     */
    public static boolean isBoolean(String value) {
        if (value == null)
            return false;
        if (value.equals("0")) return false;
        if (value.equals("1")) return true;
        if (value.equalsIgnoreCase("false")) return false;
        if (value.equalsIgnoreCase("true")) return true;
        return false;
    }

    /**
     * IExpressions要素から子要素を取得する
     * @param expr		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(IExpressions expr) {

        IXmlNode node = null;
        if (expr == null) return node;

        if (expr.getDivExpr() != null) node = expr.getDivExpr();
        else if (expr.getModExpr() != null) node = expr.getModExpr();
        else if (expr.getAsgDivExpr() != null) node = expr.getAsgDivExpr();
        else if (expr.getAsgModExpr() != null) node = expr.getAsgModExpr();
        else if (expr.getLshiftExpr() != null) node = expr.getLshiftExpr();
        else if (expr.getRshiftExpr() != null) node = expr.getRshiftExpr();
        else if (expr.getBitAndExpr() != null) node = expr.getBitAndExpr();
        else if (expr.getBitOrExpr() != null) node = expr.getBitOrExpr();
        else if (expr.getBitXorExpr() != null) node = expr.getBitXorExpr();
        else if (expr.getAsgLshiftExpr() != null) node = expr.getAsgLshiftExpr();
        else if (expr.getAsgRshiftExpr() != null) node = expr.getAsgRshiftExpr();
        else if (expr.getAsgBitAndExpr() != null) node = expr.getAsgBitAndExpr();
        else if (expr.getAsgBitOrExpr() != null) node = expr.getAsgBitOrExpr();
        else if (expr.getAsgBitXorExpr() != null) node = expr.getAsgBitXorExpr();
        else if (expr.getPointerRef() != null) node = expr.getPointerRef();
        else if (expr.getStringConstant() != null) node = expr.getStringConstant();
        else if (expr.getMoeConstant() != null) node = expr.getMoeConstant();
        else if (expr.getArrayRef() != null) node = expr.getArrayRef();
        else if (expr.getArrayAddr() != null) node = expr.getArrayAddr();
        else if (expr.getMemberAddr() != null) node = expr.getMemberAddr();
        else if (expr.getMemberRef() != null) node = expr.getMemberRef();
        else if (expr.getMemberArrayAddr() != null) node = expr.getMemberArrayAddr();
        else if (expr.getMemberArrayRef() != null) node = expr.getMemberArrayRef();
        else if (expr.getCoArrayRef() != null) node = expr.getCoArrayRef();
        else if (expr.getCoArrayAssignExpr() != null) node = expr.getCoArrayAssignExpr();
        else if (expr.getIntConstant() != null) node = expr.getIntConstant();
        else if (expr.getLonglongConstant() != null) node = expr.getLonglongConstant();
        else if (expr.getFloatConstant() != null) node = expr.getFloatConstant();
        else if (expr.getFuncAddr() != null) node = expr.getFuncAddr();
        else if (expr.getFunctionCall() != null) node = expr.getFunctionCall();
        else if (expr.getAssignExpr() != null) node = expr.getAssignExpr();
        else if (expr.getLogAndExpr() != null) node = expr.getLogAndExpr();
        else if (expr.getLogEQExpr() != null) node = expr.getLogEQExpr();
        else if (expr.getLogGEExpr() != null) node = expr.getLogGEExpr();
        else if (expr.getLogGTExpr() != null) node = expr.getLogGTExpr();
        else if (expr.getLogLEExpr() != null) node = expr.getLogLEExpr();
        else if (expr.getLogLTExpr() != null) node = expr.getLogLTExpr();
        else if (expr.getLogNEQExpr() != null) node = expr.getLogNEQExpr();
        else if (expr.getBitNotExpr() != null) node = expr.getBitNotExpr();
        else if (expr.getLogNotExpr() != null) node = expr.getLogNotExpr();
        else if (expr.getCommaExpr() != null) node = expr.getCommaExpr();
        else if (expr.getPostIncrExpr() != null) node = expr.getPostIncrExpr();
        else if (expr.getPostDecrExpr() != null) node = expr.getPostDecrExpr();
        else if (expr.getPreIncrExpr() != null) node = expr.getPreIncrExpr();
        else if (expr.getPreDecrExpr() != null) node = expr.getPreDecrExpr();
        else if (expr.getCastExpr() != null) node = expr.getCastExpr();
        else if (expr.getCondExpr() != null) node = expr.getCondExpr();
        else if (expr.getSizeOfExpr() != null) node = expr.getSizeOfExpr();
        else if (expr.getAddrOfExpr() != null) node = expr.getAddrOfExpr();
        else if (expr.getXmpDescOf() != null) node = expr.getXmpDescOf();
        else if (expr.getCompoundValue() != null) node = expr.getCompoundValue();
        else if (expr.getCompoundValueAddr() != null) node = expr.getCompoundValueAddr();
        else if (expr.getGccAlignOfExpr() != null) node = expr.getGccAlignOfExpr();
        else if (expr.getGccLabelAddr() != null) node = expr.getGccLabelAddr();
        else if (expr.getGccCompoundExpr() != null) node = expr.getGccCompoundExpr();
        else if (expr.getBuiltinOp() != null) node = expr.getBuiltinOp();
        else if (expr.getSubArrayRef() != null) node = expr.getSubArrayRef();
        else if (expr.getLogOrExpr() != null) node = expr.getLogOrExpr();
        else if (expr.getMinusExpr() != null) node = expr.getMinusExpr();
        else if (expr.getAsgMinusExpr() != null) node = expr.getAsgMinusExpr();
        else if (expr.getMulExpr() != null) node = expr.getMulExpr();
        else if (expr.getAsgMulExpr() != null) node = expr.getAsgMulExpr();
        else if (expr.getPlusExpr() != null) node = expr.getPlusExpr();
        else if (expr.getAsgPlusExpr() != null) node = expr.getAsgPlusExpr();
        else if (expr.getUnaryMinusExpr() != null) node = expr.getUnaryMinusExpr();
        else if (expr.getVar() != null) node = expr.getVar();
        else if (expr.getVarAddr() != null) node = expr.getVarAddr();

        return node;
    }


    /**
     * ExprModel要素であるかチェックする。
     *
     * @param expr
     *            XML要素
     * @return ExprModel要素である。
     */
    public static boolean isExpressions(IXmlNode expr) {

        if (expr == null)
            return false;

        if (expr instanceof DivExpr) return true;
        else if (expr instanceof ModExpr) return true;
        else if (expr instanceof AsgDivExpr) return true;
        else if (expr instanceof AsgModExpr) return true;
        else if (expr instanceof LshiftExpr) return true;
        else if (expr instanceof RshiftExpr) return true;
        else if (expr instanceof BitAndExpr) return true;
        else if (expr instanceof BitOrExpr) return true;
        else if (expr instanceof BitXorExpr) return true;
        else if (expr instanceof AsgLshiftExpr) return true;
        else if (expr instanceof AsgRshiftExpr) return true;
        else if (expr instanceof AsgBitAndExpr) return true;
        else if (expr instanceof AsgBitOrExpr) return true;
        else if (expr instanceof AsgBitXorExpr) return true;
        else if (expr instanceof PointerRef) return true;
        else if (expr instanceof StringConstant) return true;
        else if (expr instanceof MoeConstant) return true;
        else if (expr instanceof ArrayRef) return true;
        else if (expr instanceof ArrayAddr) return true;
        else if (expr instanceof MemberAddr) return true;
        else if (expr instanceof MemberRef) return true;
        else if (expr instanceof MemberArrayAddr) return true;
        else if (expr instanceof MemberArrayRef) return true;
        else if (expr instanceof CoArrayRef) return true;
        else if (expr instanceof CoArrayAssignExpr) return true;
        else if (expr instanceof IntConstant) return true;
        else if (expr instanceof LonglongConstant) return true;
        else if (expr instanceof FloatConstant) return true;
        else if (expr instanceof FuncAddr) return true;
        else if (expr instanceof FunctionCall) return true;
        else if (expr instanceof AssignExpr) return true;
        else if (expr instanceof LogAndExpr) return true;
        else if (expr instanceof LogEQExpr) return true;
        else if (expr instanceof LogGEExpr) return true;
        else if (expr instanceof LogGTExpr) return true;
        else if (expr instanceof LogLEExpr) return true;
        else if (expr instanceof LogLTExpr) return true;
        else if (expr instanceof LogNEQExpr) return true;
        else if (expr instanceof BitNotExpr) return true;
        else if (expr instanceof LogNotExpr) return true;
        else if (expr instanceof CommaExpr) return true;
        else if (expr instanceof PostIncrExpr) return true;
        else if (expr instanceof PostDecrExpr) return true;
        else if (expr instanceof PreIncrExpr) return true;
        else if (expr instanceof PreDecrExpr) return true;
        else if (expr instanceof CastExpr) return true;
        else if (expr instanceof CondExpr) return true;
        else if (expr instanceof SizeOfExpr) return true;
        else if (expr instanceof AddrOfExpr) return true;
        else if (expr instanceof XmpDescOf) return true;
        else if (expr instanceof CompoundValueExpr) return true;
        else if (expr instanceof CompoundValueAddr) return true;
        else if (expr instanceof GccAlignOfExpr) return true;
        else if (expr instanceof GccLabelAddr) return true;
        else if (expr instanceof GccCompoundExpr) return true;
        else if (expr instanceof BuiltinOp) return true;
        else if (expr instanceof SubArrayRef) return true;
        else if (expr instanceof LogOrExpr) return true;
        else if (expr instanceof MinusExpr) return true;
        else if (expr instanceof AsgMinusExpr) return true;
        else if (expr instanceof MulExpr) return true;
        else if (expr instanceof AsgMulExpr) return true;
        else if (expr instanceof PlusExpr) return true;
        else if (expr instanceof AsgPlusExpr) return true;
        else if (expr instanceof UnaryMinusExpr) return true;
        else if (expr instanceof Var) return true;
        else if (expr instanceof VarAddr) return true;

        return false;
    }


    /**
     * Declarations要素から子要素を取得する
     * @param visitable		親要素
     * @return			子要素
     */
    public static IXmlNode[] getXmlChildNodes(Declarations visitable) {
        List<IXmlNode> list = visitable.getFunctionDefinitionOrVarDeclOrFunctionDecl();
        if (list == null || list.size() == 0)
            return null;

        return (IXmlNode[]) list.toArray(new IXmlNode[0]);
    }

    /**
     * GlobalDeclarations要素から子要素を取得する
     * @param visitable		親要素
     * @return			子要素
     */
    public static IXmlNode[] getXmlChildNodes(GlobalDeclarations visitable) {
        List<IXmlNode> list = visitable.getFunctionDefinitionOrVarDeclOrFunctionDecl();
        if (list == null || list.size() == 0)
            return null;

        return (IXmlNode[]) list.toArray(new IXmlNode[0]);
    }

    /**
     * XcodeProgram要素から子要素を取得する
     * @param visitable		親要素
     * @return			子要素
     */
    public static IXmlNode[] getXmlChildNodes(XcodeProgram visitable) {
        java.util.List<IXmlNode> list = new java.util.ArrayList<IXmlNode>();

        if (visitable.getTypeTable() != null) {
            list.add(visitable.getTypeTable());
        }
        if (visitable.getGlobalSymbols() != null) {
            list.add(visitable.getGlobalSymbols());
        }
        if (visitable.getGlobalDeclarations() != null) {
            list.add(visitable.getGlobalDeclarations());
        }

        if (list.size() == 0)
            return null;

        return (IXmlNode[]) list.toArray(new IXmlNode[0]);
    }

    /**
     * 文要素であるかチェックする。
     * fileとlinenoを持つ文要素であるかチェックする.
     * 式を'(',')'で囲むか判断を行う.親要素が文要素であれば()は必要ない。
     * @param expr           XML要素
     * @return 文要素である。
     */
    public static boolean isStatement(IXmlNode expr) {

        if (expr == null)
            return false;

        if (expr instanceof IBaseStatement)
            return true;

        return false;
    }


    /**
     * Then要素から子要素を取得する
     * @param expr		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(Then expr) {

        IXmlNode node = null;
        if (expr == null) return node;

        if (expr.getBreakStatement() != null) node = expr.getBreakStatement();
        else if (expr.getCaseLabel() != null) node = expr.getCaseLabel();
        else if (expr.getCompoundStatement() != null) node = expr.getCompoundStatement();
        else if (expr.getContinueStatement() != null) node = expr.getContinueStatement();
        else if (expr.getDefaultLabel() != null) node = expr.getDefaultLabel();
        else if (expr.getDoStatement() != null) node = expr.getDoStatement();
        else if (expr.getExprStatement() != null) node = expr.getExprStatement();
        else if (expr.getForStatement() != null) node = expr.getForStatement();
        else if (expr.getGccAsmStatement() != null) node = expr.getGccAsmStatement();
        else if (expr.getGccRangedCaseLabel() != null) node = expr.getGccRangedCaseLabel();
        else if (expr.getGotoStatement() != null) node = expr.getGotoStatement();
        else if (expr.getIfStatement() != null) node = expr.getIfStatement();
        else if (expr.getPragma() != null) node = expr.getPragma();
        else if (expr.getReturnStatement() != null) node = expr.getReturnStatement();
        else if (expr.getStatementLabel() != null) node = expr.getStatementLabel();
        else if (expr.getSwitchStatement() != null) node = expr.getSwitchStatement();
        else if (expr.getText() != null) node = expr.getText();
        else if (expr.getWhileStatement() != null) node = expr.getWhileStatement();

        return node;
    }

    /**
     * Else要素から子要素を取得する
     * @param expr		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(Else expr) {

        IXmlNode node = null;
        if (expr == null) return node;

        if (expr.getBreakStatement() != null) node = expr.getBreakStatement();
        else if (expr.getCaseLabel() != null) node = expr.getCaseLabel();
        else if (expr.getCompoundStatement() != null) node = expr.getCompoundStatement();
        else if (expr.getContinueStatement() != null) node = expr.getContinueStatement();
        else if (expr.getDefaultLabel() != null) node = expr.getDefaultLabel();
        else if (expr.getDoStatement() != null) node = expr.getDoStatement();
        else if (expr.getExprStatement() != null) node = expr.getExprStatement();
        else if (expr.getForStatement() != null) node = expr.getForStatement();
        else if (expr.getGccAsmStatement() != null) node = expr.getGccAsmStatement();
        else if (expr.getGccRangedCaseLabel() != null) node = expr.getGccRangedCaseLabel();
        else if (expr.getGotoStatement() != null) node = expr.getGotoStatement();
        else if (expr.getIfStatement() != null) node = expr.getIfStatement();
        else if (expr.getPragma() != null) node = expr.getPragma();
        else if (expr.getReturnStatement() != null) node = expr.getReturnStatement();
        else if (expr.getStatementLabel() != null) node = expr.getStatementLabel();
        else if (expr.getSwitchStatement() != null) node = expr.getSwitchStatement();
        else if (expr.getText() != null) node = expr.getText();
        else if (expr.getWhileStatement() != null) node = expr.getWhileStatement();

        return node;
    }
}
