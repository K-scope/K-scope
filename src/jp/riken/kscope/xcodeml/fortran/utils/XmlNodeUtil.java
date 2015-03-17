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

package jp.riken.kscope.xcodeml.fortran.utils;

import java.util.List;

import jp.riken.kscope.xcodeml.EnumError;
import jp.riken.kscope.xcodeml.fortran.xml.IDefModelExpr;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Alloc;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Declarations;
import jp.riken.kscope.xcodeml.fortran.xml.gen.DefModelLValue;
import jp.riken.kscope.xcodeml.fortran.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FarrayConstructor;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FarrayRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcharacterConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcharacterRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcoArrayRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcomplexConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FconcatExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdoLoop;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FintConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FlogicalConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FmemberRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FpowerExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FrealConstant;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FstructConstructor;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.fortran.xml.gen.GlobalDeclarations;
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
import jp.riken.kscope.xcodeml.fortran.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UnaryMinusExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UserBinaryExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.UserUnaryExpr;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Var;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.XcodeProgram;


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
     * IDefModelExpr要素から子要素を取得する
     * @param expr		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(IDefModelExpr expr) {

        IXmlNode node = null;
        if (expr == null)
            return node;

        if (expr.getFunctionCall() != null)
            node = expr.getFunctionCall();
        else if (expr.getUserBinaryExpr() != null)
            node = expr.getUserBinaryExpr();
        else if (expr.getFcharacterRef() != null)
            node = expr.getFcharacterRef();
        else if (expr.getFmemberRef() != null)
            node = expr.getFmemberRef();
        else if (expr.getFcoArrayRef() != null)
            node = expr.getFcoArrayRef();
        else if (expr.getFdoLoop() != null)
            node = expr.getFdoLoop();
        else if (expr.getFarrayRef() != null)
            node = expr.getFarrayRef();
        else if (expr.getFcomplexConstant() != null)
            node = expr.getFcomplexConstant();
        else if (expr.getUserUnaryExpr() != null)
            node = expr.getUserUnaryExpr();
        else if (expr.getFpowerExpr() != null)
            node = expr.getFpowerExpr();
        else if (expr.getLogLEExpr() != null)
            node = expr.getLogLEExpr();
        else if (expr.getFintConstant() != null)
            node = expr.getFintConstant();
        else if (expr.getFrealConstant() != null)
            node = expr.getFrealConstant();
        else if (expr.getFcharacterConstant() != null)
            node = expr.getFcharacterConstant();
        else if (expr.getFlogicalConstant() != null)
            node = expr.getFlogicalConstant();
        else if (expr.getVar() != null)
            node = expr.getVar();
        else if (expr.getFarrayConstructor() != null)
            node = expr.getFarrayConstructor();
        else if (expr.getFstructConstructor() != null)
            node = expr.getFstructConstructor();
        else if (expr.getUnaryMinusExpr() != null)
            node = expr.getUnaryMinusExpr();
        else if (expr.getLogNotExpr() != null)
            node = expr.getLogNotExpr();
        else if (expr.getVarRef() != null)
            node = expr.getVarRef();
        else if (expr.getPlusExpr() != null)
            node = expr.getPlusExpr();
        else if (expr.getMinusExpr() != null)
            node = expr.getMinusExpr();
        else if (expr.getMulExpr() != null)
            node = expr.getMulExpr();
        else if (expr.getDivExpr() != null)
            node = expr.getDivExpr();
        else if (expr.getFconcatExpr() != null)
            node = expr.getFconcatExpr();
        else if (expr.getLogEQExpr() != null)
            node = expr.getLogEQExpr();
        else if (expr.getLogNEQExpr() != null)
            node = expr.getLogNEQExpr();
        else if (expr.getLogGEExpr() != null)
            node = expr.getLogGEExpr();
        else if (expr.getLogGTExpr() != null)
            node = expr.getLogGTExpr();
        else if (expr.getLogLTExpr() != null)
            node = expr.getLogLTExpr();
        else if (expr.getLogAndExpr() != null)
            node = expr.getLogAndExpr();
        else if (expr.getLogOrExpr() != null)
            node = expr.getLogOrExpr();
        else if (expr.getLogEQVExpr() != null)
            node = expr.getLogEQVExpr();
        else if (expr.getLogNEQVExpr() != null)
            node = expr.getLogNEQVExpr();

        return node;
    }

    /**
     * DefModelLValue要素から子要素を取得する
     * @param expr		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(DefModelLValue expr) {

        IXmlNode node = null;
        if (expr == null)
            return node;

        if (expr.getVar() != null)
            node = expr.getVar();
        else if (expr.getFarrayRef() != null)
            node = expr.getFarrayRef();
        else if (expr.getFcharacterRef() != null)
            node = expr.getFcharacterRef();
        else if (expr.getFmemberRef() != null)
            node = expr.getFmemberRef();
        else if (expr.getFcoArrayRef() != null)
            node = expr.getFcoArrayRef();

        return node;
    }
    
    /**
     * ExprModel要素であるかチェックする。
     *
     * @param expr
     *            XML要素
     * @return ExprModel要素である。
     */
    public static boolean isExprModel(IXmlNode expr) {

        if (expr == null)
            return false;

        if (expr instanceof FunctionCall)
            return true;
        else if (expr instanceof UserBinaryExpr)
            return true;
        else if (expr instanceof FcharacterRef)
            return true;
        else if (expr instanceof FmemberRef)
            return true;
        else if (expr instanceof FcoArrayRef)
            return true;
        else if (expr instanceof FdoLoop)
            return true;
        else if (expr instanceof FarrayRef)
            return true;
        else if (expr instanceof FcomplexConstant)
            return true;
        else if (expr instanceof UserUnaryExpr)
            return true;
        else if (expr instanceof FpowerExpr)
            return true;
        else if (expr instanceof LogLEExpr)
            return true;
        else if (expr instanceof FintConstant)
            return true;
        else if (expr instanceof FrealConstant)
            return true;
        else if (expr instanceof FcharacterConstant)
            return true;
        else if (expr instanceof FlogicalConstant)
            return true;
        else if (expr instanceof Var)
            return true;
        else if (expr instanceof FarrayConstructor)
            return true;
        else if (expr instanceof FstructConstructor)
            return true;
        else if (expr instanceof UnaryMinusExpr)
            return true;
        else if (expr instanceof LogNotExpr)
            return true;
        else if (expr instanceof VarRef)
            return true;
        else if (expr instanceof PlusExpr)
            return true;
        else if (expr instanceof MinusExpr)
            return true;
        else if (expr instanceof MulExpr)
            return true;
        else if (expr instanceof DivExpr)
            return true;
        else if (expr instanceof FconcatExpr)
            return true;
        else if (expr instanceof LogEQExpr)
            return true;
        else if (expr instanceof LogNEQExpr)
            return true;
        else if (expr instanceof LogGEExpr)
            return true;
        else if (expr instanceof LogGTExpr)
            return true;
        else if (expr instanceof LogLTExpr)
            return true;
        else if (expr instanceof LogAndExpr)
            return true;
        else if (expr instanceof LogOrExpr)
            return true;
        else if (expr instanceof LogEQVExpr)
            return true;
        else if (expr instanceof LogNEQVExpr)
            return true;

        return false;
    }

    /**
     * VarRef要素から子要素を取得する
     * @param expr		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(VarRef expr) {

        IXmlNode node = null;
        if (expr == null)
            return node;

        if (expr.getFcharacterRef() != null)
            node = expr.getFcharacterRef();
        else if (expr.getFmemberRef() != null)
            node = expr.getFmemberRef();
        else if (expr.getFcoArrayRef() != null)
            node = expr.getFcoArrayRef();
        else if (expr.getFarrayRef() != null)
            node = expr.getFarrayRef();
        else if (expr.getVar() != null)
            node = expr.getVar();

        return node;
    }


    /**
     * Alloc要素から子要素を取得する
     * @param visitable		親要素
     * @return			子要素
     */
    public static IXmlNode getXmlNodeChoice(Alloc visitable) {
        IXmlNode node = null;

        if (visitable.getFmemberRef() != null)
            node = visitable.getFmemberRef();
        else if (visitable.getVar() != null)
            node = visitable.getVar();

        return node;
    }

    /**
     * Declarations要素から子要素を取得する
     * @param visitable		親要素
     * @return			子要素
     */
    public static IXmlNode[] getXmlChildNodes(Declarations visitable) {
        List<IXmlNode> list = visitable.getVarDeclOrExternDeclOrFuseDecl();
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
        List<IXmlNode> list = visitable
                .getFfunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition();
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

}
