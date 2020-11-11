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

import java.util.List;
import jp.riken.kscope.xcodeml.xml.*;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * XML element utility class
 *
 * @author RIKEN
 */
public class XmlNodeUtil {

  /**
   * Get the element name from the XML node.
   *
   * @param node XML node
   * @return element name
   */
  public static String getElementName(IXmlNode node) {
    String nodeName = node.getClass().getSimpleName();
    return nodeName;
  }

  /**
   * Create an error message
   *
   * @param errorNode Error node
   * @param errorCode Error number
   * @param args Error additional information
   * @return error message
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
   * Convert Boolean values to boolean. <br>
   * Returns false if null
   *
   * @param value Boolean value
   * @return boolean value
   */
  public static boolean isBoolean(Boolean value) {
    if (value == null) return false;
    return value.booleanValue();
  }

  /**
   * Get child elements from IDefModelExpr element
   *
   * @param expr Parent element
   * @return Child element
   */
  public static IXmlNode getXmlNodeChoice(IDefModelExpr expr) {

    IXmlNode node = null;
    if (expr == null) return node;

    if (expr.getFunctionCall() != null) node = expr.getFunctionCall();
    else if (expr.getUserBinaryExpr() != null) node = expr.getUserBinaryExpr();
    else if (expr.getFcharacterRef() != null) node = expr.getFcharacterRef();
    else if (expr.getFmemberRef() != null) node = expr.getFmemberRef();
    else if (expr.getFcoArrayRef() != null) node = expr.getFcoArrayRef();
    else if (expr.getFdoLoop() != null) node = expr.getFdoLoop();
    else if (expr.getFarrayRef() != null) node = expr.getFarrayRef();
    else if (expr.getFcomplexConstant() != null) node = expr.getFcomplexConstant();
    else if (expr.getUserUnaryExpr() != null) node = expr.getUserUnaryExpr();
    else if (expr.getFpowerExpr() != null) node = expr.getFpowerExpr();
    else if (expr.getLogLEExpr() != null) node = expr.getLogLEExpr();
    else if (expr.getFintConstant() != null) node = expr.getFintConstant();
    else if (expr.getFrealConstant() != null) node = expr.getFrealConstant();
    else if (expr.getFcharacterConstant() != null) node = expr.getFcharacterConstant();
    else if (expr.getFlogicalConstant() != null) node = expr.getFlogicalConstant();
    else if (expr.getVar() != null) node = expr.getVar();
    else if (expr.getFarrayConstructor() != null) node = expr.getFarrayConstructor();
    else if (expr.getFstructConstructor() != null) node = expr.getFstructConstructor();
    else if (expr.getUnaryMinusExpr() != null) node = expr.getUnaryMinusExpr();
    else if (expr.getLogNotExpr() != null) node = expr.getLogNotExpr();
    else if (expr.getVarRef() != null) node = expr.getVarRef();
    else if (expr.getPlusExpr() != null) node = expr.getPlusExpr();
    else if (expr.getMinusExpr() != null) node = expr.getMinusExpr();
    else if (expr.getMulExpr() != null) node = expr.getMulExpr();
    else if (expr.getDivExpr() != null) node = expr.getDivExpr();
    else if (expr.getFconcatExpr() != null) node = expr.getFconcatExpr();
    else if (expr.getLogEQExpr() != null) node = expr.getLogEQExpr();
    else if (expr.getLogNEQExpr() != null) node = expr.getLogNEQExpr();
    else if (expr.getLogGEExpr() != null) node = expr.getLogGEExpr();
    else if (expr.getLogGTExpr() != null) node = expr.getLogGTExpr();
    else if (expr.getLogLTExpr() != null) node = expr.getLogLTExpr();
    else if (expr.getLogAndExpr() != null) node = expr.getLogAndExpr();
    else if (expr.getLogOrExpr() != null) node = expr.getLogOrExpr();
    else if (expr.getLogEQVExpr() != null) node = expr.getLogEQVExpr();
    else if (expr.getLogNEQVExpr() != null) node = expr.getLogNEQVExpr();

    return node;
  }

  /**
   * Check if it is an ExprModel element.
   *
   * @param expr XML element
   * @return ExprModel element.
   */
  public static boolean isExprModel(IXmlNode expr) {

    if (expr == null) return false;

    if (expr instanceof FunctionCall) return true;
    else if (expr instanceof UserBinaryExpr) return true;
    else if (expr instanceof FcharacterRef) return true;
    else if (expr instanceof FmemberRef) return true;
    else if (expr instanceof FcoArrayRef) return true;
    else if (expr instanceof FdoLoop) return true;
    else if (expr instanceof FarrayRef) return true;
    else if (expr instanceof FcomplexConstant) return true;
    else if (expr instanceof UserUnaryExpr) return true;
    else if (expr instanceof FpowerExpr) return true;
    else if (expr instanceof LogLEExpr) return true;
    else if (expr instanceof FintConstant) return true;
    else if (expr instanceof FrealConstant) return true;
    else if (expr instanceof FcharacterConstant) return true;
    else if (expr instanceof FlogicalConstant) return true;
    else if (expr instanceof Var) return true;
    else if (expr instanceof FarrayConstructor) return true;
    else if (expr instanceof FstructConstructor) return true;
    else if (expr instanceof UnaryMinusExpr) return true;
    else if (expr instanceof LogNotExpr) return true;
    else if (expr instanceof VarRef) return true;
    else if (expr instanceof PlusExpr) return true;
    else if (expr instanceof MinusExpr) return true;
    else if (expr instanceof MulExpr) return true;
    else if (expr instanceof DivExpr) return true;
    else if (expr instanceof FconcatExpr) return true;
    else if (expr instanceof LogEQExpr) return true;
    else if (expr instanceof LogNEQExpr) return true;
    else if (expr instanceof LogGEExpr) return true;
    else if (expr instanceof LogGTExpr) return true;
    else if (expr instanceof LogLTExpr) return true;
    else if (expr instanceof LogAndExpr) return true;
    else if (expr instanceof LogOrExpr) return true;
    else if (expr instanceof LogEQVExpr) return true;
    else if (expr instanceof LogNEQVExpr) return true;

    return false;
  }

  /**
   * Get child elements from VarRef element
   *
   * @param expr Parent element
   * @return Child element
   */
  public static IXmlNode getXmlNodeChoice(VarRef expr) {

    IXmlNode node = null;
    if (expr == null) return node;

    if (expr.getFcharacterRef() != null) node = expr.getFcharacterRef();
    else if (expr.getFmemberRef() != null) node = expr.getFmemberRef();
    else if (expr.getFcoArrayRef() != null) node = expr.getFcoArrayRef();
    else if (expr.getFarrayRef() != null) node = expr.getFarrayRef();
    else if (expr.getVar() != null) node = expr.getVar();

    return node;
  }

  /**
   * Get child elements from LogNotExpr element
   *
   * @param visitable parent element
   * @return Child element
   */
  public static IXmlNode getXmlNodeChoice(LogNotExpr visitable) {

    List<IXmlNode> list = visitable.getContent();
    if (list.size() <= 0) return null;

    return list.get(0);
  }

  /**
   * Get child elements from UnaryMinusExpr element
   *
   * @param visitable parent element
   * @return Child element
   */
  public static IXmlNode getXmlNodeChoice(UnaryMinusExpr visitable) {

    List<IXmlNode> list = visitable.getContent();
    if (list.size() <= 0) return null;

    return list.get(0);
  }

  /**
   * Get child elements from Alloc elements
   *
   * @param visitable parent element
   * @return Child element
   */
  public static IXmlNode getXmlNodeChoice(Alloc visitable) {
    IXmlNode node = null;

    if (visitable.getFmemberRef() != null) node = visitable.getFmemberRef();
    else if (visitable.getVar() != null) node = visitable.getVar();

    return node;
  }

  /**
   * Get child elements from Declarations element
   *
   * @param visitable parent element
   * @return Child element
   */
  public static IXmlNode[] getXmlChildNodes(Declarations visitable) {
    List<IXmlNode> list = visitable.getVarDeclOrExternDeclOrFuseDecl();
    if (list == null || list.size() == 0) return null;

    return (IXmlNode[]) list.toArray(new IXmlNode[0]);
  }

  /**
   * Get child elements from GlobalDeclarations element
   *
   * @param visitable parent element
   * @return Child element
   */
  public static IXmlNode[] getXmlChildNodes(GlobalDeclarations visitable) {
    List<IXmlNode> list =
        visitable.getFfunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition();
    if (list == null || list.size() == 0) return null;

    return (IXmlNode[]) list.toArray(new IXmlNode[0]);
  }

  /**
   * Get child elements from XcodeProgram element
   *
   * @param visitable parent element
   * @return Child element
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

    if (list.size() == 0) return null;

    return (IXmlNode[]) list.toArray(new IXmlNode[0]);
  }
}
