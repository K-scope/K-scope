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

package jp.riken.kscope.xcodeml.xml.gen;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;
import jp.riken.kscope.xcodeml.xml.*;

/**
 * Java class for defModelExprList complex type.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType name="defModelExprList">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;group ref="{}defModelExpr" maxOccurs="unbounded" minOccurs="0"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "defModelExprList",
    propOrder = {"defModelExpr"})
@XmlSeeAlso({FarrayConstructor.class, FstructConstructor.class})
public class DefModelExprList implements IXmlNode {

  @XmlElements({
    @XmlElement(name = "FconcatExpr", type = FconcatExpr.class),
    @XmlElement(name = "logNEQVExpr", type = LogNEQVExpr.class),
    @XmlElement(name = "FlogicalConstant", type = FlogicalConstant.class),
    @XmlElement(name = "divExpr", type = DivExpr.class),
    @XmlElement(name = "FmemberRef", type = FmemberRef.class),
    @XmlElement(name = "FcharacterConstant", type = FcharacterConstant.class),
    @XmlElement(name = "userUnaryExpr", type = UserUnaryExpr.class),
    @XmlElement(name = "FpowerExpr", type = FpowerExpr.class),
    @XmlElement(name = "logEQExpr", type = LogEQExpr.class),
    @XmlElement(name = "plusExpr", type = PlusExpr.class),
    @XmlElement(name = "FstructConstructor", type = FstructConstructor.class),
    @XmlElement(name = "logOrExpr", type = LogOrExpr.class),
    @XmlElement(name = "minusExpr", type = MinusExpr.class),
    @XmlElement(name = "logNEQExpr", type = LogNEQExpr.class),
    @XmlElement(name = "logNotExpr", type = LogNotExpr.class),
    @XmlElement(name = "logLTExpr", type = LogLTExpr.class),
    @XmlElement(name = "logGTExpr", type = LogGTExpr.class),
    @XmlElement(name = "functionCall", type = FunctionCall.class),
    @XmlElement(name = "FarrayConstructor", type = FarrayConstructor.class),
    @XmlElement(name = "varRef", type = VarRef.class),
    @XmlElement(name = "unaryMinusExpr", type = UnaryMinusExpr.class),
    @XmlElement(name = "FdoLoop", type = FdoLoop.class),
    @XmlElement(name = "logEQVExpr", type = LogEQVExpr.class),
    @XmlElement(name = "logLEExpr", type = LogLEExpr.class),
    @XmlElement(name = "FcoArrayRef", type = FcoArrayRef.class),
    @XmlElement(name = "Var", type = Var.class),
    @XmlElement(name = "FcomplexConstant", type = FcomplexConstant.class),
    @XmlElement(name = "logGEExpr", type = LogGEExpr.class),
    @XmlElement(name = "FarrayRef", type = FarrayRef.class),
    @XmlElement(name = "userBinaryExpr", type = UserBinaryExpr.class),
    @XmlElement(name = "FcharacterRef", type = FcharacterRef.class),
    @XmlElement(name = "FintConstant", type = FintConstant.class),
    @XmlElement(name = "mulExpr", type = MulExpr.class),
    @XmlElement(name = "logAndExpr", type = LogAndExpr.class),
    @XmlElement(name = "FrealConstant", type = FrealConstant.class)
  })
  protected List<IXmlNode> defModelExpr;

  /**
   * Gets the value of the defModelExpr property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the defModelExpr property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getDefModelExpr().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link FconcatExpr } {@link
   * LogNEQVExpr } {@link FlogicalConstant } {@link DivExpr } {@link FmemberRef } {@link
   * FcharacterConstant } {@link UserUnaryExpr } {@link FpowerExpr } {@link LogEQExpr } {@link
   * PlusExpr } {@link FstructConstructor } {@link LogOrExpr } {@link MinusExpr } {@link LogNEQExpr
   * } {@link LogNotExpr } {@link LogLTExpr } {@link LogGTExpr } {@link FunctionCall } {@link
   * FarrayConstructor } {@link VarRef } {@link UnaryMinusExpr } {@link FdoLoop } {@link LogEQVExpr
   * } {@link LogLEExpr } {@link FcoArrayRef } {@link Var } {@link FcomplexConstant } {@link
   * LogGEExpr } {@link FarrayRef } {@link UserBinaryExpr } {@link FcharacterRef } {@link
   * FintConstant } {@link MulExpr } {@link LogAndExpr } {@link FrealConstant }
   *
   * @return IXmlNode List
   */
  public List<IXmlNode> getDefModelExpr() {
    if (defModelExpr == null) {
      defModelExpr = new ArrayList<IXmlNode>();
    }
    return this.defModelExpr;
  }

  @Override
  public boolean enter(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  @Override
  public void leave(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
