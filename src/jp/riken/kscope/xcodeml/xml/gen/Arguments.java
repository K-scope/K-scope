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
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import jp.riken.kscope.xcodeml.xml.*;

/**
 * Java class for anonymous complex type.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;choice maxOccurs="unbounded" minOccurs="0">
 *         &lt;group ref="{}defModelExpr"/>
 *         &lt;element ref="{}Ffunction"/>
 *         &lt;element ref="{}namedValue"/>
 *       &lt;/choice>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"fintConstantOrFrealConstantOrFcomplexConstant"})
@XmlRootElement(name = "arguments")
public class Arguments implements IXmlNode {

  @XmlElements({
    @XmlElement(name = "FconcatExpr", type = FconcatExpr.class),
    @XmlElement(name = "unaryMinusExpr", type = UnaryMinusExpr.class),
    @XmlElement(name = "logNEQVExpr", type = LogNEQVExpr.class),
    @XmlElement(name = "logOrExpr", type = LogOrExpr.class),
    @XmlElement(name = "FcharacterConstant", type = FcharacterConstant.class),
    @XmlElement(name = "logEQExpr", type = LogEQExpr.class),
    @XmlElement(name = "FlogicalConstant", type = FlogicalConstant.class),
    @XmlElement(name = "FstructConstructor", type = FstructConstructor.class),
    @XmlElement(name = "mulExpr", type = MulExpr.class),
    @XmlElement(name = "Var", type = Var.class),
    @XmlElement(name = "FdoLoop", type = FdoLoop.class),
    @XmlElement(name = "logAndExpr", type = LogAndExpr.class),
    @XmlElement(name = "logGTExpr", type = LogGTExpr.class),
    @XmlElement(name = "logGEExpr", type = LogGEExpr.class),
    @XmlElement(name = "Ffunction", type = Ffunction.class),
    @XmlElement(name = "namedValue", type = NamedValue.class),
    @XmlElement(name = "plusExpr", type = PlusExpr.class),
    @XmlElement(name = "FarrayRef", type = FarrayRef.class),
    @XmlElement(name = "FrealConstant", type = FrealConstant.class),
    @XmlElement(name = "logNotExpr", type = LogNotExpr.class),
    @XmlElement(name = "userUnaryExpr", type = UserUnaryExpr.class),
    @XmlElement(name = "functionCall", type = FunctionCall.class),
    @XmlElement(name = "FcomplexConstant", type = FcomplexConstant.class),
    @XmlElement(name = "FpowerExpr", type = FpowerExpr.class),
    @XmlElement(name = "logNEQExpr", type = LogNEQExpr.class),
    @XmlElement(name = "logLTExpr", type = LogLTExpr.class),
    @XmlElement(name = "FmemberRef", type = FmemberRef.class),
    @XmlElement(name = "logLEExpr", type = LogLEExpr.class),
    @XmlElement(name = "FintConstant", type = FintConstant.class),
    @XmlElement(name = "userBinaryExpr", type = UserBinaryExpr.class),
    @XmlElement(name = "FcharacterRef", type = FcharacterRef.class),
    @XmlElement(name = "divExpr", type = DivExpr.class),
    @XmlElement(name = "minusExpr", type = MinusExpr.class),
    @XmlElement(name = "logEQVExpr", type = LogEQVExpr.class),
    @XmlElement(name = "varRef", type = VarRef.class),
    @XmlElement(name = "FcoArrayRef", type = FcoArrayRef.class),
    @XmlElement(name = "FarrayConstructor", type = FarrayConstructor.class)
  })
  protected List<IXmlNode> fintConstantOrFrealConstantOrFcomplexConstant;

  /**
   * Gets the value of the fintConstantOrFrealConstantOrFcomplexConstant property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the fintConstantOrFrealConstantOrFcomplexConstant
   * property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getFintConstantOrFrealConstantOrFcomplexConstant().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link FconcatExpr } {@link
   * UnaryMinusExpr } {@link LogNEQVExpr } {@link LogOrExpr } {@link FcharacterConstant } {@link
   * LogEQExpr } {@link FlogicalConstant } {@link FstructConstructor } {@link MulExpr } {@link Var }
   * {@link FdoLoop } {@link LogAndExpr } {@link LogGTExpr } {@link LogGEExpr } {@link Ffunction }
   * {@link NamedValue } {@link PlusExpr } {@link FarrayRef } {@link FrealConstant } {@link
   * LogNotExpr } {@link UserUnaryExpr } {@link FunctionCall } {@link FcomplexConstant } {@link
   * FpowerExpr } {@link LogNEQExpr } {@link LogLTExpr } {@link FmemberRef } {@link LogLEExpr }
   * {@link FintConstant } {@link UserBinaryExpr } {@link FcharacterRef } {@link DivExpr } {@link
   * MinusExpr } {@link LogEQVExpr } {@link VarRef } {@link FcoArrayRef } {@link FarrayConstructor }
   *
   * @return IXmlNode List
   */
  public List<IXmlNode> getFintConstantOrFrealConstantOrFcomplexConstant() {
    if (fintConstantOrFrealConstantOrFcomplexConstant == null) {
      fintConstantOrFrealConstantOrFcomplexConstant = new ArrayList<IXmlNode>();
    }
    return this.fintConstantOrFrealConstantOrFcomplexConstant;
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
