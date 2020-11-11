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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
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
 *       &lt;group ref="{}defModelExpr"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {
      "fintConstant",
      "frealConstant",
      "fcomplexConstant",
      "fcharacterConstant",
      "flogicalConstant",
      "farrayConstructor",
      "fstructConstructor",
      "var",
      "farrayRef",
      "fcharacterRef",
      "fmemberRef",
      "fcoArrayRef",
      "varRef",
      "functionCall",
      "plusExpr",
      "minusExpr",
      "mulExpr",
      "divExpr",
      "fpowerExpr",
      "fconcatExpr",
      "logEQExpr",
      "logNEQExpr",
      "logGEExpr",
      "logGTExpr",
      "logLEExpr",
      "logLTExpr",
      "logAndExpr",
      "logOrExpr",
      "logEQVExpr",
      "logNEQVExpr",
      "unaryMinusExpr",
      "logNotExpr",
      "userBinaryExpr",
      "userUnaryExpr",
      "fdoLoop"
    })
@XmlRootElement(name = "step")
public class Step implements IDefModelExpr, IXmlNode {

  @XmlElement(name = "FintConstant")
  protected FintConstant fintConstant;

  @XmlElement(name = "FrealConstant")
  protected FrealConstant frealConstant;

  @XmlElement(name = "FcomplexConstant")
  protected FcomplexConstant fcomplexConstant;

  @XmlElement(name = "FcharacterConstant")
  protected FcharacterConstant fcharacterConstant;

  @XmlElement(name = "FlogicalConstant")
  protected FlogicalConstant flogicalConstant;

  @XmlElement(name = "FarrayConstructor")
  protected FarrayConstructor farrayConstructor;

  @XmlElement(name = "FstructConstructor")
  protected FstructConstructor fstructConstructor;

  @XmlElement(name = "Var")
  protected Var var;

  @XmlElement(name = "FarrayRef")
  protected FarrayRef farrayRef;

  @XmlElement(name = "FcharacterRef")
  protected FcharacterRef fcharacterRef;

  @XmlElement(name = "FmemberRef")
  protected FmemberRef fmemberRef;

  @XmlElement(name = "FcoArrayRef")
  protected FcoArrayRef fcoArrayRef;

  protected VarRef varRef;
  protected FunctionCall functionCall;
  protected PlusExpr plusExpr;
  protected MinusExpr minusExpr;
  protected MulExpr mulExpr;
  protected DivExpr divExpr;

  @XmlElement(name = "FpowerExpr")
  protected FpowerExpr fpowerExpr;

  @XmlElement(name = "FconcatExpr")
  protected FconcatExpr fconcatExpr;

  protected LogEQExpr logEQExpr;
  protected LogNEQExpr logNEQExpr;
  protected LogGEExpr logGEExpr;
  protected LogGTExpr logGTExpr;
  protected LogLEExpr logLEExpr;
  protected LogLTExpr logLTExpr;
  protected LogAndExpr logAndExpr;
  protected LogOrExpr logOrExpr;
  protected LogEQVExpr logEQVExpr;
  protected LogNEQVExpr logNEQVExpr;
  protected UnaryMinusExpr unaryMinusExpr;
  protected LogNotExpr logNotExpr;
  protected UserBinaryExpr userBinaryExpr;
  protected UserUnaryExpr userUnaryExpr;

  @XmlElement(name = "FdoLoop")
  protected FdoLoop fdoLoop;

  /**
   * Gets the value of the fintConstant property.
   *
   * @return possible object is {@link FintConstant }
   */
  public FintConstant getFintConstant() {
    return fintConstant;
  }

  /**
   * Sets the value of the fintConstant property.
   *
   * @param value allowed object is {@link FintConstant }
   */
  public void setFintConstant(FintConstant value) {
    this.fintConstant = value;
  }

  /**
   * Gets the value of the frealConstant property.
   *
   * @return possible object is {@link FrealConstant }
   */
  public FrealConstant getFrealConstant() {
    return frealConstant;
  }

  /**
   * Sets the value of the frealConstant property.
   *
   * @param value allowed object is {@link FrealConstant }
   */
  public void setFrealConstant(FrealConstant value) {
    this.frealConstant = value;
  }

  /**
   * Gets the value of the fcomplexConstant property.
   *
   * @return possible object is {@link FcomplexConstant }
   */
  public FcomplexConstant getFcomplexConstant() {
    return fcomplexConstant;
  }

  /**
   * Sets the value of the fcomplexConstant property.
   *
   * @param value allowed object is {@link FcomplexConstant }
   */
  public void setFcomplexConstant(FcomplexConstant value) {
    this.fcomplexConstant = value;
  }

  /**
   * Gets the value of the fcharacterConstant property.
   *
   * @return possible object is {@link FcharacterConstant }
   */
  public FcharacterConstant getFcharacterConstant() {
    return fcharacterConstant;
  }

  /**
   * Sets the value of the fcharacterConstant property.
   *
   * @param value allowed object is {@link FcharacterConstant }
   */
  public void setFcharacterConstant(FcharacterConstant value) {
    this.fcharacterConstant = value;
  }

  /**
   * Gets the value of the flogicalConstant property.
   *
   * @return possible object is {@link FlogicalConstant }
   */
  public FlogicalConstant getFlogicalConstant() {
    return flogicalConstant;
  }

  /**
   * Sets the value of the flogicalConstant property.
   *
   * @param value allowed object is {@link FlogicalConstant }
   */
  public void setFlogicalConstant(FlogicalConstant value) {
    this.flogicalConstant = value;
  }

  /**
   * Gets the value of the farrayConstructor property.
   *
   * @return possible object is {@link FarrayConstructor }
   */
  public FarrayConstructor getFarrayConstructor() {
    return farrayConstructor;
  }

  /**
   * Sets the value of the farrayConstructor property.
   *
   * @param value allowed object is {@link FarrayConstructor }
   */
  public void setFarrayConstructor(FarrayConstructor value) {
    this.farrayConstructor = value;
  }

  /**
   * Gets the value of the fstructConstructor property.
   *
   * @return possible object is {@link FstructConstructor }
   */
  public FstructConstructor getFstructConstructor() {
    return fstructConstructor;
  }

  /**
   * Sets the value of the fstructConstructor property.
   *
   * @param value allowed object is {@link FstructConstructor }
   */
  public void setFstructConstructor(FstructConstructor value) {
    this.fstructConstructor = value;
  }

  /**
   * Gets the value of the var property.
   *
   * @return possible object is {@link Var }
   */
  public Var getVar() {
    return var;
  }

  /**
   * Sets the value of the var property.
   *
   * @param value allowed object is {@link Var }
   */
  public void setVar(Var value) {
    this.var = value;
  }

  /**
   * Gets the value of the farrayRef property.
   *
   * @return possible object is {@link FarrayRef }
   */
  public FarrayRef getFarrayRef() {
    return farrayRef;
  }

  /**
   * Sets the value of the farrayRef property.
   *
   * @param value allowed object is {@link FarrayRef }
   */
  public void setFarrayRef(FarrayRef value) {
    this.farrayRef = value;
  }

  /**
   * Gets the value of the fcharacterRef property.
   *
   * @return possible object is {@link FcharacterRef }
   */
  public FcharacterRef getFcharacterRef() {
    return fcharacterRef;
  }

  /**
   * Sets the value of the fcharacterRef property.
   *
   * @param value allowed object is {@link FcharacterRef }
   */
  public void setFcharacterRef(FcharacterRef value) {
    this.fcharacterRef = value;
  }

  /**
   * Gets the value of the fmemberRef property.
   *
   * @return possible object is {@link FmemberRef }
   */
  public FmemberRef getFmemberRef() {
    return fmemberRef;
  }

  /**
   * Sets the value of the fmemberRef property.
   *
   * @param value allowed object is {@link FmemberRef }
   */
  public void setFmemberRef(FmemberRef value) {
    this.fmemberRef = value;
  }

  /**
   * Gets the value of the fcoArrayRef property.
   *
   * @return possible object is {@link FcoArrayRef }
   */
  public FcoArrayRef getFcoArrayRef() {
    return fcoArrayRef;
  }

  /**
   * Sets the value of the fcoArrayRef property.
   *
   * @param value allowed object is {@link FcoArrayRef }
   */
  public void setFcoArrayRef(FcoArrayRef value) {
    this.fcoArrayRef = value;
  }

  /**
   * Gets the value of the varRef property.
   *
   * @return possible object is {@link VarRef }
   */
  public VarRef getVarRef() {
    return varRef;
  }

  /**
   * Sets the value of the varRef property.
   *
   * @param value allowed object is {@link VarRef }
   */
  public void setVarRef(VarRef value) {
    this.varRef = value;
  }

  /**
   * Gets the value of the functionCall property.
   *
   * @return possible object is {@link FunctionCall }
   */
  public FunctionCall getFunctionCall() {
    return functionCall;
  }

  /**
   * Sets the value of the functionCall property.
   *
   * @param value allowed object is {@link FunctionCall }
   */
  public void setFunctionCall(FunctionCall value) {
    this.functionCall = value;
  }

  /**
   * Gets the value of the plusExpr property.
   *
   * @return possible object is {@link PlusExpr }
   */
  public PlusExpr getPlusExpr() {
    return plusExpr;
  }

  /**
   * Sets the value of the plusExpr property.
   *
   * @param value allowed object is {@link PlusExpr }
   */
  public void setPlusExpr(PlusExpr value) {
    this.plusExpr = value;
  }

  /**
   * Gets the value of the minusExpr property.
   *
   * @return possible object is {@link MinusExpr }
   */
  public MinusExpr getMinusExpr() {
    return minusExpr;
  }

  /**
   * Sets the value of the minusExpr property.
   *
   * @param value allowed object is {@link MinusExpr }
   */
  public void setMinusExpr(MinusExpr value) {
    this.minusExpr = value;
  }

  /**
   * Gets the value of the mulExpr property.
   *
   * @return possible object is {@link MulExpr }
   */
  public MulExpr getMulExpr() {
    return mulExpr;
  }

  /**
   * Sets the value of the mulExpr property.
   *
   * @param value allowed object is {@link MulExpr }
   */
  public void setMulExpr(MulExpr value) {
    this.mulExpr = value;
  }

  /**
   * Gets the value of the divExpr property.
   *
   * @return possible object is {@link DivExpr }
   */
  public DivExpr getDivExpr() {
    return divExpr;
  }

  /**
   * Sets the value of the divExpr property.
   *
   * @param value allowed object is {@link DivExpr }
   */
  public void setDivExpr(DivExpr value) {
    this.divExpr = value;
  }

  /**
   * Gets the value of the fpowerExpr property.
   *
   * @return possible object is {@link FpowerExpr }
   */
  public FpowerExpr getFpowerExpr() {
    return fpowerExpr;
  }

  /**
   * Sets the value of the fpowerExpr property.
   *
   * @param value allowed object is {@link FpowerExpr }
   */
  public void setFpowerExpr(FpowerExpr value) {
    this.fpowerExpr = value;
  }

  /**
   * Gets the value of the fconcatExpr property.
   *
   * @return possible object is {@link FconcatExpr }
   */
  public FconcatExpr getFconcatExpr() {
    return fconcatExpr;
  }

  /**
   * Sets the value of the fconcatExpr property.
   *
   * @param value allowed object is {@link FconcatExpr }
   */
  public void setFconcatExpr(FconcatExpr value) {
    this.fconcatExpr = value;
  }

  /**
   * Gets the value of the logEQExpr property.
   *
   * @return possible object is {@link LogEQExpr }
   */
  public LogEQExpr getLogEQExpr() {
    return logEQExpr;
  }

  /**
   * Sets the value of the logEQExpr property.
   *
   * @param value allowed object is {@link LogEQExpr }
   */
  public void setLogEQExpr(LogEQExpr value) {
    this.logEQExpr = value;
  }

  /**
   * Gets the value of the logNEQExpr property.
   *
   * @return possible object is {@link LogNEQExpr }
   */
  public LogNEQExpr getLogNEQExpr() {
    return logNEQExpr;
  }

  /**
   * Sets the value of the logNEQExpr property.
   *
   * @param value allowed object is {@link LogNEQExpr }
   */
  public void setLogNEQExpr(LogNEQExpr value) {
    this.logNEQExpr = value;
  }

  /**
   * Gets the value of the logGEExpr property.
   *
   * @return possible object is {@link LogGEExpr }
   */
  public LogGEExpr getLogGEExpr() {
    return logGEExpr;
  }

  /**
   * Sets the value of the logGEExpr property.
   *
   * @param value allowed object is {@link LogGEExpr }
   */
  public void setLogGEExpr(LogGEExpr value) {
    this.logGEExpr = value;
  }

  /**
   * Gets the value of the logGTExpr property.
   *
   * @return possible object is {@link LogGTExpr }
   */
  public LogGTExpr getLogGTExpr() {
    return logGTExpr;
  }

  /**
   * Sets the value of the logGTExpr property.
   *
   * @param value allowed object is {@link LogGTExpr }
   */
  public void setLogGTExpr(LogGTExpr value) {
    this.logGTExpr = value;
  }

  /**
   * Gets the value of the logLEExpr property.
   *
   * @return possible object is {@link LogLEExpr }
   */
  public LogLEExpr getLogLEExpr() {
    return logLEExpr;
  }

  /**
   * Sets the value of the logLEExpr property.
   *
   * @param value allowed object is {@link LogLEExpr }
   */
  public void setLogLEExpr(LogLEExpr value) {
    this.logLEExpr = value;
  }

  /**
   * Gets the value of the logLTExpr property.
   *
   * @return possible object is {@link LogLTExpr }
   */
  public LogLTExpr getLogLTExpr() {
    return logLTExpr;
  }

  /**
   * Sets the value of the logLTExpr property.
   *
   * @param value allowed object is {@link LogLTExpr }
   */
  public void setLogLTExpr(LogLTExpr value) {
    this.logLTExpr = value;
  }

  /**
   * Gets the value of the logAndExpr property.
   *
   * @return possible object is {@link LogAndExpr }
   */
  public LogAndExpr getLogAndExpr() {
    return logAndExpr;
  }

  /**
   * Sets the value of the logAndExpr property.
   *
   * @param value allowed object is {@link LogAndExpr }
   */
  public void setLogAndExpr(LogAndExpr value) {
    this.logAndExpr = value;
  }

  /**
   * Gets the value of the logOrExpr property.
   *
   * @return possible object is {@link LogOrExpr }
   */
  public LogOrExpr getLogOrExpr() {
    return logOrExpr;
  }

  /**
   * Sets the value of the logOrExpr property.
   *
   * @param value allowed object is {@link LogOrExpr }
   */
  public void setLogOrExpr(LogOrExpr value) {
    this.logOrExpr = value;
  }

  /**
   * Gets the value of the logEQVExpr property.
   *
   * @return possible object is {@link LogEQVExpr }
   */
  public LogEQVExpr getLogEQVExpr() {
    return logEQVExpr;
  }

  /**
   * Sets the value of the logEQVExpr property.
   *
   * @param value allowed object is {@link LogEQVExpr }
   */
  public void setLogEQVExpr(LogEQVExpr value) {
    this.logEQVExpr = value;
  }

  /**
   * Gets the value of the logNEQVExpr property.
   *
   * @return possible object is {@link LogNEQVExpr }
   */
  public LogNEQVExpr getLogNEQVExpr() {
    return logNEQVExpr;
  }

  /**
   * Sets the value of the logNEQVExpr property.
   *
   * @param value allowed object is {@link LogNEQVExpr }
   */
  public void setLogNEQVExpr(LogNEQVExpr value) {
    this.logNEQVExpr = value;
  }

  /**
   * Gets the value of the unaryMinusExpr property.
   *
   * @return possible object is {@link UnaryMinusExpr }
   */
  public UnaryMinusExpr getUnaryMinusExpr() {
    return unaryMinusExpr;
  }

  /**
   * Sets the value of the unaryMinusExpr property.
   *
   * @param value allowed object is {@link UnaryMinusExpr }
   */
  public void setUnaryMinusExpr(UnaryMinusExpr value) {
    this.unaryMinusExpr = value;
  }

  /**
   * Gets the value of the logNotExpr property.
   *
   * @return possible object is {@link LogNotExpr }
   */
  public LogNotExpr getLogNotExpr() {
    return logNotExpr;
  }

  /**
   * Sets the value of the logNotExpr property.
   *
   * @param value allowed object is {@link LogNotExpr }
   */
  public void setLogNotExpr(LogNotExpr value) {
    this.logNotExpr = value;
  }

  /**
   * Gets the value of the userBinaryExpr property.
   *
   * @return possible object is {@link UserBinaryExpr }
   */
  public UserBinaryExpr getUserBinaryExpr() {
    return userBinaryExpr;
  }

  /**
   * Sets the value of the userBinaryExpr property.
   *
   * @param value allowed object is {@link UserBinaryExpr }
   */
  public void setUserBinaryExpr(UserBinaryExpr value) {
    this.userBinaryExpr = value;
  }

  /**
   * Gets the value of the userUnaryExpr property.
   *
   * @return possible object is {@link UserUnaryExpr }
   */
  public UserUnaryExpr getUserUnaryExpr() {
    return userUnaryExpr;
  }

  /**
   * Sets the value of the userUnaryExpr property.
   *
   * @param value allowed object is {@link UserUnaryExpr }
   */
  public void setUserUnaryExpr(UserUnaryExpr value) {
    this.userUnaryExpr = value;
  }

  /**
   * Gets the value of the fdoLoop property.
   *
   * @return possible object is {@link FdoLoop }
   */
  public FdoLoop getFdoLoop() {
    return fdoLoop;
  }

  /**
   * Sets the value of the fdoLoop property.
   *
   * @param value allowed object is {@link FdoLoop }
   */
  public void setFdoLoop(FdoLoop value) {
    this.fdoLoop = value;
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
