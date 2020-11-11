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
package jp.riken.kscope.xcodeml.xml;

import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * Expression model interface class
 *
 * @author RIKEN
 */
public interface IDefModelExpr {

  /**
   * Get the DivExpr element
   *
   * @return DivExpr (division) element
   */
  public DivExpr getDivExpr();

  /**
   * Get the FarrayConstructor element
   *
   * @return FarrayConstructor element
   */
  public FarrayConstructor getFarrayConstructor();

  /**
   * Get a FarrayRef (see subarray or array element) element
   *
   * @return FarrayRef (see subarray or array element) element
   */
  public FarrayRef getFarrayRef();

  /**
   * Get the FcharacterConstant element
   *
   * @return FcharacterConstant (string) element
   */
  public FcharacterConstant getFcharacterConstant();

  /**
   * Get the FcharacterRef (see substring) element
   *
   * @return FcharacterRef (see substring) element
   */
  public FcharacterRef getFcharacterRef();

  /**
   * Get the FcoArrayRef (see coarray) element
   *
   * @return FcoArrayRef (see coarray) element
   */
  public FcoArrayRef getFcoArrayRef();

  /**
   * Get the FcomplexConstant (complex type constant) element
   *
   * @return FcomplexConstant (COMPLEX type constant) element
   */
  public FcomplexConstant getFcomplexConstant();

  /**
   * Get the FconcatExpr (concatenation of character expressions) element
   *
   * @return FconcatExpr (character expression concatenation) element
   */
  public FconcatExpr getFconcatExpr();

  /**
   * Get FdoLoop (DO type iteration) element
   *
   * @return FdoLoop (DO type iteration) element
   */
  public FdoLoop getFdoLoop();

  /**
   * Get the FintConstant (integer constant) element
   *
   * @return FintConstant (integer constant) element
   */
  public FintConstant getFintConstant();

  /**
   * Get the FlogicalConstant element
   *
   * @return FlogicalConstant element
   */
  public FlogicalConstant getFlogicalConstant();

  /**
   * Get the FmemberRef (struct member reference) element
   *
   * @return FmemberRef (see structure member) element
   */
  public FmemberRef getFmemberRef();

  /**
   * Get the FpowerExpr element
   *
   * @return FpowerExpr element
   */
  public FpowerExpr getFpowerExpr();

  /**
   * Get FrealConstant (floating point constant) element
   *
   * @return FrealConstant (floating point constant) element
   */
  public FrealConstant getFrealConstant();

  /**
   * Get the FstructConstructor element
   *
   * @return FstructConstructor element
   */
  public FstructConstructor getFstructConstructor();

  /**
   * Get a FunctionCall element
   *
   * @return FunctionCall (function / subroutine call) element
   */
  public FunctionCall getFunctionCall();

  /**
   * Get the LogAndExpr (logical product) element
   *
   * @return LogAndExpr (logical product) element
   */
  public LogAndExpr getLogAndExpr();

  /**
   * Get the LogEQExpr (equivalent) element
   *
   * @return LogEQExpr (equivalent) element
   */
  public LogEQExpr getLogEQExpr();

  /**
   * Get the LogEQVExpr (logical equivalent) element
   *
   * @return LogEQVExpr (logical equivalent) element
   */
  public LogEQVExpr getLogEQVExpr();

  /**
   * Get the LogGEExpr (greater than or equivalent) element
   *
   * @return LogGEExpr (greater than or equivalent) element
   */
  public LogGEExpr getLogGEExpr();

  /**
   * Get the LogGTExpr (greater than) element
   *
   * @return LogGTExpr (greater than) element
   */
  public LogGTExpr getLogGTExpr();

  /**
   * Get LogLEExpr (less or equivalent) element
   *
   * @return LogLEExpr (less or equivalent) element
   */
  public LogLEExpr getLogLEExpr();

  /**
   * Get the LogLTExpr (less than) element
   *
   * @return LogLTExpr (less) element
   */
  public LogLTExpr getLogLTExpr();

  /**
   * Get LogNEQExpr (non-equivalent) element
   *
   * @return LogNEQExpr (non-equivalent) element
   */
  public LogNEQExpr getLogNEQExpr();

  /**
   * Get LogNEQVExpr (logical non-equivalent) element
   *
   * @return LogNEQVExpr (logical non-equivalent) element
   */
  public LogNEQVExpr getLogNEQVExpr();

  /**
   * Get the LogNotExpr (logical negation) element
   *
   * @return LogNotExpr (logical negation) element
   */
  public LogNotExpr getLogNotExpr();

  /**
   * Get the LogOrExpr (OR) element
   *
   * @return LogOrExpr (logical sum) element
   */
  public LogOrExpr getLogOrExpr();

  /**
   * Get the MinusExpr (subtraction) element
   *
   * @return MinusExpr (subtraction) element
   */
  public MinusExpr getMinusExpr();

  /**
   * Get the MulExpr (multiplication) element
   *
   * @return MulExpr (multiplication) element
   */
  public MulExpr getMulExpr();

  /**
   * Get the PlusExpr element
   *
   * @return PlusExpr (addition) element
   */
  public PlusExpr getPlusExpr();

  /**
   * Get the UnaryMinusExpr (sign inversion) element
   *
   * @return UnaryMinusExpr (sign inversion) element
   */
  public UnaryMinusExpr getUnaryMinusExpr();

  /**
   * Get UserBinaryExpr (INTERFACE dependent ternary operator) element
   *
   * @return UserBinaryExpr (INTERFACE dependent ternary operator) element
   */
  public UserBinaryExpr getUserBinaryExpr();

  /**
   * Get the UserUnaryExpr (INTERFACE dependent unary operator) element
   *
   * @return UserUnaryExpr (INTERFACE dependent unary operator) element
   */
  public UserUnaryExpr getUserUnaryExpr();

  /**
   * Get Var (variable name) element
   *
   * @return Var (variable name) element
   */
  public Var getVar();

  /**
   * Get VarRef (variable reference) element
   *
   * @return VarRef (variable reference) element
   */
  public VarRef getVarRef();

  /**
   * Set the DivExpr element
   *
   * @param value DivExpr (division) element
   */
  public void setDivExpr(DivExpr value);

  /**
   * Get the FarrayConstructor element
   *
   * @param value FarrayConstructor
   */
  public void setFarrayConstructor(FarrayConstructor value);

  /**
   * Get a FarrayRef (see subarray or array element) element
   *
   * @param value FarrayRef (see subarray or array element) element
   */
  public void setFarrayRef(FarrayRef value);

  /**
   * Get the FcharacterConstant element
   *
   * @param value FcharacterConstant element
   */
  public void setFcharacterConstant(FcharacterConstant value);

  /**
   * Get the FcharacterRef (see substring) element
   *
   * @param value FcharacterRef (see substring) element
   */
  public void setFcharacterRef(FcharacterRef value);

  /**
   * Get the FcoArrayRef (see coarray) element
   *
   * @param value FcoArrayRef (see coarray) element
   */
  public void setFcoArrayRef(FcoArrayRef value);

  /**
   * Get the FcomplexConstant (complex type constant) element
   *
   * @param value FcomplexConstant (COMPLEX type constant) element
   */
  public void setFcomplexConstant(FcomplexConstant value);

  /**
   * Get the FconcatExpr (concatenation of character expressions) element
   *
   * @param value FconcatExpr (concatenation of character expressions) element
   */
  public void setFconcatExpr(FconcatExpr value);

  /**
   * Get FdoLoop (DO type iteration) element
   *
   * @param value FdoLoop (DO type iteration) element
   */
  public void setFdoLoop(FdoLoop value);

  /**
   * Get the FintConstant (integer constant) element
   *
   * @param value FintConstant (integer constant) element
   */
  public void setFintConstant(FintConstant value);

  /**
   * Get the FlogicalConstant element
   *
   * @param value FlogicalConstant element
   */
  public void setFlogicalConstant(FlogicalConstant value);

  /**
   * Get the FmemberRef (struct member reference) element
   *
   * @param value FmemberRef (see structure member) element
   */
  public void setFmemberRef(FmemberRef value);

  /**
   * Get the FpowerExpr element
   *
   * @param value FpowerExpr element
   */
  public void setFpowerExpr(FpowerExpr value);

  /**
   * Get FrealConstant (floating point constant) element
   *
   * @param value FrealConstant (floating point constant) element
   */
  public void setFrealConstant(FrealConstant value);

  /**
   * Get the FstructConstructor element
   *
   * @param value FstructConstructor element
   */
  public void setFstructConstructor(FstructConstructor value);

  /**
   * Get a FunctionCall element
   *
   * @param value FunctionCall (function / subroutine call) element
   */
  public void setFunctionCall(FunctionCall value);

  /**
   * Get the LogAndExpr (logical product) element
   *
   * @param value LogAndExpr (logical product) element
   */
  public void setLogAndExpr(LogAndExpr value);

  /**
   * Get the LogEQExpr (equivalent) element
   *
   * @param value LogEQExpr (equivalent) element
   */
  public void setLogEQExpr(LogEQExpr value);

  /**
   * Get the LogEQVExpr (logical equivalent) element
   *
   * @param value LogEQVExpr (logical equivalent) element
   */
  public void setLogEQVExpr(LogEQVExpr value);

  /**
   * Get the LogGEExpr (greater than or equivalent) element
   *
   * @param value LogGEExpr (greater than or equivalent) element
   */
  public void setLogGEExpr(LogGEExpr value);

  /**
   * Get the LogGTExpr (greater than) element
   *
   * @param value LogGTExpr (greater than) element
   */
  public void setLogGTExpr(LogGTExpr value);

  /**
   * Get LogLEExpr (less or equivalent) element
   *
   * @param value LogLEExpr (less or equivalent) element
   */
  public void setLogLEExpr(LogLEExpr value);

  /**
   * Get the LogLTExpr (less than) element
   *
   * @param value LogLTExpr (less than) element
   */
  public void setLogLTExpr(LogLTExpr value);

  /**
   * Get LogNEQExpr (non-equivalent) element
   *
   * @param value LogNEQExpr (non-equivalent) element
   */
  public void setLogNEQExpr(LogNEQExpr value);

  /**
   * Get LogNEQVExpr (logical non-equivalent) element
   *
   * @param value LogNEQVExpr (logical non-equivalent) element
   */
  public void setLogNEQVExpr(LogNEQVExpr value);

  /**
   * Get the LogNotExpr (logical negation) element
   *
   * @param value LogNotExpr (logical negation) element
   */
  public void setLogNotExpr(LogNotExpr value);

  /**
   * Get the LogOrExpr (OR) element
   *
   * @param value LogOrExpr (logical sum) element
   */
  public void setLogOrExpr(LogOrExpr value);

  /**
   * Get the MinusExpr (subtraction) element
   *
   * @param value MinusExpr (subtraction) element
   */
  public void setMinusExpr(MinusExpr value);

  /**
   * Get the MulExpr (multiplication) element
   *
   * @param value MulExpr (multiplication) element
   */
  public void setMulExpr(MulExpr value);

  /**
   * Get the PlusExpr element
   *
   * @param value PlusExpr (addition) element
   */
  public void setPlusExpr(PlusExpr value);

  /**
   * Get the UnaryMinusExpr (sign inversion) element
   *
   * @param value UnaryMinusExpr (sign inversion) element
   */
  public void setUnaryMinusExpr(UnaryMinusExpr value);

  /**
   * Get UserBinaryExpr (INTERFACE dependent ternary operator) element
   *
   * @param value UserBinaryExpr (INTERFACE dependent ternary operator) element
   */
  public void setUserBinaryExpr(UserBinaryExpr value);

  /**
   * Get the UserUnaryExpr (INTERFACE dependent unary operator) element
   *
   * @param value UserUnaryExpr (INTERFACE dependent unary operator) element
   */
  public void setUserUnaryExpr(UserUnaryExpr value);

  /**
   * Get Var (variable name) element
   *
   * @param value Var (variable name) element
   */
  public void setVar(Var value);

  /**
   * Get VarRef (variable reference) element
   *
   * @param value VarRef (variable reference) element
   */
  public void setVarRef(VarRef value);
}
