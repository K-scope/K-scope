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
 * XcodeML node discovery interface
 *
 * @author RIKEN
 */
public interface IXmlVisitor {
  /**
   * Visits this node for enter behavior.
   *
   * @param visitable XcodeProgram
   * @return boolean
   */
  public boolean enter(XcodeProgram visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable XcodeProgram
   */
  void leave(XcodeProgram visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable TypeTable
   * @return boolean
   */
  boolean enter(TypeTable visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable TypeTable
   */
  void leave(TypeTable visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FfunctionType
   * @return boolean
   */
  boolean enter(FfunctionType visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FfunctionType
   */
  void leave(FfunctionType visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Params
   * @return boolean
   */
  boolean enter(Params visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Params
   */
  void leave(Params visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Name
   * @return boolean
   */
  boolean enter(Name visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Name
   */
  void leave(Name visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FbasicType
   * @return boolean
   */
  boolean enter(FbasicType visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FbasicType
   */
  void leave(FbasicType visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Kind
   * @return boolean
   */
  boolean enter(Kind visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Kind
   */
  void leave(Kind visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FunctionCall
   * @return boolean
   */
  boolean enter(FunctionCall visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FunctionCall
   */
  void leave(FunctionCall visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Arguments
   * @return boolean
   */
  boolean enter(Arguments visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Arguments
   */
  void leave(Arguments visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcharacterRef
   * @return boolean
   */
  boolean enter(FcharacterRef visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcharacterRef
   */
  void leave(FcharacterRef visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable VarRef
   * @return boolean
   */
  boolean enter(VarRef visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable VarRef
   */
  void leave(VarRef visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FarrayRef
   * @return boolean
   */
  boolean enter(FarrayRef visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FarrayRef
   */
  void leave(FarrayRef visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable IndexRange
   * @return boolean
   */
  boolean enter(IndexRange visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable IndexRange
   */
  void leave(IndexRange visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LowerBound
   * @return boolean
   */
  boolean enter(LowerBound visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LowerBound
   */
  void leave(LowerBound visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable UpperBound
   * @return boolean
   */
  boolean enter(UpperBound visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable UpperBound
   */
  void leave(UpperBound visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Step
   * @return boolean
   */
  boolean enter(Step visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Step
   */
  void leave(Step visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable ArrayIndex
   * @return boolean
   */
  boolean enter(ArrayIndex visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable ArrayIndex
   */
  void leave(ArrayIndex visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FarrayConstructor
   * @return boolean
   */
  boolean enter(FarrayConstructor visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FarrayConstructor
   */
  void leave(FarrayConstructor visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FmemberRef
   * @return boolean
   */
  boolean enter(FmemberRef visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FmemberRef
   */
  void leave(FmemberRef visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcoArrayRef
   * @return boolean
   */
  boolean enter(FcoArrayRef visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable
   */
  void leave(FcoArrayRef visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Var
   * @return boolean
   */
  boolean enter(Var visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Var
   */
  void leave(Var visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdoLoop
   * @return boolean
   */
  boolean enter(FdoLoop visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdoLoop
   */
  void leave(FdoLoop visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Value
   * @return boolean
   */
  boolean enter(Value visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Value
   */
  void leave(Value visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable RepeatCount
   * @return boolean
   */
  boolean enter(RepeatCount visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable RepeatCount
   */
  void leave(RepeatCount visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable NamedValue
   * @return boolean
   */
  boolean enter(NamedValue visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable NamedValue
   */
  void leave(NamedValue visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FintConstant
   * @return boolean
   */
  boolean enter(FintConstant visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FintConstant
   */
  void leave(FintConstant visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FrealConstant
   * @return boolean
   */
  boolean enter(FrealConstant visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FrealConstant
   */
  void leave(FrealConstant visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcharacterConstant
   * @return boolean
   */
  boolean enter(FcharacterConstant visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcharacterConstant
   */
  void leave(FcharacterConstant visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FlogicalConstant
   * @return boolean
   */
  boolean enter(FlogicalConstant visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FlogicalConstant
   */
  void leave(FlogicalConstant visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcomplexConstant
   * @return boolean
   */
  boolean enter(FcomplexConstant visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcomplexConstant
   */
  void leave(FcomplexConstant visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FstructConstructor
   * @return boolean
   */
  boolean enter(FstructConstructor visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FstructConstructor
   */
  void leave(FstructConstructor visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable PlusExpr
   * @return boolean
   */
  boolean enter(PlusExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable PlusExpr
   */
  void leave(PlusExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable MulExpr
   * @return boolean
   */
  boolean enter(MulExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable MulExpr
   */
  void leave(MulExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogNEQExpr
   * @return boolean
   */
  boolean enter(LogNEQExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogNEQExpr
   */
  void leave(LogNEQExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogGEExpr
   * @return boolean
   */
  boolean enter(LogGEExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogGEExpr
   */
  void leave(LogGEExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogOrExpr
   * @return boolean
   */
  boolean enter(LogOrExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogOrExpr
   */
  void leave(LogOrExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable UserBinaryExpr
   * @return boolean
   */
  boolean enter(UserBinaryExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable UserBinaryExpr
   */
  void leave(UserBinaryExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable UserUnaryExpr
   * @return boolean
   */
  boolean enter(UserUnaryExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable UserUnaryExpr
   */
  void leave(UserUnaryExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Ffunction
   * @return boolean
   */
  boolean enter(Ffunction visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Ffunction
   */
  void leave(Ffunction visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable DivExpr
   * @return boolean
   */
  boolean enter(DivExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable DivExpr
   */
  void leave(DivExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogEQVExpr
   * @return boolean
   */
  boolean enter(LogEQVExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogEQVExpr
   */
  void leave(LogEQVExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable MinusExpr
   * @return boolean
   */
  boolean enter(MinusExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable MinusExpr
   */
  void leave(MinusExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FpowerExpr
   * @return boolean
   */
  boolean enter(FpowerExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FpowerExpr
   */
  void leave(FpowerExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FconcatExpr
   * @return boolean
   */
  boolean enter(FconcatExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FconcatExpr
   */
  void leave(FconcatExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogEQExpr
   * @return boolean
   */
  boolean enter(LogEQExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogEQExpr
   */
  void leave(LogEQExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogGTExpr
   * @return boolean
   */
  boolean enter(LogGTExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogGTExpr
   */
  void leave(LogGTExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogLEExpr
   * @return boolean
   */
  boolean enter(LogLEExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogLEExpr
   */
  void leave(LogLEExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogLTExpr
   * @return boolean
   */
  boolean enter(LogLTExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogLTExpr
   */
  void leave(LogLTExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogAndExpr
   * @return boolean
   */
  boolean enter(LogAndExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogAndExpr
   */
  void leave(LogAndExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogNEQVExpr
   * @return boolean
   */
  boolean enter(LogNEQVExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogNEQVExpr
   */
  void leave(LogNEQVExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable UnaryMinusExpr
   * @return boolean
   */
  boolean enter(UnaryMinusExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable UnaryMinusExpr
   */
  void leave(UnaryMinusExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable LogNotExpr
   * @return boolean
   */
  boolean enter(LogNotExpr visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable LogNotExpr
   */
  void leave(LogNotExpr visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Len
   * @return boolean
   */
  boolean enter(Len visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Len
   */
  void leave(Len visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable CoShape
   * @return boolean
   */
  boolean enter(CoShape visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable CoShape
   */
  void leave(CoShape visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FstructType
   * @return boolean
   */
  boolean enter(FstructType visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FstructType
   */
  void leave(FstructType visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Symbols
   * @return boolean
   */
  boolean enter(Symbols visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Symbols
   */
  void leave(Symbols visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Id
   * @return boolean
   */
  boolean enter(Id visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Id
   */
  void leave(Id visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable GlobalSymbols
   * @return boolean
   */
  boolean enter(GlobalSymbols visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable GlobalSymbols
   */
  void leave(GlobalSymbols visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable GlobalDeclarations
   * @return boolean
   */
  boolean enter(GlobalDeclarations visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable GlobalDeclarations
   */
  void leave(GlobalDeclarations visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FfunctionDefinition
   * @return boolean
   */
  boolean enter(FfunctionDefinition visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FfunctionDefinition
   */
  void leave(FfunctionDefinition visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Declarations
   * @return boolean
   */
  boolean enter(Declarations visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Declarations
   */
  void leave(Declarations visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FinterfaceDecl
   * @return boolean
   */
  boolean enter(FinterfaceDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FinterfaceDecl
   */
  void leave(FinterfaceDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FfunctionDecl
   * @return boolean
   */
  boolean enter(FfunctionDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FfunctionDecl
   */
  void leave(FfunctionDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FmoduleProcedureDecl
   * @return boolean
   */
  boolean enter(FmoduleProcedureDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FmoduleProcedureDecl
   */
  void leave(FmoduleProcedureDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable VarDecl
   * @return boolean
   */
  boolean enter(VarDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable VarDecl
   */
  void leave(VarDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FuseDecl
   * @return boolean
   */
  boolean enter(FuseDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FuseDecl
   */
  void leave(FuseDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Rename
   * @return boolean
   */
  boolean enter(Rename visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Rename
   */
  void leave(Rename visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FuseOnlyDecl
   * @return boolean
   */
  boolean enter(FuseOnlyDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FuseOnlyDecl
   */
  void leave(FuseOnlyDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Renamable
   * @return boolean
   */
  boolean enter(Renamable visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Renamable
   */
  void leave(Renamable visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable ExternDecl
   * @return boolean
   */
  boolean enter(ExternDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable ExternDecl
   */
  void leave(ExternDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FnamelistDecl
   * @return boolean
   */
  boolean enter(FnamelistDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FnamelistDecl
   */
  void leave(FnamelistDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable VarList
   * @return boolean
   */
  boolean enter(VarList visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable VarList
   */
  void leave(VarList visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcommonDecl
   * @return boolean
   */
  boolean enter(FcommonDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcommonDecl
   */
  void leave(FcommonDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FstructDecl
   * @return boolean
   */
  boolean enter(FstructDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FstructDecl
   */
  void leave(FstructDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FentryDecl
   * @return boolean
   */
  boolean enter(FentryDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FentryDecl
   */
  void leave(FentryDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FequivalenceDecl
   * @return boolean
   */
  boolean enter(FequivalenceDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FequivalenceDecl
   */
  void leave(FequivalenceDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdataDecl
   * @return boolean
   */
  boolean enter(FdataDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdataDecl
   */
  void leave(FdataDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable ValueList
   * @return boolean
   */
  boolean enter(ValueList visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable ValueList
   */
  void leave(ValueList visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FpragmaStatement
   * @return boolean
   */
  boolean enter(FpragmaStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FpragmaStatement
   */
  void leave(FpragmaStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Body
   * @return boolean
   */
  boolean enter(Body visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Body
   */
  void leave(Body visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdoStatement
   * @return boolean
   */
  boolean enter(FdoStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdoStatement
   */
  void leave(FdoStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FselectCaseStatement
   * @return boolean
   */
  boolean enter(FselectCaseStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FselectCaseStatement
   */
  void leave(FselectCaseStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcaseLabel
   * @return boolean
   */
  boolean enter(FcaseLabel visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcaseLabel
   */
  void leave(FcaseLabel visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable GotoStatement
   * @return boolean
   */
  boolean enter(GotoStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable GotoStatement
   */
  void leave(GotoStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FstopStatement
   * @return boolean
   */
  boolean enter(FstopStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FstopStatement
   */
  void leave(FstopStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FpauseStatement
   * @return boolean
   */
  boolean enter(FpauseStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FpauseStatement
   */
  void leave(FpauseStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable ExprStatement
   * @return boolean
   */
  boolean enter(ExprStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable ExprStatement
   */
  void leave(ExprStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FifStatement
   * @return boolean
   */
  boolean enter(FifStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FifStatement
   */
  void leave(FifStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Condition
   * @return boolean
   */
  boolean enter(Condition visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Condition
   */
  void leave(Condition visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Then
   * @return boolean
   */
  boolean enter(Then visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Then
   */
  void leave(Then visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Else
   * @return boolean
   */
  boolean enter(Else visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Else
   */
  void leave(Else visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdoWhileStatement
   * @return boolean
   */
  boolean enter(FdoWhileStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdoWhileStatement
   */
  void leave(FdoWhileStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcycleStatement
   * @return boolean
   */
  boolean enter(FcycleStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcycleStatement
   */
  void leave(FcycleStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FexitStatement
   * @return boolean
   */
  boolean enter(FexitStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FexitStatement
   */
  void leave(FexitStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable StatementLabel
   * @return boolean
   */
  boolean enter(StatementLabel visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable StatementLabel
   */
  void leave(StatementLabel visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FreadStatement
   * @return boolean
   */
  boolean enter(FreadStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FreadStatement
   */
  void leave(FreadStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable NamedValueList
   * @return boolean
   */
  boolean enter(NamedValueList visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable NamedValueList
   */
  void leave(NamedValueList visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FwriteStatement
   * @return boolean
   */
  boolean enter(FwriteStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FwriteStatement
   */
  void leave(FwriteStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FprintStatement
   * @return boolean
   */
  boolean enter(FprintStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FprintStatement
   */
  void leave(FprintStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FrewindStatement
   * @return boolean
   */
  boolean enter(FrewindStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FrewindStatement
   */
  void leave(FrewindStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FendFileStatement
   * @return boolean
   */
  boolean enter(FendFileStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FendFileStatement
   */
  void leave(FendFileStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FbackspaceStatement
   * @return boolean
   */
  boolean enter(FbackspaceStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FbackspaceStatement
   */
  void leave(FbackspaceStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FopenStatement
   * @return boolean
   */
  boolean enter(FopenStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FopenStatement
   */
  void leave(FopenStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcloseStatement
   * @return boolean
   */
  boolean enter(FcloseStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcloseStatement
   */
  void leave(FcloseStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FinquireStatement
   * @return boolean
   */
  boolean enter(FinquireStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FinquireStatement
   */
  void leave(FinquireStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FformatDecl
   * @return boolean
   */
  boolean enter(FformatDecl visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FformatDecl
   */
  void leave(FformatDecl visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FallocateStatement
   * @return boolean
   */
  boolean enter(FallocateStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FallocateStatement
   */
  void leave(FallocateStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Alloc
   * @return boolean
   */
  boolean enter(Alloc visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Alloc
   */
  void leave(Alloc visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdeallocateStatement
   * @return boolean
   */
  boolean enter(FdeallocateStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdeallocateStatement
   */
  void leave(FdeallocateStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FcontainsStatement
   * @return boolean
   */
  boolean enter(FcontainsStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FcontainsStatement
   */
  void leave(FcontainsStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable ContinueStatement
   * @return boolean
   */
  boolean enter(ContinueStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable ContinueStatement
   */
  void leave(ContinueStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FreturnStatement
   * @return boolean
   */
  boolean enter(FreturnStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FreturnStatement
   */
  void leave(FreturnStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FwhereStatement
   * @return boolean
   */
  boolean enter(FwhereStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FwhereStatement
   */
  void leave(FwhereStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FnullifyStatement
   * @return boolean
   */
  boolean enter(FnullifyStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FnullifyStatement
   */
  void leave(FnullifyStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable Text
   * @return boolean
   */
  boolean enter(Text visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable Text
   */
  void leave(Text visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FassignStatement
   * @return boolean
   */
  boolean enter(FassignStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FassignStatement
   */
  void leave(FassignStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FpointerAssignStatement
   * @return boolean
   */
  boolean enter(FpointerAssignStatement visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FpointerAssignStatement
   */
  void leave(FpointerAssignStatement visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FmoduleDefinition
   * @return boolean
   */
  boolean enter(FmoduleDefinition visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FmoduleDefinition
   */
  void leave(FmoduleDefinition visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FblockDataDefinition
   * @return boolean
   */
  boolean enter(FblockDataDefinition visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FblockDataDefinition
   */
  void leave(FblockDataDefinition visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable DefModelArraySubscriptSequence
   * @return boolean
   */
  boolean enter(DefModelArraySubscriptSequence visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable DefModelArraySubscriptSequence
   */
  void leave(DefModelArraySubscriptSequence visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FequivalenceDeclSequence
   * @return boolean
   */
  boolean enter(FequivalenceDeclSequence visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FequivalenceDeclSequence
   */
  void leave(FequivalenceDeclSequence visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdataDeclSequence
   * @return boolean
   */
  boolean enter(FdataDeclSequence visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdataDeclSequence
   */
  void leave(FdataDeclSequence visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable FdoStatementSequence
   * @return boolean
   */
  boolean enter(FdoStatementSequence visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable FdoStatementSequence
   */
  void leave(FdoStatementSequence visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable GotoStatementSequence
   * @return boolean
   */
  boolean enter(GotoStatementSequence visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable GotoStatementSequence
   */
  void leave(GotoStatementSequence visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable DefModelBinaryOperation
   * @return boolean
   */
  public boolean enter(DefModelBinaryOperation visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable DefModelBinaryOperation
   */
  public void leave(DefModelBinaryOperation visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable DefModelExprList
   * @return boolean
   */
  public boolean enter(DefModelExprList visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable DefModelExprList
   */
  public void leave(DefModelExprList visitable);

  /**
   * Visits this node for enter behavior.
   *
   * @param visitable DefModelUnaryOperation
   * @return boolean
   */
  public boolean enter(DefModelUnaryOperation visitable);

  /**
   * Visits this node for leave behavior.
   *
   * @param visitable DefModelUnaryOperation
   */
  public void leave(DefModelUnaryOperation visitable);
}
