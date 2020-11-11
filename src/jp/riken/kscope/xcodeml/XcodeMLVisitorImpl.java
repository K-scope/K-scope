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
package jp.riken.kscope.xcodeml;

import jp.riken.kscope.xcodeml.xml.DefModelArraySubscriptSequence;
import jp.riken.kscope.xcodeml.xml.FdataDeclSequence;
import jp.riken.kscope.xcodeml.xml.FdoStatementSequence;
import jp.riken.kscope.xcodeml.xml.FequivalenceDeclSequence;
import jp.riken.kscope.xcodeml.xml.GotoStatementSequence;
import jp.riken.kscope.xcodeml.xml.IXmlVisitor;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * XcodeML node search class
 *
 * @author RIKEN
 */
public class XcodeMLVisitorImpl implements IXmlVisitor {

  @Override
  public boolean enter(XcodeProgram visitable) {

    return true;
  }

  @Override
  public boolean enter(TypeTable visitable) {

    return true;
  }

  @Override
  public boolean enter(FfunctionType visitable) {

    return true;
  }

  @Override
  public boolean enter(Params visitable) {

    return true;
  }

  @Override
  public boolean enter(Name visitable) {

    return true;
  }

  @Override
  public boolean enter(FbasicType visitable) {

    return true;
  }

  @Override
  public boolean enter(Kind visitable) {

    return true;
  }

  @Override
  public boolean enter(FunctionCall visitable) {

    return true;
  }

  @Override
  public boolean enter(Arguments visitable) {

    return true;
  }

  @Override
  public boolean enter(FcharacterRef visitable) {

    return true;
  }

  @Override
  public boolean enter(VarRef visitable) {

    return true;
  }

  @Override
  public boolean enter(FarrayRef visitable) {

    return true;
  }

  @Override
  public boolean enter(IndexRange visitable) {

    return true;
  }

  @Override
  public boolean enter(LowerBound visitable) {

    return true;
  }

  @Override
  public boolean enter(UpperBound visitable) {

    return true;
  }

  @Override
  public boolean enter(Step visitable) {

    return true;
  }

  @Override
  public boolean enter(ArrayIndex visitable) {

    return true;
  }

  @Override
  public boolean enter(FarrayConstructor visitable) {

    return true;
  }

  @Override
  public boolean enter(FmemberRef visitable) {

    return true;
  }

  @Override
  public boolean enter(FcoArrayRef visitable) {

    return true;
  }

  @Override
  public boolean enter(Var visitable) {

    return true;
  }

  @Override
  public boolean enter(FdoLoop visitable) {

    return true;
  }

  @Override
  public boolean enter(Value visitable) {

    return true;
  }

  @Override
  public boolean enter(RepeatCount visitable) {

    return true;
  }

  @Override
  public boolean enter(NamedValue visitable) {

    return true;
  }

  @Override
  public boolean enter(FintConstant visitable) {

    return true;
  }

  @Override
  public boolean enter(FrealConstant visitable) {

    return true;
  }

  @Override
  public boolean enter(FcharacterConstant visitable) {

    return true;
  }

  @Override
  public boolean enter(FlogicalConstant visitable) {

    return true;
  }

  @Override
  public boolean enter(FcomplexConstant visitable) {

    return true;
  }

  @Override
  public boolean enter(FstructConstructor visitable) {

    return true;
  }

  @Override
  public boolean enter(PlusExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(MulExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogNEQExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogGEExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogOrExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(UserBinaryExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(UserUnaryExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(Ffunction visitable) {

    return true;
  }

  @Override
  public boolean enter(DivExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogEQVExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(MinusExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(FpowerExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(FconcatExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogEQExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogGTExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogLEExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogLTExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogAndExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogNEQVExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(UnaryMinusExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(LogNotExpr visitable) {

    return true;
  }

  @Override
  public boolean enter(Len visitable) {

    return true;
  }

  @Override
  public boolean enter(CoShape visitable) {

    return true;
  }

  @Override
  public boolean enter(FstructType visitable) {

    return true;
  }

  @Override
  public boolean enter(Symbols visitable) {

    return true;
  }

  @Override
  public boolean enter(Id visitable) {

    return true;
  }

  @Override
  public boolean enter(GlobalSymbols visitable) {

    return true;
  }

  @Override
  public boolean enter(GlobalDeclarations visitable) {

    return true;
  }

  @Override
  public boolean enter(FfunctionDefinition visitable) {

    return true;
  }

  @Override
  public boolean enter(Declarations visitable) {

    return true;
  }

  @Override
  public boolean enter(FinterfaceDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FfunctionDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FmoduleProcedureDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(VarDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FuseDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(Rename visitable) {

    return true;
  }

  @Override
  public boolean enter(FuseOnlyDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(Renamable visitable) {

    return true;
  }

  @Override
  public boolean enter(ExternDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FnamelistDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(VarList visitable) {

    return true;
  }

  @Override
  public boolean enter(FcommonDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FstructDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FentryDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FequivalenceDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FdataDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(ValueList visitable) {

    return true;
  }

  @Override
  public boolean enter(FpragmaStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(Body visitable) {

    return true;
  }

  @Override
  public boolean enter(FdoStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FselectCaseStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FcaseLabel visitable) {

    return true;
  }

  @Override
  public boolean enter(GotoStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FstopStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FpauseStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(ExprStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FifStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(Condition visitable) {

    return true;
  }

  @Override
  public boolean enter(Then visitable) {

    return true;
  }

  @Override
  public boolean enter(Else visitable) {

    return true;
  }

  @Override
  public boolean enter(FdoWhileStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FcycleStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FexitStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(StatementLabel visitable) {

    return true;
  }

  @Override
  public boolean enter(FreadStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(NamedValueList visitable) {

    return true;
  }

  @Override
  public boolean enter(FwriteStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FprintStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FrewindStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FendFileStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FbackspaceStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FopenStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FcloseStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FinquireStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FformatDecl visitable) {

    return true;
  }

  @Override
  public boolean enter(FallocateStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(Alloc visitable) {

    return true;
  }

  @Override
  public boolean enter(FdeallocateStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FcontainsStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(ContinueStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FreturnStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FwhereStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FnullifyStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(Text visitable) {

    return true;
  }

  @Override
  public boolean enter(FassignStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FpointerAssignStatement visitable) {

    return true;
  }

  @Override
  public boolean enter(FmoduleDefinition visitable) {

    return true;
  }

  @Override
  public boolean enter(FblockDataDefinition visitable) {

    return true;
  }

  @Override
  public void leave(XcodeProgram visitable) {}

  @Override
  public void leave(TypeTable visitable) {}

  @Override
  public void leave(FfunctionType visitable) {}

  @Override
  public void leave(Params visitable) {}

  @Override
  public void leave(Name visitable) {}

  @Override
  public void leave(FbasicType visitable) {}

  @Override
  public void leave(Kind visitable) {}

  @Override
  public void leave(FunctionCall visitable) {}

  @Override
  public void leave(Arguments visitable) {}

  @Override
  public void leave(FcharacterRef visitable) {}

  @Override
  public void leave(VarRef visitable) {}

  @Override
  public void leave(FarrayRef visitable) {}

  @Override
  public void leave(IndexRange visitable) {}

  @Override
  public void leave(LowerBound visitable) {}

  @Override
  public void leave(UpperBound visitable) {}

  @Override
  public void leave(Step visitable) {}

  @Override
  public void leave(ArrayIndex visitable) {}

  @Override
  public void leave(FarrayConstructor visitable) {}

  @Override
  public void leave(FmemberRef visitable) {}

  @Override
  public void leave(FcoArrayRef visitable) {}

  @Override
  public void leave(Var visitable) {}

  @Override
  public void leave(FdoLoop visitable) {}

  @Override
  public void leave(Value visitable) {}

  @Override
  public void leave(RepeatCount visitable) {}

  @Override
  public void leave(NamedValue visitable) {}

  @Override
  public void leave(FintConstant visitable) {}

  @Override
  public void leave(FrealConstant visitable) {}

  @Override
  public void leave(FcharacterConstant visitable) {}

  @Override
  public void leave(FlogicalConstant visitable) {}

  @Override
  public void leave(FcomplexConstant visitable) {}

  @Override
  public void leave(FstructConstructor visitable) {}

  @Override
  public void leave(PlusExpr visitable) {}

  @Override
  public void leave(MulExpr visitable) {}

  @Override
  public void leave(LogNEQExpr visitable) {}

  @Override
  public void leave(LogGEExpr visitable) {}

  @Override
  public void leave(LogOrExpr visitable) {}

  @Override
  public void leave(UserBinaryExpr visitable) {}

  @Override
  public void leave(UserUnaryExpr visitable) {}

  @Override
  public void leave(Ffunction visitable) {}

  @Override
  public void leave(DivExpr visitable) {}

  @Override
  public void leave(LogEQVExpr visitable) {}

  @Override
  public void leave(MinusExpr visitable) {}

  @Override
  public void leave(FpowerExpr visitable) {}

  @Override
  public void leave(FconcatExpr visitable) {}

  @Override
  public void leave(LogEQExpr visitable) {}

  @Override
  public void leave(LogGTExpr visitable) {}

  @Override
  public void leave(LogLEExpr visitable) {}

  @Override
  public void leave(LogLTExpr visitable) {}

  @Override
  public void leave(LogAndExpr visitable) {}

  @Override
  public void leave(LogNEQVExpr visitable) {}

  @Override
  public void leave(UnaryMinusExpr visitable) {}

  @Override
  public void leave(LogNotExpr visitable) {}

  @Override
  public void leave(Len visitable) {}

  @Override
  public void leave(CoShape visitable) {}

  @Override
  public void leave(FstructType visitable) {}

  @Override
  public void leave(Symbols visitable) {}

  @Override
  public void leave(Id visitable) {}

  @Override
  public void leave(GlobalSymbols visitable) {}

  @Override
  public void leave(GlobalDeclarations visitable) {}

  @Override
  public void leave(FfunctionDefinition visitable) {}

  @Override
  public void leave(Declarations visitable) {}

  @Override
  public void leave(FinterfaceDecl visitable) {}

  @Override
  public void leave(FfunctionDecl visitable) {}

  @Override
  public void leave(FmoduleProcedureDecl visitable) {}

  @Override
  public void leave(VarDecl visitable) {}

  @Override
  public void leave(FuseDecl visitable) {}

  @Override
  public void leave(Rename visitable) {}

  @Override
  public void leave(FuseOnlyDecl visitable) {}

  @Override
  public void leave(Renamable visitable) {}

  @Override
  public void leave(ExternDecl visitable) {}

  @Override
  public void leave(FnamelistDecl visitable) {}

  @Override
  public void leave(VarList visitable) {}

  @Override
  public void leave(FcommonDecl visitable) {}

  @Override
  public void leave(FstructDecl visitable) {}

  @Override
  public void leave(FentryDecl visitable) {}

  @Override
  public void leave(FequivalenceDecl visitable) {}

  @Override
  public void leave(FdataDecl visitable) {}

  @Override
  public void leave(ValueList visitable) {}

  @Override
  public void leave(FpragmaStatement visitable) {}

  @Override
  public void leave(Body visitable) {}

  @Override
  public void leave(FdoStatement visitable) {}

  @Override
  public void leave(FselectCaseStatement visitable) {}

  @Override
  public void leave(FcaseLabel visitable) {}

  @Override
  public void leave(GotoStatement visitable) {}

  @Override
  public void leave(FstopStatement visitable) {}

  @Override
  public void leave(FpauseStatement visitable) {}

  @Override
  public void leave(ExprStatement visitable) {}

  @Override
  public void leave(FifStatement visitable) {}

  @Override
  public void leave(Condition visitable) {}

  @Override
  public void leave(Then visitable) {}

  @Override
  public void leave(Else visitable) {}

  @Override
  public void leave(FdoWhileStatement visitable) {}

  @Override
  public void leave(FcycleStatement visitable) {}

  @Override
  public void leave(FexitStatement visitable) {}

  @Override
  public void leave(StatementLabel visitable) {}

  @Override
  public void leave(FreadStatement visitable) {}

  @Override
  public void leave(NamedValueList visitable) {}

  @Override
  public void leave(FwriteStatement visitable) {}

  @Override
  public void leave(FprintStatement visitable) {}

  @Override
  public void leave(FrewindStatement visitable) {}

  @Override
  public void leave(FendFileStatement visitable) {}

  @Override
  public void leave(FbackspaceStatement visitable) {}

  @Override
  public void leave(FopenStatement visitable) {}

  @Override
  public void leave(FcloseStatement visitable) {}

  @Override
  public void leave(FinquireStatement visitable) {}

  @Override
  public void leave(FformatDecl visitable) {}

  @Override
  public void leave(FallocateStatement visitable) {}

  @Override
  public void leave(Alloc visitable) {}

  @Override
  public void leave(FdeallocateStatement visitable) {}

  @Override
  public void leave(FcontainsStatement visitable) {}

  @Override
  public void leave(ContinueStatement visitable) {}

  @Override
  public void leave(FreturnStatement visitable) {}

  @Override
  public void leave(FwhereStatement visitable) {}

  @Override
  public void leave(FnullifyStatement visitable) {}

  @Override
  public void leave(Text visitable) {}

  @Override
  public void leave(FassignStatement visitable) {}

  @Override
  public void leave(FpointerAssignStatement visitable) {}

  @Override
  public void leave(FmoduleDefinition visitable) {}

  @Override
  public void leave(FblockDataDefinition visitable) {}

  @Override
  public boolean enter(DefModelArraySubscriptSequence visitable) {
    return true;
  }

  @Override
  public boolean enter(FdataDeclSequence visitable) {
    return true;
  }

  @Override
  public boolean enter(FdoStatementSequence visitable) {
    return true;
  }

  @Override
  public boolean enter(FequivalenceDeclSequence visitable) {
    return true;
  }

  @Override
  public boolean enter(GotoStatementSequence visitable) {
    return true;
  }

  @Override
  public void leave(DefModelArraySubscriptSequence visitable) {}

  @Override
  public void leave(FdataDeclSequence visitable) {}

  @Override
  public void leave(FdoStatementSequence visitable) {}

  @Override
  public void leave(FequivalenceDeclSequence visitable) {}

  @Override
  public void leave(GotoStatementSequence visitable) {}

  @Override
  public boolean enter(DefModelBinaryOperation visitable) {
    return true;
  }

  @Override
  public boolean enter(DefModelExprList visitable) {
    return true;
  }

  @Override
  public boolean enter(DefModelUnaryOperation visitable) {
    return true;
  }

  @Override
  public void leave(DefModelBinaryOperation visitable) {}

  @Override
  public void leave(DefModelExprList visitable) {}

  @Override
  public void leave(DefModelUnaryOperation visitable) {}
}
