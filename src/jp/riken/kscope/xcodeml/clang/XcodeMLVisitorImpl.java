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
package jp.riken.kscope.xcodeml.clang;

import jp.riken.kscope.xcodeml.clang.xml.IXmlVisitor;
import jp.riken.kscope.xcodeml.clang.xml.gen.*;

/**
 * XcodeMLノード探索クラス
 * @author RIKEN
 */
public class XcodeMLVisitorImpl implements IXmlVisitor {

    @Override
    public boolean enter(XcodeProgram visitable) {
        return true;
    }

    @Override
    public void leave(XcodeProgram visitable) {
        return;
    }

    @Override
    public boolean enter(TypeTable visitable) {
        return true;
    }

    @Override
    public void leave(TypeTable visitable) {
        return;
    }

    @Override
    public boolean enter(FunctionType visitable) {
        return true;
    }

    @Override
    public void leave(FunctionType visitable) {
        return;
    }

    @Override
    public boolean enter(GccAttributes visitable) {
        return true;
    }

    @Override
    public void leave(GccAttributes visitable) {
        return;
    }

    @Override
    public boolean enter(GccAttribute visitable) {
        return true;
    }

    @Override
    public void leave(GccAttribute visitable) {
        return;
    }

    @Override
    public boolean enter(BuiltinOp visitable) {
        return true;
    }

    @Override
    public void leave(BuiltinOp visitable) {
        return;
    }

    @Override
    public boolean enter(ArrayRef visitable) {
        return true;
    }

    @Override
    public void leave(ArrayRef visitable) {
        return;
    }

    @Override
    public boolean enter(ArrayAddr visitable) {
        return true;
    }

    @Override
    public void leave(ArrayAddr visitable) {
        return;
    }

    @Override
    public boolean enter(FunctionCall visitable) {
        return true;
    }

    @Override
    public void leave(FunctionCall visitable) {
        return;
    }

    @Override
    public boolean enter(Function visitable) {
        return true;
    }

    @Override
    public void leave(Function visitable) {
        return;
    }

    @Override
    public boolean enter(Arguments visitable) {
        return true;
    }

    @Override
    public void leave(Arguments visitable) {
        return;
    }

    @Override
    public boolean enter(GccCompoundExpr visitable) {
        return true;
    }

    @Override
    public void leave(GccCompoundExpr visitable) {
        return;
    }

    @Override
    public boolean enter(CompoundStatement visitable) {
        return true;
    }

    @Override
    public void leave(CompoundStatement visitable) {
        return;
    }

    @Override
    public boolean enter(Symbols visitable) {
        return true;
    }

    @Override
    public void leave(Symbols visitable) {
        return;
    }

    @Override
    public boolean enter(Id visitable) {
        return true;
    }

    @Override
    public void leave(Id visitable) {
        return;
    }

    @Override
    public boolean enter(Name visitable) {
        return true;
    }

    @Override
    public void leave(Name visitable) {
        return;
    }

    @Override
    public boolean enter(Value visitable) {
        return true;
    }

    @Override
    public void leave(Value visitable) {
        return;
    }

    @Override
    public boolean enter(SubArrayRef visitable) {
        return true;
    }

    @Override
    public void leave(SubArrayRef visitable) {
        return;
    }

    @Override
    public boolean enter(CoArrayRef visitable) {
        return true;
    }

    @Override
    public void leave(CoArrayRef visitable) {
        return;
    }

    @Override
    public boolean enter(Var visitable) {
        return true;
    }

    @Override
    public void leave(Var visitable) {
        return;
    }

    @Override
    public boolean enter(MemberRef visitable) {
        return true;
    }

    @Override
    public void leave(MemberRef visitable) {
        return;
    }

    @Override
    public boolean enter(CastExpr visitable) {
        return true;
    }

    @Override
    public void leave(CastExpr visitable) {
        return;
    }

    @Override
    public boolean enter(StringConstant visitable) {
        return true;
    }

    @Override
    public void leave(StringConstant visitable) {
        return;
    }

    @Override
    public boolean enter(VarAddr visitable) {
        return true;
    }

    @Override
    public void leave(VarAddr visitable) {
        return;
    }

    @Override
    public boolean enter(CompoundValueExpr visitable) {
        return true;
    }

    @Override
    public void leave(CompoundValueExpr visitable) {
        return;
    }

    @Override
    public boolean enter(CompoundValueAddr visitable) {
        return true;
    }

    @Override
    public void leave(CompoundValueAddr visitable) {
        return;
    }

    @Override
    public boolean enter(XmpDescOf visitable) {
        return true;
    }

    @Override
    public void leave(XmpDescOf visitable) {
        return;
    }

    @Override
    public boolean enter(IntConstant visitable) {
        return true;
    }

    @Override
    public void leave(IntConstant visitable) {
        return;
    }

    @Override
    public boolean enter(FloatConstant visitable) {
        return true;
    }

    @Override
    public void leave(FloatConstant visitable) {
        return;
    }

    @Override
    public boolean enter(LonglongConstant visitable) {
        return true;
    }

    @Override
    public void leave(LonglongConstant visitable) {
        return;
    }

    @Override
    public boolean enter(MoeConstant visitable) {
        return true;
    }

    @Override
    public void leave(MoeConstant visitable) {
        return;
    }

    @Override
    public boolean enter(FuncAddr visitable) {
        return true;
    }

    @Override
    public void leave(FuncAddr visitable) {
        return;
    }

    @Override
    public boolean enter(SizeOfExpr visitable) {
        return true;
    }

    @Override
    public void leave(SizeOfExpr visitable) {
        return;
    }

    @Override
    public boolean enter(CoArrayAssignExpr visitable) {
        return true;
    }

    @Override
    public void leave(CoArrayAssignExpr visitable) {
        return;
    }

    @Override
    public boolean enter(ModExpr visitable) {
        return true;
    }

    @Override
    public void leave(ModExpr visitable) {
        return;
    }

    @Override
    public boolean enter(BitOrExpr visitable) {
        return true;
    }

    @Override
    public void leave(BitOrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogOrExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogOrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(PlusExpr visitable) {
        return true;
    }

    @Override
    public void leave(PlusExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgPlusExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgPlusExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgModExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgModExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgBitOrExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgBitOrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogGTExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogGTExpr visitable) {
        return;
    }

    @Override
    public boolean enter(CondExpr visitable) {
        return true;
    }

    @Override
    public void leave(CondExpr visitable) {
        return;
    }

    @Override
    public boolean enter(MinusExpr visitable) {
        return true;
    }

    @Override
    public void leave(MinusExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AddrOfExpr visitable) {
        return true;
    }

    @Override
    public void leave(AddrOfExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LshiftExpr visitable) {
        return true;
    }

    @Override
    public void leave(LshiftExpr visitable) {
        return;
    }

    @Override
    public boolean enter(RshiftExpr visitable) {
        return true;
    }

    @Override
    public void leave(RshiftExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgBitXorExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgBitXorExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogEQExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogEQExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogNEQExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogNEQExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogGEExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogGEExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogLTExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogLTExpr visitable) {
        return;
    }

    @Override
    public boolean enter(GccAlignOfExpr visitable) {
        return true;
    }

    @Override
    public void leave(GccAlignOfExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AssignExpr visitable) {
        return true;
    }

    @Override
    public void leave(AssignExpr visitable) {
        return;
    }

    @Override
    public boolean enter(MulExpr visitable) {
        return true;
    }

    @Override
    public void leave(MulExpr visitable) {
        return;
    }

    @Override
    public boolean enter(DivExpr visitable) {
        return true;
    }

    @Override
    public void leave(DivExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgMulExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgMulExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgRshiftExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgRshiftExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgBitAndExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgBitAndExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogLEExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogLEExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogAndExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogAndExpr visitable) {
        return;
    }

    @Override
    public boolean enter(CommaExpr visitable) {
        return true;
    }

    @Override
    public void leave(CommaExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgDivExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgDivExpr visitable) {
        return;
    }

    @Override
    public boolean enter(GccLabelAddr visitable) {
        return true;
    }

    @Override
    public void leave(GccLabelAddr visitable) {
        return;
    }

    @Override
    public boolean enter(BitAndExpr visitable) {
        return true;
    }

    @Override
    public void leave(BitAndExpr visitable) {
        return;
    }

    @Override
    public boolean enter(BitXorExpr visitable) {
        return true;
    }

    @Override
    public void leave(BitXorExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgMinusExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgMinusExpr visitable) {
        return;
    }

    @Override
    public boolean enter(AsgLshiftExpr visitable) {
        return true;
    }

    @Override
    public void leave(AsgLshiftExpr visitable) {
        return;
    }

    @Override
    public boolean enter(MemberAddr visitable) {
        return true;
    }

    @Override
    public void leave(MemberAddr visitable) {
        return;
    }

    @Override
    public boolean enter(MemberArrayRef visitable) {
        return true;
    }

    @Override
    public void leave(MemberArrayRef visitable) {
        return;
    }

    @Override
    public boolean enter(MemberArrayAddr visitable) {
        return true;
    }

    @Override
    public void leave(MemberArrayAddr visitable) {
        return;
    }

    @Override
    public boolean enter(TypeName visitable) {
        return true;
    }

    @Override
    public void leave(TypeName visitable) {
        return;
    }

    @Override
    public boolean enter(PointerRef visitable) {
        return true;
    }

    @Override
    public void leave(PointerRef visitable) {
        return;
    }

    @Override
    public boolean enter(UnaryMinusExpr visitable) {
        return true;
    }

    @Override
    public void leave(UnaryMinusExpr visitable) {
        return;
    }

    @Override
    public boolean enter(BitNotExpr visitable) {
        return true;
    }

    @Override
    public void leave(BitNotExpr visitable) {
        return;
    }

    @Override
    public boolean enter(LogNotExpr visitable) {
        return true;
    }

    @Override
    public void leave(LogNotExpr visitable) {
        return;
    }

    @Override
    public boolean enter(PostIncrExpr visitable) {
        return true;
    }

    @Override
    public void leave(PostIncrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(PostDecrExpr visitable) {
        return true;
    }

    @Override
    public void leave(PostDecrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(PreIncrExpr visitable) {
        return true;
    }

    @Override
    public void leave(PreIncrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(PreDecrExpr visitable) {
        return true;
    }

    @Override
    public void leave(PreDecrExpr visitable) {
        return;
    }

    @Override
    public boolean enter(IndexRange visitable) {
        return true;
    }

    @Override
    public void leave(IndexRange visitable) {
        return;
    }

    @Override
    public boolean enter(LowerBound visitable) {
        return true;
    }

    @Override
    public void leave(LowerBound visitable) {
        return;
    }

    @Override
    public boolean enter(UpperBound visitable) {
        return true;
    }

    @Override
    public void leave(UpperBound visitable) {
        return;
    }

    @Override
    public boolean enter(Step visitable) {
        return true;
    }

    @Override
    public void leave(Step visitable) {
        return;
    }

    @Override
    public boolean enter(DesignatedValue visitable) {
        return true;
    }

    @Override
    public void leave(DesignatedValue visitable) {
        return;
    }

    @Override
    public boolean enter(CompoundValue visitable) {
        return true;
    }

    @Override
    public void leave(CompoundValue visitable) {
        return;
    }

    @Override
    public boolean enter(BitField visitable) {
        return true;
    }

    @Override
    public void leave(BitField visitable) {
        return;
    }

    @Override
    public boolean enter(Pragma visitable) {
        return true;
    }

    @Override
    public void leave(Pragma visitable) {
        return;
    }

    @Override
    public boolean enter(Text visitable) {
        return true;
    }

    @Override
    public void leave(Text visitable) {
        return;
    }

    @Override
    public boolean enter(Declarations visitable) {
        return true;
    }

    @Override
    public void leave(Declarations visitable) {
        return;
    }

    @Override
    public boolean enter(FunctionDefinition visitable) {
        return true;
    }

    @Override
    public void leave(FunctionDefinition visitable) {
        return;
    }

    @Override
    public boolean enter(Params visitable) {
        return true;
    }

    @Override
    public void leave(Params visitable) {
        return;
    }

    @Override
    public boolean enter(Body visitable) {
        return true;
    }

    @Override
    public void leave(Body visitable) {
        return;
    }

    @Override
    public boolean enter(GccAsmStatement visitable) {
        return true;
    }

    @Override
    public void leave(GccAsmStatement visitable) {
        return;
    }

    @Override
    public boolean enter(GccAsmOperands visitable) {
        return true;
    }

    @Override
    public void leave(GccAsmOperands visitable) {
        return;
    }

    @Override
    public boolean enter(GccAsmOperand visitable) {
        return true;
    }

    @Override
    public void leave(GccAsmOperand visitable) {
        return;
    }

    @Override
    public boolean enter(GccAsmClobbers visitable) {
        return true;
    }

    @Override
    public void leave(GccAsmClobbers visitable) {
        return;
    }

    @Override
    public boolean enter(ForStatement visitable) {
        return true;
    }

    @Override
    public void leave(ForStatement visitable) {
        return;
    }

    @Override
    public boolean enter(Init visitable) {
        return true;
    }

    @Override
    public void leave(Init visitable) {
        return;
    }

    @Override
    public boolean enter(Condition visitable) {
        return true;
    }

    @Override
    public void leave(Condition visitable) {
        return;
    }

    @Override
    public boolean enter(Iter visitable) {
        return true;
    }

    @Override
    public void leave(Iter visitable) {
        return;
    }

    @Override
    public boolean enter(IfStatement visitable) {
        return true;
    }

    @Override
    public void leave(IfStatement visitable) {
        return;
    }

    @Override
    public boolean enter(Then visitable) {
        return true;
    }

    @Override
    public void leave(Then visitable) {
        return;
    }

    @Override
    public boolean enter(Else visitable) {
        return true;
    }

    @Override
    public void leave(Else visitable) {
        return;
    }

    @Override
    public boolean enter(WhileStatement visitable) {
        return true;
    }

    @Override
    public void leave(WhileStatement visitable) {
        return;
    }

    @Override
    public boolean enter(DoStatement visitable) {
        return true;
    }

    @Override
    public void leave(DoStatement visitable) {
        return;
    }

    @Override
    public boolean enter(SwitchStatement visitable) {
        return true;
    }

    @Override
    public void leave(SwitchStatement visitable) {
        return;
    }

    @Override
    public boolean enter(GccRangedCaseLabel visitable) {
        return true;
    }

    @Override
    public void leave(GccRangedCaseLabel visitable) {
        return;
    }

    @Override
    public boolean enter(StatementLabel visitable) {
        return true;
    }

    @Override
    public void leave(StatementLabel visitable) {
        return;
    }

    @Override
    public boolean enter(CaseLabel visitable) {
        return true;
    }

    @Override
    public void leave(CaseLabel visitable) {
        return;
    }

    @Override
    public boolean enter(BreakStatement visitable) {
        return true;
    }

    @Override
    public void leave(BreakStatement visitable) {
        return;
    }

    @Override
    public boolean enter(ContinueStatement visitable) {
        return true;
    }

    @Override
    public void leave(ContinueStatement visitable) {
        return;
    }

    @Override
    public boolean enter(ReturnStatement visitable) {
        return true;
    }

    @Override
    public void leave(ReturnStatement visitable) {
        return;
    }

    @Override
    public boolean enter(GotoStatement visitable) {
        return true;
    }

    @Override
    public void leave(GotoStatement visitable) {
        return;
    }

    @Override
    public boolean enter(DefaultLabel visitable) {
        return true;
    }

    @Override
    public void leave(DefaultLabel visitable) {
        return;
    }

    @Override
    public boolean enter(ExprStatement visitable) {
        return true;
    }

    @Override
    public void leave(ExprStatement visitable) {
        return;
    }

    @Override
    public boolean enter(VarDecl visitable) {
        return true;
    }

    @Override
    public void leave(VarDecl visitable) {
        return;
    }

    @Override
    public boolean enter(GccAsm visitable) {
        return true;
    }

    @Override
    public void leave(GccAsm visitable) {
        return;
    }

    @Override
    public boolean enter(FunctionDecl visitable) {
        return true;
    }

    @Override
    public void leave(FunctionDecl visitable) {
        return;
    }

    @Override
    public boolean enter(GccMemberDesignator visitable) {
        return true;
    }

    @Override
    public void leave(GccMemberDesignator visitable) {
        return;
    }

    @Override
    public boolean enter(ArrayType visitable) {
        return true;
    }

    @Override
    public void leave(ArrayType visitable) {
        return;
    }

    @Override
    public boolean enter(ArraySize visitable) {
        return true;
    }

    @Override
    public void leave(ArraySize visitable) {
        return;
    }

    @Override
    public boolean enter(BasicType visitable) {
        return true;
    }

    @Override
    public void leave(BasicType visitable) {
        return;
    }

    @Override
    public boolean enter(PointerType visitable) {
        return true;
    }

    @Override
    public void leave(PointerType visitable) {
        return;
    }

    @Override
    public boolean enter(StructType visitable) {
        return true;
    }

    @Override
    public void leave(StructType visitable) {
        return;
    }

    @Override
    public boolean enter(UnionType visitable) {
        return true;
    }

    @Override
    public void leave(UnionType visitable) {
        return;
    }

    @Override
    public boolean enter(EnumType visitable) {
        return true;
    }

    @Override
    public void leave(EnumType visitable) {
        return;
    }

    @Override
    public boolean enter(CoArrayType visitable) {
        return true;
    }

    @Override
    public void leave(CoArrayType visitable) {
        return;
    }

    @Override
    public boolean enter(GlobalSymbols visitable) {
        return true;
    }

    @Override
    public void leave(GlobalSymbols visitable) {
        return;
    }

    @Override
    public boolean enter(GlobalDeclarations visitable) {
        return true;
    }

    @Override
    public void leave(GlobalDeclarations visitable) {
        return;
    }

    @Override
    public boolean enter(GccAsmDefinition visitable) {
        return true;
    }

    @Override
    public void leave(GccAsmDefinition visitable) {
        return;
    }
}
