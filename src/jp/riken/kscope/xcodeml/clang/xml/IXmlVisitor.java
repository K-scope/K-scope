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
package jp.riken.kscope.xcodeml.clang.xml;

import jp.riken.kscope.xcodeml.clang.xml.gen.*;

/**
 * XcodeMLノード探索インターフェイス
 * @author RIKEN
 */
public interface IXmlVisitor {

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(XcodeProgram visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(XcodeProgram visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(TypeTable visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(TypeTable visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(FunctionType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(FunctionType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAttributes visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAttributes visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAttribute visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAttribute visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BuiltinOp visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BuiltinOp visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ArrayRef visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ArrayRef visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ArrayAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ArrayAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(FunctionCall visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(FunctionCall visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Function visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Function visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Arguments visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Arguments visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccCompoundExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccCompoundExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CompoundStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CompoundStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Symbols visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Symbols visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Id visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Id visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Name visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Name visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Value visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Value visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(SubArrayRef visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(SubArrayRef visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CoArrayRef visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CoArrayRef visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Var visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Var visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MemberRef visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MemberRef visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CastExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CastExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(StringConstant visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(StringConstant visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(VarAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(VarAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CompoundValueExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CompoundValueExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CompoundValueAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CompoundValueAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(XmpDescOf visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(XmpDescOf visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(IntConstant visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(IntConstant visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(FloatConstant visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(FloatConstant visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LonglongConstant visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LonglongConstant visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MoeConstant visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MoeConstant visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(FuncAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(FuncAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(SizeOfExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(SizeOfExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CoArrayAssignExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CoArrayAssignExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ModExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ModExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BitOrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BitOrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogOrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogOrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PlusExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PlusExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgPlusExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgPlusExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgModExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgModExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgBitOrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgBitOrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogGTExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogGTExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CondExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CondExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MinusExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MinusExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AddrOfExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AddrOfExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LshiftExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LshiftExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(RshiftExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(RshiftExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgBitXorExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgBitXorExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogEQExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogEQExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogNEQExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogNEQExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogGEExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogGEExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogLTExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogLTExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAlignOfExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAlignOfExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AssignExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AssignExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MulExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MulExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(DivExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(DivExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgMulExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgMulExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgRshiftExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgRshiftExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgBitAndExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgBitAndExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogLEExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogLEExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogAndExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogAndExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CommaExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CommaExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgDivExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgDivExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccLabelAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccLabelAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BitAndExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BitAndExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BitXorExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BitXorExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgMinusExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgMinusExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(AsgLshiftExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(AsgLshiftExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MemberAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MemberAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MemberArrayRef visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MemberArrayRef visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(MemberArrayAddr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(MemberArrayAddr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(TypeName visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(TypeName visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PointerRef visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PointerRef visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(UnaryMinusExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(UnaryMinusExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BitNotExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BitNotExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LogNotExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LogNotExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PostIncrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PostIncrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PostDecrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PostDecrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PreIncrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PreIncrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PreDecrExpr visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PreDecrExpr visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(IndexRange visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(IndexRange visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(LowerBound visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(LowerBound visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(UpperBound visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(UpperBound visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Step visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Step visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(DesignatedValue visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(DesignatedValue visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CompoundValue visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CompoundValue visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BitField visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BitField visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Pragma visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Pragma visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Text visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Text visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Declarations visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Declarations visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(FunctionDefinition visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(FunctionDefinition visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Params visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Params visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Body visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Body visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAsmStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAsmStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAsmOperands visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAsmOperands visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAsmOperand visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAsmOperand visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAsmClobbers visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAsmClobbers visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ForStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ForStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Init visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Init visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Condition visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Condition visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Iter visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Iter visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(IfStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(IfStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Then visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Then visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(Else visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(Else visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(WhileStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(WhileStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(DoStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(DoStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(SwitchStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(SwitchStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccRangedCaseLabel visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccRangedCaseLabel visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(StatementLabel visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(StatementLabel visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CaseLabel visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CaseLabel visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BreakStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BreakStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ContinueStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ContinueStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ReturnStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ReturnStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GotoStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GotoStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(DefaultLabel visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(DefaultLabel visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ExprStatement visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ExprStatement visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(VarDecl visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(VarDecl visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAsm visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAsm visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(FunctionDecl visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(FunctionDecl visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccMemberDesignator visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccMemberDesignator visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ArrayType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ArrayType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(ArraySize visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(ArraySize visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(BasicType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(BasicType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(PointerType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(PointerType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(StructType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(StructType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(UnionType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(UnionType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(EnumType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(EnumType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(CoArrayType visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(CoArrayType visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GlobalSymbols visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GlobalSymbols visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GlobalDeclarations visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GlobalDeclarations visitable);

    /**
     * Visits this node for enter behavior.
     *
     * @param visitable
     * @return boolean
     */
    boolean enter(GccAsmDefinition visitable);

    /**
     * Visits this node for leave behavior.
     *
     * @param visitable
     */
    void leave(GccAsmDefinition visitable);

}

