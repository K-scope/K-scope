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
package jp.riken.kscope.xcodeml.clang.xml;

import jp.riken.kscope.xcodeml.clang.xml.gen.AddrOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgDivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgLshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgMulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgPlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgRshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BuiltinOp;
import jp.riken.kscope.xcodeml.clang.xml.gen.CastExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayAssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.CommaExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValueAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundValueExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CondExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.FloatConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.FuncAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccAlignOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccCompoundExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.GccLabelAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.IntConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogEQExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogGEExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogGTExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogLEExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogLTExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogNEQExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LonglongConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.LshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MoeConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.RshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.SizeOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.StringConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.SubArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnaryMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.XmpDescOf;

/**
 * C言語:式モデルインターフェイスクラス
 * 
  <xs:group name="expressions">
    <xs:choice>
      <xs:element ref="intConstant"/>
      <xs:element ref="floatConstant"/>
      <xs:element ref="longlongConstant"/>
      <xs:element ref="stringConstant"/>
      <xs:element ref="moeConstant"/>
      <xs:element ref="funcAddr"/>
      <xs:element ref="pointerRef"/>
      <xs:element ref="Var"/>
      <xs:element ref="varAddr"/>
      <xs:element ref="arrayRef"/>
      <xs:element ref="arrayAddr"/>
      <xs:element ref="memberAddr"/>
      <xs:element ref="memberRef"/>
      <xs:element ref="memberArrayRef"/>
      <xs:element ref="memberArrayAddr"/>
      <xs:element ref="assignExpr"/>
      <xs:element ref="plusExpr"/>
      <xs:element ref="minusExpr"/>
      <xs:element ref="mulExpr"/>
      <xs:element ref="divExpr"/>
      <xs:element ref="modExpr"/>
      <xs:element ref="LshiftExpr"/>
      <xs:element ref="RshiftExpr"/>
      <xs:element ref="bitAndExpr"/>
      <xs:element ref="bitOrExpr"/>
      <xs:element ref="bitXorExpr"/>
      <xs:element ref="asgPlusExpr"/>
      <xs:element ref="asgMinusExpr"/>
      <xs:element ref="asgMulExpr"/>
      <xs:element ref="asgDivExpr"/>
      <xs:element ref="asgModExpr"/>
      <xs:element ref="asgLshiftExpr"/>
      <xs:element ref="asgRshiftExpr"/>
      <xs:element ref="asgBitAndExpr"/>
      <xs:element ref="asgBitOrExpr"/>
      <xs:element ref="asgBitXorExpr"/>
      <xs:element ref="logEQExpr"/>
      <xs:element ref="logNEQExpr"/>
      <xs:element ref="logGEExpr"/>
      <xs:element ref="logGTExpr"/>
      <xs:element ref="logLEExpr"/>
      <xs:element ref="logLTExpr"/>
      <xs:element ref="logAndExpr"/>
      <xs:element ref="logOrExpr"/>
      <xs:element ref="unaryMinusExpr"/>
      <xs:element ref="bitNotExpr"/>
      <xs:element ref="logNotExpr"/>
      <xs:element ref="functionCall"/>
      <xs:element ref="commaExpr"/>
      <xs:element ref="postIncrExpr"/>
      <xs:element ref="postDecrExpr"/>
      <xs:element ref="preIncrExpr"/>
      <xs:element ref="preDecrExpr"/>
      <xs:element ref="castExpr"/>
      <xs:element ref="condExpr"/>
      <xs:element ref="sizeOfExpr"/>
      <xs:element ref="addrOfExpr"/>
      <xs:element ref="xmpDescOf"/>
      <xs:element ref="compoundValue"/>
      <xs:element ref="compoundValueAddr"/>
      <xs:element ref="gccAlignOfExpr"/>
      <xs:element ref="gccLabelAddr"/>
      <xs:element ref="gccCompoundExpr"/>
      <xs:element ref="builtin_op"/>
      <xs:element ref="subArrayRef"/>
      <xs:element ref="coArrayRef"/>
      <xs:element ref="coArrayAssignExpr"/>
    </xs:choice>
  </xs:group>
 * @author RIKEN
 *
 */
public interface IExpressions {
	
	/**
	 * DivExpr(除算)要素を取得する
	 * @return    DivExpr(除算)要素
	 */
	public DivExpr getDivExpr();

	/**
	 * modExpr(剰余)要素を取得する
	 * @return    modExpr(剰余)要素
	 */
	public ModExpr getModExpr();

	/**
	 * asgDivExpr(代入除算)要素を取得する
	 * @return    asgDivExpr(代入除算)要素
	 */
	public AsgDivExpr getAsgDivExpr();

	/**
	 * asgModExpr(代入剰余)要素を取得する
	 * @return    asgModExpr(代入剰余)要素
	 */
	public AsgModExpr getAsgModExpr();
	
	/**
	 * LshiftExpr(左シフト)要素を取得する
	 * @return    LshiftExpr(左シフト)要素
	 */
	public LshiftExpr getLshiftExpr();

	/**
	 * RshiftExpr(右シフト)要素を取得する
	 * @return    RshiftExpr(右シフト)要素
	 */
	public RshiftExpr getRshiftExpr();

	/**
	 * bitAndExpr(ビット論理積)要素を取得する
	 * @return    bitAndExpr(ビット論理積)
	 */
	public BitAndExpr getBitAndExpr();

	/**
	 * bitOrExpr(ビット論理和)要素を取得する
	 * @return    bitOrExpr(ビット論理和)
	 */
	public BitOrExpr getBitOrExpr();

	/**
	 * bitXorExpr(ビット論理排他和)要素を取得する
	 * @return    bitOrExpr(ビット論理排他和)
	 */
	public BitXorExpr getBitXorExpr();
	

	/**
	 * asgLshiftExpr(代入左シフト)要素を取得する
	 * @return    asgLshiftExpr(代入左シフト)要素
	 */
	public AsgLshiftExpr getAsgLshiftExpr();

	/**
	 * asgRshiftExpr(代入右シフト)要素を取得する
	 * @return    asgRshiftExpr(代入右シフト)要素
	 */
	public AsgRshiftExpr getAsgRshiftExpr();

	/**
	 * asgBitAndExpr(代入ビット論理積)要素を取得する
	 * @return    asgBitAndExpr(代入ビット論理積)
	 */
	public AsgBitAndExpr getAsgBitAndExpr();

	/**
	 * asgBitOrExpr(代入ビット論理和)要素を取得する
	 * @return    asgBitOrExpr(代入ビット論理和)
	 */
	public AsgBitOrExpr getAsgBitOrExpr();

	/**
	 * asgBitXorExpr(代入ビット論理排他和)要素を取得する
	 * @return    asgBitOrExpr(代入ビット論理排他和)
	 */
	public AsgBitXorExpr getAsgBitXorExpr();
	
	
	/**
	 * pointerRef(メモリ参照)要素を取得する
	 * @return		pointerRef(メモリ参照)要素
	 */
	public PointerRef getPointerRef();

	/**
	 * stringConstant(文字列)要素を取得する
	 * @return		stringConstant(文字列)要素
	 */
	public StringConstant getStringConstant();

	/**
	 * moeConstant(enum型定数)要素を取得する
	 * @return		moeConstant(enum型定数)要素
	 */
	public MoeConstant getMoeConstant();
	
	/**
	 * arrayRef(配列の先頭要素アドレス)要素を取得する
	 * @return		arrayRef(配列の先頭要素アドレス)要素
	 */
	public ArrayRef getArrayRef();

	/**
	 * arrayAddr(配列アドレス)要素を取得する
	 * @return		arrayAddr(配列アドレス)要素
	 */
	public ArrayAddr getArrayAddr();

	/**
	 * memberAddr(構造体アドレス)要素を取得する
	 * @return		memberAddr(構造体アドレス)要素
	 */
	public MemberAddr getMemberAddr();

	/**
	 * memberRef(構造体参照)要素を取得する
	 * @return		memberRef(構造体参照)要素
	 */
	public MemberRef getMemberRef();

	/**
	 * memberArrayAddr(構造体配列メンバアドレス)要素を取得する
	 * @return		memberArrayAddr(構造体配列メンバアドレス)要素
	 */
	public MemberArrayAddr getMemberArrayAddr();

	/**
	 * memberArrayRef(構造体配列メンバ参照)要素を取得する
	 * @return		memberArrayRef(構造体配列メンバ参照)要素
	 */
	public MemberArrayRef getMemberArrayRef();
	
	/**
	 * coArrayRef(XcalableMP:coarray参照)要素を取得する
	 * @return		coArrayRef(XcalableMP:coarray参照)要素
	 */
	public CoArrayRef getCoArrayRef();
	
	
	/**
	 * coArrayAssignExpr(XcalableMP:coarray代入式)要素を取得する
	 * @return		coArrayAssignExpr(XcalableMP:coarray代入式)要素
	 */
	public CoArrayAssignExpr getCoArrayAssignExpr();
	
	
	/**
	 * intConstant(整数定数)要素を取得する
	 * @return		intConstant(整数定数)要素
	 */
	public IntConstant getIntConstant();

	/**
	 * longlongConstant(整数定数)要素を取得する
	 * @return		longlongConstant(整数定数)要素
	 */
	public LonglongConstant getLonglongConstant();
	
	/**
	 * FloatConstant(単精度浮動小数点数定数)要素を取得する
	 * @return		FloatConstant(単精度浮動小数点数定数)要素
	 */
	public FloatConstant getFloatConstant();

	/**
	 * funcAddr(関数アドレス)要素を取得する
	 * @return		funcAddr(関数アドレス)要素
	 */
	public FuncAddr getFuncAddr();

	/**
	 * functionCall(関数呼出)要素を取得する
	 * @return		functionCall(関数呼出)
	 */
	public FunctionCall getFunctionCall();
	
	/**
	 * assignExpr(代入式)要素を取得する
	 * @return		assignExpr(代入式)要素
	 */
	public AssignExpr getAssignExpr();
	
	/**
	 * LogAndExpr(論理積)要素を取得する
	 * @return		LogAndExpr(論理積)要素
	 */
	public LogAndExpr getLogAndExpr();

	/**
	 * LogEQExpr(等価)要素を取得する
	 * @return		LogEQExpr(等価)要素
	 */
	public LogEQExpr getLogEQExpr();

	/**
	 * LogGEExpr(大なり、または同値)要素を取得する
	 * @return		LogGEExpr(大なり、または同値)要素
	 */
	public LogGEExpr getLogGEExpr();

	/**
	 * LogGTExpr(大なり)要素を取得する
	 * @return		LogGTExpr(大なり)要素
	 */
	public LogGTExpr getLogGTExpr();

	/**
	 * LogLEExpr(小なり、または等価)要素を取得する
	 * @return		LogLEExpr(小なり、または等価)要素
	 */
	public LogLEExpr getLogLEExpr();

	/**
	 * LogLTExpr(小なり)要素を取得する
	 * @return		LogLTExpr(小なり)要素
	 */
	public LogLTExpr getLogLTExpr();

	/**
	 * LogNEQExpr(非等価)要素を取得する
	 * @return		LogNEQExpr(非等価)要素
	 */
	public LogNEQExpr getLogNEQExpr();

	/**
	 * bitNotExpr(ビット反転)要素を取得する
	 * @return		bitNotExpr(ビット反転)要素
	 */
	public BitNotExpr getBitNotExpr();

	/**
	 * LogNotExpr(論理否定)要素を取得する
	 * @return		LogNotExpr(論理否定)要素
	 */
	public LogNotExpr getLogNotExpr();

	/**
	 * commaExpr(コンマ式)要素を取得する
	 * @return		commaExpr(コンマ式)要素
	 */
	public CommaExpr getCommaExpr();

	/**
	 * postIncrExpr(ポストインクリメント)要素を取得する
	 * @return		postIncrExpr(ポストインクリメント)要素
	 */
	public PostIncrExpr getPostIncrExpr();
	
	/**
	 * postDecrExpr(ポストデクリメント)要素を取得する
	 * @return		postDecrExpr(ポストデクリメント)要素
	 */
	public PostDecrExpr getPostDecrExpr();

	/**
	 * preIncrExpr(プレインクリメント)要素を取得する
	 * @return		preIncrExpr(プレインクリメント)要素
	 */
	public PreIncrExpr getPreIncrExpr();
	
	/**
	 * preDecrExpr(プレデクリメント)要素を取得する
	 * @return		preDecrExpr(プレデクリメント)要素
	 */
	public PreDecrExpr getPreDecrExpr();

	/**
	 * castExpr(型変換)要素を取得する
	 * @return		castExpr(型変換)要素
	 */
	public CastExpr getCastExpr();

	/**
	 * condExpr(3項演算子)要素を取得する
	 * @return		condExpr(3項演算子)要素
	 */
	public CondExpr getCondExpr();

	/**
	 * sizeOfExpr(sizeof演算子)要素を取得する
	 * @return		sizeOfExpr(sizeof演算子)要素
	 */
	public SizeOfExpr getSizeOfExpr();

	/**
	 * addrOfExpr(アドレス演算子:&)要素を取得する
	 * @return		addrOfExpr(アドレス演算子:&)要素
	 */
	public AddrOfExpr getAddrOfExpr();
	

	/**
	 * XmpDescOf(XcalableMP:xmp_desc_of)要素を取得する
	 * @return		XmpDescOf(XcalableMP:xmp_desc_of)要素
	 */
	public XmpDescOf getXmpDescOf();

	/**
	 * CompoundValue(複合式)要素を取得する
	 * @return		CompoundValueExpr(複合式)要素
	 */
	public CompoundValueExpr getCompoundValue();

	/**
	 * CompoundValueAddr(複合アドレス式)要素を取得する
	 * @return		CompoundValueAddr(複合アドレス式)要素
	 */
	public CompoundValueAddr getCompoundValueAddr();
	

	/**
	 * gccAlignOfExpr(GCC:__alignof__演算子)要素を取得する
	 * @return		gccAlignOfExpr(GCC:__alignof__演算子)要素
	 */
	public GccAlignOfExpr getGccAlignOfExpr();

	/**
	 * gccLabelAddr(GCC:ラベル&&単項演算子)要素を取得する
	 * @return		gccAlignOfExpr(GCC:ラベル&&単項演算子)要素
	 */
	public GccLabelAddr getGccLabelAddr();

	/**
	 * gccCompoundExpr(GCC:複合式)要素を取得する
	 * @return		gccCompoundExpr(GCC:複合式)要素
	 */
	public GccCompoundExpr getGccCompoundExpr();
	

	/**
	 * builtin_op(コンパイラ組込関数呼出)要素を取得する
	 * @return		builtin_op(コンパイラ組込関数呼出)要素
	 */
	public BuiltinOp getBuiltinOp();
	
	

	/**
	 * subArrayRef(部分配列参照)要素を取得する
	 * @return		subArrayRef(部分配列参照)要素
	 */
	public SubArrayRef getSubArrayRef();
	
	/**
	 * LogOrExpr(論理和)要素を取得する
	 * @return		LogOrExpr(論理和)要素
	 */
	public LogOrExpr getLogOrExpr();

	/**
	 * MinusExpr(減算)要素を取得する
	 * @return		MinusExpr(減算)要素
	 */
	public MinusExpr getMinusExpr();

	/**
	 * AsgMinusExpr(代入減算)要素を取得する
	 * @return		AsgMinusExpr(代入減算)要素
	 */
	public AsgMinusExpr getAsgMinusExpr();

	/**
	 * MulExpr(乗算)要素を取得する
	 * @return		MulExpr(乗算)要素
	 */
	public MulExpr getMulExpr();

	/**
	 * AsgMulExpr(代入乗算)要素を取得する
	 * @return		AsgMulExpr(代入乗算)要素
	 */
	public AsgMulExpr getAsgMulExpr();
	
	/**
	 * PlusExpr(加算)要素を取得する
	 * @return		PlusExpr(加算)要素
	 */
	public PlusExpr getPlusExpr();

	/**
	 * asgPlusExpr(代入加算)要素を取得する
	 * @return		PlusExpr(代入加算)要素
	 */
	public AsgPlusExpr getAsgPlusExpr();
	
	/**
	 * UnaryMinusExpr(符号反転)要素を取得する
	 * @return		UnaryMinusExpr(符号反転)要素
	 */
	public UnaryMinusExpr getUnaryMinusExpr();

	/**
	 * Var(変数名)要素を取得する
	 * @return		Var(変数名)要素
	 */
	public Var getVar();

	/**
	 * VarAddr(変数参照)要素を取得する
	 * @return		VarAddr(変数参照)要素
	 */
	public VarAddr getVarAddr();

	/**
	 * DivExpr(除算)要素を設定する
	 * @param  value    DivExpr(除算)要素
	 */
	public void setDivExpr(DivExpr value);
	
	/**
	 * modExpr(剰余)要素を設定する
	 * @param  value    modExpr(剰余)要素
	 */
	public void setModExpr(ModExpr value);

	/**
	 * asgDivExpr(代入除算)要素を設定する
	 * @param  value    asgDivExpr(代入除算)要素
	 */
	public void setAsgDivExpr(AsgDivExpr value);
	
	/**
	 * asgModExpr(代入剰余)要素を設定する
	 * @param  value    asgModExpr(代入剰余)要素
	 */
	public void setAsgModExpr(AsgModExpr value);

	/**
	 * LshiftExpr(左シフト)要素を設定する
	 * @param  value    LshiftExpr(左シフト)要素
	 */
	public void setLshiftExpr(LshiftExpr value);

	/**
	 * RshiftExpr(右シフト)要素を設定する
	 * @param  value    RshiftExpr(右シフト)要素
	 */
	public void setRshiftExpr(RshiftExpr value);
		
	/**
	 * bitAndExpr(ビット論理積)を設定する
	 * @param  value    bitAndExpr(ビット論理積)
	 */
	public void setBitAndExpr(BitAndExpr value);

	/**
	 * bitOrExpr(ビット論理和)を設定する
	 * @param  value    bitOrExpr(ビット論理和)
	 */
	public void setBitOrExpr(BitOrExpr value);

	/**
	 * bitXorExpr(ビット論理排他和)を設定する
	 * @param  value    bitOxrExpr(ビット論理h排他和)
	 */
	public void setBitXorExpr(BitXorExpr value);

	/**
	 * asgLshiftExpr(代入左シフト)要素を設定する
	 * @param  value    asgLshiftExpr(代入左シフト)要素
	 */
	public void setAsgLshiftExpr(AsgLshiftExpr value);

	/**
	 * AsgRshiftExpr(代入右シフト)要素を設定する
	 * @param  value    asgRshiftExpr(代入右シフト)要素
	 */
	public void setAsgRshiftExpr(AsgRshiftExpr value);
		
	/**
	 * asgBitAndExpr(代入ビット論理積)を設定する
	 * @param  value    asgBitAndExpr(代入ビット論理積)
	 */
	public void setAsgBitAndExpr(AsgBitAndExpr value);

	/**
	 * asgBitOrExpr(代入ビット論理和)を設定する
	 * @param  value    asgBitOrExpr(代入ビット論理和)
	 */
	public void setAsgBitOrExpr(AsgBitOrExpr value);

	/**
	 * asgBitXorExpr(代入ビット論理排他和)を設定する
	 * @param  value    asgBitOxrExpr(代入ビット論理h排他和)
	 */
	public void setAsgBitXorExpr(AsgBitXorExpr value);
	
	/**
	 * pointerRef(メモリ参照)要素を取得する
	 * @param  value		pointerRef(メモリ参照)要素
	 */
	public void setPointerRef(PointerRef value);

	/**
	 * stringConstant(文字列)要素を取得する
	 * @param  value		stringConstant(文字列)要素
	 */
	public void setStringConstant(StringConstant value);
	
	
	/**
	 * moeConstant(enum型定数)要素を取得する
	 * @param  value		moeConstant(enum型定数)
	 */
	public void setMoeConstant(MoeConstant value);
	
	/**
	 * arrayAddr(配列アドレス)要素を取得する
	 * @param  value		arrayAddr(配列アドレス)要素
	 */
	public void setArrayAddr(ArrayAddr value);
	

	/**
	 * memberAddr(構造体アドレス)要素を取得する
	 * @param  value		memberAddr(構造体アドレス)要素
	 */
	public void setMemberAddr(MemberAddr value);

	/**
	 * memberRef(構造体参照)要素を取得する
	 * @param  value		memberRef(構造体参照)要素
	 */
	public void setMemberRef(MemberRef value);

	/**
	 * memberArrayAddr(構造体配列メンバアドレス)要素を取得する
	 * @param  value		memberArrayAddr(構造体配列メンバアドレス)要素
	 */
	public void setMemberArrayAddr(MemberArrayAddr value);

	/**
	 * memberArrayRef(構造体配列メンバ参照)要素を取得する
	 * @param  value		memberArrayRef(構造体配列メンバ参照)要素
	 */
	public void setMemberArrayRef(MemberArrayRef value);
	
	/**
	 * arrayRef(配列の先頭要素アドレス)要素を取得する
	 * @param  value		carrayRef(配列の先頭要素アドレス)要素
	 */
	public void setArrayRef(ArrayRef value);
	
	/**
	 * coArrayRef(coarray参照)要素を取得する
	 * @param  value		coArrayRef(coarray参照)要素
	 */
	public void setCoArrayRef(CoArrayRef value);

	/**
	 * coArrayAssignExpr(XcalableMP:coarray代入式)要素を取得する
	 * @param  value		coArrayAssignExpr(XcalableMP:coarray代入式)要素
	 */
	public void setCoArrayAssignExpr(CoArrayAssignExpr value);
	
	/**
	 * intConstant(整数定数)要素を取得する
	 * @param  value		intConstant(整数定数)要素
	 */
	public void setIntConstant(IntConstant value);

	/**
	 * longlongConstant(整数定数)要素を取得する
	 * @param  value		longlongConstant(整数定数)要素
	 */
	public void setLonglongConstant(LonglongConstant value);

	/**
	 * FloatConstant(単精度浮動小数点数定数)要素を取得する
	 * @param  value		FloatConstant(単精度浮動小数点数定数)要素
	 */
	public void setFloatConstant(FloatConstant value);

	/**
	 * funcAddr(関数アドレス)要素を取得する
	 * @param  value		funcAddr(関数アドレス)要素
	 */
	public void setFuncAddr(FuncAddr value);
	
	
	/**
	 * functionCall(関数呼出)を取得する
	 * @param  value		functionCall(関数呼出)
	 */
	public void setFunctionCall(FunctionCall value);
	
	/**
	 * assignExpr(代入式)要素を取得する
	 * @param  value		assignExpr(代入式)要素
	 */
	public void setAssignExpr(AssignExpr value);
	
	/**
	 * LogAndExpr(論理積)要素を取得する
	 * @param  value		LogAndExpr(論理積)要素
	 */
	public void setLogAndExpr(LogAndExpr value);

	/**
	 * LogEQExpr(等価)要素を取得する
	 * @param  value		LogEQExpr(等価)要素
	 */
	public void setLogEQExpr(LogEQExpr value);

	/**
	 * LogGEExpr(大なり、または同値)要素を取得する
	 * @param  value		LogGEExpr(大なり、または同値)要素
	 */
	public void setLogGEExpr(LogGEExpr value);

	/**
	 * LogGTExpr(大なり)要素を取得する
	 * @param  value		LogGTExpr(大なり)要素
	 */
	public void setLogGTExpr(LogGTExpr value);

	/**
	 * LogLEExpr(小なり、または等価)要素を取得する
	 * @param  value		LogLEExpr(小なり、または等価)要素
	 */
	public void setLogLEExpr(LogLEExpr value);

	/**
	 * LogLTExpr(小なり)要素を取得する
	 * @param  value		LogLTExpr(小なり)要素
	 */
	public void setLogLTExpr(LogLTExpr value);

	/**
	 * LogNEQExpr(非等価)要素を取得する
	 * @param  value		LogNEQExpr(非等価)要素
	 */
	public void setLogNEQExpr(LogNEQExpr value);

	/**
	 * bitNotExpr(ビット反転)要素を取得する
	 * @param  value		bitNotExpr(ビット反転)要素
	 */
	public void setBitNotExpr(BitNotExpr value);

	/**
	 * LogNotExpr(論理否定)要素を取得する
	 * @param  value		LogNotExpr(論理否定)要素
	 */
	public void setLogNotExpr(LogNotExpr value);
	

	/**
	 * commaExpr(コンマ式)要素を取得する
	 * @param  value		commaExpr(コンマ式)要素
	 */
	public void setCommaExpr(CommaExpr value);
	

	/**
	 * postIncrExpr(ポストインクリメント)要素を取得する
	 * @param  value		postIncrExpr(ポストインクリメント)要素
	 */
	public void setPostIncrExpr(PostIncrExpr value);

	/**
	 * postDecrExpr(ポストデクリメント)要素を取得する
	 * @param  value		postDecrExpr(ポストデクリメント)要素
	 */
	public void setPostDecrExpr(PostDecrExpr value);

	/**
	 * preIncrExpr(プレインクリメント)要素要素を取得する
	 * @param  value		preIncrExpr(プレインクリメント)要素
	 */
	public void setPreIncrExpr(PreIncrExpr value);

	/**
	 * preDecrExpr(プレデクリメント)要素を取得する
	 * @param  value		preDecrExpr(プレデクリメント)要素
	 */
	public void setPreDecrExpr(PreDecrExpr value);

	/**
	 * castExpr(型変換)要素を設定する
	 * @param  value		castExpr(型変換)要素
	 */
	public void setCastExpr(CastExpr value);

	/**
	 * condExpr(3項演算子)要素を設定する
	 * @param  value		condExpr(3項演算子)要素
	 */
	public void setCondExpr(CondExpr value);

	/**
	 * sizeOfExpr(sizeof演算子)要素を取得する
	 * @param  value		sizeOfExpr(sizeof演算子)要素
	 */
	public void setSizeOfExpr(SizeOfExpr value);

	/**
	 * addrOfExpr(アドレス演算子:&)要素を取得する
	 * @param  value		addrOfExpr(アドレス演算子:&)要素
	 */
	public void setAddrOfExpr(AddrOfExpr value);

	/**
	 * XmpDescOf(XcalableMP:xmp_desc_of)要素を取得する
	 * @param  value		XmpDescOf(XcalableMP:xmp_desc_of)要素
	 */
	public void setXmpDescOf(XmpDescOf value);

	/**
	 * CompoundValue(複合式)要素を取得する
	 * @param  value		CompoundValueExpr(複合式)要素
	 */
	public void setCompoundValue(CompoundValueExpr value);

	/**
	 * CompoundValueAddr(複合アドレス式)要素を取得する
	 * @param  value		CompoundValueAddr(複合アドレス式)要素
	 */
	public void setCompoundValueAddr(CompoundValueAddr value);

	/**
	 * gccAlignOfExpr(GCC:__alignof__演算子)要素を取得する
	 * @param  value		gccAlignOfExpr(GCC:__alignof__演算子)要素
	 */
	public void setGccAlignOfExpr(GccAlignOfExpr value);

	/**
	 * gccLabelAddr(GCC:ラベル&&単項演算子)要素を取得する
	 * @param  value		gccAlignOfExpr(GCC:ラベル&&単項演算子)要素
	 */
	public void setGccLabelAddr(GccLabelAddr value);

	/**
	 * gccCompoundExpr(GCC:複合式)要素を取得する
	 * @param  value		gccCompoundExpr(GCC:複合式)要素
	 */
	public void setGccCompoundExpr(GccCompoundExpr value);

	/**
	 * builtin_op(コンパイラ組込関数呼出)要素を取得する
	 * @param  value		builtin_op(コンパイラ組込関数呼出)要素
	 */
	public void setBuiltinOp(BuiltinOp value);

	/**
	 * subArrayRef(部分配列参照)要素を取得する
	 * @param  value		subArrayRef(部分配列参照)要素
	 */
	public void setSubArrayRef(SubArrayRef value);
	
	/**
	 * LogOrExpr(論理和)要素を取得する
	 * @param  value		LogOrExpr(論理和)要素
	 */
	public void setLogOrExpr(LogOrExpr value);

	/**
	 * MinusExpr(減算)要素を取得する
	 * @param  value		MinusExpr(減算)要素
	 */
	public void setMinusExpr(MinusExpr value);

	/**
	 * AsgMinusExpr(代入減算)要素を取得する
	 * @param  value		AsgMinusExpr(代入減算)要素
	 */
	public void setAsgMinusExpr(AsgMinusExpr value);

	/**
	 * MulExpr(乗算)要素を取得する
	 * @param  value		MulExpr(乗算)要素
	 */
	public void setMulExpr(MulExpr value);

	/**
	 * AsgMulExpr(代入乗算)要素を取得する
	 * @param  value		AsgMulExpr(代入乗算)要素
	 */
	public void setAsgMulExpr(AsgMulExpr value);

	/**
	 * PlusExpr(加算)要素を取得する
	 * @param  value		PlusExpr(加算)要素
	 */
	public void setPlusExpr(PlusExpr value);

	/**
	 * AsgPlusExpr(代入加算)要素を取得する
	 * @param  value		AsgPlusExpr(代入加算)要素
	 */
	public void setAsgPlusExpr(AsgPlusExpr value);

	/**
	 * UnaryMinusExpr(符号反転)要素を取得する
	 * @param  value		UnaryMinusExpr(符号反転)要素
	 */
	public void setUnaryMinusExpr(UnaryMinusExpr value);

	/**
	 * Var(変数名)要素を取得する
	 * @param  value		Var(変数名)要素
	 */
	public void setVar(Var value);

	/**
	 * VarAddr(変数参照)要素を取得する
	 * @param  value		VarAddr(変数参照)要素
	 */
	public void setVarAddr(VarAddr value);
}
