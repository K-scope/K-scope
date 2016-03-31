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

import jp.riken.kscope.xcodeml.xml.IXmlVisitor;
import jp.riken.kscope.xcodeml.xml.gen.ValueList;
import jp.riken.kscope.xcodeml.xml.gen.VarList;

/**
 * FdataDecl要素(DATA文)クラス
 * @author RIKEN
 *
 */
public class FdataDeclSequence implements IXmlNode {
	
	/** 初期化項目並び */
	protected VarList varList;
	/** 初期値項目並び */
	protected ValueList valueList;
	
	/**
	 * コンストラクタ
	 * @param varList		初期化項目並び
	 * @param valueList		初期値項目並び
	 */
	public FdataDeclSequence(VarList varList, ValueList valueList) {
		this.varList = varList;
		this.valueList = valueList;
	}
	
	/**
	 * 初期化項目並びを取得する
	 * @return		初期化項目並び
	 */
	public VarList getVarList() {
		return varList;
	}
	
	/**
	 * 初期化項目並びを設定する
	 * @param varList		初期化項目並び
	 */
	public void setVarList(VarList varList) {
		this.varList = varList;
	}
	
	/**
	 * 初期値項目並びを取得する
	 * @return		初期値項目並び
	 */
	public ValueList getValueList() {
		return valueList;
	}
	
	/**
	 * 初期値項目並びを取得する
	 * @param valueList		初期値項目並び
	 */
	public void setValueList(ValueList valueList) {
		this.valueList = valueList;
	}
	
	/**
	 * FdataDecl要素(DATA文)の探索を開始する
	 * @param visitor		XcodeMLノード探索
	 * @return		成否
	 */
	@Override
	public boolean enter(IXmlVisitor visitor) {
		return (visitor.enter(this));
	}

	/**
	 * FdataDecl要素(DATA文)の探索を終了する
	 * @param visitor		XcodeMLノード探索
	 */
	@Override
	public void leave(IXmlVisitor visitor) {
		visitor.leave(this);
	}

}
