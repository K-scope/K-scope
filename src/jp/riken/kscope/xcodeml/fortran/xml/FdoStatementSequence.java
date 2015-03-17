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

package jp.riken.kscope.xcodeml.fortran.xml;

import jp.riken.kscope.xcodeml.fortran.xml.gen.IndexRange;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Var;


/**
 * FdoStatement要素(DO文)クラス
 * @author RIKEN
 *
 */
public class FdoStatementSequence implements IXmlNode {
	/** DO変数 */
	protected Var var;
	/** DO変数の値範囲 */
	protected IndexRange indexRange;
	
	/**
	 * コンストラクタ
	 * @param var		DO変数
	 * @param index		DO変数の値範囲 
	 */
	public FdoStatementSequence(Var var, IndexRange index) {
		this.var = var;
		indexRange = index;
	}
	
	/**
	 * DO変数を取得する
	 * @return		DO変数
	 */
	public Var getVar() {
		return var;
	}
	
	/**
	 * DO変数を設定する
	 * @param var		DO変数
	 */
	public void setVar(Var var) {
		this.var = var;
	}
	
	/**
	 * DO変数の値範囲を取得する
	 * @return		DO変数の値範囲 
	 */
	public IndexRange getIndexRange() {
		return indexRange;
	}
	
	/**
	 * DO変数の値範囲を設定する
	 * @param indexRange		DO変数の値範囲 
	 */
	public void setIndexRange(IndexRange indexRange) {
		this.indexRange = indexRange;
	}

	/**
	 * FdoStatement要素(DO文)の探索を開始する
	 * @param visitor		XcodeMLノード探索
	 * @return		成否
	 */
	@Override
	public boolean enter(IXmlVisitor visitor) {
		return (visitor.enter(this));
	}

	/**
	 * FdoStatement要素(DO文)の探索を終了する
	 * @param visitor		XcodeMLノード探索
	 */
	@Override
	public void leave(IXmlVisitor visitor) {
		visitor.leave(this);
	}

}
