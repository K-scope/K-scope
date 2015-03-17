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

import jp.riken.kscope.xcodeml.fortran.xml.gen.Params;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Value;

/**
 * GotoStatement要素(GOTO文)クラス
 * @author RIKEN
 *
 */
public class GotoStatementSequence implements IXmlNode {

    /** 計算形GOTO文の文番号並び */
    protected Params params;
    /** 計算形GOTO文の式 */
    protected Value value;

    /**
     * コンストラクタ
     * @param params		計算形GOTO文の文番号並び
     * @param value			計算形GOTO文の式
     */
    public GotoStatementSequence(Params params, Value value) {
        this.params = params;
        this.value = value;
    }

    /**
     * 計算形GOTO文の文番号並びを取得する
     * @return		計算形GOTO文の文番号並び
     */
    public Params getParams() {
        return params;
    }

    /**
     * 計算形GOTO文の文番号並びを設定する
     * @param params		計算形GOTO文の文番号並び
     */
    public void setParams(Params params) {
        this.params = params;
    }

    /**
     * 計算形GOTO文の式を取得する
     * @return		計算形GOTO文の式
     */
    public Value getValue() {
        return value;
    }

    /**
     * 計算形GOTO文の式を設定する
     * @param value		計算形GOTO文の式
     */
    public void setValue(Value value) {
        this.value = value;
    }


    /**
     * GotoStatement要素(GOTO文)の探索を開始する
     * @param visitor		XcodeMLノード探索
     * @return		成否
     */
    @Override
    public boolean enter(IXmlVisitor visitor) {
        return (visitor.enter(this));
    }

    /**
     * GotoStatement要素(GOTO文)の探索を終了する
     * @param visitor		XcodeMLノード探索
     */
    @Override
    public void leave(IXmlVisitor visitor) {
        visitor.leave(this);
    }

}
