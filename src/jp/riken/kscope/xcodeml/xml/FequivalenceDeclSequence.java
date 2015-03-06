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

import jp.riken.kscope.xcodeml.xml.gen.VarList;
import jp.riken.kscope.xcodeml.xml.gen.VarRef;

/**
 * FequivalenceDecl要素(EQUIVALENCE文)クラス
 * @author RIKEN
 *
 */
public class FequivalenceDeclSequence implements IXmlNode {
    /** 結合実体 */
    protected VarRef varRef;
    /** 結合実体並び */
    protected VarList varList;

    /**
     * コンストラクタ
     * @param varRef		結合実体
     * @param varList		結合実体並び
     */
    public FequivalenceDeclSequence(VarRef varRef, VarList varList) {
        this.varRef = varRef;
        this.varList = varList;
    }

    /**
     * 結合実体を取得する
     * @return		結合実体
     */
    public VarRef getVarRef() {
        return varRef;
    }

    /**
     * 結合実体を設定する
     * @param varRef		結合実体
     */
    public void setVarRef(VarRef varRef) {
        this.varRef = varRef;
    }

    /**
     * 結合実体並びを取得する
     * @return		結合実体並び
     */
    public VarList getVarList() {
        return varList;
    }

    /**
     * 結合実体並びを設定する
     * @param varList		結合実体並び
     */
    public void setVarList(VarList varList) {
        this.varList = varList;
    }

    /**
     * FequivalenceDecl要素(EQUIVALENCE文)の探索を開始する
     * @param visitor		XcodeMLノード探索
     * @return		成否
     */
    @Override
    public boolean enter(IXmlVisitor visitor) {
        return (visitor.enter(this));
    }

    /**
     * FequivalenceDecl要素(EQUIVALENCE文)の探索を終了する
     * @param visitor		XcodeMLノード探索
     */
    @Override
    public void leave(IXmlVisitor visitor) {
        visitor.leave(this);
    }
}
