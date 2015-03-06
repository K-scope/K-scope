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

import java.util.List;

/**
 * 配列要素クラス
 * @author RIKEN
 */
public class DefModelArraySubscriptSequence implements IXmlNode {

    /** 配列要素リスト */
    protected List<IXmlNode> indexRangeOrArrayIndex;

    /**
     * コンストラクタ
     * @param index		配列要素リスト
     */
    public DefModelArraySubscriptSequence(List<IXmlNode> index) {
        this.indexRangeOrArrayIndex = index;
    }

    /**
     * 配列要素リストを取得する
     * @return		配列要素リスト
     */
    public IXmlNode[] getIndexRangeOrArrayIndex() {
        IXmlNode[] array = new IXmlNode[indexRangeOrArrayIndex.size()];
        return ((IXmlNode[]) indexRangeOrArrayIndex.toArray(array));
    }

    /**
     * 配列要素のパースを開始する
     * @param visitor		パーサVisitor
     * @return		成否
     */
    @Override
    public boolean enter(IXmlVisitor visitor) {
        return (visitor.enter(this));
    }

    /**
     * 配列要素のパースを終了する
     * @param visitor		パーサVisitor
     */
    @Override
    public void leave(IXmlVisitor visitor) {
        visitor.leave(this);
    }

}
