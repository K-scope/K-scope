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

package jp.riken.kscope.language;

import java.util.Set;

/**
*
* 変数の属性（修飾子）を示すインターフェース.<br>
* 各言語に対応したVariableAttributeクラスが、このインターフェースを実装して使用する。
*
* @author RIKEN
*
*/
public interface IVariableAttribute {

    /**
     * 属性の設定.
     *
     * @param attrbts
     *            設定すべき全属性
     */
    void setAttributes(Set<String> attrbts);

    /**
     * 属性の設定.
     *
     * @param attrbts
     *            設定すべき全属性
     */
    void setAttributes(String[] attrbts);

    /**
     * 属性の取得.
     *
     * @return 全属性
     */
    Set<String> getAttributes();

    /**
     * 属性の追加.
     *
     * @param attrbt
     *           追加すべき属性
     */
    void addAttribute(String attrbt);

    /**
     * 対象文字列が属性内に含まれているかどうか。<br>
     * ただし、対象文字列の大文字小文字は無視する。<br>
     *
     * @param keyword
     *               対象文字列
     * @return true : 対象文字列が含まれる
     */
    boolean contains(String keyword);

    /**
     * 変数の属性（修飾子）文字列を返す.
     *
     * @return 属性（修飾子）文字列
     */
    String toString();

    /**
     * 属性が適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の属性チェックをする必要がある。<br>
     * 「適合している」とは、この属性チェックで、同一の属性と判定される
     * 事を意味している。
     *
     * @param value
     *          属性
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     *
     */
    boolean matches(IVariableAttribute value);
}
