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

package jp.riken.kscope.language;

import java.util.Set;

/**
*
* データ型を示すインターフェース.<br>
* 各言語に対応したVariableType(enum型)がこのインターフェースを実装して使用する。
*
* @author RIKEN
*
*/
public interface IVariableType {

    /**
     * 型名の取得。
     *
     * @return 型名
     */
    String getName();

    /**
     * 型名からVariableTypeを検索する.
     *
     * @param name
     *          型名
     *
     * @return 型名に対応したVariableType
     *
     */
    IVariableType findTypeBy(String name);


    /**
     * 型が適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の型チェックをする必要がある。<br>
     * 「適合している」とは、この型チェックで、同一の型と判定される
     * 事を意味している。
     *
     * @param value
     *          型
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     *
     */
    boolean matches(IVariableType value);

    /**
     * 実数変数であるかチェックする.
     * @return        true=実数
     */
    boolean isRealType();

    /**
     * 整数変数であるかチェックする.
     * @return        true=整数
     */
    boolean isIntegerType();

    /**
     * structure型であるかチェックする
     * @return        true=structure
     */
    boolean isStruct();

    /**
     * 変数リストを取得する.
     */
    Set<Variable> getAllVariables();


    /**
     * 親ブロックを設定する.
     * @param parent 親ブロック
     */
    void setParentStatement(IBlock parent);
}
