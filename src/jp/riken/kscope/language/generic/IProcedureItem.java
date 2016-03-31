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

package jp.riken.kscope.language.generic;

import java.util.List;

import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Procedure;

/**
 * 関数名の総称定義に対応したクラス。
 *
 * @author RIKEN
 *
 */
public interface IProcedureItem {

    /**
     * 候補対象となる関数が自分の情報と適合しているかどうかを<br>
     * 調べるメソッド。
     *
     * @param target
     *            候補対象関数
     * @param actualArguments
     *            実引数リスト
     *
     * @return true:  適合している
     *         false: 適合していない
     */
    boolean matches(Procedure target, List<Expression> actualArguments);
    
    /**
     * 手続き引用仕様の個別名を返す。
     * @return 手続き引用仕様の個別名
     */
    String getName();

    /**
     * 対象となる実引数リストが自分の情報と適合しているかどうかを調べるメソッド。
     * @param arguments 実引数リスト
     * @return true:  適合している
     *         false: 適合していない
     */
    boolean matches(List<Expression> arguments);
}
