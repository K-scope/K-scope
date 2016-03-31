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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import java.util.List;

import jp.riken.kscope.language.VariableDefinition;

/**
 * map型クラス。
 * @deprecated 未使用
 * @author RIKEN
 *
 */
public class Map implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = -6468256404696838753L;
    /**
     * structure型とほぼ同じなため、structureを包含して使う。
     */
    private Structure core = new Structure();

    /**
     * コンストラクタ
     */
    public Map() {
    }

    /**
     * コピーコンストラクタ
     */
    public Map(Map map) {
        if (map == null) return;
        this.core = map.core;
    }

    /**
     * 変数定義文の追加。
     *
     * @param definition
     *          変数定義文
     */
    public void add(VariableDefinition definition) {
        core.add(definition);
    }

    /**
     * 指定の型の変数定義文の追加。
     *
     * @param typ
     *          変数の型
     * @param nm
     *          変数名
     */
    public void add(VariableType typ, String nm) {
        core.add(typ, nm);
    }

    /**
     * type文の追加。
     *
     * @param type
     *          構造体
     * @param variableName
     *          変数名
     */
    public void add(Type type, String variableName) {
        core.add(type, variableName);
    }

    /**
     * structure文の追加。
     *
     * @param structure
     *          構造体
     * @param variableName
     *          変数名
     */
    public void add(Structure structure, String variableName) {
        core.add(structure, variableName);
    }

    /**
     * union文の追加。
     *
     * @param union
     *          共用体
     */
    public void add(Union union) {
        core.add(union);
    }

    /**
     * 構造体内の変数定義文リストの取得。
     *
     * @return 変数定義文リスト
     */
    public List<VariableDefinition> getDefinitions() {
        return core.getDefinitions();
    }
}
