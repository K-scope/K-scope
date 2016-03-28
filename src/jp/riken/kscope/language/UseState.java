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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


/**
 * モジュールへの参照を表現するクラス。 FortranにおけるUSE文に対応。
 * C言語におけるinclude文に対応させる      at 2015/09/01 by @hira
 * @author RIKEN
 *
 */
public class UseState extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 4969852934134083633L;
    private String moduleName;
    private Set<String> onlyMember;
    private Map<String, String> translationName;
    private Map<String, String> translationNameReverse;

    /**
     * コンストラクタ
     */
    public UseState() {
    }

    /**
     * コンストラクタ
     * @param module_name            モジュール名
     */
    public UseState(String module_name) {
        this.moduleName = module_name;
    }

    @Override
    public String toString() {
        StringBuilder st = new StringBuilder(this.moduleName);
        if (this.onlyMember != null) {
            st.append(", only:");
            for (String on: this.getOnlyMember()) {
                st.append(" " + on);
            }
        }
        return st.toString();
    }
    /**
     * 変数名の変換マップを返す。
     * @return 変数名の変換マップ＜変換後の変数名、モジュールでの変数名＞.ない場合はnullを返す。
     */
    public Map<String, String> getTranslationName() {
        return translationName;
    }
    /**
     * 変換後の名前に対応する元の変数名を返す。
     * @param nm 変換後の名前
     * @return 元の変数名。見つからなければnullを返す。
     */
    public String getTranslationName(String nm) {
        if (this.translationName == null) {
            return null;
        } else {
            return this.translationName.get(nm);
        }
    }
    /**
     * 変数名の変換マップを取得する（逆）。
     * @return 変数名の変換マップ＜モジュールでの変数名、変換後の変数名＞
     */
    public Map<String, String> getTranslationNameReverse() {
        return translationNameReverse;
    }

    /**
     * 変数名の変換マップをセットする。
     * @param transName 変数名の変換マップ＜変換後の変数名、モジュールでの変数名＞
     */
    @Deprecated
    public void setTranslationName(Map<String, String> transName) {
        this.translationName = transName;
    }

    /**
     * 変数名の変換対応を追加する。
     * @param transName 変換後の名前
     * @param orgName モジュール内での名前
     *
     */
    public void addTranslationName(String transName, String orgName) {
        if (this.translationName == null) {
            this.translationName = new HashMap<String, String>();
            this.translationNameReverse = new HashMap<String, String>();
        }
        this.addOnlyMember(orgName);
        this.translationName.put(transName, orgName);
        this.translationNameReverse.put(orgName, transName);
    }

    /**
     * 参照するモジュールの名前をセットする。
     *
     * @param name
     *            モジュール名
     */
    public void setModuleName(String name) {
        moduleName = name.toLowerCase();
    }

    /**
     * 参照するモジュールの名前を取得する。
     *
     * @return モジュール名
     */
    public String getModuleName() {
        return moduleName;
    }

    /**
     * 参照する変数・サブルーチンが限定される場合に、その名前を追加する。
     *
     * @param member
     *            変数・サブルーチン名
     */
    public void addOnlyMember(String member) {
        if (this.onlyMember == null) {
            this.onlyMember = new HashSet<String>();
        }
        onlyMember.add(member.toLowerCase());
    }

    /**
     * 参照する変数・サブルーチン名を取得する。
     *
     * @return 変数・サブルーチン名のリスト。無ければ空のセットを返す。
     */
    public Set<String> getOnlyMember() {
        if (this.onlyMember == null) {
            return new HashSet<String>();
        }
        return onlyMember;
    }

    /**
     * 参照する変数・サブルーチン名を保持しているか判定する。
     * @param name 変数・サブルーチン名
     * @return 名前を保持していれば真
     */
    public boolean hasOnlyMember(String name) {
        if (onlyMember != null) {
            if (onlyMember.contains(name)) {
                return true;
            }
        }
        return false;
    }
    /**
     * only句を持つか判定する。
     * @return only句を持つ場合：真
     */
    public boolean hasOnlyMember() {
        if (onlyMember == null) return false;
        if (onlyMember.size() > 0) {
            return true;
        }
        return false;
    }

    /**
     * 与えられた変数を変換するルールがある場合、変換後の名前を返す。
     *
     * @param var
     *            変換前の変数（モジュールで宣言されている変数）
     * @return 変数名。ルールが無ければ変換前の変数名を返す。
     */
    public String translation(VariableDefinition var) {
        ProgramUnit prog = var.getParentProgram();
        if (prog == null) return var.get_name();

        if (prog.get_name().equalsIgnoreCase(this.getModuleName())) {
            if (this.translationNameReverse != null) {
                String nm = this.translationNameReverse.get(var.get_name());
                if (nm != null) {
                    return nm;
                }
            }
        }
        return var.get_name();
    }

    /**
     * 与えられた変数名に変換するルールがある場合、変換前の変数名を返す。
     *
     * @param name
     *            変換後の変数名
     * @return 変換前の変数名（モジュールで宣言されている名前)。ルールが無ければ元の名前を返す。
     */
    public String translationReverse(String name) {
        if (this.translationName != null) {
            String nm = this.translationName.get(name);
            if (nm != null) {
                return nm;
            }
        }
        return name;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.USE
     */
    public BlockType getBlockType() {
        return BlockType.USE;
    }

    /**
     * モジュール名が等しいかチェックする.
     * @param mod_name        チェック対象モジュール名
     * @return        true=モジュール名が等しい
     */
    public boolean equalsModuleName(String mod_name) {
        if (mod_name == null) return false;
        if (this.moduleName == null) return false;
        if (this.isClang()) {
            // C言語
            return this.moduleName.equals(mod_name);
        }
        else {
            // Fortran
            return this.moduleName.equalsIgnoreCase(mod_name);
        }
    }

    /**
     * USE文メンバ変数に対象変数名が存在するかチェックする.
     * @param var_name        検索対象変数名
     * @return        true=メンバに存在する
     */
    public boolean containsMember(String var_name) {

        Set<String> om = this.getOnlyMember();
        for (String oName : om) {
            if (this.isClang()) {
                if (oName.equals(var_name)) {
                    return true;
                }
            }
            else {
                if (oName.equalsIgnoreCase(var_name)) {
                    return true;
                }
            }
        }

        return false;
    }
}


