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
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;


/**
 * モジュールへの参照を表現するクラス。 FortranにおけるUSE文に対応。
 *
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
     * コピーコンストラクタ
     * @param   dest     コピー元UseStateクラス
     */
    public UseState(UseState dest) {
        super.clone(dest);
        this.moduleName = dest.moduleName;
        if (dest.onlyMember != null) {
            this.onlyMember = new HashSet<String>();
            for (String value : dest.onlyMember) {
                this.onlyMember.add(value);
            }
        }
        if (dest.translationName != null) {
            this.translationName = new HashMap<String, String>();
            for (String key : dest.translationName.keySet()) {
                String value = dest.translationName.get(key);
                this.translationName.put(key, value);
            }
        }
        if (dest.translationNameReverse != null) {
            this.translationNameReverse = new HashMap<String, String>();
            for (String key : dest.translationNameReverse.keySet()) {
                String value = dest.translationNameReverse.get(key);
                this.translationNameReverse.put(key, value);
            }
        }
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

        // modify onlyなしのrenameに対応 at 2016/03/28 by @hira
        if (this.onlyMember != null && this.onlyMember.size() > 0) {
            st.append(", ");
            st.append("only: ");

            StringBuilder members = new StringBuilder();
            for (String on: this.getOnlyMember()) {
                String trans_name = null;
                if (this.translationNameReverse != null) {
                    trans_name = this.translationNameReverse.get(on);
                }
                if (members.length() > 0) members.append(", ");
                if (trans_name == null) {
                    members.append(on);
                }
                else {
                    // local_name => module_name
                    members.append(trans_name);
                    members.append(" => " + on);
                }
            }
            st.append(members);
        }
        else if (this.translationNameReverse != null) {
            st.append(", ");
            StringBuilder members = new StringBuilder();
            for (String key : this.translationNameReverse.keySet()) {
                String trans_name = this.translationNameReverse.get(key);
                if (trans_name == null) continue;

                if (members.length() > 0) members.append(", ");
                // local_name => module_name
                members.append(trans_name);
                members.append(" => " + key);
            }
            st.append(members);
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
     * use文 : transName => orgName
     * @param transName 変換後の名前
     * @param orgName モジュール内での名前
     * @param only   true=only
     *
     */
    public void addTranslationName(String transName, String orgName, boolean only) {
        if (this.translationName == null) {
            this.translationName = new HashMap<String, String>();
            this.translationNameReverse = new HashMap<String, String>();
        }
        if (only) {
            this.addOnlyMember(orgName);
        }
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
        if (member == null || member.isEmpty()) return;
        if (this.onlyMember == null) {
            this.onlyMember = new LinkedHashSet<String>();
        }
        for (String only : this.onlyMember) {
            if (member.equalsIgnoreCase(only)) {
                return;
            }
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
        ProgramUnit prog = var.getScopeDeclarationsBlock();
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
     * only, transが無ければ変換前の変数名を返す。
     * only, transがあり、変数名が存在しなければ、NULLを返す。
     * @param name
     *            変換後の変数名
     * @return 変換前の変数名（モジュールで宣言されている名前)。ルールが無ければ変換前の変数名を返す。
     * modify by hira at 20160130    変数名が存在しなければ、NULLを返す様に変更
     */
    public String translationReverse(String name) {

        // add 変換するルールがあるかチェックする at 2016/03/28 by @hira
        if (this.hasTranslation(name)) {
            if (this.translationName != null) {
                String nm = this.translationName.get(name);
                if (nm != null) {
                    return nm;     // 変換前の変数名（モジュールで宣言されている名前)
                }
            }
        }
        else {
            if (this.hasOnlyMember()) {
                if (this.hasOnlyMember(name)) {
                    return name;
                }
            }
        }
        return null;
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
     * USE文メンバ変数に対象変数名が存在するかチェックする.
     * @param var_name        検索対象変数名
     * @return        true=メンバに存在する
     */
    public boolean containsMember(String var_name) {

        Set<String> om = this.getOnlyMember();
        for (String oName : om) {
            if (oName.equalsIgnoreCase(var_name)) {
                return true;
            }
        }

        return false;
    }

    /**
     * ONLY句,別名変数から変数を削除する
     * @param var_name        モジュール変数
     */
    public void removeMember(String var_name) {
        if (var_name == null || var_name.isEmpty()) return;

        Set<String> om = this.getOnlyMember();
        Iterator<String> itr = om.iterator();
        while (itr.hasNext()) {
            if (var_name.equalsIgnoreCase(itr.next())) {
                itr.remove();
            }
        }

        if (this.translationName != null) {
            itr = this.translationName.keySet().iterator();
            while (itr.hasNext()) {
                String key = itr.next();
                String value = this.translationName.get(key);
                if (var_name.equalsIgnoreCase(value)) {
                    itr.remove();
                }
            }
        }

        if (this.translationNameReverse != null) {
            this.translationNameReverse.remove(var_name);
        }

        return;
    }

    /**
     * USE文をマージする
     * @param dest_use        マージUSE文
     */
    public void margeUseState(UseState dest_use) {
        if (dest_use == null) return;
        if (dest_use.getModuleName() == null) return;
        // モジュール名が異なる
        if (!(dest_use.getModuleName().equalsIgnoreCase(this.getModuleName()))) return;

        Set<String> onlys = dest_use.getOnlyMember();
        if (onlys != null) {
            for (String only : onlys) {
                this.addOnlyMember(only);
            }
        }

        Map<String, String> trans = dest_use.getTranslationName();
        if (trans != null) {
            for (String key : trans.keySet()) {
                String value = trans.get(key);
                if (this.translationName == null) {
                    this.translationName = new HashMap<String, String>();
                }
                this.translationName.put(key, value);
            }
        }

        Map<String, String> trans_rev = dest_use.getTranslationNameReverse();
        if (trans_rev != null) {
            for (String key : trans_rev.keySet()) {
                String value = trans_rev.get(key);
                if (this.translationNameReverse == null) {
                    this.translationNameReverse = new HashMap<String, String>();
                }
                this.translationNameReverse.put(key, value);
            }
        }

        return;
    }


    /**
     * 変換ルールが存在しているかチェックする。
     * @param name     変換名
     * @return   true=変換ルールが存在している
     */
    public boolean hasTranslation(String name) {
        if (this.translationName != null) {
            String nm = this.translationName.get(name);
            if (nm != null) {
                return  true;
            }
        }

        if (this.translationNameReverse != null) {
            String nm = this.translationNameReverse.get(name);
            if (nm != null) {
                return true;
            }
        }

        return false;
    }
}


