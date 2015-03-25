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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * 変数の属性を示すクラス.<br>
 *
 * @author RIKEN
 * @version    2015/03/15     C言語変数属性の追加
 *
 */
public class VariableAttribute implements Serializable,
        jp.riken.kscope.language.IVariableAttribute {
    /** シリアル番号 */
    private static final long serialVersionUID = 3279154150773399072L;

    /**
     * 変数のスコープ属性(public/private/[未指定]).
     */
    public enum ScopeAttribute {
        /** 未指定. */
        NONE,
        /** public/global */
        PUBLIC,
        /** private/local. */
        PRIVATE,
        /** param:C */
        PARAM
    }

    /**
     * 変数のpointer(あるいはtarget)属性(pointer/target/[未指定]).
     */
    public enum PointerAttribute {
        /** 未指定. */
        NONE,
        /** pointer. */
        POINTER,
        /** target. */
        TARGET,
        /** array : C言語のみ:表示出力は行わない. */
        ARRAY,
    }

    /**
     * 外部(external)、内部(intrinsic)、未指定([none])からなる、関数の位置属性。
     */
    public enum FunctionPositionAttribute {
        /** 未指定. */
        NONE,
        /** 外部. */
        EXTERNAL,
        /** 内部. */
        INTRINSIC,
    }

    /**
     * intent属性。
     */
    public enum IntentAttribute {
        /** 未指定. */
        NONE,
        /** 入力. */
        IN,
        /** 出力. */
        OUT,
        /** 入出力. */
        INOUT,
    }

    /**
     * 記憶クラス:sclass属性 : C言語
     */
    public enum SclassAttribute {
        /** 未指定. */
        NONE,
        /** 自動変数 */
        AUTO,
        /** 引数 */
        PARAM,
        /** 外部変数 */
        EXTERN,
        /**  extern省略外部変数 */
        EXTERN_DEF,
        /** 静的変数 */
        STATIC,
        /** レジスタ */
        REGISTER,
        /** ラベル */
        LABEL,
        /** タグ名 */
        TAGNAME,
        /** 構造体メンバ */
        MOE,
        /** 別名定義 : typedef */
        TYPEDEF_NAME,
        /** GCC拡張ラベル */
        GCCLABEL,
    }

    /** 全属性の情報. */
    private List<String> attributes = new ArrayList<String>();

    /**
     * デフォルトコンストラクタ。
     */
    public VariableAttribute() {
        // 何もしない
    }

    /**
     * コンストラクタ。
     *
     * @param attrbts
     *            全属性リスト
     */
    public VariableAttribute(List<String> attrbts) {
        this.setAttributes(attrbts);
    }

    /**
     * parameter属性を持つかどうか.
     *
     * @return true : parameter属性を持つ
     *         false: parameter属性を持たない
     */
    public boolean hasParameter() {
        return this.contains("parameter");
    }

    /**
     * スコープ属性(public/private/[未指定])の取得.
     *
     * @return NONE   : 未指定
     *         PUBLIC : public属性を持つ
     *         PRIVATE: private属性を持つ
     */
    public ScopeAttribute getScope() {
        ScopeAttribute result = ScopeAttribute.NONE;
        if (this.contains("public")) {
            result = ScopeAttribute.PUBLIC;
        } else if (this.contains("private")) {
            result = ScopeAttribute.PRIVATE;
        } else if (this.contains("global")) {
            result = ScopeAttribute.PUBLIC;
        } else if (this.contains("local")) {
            result = ScopeAttribute.PRIVATE;
        } else if (this.contains("param")) {
            result = ScopeAttribute.PARAM;
        }
        return result;
    }

    /**
     * optional属性を持つかどうか.
     *
     * @return true : optional属性を持つ
     *         false: optional属性を持たない
     */
    public boolean hasOptional() {
        return this.contains("optional");
    }

    /**
     * 変数のpointer(あるいはtarget)属性の取得.
     *
     * @return NONE   : 未指定
     *         POINTER: pointer属性を持つ
     *         TARGET : target属性を持つ
     */
    public PointerAttribute getPointerOrTarget() {
        PointerAttribute result = PointerAttribute.NONE;
        if (this.contains("pointer")) {
            result = PointerAttribute.POINTER;
        } else if (this.contains("private")) {
            result = PointerAttribute.TARGET;
        }
        return result;
    }

    /**
     * save属性を持つかどうか.
     *
     * @return true : save属性を持つ
     *         false: save属性を持たない
     */
    public boolean hasSave() {
        return this.contains("save");
    }

    /**
     * 関数の位置属性の取得.
     *
     * @return NONE     : 未指定
     *         EXTERNAL : external属性を持つ
     *         INTRINSIC: intrinsic属性を持つ
     */
    public FunctionPositionAttribute getFunctionPosition() {
        FunctionPositionAttribute result = FunctionPositionAttribute.NONE;
        if (this.contains("external")) {
            result = FunctionPositionAttribute.EXTERNAL;
        } else if (this.contains("intrinsic")) {
            result = FunctionPositionAttribute.INTRINSIC;
        }
        return result;
    }

    /**
     * equivalence属性を持つかどうか.
     *
     * @return true : equivalence属性を持つ
     *         false: equivalence属性を持たない
     */
    public boolean hasEquivalence() {
        return this.contains("equivalence");
    }

    /**
     * common属性を持つかどうか.
     *
     * @return true : common属性を持つ
     *         false: common属性を持たない
     */
    public boolean hasCommon() {
        return this.contains("common");
    }

    /**
     * dimension属性を持つかどうか.
     *
     * @return true : dimension属性を持つ
     *         false: dimension属性を持たない
     */
    public boolean hasDimension() {
        return this.contains("dimension");
    }

    /**
     * 変数のintent属性の取得.
     *
     * @return NONE : 未指定
     *         IN   : intent(in)属性を持つ
     *         OUT  : intent(out)属性を持つ
     *         INOUT: intent(inout)属性を持つ
     */
    public IntentAttribute getIntent() {
        IntentAttribute result = IntentAttribute.NONE;
        String intentItem = this.getAttributeBy("intent");
        if (intentItem != "") {
            if (intentItem.toLowerCase().contains("inout")) {
                result = IntentAttribute.INOUT;
            } else if (intentItem.toLowerCase().contains("out")) {
                result = IntentAttribute.OUT;
            } else if (intentItem.toLowerCase().contains("in")) {
                result = IntentAttribute.IN;
            }
        }
        return result;
    }

    /**
     * allocatable属性を持つかどうか.
     *
     * @return true : allocatable属性を持つ
     *         false: allocatable属性を持たない
     */
    public boolean hasAllocatable() {
        return this.contains("allocatable");
    }

    /**
     * 属性の設定.
     *
     * @param attrbts
     *            設定すべき全属性
     */
    @Override
    public void setAttributes(List<String> attrbts) {
        if (attrbts != null) {
            attributes = attrbts;
        }
    }

    /**
     * 属性の設定.
     *
     * @param attrbts
     *            設定すべき全属性
     */
    @Override
    public void setAttributes(String[] attrbts) {
        if (attrbts != null) {
            attributes.clear();
            for (String attribute : attrbts) {
                attributes.add(attribute);
            }
        }
    }

    /**
     * 属性の取得.
     *
     * @return 全属性
     */
    @Override
    public List<String> getAttributes() {
        return attributes;
    }

    /**
     * 属性の追加.
     *
     * @param attrbt
     *           追加すべき属性
     */
    @Override
    public void addAttribute(String attrbt) {
        if (attrbt == null || attrbt.isEmpty()) {
            return;
        }
        attributes.add(attrbt);
    }

    /**
     * 変数の属性を返す.
     *
     * @return 変数の属性文字列
     */
    @Override
    public String toString() {
        String result = "";

        for (String attrbt : attributes) {
            if (PointerAttribute.ARRAY.toString().equalsIgnoreCase(attrbt)) continue;
            result = result + ", " + attrbt;
        }
        result = result.trim();

        /* 頭の","を削除する */
        if (result.startsWith(",")) {
            result = result.substring(1);
            result = result.trim();
        }

        return result;
    }

    /**
     * C言語変数の属性を返す.
     * @return 変数の属性文字列
     */
    @Override
    public String toStringClang() {
        if (this.attributes == null || this.attributes.size() <= 0) return null;
        StringBuffer buf = new StringBuffer();

        // sclass
        SclassAttribute sclass_value = this.getSclass();

        // その他の属性
        for (String attr : this.attributes) {
            if (attr == null) continue;
            if (PointerAttribute.POINTER.toString().equalsIgnoreCase(attr)) continue;
            if (PointerAttribute.ARRAY.toString().equalsIgnoreCase(attr)) continue;
            if (attr.equalsIgnoreCase(sclass_value.toString())) continue;
            buf.append(attr);
            buf.append(" ");
        }
        String result = buf.toString().trim().toLowerCase();

        // sclass
        String sclass = null;
        // 外部変数
        if (sclass_value == SclassAttribute.EXTERN) sclass = sclass_value.toString().toLowerCase();
        // 静的変数
        if (sclass_value == SclassAttribute.STATIC) sclass = sclass_value.toString().toLowerCase();
        // レジスタ
        if (sclass_value == SclassAttribute.REGISTER) sclass = sclass_value.toString().toLowerCase();

        if (sclass != null) {
            result = sclass + " " + result;
        }

        return result;
    }

    /**
     * 対象文字列が属性内に含まれているかどうか。<br>
     * ただし、対象文字列の大文字小文字は無視する。<br>
     *
     * @param keyword
     *               対象文字列
     * @return true : 対象文字列が含まれる
     */
    @Override
    public boolean contains(String keyword) {
        return (this.getAttributeBy(keyword) != "");
    }

    /**
     * 対象文字列（大文字、小文字は無視）が含まれている属性の取得.
     *
     * @param keyword
     *               対象文字列
     * @return 最初に見つかった属性<br>
     *         見つからなかった場合は、空文字を返します。
     */
    public String getAttributeBy(String keyword) {
        String result = "";
        for (String item : attributes) {
            // すべて小文字にして比較する
            if (item.toLowerCase().equals(keyword.toLowerCase())) {
                result = item;
                break;
            }
        }
        return result;
    }
    /**
     * 次元数の取得。
     *
     * @return 次元数
     */
    public int getDimensionNum() {
        int result = 1;
        String dimensionAttr = getAttributeBy("dimension");
        if (!dimensionAttr.equals("")) {
            result = dimensionAttr.split(",").length;
        }
        return result;
}

    /**
     * 属性が適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の属性チェックをする必要がある。<br>
     * 「適合している」とは、この属性チェックで、同一の属性と判定される
     * 事を意味している。fortranの場合は、次元数のチェックのみ行う。
     *
     * @param value
     *          属性
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     *
     */
    @Override
    public boolean matches(jp.riken.kscope.language.IVariableAttribute value) {

        if (!(value instanceof VariableAttribute)) {
            return false;
        }
        VariableAttribute target = (VariableAttribute) value;

        return (this.getDimensionNum() == target.getDimensionNum());
    }


    /**
     * 記憶クラス属性(sclass)の取得.
     * @return 記憶クラス:SclassAttribute
     */
    public SclassAttribute getSclass() {
        SclassAttribute result = SclassAttribute.NONE;
        for (SclassAttribute sclass : SclassAttribute.values()) {
            if (this.contains(sclass.name().toLowerCase())) {
                result = sclass;
                break;
            }
        }

        return result;
    }

    /**
     * ポインタと配列との優先順位をチェックする.
     * true : ポインタ優先  =  (*plist)[10]
     * false : 配列優先  =  *parray[10]
     * @return		true : ポインタ優先
     */
    public boolean  isPointerArrayPriority() {
        int pointer = -1;
        int array = -1;
        int count = 0;
        for (String attr : this.attributes) {
            if (PointerAttribute.POINTER.toString().equalsIgnoreCase(attr)) {
                pointer = count;
            }
            if (PointerAttribute.ARRAY.toString().equalsIgnoreCase(attr)) {
                array = count;
            }
            count++;
        }
        if (pointer < 0) return false;
        if (array < 0) return false;
        if (array < pointer) return false;

        return true;
    }

    /**
     * 対象文字列が属性内に含まれている数を取得する。<br>
     * ただし、対象文字列の大文字小文字は無視する。<br>
     * @param keyword              対象文字列
     * @return  count: 対象文字列が含まれる数
     */
    @Override
    public int countOf(String keyword) {

        int count = 0;
        for (String item : attributes) {
            // すべて小文字にして比較する
            if (item.toLowerCase().equals(keyword.toLowerCase())) {
                count++;
            }
        }
        return count;
    }
}
