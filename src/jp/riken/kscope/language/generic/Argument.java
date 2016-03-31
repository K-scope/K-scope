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

import java.io.Serializable;

import jp.riken.kscope.language.IVariableAttribute;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;

/**
 * 総称関数の仮引数に対応したクラス。
 *
 * @author RIKEN
 *
 */
public class Argument implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = -5912521170672226755L;
    private IVariableType type;
    private IVariableAttribute attribute;
    private String name;

    /**
     * コンストラクタ。
     * @param nm 名前
     */
    public Argument(String nm) {
        this.name = nm;
    }


    /**
     * コンストラクタ。
     *
     * @param typ
     *          仮引数の型
     * @param attrbt
     *          仮引数の属性
     */
    public Argument(IVariableType typ, IVariableAttribute attrbt) {
        type = typ;
        attribute = attrbt;
    }

    /**
     * コピーコンストラクタ。
     * @param dest        コピー元
     */
    public Argument(Argument dest) {
        if (dest == null) return;

        if (dest.type != null) {
            this.type = new VariableType((VariableType)dest.type);
        }
        if (dest.attribute != null) {
            this.attribute = new VariableAttribute((VariableAttribute)dest.attribute);
        }
        this.name = dest.name;
    }

    /**
     * 仮引数の型をセットする。
     * @param tp 仮引数の型
     */
    public void setType(IVariableType tp) {
        this.type = tp;
    }

    /**
     * 仮引数の属性をセットする。
     * @param att 仮引数の属性
     */
    public void setVariableAttributes(IVariableAttribute att) {
        this.attribute = att;
    }

    /**
     * 仮引数の型の取得。
     *
     * @return 仮引数の型
     */
    public IVariableType getType() {
        return type;
    }

    /**
     * 仮引数の属性の取得。
     *
     * @return 仮引数の属性
     */
    public IVariableAttribute getAttribute() {
        return attribute;
    }

    /**
     * 引数の型と属性が適合しているか。
     *
     * @param typ
     *         対象となる引数の型
     * @param attrbt
     *         対象となる属性
     *
     * @return true:  適合している。<br>
     *         false: 適合していない。
     */
    public boolean matches(IVariableType typ, IVariableAttribute attrbt) {
        if (typ == null || attrbt == null
         || this.type == null || this.attribute == null) {
            return false;
        }
        return (this.type.matches(typ) && this.attribute.matches(attrbt));
    }


    /**
     * 引数名を取得する。
     * @return 引数名
     */
    public String getName() {
        return this.name;
    }

    /**
     * 引数の定義文を取得する
     * @return        変数定義文表記
     */
    public String toDefinition() {
        StringBuilder buf = new StringBuilder();
        if (this.type != null) {
            buf.append(this.type.toString());
        }
        if (this.attribute != null) {
            buf.append(",");
            buf.append(this.attribute.toString());
        }
           buf.append(" :: ");
           buf.append(this.name);

           return buf.toString();
    }
}
