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
import java.util.HashSet;
import java.util.Set;

import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Variable;

/**
*
* 変数のデータ型を示すクラス.<br>
*
* @author RIKEN
*
*/
public class VariableType implements Serializable,
        jp.riken.kscope.language.IVariableType {

    /** シリアル番号 */
    private static final long serialVersionUID = -7257691935972191261L;

    /** 変数の親ブロック */
    private IBlock parentStatement;

    /**
     * 基本データ型を示すenum。
     *
     * @author RIKEN
     *
     */
    public enum PrimitiveDataType {
        /** 不定. */
        UNKOWN("Unkown"),
        /** 型なし（subroutineの型）. */
        VOID(""),
        /** byte型. */
        BYTE("byte"),
        /** integer型. */
        INTEGER("integer"),
        /** real型. */
        REAL("real"),
        /** double precision型. */
        DOUBLE_PRECISION("double precision"),
        /** logical型. */
        LOGICAL("logical"),
        /** complex型. */
        COMPLEX("complex"),
        /** double complex型. */
        DOUBLE_COMPLEX("double complex"),
        /** character型. */
        CHARACTER("character"),
        /** type型. */
        TYPE("type"),
        /** structure型. */
        STRUCTURE("structure"),
        /** union型. */
        UNION("union");

        /** データ型名 */
        private String name = "";

        /**
         * コンストラクタ。
         *
         * @param nm
         *          型名
         */
        private PrimitiveDataType(String nm) {
            this.name = nm;
        }

        /**
         * 型名の取得。
         *
         * @return 型名
         */
        public String getName() {
            return name;
        }

        /**
         * 型名からVariableTypeを検索する.
         *
         * @param typeName
         *          型名
         *
         * @return 型名に対応したVariableType
         *
         */
        public static PrimitiveDataType findTypeBy(String typeName) {
            PrimitiveDataType result = PrimitiveDataType.UNKOWN;
            for (PrimitiveDataType type : PrimitiveDataType.values()) {
                if (type.getName().equalsIgnoreCase(typeName)) {
                    result = type;
                    break;
                }
            }
            return result;
        }
    }

    /** 基本データ型 */
    private PrimitiveDataType primitiveDataType = PrimitiveDataType.UNKOWN;
    /** KIND属性値 */
    private Expression kind = null;
    /** LEN属性値 */
    private Expression len = null;
    /** TYPE文 */
    private Type type = null;
    /** STRUCTURE文 */
    private Structure structure = null;
    /** UNION文 */
    private Union union = null;

    /**
     * コンストラクタ。
     *
     * @param typ
     *          基本データ型
     */
    public VariableType(PrimitiveDataType typ) {
        this.primitiveDataType = typ;
    }

    /**
     * コンストラクタ。
     *
     * @param value
     *          type型の型定義
     */
    public VariableType(Type value) {
        this.primitiveDataType = PrimitiveDataType.TYPE;
        this.type = new Type(value);
    }

    /**
     * コンストラクタ。
     *
     * @param value
     *          structure型の型定義
     */
    public VariableType(Structure value) {
        this.primitiveDataType = PrimitiveDataType.STRUCTURE;
        this.structure = value;
    }

    /**
     * コンストラクタ。
     *
     * @param value
     *          union型の型定義
     */
    public VariableType(Union value) {
        this.primitiveDataType = PrimitiveDataType.UNION;
        this.union = value;
    }

    /**
     * コピーコンストラクタ。
     *
     * @param value        データ型
     */
    public VariableType(VariableType value) {
        if (value == null) return;
        this.primitiveDataType = value.primitiveDataType;
        this.kind = value.kind;
        this.len = value.len;
        this.structure = value.structure;
        this.type = value.type;
        this.union = value.union;
    }

    /**
     * 型名の取得。
     *
     * @return 型名。<br>
     *         ただし、基本データ型がstructureあるいはtypeの場合は、<br>
     *         structure名あるいはtype名を返します。
     */
    @Override
    public String getName() {
        String result = "";
        if (this.primitiveDataType == PrimitiveDataType.TYPE) {
            if (this.type != null) {
                result = this.type.getName();
            }
        } else if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) {
            if (this.structure != null) {
                result = this.structure.getName();
            }
        } else {
            result = primitiveDataType.getName();
        }
        return result;
    }

    /**
     * 基本データ型の取得.
     *
     * @return 基本データ型
     */
    public PrimitiveDataType getPrimitiveDataType() {
        return primitiveDataType;
    }

    /**
     * kind値の取得.
     *
     * @return kind値
     */
    public Expression getKind() {
        return kind;
    }

    /**
     * kind値の設定.
     *
     * @param value
     *          kind値
     */
    public void setKind(Expression value) {
        kind = value;
    }

    /**
     * len値の取得.
     *
     * @return len値
     */
    public Expression getLen() {
        return len;
    }

    /**
     * len値の設定.
     *
     * @param value
     *          len値
     */
    public void setLen(Expression value) {
        len = value;
    }

    /**
     * type型の取得.
     *
     * @return type型の値
     */
    public Type getType() {
        return type;
    }

    /**
     * type型の設定.
     * @param type型の値
     */
    public void setType(Type type) {
        this.type = type;
    }

    /**
     * structure型の取得.
     *
     * @return structure型の値
     */
    public Structure getStructure() {
        return structure;
    }

    /**
     * union型の取得.
     *
     * @return union型の値
     */
    public Union getUnion() {
        return union;
    }

    /**
     * 型名からVariableTypeを検索する.
     *
     * @param typeName
     *          型名
     *
     * @return 型名に対応したVariableType
     *
     */
    @Override
    public VariableType findTypeBy(String typeName) {
        VariableType result = new VariableType(PrimitiveDataType.UNKOWN);
        result.primitiveDataType = PrimitiveDataType.findTypeBy(typeName);
        return result;
    }

    /**
     * 型が適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の型チェックをする必要がある。<br>
     * 「適合している」とは、この型チェックで、同一の型と判定される 事を意味している。
     *
     * @param value
     *          型
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     *
     */
    @Override
    public boolean matches(jp.riken.kscope.language.IVariableType value) {
        if (!(value instanceof VariableType)) {
            return false;
        }

        boolean result = false;
        VariableType targetType = (VariableType) value;

        if (targetType.getPrimitiveDataType() == this.primitiveDataType) {
            switch (this.primitiveDataType) {
            case STRUCTURE:
                if (this.structure == null) {
                    break;
                }
                result = this.structure.matches(targetType.getStructure());
                break;
            case TYPE:
                if (this.type == null) {
                    break;
                }
                result = this.type.matches(targetType.getType());
                break;
            case UNION:
                break;
            default:
                if (this.kind == null) {
                    if (targetType.getKind() == null) {
                        result = true;
                    } else {
                        result = false;
                    }
                } else {
                    if (targetType.getKind() == null) {
                        result = false;
                    } else {
                        //TODO kindが変数で表されている場合、実行時には同じkindなのにfalseを返す場合がある。
                        result = (targetType.getKind().toString().equals(this.kind
                                .toString()));
                    }
                }
                break;
            }

        }
        return result;
    }


    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        if (this.primitiveDataType != null) {
            if (this.primitiveDataType == PrimitiveDataType.TYPE) {
                if (this.type != null) {
                    buf.append(this.type.toString());
                }
            } else if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) {
                if (this.structure != null) {
                    buf.append(this.structure.toString());
                }
            } else if (this.primitiveDataType != PrimitiveDataType.UNKOWN) {
                buf.append(this.primitiveDataType.getName());
            }

            if (kind != null) {
                if (this.isKindDouble()) {
                    buf.append("(kind(0d0))");
                }
                else {
                    buf.append("(" + kind + ")");
                }
            }
            if (len != null) {
                buf.append("(len=" + len + ")");
            }
        }

        return buf.toString();
    }

    /**
     * 実数変数であるかチェックする.
     * @return        true=実数
     */
    @Override
    public boolean isRealType() {
        if (this.primitiveDataType == null) return false;
        if (this.primitiveDataType == PrimitiveDataType.REAL) return true;
        if (this.primitiveDataType == PrimitiveDataType.DOUBLE_PRECISION) return true;
        if (this.primitiveDataType == PrimitiveDataType.COMPLEX) return true;
        if (this.primitiveDataType == PrimitiveDataType.DOUBLE_COMPLEX) return true;

        return false;
    }

    /**
     * 整数変数であるかチェックする.
     * @return        true=整数
     */
    @Override
    public boolean isIntegerType() {
        if (this.primitiveDataType == null) return false;
        if (this.primitiveDataType == PrimitiveDataType.INTEGER) return true;
        if (this.primitiveDataType == PrimitiveDataType.BYTE) return true;

        return false;
    }


    /**
     * structure型であるかチェックする
     * @return        true=structure
     */
    @Override
    public boolean isStruct() {
        if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) return true;
        if (this.primitiveDataType == PrimitiveDataType.TYPE) return true;
        return false;
    }

    /**
     * 変数リストを取得する.
     */
    @Override
    public Set<Variable> getAllVariables() {
        Set<Variable> vars = new HashSet<Variable>();
        if (this.kind != null) {
            Set<Variable> list = this.kind.getAllVariables();
            if (list != null && list.size() > 0) {
                vars.addAll(list);
            }
        }

        if (this.len != null) {
            Set<Variable> list = this.len.getAllVariables();
            if (list != null && list.size() > 0) {
                vars.addAll(list);
            }
        }
        if (this.structure != null) {
            Set<Variable> list = this.structure.getAllVariables();
            if (list != null && list.size() > 0) {
                vars.addAll(list);
            }
        }

        if (vars.size() <= 0) return null;

        return vars;
    }

    /**
     * 親ブロックを設定する.
     * @param parent 親ブロック
     */
    public void setParentStatement(IBlock parent) {
        this.parentStatement = parent;
        // 子変数に対して設定する

        if (this.kind != null) {
            this.kind.setParentStatement(parent);
        }

        if (this.len != null) {
            this.len.setParentStatement(parent);
        }
        if (this.structure != null) {
            this.structure.setMotherBlock(parent);
        }
        if (this.type != null) {
            this.type.setMotherBlock(parent);
        }
    }

    /**
     * double型表記のkindであるかチェックする
     * @return        true=double型kind
     */
    public boolean isKindDouble() {
        if (primitiveDataType != null) {
            if (kind != null) {
                String kind_str = kind.toString();
                if (kind_str.toLowerCase().indexOf("0d0") >= 0) {
                    return true;
                }
            }
        }

        return false;
    }
}
