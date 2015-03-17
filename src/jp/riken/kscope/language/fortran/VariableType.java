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

import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IVariableAttribute;
import jp.riken.kscope.utils.StringUtils;

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
        UNION("union"),

        /** C:void型 */
        CVOID("void"),
        /** C:char型 */
        CHAR("char"),
        /** C:short型 */
        SHORT("short"),
        /** C:int型 */
        INT("int"),
        /** C:long型 */
        LONG("long"),
        /** C:long long型 */
        LONG_LONG("long long"),
        /** C:unsigned char型 */
        UNSIGNED_CHAR("unsigned char"),
        /** C:unsigned short型 */
        UNSIGNED_SHORT("unsigned short"),
        /** C:unsigned int型 */
        UNSIGNED("unsigned int"),
        /** C:unsigned long型 */
        UNSIGNED_LONG("unsigned long"),
        /** C:unsigned long long型 */
        UNSIGNED_LONG_LONG("unsigned long long"),
        /** C:float型 */
        FLOAT("float"),
        /** C:double型 */
        DOUBLE("double"),
        /** C:long double型 */
        LONG_DOUBLE("long double"),
        /** C:bool型 */
        BOOL("bool"),
        /** C:wchar_t型 */
        WCHAR_T("wchar_t"),
        /** C:float complex型 */
        FLOAT_COMPLEX("float complex"),
        /** C:double complex型 */
        // DOUBLE_COMPLEX("double complex"),
        /** C:long double complex型 */
        LONG_DOUBLE_COMPLEX("long double complex"),
        /** C:float complex型 */
        FLOAT_IMAGINARY("float complex"),
        /** C:double complex型 */
        DOUBLE_IMAGINARY("double complex"),
        /** C:long double complex型 */
        LONG_DOUBLE_IMAGINARY("long double complex"),
        /** C:__builtin_va_list型 */
        __BUILTIN_VA_ARG("__builtin_va_list")
        ;

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
         * タイプ名からVariableTypeを検索する.
         * UNSIGNED("unsigned int") = タイプ名(データ型名)
         * @param typeName          タイプ名
         *
         * @return タイプ名に対応したVariableType
         *
         */
        public static PrimitiveDataType findByTypeName(String typeName) {
            PrimitiveDataType result = PrimitiveDataType.UNKOWN;
            for (PrimitiveDataType type : PrimitiveDataType.values()) {
                if (type.toString().equalsIgnoreCase(typeName)) {
                    result = type;
                    break;
                }
            }
            return result;
        }

        /**
         * データ型名からVariableTypeを検索する.
         * UNSIGNED("unsigned int") = タイプ名(データ型名)
         * @param dataName          データ型名
         *
         * @return データ型名に対応したVariableType
         *
         */
        public static PrimitiveDataType findByDataName(String dataName) {
            PrimitiveDataType result = PrimitiveDataType.UNKOWN;
            for (PrimitiveDataType type : PrimitiveDataType.values()) {
                if (type.getName().equalsIgnoreCase(dataName)) {
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
    /** TYPE文:Fortran構造体  */
    private Type type = null;
    /** STRUCTURE文:C言語構造体,共同体 */
    private Structure structure = null;
    /** UNION文:Fortranのみ （C言語では未使用） */
    private Union union = null;
    /** 属性. */
    private IVariableAttribute attribute;

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
        type = value;
    }

    /**
     * コンストラクタ。
     *
     * @param value
     *          structure型の型定義
     */
    public VariableType(Structure value) {
        this.primitiveDataType = PrimitiveDataType.STRUCTURE;
        structure = value;
    }

    /**
     * コンストラクタ。
     *
     * @param value
     *          union型の型定義
     */
    public VariableType(Union value) {
        this.primitiveDataType = PrimitiveDataType.UNION;
        union = value;
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
            if (type != null) {
                result = type.getName();
            }
        } else if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) {
            if (structure != null) {
                result = structure.getName();
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
        result.primitiveDataType = PrimitiveDataType.findByTypeName(typeName);
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
        if (primitiveDataType != null) {
            buf.append(this.getName());
            if (kind != null) {
                buf.append("(" + kind + ")");
            }
            if (len != null) {
                buf.append("(len=" + len + ")");
            }
        }

        return buf.toString();
    }

    @Override
    public String toStringClang() {
        StringBuffer buf = new StringBuffer();
        if (primitiveDataType != null) {
            if (structure != null) {
                if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) {
                    BlockType block_type = structure.getBlockType();
                    if (block_type == BlockType.STRUCT
                        || block_type == BlockType.UNION
                        || block_type == BlockType.ENUM) {
                        buf.append(block_type.toString().toLowerCase());
                        buf.append(" ");
                    }
                }
            }
            if (this.isVoid() && this.isPointer()) {
                buf.append("void*");
            }
            else {
                buf.append(this.getName());
            }
        }

        return buf.toString();
    }

    public String toStringFull() {
        StringBuffer buf = new StringBuffer();
        if (primitiveDataType != null) {
            if (structure != null) {
                if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) {
                    BlockType block_type = structure.getBlockType();
                    if (block_type == BlockType.STRUCT
                        || block_type == BlockType.UNION
                        || block_type == BlockType.ENUM) {
                        buf.append(block_type.toString().toLowerCase());
                        buf.append(" ");
                    }
                }
            }
            if (this.primitiveDataType == PrimitiveDataType.VOID) {
                buf.append("void");
            }
            else {
                buf.append(this.getName());
            }
        }

        // 属性
        String var_attr = null;
        if (this.attribute != null) {
            boolean is_pointer = this.attribute.contains("pointer");
            var_attr = this.attribute.toStringClang();
            if (is_pointer) {
                buf.append("*");
            }
        }
        if (StringUtils.isNullOrEmpty(var_attr)) {
            return buf.toString();
        }

        return var_attr + " " + buf.toString();
    }

    /**
     * 実数変数であるかチェックする.
     * @return		true=実数
     */
    @Override
    public boolean isRealType() {
        if (this.primitiveDataType == null) return false;
        if (this.primitiveDataType == PrimitiveDataType.REAL) return true;
        if (this.primitiveDataType == PrimitiveDataType.DOUBLE_PRECISION) return true;
        if (this.primitiveDataType == PrimitiveDataType.COMPLEX) return true;
        if (this.primitiveDataType == PrimitiveDataType.DOUBLE_COMPLEX) return true;
        if (this.primitiveDataType == PrimitiveDataType.FLOAT) return true;
        if (this.primitiveDataType == PrimitiveDataType.DOUBLE) return true;
        if (this.primitiveDataType == PrimitiveDataType.LONG_DOUBLE) return true;
        if (this.primitiveDataType == PrimitiveDataType.FLOAT_COMPLEX) return true;
        if (this.primitiveDataType == PrimitiveDataType.LONG_DOUBLE_COMPLEX) return true;
        if (this.primitiveDataType == PrimitiveDataType.FLOAT_IMAGINARY) return true;
        if (this.primitiveDataType == PrimitiveDataType.DOUBLE_IMAGINARY) return true;
        if (this.primitiveDataType == PrimitiveDataType.LONG_DOUBLE_IMAGINARY) return true;

        return false;
    }

    /**
     * 整数変数であるかチェックする.
     * @return		true=整数
     */
    @Override
    public boolean isIntegerType() {
        if (this.primitiveDataType == null) return false;
        if (this.primitiveDataType == PrimitiveDataType.INTEGER) return true;
        if (this.primitiveDataType == PrimitiveDataType.BYTE) return true;

        return false;
    }

    /**
     * 属性をセットする。
     *
     * @param att
     *            属性
     */
    public void setVariableAttributes(IVariableAttribute att) {
        attribute = att;
    }

    /**
     * 属性を取得する
     *
     * @return 属性
     */
    public IVariableAttribute getAttribute() {
        return attribute;
    }

    /**
     * ポインタであるかチェックする
     * @return		true=pointer
     */
    public boolean isPointer() {
        if (this.attribute == null) return false;
        return this.attribute.contains("pointer");
    }

    /**
     * voidデータ型であるかチェックする
     * @return		true=pointer
     */
    public boolean isVoid() {
        if (this.primitiveDataType == PrimitiveDataType.VOID) return true;
        if (this.primitiveDataType == PrimitiveDataType.CVOID) return true;
        return false;
    }


    /**
     * Functionデータ型であるかチェックする
     * @return		true=function
     */
    public boolean isFunction() {
        if (this.attribute == null) return false;
        return this.attribute.contains("function");
    }
}
