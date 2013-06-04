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

package jp.riken.kscope.xcodeml.xml;

/**
 * データ型識別子
 * @author riken
 */
public enum EnumType {
    /** void型 */
    VOID("Fvoid", null, false),
    /** integer型 */
    INT("Fint", "INTEGER", true),
    /** real型 */
    REAL("Freal", "REAL", true),
    /** complex型 */
    COMPLEX("Fcomplex", "COMPLEX", true),
    /** logical型 */
    LOGICAL("Flogical", "LOGICAL", true),
    /** character型 */
    CHARACTER("Fcharacter", "CHARACTER", true),
    /** numeric型 */
    NUMERIC("Fnumeric", null, true),
    /** numericAll型 */
    NUMERICALL("FnumericAll", null, true),
    /** 不明 */
    DERIVED(null, null, false);

    /** プリミティブ型 */
    private boolean _isPrimitive = false;
    /** XML要素名 */
    private String _xcodemlName;
    /** フォートラン型名 */
    private String _fortranName;

    /**
     * コンストラクタ
     * @param xcodemlName		XML要素名
     * @param fortranName		フォートラン型名
     * @param isPrimitive		true=プリミティブ型
     */
    private EnumType(String xcodemlName, String fortranName, boolean isPrimitive) {
        _isPrimitive = isPrimitive;
        _xcodemlName = xcodemlName;
        _fortranName = fortranName;
    }

    /**
     * プリミティブ型であるか取得する
     * @return		true=プリミティブ型
     */
    public boolean isPrimitive() {
        return _isPrimitive;
    }

    /**
     * XML要素名を取得する
     * @return		XML要素名
     */
    public String xcodemlName() {
        return _xcodemlName;
    }

    /**
     * フォートラン型名を取得する
     * @return		フォートラン型名
     */
    public String fortranName() {
        return _fortranName;
    }

    /**
     * XML要素名からデータ型識別子を取得する
     * @param xcodemlTypeName		XML要素名
     * @return		データ型識別子
     */
    public static EnumType getTypeIdFromXcodemlTypeName(String xcodemlTypeName) {
        if (xcodemlTypeName == null) {
            throw new IllegalArgumentException();
        }

        for (EnumType type : EnumType.values()) {
            String workTypeName = type.xcodemlName();
            if (workTypeName != null) {
                if (xcodemlTypeName.compareToIgnoreCase(type.xcodemlName()) == 0) {
                    return type;
                }
            }
        }
        return DERIVED;
    }
}
