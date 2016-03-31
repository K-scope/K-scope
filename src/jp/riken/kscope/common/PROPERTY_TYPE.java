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

package jp.riken.kscope.common;

/**
 * プロパティ値タイプ
 * @author RIKEN
 */
public enum PROPERTY_TYPE {
    // プロパティ値タイプ 
	/** フォント設定 */
    FONT("font"),
    /** 色設定 */
    COLOR("color"),
    /** 整数値設定 */
    INTEGER("integer"),
    /** 不明 */
    UNKNOWN("unknown");

    /** タイプ名 */
    private String typename;

    /**
     * コンストラクタ
     * @param tabname		タイプ名
     */
    private PROPERTY_TYPE(String type) {
        this.typename = type;
    }

    /**
     * タイプ名を取得する
     * @return		タイプ名
     */
    public String getTypename() {
        return this.typename;
    }

    /**
     * タイプ名からプロパティ値タイプを取得する
     * @param type			タイプ名
     * @return			プロパティ値タイプ
     */
    public static PROPERTY_TYPE parseType(String type) {
        if (type == null) return PROPERTY_TYPE.UNKNOWN;

        PROPERTY_TYPE types[] = PROPERTY_TYPE.values();
        for (int i=0; i<types.length; i++) {
            if (types[i].getTypename().equalsIgnoreCase(type)) {
                return types[i];
            }
        }
        return PROPERTY_TYPE.UNKNOWN;
    }
}



