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

import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;

/**
 * アクセス先メモリタイプ
 * @author riken
 */
public enum ACCESSMEMORY_TYPE {
    /** Memory */
    MEMORY("Memory", "Mem", "memory"),
    /** L1 Cache */
    L1_CACHE("L1 Cache", "L1", "l1_cache"),
    /** L2 Cache */
    L2_CACHE("L2 Cache", "L2", "l2_cache"),
    /** Register */
    REGISTER("Register", "Reg", "register"),
    /** CUSTOM設定 */
    CUSTOM("Custom", "Custom", "custom"),
    /** Default設定 */
    DEFAULT("Default", "Default", "default");

    /** アクセス先名 */
    private String name;
    /** アクセス先短縮名 */
    private String shortname;
    /** プロパティファイルキー名 */
    private String key;

    /**
     * コンストラクタ
     * @param name		アクセス先名
     * @param shortname		アクセス先短縮名
     */
    private ACCESSMEMORY_TYPE(String name, String shortname, String key) {
        this.name = name;
        this.shortname = shortname;
        this.key = key;
    }

    /**
     * アクセス先名を取得する
     * @return name		アクセス先名
     */
    public String getName() {
        return name;
    }

    /**
     * アクセス先短縮名を取得する
     * @return name		アクセス先短縮名
     */
    public String getShortname() {
        return this.shortname;
    }

    /**
     * プロパティファイルキー名を取得する
     * @return name		プロパティファイルキー名
     */
    public String getKey() {
        return this.key;
    }


    /**
     * アクセス先メモリ名リストを取得する
     * @return			アクセス先メモリ名リスト
     */
    public static String[] getAccessMemoryList() {
        ACCESSMEMORY_TYPE types[] = ACCESSMEMORY_TYPE.values();
        String list[] = new String[types.length];
        for (int i=0; i<types.length; i++) {
        	list[i] = types[i].getName();
        }
        return list;
    }

    /**
     * 変数によるデフォルトアクセス先メモリを取得する.
     * @param def		変数
     * @return			デフォルトアクセス先メモリ
     */
    public static ACCESSMEMORY_TYPE getDefaultType() {
    	return MEMORY;
    }

    /**
     * 変数によるデフォルトアクセス先メモリを取得する.
     * @param def		変数
     * @return			デフォルトアクセス先メモリ
     */
    public static ACCESSMEMORY_TYPE getDefaultType(Variable var) {
    	if (var == null) return getDefaultType();
    	if (var.getDefinition() == null) return getDefaultType();
    	return getDefaultType(var.getDefinition());
    }

    /**
     * 変数型によるデフォルトアクセス先メモリを取得する.
     * @param def		変数定義
     * @return			デフォルトアクセス先メモリ
     */
    public static ACCESSMEMORY_TYPE getDefaultType(VariableDefinition def) {
    	if (def == null) return getDefaultType();
    	if (def.getVariableType() == null) return getDefaultType();
    	// scaler変数はレジスタとする
    	if (def.get_dimension_size() <= 0) return REGISTER;
    	if (def.getVariableType().isIntegerType()) return MEMORY;
    	if (def.getVariableType().isRealType()) return MEMORY;

    	return getDefaultType();
    }

    /**
     * 文字列からACCESSMEMORY_TYPEを取得する.
     * enum名、アクセス先名, アクセス先短縮名, プロパティファイルキー名から判断する.
     * @param value		ACCESSMEMORY_TYPE文字列
     * @return		ACCESSMEMORY_TYPE
     */
    public static ACCESSMEMORY_TYPE getAccessMemoryType(String value) {
    	if (value == null) return null;
		ACCESSMEMORY_TYPE type = null;
		try {
			type = ACCESSMEMORY_TYPE.valueOf(value.toUpperCase());
		} catch (Exception ex) {}
		if (type != null) {
			return type;
		}
        ACCESSMEMORY_TYPE types[] = ACCESSMEMORY_TYPE.values();
        for (ACCESSMEMORY_TYPE access : types) {
        	if (value.equalsIgnoreCase(access.getName())) return access;
        	if (value.equalsIgnoreCase(access.getShortname())) return access;
        	if (value.equalsIgnoreCase(access.getKey())) return access;
        }
    	return null;
    }
}


