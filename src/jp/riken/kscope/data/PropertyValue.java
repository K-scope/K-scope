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

package jp.riken.kscope.data;

/**
 * プロパティ値クラス
 * @author RIKEN
 */
public class PropertyValue {

    /** プロパティキー */
    private String key;
    /** プロパティ名 */
    private String name;
    /** プロパティタイプ名 */
    private String type;
    /** プロパティ値 */
    private Object value;
    /** メッセージ */
    private String message;
    /** オプション */
    private String option;

    /**
     * コンストラクタ
     * @param key		プロパティキー
     * @param name		プロパティ名
     * @param type		プロパティタイプ名
     * @param value		プロパティ値
     * @param message		メッセージ
     */
    public PropertyValue(String key, String name, String type, Object value, String message) {
        super();
        this.key = key;
        this.name = name;
        this.type = type;
        this.value = value;
        this.message = message;
    }

    /**
     * コンストラクタ
     * @param key		プロパティキー
     * @param name		プロパティ名
     * @param type		プロパティタイプ名
     * @param value		プロパティ値
     * @param message		メッセージ
     * @param option		オプション
     */
    public PropertyValue(String key, String name, String type, Object value, String message, String option) {
        this(key, name, type, value, message);
        this.setOption(option);
    }

    /**
     * プロパティ名を取得する.
     * @return		プロパティ名
     */
    public String getName() {
        return name;
    }

    /**
     * プロパティ名を設定する.
     * @param name		プロパティ名
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * プロパティタイプ名を取得する
     * @return		プロパティタイプ名
     */
    public String getType() {
        return type;
    }

    /**
     * プロパティタイプ名を設定する.
     * @param type		プロパティタイプ名
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * プロパティ値を取得する.
     * @return		プロパティ値
     */
    public Object getValue() {
        return value;
    }

    /**
     * プロパティ値を設定する
     * @param value		プロパティ値
     */
    public void setValue(Object value) {
        this.value = value;
    }

    /**
     * プロパティキーを取得する.
     * @return		プロパティキー
     */
    public String getKey() {
        return key;
    }

    /**
     * プロパティキーを設定する.
     * @param key		プロパティキー
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * メッセージを取得する
     * @return		メッセージ
     */
    public String getMessage() {
        return message;
    }

    /**
     * メッセージを設定する
     * @param message		メッセージ
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * オプションを取得する
     * @return オプション
     */
    public String getOption() {
        return option;
    }

    /**
     * オプションを設定する
     * @param option オプション
     */
    public void setOption(String option) {
        this.option = option;
    }


}
