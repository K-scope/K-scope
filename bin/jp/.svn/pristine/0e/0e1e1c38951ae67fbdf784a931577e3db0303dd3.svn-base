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
package jp.riken.kscope.properties;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Properties;

/**
 * プロパティ基底クラス
 * @author riken
 *
 */
public abstract class PropertiesBase extends Properties {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** プロパティの変更通知をサポートするオブジェクト */
    protected PropertyChangeSupport changes;

    /**
     * コンストラクタ
     */
    public PropertiesBase() {
        super();
        this.changes = new PropertyChangeSupport(this);
    }

    /**
     * プロパティ変更イベント発生
     */
    public abstract void firePropertyChange();

    /**
     * リスナの追加を行います.<br/>
     * @param listener 追加するリスナ
     */
    public void addPropertyChangeListener(PropertyChangeListener listener){
        this.changes.addPropertyChangeListener(listener);
    }

    /**
     * リスナの削除を行います.<br/>
     * @param listener 削除するリスナ
     */
    public void removePropertyChangeListener(PropertyChangeListener listener){
        this.changes.removePropertyChangeListener(listener);
    }


    /**
     * String型データの取得
     * @param key - ハッシュテーブルキー
     * @param defaultValue - デフォルト値
     * @return -指定されたキー値を持つこのプロパティリストの値
     */
    public String get(final String key, final String defaultValue) {
        return getProperty(key, defaultValue);
    }

    /**
     * boolean型データの取得
     * @param key - ハッシュテーブルキー
     * @param defaultValue - デフォルト値
     * @return -指定されたキー値を持つこのプロパティリストの値
     */
    public boolean getBoolean(final String key, final boolean defaultValue) {
        return new Boolean(getProperty(key, Boolean.toString(defaultValue)));
    }

    /**
     * int型データの取得
     * @param key - ハッシュテーブルキー
     * @param defaultValue - デフォルト値
     * @return -指定されたキー値を持つこのプロパティリストの値
     */
    public int getInt(final String key, final int defaultValue) {
        String value = getProperty(key, Integer.toString(defaultValue));
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return 0;
        }
    }


    /**
     * String型データの設定
     * @param key -プロパティリストに配置されるキー
     * @param value key -に対応する値
     */
    public void put(final String key, final String value) {
        setProperty(key, value);
    }

    /**
     * boolean型データの設定
     * @param key -プロパティリストに配置されるキー
     * @param value key -に対応する値
     */
    public void putBoolean(final String key, final boolean value) {
        setProperty(key, Boolean.toString(value));
    }

    /**
     * int型データの設定
     * @param key -プロパティリストに配置されるキー
     * @param value key -に対応する値
     */
    public void putInt(final String key, final int value) {
        setProperty(key, Integer.toString(value));
    }
    
    /**
     * Object型データの取得
     * @param key -プロパティリストに配置されるキー
     * @return -指定されたキー値を持つこのプロパティリストの値
     */
    public Object getObject(final String key) {
        return this.get(key);
    }

    /**
     * Object型データの設定
     * @param key -プロパティリストに配置されるキー
     * @param value key -に対応する値
     */
    public void putObject(final String key, final Object value) {
        this.put(key, value);
    }
}
