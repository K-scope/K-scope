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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import jp.riken.kscope.Message;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * アプリケーションプロパティクラス
 * @author RIKEN
 */
public class ApplicationProperties extends PropertiesBase {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    // プロパティキー
    /** プロジェクトの新規作成後、プロジェクトの自動保存プロパティ */
    private final String NEWPROJECT_SAVE = "newproject_save";
    /** ソースファイルのエクスポートの除外ファイル有無プロパティ */
    private final String EXPORTSOURCE_EXCLUDE = "exportsource_exclude";

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public ApplicationProperties() throws Exception {
    	loadProperties();
    }

    /**
     * プロパティ変更イベントを通知する。
     */
	@Override
	public void firePropertyChange() {
		this.changes.firePropertyChange(this.getClass().getName(), null, this);
	}

    /**
     * ソース設定プロパティをデフォルト設定ファイルから読み込む。
     *
     * @throws Exception プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
        InputStream stream = null;

        // リソースファイルの読込
        stream = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);

        // ソース設定プロパティを設定ファイルから読み込む。
        loadProperties(stream);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     *
     * @param propertiesFile ソース設定プロパティ設定ファイル
     * @throws Exception プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw (new Exception(Message.getString("propertiesbase.exeption.notexist"))); //プロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // ソース設定プロパティを設定ファイルから読み込む。
        loadProperties(stream);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     *
     * @param stream ソース設定プロパティ設定ファイルストリーム
     * @throws Exception プロパティ読込エラー
     */
    public void loadProperties(InputStream stream) throws Exception {

        // XMLファイルのパース
        String key = null;
        XmlUtils xml = new XmlUtils(stream);

        // プロジェクト作成直後に保存
        {
	        key = NEWPROJECT_SAVE;
	        boolean b = false;
	        String val = xml.getString("//settings/application[@key='" + key + "']/@value");
	        if (!StringUtils.isNullOrEmpty(val)) {
	        	if (val.equalsIgnoreCase("true")) {
	        		b = true;
	        	}
	        }
	        this.putBoolean(key, b);
        }
        // ソースファイルエクスポート　除外ファイルパターン
        {
        	key = EXPORTSOURCE_EXCLUDE;
        	String val = xml.getString("//settings/application[@key='" + key + "']/@value");
        	if (StringUtils.isNullOrEmpty(val)) {
        		val = "";
        	}
        	this.put(key, val);
        }
    }

    /**
     * プロパティをDOMノードに出力する
     * @param node		出力ノード
     */
    public void writeProperties(org.w3c.dom.Element node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("applicationproperties.document.comment")); //アプリケーションプロパティ
            node.appendChild(comment);
        }
        // プロジェクト新規作成直後にプロジェクトを保存するか
        {
            String key = NEWPROJECT_SAVE;
            boolean value = this.getBoolean(key, false);
            org.w3c.dom.Element elem = createPropertyElement(document, key);
            org.w3c.dom.Attr attrValue = document.createAttribute("value");
            attrValue.setValue(String.valueOf(value));
            elem.setAttributeNode(attrValue);
            node.appendChild(elem);
        }
    }

    /**
     * プロパティ要素を作成する
     * @param document		XMLドキュメント
     * @param key			key属性値
     * @return				プロパティ要素
     */
    private org.w3c.dom.Element createPropertyElement(org.w3c.dom.Document document, String key) {

        org.w3c.dom.Element elem = document.createElement("application");
        // プロパティキー
        {
            org.w3c.dom.Attr attr = document.createAttribute("key");
            attr.setValue(key);
            elem.setAttributeNode(attr);
        }

        return elem;
    }

    /**
     * 新規作成後にプロジェクトを保存するか否かを取得
     * @return		true=保存する
     */
    public boolean getSaveProjectAfterCreate() {
    	return this.getBoolean(NEWPROJECT_SAVE, false);
    }

    /**
     * ソースファイルエクスポートの除外ファイルパターン文字列を取得
     * @return		exclude
     */
    public String getSourceExportExclude() {
    	return this.get(EXPORTSOURCE_EXCLUDE, "");
    }
}
