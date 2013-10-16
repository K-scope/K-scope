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

import java.awt.Color;
import java.awt.Font;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.PropertyValue;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * ソースコードの表示設定クラス
 *
 * @author riken
 */
public class SourceProperties extends PropertiesBase {

    /**
     * シリアル番号
     */
    private static final long serialVersionUID = 1L;

    /* プロパティキー */
    /**
     * ソースフォント
     */
    private final String KEY_FONT_SOURCE = "font-source";
    /**
     * ソースフォント色
     */
    private final String KEY_FONTCOLOR_SOURCE = "fontcolor-source";
    /**
     * ソース背景色
     */
    private final String KEY_BACKGROUND_SOURCE = "background-source";
    /**
     * アクティブ行背景色
     */
    private final String KEY_BACKGROUND_SELECTEDROW = "background-selectedrow";
    /**
     * 折り返し位置
     */
    private final String KEY_WORDWRAP = "wordwrap";
    /**
     * 検索文字色
     */
    private final String KEY_FONTCOLOR_SEARCH = "fontcolor-search";
    /**
     * 検索文字背景色
     */
    private final String KEY_BACKGROUND_SEARCH = "background-search";
    /**
     * 強調範囲背景色
     */
    private final String KEY_BACKGROUND_AREA = "background-area";
    /**
     * 選択範囲背景色
     */
    private final String KEY_BACKGROUND_BLOCK = "background-block";

    /*行番号背景色*/
    private final String KEY_BACKGROUND_LINENUMBER = "background-linenumber"; //(2012/4/10) added by teraim
    /** 選択ノード背景色 */
    private final String KEY_BACKGROUND_SELECTNODE = "background-selectnode";
    private final String KEY_BACKGROUND_VIEW2 = "background-view2";
    /** 付加情報ノードフォント色 */
    private final String KEY_FONTCOLOR_INFORMATIONNODE = "fontcolor-informationnode";
    /** リンク切れノード文字色 */
    private final String KEY_FONTCOLOR_BROKENLINKNODE = "fontcolor-brokenlinknode";

    /**
     * ソースコードの表示設定リスト
     */
    private List<PropertyValue> listProperty = new ArrayList<PropertyValue>();

    /**
     * コンストラクタ
     *
     * @throws Exception プロパティ読込エラー
     */
    public SourceProperties() throws Exception {
        loadProperties();
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
            throw (new Exception(Message.getString("propertiesbase.exeption.notexist"))); //ソース設定プロパティファイルが存在しません。
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
        String name = null;
        String type = null;
        String message = null;
        XmlUtils xml = new XmlUtils(stream);

        // フォント
        key = KEY_FONT_SOURCE;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Font font = xml.getFont("//settings/property[@key='" + key + "']");
        this.setPropertyValue(key, name, type, font, message);

        // フォント色
        key = KEY_FONTCOLOR_SOURCE;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color forecolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, forecolor, message);

        // 背景色
        key = KEY_BACKGROUND_SOURCE;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color backcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, backcolor, message);

        // 選択行背景色
        key = KEY_BACKGROUND_SELECTEDROW;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color activerow = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, activerow, message);

        // 強調範囲背景色
        key = KEY_BACKGROUND_AREA;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color areacolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, areacolor, message);

        // 選択範囲背景色
        key = KEY_BACKGROUND_BLOCK;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color blockcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, blockcolor, message);

        // 行番号背景色(2012/4/12)
        key = KEY_BACKGROUND_LINENUMBER;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color linecolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, linecolor, message);

        // 選択行背景色１(2012/4/12)
        key = KEY_BACKGROUND_SELECTNODE;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color viewcolor1 = xml.getColor("//settings/property[@key='" + key + "']/@color");
    	if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
    		this.setPropertyValue(key, name, type, viewcolor1, message);
    	}

        // 選択行背景色２(2012/4/12)
        key = KEY_BACKGROUND_VIEW2;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color viewcolor2 = xml.getColor("//settings/property[@key='" + key + "']/@color");
    	if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
    		this.setPropertyValue(key, name, type, viewcolor2, message);
    	}

        // 検索文字色
        key = KEY_FONTCOLOR_SEARCH;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color searchforecolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, searchforecolor, message);

        // 検索文字背景色
        key = KEY_BACKGROUND_SEARCH;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color searchbackcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
        this.setPropertyValue(key, name, type, searchbackcolor, message);

        // 折り返し位置
        key = KEY_WORDWRAP;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Integer wordwrap = xml.getInt("//settings/property[@key='" + key + "']/@value");
        if (wordwrap < 0) wordwrap = 0;
        this.setPropertyValue(key, name, type, wordwrap, message);

        // 付加情報フォント色
        key = KEY_FONTCOLOR_INFORMATIONNODE;
        name = xml.getString("//settings/property[@key='" + key + "']/@name");
        type = xml.getString("//settings/property[@key='" + key + "']/@type");
        message = xml.getString("//settings/property[@key='" + key + "']/@message");
        Color informationcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    	if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
            this.setPropertyValue(key, name, type, informationcolor, message);
    	}

    	// リンク切れ文字色
    	key = KEY_FONTCOLOR_BROKENLINKNODE;
    	name = xml.getString("//settings/property[@key='" + key + "']/@name");
    	type = xml.getString("//settings/property[@key='" + key + "']/@type");
    	message = xml.getString("//setting/property[@key='" + key + "']/@type");
    	Color brokenlinkcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    	if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
    		this.setPropertyValue(key, name, type, brokenlinkcolor, message);
    	}
    }

    /**
     * プロパティ値を設定する.
     *
     * @param key	キー
     * @param name	プロパティ名
     * @param type	プロパティタイプ
     * @param value	プロパティ値
     * @param message	メッセージ
     */
    private void setPropertyValue(String key, String name, String type, Object value, String message) {
        PropertyValue property = new PropertyValue(key, name, type, value, message);
        setPropertyValue(property);
    }

    /**
     * プロパティ値を設定する.
     *
     * @param value	プロパティ値
     */
    public void setPropertyValue(PropertyValue value) {
        if (value == null) {
            return;
        }

        for (int i = 0; i < this.listProperty.size(); i++) {
            PropertyValue property = this.listProperty.get(i);
            if (value.getKey().equalsIgnoreCase(property.getKey())) {
                this.listProperty.set(i, value);
                return;
            }
        }

        // 新規追加
        this.listProperty.add(value);
    }

    /**
     * プロパティ値を取得する.
     * @param key	キー
     * @return    プロパティ値
     */
    private PropertyValue getPropertyValue(String key) {
        if (key == null) {
            return null;
        }

        for (PropertyValue property : this.listProperty) {
            if (key.equalsIgnoreCase(property.getKey())) {
                return property;
            }
        }
        return null;
    }

    /**
     * ソースコード表示フォントを取得する。
     *
     * @return	ソース表示フォント
     */
    public Font getFont() {
        PropertyValue value = getPropertyValue(KEY_FONT_SOURCE);
        if (value == null) {
            return null;
        }

        return (Font) value.getValue();
    }

    /**
     * ソースコード表示フォント色を取得する
     *
     * @return	ソースコード表示フォント色
     */
    public Color getFontColor() {
        PropertyValue value = getPropertyValue(KEY_FONTCOLOR_SOURCE);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * ソースコード背景色を取得する
     *
     * @return	ソースコード背景色
     */
    public Color getBackgroundColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_SOURCE);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * アクティブ行背景色を取得する
     *
     * @return	アクティブ行背景色
     */
    public Color getActiverowColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_SELECTEDROW);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * アクティブ行背景色(選択ツリーノード)を取得する
     *
     * @return	アクティブ行背景色
     */
    public Color getBackgoundSelectNodeColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_SELECTNODE);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * アクティブ行背景色２を取得する
     *
     * @return	アクティブ行背景色
     */
    public Color getBackgoundView2Color() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_VIEW2);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * 検索文字色を取得する
     *
     * @return	検索文字色
     */
    public Color getSearchFontColor() {
        PropertyValue value = getPropertyValue(KEY_FONTCOLOR_SEARCH);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * 検索背景色を取得する
     *
     * @return	検索背景色
     */
    public Color getSearchBackgroundColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_SEARCH);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * 折り返し位置を取得する
     *
     * @return	折り返し位置
     */
    public int getWordwrap() {
        PropertyValue value = getPropertyValue(KEY_WORDWRAP);
        if (value == null) {
            return 0;
        }

        return (Integer) value.getValue();
    }

    /**
     * 強調範囲背景色を取得する
     *
     * @return	強調範囲背景色
     */
    public Color getAreaColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_AREA);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * 選択範囲背景色を取得する
     *
     * @return	選択範囲背景色
     */
    public Color getBlockColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_BLOCK);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * 行番号背景色を取得する (2012/4/10) added by teriam
     *
     * @return	選択範囲背景色
     */
    public Color getLineNumberColor() {
        PropertyValue value = getPropertyValue(KEY_BACKGROUND_LINENUMBER);
        if (value == null) {
            return null;
        }

        return (Color) value.getValue();
    }

    /**
     * 付加情報フォント色を取得する
     * @return	付加情報フォント色
     */
    public Color getInformationNodeFontColor() {
        PropertyValue value = getPropertyValue(KEY_FONTCOLOR_INFORMATIONNODE);
        if (value == null) {
            return null;
        }
        return (Color) value.getValue();
    }

    /**
     * リンク切れ文字色を取得する
     * @return リンク切れ文字色
     */
    public Color getBrokenLinkNodeFontColor() {
    	PropertyValue value = getPropertyValue(KEY_FONTCOLOR_BROKENLINKNODE);
    	if (value == null) {
    		return null;
    	}
    	return (Color) value.getValue();
    }


    /**
     * プロパティ変更イベントを通知する。
     */
    @Override
    public void firePropertyChange() {
        this.changes.firePropertyChange(this.getClass().getName(), null, this);
    }

    /**
     * プロパティをDOMノードに出力する
     *
     * @param node	出力ノード
     */
    public void writeProperties(org.w3c.dom.Element node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("sourceproperties.document.comment")); //表示プロパティ
            node.appendChild(comment);
        }
        // ソースフォント
        {
            String key = KEY_FONT_SOURCE;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createFontAttribute(elem, (Font) value.getValue());
                node.appendChild(elem);
            }
        }
        // ソースフォント色
        {
            String key = KEY_FONTCOLOR_SOURCE;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // ソース背景色
        {
            String key = KEY_BACKGROUND_SOURCE;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // アクティブ行背景色１を取得する
        {
            String key = KEY_BACKGROUND_SELECTNODE;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // アクティブ行背景色２を取得する
        {
            String key = KEY_BACKGROUND_VIEW2;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // アクティブ行背景色
        {
            String key = KEY_BACKGROUND_SELECTEDROW;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // 折り返し位置
        {
            String key = KEY_WORDWRAP;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                org.w3c.dom.Attr attrValue = document.createAttribute("value");
                attrValue.setValue(String.valueOf((Integer) value.getValue()));
                elem.setAttributeNode(attrValue);
                node.appendChild(elem);
            }
        }
        // 検索文字色
        {
            String key = KEY_FONTCOLOR_SEARCH;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // 検索文字背景色
        {
            String key = KEY_BACKGROUND_SEARCH;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // 強調範囲背景色
        {
            String key = KEY_BACKGROUND_AREA;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // 選択範囲背景色
        {
            String key = KEY_BACKGROUND_BLOCK;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        //行番号背景色を取得する (2012/4/10) added by teriam
        {
            String key = KEY_BACKGROUND_LINENUMBER;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // 付加情報フォント色
        {
            String key = KEY_FONTCOLOR_INFORMATIONNODE;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
        // リンク切れ文字色
        {
        	String key = KEY_FONTCOLOR_BROKENLINKNODE;
            PropertyValue value = this.getPropertyValue(key);
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, value);
                XmlUtils.createColorAttribute(elem, (Color) value.getValue());
                node.appendChild(elem);
            }
        }
    }

    /**
     * プロパティ要素を作成する
     *
     * @param document	XMLドキュメント
     * @param value	プロパティ値
     * @return	プロパティ要素
     */
    private org.w3c.dom.Element createPropertyElement(org.w3c.dom.Document document, PropertyValue value) {

        org.w3c.dom.Element elem = document.createElement("property");
        // プロパティキー
        {
            org.w3c.dom.Attr attr = document.createAttribute("key");
            attr.setValue(value.getKey());
            elem.setAttributeNode(attr);
        }
        // プロパティ名
        {
            org.w3c.dom.Attr attr = document.createAttribute("name");
            attr.setValue(value.getName());
            elem.setAttributeNode(attr);
        }
        // プロパティタイプ
        {
            org.w3c.dom.Attr attr = document.createAttribute("type");
            attr.setValue(value.getType());
            elem.setAttributeNode(attr);
        }
        // メッセージ
        {
            org.w3c.dom.Attr attr = document.createAttribute("message");
            attr.setValue(value.getMessage());
            elem.setAttributeNode(attr);
        }

        return elem;
    }

    /**
     * ソースプロパティリストを取得する.
     *
     * @return	ソースプロパティリスト
     */
    public PropertyValue[] getPropertyValues() {
        return this.listProperty.toArray(new PropertyValue[0]);
    }
}
