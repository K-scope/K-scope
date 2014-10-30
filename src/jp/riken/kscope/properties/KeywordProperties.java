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

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * ソースコードのキーワード(ハイライト)設定クラス
 * @author RIKEN
 *
 */
public class KeywordProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キーワード(ハイライト)設定リスト */
    private List<Keyword> listKeyword = new ArrayList<Keyword>();

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public KeywordProperties() throws Exception {
        loadProperties();
    }

    /**
     * ソース設定プロパティをデフォルト設定ファイルから読み込む。
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
        InputStream is = null;

        // リソースファイルの読込
        is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);

        loadProperties(is);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     * @param  propertiesFile 		ソース設定プロパティ設定ファイル
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw(new Exception(Message.getString("propertiesbase.exeption.notexist"))); //ソース設定プロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // XMLファイルのパース
        loadProperties(stream);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(InputStream stream ) throws Exception {
        // XMLファイルのパース
        listKeyword = parseKeyword(stream, "//keyword");
    }

    /**
     * プロパティ変更イベントを通知する。
     */
    @Override
    public void firePropertyChange() {
        this.changes.firePropertyChange(this.getClass().getName(), null, this);
    }

    /**
     * キーワード(ハイライト)設定リストを取得する。
     * @return		ハイライト設定リスト
     */
    public List<Keyword> getListKeyword() {
        return listKeyword;
    }

    /**
     * キーワード(ハイライト)数を取得する。
     * @return		キーワード(ハイライト)数
     */
    public int getKeywordCount() {
        if (listKeyword == null || listKeyword.size() <= 0) {return 0;}
        return listKeyword.size();
    }

    /**
     * キーワード(ハイライト)を取得する。
     * @param	index		インデックス
     * @return		キーワード(ハイライト)
     */
    public Keyword getKeyword(int index) {
        if (listKeyword == null || listKeyword.size() <= 0) {return null;}
        if (listKeyword.size() <= index) {return null;}
        return listKeyword.get(index);
    }

    /**
     * キーワード(ハイライト)を設定する。
     * @param	index		インデックス
     * @param	keyword		キーワード(ハイライト)
     */
    public void setKeyword(int index, Keyword keyword) {
        if (listKeyword == null || listKeyword.size() <= 0) {return;}
        if (listKeyword.size() <= index) {return;}
        listKeyword.set(index, keyword);
    }

    /**
     * キーワード(ハイライト)を追加する。
     * @param	keyword		キーワード(ハイライト)
     */
    public void addKeyword(Keyword keyword) {
        if (listKeyword == null) {
            listKeyword = new ArrayList<Keyword>();
        }
        listKeyword.add(keyword);
    }

    /**
     * キーワード(ハイライト)を削除する。
     * @param	keyword		キーワード(ハイライト)
     */
    public void removeKeyword(Keyword keyword) {
        if (listKeyword == null) return;
        listKeyword.remove(keyword);
    }

    /**
     * キーワード(ハイライト)を削除する。
     * @param	index		インデックス
     */
    public void removeKeyword(int index) {
        if (listKeyword == null) return;
        listKeyword.remove(index);
    }

    /**
     * キーワード(ハイライト)リストをクリアする。
     */
    public void clearKeyword() {
        listKeyword = new ArrayList<Keyword>();
    }

    /**
     * 検索キーワードと一致するKeyword情報を取得する
     * @param word		検索キーワード
     * @return		Keyword情報
     */
    public Keyword getKeyword(String word) {
        if (word == null || word.isEmpty()) return null;

        for (Keyword keyword : listKeyword) {
            String srcWord = keyword.getKeyword();
            boolean sensitivecase = keyword.isSensitivecase();
            if (sensitivecase) {
                // 大文字小文字を区別する
                if (word.equals(srcWord)) {
                    return keyword;
                }
            }
            else {
                // 大文字小文字を区別しない
                if (word.equalsIgnoreCase(srcWord)) {
                    return keyword;
                }
            }
        }

        return null;
    }

    /**
     * キーワードを取得する
     * @param stream		XML入力ストリーム
     * @param path		キーワードXPATH
     * @return		キーワードリスト
     * @throws Exception 		キーワードパースエラー
     */
    public List<Keyword> parseKeyword(InputStream stream, String path) throws Exception {

        List<Keyword> list = new ArrayList<Keyword>();

        // XML
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = dbfactory.newDocumentBuilder();
        org.w3c.dom.Document document = builder.parse(stream);

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();

        XPathExpression expr = xpath.compile(path);

        Object result = expr.evaluate(document, XPathConstants.NODESET);

        NodeList nodelist = (NodeList) result;
        if (nodelist == null) return null;

        for (int i=0; i<nodelist.getLength(); i++) {
            try {
                Node node = nodelist.item(i);
                Keyword keyword = new Keyword(KEYWORD_TYPE.KEYWORD);

                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode;
                String value;
                // キーワード名
                attrnode = attrs.getNamedItem("name");
                String name = null;
                if (attrnode != null) {
                    name = attrnode.getNodeValue();
                    keyword.setName(name);
                }
                // キーワード
                attrnode = attrs.getNamedItem("keyword");
                String word = null;
                if (attrnode != null) {
                    word = attrnode.getNodeValue();
                    keyword.setKeyword(word);
                }

                // クラスモード
                attrnode = attrs.getNamedItem("class");
                String class_mode = null;
                if (attrnode != null) {
                    class_mode = attrnode.getNodeValue();
                    keyword.setClassmode(class_mode);
                }
                // 太字
                attrnode = attrs.getNamedItem("bold");
                boolean bold = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    bold = Boolean.parseBoolean(value);
                }
                // イタリック
                attrnode = attrs.getNamedItem("italic");
                boolean italic = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    italic = Boolean.parseBoolean(value);
                }
                int style = Font.PLAIN;
                if (bold) style += Font.BOLD;
                if (italic) style += Font.ITALIC;
                keyword.setStyle(style);

                // 文字色
                attrnode = attrs.getNamedItem("forecolor");
                Color forecolor = null;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    forecolor = StringUtils.parseColor(value);
                    keyword.setForecolor(forecolor);
                }

                // 有効・無効
                attrnode = attrs.getNamedItem("enabled");
                boolean enabled = true;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    enabled = Boolean.parseBoolean(value);
                    keyword.setEnabled(enabled);
                }
                // 大文字・小文字の区別
                attrnode = attrs.getNamedItem("sensitivecase");
                boolean sensitivecase = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    sensitivecase = Boolean.parseBoolean(value);
                    keyword.setSensitivecase(sensitivecase);
                }
                // 正規表現
                attrnode = attrs.getNamedItem("regex");
                boolean regex = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    regex = Boolean.parseBoolean(value);
                    keyword.setRegex(regex);
                }
                // キーワード変更付加
                attrnode = attrs.getNamedItem("keywordlock");
                boolean keywordlock = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    keywordlock = Boolean.parseBoolean(value);
                    keyword.setKeywordlock(keywordlock);
                }

                // キーワードは単語検索とする
                keyword.setSearchWord(true);

                list.add(keyword);

            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }

        if (list.size() <= 0) {
            list = null;
        }

        return list;
    }


    /**
     * プロパティをDOMノードに出力する
     * @param node		出力ノード
     */
    public void writeProperties(org.w3c.dom.Node node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("keywordproperties.document.comment")); //キーワードプロパティ
            node.appendChild(comment);
        }

        if (this.listKeyword == null || this.listKeyword.size() <= 0) return;

        // キーワード
        for (Keyword keyword : this.listKeyword) {
            org.w3c.dom.Element elem = document.createElement("keyword");

            // キーワード名
            {
                org.w3c.dom.Attr attr;
                attr = document.createAttribute("name");
                attr.setValue(keyword.getName());
                elem.setAttributeNode(attr);
            }
            // キーワード
            {
                org.w3c.dom.Attr attr = document.createAttribute("keyword");
                attr.setNodeValue(keyword.getKeyword());
                elem.setAttributeNode(attr);
            }
            // クラス
            if (keyword.getClassmode() != null && !keyword.getClassmode().isEmpty()){
                org.w3c.dom.Attr attr = document.createAttribute("class");
                attr.setNodeValue(keyword.getClassmode());
                elem.setAttributeNode(attr);
            }
            // スタイル
            int style = keyword.getStyle();
            boolean bold = false;
            boolean italic = false;
            if ((style & Font.BOLD) != 0) bold = true;
            if ((style & Font.ITALIC) != 0) italic = true;
            // bold
            {
                org.w3c.dom.Attr attr = document.createAttribute("bold");
                attr.setNodeValue(String.valueOf(bold));
                elem.setAttributeNode(attr);
            }
            // italic
            {
                org.w3c.dom.Attr attr = document.createAttribute("italic");
                attr.setNodeValue(String.valueOf(italic));
                elem.setAttributeNode(attr);
            }
            // 文字色
            {
                Color color = keyword.getForecolor();
                org.w3c.dom.Attr attr = document.createAttribute("forecolor");
                attr.setNodeValue(StringUtils.parseColorCode(color));
                elem.setAttributeNode(attr);
            }
            // 文字色
            {
                Color color = keyword.getForecolor();
                org.w3c.dom.Attr attr = document.createAttribute("forecolor");
                attr.setNodeValue(StringUtils.parseColorCode(color));
                elem.setAttributeNode(attr);
            }
            // 有効・無効
            {
                org.w3c.dom.Attr attr = document.createAttribute("enabled");
                attr.setNodeValue(String.valueOf(keyword.isEnabled()));
                elem.setAttributeNode(attr);
            }
            // 大文字・小文字の区別
            {
                org.w3c.dom.Attr attr = document.createAttribute("sensitivecase");
                attr.setNodeValue(String.valueOf(keyword.isSensitivecase()));
                elem.setAttributeNode(attr);
            }
            // 正規表現
            {
                org.w3c.dom.Attr attr = document.createAttribute("regex");
                attr.setNodeValue(String.valueOf(keyword.isRegex()));
                elem.setAttributeNode(attr);
            }
            // ノード追加
            node.appendChild(elem);
        }
    }
}


