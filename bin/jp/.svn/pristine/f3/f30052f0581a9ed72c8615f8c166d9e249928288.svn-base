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
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 外部ツールプロパティクラス
 * @author riken
 */
public class ProgramProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 外部ツール設定リスト */
    private List<Program> listProgram = new ArrayList<Program>();

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public ProgramProperties() throws Exception {
        loadProperties();
    }

    /**
     * 外部ツールプロパティをデフォルト設定ファイルから読み込む。
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
     * 外部ツールプロパティを設定ファイルから読み込む。
     *
     * @param propertiesFile 設定ファイル
     * @throws Exception プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw (new Exception(Message.getString("propertiesbase.exeption.notexist"))); //外部ツールプロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // 外部ツールプロパティを設定ファイルから読み込む。
        loadProperties(stream);
    }


    /**
     * 外部ツールプロパティをデフォルト設定ファイルから読み込む。
     * @param  stream 		設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     *
     */
    public void loadProperties(InputStream stream) throws Exception {
        // XMLファイルのパース
        listProgram = parseProgram(stream, "//program");
    }

    /**
     * プロパティ変更イベントを通知する。
     */
    @Override
    public void firePropertyChange() {
        this.changes.firePropertyChange(this.getClass().getName(), null, this);
    }


    /**
     * 外部ツール設定リストを取得する。
     * @return		外部ツール設定リスト
     */
    public List<Program> getListProgram() {
        return listProgram;
    }

    /**
     * 外部ツール設定数を取得する。
     * @return		外部ツール設定数
     */
    public int getProgramCount() {
        if (listProgram == null || listProgram.size() <= 0) return 0;
        return listProgram.size();
    }

    /**
     * 外部ツール設定を取得する。
     * @param	index		インデックス
     * @return		外部ツール設定
     */
    public Program getProgram(int index) {
        if (listProgram == null || listProgram.size() <= 0) return null;
        if (listProgram.size() <= index) return null;

        return listProgram.get(index);
    }

    /**
     * 外部ツール設定を設定する。
     * @param	index		インデックス
     * @param	exe		外部ツール設定
     */
    public void setProgram(int index, Program exe) {
        if (listProgram == null || listProgram.size() <= 0) return;
        if (listProgram.size() <= index) return;
        listProgram.set(index, exe);
    }

    /**
     * 外部ツール設定を追加する。
     * @param	exe		外部ツール設定
     */
    public void addProgram(Program exe) {
        if (listProgram == null) {
            listProgram = new ArrayList<Program>();
        }

        listProgram.add(exe);
    }

    /**
     * 外部ツール設定を削除する。
     * @param	exe		外部ツール設定
     */
    public void removeProgram(Program exe) {
        if (listProgram == null) return;

        listProgram.remove(exe);
    }

    /**
     * 外部ツール設定を削除する。
     * @param	index		インデックス
     */
    public void removeProgram(int index) {
        if (listProgram == null) return;

        listProgram.remove(index);
    }

    /**
     * 外部ツール設定を削除する。
     */
    public void clearProgram() {
        if (listProgram == null) return;

        listProgram.clear();
    }

    /**
     * 検索パターンと一致する外部ツール設定を取得する
     * @param pattern		検索パターン
     * @return		外部ツール設定
     */
    public Program getProgram(String pattern) {
        if (pattern == null || pattern.isEmpty()) return null;

        for (Program exe : listProgram) {
            String srcPattern = exe.getPattern();
            // 大文字小文字を区別しない
            if (pattern.equalsIgnoreCase(srcPattern)) {
                return exe;
            }
        }

        return null;
    }

    /**
     * 外部ツール設定を取得する
     * @param stream		XMLファイルストリーム
     * @param path		外部ツール設定XPATH
     * @return		外部ツール設定リスト
     * @throws Exception     プロパティ読込エラー
     */
    public List<Program> parseProgram(InputStream stream, String path) throws Exception {

        List<Program> list = new ArrayList<Program>();

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
                Program exe = new Program();

                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode;
                String value;
                // プログラム名
                attrnode = attrs.getNamedItem("name");
                String name = null;
                if (attrnode != null) {
                    name = attrnode.getNodeValue();
                    exe.setName(name);
                }
                // パターン
                attrnode = attrs.getNamedItem("pattern");
                String pattern = null;
                if (attrnode != null) {
                    pattern = attrnode.getNodeValue();
                    exe.setPattern(pattern);
                }
                // 拡張子
                attrnode = attrs.getNamedItem("exts");
                boolean exts = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    exts = Boolean.parseBoolean(value);
                    exe.setExts(exts);
                }
                // 正規表現
                attrnode = attrs.getNamedItem("regex");
                boolean regex = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    regex = Boolean.parseBoolean(value);
                    exe.setRegex(regex);
                }
                // 関連付け
                attrnode = attrs.getNamedItem("relation");
                boolean relation = false;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    relation = Boolean.parseBoolean(value);
                    exe.setRelation(relation);
                }
                // 外部プログラム
                attrnode = attrs.getNamedItem("program");
                String program = null;
                if (attrnode != null) {
                    program = attrnode.getNodeValue();
                    exe.setExename(program);
                }
                // オプション
                attrnode = attrs.getNamedItem("option");
                String option = null;
                if (attrnode != null) {
                    option = attrnode.getNodeValue();
                    exe.setOption(option);
                }

                list.add(exe);

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
            org.w3c.dom.Comment comment = document.createComment(Message.getString("programproperties.document.comment")); //外部プログラムプロパティ
            node.appendChild(comment);
        }

        if (this.listProgram == null || this.listProgram.size() <= 0) return;

        // キーワード
        for (Program program : this.listProgram) {
            org.w3c.dom.Element elem = document.createElement("program");

            // プログラム名
            {
                org.w3c.dom.Attr attr;
                attr = document.createAttribute("name");
                attr.setNodeValue(program.getName());
                elem.setAttributeNode(attr);
            }
            // パターン
            {
                org.w3c.dom.Attr attr = document.createAttribute("pattern");
                attr.setNodeValue(program.getPattern());
                elem.setAttributeNode(attr);
            }
            // 拡張子
            {
                org.w3c.dom.Attr attr = document.createAttribute("exts");
                attr.setNodeValue(String.valueOf(program.isExts()));
                elem.setAttributeNode(attr);
            }
            // 正規表現
            {
                org.w3c.dom.Attr attr = document.createAttribute("regex");
                attr.setNodeValue(String.valueOf(program.isRegex()));
                elem.setAttributeNode(attr);
            }
            // 関連付け
            {
                org.w3c.dom.Attr attr = document.createAttribute("relation");
                attr.setNodeValue(String.valueOf(program.isRelation()));
                elem.setAttributeNode(attr);
            }
            // 外部プログラム
            if (program.getExename() != null && !program.getExename().isEmpty()){
                org.w3c.dom.Attr attr = document.createAttribute("program");
                String exename = program.getExename();
                exename = StringUtils.escapeFilePath(exename);
                attr.setNodeValue(exename);
                elem.setAttributeNode(attr);
            }
            // オプション
            if (program.getOption() != null && !program.getOption().isEmpty()){
                org.w3c.dom.Attr attr = document.createAttribute("option");
                attr.setNodeValue(program.getOption());
                elem.setAttributeNode(attr);
            }
            // ノード追加
            node.appendChild(elem);
        }
    }

    /**
     * 検索文字と一致するプログラムを取得する
     * @param text		検索文字
     * @return			一致プログラム
     */
    public Program getMatchProgram(String text) {
        if (listProgram == null || listProgram.size() <= 0) return null;
        if (text == null || text.isEmpty()) return null;

        for (Program prog : this.listProgram) {
            if (prog.isMatchProgram(text)) {
                return prog;
            }
        }

        return null;
    }
}
