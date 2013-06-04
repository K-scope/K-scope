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
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.utils.ResourceUtils;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * アプリケーションプロパティクラス アプリケーションプロパティを"properties.xml"から読み込む。 アプリケーションプロパティ値の取得を行う。
 *
 * @author riken
 *
 */
public class AppProperties {

    /** アプリケーション英名 */
    public static final String APPLICATION_NAMEEN = java.util.ResourceBundle.getBundle("jp/riken/kscope/properties/AppProperties").getString("KEY1"); //K-scope
    /** アプリケーション日本語名 */
    public static final String APPLICATION_NAMEJP = java.util.ResourceBundle.getBundle("jp/riken/kscope/properties/AppProperties").getString("KEY1"); //K-scope

    /**
     * アプリケーションバージョン
     * version : 0.2.0        2013/03/22 release
     * version : 0.2.1        2013/04/12 release
     */
    public static final String APPLICATION_VERSION = "0.2.2";
    /** アプリケーションビルドID */
    //public static final String APPLICATION_BUILDID = "20120315-0900";

    /** アプリケーションプロパティファイル (日本語用)*/
    // public static final String PROPERTIES_FILE_JA = "properties_ja.xml";
    /** アプリケーションプロパティファイル(英語用)*/
    public static final String PROPERTIES_FILE_DEFAULT = "properties.xml";
    /** アプリケーションプロパティファイル */
    public static String PROPERTIES_FILE = null;

    /** アプリケーションプロパティフォルダ：システム初期設定 */
    public static final String PROPERTIES_FOLDER = "properties";
    /** プロジェクトファイル */
    public static final String PROJECT_FILE = "Kscope_project.ksx";
    /** 設定フォルダ */
    public static final String SETTINGS_FOLDER = "Kscope_settings";

    /** アプリケーションプロパティテーブル */
    private static HashMap<String, Object> m_properties = new HashMap<String, Object>();
    /** Fortran重要コメントキー */
    private static final String PROPERTY_FORTRAN_COMMENT = "fortran_valid_comment";
    /** C言語重要コメントキー */
    private static final String PROPERTY_CLANG_COMMENT = "clang_valid_comment";

    /** デフォルト拡張子設定 */
    /** Fortran:固定形式(72桁) */
    private static final String PROPERTY_EXT_FORTRAN_FIXED_72 = "ext_fortran_fixed_72";
    /** Fortran:固定形式(拡張桁数) */
    private static final String PROPERTY_EXT_FORTRAN_FIXED_EXT = "ext_fortran_fixed_ext";
    /** Fortran:自由形式 */
    private static final String PROPERTY_EXT_FORTRAN_FREE = "ext_fortran_free";
    /** C言語 */
    private static final String PROPERTY_EXT_CLANG = "ext_clang";
    /** XcdoeML */
    private static final String PROPERTY_EXT_XCODEML = "ext_xcodeml";
    /** 最終アクセスフォルダ */
    private static String m_lastAccessFolder = null;
    /** コンソールキューイング数 */
    public static int CONSOLE_QUEUESIZE = 200;
    /** 選択背景色 */
    public static Color SELECTION_BACKGROUND = new Color(100, 149, 237);

    /** 単語区切り位置 */
    public static final String DELIMITER_CHARS = " ;:{}()[]+-/%<=>!&|^~*,";
    /** 分析:検索,トレースツリーの検索文字列色 */
    public static final int SEARCHTREE_FONTSTYLE = Font.PLAIN;

    /** 構造ツリーデフォルトフィルタリスト */
    public static final FILTER_TYPE[] LANGUGE_DEFAULTFILTERS = {
                                        FILTER_TYPE.PROCEDURE,
                                        FILTER_TYPE.PROCEDUREUSAGE,
                                        FILTER_TYPE.REPETITION,
                                        FILTER_TYPE.SELECTION_SELECT,
                                        FILTER_TYPE.SELECTION_IF};

    /**
     * コンストラクタ
     */
    private AppProperties() {
    }

    /**
     * アプリケーションプロパティファイルを読み込む. <br/>
     * アプリケーションプロパティファイルから設定値を取得し、アプリケーションプロパティテーブルに設定する. <br/>
     * アプリケーションプロパティファイルはクラスディレクトリに置いてあるリソースファイルとする. <br/>
     */
    public static void loadXml() {
        try {
            // リソースファイルの読込（ロケールで設定ファイルを切り替える）
            InputStream is = null;
            is = ResourceUtils.getPropertiesFile(PROPERTIES_FILE);
            if (is == null) {
            	JOptionPane.showMessageDialog(null,
            			java.util.ResourceBundle.getBundle("jp/riken/kscope/properties/AppProperties").getString("KEY2"),
            			java.util.ResourceBundle.getBundle("jp/riken/kscope/properties/AppProperties").getString("KEY3"),
            			JOptionPane.ERROR_MESSAGE);
            }

            // XMLパース
            DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = dbfactory.newDocumentBuilder();
            Document doc = builder.parse(is);

            // 言語別拡張子のパース
            parseExtFortranFixed72(doc);
            parseExtFortranFixedExt(doc);
            parseExtFortranFree(doc);
            parseExtCLang(doc);
            parseExtXcodeml(doc);

            // Fortran重要コメントのパース
            parseFortranComment(doc);
            // C言語重要コメントのパース
            parseClangComment(doc);

        } catch (IOException e) {
        	JOptionPane.showMessageDialog(null, e, "Error", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        } catch (ParserConfigurationException e) {
        	JOptionPane.showMessageDialog(null, e, "Error", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        } catch (SAXException e) {
        	JOptionPane.showMessageDialog(null, e, "Error", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }
    }

    /**
     * Fortran:固定形式(72桁)拡張子を取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseExtFortranFixed72(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//extension/fortran_fixed_72/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;
            if (nodes == null || nodes.getLength() <= 0) return;

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_EXT_FORTRAN_FIXED_72, nodes.item(0).getNodeValue());

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }
    }

    /**
     * Fortran:固定形式(拡張桁数)拡張子を取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseExtFortranFixedExt(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//extension/fortran_fixed_ext/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;
            if (nodes == null || nodes.getLength() <= 0) return;

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_EXT_FORTRAN_FIXED_EXT, nodes.item(0).getNodeValue());

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }
    }

    /**
     * Fortran:自由形式拡張子を取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseExtFortranFree(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//extension/fortran_free/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;
            if (nodes == null || nodes.getLength() <= 0) return;

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_EXT_FORTRAN_FREE, nodes.item(0).getNodeValue());

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }
    }

    /**
     * C言語形式拡張子を取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseExtCLang(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//extension/clang/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;
            if (nodes == null || nodes.getLength() <= 0) return;

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_EXT_CLANG, nodes.item(0).getNodeValue());

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }
    }

    /**
     * XcodeML拡張子を取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseExtXcodeml(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//extension/xcodeml/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;
            if (nodes == null || nodes.getLength() <= 0)
                return;

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_EXT_XCODEML, nodes.item(0).getNodeValue());

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }
    }

    /**
     * Fortran重要コメントをアプリケーションプロパティファイルから取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseFortranComment(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//fortran/comment/valid_comment/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;

            String list[] = new String[nodes.getLength()];
            for (int i = 0; i < nodes.getLength(); i++) {
                list[i] = nodes.item(i).getNodeValue();
            }

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_FORTRAN_COMMENT, list);

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }
    }

    /**
     * C言語重要コメントをアプリケーションプロパティファイルから取得する。
     *
     * @param doc
     *            アプリケーションプロパティファイルXMLドキュメント
     */
    private static void parseClangComment(Document doc) {
        try {
            XPathFactory factory = XPathFactory.newInstance();
            XPath xpath = factory.newXPath();
            XPathExpression expr = xpath.compile("//clang/comment/valid_comment/text()");

            Object result = expr.evaluate(doc, XPathConstants.NODESET);
            NodeList nodes = (NodeList) result;

            String list[] = new String[nodes.getLength()];
            for (int i = 0; i < nodes.getLength(); i++) {
                list[i] = nodes.item(i).getNodeValue();
            }

            // アプリケーションプロパティテーブルに追加する。
            m_properties.put(PROPERTY_CLANG_COMMENT, list);

        } catch (XPathExpressionException e) {
            e.printStackTrace();
        } catch (DOMException e) {
            e.printStackTrace();
        }

    }

    /**
     * Fortran重要コメントを取得する。
     *
     * @return Fortran重要コメント
     */
    public static String[] getFortranValidComment() {
        return (String[]) m_properties.get(PROPERTY_FORTRAN_COMMENT);
    }

    /**
     * C言語重要コメントを取得する。
     *
     * @return C言語
     */
    public static String[] getClangValidComment() {
        return (String[]) m_properties.get(PROPERTY_CLANG_COMMENT);
    }

    /**
     * Fortran:固定形式(72桁)拡張子を取得する。を取得する。
     *
     * @return Fortran:固定形式(72桁)拡張子
     */
    public static String[] getExtFortranFixed72() {
        String exts = (String) m_properties.get(PROPERTY_EXT_FORTRAN_FIXED_72);
        if (exts == null || exts.length() <= 0){ return null; }
        return exts.split(",");
    }

    /**
     * Fortran:固定形式(拡張桁数)拡張子を取得する。
     *
     * @return Fortran:固定形式(拡張桁数)拡張子
     */
    public static String[] getExtFortranFixedExt() {
        String exts = (String) m_properties.get(PROPERTY_EXT_FORTRAN_FIXED_EXT);
        if (exts == null || exts.length() <= 0){ return null; }
        return exts.split(",");
    }

    /**
     * Fortran:自由形式拡張子を取得する。
     *
     * @return Fortran:自由形式拡張子
     */
    public static String[] getExtFortranFree() {
        String exts = (String) m_properties.get(PROPERTY_EXT_FORTRAN_FREE);
        if (exts == null || exts.length() <= 0){ return null; }
        return exts.split(",");
    }

    /**
     * C言語形式拡張子を取得する。
     *
     * @return C言語形式拡張子
     */
    public static String[] getExtCLang() {
        String exts = (String) m_properties.get(PROPERTY_EXT_CLANG);
        if (exts == null || exts.length() <= 0){ return null; }
        return exts.split(",");
    }

    /**
     * XcdoeML拡張子を取得する。
     *
     * @return XcdoeML拡張子
     */
    public static String[] getExtXcodeml() {
        String exts = (String) m_properties.get(PROPERTY_EXT_XCODEML);
        if (exts == null || exts.length() <= 0){ return null; }
        return exts.split(",");
    }

    /**
     * 最終アクセスフォルダを取得する。
     *
     * @return 最終アクセスフォルダ
     */
    public static String getLastAccessFolder() {
        if (m_lastAccessFolder == null) {
            m_lastAccessFolder = System.getProperty("user.dir");
        }
        return m_lastAccessFolder;
    }

    /**
     * 最終アクセスフォルダを設定する。
     *
     * @param folder
     *            最終アクセスフォルダ
     */
    public static void setLastAccessFolder(String folder) {
        m_lastAccessFolder = folder;
    }

    /**
     * MacOSであるかチェックする.<br/>
     * システムプロパティからOS名を取得する。
     * @return    true=MacOSである。
     */
    public static boolean isMac() {
        String lcOSName = System.getProperty("os.name");
        if (lcOSName == null){ return false; }
        lcOSName = lcOSName.toLowerCase();
        return lcOSName.startsWith("mac os x");
    }

    /**
     * Windowsであるかチェックする.<br/>
     * システムプロパティからOS名を取得する。
     * @return    true=Windowsである。
     */
    public static boolean isWindows() {
        String lcOSName = System.getProperty("os.name");
        if (lcOSName == null){ return false; }
        lcOSName = lcOSName.toLowerCase();
        return lcOSName.startsWith("windows");
    }

    /**
     * Linuxであるかチェックする.<br/>
     * システムプロパティからOS名を取得する。
     * @return    true=Linuxである。
     */
    public static boolean isLinux() {
        String lcOSName = System.getProperty("os.name");
        if (lcOSName == null){ return false;}
        lcOSName = lcOSName.toLowerCase();
        return lcOSName.startsWith("linux");
    }


}
