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
import java.util.Enumeration;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 演算カウントプロパティクラス
 * @author RIKEN
 */
public class OperationProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 演算子FLOP:+ */
    private int flopAdd;
    /** 演算子FLOP:* */
    private int flopMul;
    /** 演算子FLOP:- */
    private int flopSub;
    /** 演算子FLOP:/ */
    private int flopDiv;
    /** 演算子FLOP:** */
    private int flopPow;

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public OperationProperties() throws Exception {
        loadProperties();
    }

    /**
     * 演算カウントプロパティをデフォルト設定ファイルから読み込む。
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
        InputStream stream = null;
        // リソースファイルの読込
        stream = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
        // 演算カウントプロパティを設定ファイルから読み込む。
        loadProperties(stream);
    }

    /**
     * 演算カウントプロパティを設定ファイルから読み込む。
     * @param  propertiesFile 		演算カウントプロパティ設定ファイル
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw(new Exception(Message.getString("propertiesbase.exeption.notexist"))); //演算カウントプロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // XMLファイルのパース
        loadProperties(stream);
    }


    /**
     * 演算カウントプロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(InputStream stream) throws Exception {

        // XML
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = dbfactory.newDocumentBuilder();
        org.w3c.dom.Document document = builder.parse(stream);

        // XMLファイルのパース
        List<OperationCount> list = parseOperation(document, "//operation");

        // PropertiesクラスのHashTableに追加する
        // キーは組込み関数名(=name)とする
        for (OperationCount opc : list) {
            String name = opc.getName();
            if (name != null && !name.isEmpty()) {
                addOperationProperty(name, opc);
            }
        }

        // 四則演算FLOP設定
        parseOperatorFlop(document, "//operator_flop");
    }


    /**
     * 演算カウント設定を取得する
     * @param document		XMLドキュメント
     * @param path		演算カウント設定XPATH
     * @return		演算カウント設定リスト
     * @throws Exception     プロパティ読込エラー
     */
    public List<OperationCount> parseOperation(org.w3c.dom.Document document, String path) throws Exception {
        List<OperationCount> list = new ArrayList<OperationCount>();

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();

        XPathExpression expr = xpath.compile(path);

        Object result = expr.evaluate(document, XPathConstants.NODESET);

        NodeList nodelist = (NodeList) result;
        if (nodelist == null) return null;

        for (int i=0; i<nodelist.getLength(); i++) {
            try {
                Node node = nodelist.item(i);
                OperationCount opc = new OperationCount();

                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode;
                String value = null;
                // 組込み関数名
                String name = null;
                attrnode = attrs.getNamedItem("name");
                if (attrnode != null) {
                    name = attrnode.getNodeValue();
                    if (name != null && !name.isEmpty()) {
                        opc.setName(name);
                    }
                }
                if (name == null || name.isEmpty()) {
                    continue;
                }

                // 演算子:+カウント
                attrnode = attrs.getNamedItem("add");
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        opc.setAdd(Integer.parseInt(value));
                    }
                }
                // 演算子:-カウント
                attrnode = attrs.getNamedItem("sub");
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        opc.setSub(Integer.parseInt(value));
                    }
                }
                // 演算子:*カウント
                attrnode = attrs.getNamedItem("mul");
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        opc.setMul(Integer.parseInt(value));
                    }
                }
                // 演算子:/カウント
                attrnode = attrs.getNamedItem("div");
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        opc.setDiv(Integer.parseInt(value));

                    }
                }

                list.add(opc);

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
     * 四則演算FLOP設定を取得する
     * @param document		XMLドキュメント
     * @param path		四則演算FLOP設定XPATH
     * @return		演算カウント設定リスト
     * @throws Exception     プロパティ読込エラー
     */
    public void parseOperatorFlop(org.w3c.dom.Document document, String path) throws Exception {

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();

        XPathExpression expr = xpath.compile(path);

        Object result = expr.evaluate(document, XPathConstants.NODESET);

        NodeList nodelist = (NodeList) result;
        if (nodelist == null || nodelist.getLength() <= 0) return;

        try {
            Node node = nodelist.item(0);

            // 属性の取得
            NamedNodeMap attrs = node.getAttributes();
            Node attrnode;
            String value = null;
            // 演算子:+:FLOP
            attrnode = attrs.getNamedItem("add");
            if (attrnode != null) {
                value = attrnode.getNodeValue();
                if (StringUtils.isNumeric(value)) {
                    this.flopAdd = Integer.parseInt(value);
                }
            }
            // 演算子:-FLOP
            attrnode = attrs.getNamedItem("sub");
            if (attrnode != null) {
                value = attrnode.getNodeValue();
                if (StringUtils.isNumeric(value)) {
                    this.flopSub = Integer.parseInt(value);
                }
            }
            // 演算子:*FLOP
            attrnode = attrs.getNamedItem("mul");
            if (attrnode != null) {
                value = attrnode.getNodeValue();
                if (StringUtils.isNumeric(value)) {
                    this.flopMul = Integer.parseInt(value);
                }
            }
            // 演算子:/FLOP
            attrnode = attrs.getNamedItem("div");
            if (attrnode != null) {
                value = attrnode.getNodeValue();
                if (StringUtils.isNumeric(value)) {
                    this.flopDiv = Integer.parseInt(value);
                }
            }
            // 演算子:**FLOP
            attrnode = attrs.getNamedItem("pow");
            if (attrnode != null) {
                value = attrnode.getNodeValue();
                if (StringUtils.isNumeric(value)) {
                    this.flopPow = Integer.parseInt(value);
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return;
    }



    /**
     * 演算カウントを追加する
     * @param key		組込み関数名
     * @param value		演算カウント
     */
    public void addOperationProperty(String key, OperationCount value) {
        this.put(key, value);
    }

    /**
     * 演算カウントを取得する
     * @param key		組込み関数名
     * @return        演算カウント
     */
    public OperationCount getOperationProperty(String key) {
        return (OperationCount) this.get(key);
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
     * @param node		出力ノード
     */
    public void writeProperties(org.w3c.dom.Node node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("operationproperties.document.comment")); //演算カウントプロパティ
            node.appendChild(comment);
        }

        // 四則演算FLOP設定
        {
        	org.w3c.dom.Element elem = document.createElement("operation_flop");
            // 演算子:+FLOP
        	{
                org.w3c.dom.Attr attr = document.createAttribute("add");
                attr.setNodeValue(String.valueOf(this.flopAdd));
                elem.setAttributeNode(attr);
            }
            // 演算子:*FLOP
            {
                org.w3c.dom.Attr attr = document.createAttribute("mul");
                attr.setNodeValue(String.valueOf(this.flopMul));
                elem.setAttributeNode(attr);
            }
            // 演算子:-FLOP
            {
                org.w3c.dom.Attr attr = document.createAttribute("sub");
                attr.setNodeValue(String.valueOf(this.flopSub));
                elem.setAttributeNode(attr);
            }
            // 演算子:/FLOP
            {
                org.w3c.dom.Attr attr = document.createAttribute("div");
                attr.setNodeValue(String.valueOf(this.flopDiv));
                elem.setAttributeNode(attr);
            }
            // 演算子:**FLOP
            {
                org.w3c.dom.Attr attr = document.createAttribute("pow");
                attr.setNodeValue(String.valueOf(this.flopPow));
                elem.setAttributeNode(attr);
            }
            // ノード追加
            node.appendChild(elem);
        }
        // 組み込み関数演算数
        Enumeration<Object> enumKeys = this.keys();
        if (enumKeys == null) return;

        // キーワード
        while (enumKeys.hasMoreElements()){
            String key = (String)enumKeys.nextElement();
            OperationCount opc = getOperationProperty(key);

            org.w3c.dom.Element elem = document.createElement("operation");
            // 組込み関数名
            {
                org.w3c.dom.Attr attr = document.createAttribute("name");
                attr.setNodeValue(opc.getName());
                elem.setAttributeNode(attr);
            }
            // 演算子:+カウント
            if (opc.getAdd() != null) {
                org.w3c.dom.Attr attr = document.createAttribute("add");
                attr.setNodeValue(String.valueOf(opc.getAdd()));
                elem.setAttributeNode(attr);
            }
            // 演算子:*カウント
            if (opc.getMul() != null) {
                org.w3c.dom.Attr attr = document.createAttribute("mul");
                attr.setNodeValue(String.valueOf(opc.getMul()));
                elem.setAttributeNode(attr);
            }
            // 演算子:-カウント
            if (opc.getSub() != null) {
                org.w3c.dom.Attr attr = document.createAttribute("sub");
                attr.setNodeValue(String.valueOf(opc.getSub()));
                elem.setAttributeNode(attr);
            }
            // 演算子:/カウント
            if (opc.getDiv() != null) {
                org.w3c.dom.Attr attr = document.createAttribute("div");
                attr.setNodeValue(String.valueOf(opc.getDiv()));
                elem.setAttributeNode(attr);
            }

            // ノード追加
            node.appendChild(elem);
        }
    }

	/**
	 * 演算子FLOP:+を取得する
	 * @return flopAdd		演算子FLOP:+
	 */
	public int getFlopAdd() {
		return flopAdd;
	}

	/**
	 * 演算子FLOP:+を設定する
	 * @param value  演算子FLOP:+
	 */
	public void setFlopAdd(int value) {
		this.flopAdd = value;
	}

	/**
	 * 演算子FLOP:*を取得する
	 * @return flopMul		演算子FLOP:*
	 */
	public int getFlopMul() {
		return flopMul;
	}

	/**
	 * 演算子FLOP:*を設定する
	 * @param value  演算子FLOP:*
	 */
	public void setFlopMul(int value) {
		this.flopMul = value;
	}

	/**
	 * 演算子FLOP:-を取得する
	 * @return flopSub		演算子FLOP:-
	 */
	public int getFlopSub() {
		return flopSub;
	}

	/**
	 * 演算子FLOP:-を設定する
	 * @param value  演算子FLOP:-
	 */
	public void setFlopSub(int value) {
		this.flopSub = value;
	}

	/**
	 * 演算子FLOP:/を取得する.
	 * @return flopDiv		演算子FLOP:/
	 */
	public int getFlopDiv() {
		return flopDiv;
	}

	/**
	 * 演算子FLOP:/を設定する
	 * @param value  演算子FLOP:/
	 */
	public void setFlopDiv(int value) {
		this.flopDiv = value;
	}

	/**
	 * 演算子FLOP:**を取得する.
	 * @return flopPow		演算子FLOP:**
	 */
	public int getFlopPow() {
		return flopPow;
	}

	/**
	 * 演算子FLOP:**を設定する
	 * @param value  演算子FLOP:**
	 */
	public void setFlopPow(int value) {
		this.flopPow = value;
	}

}
