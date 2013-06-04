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

package jp.riken.kscope.utils;

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

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * XMLファイル入出力ユーティリティクラス
 * 
 * @author riken
 */
public class XmlUtils {

	/** XMLドキュメント */
	private Document document;
	/** XMLパス */
	private XPath xpath;

	/**
	 * コンストラクタ
	 * 
	 * @param filename
	 *            XMLファイル名
	 * @throws Exception
	 *             ファイルが存在しない, DOM生成エラー
	 */
	public XmlUtils(String filename) throws Exception {
		InputStream is = new FileInputStream(filename);
		// DOMの生成を行う
		initialize(is);
	}

	/**
	 * コンストラクタ
	 * 
	 * @param file
	 *            XMLファイル
	 * @throws Exception
	 *             ファイルが存在しない, DOM生成エラー
	 */
	public XmlUtils(File file) throws Exception {
		InputStream is = new FileInputStream(file);
		// DOMの生成を行う
		initialize(is);
	}

	/**
	 * コンストラクタ
	 * 
	 * @param is
	 *            XML入力ストリーム
	 * @throws Exception
	 *             DOM生成エラー
	 */
	public XmlUtils(InputStream is) throws Exception {
		// DOMの生成を行う
		initialize(is);
	}

	/**
	 * 初期化を行う.<br/>
	 * DOMの生成を行う
	 * 
	 * @param is
	 *            XML入力ストリーム
	 * @throws Exception
	 */
	private void initialize(InputStream is) throws Exception {

		// XMLパース
		DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = dbfactory.newDocumentBuilder();
		this.document = builder.parse(is);

		XPathFactory factory = XPathFactory.newInstance();
		this.xpath = factory.newXPath();
	}

	/**
	 * XPATHの要素、属性の値を取得する:数値.
	 * 
	 * @param path
	 *            XPATH
	 * @return 値:数値
	 */
	public int getInt(String path) {

		int value = 0;
		try {
			XPathExpression expr = xpath.compile(path);

			Object result = expr.evaluate(document, XPathConstants.STRING);

			value = Integer.parseInt((String) result);
		} catch (Exception ex) {
			// ex.printStackTrace();
			value = -1;
		}

		return value;
	}

	/**
	 * XPATHの要素、属性の値を取得する:文字列.
	 * 
	 * @param path
	 *            XPATH
	 * @return 値:文字列
	 */
	public String getString(String path) {

		String value = null;
		try {
			XPathExpression expr = xpath.compile(path);

			Object result = expr.evaluate(document, XPathConstants.STRING);

			value = (String) result;
		} catch (Exception ex) {
			ex.printStackTrace();
		}

		return value;
	}

	/**
	 * XPATHの要素、属性のリストを取得する
	 * 
	 * @param path
	 *            XPATH
	 * @return リスト:文字列
	 */
	public List<String> getList(String path) {

		List<String> list = new ArrayList<String>();
		try {
			XPathExpression expr = xpath.compile(path);
			NodeList itemNodeList = (NodeList) expr.evaluate(document,
					XPathConstants.NODESET);

			if (itemNodeList.getLength() == 0)
				return null;

			for (int i = 0; i < itemNodeList.getLength(); i++) {
				Node node = itemNodeList.item(i);
				NodeList textNodes = node.getChildNodes();
				if (textNodes.getLength() <= 0)
					continue;

				String value = textNodes.item(0).getNodeValue();
				if (value == null)
					continue;
				list.add(value);
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}
		if (list.size() <= 0)
			return null;

		return list;
	}

	/**
	 * XPATHの要素、属性の値を取得する:色.
	 * 
	 * @param path
	 *            XPATH
	 * @return 値:色
	 */
	public Color getColor(String path) {

		Color color = null;
		try {
			XPathExpression expr = xpath.compile(path);

			Object result = expr.evaluate(document, XPathConstants.STRING);

			color = StringUtils.parseColor((String) result);

		} catch (Exception ex) {
			ex.printStackTrace();
		}

		return color;
	}

	/**
	 * XPATHの要素の値を取得する:フォント.<br/>
	 * フォント要素の属性からフォントを作成する.<br/>
	 * name:フォント名<br/>
	 * size:フォントサイズ<br/>
	 * bold:ボイド('true' or 'false')<br/>
	 * italic:ボイド('true' or 'false')
	 * 
	 * @param path
	 *            XPATH
	 * @return 値:フォント
	 */
	public Font getFont(String path) {

		Font font = null;
		try {
			XPathExpression expr = xpath.compile(path);

			Object result = expr.evaluate(document, XPathConstants.NODE);

			Node node = (Node) result;
			if (node == null)
				return null;

			NamedNodeMap attrs = node.getAttributes();
			Node attrnode;
			String value;
			attrnode = attrs.getNamedItem("fontname");
			String name = attrnode.getNodeValue();
			attrnode = attrs.getNamedItem("size");
			value = attrnode.getNodeValue();
			int size = Integer.parseInt(value);
			attrnode = attrs.getNamedItem("bold");
			value = attrnode.getNodeValue();
			boolean bold = Boolean.parseBoolean(value);
			attrnode = attrs.getNamedItem("italic");
			value = attrnode.getNodeValue();
			boolean italic = Boolean.parseBoolean(value);
			int style = Font.PLAIN;
			if (bold)
				style += Font.BOLD;
			if (italic)
				style += Font.ITALIC;

			// System.out.println("property:font name" + name);

			font = new Font(name, style, size);

		} catch (Exception ex) {
			ex.printStackTrace();
		}

		return font;
	}

	/**
	 * フォント属性を作成する
	 * 
	 * @param node
	 *            作成ノード
	 * @param font
	 *            作成フォント
	 */
	public static void createFontAttribute(org.w3c.dom.Element node, Font font) {

		// ドキュメントの取得
		org.w3c.dom.Document document = node.getOwnerDocument();

		// name属性
		{
			org.w3c.dom.Attr attr = document.createAttribute("fontname");
			attr.setValue(font.getFamily());
			node.setAttributeNode(attr);
		}
		// サイズ
		{
			org.w3c.dom.Attr attr = document.createAttribute("size");
			attr.setValue(String.valueOf(font.getSize()));
			node.setAttributeNode(attr);
		}
		// スタイル
		int style = font.getStyle();
		boolean bold = false;
		boolean italic = false;
		if ((style & Font.BOLD) != 0)
			bold = true;
		if ((style & Font.ITALIC) != 0)
			italic = true;
		// bold
		{
			org.w3c.dom.Attr attr = document.createAttribute("bold");
			attr.setValue(String.valueOf(bold));
			node.setAttributeNode(attr);
		}
		// italic
		{
			org.w3c.dom.Attr attr = document.createAttribute("italic");
			attr.setValue(String.valueOf(italic));
			node.setAttributeNode(attr);
		}

		return;
	}

	/**
	 * 色属性を作成する
	 * 
	 * @param node
	 *            作成ノード
	 * @param color
	 *            作成色
	 */
	public static void createColorAttribute(org.w3c.dom.Element node,
			Color color) {

		// ドキュメントの取得
		org.w3c.dom.Document document = node.getOwnerDocument();

		// 色属性
		{
			org.w3c.dom.Attr attr = document.createAttribute("color");
			String value = StringUtils.parseColorCode(color);
			attr.setValue(value);
			node.setAttributeNode(attr);
		}

		return;
	}

}
