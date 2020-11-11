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
 * XML file I / O utility class
 *
 * @author RIKEN
 */
public class XmlUtils {

  /** XML document */
  private Document document;
  /** XML path */
  private XPath xpath;

  /**
   * Constructor
   *
   * @param filename XML file name
   * @throws Exception File does not exist, DOM generation error
   */
  public XmlUtils(String filename) throws Exception {
    InputStream is = new FileInputStream(filename);
    // Generate DOM
    initialize(is);
  }

  /**
   * Constructor
   *
   * @param file XML file
   * @throws Exception File does not exist, DOM generation error
   */
  public XmlUtils(File file) throws Exception {
    InputStream is = new FileInputStream(file);
    // Generate DOM
    initialize(is);
  }

  /**
   * Constructor
   *
   * @param is XML input stream
   * @throws Exception DOM generation error
   */
  public XmlUtils(InputStream is) throws Exception {
    // Generate DOM
    initialize(is);
  }

  /**
   * Initialize. <br>
   * Generate DOM
   *
   * @param is XML input stream
   * @throws Exception
   */
  private void initialize(InputStream is) throws Exception {

    // XML perspective
    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = dbfactory.newDocumentBuilder();
    this.document = builder.parse(is);

    XPathFactory factory = XPathFactory.newInstance();
    this.xpath = factory.newXPath();
  }

  /**
   * Get XPATH element, attribute values: numbers.
   *
   * @param path XPATH
   * @return Value: Numeric
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
   * Get XPATH element, attribute value: string.
   *
   * @param path XPATH
   * @return Value: String
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
   * Get a list of XPATH elements and attributes
   *
   * @param path XPATH
   * @return list: string
   */
  public List<String> getList(String path) {

    List<String> list = new ArrayList<String>();
    try {
      XPathExpression expr = xpath.compile(path);
      NodeList itemNodeList = (NodeList) expr.evaluate(document, XPathConstants.NODESET);

      if (itemNodeList.getLength() == 0) return null;

      for (int i = 0; i < itemNodeList.getLength(); i++) {
        Node node = itemNodeList.item(i);
        NodeList textNodes = node.getChildNodes();
        if (textNodes.getLength() <= 0) continue;

        String value = textNodes.item(0).getNodeValue();
        if (value == null) continue;
        list.add(value);
      }

    } catch (Exception ex) {
      ex.printStackTrace();
    }
    if (list.size() <= 0) return null;

    return list;
  }

  /**
   * Get XPATH element, attribute values: color.
   *
   * @param path XPATH
   * @return Value: Color
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
   * Get the value of an XPATH element: Font. <br>
   * Create a font from the attributes of the font element. <br>
   * name: font name <br>
   * size: font size <br>
   * bold: Void ('true' or'false') <br>
   * italic: void ('true' or'false')
   *
   * @param path XPATH
   * @return Value: Font
   */
  public Font getFont(String path) {

    Font font = null;
    try {
      XPathExpression expr = xpath.compile(path);

      Object result = expr.evaluate(document, XPathConstants.NODE);

      Node node = (Node) result;
      if (node == null) return null;

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
      if (bold) style += Font.BOLD;
      if (italic) style += Font.ITALIC;

      // System.out.println("property:font name" + name);

      font = new Font(name, style, size);

    } catch (Exception ex) {
      ex.printStackTrace();
    }

    return font;
  }

  /**
   * Create font attributes
   *
   * @param node Create node
   * @param font Created font
   */
  public static void createFontAttribute(org.w3c.dom.Element node, Font font) {

    // Get documentation
    org.w3c.dom.Document document = node.getOwnerDocument();

    // name attribute
    {
      org.w3c.dom.Attr attr = document.createAttribute("fontname");
      attr.setValue(font.getFamily());
      node.setAttributeNode(attr);
    }
    // size
    {
      org.w3c.dom.Attr attr = document.createAttribute("size");
      attr.setValue(String.valueOf(font.getSize()));
      node.setAttributeNode(attr);
    }
    // Style
    int style = font.getStyle();
    boolean bold = false;
    boolean italic = false;
    if ((style & Font.BOLD) != 0) bold = true;
    if ((style & Font.ITALIC) != 0) italic = true;
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
   * Create color attributes
   *
   * @param node Create node
   * @param color Created color
   */
  public static void createColorAttribute(org.w3c.dom.Element node, Color color) {

    // Get documentation
    org.w3c.dom.Document document = node.getOwnerDocument();

    // Color attributes
    {
      org.w3c.dom.Attr attr = document.createAttribute("color");
      String value = StringUtils.parseColorCode(color);
      attr.setValue(value);
      node.setAttributeNode(attr);
    }

    return;
  }
}
