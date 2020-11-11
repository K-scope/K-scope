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
 * Arithmetic count property class
 *
 * @author RIKEN
 */
public class OperationProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Operator FLOP: + */
  private int flopAdd;
  /** Operator FLOP: * */
  private int flopMul;
  /** Operator FLOP:- */
  private int flopSub;
  /** Operator FLOP: / */
  private int flopDiv;
  /** Operator FLOP: ** */
  private int flopPow;

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public OperationProperties() throws Exception {
    loadProperties();
  }

  /**
   * Read the operation count property from the default configuration file.
   *
   * @throws Exception Property read error
   */
  public void loadProperties() throws Exception {
    InputStream stream = null;
    // Read resource file
    stream = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
    // Read the operation count property from the configuration file.
    loadProperties(stream);
  }

  /**
   * Read the operation count property from the configuration file.
   *
   * @param propertiesFile Arithmetic count property setting file
   * @throws Exception Property read error
   */
  public void loadProperties(File propertiesFile) throws Exception {

    if (!propertiesFile.exists()) {
      throw (new Exception(
          Message.getString(
              "propertiesbase.exeption.notexist"))); // The operation count property file does not
      // exist.
    }

    // Read resource file
    InputStream stream = new FileInputStream(propertiesFile);

    // Parsing the XML file
    loadProperties(stream);
  }

  /**
   * Read the operation count property from the configuration file.
   *
   * @param stream Configuration file stream
   * @throws Exception Property read error
   */
  public void loadProperties(InputStream stream) throws Exception {

    // XML
    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = dbfactory.newDocumentBuilder();
    org.w3c.dom.Document document = builder.parse(stream);

    // Parsing the XML file
    List<OperationCount> list = parseOperation(document, "//operation");

    // For compatibility with older projects
    if (list == null) {
      list = parseOperation(document, "//operand");
    }

    // Add to HashTable of Properties class
    // The key is the built-in function name (= name)
    for (OperationCount opc : list) {
      String name = opc.getName();
      if (name != null && !name.isEmpty()) {
        addOperationProperty(name, opc);
      }
    }

    // Four arithmetic FLOP settings
    parseOperatorFlop(document, "//operator_flop");
  }

  /**
   * Get the operation count setting
   *
   * @param document XML document
   * @param path Operation count setting XPATH
   * @return Operation count setting list
   * @throws Exception Property read error
   */
  public List<OperationCount> parseOperation(org.w3c.dom.Document document, String path)
      throws Exception {
    List<OperationCount> list = new ArrayList<OperationCount>();

    XPathFactory factory = XPathFactory.newInstance();
    XPath xpath = factory.newXPath();

    XPathExpression expr = xpath.compile(path);

    Object result = expr.evaluate(document, XPathConstants.NODESET);

    NodeList nodelist = (NodeList) result;
    if (nodelist == null) return null;

    for (int i = 0; i < nodelist.getLength(); i++) {
      try {
        Node node = nodelist.item(i);
        OperationCount opc = new OperationCount();

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode;
        String value = null;
        // Built-in function name
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

        // Operator: + count
        attrnode = attrs.getNamedItem("add");
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          if (StringUtils.isNumeric(value)) {
            opc.setAdd(Integer.parseInt(value));
          }
        }
        // Operator:-Count
        attrnode = attrs.getNamedItem("sub");
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          if (StringUtils.isNumeric(value)) {
            opc.setSub(Integer.parseInt(value));
          }
        }
        // Operator: * Count
        attrnode = attrs.getNamedItem("mul");
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          if (StringUtils.isNumeric(value)) {
            opc.setMul(Integer.parseInt(value));
          }
        }
        // Operator: / Count
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
   * Get the four arithmetic FLOP settings
   *
   * @param document XML document
   * @param path Four arithmetic FLOP settings XPATH
   * @return Operation count setting list
   * @throws Exception Property read error
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

      // Get attributes
      NamedNodeMap attrs = node.getAttributes();
      Node attrnode;
      String value = null;
      // Operator: +: FLOP
      attrnode = attrs.getNamedItem("add");
      if (attrnode != null) {
        value = attrnode.getNodeValue();
        if (StringUtils.isNumeric(value)) {
          this.flopAdd = Integer.parseInt(value);
        }
      }
      // Operator: -FLOP
      attrnode = attrs.getNamedItem("sub");
      if (attrnode != null) {
        value = attrnode.getNodeValue();
        if (StringUtils.isNumeric(value)) {
          this.flopSub = Integer.parseInt(value);
        }
      }
      // Operator: * FLOP
      attrnode = attrs.getNamedItem("mul");
      if (attrnode != null) {
        value = attrnode.getNodeValue();
        if (StringUtils.isNumeric(value)) {
          this.flopMul = Integer.parseInt(value);
        }
      }
      // Operator: / FLOP
      attrnode = attrs.getNamedItem("div");
      if (attrnode != null) {
        value = attrnode.getNodeValue();
        if (StringUtils.isNumeric(value)) {
          this.flopDiv = Integer.parseInt(value);
        }
      }
      // Operator: ** FLOP
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
   * Add operation count
   *
   * @param key Built-in function name
   * @param value Operation count
   */
  public void addOperationProperty(String key, OperationCount value) {
    this.put(key, value);
  }

  /**
   * Get the operation count
   *
   * @param key Built-in function name
   * @return operation count
   */
  public OperationCount getOperationProperty(String key) {
    return (OperationCount) this.get(key);
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /**
   * Output properties to DOM node
   *
   * @param node Output node
   */
  public void writeProperties(org.w3c.dom.Node node) {

    // Get documentation
    org.w3c.dom.Document document = node.getOwnerDocument();

    // add comment
    {
      org.w3c.dom.Comment comment =
          document.createComment(
              Message.getString(
                  "operationproperties.document.comment")); // Operation count property
      node.appendChild(comment);
    }

    // Four arithmetic FLOP settings
    {
      org.w3c.dom.Element elem = document.createElement("operation_flop");
      // Operator: + FLOP
      {
        org.w3c.dom.Attr attr = document.createAttribute("add");
        attr.setNodeValue(String.valueOf(this.flopAdd));
        elem.setAttributeNode(attr);
      }
      // Operator: * FLOP
      {
        org.w3c.dom.Attr attr = document.createAttribute("mul");
        attr.setNodeValue(String.valueOf(this.flopMul));
        elem.setAttributeNode(attr);
      }
      // Operator: -FLOP
      {
        org.w3c.dom.Attr attr = document.createAttribute("sub");
        attr.setNodeValue(String.valueOf(this.flopSub));
        elem.setAttributeNode(attr);
      }
      // Operator: / FLOP
      {
        org.w3c.dom.Attr attr = document.createAttribute("div");
        attr.setNodeValue(String.valueOf(this.flopDiv));
        elem.setAttributeNode(attr);
      }
      // Operator: ** FLOP
      {
        org.w3c.dom.Attr attr = document.createAttribute("pow");
        attr.setNodeValue(String.valueOf(this.flopPow));
        elem.setAttributeNode(attr);
      }
      // Add node
      node.appendChild(elem);
    }
    // Number of built-in function operations
    Enumeration<Object> enumKeys = this.keys();
    if (enumKeys == null) return;

    // Keywords
    while (enumKeys.hasMoreElements()) {
      String key = (String) enumKeys.nextElement();
      OperationCount opc = getOperationProperty(key);

      org.w3c.dom.Element elem = document.createElement("operation");
      // Built-in function name
      {
        org.w3c.dom.Attr attr = document.createAttribute("name");
        attr.setNodeValue(opc.getName());
        elem.setAttributeNode(attr);
      }
      // Operator: + count
      if (opc.getAdd() != null) {
        org.w3c.dom.Attr attr = document.createAttribute("add");
        attr.setNodeValue(String.valueOf(opc.getAdd()));
        elem.setAttributeNode(attr);
      }
      // Operator: * Count
      if (opc.getMul() != null) {
        org.w3c.dom.Attr attr = document.createAttribute("mul");
        attr.setNodeValue(String.valueOf(opc.getMul()));
        elem.setAttributeNode(attr);
      }
      // Operator:-Count
      if (opc.getSub() != null) {
        org.w3c.dom.Attr attr = document.createAttribute("sub");
        attr.setNodeValue(String.valueOf(opc.getSub()));
        elem.setAttributeNode(attr);
      }
      // Operator: / Count
      if (opc.getDiv() != null) {
        org.w3c.dom.Attr attr = document.createAttribute("div");
        attr.setNodeValue(String.valueOf(opc.getDiv()));
        elem.setAttributeNode(attr);
      }

      // Add node
      node.appendChild(elem);
    }
  }

  /**
   * Get operator FLOP: +
   *
   * @return flopAdd operator FLOP: +
   */
  public int getFlopAdd() {
    return flopAdd;
  }

  /**
   * Set operator FLOP: +
   *
   * @param value operator FLOP: +
   */
  public void setFlopAdd(int value) {
    this.flopAdd = value;
  }

  /**
   * Get operator FLOP: *
   *
   * @return flopMul operator FLOP: *
   */
  public int getFlopMul() {
    return flopMul;
  }

  /**
   * Set operator FLOP: *
   *
   * @param value operator FLOP: *
   */
  public void setFlopMul(int value) {
    this.flopMul = value;
  }

  /**
   * Get operator FLOP:-
   *
   * @return flopSub operator FLOP:-
   */
  public int getFlopSub() {
    return flopSub;
  }

  /**
   * Set operator FLOP:-
   *
   * @param value operator FLOP:-
   */
  public void setFlopSub(int value) {
    this.flopSub = value;
  }

  /**
   * Get the operator FLOP: /.
   *
   * @return flopDiv operator FLOP: /
   */
  public int getFlopDiv() {
    return flopDiv;
  }

  /**
   * Set operator FLOP: /
   *
   * @param value operator FLOP: /
   */
  public void setFlopDiv(int value) {
    this.flopDiv = value;
  }

  /**
   * Get the operator FLOP: **.
   *
   * @return flopPow operator FLOP: **
   */
  public int getFlopPow() {
    return flopPow;
  }

  /**
   * Set operator FLOP: **
   *
   * @param value operator FLOP: **
   */
  public void setFlopPow(int value) {
    this.flopPow = value;
  }
}
