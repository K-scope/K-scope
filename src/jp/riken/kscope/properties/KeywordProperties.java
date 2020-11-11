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
 * Source code keyword (highlight) setting class
 *
 * @author RIKEN
 */
public class KeywordProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Keyword (highlight) setting list */
  private List<Keyword> listKeyword = new ArrayList<Keyword>();

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public KeywordProperties() throws Exception {
    loadProperties();
  }

  /**
   * Read source configuration properties from the default configuration file.
   *
   * @throws Exception Property read error
   */
  public void loadProperties() throws Exception {
    InputStream is = null;

    // Read resource file
    is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);

    loadProperties(is);
  }

  /**
   * Read source configuration properties from the configuration file.
   *
   * @param propertiesFile Source configuration property configuration file
   * @throws Exception Property read error
   */
  public void loadProperties(File propertiesFile) throws Exception {

    if (!propertiesFile.exists()) {
      throw (new Exception(
          Message.getString(
              "propertiesbase.exeption.notexist"))); // Source settings property file does not
      // exist.
    }

    // Read resource file
    InputStream stream = new FileInputStream(propertiesFile);

    // Parsing the XML file
    loadProperties(stream);
  }

  /**
   * Read source configuration properties from the configuration file.
   *
   * @param stream Configuration file stream
   * @throws Exception Property read error
   */
  public void loadProperties(InputStream stream) throws Exception {
    // Parsing the XML file
    listKeyword = parseKeyword(stream, "//keyword");
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /**
   * Get the keyword (highlight) setting list.
   *
   * @return highlight setting list
   */
  public List<Keyword> getListKeyword() {
    return listKeyword;
  }

  /**
   * Get the number of keywords (highlights).
   *
   * @return Number of keywords (highlights)
   */
  public int getKeywordCount() {
    if (listKeyword == null || listKeyword.size() <= 0) {
      return 0;
    }
    return listKeyword.size();
  }

  /**
   * Get keywords (highlights).
   *
   * @param index index
   * @return keyword (highlight)
   */
  public Keyword getKeyword(int index) {
    if (listKeyword == null || listKeyword.size() <= 0) {
      return null;
    }
    if (listKeyword.size() <= index) {
      return null;
    }
    return listKeyword.get(index);
  }

  /**
   * Set keywords (highlights).
   *
   * @param index index
   * @param keyword keyword (highlight)
   */
  public void setKeyword(int index, Keyword keyword) {
    if (listKeyword == null || listKeyword.size() <= 0) {
      return;
    }
    if (listKeyword.size() <= index) {
      return;
    }
    listKeyword.set(index, keyword);
  }

  /**
   * Add keywords (highlights).
   *
   * @param keyword keyword (highlight)
   */
  public void addKeyword(Keyword keyword) {
    if (listKeyword == null) {
      listKeyword = new ArrayList<Keyword>();
    }
    listKeyword.add(keyword);
  }

  /**
   * Remove keywords (highlights).
   *
   * @param keyword keyword (highlight)
   */
  public void removeKeyword(Keyword keyword) {
    if (listKeyword == null) return;
    listKeyword.remove(keyword);
  }

  /**
   * Remove keywords (highlights).
   *
   * @param index index
   */
  public void removeKeyword(int index) {
    if (listKeyword == null) return;
    listKeyword.remove(index);
  }

  /** Clear the keyword (highlight) list. */
  public void clearKeyword() {
    listKeyword = new ArrayList<Keyword>();
  }

  /**
   * Get Keyword information that matches the search keyword
   *
   * @param word Search keyword
   * @return Keyword information
   */
  public Keyword getKeyword(String word) {
    if (word == null || word.isEmpty()) return null;

    for (Keyword keyword : listKeyword) {
      String srcWord = keyword.getKeyword();
      boolean sensitivecase = keyword.isSensitivecase();
      if (sensitivecase) {
        // Case sensitive
        if (word.equals(srcWord)) {
          return keyword;
        }
      } else {
        // Case insensitive
        if (word.equalsIgnoreCase(srcWord)) {
          return keyword;
        }
      }
    }

    return null;
  }

  /**
   * Get keywords
   *
   * @param stream XML input stream
   * @param path Keyword XPATH
   * @return keyword list
   * @throws Exception Keyword parsing error
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

    for (int i = 0; i < nodelist.getLength(); i++) {
      try {
        Node node = nodelist.item(i);
        Keyword keyword = new Keyword(KEYWORD_TYPE.KEYWORD);

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode;
        String value;
        // Keyword name
        attrnode = attrs.getNamedItem("name");
        String name = null;
        if (attrnode != null) {
          name = attrnode.getNodeValue();
          keyword.setName(name);
        }
        // Keywords
        attrnode = attrs.getNamedItem("keyword");
        String word = null;
        if (attrnode != null) {
          word = attrnode.getNodeValue();
          keyword.setKeyword(word);
        }

        // Class mode
        attrnode = attrs.getNamedItem("class");
        String class_mode = null;
        if (attrnode != null) {
          class_mode = attrnode.getNodeValue();
          keyword.setClassmode(class_mode);
        }
        // Bold
        attrnode = attrs.getNamedItem("bold");
        boolean bold = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          bold = Boolean.parseBoolean(value);
        }
        // italics
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

        // Letter color
        attrnode = attrs.getNamedItem("forecolor");
        Color forecolor = null;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          forecolor = StringUtils.parseColor(value);
          keyword.setForecolor(forecolor);
        }

        // Valid / Invalid
        attrnode = attrs.getNamedItem("enabled");
        boolean enabled = true;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          enabled = Boolean.parseBoolean(value);
          keyword.setEnabled(enabled);
        }
        // Case sensitive
        attrnode = attrs.getNamedItem("sensitivecase");
        boolean sensitivecase = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          sensitivecase = Boolean.parseBoolean(value);
          keyword.setCaseSensitive(sensitivecase);
        }
        // Regular expressions
        attrnode = attrs.getNamedItem("regex");
        boolean regex = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          regex = Boolean.parseBoolean(value);
          keyword.setRegex(regex);
        }
        // Add keyword change
        attrnode = attrs.getNamedItem("keywordlock");
        boolean keywordlock = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          keywordlock = Boolean.parseBoolean(value);
          keyword.setKeywordlock(keywordlock);
        }

        // Keyword is word search
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
              Message.getString("keywordproperties.document.comment")); // keyword property
      node.appendChild(comment);
    }

    if (this.listKeyword == null || this.listKeyword.size() <= 0) return;

    // Keywords
    for (Keyword keyword : this.listKeyword) {
      org.w3c.dom.Element elem = document.createElement("keyword");

      // Keyword name
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("name");
        attr.setValue(keyword.getName());
        elem.setAttributeNode(attr);
      }
      // Keywords
      {
        org.w3c.dom.Attr attr = document.createAttribute("keyword");
        attr.setNodeValue(keyword.getKeyword());
        elem.setAttributeNode(attr);
      }
      // class
      if (keyword.getClassmode() != null && !keyword.getClassmode().isEmpty()) {
        org.w3c.dom.Attr attr = document.createAttribute("class");
        attr.setNodeValue(keyword.getClassmode());
        elem.setAttributeNode(attr);
      }
      // Style
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
      // Letter color
      {
        Color color = keyword.getForecolor();
        org.w3c.dom.Attr attr = document.createAttribute("forecolor");
        attr.setNodeValue(StringUtils.parseColorCode(color));
        elem.setAttributeNode(attr);
      }
      // Letter color
      {
        Color color = keyword.getForecolor();
        org.w3c.dom.Attr attr = document.createAttribute("forecolor");
        attr.setNodeValue(StringUtils.parseColorCode(color));
        elem.setAttributeNode(attr);
      }
      // Valid / Invalid
      {
        org.w3c.dom.Attr attr = document.createAttribute("enabled");
        attr.setNodeValue(String.valueOf(keyword.isEnabled()));
        elem.setAttributeNode(attr);
      }
      // Case sensitive
      {
        org.w3c.dom.Attr attr = document.createAttribute("sensitivecase");
        attr.setNodeValue(String.valueOf(keyword.isSensitivecase()));
        elem.setAttributeNode(attr);
      }
      // Regular expressions
      {
        org.w3c.dom.Attr attr = document.createAttribute("regex");
        attr.setNodeValue(String.valueOf(keyword.isRegex()));
        elem.setAttributeNode(attr);
      }
      // Add node
      node.appendChild(elem);
    }
  }
}
