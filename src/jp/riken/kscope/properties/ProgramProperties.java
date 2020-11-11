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
 * External tool property class
 *
 * @author RIKEN
 */
public class ProgramProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** External tool setting list */
  private List<Program> listProgram = new ArrayList<Program>();

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public ProgramProperties() throws Exception {
    loadProperties();
  }

  /**
   * Read external tool properties from the default configuration file.
   *
   * @throws Exception Property read error
   */
  public void loadProperties() throws Exception {
    InputStream stream = null;

    // Read resource file
    stream = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);

    // Read the source configuration properties from the configuration file.
    loadProperties(stream);
  }

  /**
   * Read external tool properties from the configuration file.
   *
   * @param propertiesFile Configuration file
   * @throws Exception Property read error
   */
  public void loadProperties(File propertiesFile) throws Exception {

    if (!propertiesFile.exists()) {
      throw (new Exception(
          Message.getString(
              "propertiesbase.exeption.notexist"))); // External tool properties file does not
      // exist.
    }

    // Read resource file
    InputStream stream = new FileInputStream(propertiesFile);

    // Load external tool properties from the config file.
    loadProperties(stream);
  }

  /**
   * Read external tool properties from the default configuration file.
   *
   * @param stream Configuration file stream
   * @throws Exception Property read error
   */
  public void loadProperties(InputStream stream) throws Exception {
    // Parsing the XML file
    listProgram = parseProgram(stream, "//program");
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /**
   * Get the external tool setting list.
   *
   * @return External tool setting list
   */
  public List<Program> getListProgram() {
    return listProgram;
  }

  /**
   * Get the number of external tool settings.
   *
   * @return Number of external tool settings
   */
  public int getProgramCount() {
    if (listProgram == null || listProgram.size() <= 0) return 0;
    return listProgram.size();
  }

  /**
   * Get external tool settings.
   *
   * @param index index
   * @return External tool settings
   */
  public Program getProgram(int index) {
    if (listProgram == null || listProgram.size() <= 0) return null;
    if (listProgram.size() <= index) return null;

    return listProgram.get(index);
  }

  /**
   * Set external tool settings.
   *
   * @param index index
   * @param exe external tool settings
   */
  public void setProgram(int index, Program exe) {
    if (listProgram == null || listProgram.size() <= 0) return;
    if (listProgram.size() <= index) return;
    listProgram.set(index, exe);
  }

  /**
   * Add external tool settings.
   *
   * @param exe external tool settings
   */
  public void addProgram(Program exe) {
    if (listProgram == null) {
      listProgram = new ArrayList<Program>();
    }

    listProgram.add(exe);
  }

  /**
   * Delete external tool settings.
   *
   * @param exe external tool settings
   */
  public void removeProgram(Program exe) {
    if (listProgram == null) return;

    listProgram.remove(exe);
  }

  /**
   * Delete external tool settings.
   *
   * @param index index
   */
  public void removeProgram(int index) {
    if (listProgram == null) return;

    listProgram.remove(index);
  }

  /** Delete external tool settings. */
  public void clearProgram() {
    if (listProgram == null) return;

    listProgram.clear();
  }

  /**
   * Get external tool settings that match your search pattern
   *
   * @param pattern Search pattern
   * @return External tool settings
   */
  public Program getProgram(String pattern) {
    if (pattern == null || pattern.isEmpty()) return null;

    for (Program exe : listProgram) {
      String srcPattern = exe.getPattern();
      // Case insensitive
      if (pattern.equalsIgnoreCase(srcPattern)) {
        return exe;
      }
    }

    return null;
  }

  /**
   * Get external tool settings
   *
   * @param stream XML file stream
   * @param path External tool settings XPATH
   * @return External tool setting list
   * @throws Exception Property read error
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

    for (int i = 0; i < nodelist.getLength(); i++) {
      try {
        Node node = nodelist.item(i);
        Program exe = new Program();

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode;
        String value;
        // program name
        attrnode = attrs.getNamedItem("name");
        String name = null;
        if (attrnode != null) {
          name = attrnode.getNodeValue();
          exe.setName(name);
        }
        // Pattern
        attrnode = attrs.getNamedItem("pattern");
        String pattern = null;
        if (attrnode != null) {
          pattern = attrnode.getNodeValue();
          exe.setPattern(pattern);
        }
        // extension
        attrnode = attrs.getNamedItem("exts");
        boolean exts = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          exts = Boolean.parseBoolean(value);
          exe.setExts(exts);
        }
        // Regular expressions
        attrnode = attrs.getNamedItem("regex");
        boolean regex = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          regex = Boolean.parseBoolean(value);
          exe.setRegex(regex);
        }
        // Association
        attrnode = attrs.getNamedItem("relation");
        boolean relation = false;
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          relation = Boolean.parseBoolean(value);
          exe.setRelation(relation);
        }
        // External program
        attrnode = attrs.getNamedItem("program");
        String program = null;
        if (attrnode != null) {
          program = attrnode.getNodeValue();
          exe.setExename(program);
        }
        // Optional
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
                  "programproperties.document.comment")); // External program properties
      node.appendChild(comment);
    }

    if (this.listProgram == null || this.listProgram.size() <= 0) return;

    // Keywords
    for (Program program : this.listProgram) {
      org.w3c.dom.Element elem = document.createElement("program");

      // program name
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("name");
        attr.setNodeValue(program.getName());
        elem.setAttributeNode(attr);
      }
      // Pattern
      {
        org.w3c.dom.Attr attr = document.createAttribute("pattern");
        attr.setNodeValue(program.getPattern());
        elem.setAttributeNode(attr);
      }
      // extension
      {
        org.w3c.dom.Attr attr = document.createAttribute("exts");
        attr.setNodeValue(String.valueOf(program.isExts()));
        elem.setAttributeNode(attr);
      }
      // Regular expressions
      {
        org.w3c.dom.Attr attr = document.createAttribute("regex");
        attr.setNodeValue(String.valueOf(program.isRegex()));
        elem.setAttributeNode(attr);
      }
      // Association
      {
        org.w3c.dom.Attr attr = document.createAttribute("relation");
        attr.setNodeValue(String.valueOf(program.isRelation()));
        elem.setAttributeNode(attr);
      }
      // External program
      if (program.getExename() != null && !program.getExename().isEmpty()) {
        org.w3c.dom.Attr attr = document.createAttribute("program");
        String exename = program.getExename();
        exename = StringUtils.escapeFilePath(exename);
        attr.setNodeValue(exename);
        elem.setAttributeNode(attr);
      }
      // Optional
      if (program.getOption() != null && !program.getOption().isEmpty()) {
        org.w3c.dom.Attr attr = document.createAttribute("option");
        attr.setNodeValue(program.getOption());
        elem.setAttributeNode(attr);
      }
      // Add node
      node.appendChild(elem);
    }
  }

  /**
   * Get the program that matches the search character
   *
   * @param text Search character
   * @return Matching program
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
