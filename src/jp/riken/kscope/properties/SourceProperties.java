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
 * Source code display setting class
 *
 * @author RIKEN
 */
public class SourceProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /* Property key */
  /** Source font */
  private final String KEY_FONT_SOURCE = "font-source";
  /** Source font color */
  private final String KEY_FONTCOLOR_SOURCE = "fontcolor-source";
  /** Source background color */
  private final String KEY_BACKGROUND_SOURCE = "background-source";
  /** Active row background color */
  private final String KEY_BACKGROUND_SELECTEDROW = "background-selectedrow";
  /** Folding position */
  private final String KEY_WORDWRAP = "wordwrap";
  /** Search text color */
  private final String KEY_FONTCOLOR_SEARCH = "fontcolor-search";
  /** Search text background color */
  private final String KEY_BACKGROUND_SEARCH = "background-search";
  /** Emphasis range background color */
  private final String KEY_BACKGROUND_AREA = "background-area";
  /** Selection background color */
  private final String KEY_BACKGROUND_BLOCK = "background-block";

  /* Line number background color */
  private final String KEY_BACKGROUND_LINENUMBER =
      "background-linenumber"; // (2012/4/10) added by teraim
  /** Selected node background color */
  private final String KEY_BACKGROUND_SELECTNODE = "background-selectnode";

  private final String KEY_BACKGROUND_VIEW2 = "background-view2";
  /** Additional information node font color */
  private final String KEY_FONTCOLOR_INFORMATIONNODE = "fontcolor-informationnode";
  /** Broken link node text color */
  private final String KEY_FONTCOLOR_BROKENLINKNODE = "fontcolor-brokenlinknode";

  /** Source code display setting list */
  private List<PropertyValue> listProperty = new ArrayList<PropertyValue>();

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public SourceProperties() throws Exception {
    loadProperties();
  }

  /**
   * Read source configuration properties from the default configuration file.
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

    // Read the source configuration properties from the configuration file.
    loadProperties(stream);
  }

  /**
   * Read source configuration properties from the configuration file.
   *
   * @param stream Source settings Property settings File stream
   * @throws Exception Property read error
   */
  public void loadProperties(InputStream stream) throws Exception {

    // Parsing the XML file
    String key = null;
    String name = null;
    String type = null;
    String message = null;
    XmlUtils xml = new XmlUtils(stream);

    // Font
    key = KEY_FONT_SOURCE;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Font font = xml.getFont("//settings/property[@key='" + key + "']");
    this.setPropertyValue(key, name, type, font, message);

    // Font color
    key = KEY_FONTCOLOR_SOURCE;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color forecolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, forecolor, message);

    // Background color
    key = KEY_BACKGROUND_SOURCE;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color backcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, backcolor, message);

    // Selected line background color
    key = KEY_BACKGROUND_SELECTEDROW;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color activerow = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, activerow, message);

    // Highlighted background color
    key = KEY_BACKGROUND_AREA;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color areacolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, areacolor, message);

    // Selection background color
    key = KEY_BACKGROUND_BLOCK;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color blockcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, blockcolor, message);

    // Line number background color (2012/4/12)
    key = KEY_BACKGROUND_LINENUMBER;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color linecolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, linecolor, message);

    // Selected line background color 1 (2012/4/12)
    key = KEY_BACKGROUND_SELECTNODE;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color viewcolor1 = xml.getColor("//settings/property[@key='" + key + "']/@color");
    if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
      this.setPropertyValue(key, name, type, viewcolor1, message);
    }

    // Selected line background color 2 (2012/4/12)
    key = KEY_BACKGROUND_VIEW2;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color viewcolor2 = xml.getColor("//settings/property[@key='" + key + "']/@color");
    if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
      this.setPropertyValue(key, name, type, viewcolor2, message);
    }

    // Search text color
    key = KEY_FONTCOLOR_SEARCH;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color searchforecolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, searchforecolor, message);

    // Search character background color
    key = KEY_BACKGROUND_SEARCH;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color searchbackcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    this.setPropertyValue(key, name, type, searchbackcolor, message);

    // Wrap position
    key = KEY_WORDWRAP;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Integer wordwrap = xml.getInt("//settings/property[@key='" + key + "']/@value");
    if (wordwrap < 0) wordwrap = 0;
    this.setPropertyValue(key, name, type, wordwrap, message);

    // Additional information font color
    key = KEY_FONTCOLOR_INFORMATIONNODE;
    name = xml.getString("//settings/property[@key='" + key + "']/@name");
    type = xml.getString("//settings/property[@key='" + key + "']/@type");
    message = xml.getString("//settings/property[@key='" + key + "']/@message");
    Color informationcolor = xml.getColor("//settings/property[@key='" + key + "']/@color");
    if (!(key.isEmpty() || name.isEmpty() || type.isEmpty())) {
      this.setPropertyValue(key, name, type, informationcolor, message);
    }

    // Broken link text color
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
   * Set the property value.
   *
   * @param key key
   * @param name Property name
   * @param type Property type
   * @param value Property value
   * @param message message
   */
  private void setPropertyValue(
      String key, String name, String type, Object value, String message) {
    PropertyValue property = new PropertyValue(key, name, type, value, message);
    setPropertyValue(property);
  }

  /**
   * Set the property value.
   *
   * @param value Property value
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

    // Add new
    this.listProperty.add(value);
  }

  /**
   * Get the property value.
   *
   * @param key key
   * @return property value
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
   * Get the source code display font.
   *
   * @return Source display font
   */
  public Font getFont() {
    PropertyValue value = getPropertyValue(KEY_FONT_SOURCE);
    if (value == null) {
      return null;
    }

    return (Font) value.getValue();
  }

  /**
   * Get source code display font color
   *
   * @return Source code display font color
   */
  public Color getFontColor() {
    PropertyValue value = getPropertyValue(KEY_FONTCOLOR_SOURCE);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get source code background color
   *
   * @return Source code background color
   */
  public Color getBackgroundColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_SOURCE);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get active row background color
   *
   * @return Active line background color
   */
  public Color getActiverowColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_SELECTEDROW);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get the active row background color (selected tree node)
   *
   * @return Active line background color
   */
  public Color getBackgoundSelectNodeColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_SELECTNODE);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get active row background color 2
   *
   * @return Active line background color
   */
  public Color getBackgoundView2Color() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_VIEW2);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get search text color
   *
   * @return Search text color
   */
  public Color getSearchFontColor() {
    PropertyValue value = getPropertyValue(KEY_FONTCOLOR_SEARCH);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get search background color
   *
   * @return Search background color
   */
  public Color getSearchBackgroundColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_SEARCH);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get the wrapping position
   *
   * @return Return position
   */
  public int getWordwrap() {
    PropertyValue value = getPropertyValue(KEY_WORDWRAP);
    if (value == null) {
      return 0;
    }

    return (Integer) value.getValue();
  }

  /**
   * Get emphasis range background color
   *
   * @return Emphasis range background color
   */
  public Color getAreaColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_AREA);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get selection background color
   *
   * @return Selection background color
   */
  public Color getBlockColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_BLOCK);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get line number background color (2012/4/10) added by teriam
   *
   * @return Selection background color
   */
  public Color getLineNumberColor() {
    PropertyValue value = getPropertyValue(KEY_BACKGROUND_LINENUMBER);
    if (value == null) {
      return null;
    }

    return (Color) value.getValue();
  }

  /**
   * Get additional information font color
   *
   * @return Additional information font color
   */
  public Color getInformationNodeFontColor() {
    PropertyValue value = getPropertyValue(KEY_FONTCOLOR_INFORMATIONNODE);
    if (value == null) {
      return null;
    }
    return (Color) value.getValue();
  }

  /**
   * Get broken link text color
   *
   * @return Broken link text color
   */
  public Color getBrokenLinkNodeFontColor() {
    PropertyValue value = getPropertyValue(KEY_FONTCOLOR_BROKENLINKNODE);
    if (value == null) {
      return null;
    }
    return (Color) value.getValue();
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
  public void writeProperties(org.w3c.dom.Element node) {

    // Get documentation
    org.w3c.dom.Document document = node.getOwnerDocument();

    // add comment
    {
      org.w3c.dom.Comment comment =
          document.createComment(
              Message.getString("sourceproperties.document.comment")); // Display properties
      node.appendChild(comment);
    }
    // Source font
    {
      String key = KEY_FONT_SOURCE;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createFontAttribute(elem, (Font) value.getValue());
        node.appendChild(elem);
      }
    }
    // Source font color
    {
      String key = KEY_FONTCOLOR_SOURCE;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Source background color
    {
      String key = KEY_BACKGROUND_SOURCE;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Get active row background color 1
    {
      String key = KEY_BACKGROUND_SELECTNODE;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Get active row background color 2
    {
      String key = KEY_BACKGROUND_VIEW2;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Active row background color
    {
      String key = KEY_BACKGROUND_SELECTEDROW;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Wrap position
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
    // Search text color
    {
      String key = KEY_FONTCOLOR_SEARCH;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Search character background color
    {
      String key = KEY_BACKGROUND_SEARCH;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Highlighted background color
    {
      String key = KEY_BACKGROUND_AREA;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Selection background color
    {
      String key = KEY_BACKGROUND_BLOCK;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Get line number background color (2012/4/10) added by teriam
    {
      String key = KEY_BACKGROUND_LINENUMBER;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Additional information font color
    {
      String key = KEY_FONTCOLOR_INFORMATIONNODE;
      PropertyValue value = this.getPropertyValue(key);
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, value);
        XmlUtils.createColorAttribute(elem, (Color) value.getValue());
        node.appendChild(elem);
      }
    }
    // Broken link text color
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
   * Create a property element
   *
   * @param document XML document
   * @param value Property value
   * @return property element
   */
  private org.w3c.dom.Element createPropertyElement(
      org.w3c.dom.Document document, PropertyValue value) {

    org.w3c.dom.Element elem = document.createElement("property");
    // Property key
    {
      org.w3c.dom.Attr attr = document.createAttribute("key");
      attr.setValue(value.getKey());
      elem.setAttributeNode(attr);
    }
    // Property name
    {
      org.w3c.dom.Attr attr = document.createAttribute("name");
      attr.setValue(value.getName());
      elem.setAttributeNode(attr);
    }
    // Property type
    {
      org.w3c.dom.Attr attr = document.createAttribute("type");
      attr.setValue(value.getType());
      elem.setAttributeNode(attr);
    }
    // Message
    {
      org.w3c.dom.Attr attr = document.createAttribute("message");
      attr.setValue(value.getMessage());
      elem.setAttributeNode(attr);
    }

    return elem;
  }

  /**
   * Get the source property list.
   *
   * @return Source property list
   */
  public PropertyValue[] getPropertyValues() {
    return this.listProperty.toArray(new PropertyValue[0]);
  }
}
