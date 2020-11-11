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
import jp.riken.kscope.Message;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * Application property class
 *
 * @author RIKEN
 */
public class ApplicationProperties extends PropertiesBase {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  // Property key
  /** Auto-save properties of the project after creating a new project */
  private final String NEWPROJECT_SAVE = "newproject_save";
  /** Exclude file presence / absence property for exporting source file */
  private final String EXPORTSOURCE_EXCLUDE = "exportsource_exclude";

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public ApplicationProperties() throws Exception {
    loadProperties();
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
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
              "propertiesbase.exeption.notexist"))); // The property file does not exist.
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
    XmlUtils xml = new XmlUtils(stream);

    // Save immediately after creating the project
    {
      key = NEWPROJECT_SAVE;
      boolean b = false;
      String val = xml.getString("//settings/application[@key='" + key + "']/@value");
      if (!StringUtils.isNullOrEmpty(val)) {
        if (val.equalsIgnoreCase("true")) {
          b = true;
        }
      }
      this.putBoolean(key, b);
    }
    // Source file export exclusion file pattern
    {
      key = EXPORTSOURCE_EXCLUDE;
      String val = xml.getString("//settings/application[@key='" + key + "']/@value");
      if (StringUtils.isNullOrEmpty(val)) {
        val = "";
      }
      this.put(key, val);
    }
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
              Message.getString(
                  "applicationproperties.document.comment")); // Application properties
      node.appendChild(comment);
    }
    // Whether to save the project immediately after creating a new project
    {
      String key = NEWPROJECT_SAVE;
      boolean value = this.getBoolean(key, false);
      org.w3c.dom.Element elem = createPropertyElement(document, key);
      org.w3c.dom.Attr attrValue = document.createAttribute("value");
      attrValue.setValue(String.valueOf(value));
      elem.setAttributeNode(attrValue);
      node.appendChild(elem);
    }
  }

  /**
   * Create a property element
   *
   * @param document XML document
   * @param key key attribute value
   * @return property element
   */
  private org.w3c.dom.Element createPropertyElement(org.w3c.dom.Document document, String key) {

    org.w3c.dom.Element elem = document.createElement("application");
    // Property key
    {
      org.w3c.dom.Attr attr = document.createAttribute("key");
      attr.setValue(key);
      elem.setAttributeNode(attr);
    }

    return elem;
  }

  /**
   * Get whether to save the project after creating a new one
   *
   * @return true = save
   */
  public boolean getSaveProjectAfterCreate() {
    return this.getBoolean(NEWPROJECT_SAVE, false);
  }

  /**
   * Get exclusion file pattern string for source file export
   *
   * @return exclude
   */
  public String getSourceExportExclude() {
    return this.get(EXPORTSOURCE_EXCLUDE, "");
  }
}
