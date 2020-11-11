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
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import jp.riken.kscope.Message;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * Profiler property class
 *
 * @author RIKEN
 */
public class ProfilerProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /* Property key */
  /** Maximum number of cost information displayed */
  private final String KEY_COSTINFO_MAXCOUNT = "costinfo-maxcount";
  /** Cost information display color: Procedure */
  private final String KEY_COSTINFO_BARCOLOR_PROCEDURE = "costinfo-barcolor-procedure";
  /** Cost information display color: Loop */
  private final String KEY_COSTINFO_BARCOLOR_LOOP = "costinfo-barcolor-loop";
  /** Cost information display color: Line */
  private final String KEY_COSTINFO_BARCOLOR_LINE = "costinfo-barcolor-line";
  /** Cost ruler: Minimum color */
  private final String KEY_RULER_COLOR_MIN = "costruler-color-minimum";
  /** Cost Ruler: Maximum Color */
  private final String KEY_RULER_COLOR_MAX = "costruler-color-maximum";
  /** Cost ruler: Code frame color */
  private final String KEY_RULER_BORDERCOLOR_PANEL = "costruler-bordercolor-panel";
  /** Cost ruler: Code background color */
  private final String KEY_RULER_BACKCOLOR_PANEL = "costruler-backcolor-panel";
  /** Measurement interval statement: Start function name */
  private final String KEY_EPROF_FUNCTION_START = "eprof-function-start";
  /** Measurement interval statement: End function name */
  private final String KEY_EPROF_FUNCTION_END = "eprof-function-end";
  /** Measurement interval statement: Start statement */
  private final String KEY_EPROF_STATEMENT_START = "eprof-statement-start";
  /** Measurement interval statement: End statement */
  private final String KEY_EPROF_STATEMENT_END = "eprof-statement-end";
  /** Maximum number of cost information displayed: Default = 0 */
  private final int DEFAULT_COSTINFO_MAXNO = 0;
  /** Profiler cost bar graph display: Initial state */
  public static boolean INITIALIZE_VISIBLE_BARGRAPH = false;
  /** Profiler cost bar graph display: Initial state false (= hidden) */
  private boolean visibleBargraph = INITIALIZE_VISIBLE_BARGRAPH;
  /** Profiler cost ruler display: Initial state */
  public static boolean INITIALIZE_VISIBLE_RULER = false;
  /** Profiler cost ruler display: Initial state false (= hidden) */
  private boolean visibleRuler = INITIALIZE_VISIBLE_RULER;
  /** Eprof measurement interval macro: function name */
  private final String MACRO_EPROF_STATEMENT_FUNCTION = "%FUNCTION";
  /** Eprof measurement interval macro: group name */
  private final String MACRO_EPROF_STATEMENT_NAME = "%NAME";
  /** Eprof measurement interval macro: detail number */
  private final String MACRO_EPROF_STATEMENT_NUMBER = "%NUMBER";
  /** Eprof Measurement Interval Macro: Priority Level */
  private final String MACRO_EPROF_STATEMENT_LEVEL = "%LEVEL";
  /** Cost display digits (number of digits after the decimal point) */
  public static final int COST_RATIO_SCALE = 2;
  /** Cost ruler: Minimum color */
  private static final Color RULER_MIN_DEFAULTCOLOR = Color.BLUE;
  /** Cost ruler: Maximum color */
  private static final Color RULER_MAX_DEFAULTCOLOR = Color.RED;
  /** Cost ruler: Code frame color */
  private static final Color RULER_PANEL_DEFAULTBORDERCOLOR = Color.GRAY;
  /** Cost ruler: Code background color */
  private static final Color RULER_PANEL_DEFAULTBACKCOLOR = new Color(220, 220, 220, 64);

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public ProfilerProperties() throws Exception {
    loadProperties();
  }

  /**
   * Copy constructor
   *
   * @param properties Profiler properties
   * @throws Exception Property read error
   */
  public ProfilerProperties(ProfilerProperties properties) throws Exception {
    // Copy menu display items
    if (properties != null) {
      this.visibleBargraph = properties.visibleBargraph;
      this.visibleRuler = properties.visibleRuler;
    }
    loadProperties();
  }

  /**
   * Read profiler properties from the default configuration file.
   *
   * @throws Exception Property read error
   */
  public void loadProperties() throws Exception {

    // Read resource file
    InputStream stream = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
    // Read profiler properties from the config file.
    loadProperties(stream);
  }

  /**
   * Read profiler properties from the config file.
   *
   * @param propertiesFile Profiler property settings file
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

    // Parsing the XML file
    XmlUtils xml = new XmlUtils(stream);

    // Maximum number of cost information displayed
    {
      String key = KEY_COSTINFO_MAXCOUNT;
      int value = xml.getInt("//settings/profiler[@key='" + key + "']/@value");
      if (value < 0) {
        value = getCostinfoMaxCount();
      }
      if (value < 0) {
        value = DEFAULT_COSTINFO_MAXNO;
      }
      this.setCostinfoMaxCount(value);
    }
    // Cost information display color: Procedure
    {
      String key = KEY_COSTINFO_BARCOLOR_PROCEDURE;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = getCostinfoBarcolorProcedure();
      }
      this.setCostinfoBarcolorProcedure(value);
    }
    // Cost information display color: Loop
    {
      String key = KEY_COSTINFO_BARCOLOR_LOOP;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = this.getCostinfoBarcolorLoop();
      }
      this.setCostinfoBarcolorLoop(value);
    }
    // Cost information display color: Line
    {
      String key = KEY_COSTINFO_BARCOLOR_LINE;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = this.getCostinfoBarcolorLine();
      }
      this.setCostinfoBarcolorLine(value);
    }
    // Measurement interval: Start function name
    {
      String key = KEY_EPROF_FUNCTION_START;
      String value = xml.getString("//settings/profiler[@key='" + key + "']/@value");
      if (value == null || value.isEmpty()) {
        value = this.getEprofFunctionStart();
      }
      this.setEprofFunctionStart(value);
    }
    // Measurement interval: End function name
    {
      String key = KEY_EPROF_FUNCTION_END;
      String value = xml.getString("//settings/profiler[@key='" + key + "']/@value");
      if (value == null || value.isEmpty()) {
        value = this.getEprofFunctionEnd();
      }
      this.setEprofFunctionEnd(value);
    }
    // Measurement interval: Start statement
    {
      String key = KEY_EPROF_STATEMENT_START;
      String value = xml.getString("//settings/profiler[@key='" + key + "']/text()");
      if (value == null || value.isEmpty()) {
        value = this.getEprofStatementStart();
      }
      this.setEprofStatementStart(value);
    }
    // Measurement interval: End statement
    {
      String key = KEY_EPROF_STATEMENT_END;
      String value = xml.getString("//settings/profiler[@key='" + key + "']/text()");
      if (value == null || value.isEmpty()) {
        value = this.getEprofStatementEnd();
      }
      this.setEprofStatementEnd(value);
    }
    // Cost ruler: Minimum color
    {
      String key = KEY_RULER_COLOR_MIN;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = getRulerColorMin();
      }
      if (value == null) {
        value = RULER_MIN_DEFAULTCOLOR;
      }
      this.setRulerColorMin(value);
    }
    // Cost ruler: Maximum color
    {
      String key = KEY_RULER_COLOR_MAX;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = getRulerColorMax();
      }
      if (value == null) {
        value = RULER_MAX_DEFAULTCOLOR;
      }
      this.setRulerColorMax(value);
    }
    // Cost ruler: Code frame color
    {
      String key = KEY_RULER_BORDERCOLOR_PANEL;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = getRulerPanelBorderColor();
      }
      if (value == null) {
        value = RULER_PANEL_DEFAULTBORDERCOLOR;
      }
      this.setRulerPanelBorderColor(value);
    }
    // Cost ruler: Code background color
    {
      String key = KEY_RULER_BACKCOLOR_PANEL;
      Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
      if (value == null) {
        value = getRulerPanelBackColor();
      }
      if (value == null) {
        value = RULER_PANEL_DEFAULTBACKCOLOR;
      }
      this.setRulerPanelBackColor(value);
    }

    return;
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
              Message.getString("profilerproperties.document.comment")); // Profiler properties
      node.appendChild(comment);
    }
    // Maximum number of cost information displayed
    {
      String key = KEY_COSTINFO_MAXCOUNT;
      int value = this.getCostinfoMaxCount();
      org.w3c.dom.Element elem = createPropertyElement(document, key);
      org.w3c.dom.Attr attrValue = document.createAttribute("value");
      attrValue.setValue(String.valueOf(value));
      elem.setAttributeNode(attrValue);
      node.appendChild(elem);
    }
    // Cost information display color: Procedure
    {
      String key = KEY_COSTINFO_BARCOLOR_PROCEDURE;
      Color value = this.getCostinfoBarcolorProcedure();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
    }
    // Cost information display color: Loop
    {
      String key = KEY_COSTINFO_BARCOLOR_LOOP;
      Color value = this.getCostinfoBarcolorLoop();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
    }
    // Cost information display color: Line
    {
      String key = KEY_COSTINFO_BARCOLOR_LINE;
      Color value = this.getCostinfoBarcolorLine();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
    }

    // Measurement interval: Start function name
    {
      String key = KEY_EPROF_FUNCTION_START;
      String value = this.getEprofFunctionStart();
      org.w3c.dom.Element elem = createPropertyElement(document, key);
      org.w3c.dom.Attr attrValue = document.createAttribute("value");
      attrValue.setValue(String.valueOf(value));
      elem.setAttributeNode(attrValue);
      node.appendChild(elem);
    }
    // Measurement interval: End function name
    {
      String key = KEY_EPROF_FUNCTION_END;
      String value = this.getEprofFunctionEnd();
      org.w3c.dom.Element elem = createPropertyElement(document, key);
      org.w3c.dom.Attr attrValue = document.createAttribute("value");
      attrValue.setValue(String.valueOf(value));
      elem.setAttributeNode(attrValue);
      node.appendChild(elem);
    }
    // Measurement interval: Start statement
    {
      String key = KEY_EPROF_STATEMENT_START;
      String value = this.getEprofStatementStart();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        elem.appendChild(document.createCDATASection(value));
        node.appendChild(elem);
      }
    }
    // Measurement interval: End statement
    {
      String key = KEY_EPROF_STATEMENT_END;
      String value = this.getEprofStatementEnd();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        elem.appendChild(document.createCDATASection(value));
        node.appendChild(elem);
      }
    }
    // Cost ruler: Minimum color
    {
      String key = KEY_RULER_COLOR_MIN;
      Color value = this.getRulerColorMin();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
    }
    // Cost ruler: Maximum color
    {
      String key = KEY_RULER_COLOR_MAX;
      Color value = this.getRulerColorMax();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
    }
    // Cost ruler: Code frame color
    {
      String key = KEY_RULER_BORDERCOLOR_PANEL;
      Color value = this.getRulerPanelBorderColor();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
    }
    // Cost ruler: Code background color
    {
      String key = KEY_RULER_BACKCOLOR_PANEL;
      Color value = this.getRulerPanelBackColor();
      if (value != null) {
        org.w3c.dom.Element elem = createPropertyElement(document, key);
        XmlUtils.createColorAttribute(elem, value);
        node.appendChild(elem);
      }
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

    org.w3c.dom.Element elem = document.createElement("profiler");
    // Property key
    {
      org.w3c.dom.Attr attr = document.createAttribute("key");
      attr.setValue(key);
      elem.setAttributeNode(attr);
    }

    return elem;
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /**
   * Get the maximum number of cost display
   *
   * @return Maximum number of cost display
   */
  public int getCostinfoMaxCount() {
    return this.getInt(KEY_COSTINFO_MAXCOUNT, DEFAULT_COSTINFO_MAXNO);
  }

  /**
   * Set the maximum number of cost display
   *
   * @param value Maximum number of cost display
   */
  public void setCostinfoMaxCount(int value) {
    this.putInt(KEY_COSTINFO_MAXCOUNT, value);
  }

  /**
   * Cost information display color: Get procedure
   *
   * @return Cost information display color: Procedure
   */
  public Color getCostinfoBarcolorProcedure() {
    Object value = this.getObject(KEY_COSTINFO_BARCOLOR_PROCEDURE);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost information display color: Set the procedure
   *
   * @param value Cost information display color: Procedure
   */
  public void setCostinfoBarcolorProcedure(Color value) {
    this.putObject(KEY_COSTINFO_BARCOLOR_PROCEDURE, value);
  }

  /**
   * Cost information display color: Get loop
   *
   * @return Cost information display color: Loop
   */
  public Color getCostinfoBarcolorLoop() {
    Object value = this.getObject(KEY_COSTINFO_BARCOLOR_LOOP);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost information display color: Set loop
   *
   * @param value Cost information display color: Loop
   */
  public void setCostinfoBarcolorLoop(Color value) {
    this.putObject(KEY_COSTINFO_BARCOLOR_LOOP, value);
  }

  /**
   * Cost information display color: Get line
   *
   * @return Cost information display color: line
   */
  public Color getCostinfoBarcolorLine() {
    Object value = this.getObject(KEY_COSTINFO_BARCOLOR_LINE);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost information display color: Set the line
   *
   * @param value Cost information display color: line
   */
  public void setCostinfoBarcolorLine(Color value) {
    this.putObject(KEY_COSTINFO_BARCOLOR_LINE, value);
  }

  /**
   * Cost ruler: Get the minimum color
   *
   * @return Cost Ruler: Minimum Color
   */
  public Color getRulerColorMin() {
    Object value = this.getObject(KEY_RULER_COLOR_MIN);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost ruler: Set minimum color
   *
   * @param value Cost ruler: Minimum color
   */
  public void setRulerColorMin(Color value) {
    this.putObject(KEY_RULER_COLOR_MIN, value);
  }

  /**
   * Cost Ruler: Get the maximum color
   *
   * @return Cost Ruler: Maximum Color
   */
  public Color getRulerColorMax() {
    Object value = this.getObject(KEY_RULER_COLOR_MAX);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost ruler: Set maximum color
   *
   * @param value Cost ruler: Maximum color
   */
  public void setRulerColorMax(Color value) {
    this.putObject(KEY_RULER_COLOR_MAX, value);
  }

  /**
   * Cost ruler: Get code panel frame color
   *
   * @return Cost ruler: Code panel frame color
   */
  public Color getRulerPanelBorderColor() {
    Object value = this.getObject(KEY_RULER_BORDERCOLOR_PANEL);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost ruler: Set the code panel frame color
   *
   * @param value Cost ruler: Code panel frame color
   */
  public void setRulerPanelBorderColor(Color value) {
    this.putObject(KEY_RULER_BORDERCOLOR_PANEL, value);
  }

  /**
   * Cost ruler: Get code panel background color
   *
   * @return Cost Ruler: Code Panel Background Color
   */
  public Color getRulerPanelBackColor() {
    Object value = this.getObject(KEY_RULER_BACKCOLOR_PANEL);
    if (value == null) return null;
    if (!(value instanceof Color)) return null;
    return (Color) value;
  }

  /**
   * Cost ruler: Set code panel background color
   *
   * @param value Cost ruler: Code panel background color
   */
  public void setRulerPanelBackColor(Color value) {
    this.putObject(KEY_RULER_BACKCOLOR_PANEL, value);
  }

  /**
   * Measurement interval: Get the start function name
   *
   * @return Measurement interval: Start function name
   */
  public String getEprofFunctionStart() {
    String value = this.getProperty(KEY_EPROF_FUNCTION_START);
    return value;
  }

  /**
   * Measurement interval: Set the start function name
   *
   * @param value Measurement interval: Start function name
   */
  public void setEprofFunctionStart(String value) {
    this.put(KEY_EPROF_FUNCTION_START, value);
  }

  /**
   * Measurement interval: Get the end function name
   *
   * @return Measurement interval: End function name
   */
  public String getEprofFunctionEnd() {
    String value = this.getProperty(KEY_EPROF_FUNCTION_END);
    return value;
  }

  /**
   * Measurement interval: Set the end function name
   *
   * @param value Measurement interval: End function name
   */
  public void setEprofFunctionEnd(String value) {
    this.put(KEY_EPROF_FUNCTION_END, value);
  }

  /**
   * Measurement interval: Get the start statement
   *
   * @return Measurement interval: Start statement
   */
  public String getEprofStatementStart() {
    String value = this.getProperty(KEY_EPROF_STATEMENT_START);
    return value;
  }

  /**
   * Measurement interval: Set the start statement
   *
   * @param value Measurement interval: Start statement
   */
  public void setEprofStatementStart(String value) {
    this.put(KEY_EPROF_STATEMENT_START, value);
  }

  /**
   * Measurement interval: Get the end statement
   *
   * @return Measurement interval: End statement
   */
  public String getEprofStatementEnd() {
    String value = this.getProperty(KEY_EPROF_STATEMENT_END);
    return value;
  }

  /**
   * Measurement interval: Set the end statement
   *
   * @param value Measurement interval: End statement
   */
  public void setEprofStatementEnd(String value) {
    this.put(KEY_EPROF_STATEMENT_END, value);
  }

  /**
   * Profiler cost bar graph display
   *
   * @return Profiler cost bar graph display
   */
  public boolean isVisibleBargraph() {
    return visibleBargraph;
  }

  /**
   * Profiler cost bar graph display
   *
   * @param visible Profiler cost bar graph display
   */
  public void setVisibleBargraph(boolean visible) {
    this.visibleBargraph = visible;
  }

  /**
   * Profiler cost ruler display
   *
   * @return Profiler cost ruler display
   */
  public boolean isVisibleRuler() {
    return visibleRuler;
  }

  /**
   * Profiler cost ruler display
   *
   * @param visible Profiler cost ruler display
   */
  public void setVisibleRuler(boolean visible) {
    this.visibleRuler = visible;
  }

  /**
   * Check if the measurement interval statement contains a group name macro
   *
   * @return true = Contains group name macro
   */
  public boolean existsMacroErofName() {
    return existsMacroErof(MACRO_EPROF_STATEMENT_NAME);
  }

  /**
   * Check if the measurement interval statement contains a detail number macro
   *
   * @return true = Contains detail number macro
   */
  public boolean existsMacroErofNumber() {
    return existsMacroErof(MACRO_EPROF_STATEMENT_NUMBER);
  }

  /**
   * Check if the measurement interval statement contains a priority level macro
   *
   * @return true = Priority level macro included
   */
  public boolean existsMacroErofLevel() {
    return existsMacroErof(MACRO_EPROF_STATEMENT_LEVEL);
  }

  /**
   * Check if the measurement interval statement contains a macro
   *
   * @return true = Contains macros
   */
  private boolean existsMacroErof(String macro) {
    boolean exists = false;
    {
      String statement = this.getEprofStatementStart();
      if (statement != null && !statement.isEmpty()) {
        exists |= (statement.indexOf(macro) >= 0);
      }
    }
    {
      String statement = this.getEprofStatementEnd();
      if (statement != null && !statement.isEmpty()) {
        exists |= (statement.indexOf(macro) >= 0);
      }
    }
    return exists;
  }

  /**
   * Measurement interval: Create a start statement
   *
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   * @return Measurement interval: Start statement
   */
  public String createEprofStatementStart(String name, String number, String level) {
    return createEprofStatement(
        this.getEprofStatementStart(), this.getEprofFunctionStart(), name, number, level);
  }

  /**
   * Measurement interval: Create an end statement
   *
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   * @return Measurement interval: End statement
   */
  public String createEprofStatementEnd(String name, String number, String level) {
    return createEprofStatement(
        this.getEprofStatementEnd(), this.getEprofFunctionEnd(), name, number, level);
  }

  /**
   * Measurement interval: Create a start statement
   *
   * @param statement Insert statement
   * @param function function name
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   * @return Measurement interval: Start statement
   */
  private String createEprofStatement(
      String statement, String function, String name, String number, String level) {
    if (statement == null || statement.isEmpty()) return null;
    String value = statement;
    // Function name
    if (function != null && !function.isEmpty()) {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_FUNCTION, function);
    } else {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_FUNCTION, "");
    }
    // group name
    if (name != null && !name.isEmpty()) {
      String rep_name = name;
      // Delete 2012/05/21 If you enclose it in double quotes, do not add double quotes.
      //            if (!(rep_name.startsWith("\"") && rep_name.endsWith("\""))) {
      //                rep_name = "\"" + rep_name + "\"";
      //            }
      value = value.replaceAll(MACRO_EPROF_STATEMENT_NAME, rep_name);
    } else {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_NAME, "");
    }
    // Detail number
    if (number != null && !number.isEmpty()) {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_NUMBER, number);
    } else {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_NUMBER, "");
    }
    // Level
    if (level != null && !level.isEmpty()) {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_LEVEL, level);
    } else {
      value = value.replaceAll(MACRO_EPROF_STATEMENT_LEVEL, "");
    }
    return value;
  }

  /**
   * Copy the profiler menu display items.
   *
   * @param properties Profiler properties
   */
  public void setVisibleProperties(ProfilerProperties properties) {
    // Copy menu display items
    if (properties != null) {
      this.visibleBargraph = properties.visibleBargraph;
      this.visibleRuler = properties.visibleRuler;
    }
  }
}
