/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.utils.ResourceUtils;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * K-scope property class Read application properties from "properties.xml". Get the application
 * property value.
 *
 * @author RIKEN
 */
public class KscopeProperties {
  public static final String APPLICATION_NAME = Message.getString("application.name"); // K-scope
  /** Application English name */
  public static final String APPLICATION_NAMEEN = Message.getString("application.name"); // K-scope
  /** Application Japanese name */
  public static final String APPLICATION_NAMEJP = Message.getString("application.name"); // K-scope

  /*** Application version
   * version: 0.2.0 2013/03/22 release
   * version: 0.2.1 2013/04/12 release
   * version: 0.2.2 2013/05/01 release
   * version: 0.2.3 2013/05/31 release
   * version: 0.2.4 2013/10/30 release
   * version: 0.2.5 2013/12/27 release
   * version: 0.2.6 2014/11/10 release
   * version: 0.2.7 2015 / XX / XX release
   */
  public static final String APPLICATION_VERSION = "0.2.7";

  /** Database file */
  public static final String DATABASE_FILE = "db.ksx";
  /** Application property file */
  public static final String PROPERTIES_FILE_DEFAULT = "properties.xml";
  /** Application property file */
  public static String PROPERTIES_FILE = null;

  /** Application Properties Folder: System Preferences */
  public static final String PROPERTIES_FOLDER = "properties";
  /** Project file */
  public static final String PROJECT_FILE = "kscope_project.ksx";
  /** Settings folder */
  public static final String SETTINGS_FOLDER = "kscope_settings";

  /** Application property table */
  private static HashMap<String, Object> m_properties = new HashMap<String, Object>();
  /** Fortran important comment key */
  private static final String PROPERTY_FORTRAN_COMMENT = "fortran_valid_comment";
  /** C language important comment key */
  private static final String PROPERTY_CLANG_COMMENT = "clang_valid_comment";

  /** Default extension setting */
  /** Fortran: Fixed format (72 digits) */
  private static final String PROPERTY_EXT_FORTRAN_FIXED_72 = "ext_fortran_fixed_72";
  /** Fortran: Fixed format (extended number of digits) */
  private static final String PROPERTY_EXT_FORTRAN_FIXED_EXT = "ext_fortran_fixed_ext";
  /** Fortran: Free format */
  private static final String PROPERTY_EXT_FORTRAN_FREE = "ext_fortran_free";
  /** C language */
  private static final String PROPERTY_EXT_CLANG = "ext_clang";
  /** XcdoeML */
  private static final String PROPERTY_EXT_XCODEML = "ext_xcodeml";
  /** Last access folder */
  private static String m_lastAccessFolder = null;
  /** Number of console queuing */
  public static int CONSOLE_QUEUESIZE = 200;
  /** Selected background color */
  public static Color SELECTION_BACKGROUND = new Color(100, 149, 237);

  /** Word break position */
  public static final String DELIMITER_CHARS = " ;:{}()[]+-/%<=>!&|^~*,";
  /** Analyze: Search, Trace Tree Search String Color */
  public static final int SEARCHTREE_FONTSTYLE = Font.PLAIN;

  /** Structure tree default filter list */
  public static final FILTER_TYPE[] LANGUGE_DEFAULTFILTERS = {
    FILTER_TYPE.PROCEDURE,
    FILTER_TYPE.PROCEDUREUSAGE,
    FILTER_TYPE.REPETITION,
    FILTER_TYPE.SELECTION_SELECT,
    FILTER_TYPE.SELECTION_IF
  };

  private KscopeProperties() {}

  /**
   * * Load the application properties file. <br>
   * Get the setting value from the application property file and set it in the application property
   * table. <br>
   * The application property file is a resource file located in the class directory. <br>
   */
  public static void loadXml() {
    try {
      // Read resource file (switch configuration file depending on locale)
      InputStream is = null;
      is = ResourceUtils.getPropertiesFile(PROPERTIES_FILE);
      if (is == null) {
        JOptionPane.showMessageDialog(
            null,
            Message.getString(
                "kscopeproperties.errdialog.cannnotopenpropertyfile"), // Could not open the
                                                                       // properties file.
            Message.getString("dialog.common.error"), // error
            JOptionPane.ERROR_MESSAGE);
      }

      // XML perspective
      DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder builder = dbfactory.newDocumentBuilder();
      Document doc = builder.parse(is);

      // Perspective of language extension
      parseExtFortranFixed72(doc);
      parseExtFortranFixedExt(doc);
      parseExtFortranFree(doc);
      parseExtCLang(doc);
      parseExtXcodeml(doc);

      // Fortran important comment parsing
      parseFortranComment(doc);
      // C ++ important comment parsing
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
   * Fortran: Get a fixed format (72 digits) extension.
   *
   * @param doc Application property file XML document
   */
  private static void parseExtFortranFixed72(Document doc) {
    try {
      XPathFactory factory = XPathFactory.newInstance();
      XPath xpath = factory.newXPath();
      XPathExpression expr = xpath.compile("//extension/fortran_fixed_72/text()");

      Object result = expr.evaluate(doc, XPathConstants.NODESET);
      NodeList nodes = (NodeList) result;
      if (nodes == null || nodes.getLength() <= 0) return;

      // Add to the application property table.
      m_properties.put(PROPERTY_EXT_FORTRAN_FIXED_72, nodes.item(0).getNodeValue());

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Fortran: Get a fixed format (extended number of digits) extension.
   *
   * @param doc Application property file XML document
   */
  private static void parseExtFortranFixedExt(Document doc) {
    try {
      XPathFactory factory = XPathFactory.newInstance();
      XPath xpath = factory.newXPath();
      XPathExpression expr = xpath.compile("//extension/fortran_fixed_ext/text()");

      Object result = expr.evaluate(doc, XPathConstants.NODESET);
      NodeList nodes = (NodeList) result;
      if (nodes == null || nodes.getLength() <= 0) return;

      // Add to the application property table.
      m_properties.put(PROPERTY_EXT_FORTRAN_FIXED_EXT, nodes.item(0).getNodeValue());

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Fortran: Get the free-form extension.
   *
   * @param doc Application property file XML document
   */
  private static void parseExtFortranFree(Document doc) {
    try {
      XPathFactory factory = XPathFactory.newInstance();
      XPath xpath = factory.newXPath();
      XPathExpression expr = xpath.compile("//extension/fortran_free/text()");

      Object result = expr.evaluate(doc, XPathConstants.NODESET);
      NodeList nodes = (NodeList) result;
      if (nodes == null || nodes.getLength() <= 0) return;

      // Add to the application property table.
      m_properties.put(PROPERTY_EXT_FORTRAN_FREE, nodes.item(0).getNodeValue());

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Get the C language format extension.
   *
   * @param doc Application property file XML document
   */
  private static void parseExtCLang(Document doc) {
    try {
      XPathFactory factory = XPathFactory.newInstance();
      XPath xpath = factory.newXPath();
      XPathExpression expr = xpath.compile("//extension/clang/text()");

      Object result = expr.evaluate(doc, XPathConstants.NODESET);
      NodeList nodes = (NodeList) result;
      if (nodes == null || nodes.getLength() <= 0) return;

      // Add to the application property table.
      m_properties.put(PROPERTY_EXT_CLANG, nodes.item(0).getNodeValue());

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Get the XcodeML extension.
   *
   * @param doc Application property file XML document
   */
  private static void parseExtXcodeml(Document doc) {
    try {
      XPathFactory factory = XPathFactory.newInstance();
      XPath xpath = factory.newXPath();
      XPathExpression expr = xpath.compile("//extension/xcodeml/text()");

      Object result = expr.evaluate(doc, XPathConstants.NODESET);
      NodeList nodes = (NodeList) result;
      if (nodes == null || nodes.getLength() <= 0) return;

      // Add to the application property table.
      m_properties.put(PROPERTY_EXT_XCODEML, nodes.item(0).getNodeValue());

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Fortran Get important comments from the application properties file.
   *
   * @param doc Application property file XML document
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

      // Add to the application property table.
      m_properties.put(PROPERTY_FORTRAN_COMMENT, list);

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Get C language important comments from the application properties file.
   *
   * @param doc Application property file XML document
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

      // Add to the application property table.
      m_properties.put(PROPERTY_CLANG_COMMENT, list);

    } catch (XPathExpressionException e) {
      e.printStackTrace();
    } catch (DOMException e) {
      e.printStackTrace();
    }
  }

  /**
   * Fortran Get important comments.
   *
   * @return Fortran Important Comments
   */
  public static String[] getFortranValidComment() {
    return (String[]) m_properties.get(PROPERTY_FORTRAN_COMMENT);
  }

  /**
   * C language Get important comments.
   *
   * @return C language
   */
  public static String[] getClangValidComment() {
    return (String[]) m_properties.get(PROPERTY_CLANG_COMMENT);
  }

  /**
   * Fortran: Get a fixed format (72 digits) extension. To get.
   *
   * @return Fortran: Fixed format (72 digits) extension
   */
  public static String[] getExtFortranFixed72() {
    String exts = (String) m_properties.get(PROPERTY_EXT_FORTRAN_FIXED_72);
    if (exts == null || exts.length() <= 0) {
      return null;
    }
    return exts.split(",");
  }

  /**
   * Fortran: Get a fixed format (extended number of digits) extension.
   *
   * @return Fortran: Fixed format (extended number of digits) extension
   */
  public static String[] getExtFortranFixedExt() {
    String exts = (String) m_properties.get(PROPERTY_EXT_FORTRAN_FIXED_EXT);
    if (exts == null || exts.length() <= 0) {
      return null;
    }
    return exts.split(",");
  }

  /**
   * Fortran: Get the free-form extension.
   *
   * @return Fortran: Free format extension
   */
  public static String[] getExtFortranFree() {
    String exts = (String) m_properties.get(PROPERTY_EXT_FORTRAN_FREE);
    if (exts == null || exts.length() <= 0) {
      return null;
    }
    return exts.split(",");
  }

  /**
   * Get the C language format extension.
   *
   * @return C language format extension
   */
  public static String[] getExtCLang() {
    String exts = (String) m_properties.get(PROPERTY_EXT_CLANG);
    if (exts == null || exts.length() <= 0) {
      return null;
    }
    return exts.split(",");
  }

  /**
   * Get the XcdoeML extension.
   *
   * @return XcdoeML extension
   */
  public static String[] getExtXcodeml() {
    String exts = (String) m_properties.get(PROPERTY_EXT_XCODEML);
    if (exts == null || exts.length() <= 0) {
      return null;
    }
    return exts.split(",");
  }

  /**
   * Get the last access folder.
   *
   * @return Last access folder
   */
  public static String getLastAccessFolder() {
    if (m_lastAccessFolder == null) {
      m_lastAccessFolder = System.getProperty("user.dir");
    }
    return m_lastAccessFolder;
  }

  /**
   * Set the last access folder.
   *
   * @param folder Last access folder
   */
  public static void setLastAccessFolder(String folder) {
    m_lastAccessFolder = folder;
  }

  /**
   * Check if it's MacOS. <br>
   * Get the OS name from the system properties.
   *
   * @return true = MacOS.
   */
  public static boolean isMac() {
    String lcOSName = System.getProperty("os.name");
    if (lcOSName == null) {
      return false;
    }
    lcOSName = lcOSName.toLowerCase();
    return lcOSName.startsWith("mac os x");
  }

  /**
   * Check if it is Windows. <br>
   * Get the OS name from the system properties.
   *
   * @return true = Windows.
   */
  public static boolean isWindows() {
    String lcOSName = System.getProperty("os.name");
    if (lcOSName == null) {
      return false;
    }
    lcOSName = lcOSName.toLowerCase();
    return lcOSName.startsWith("windows");
  }

  /**
   * Check if it is Linux. <br>
   * Get the OS name from the system properties.
   *
   * @return true = Linux.
   */
  public static boolean isLinux() {
    String lcOSName = System.getProperty("os.name");
    if (lcOSName == null) {
      return false;
    }
    lcOSName = lcOSName.toLowerCase();
    return lcOSName.startsWith("linux");
  }

  /**
   * Check if Java version is 1.8 or above. <br>
   * Get java.version from system properties.
   *
   * @return true = Java version is 1.7 or higher.
   */
  public static boolean isJava18Later() {
    String version = System.getProperty("java.specification.version");
    double verno = Double.parseDouble(version);
    return verno >= 1.8;
  }

  /*
   * Check if AppleScript is available
   * @retrun true = AppleScript is available.
   */
  public static boolean isApplescript() {
    ScriptEngine scriptEngine = new ScriptEngineManager().getEngineByName("AppleScript");
    if (scriptEngine == null) {
      return false;
    } else {
      return true;
    }
  }
}
