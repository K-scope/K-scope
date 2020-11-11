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
import java.util.ArrayList;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Request Byte / FLOP configuration property
 *
 * @author RIKEN
 */
public class RequiredBFProperties extends PropertiesBase {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Memory throughput calculation mode */
  public enum MEM_THROUGHPUT_CALC_MODE {
    AUTO, /// <Automatic judgment: Default
    STORE, /// <with store
    NOSTORE /// <No store
  }

  /** Calculation unit of request BF */
  public enum BF_CALC_TYPE {
    BYTE_FLOP, /// < Byte/FLOP
    FLOP_BYTE /// < FLOP/Byte
  }

  /** Default data type size */
  public final int DEFUALT_DATASIZE = 4;
  /* Property key */
  /** Request Byte / FLOP element */
  private final String ELEM_REQUIRED_BF = "required_bf";
  /** Computational performance GFLOPS */
  private final String KEY_FLOP_PERFORMANCE = "flop-performance";
  /**
   * Memory throughput calculation mode (selection to consider store when calculating throughput)
   */
  private final String KEY_MEM_THROUGHPUT_CALC_MODE = "mem-throughput-calc_mode";
  /** Unit of request BF calculation */
  private final String KEY_BF_CALC_TYPE = "calc-type";
  /** Default size */
  private final String KEY_DEFAULT_SIZE = "default-size";
  /** Access key attribute */
  private final String ATTR_KEY = "key";
  /** Value attribute */
  private final String ATTR_VALUE = "value";
  /** Access name attribute */
  private final String ATTR_NAME = "name";
  /** Access background color */
  private final String ATTR_BACKGROUND_COLOR = "background-color";
  /** Memory Throughput: With Store */
  private final String ATTR_MEM_THROUGHPUT_STORE = "mem-throughput-store";
  /** Memory Throughput: No Store */
  private final String ATTR_MEM_THROUGHPUT_NOSTORE = "mem-throughput-nostore";
  /** Coefficient */
  private final String ATTR_COEFFICIENT = "coef";
  /** Request B / F calculation */
  private final String ATTR_REQUIRED_BF = "reqbf";
  /** rate-determining */
  private final String ATTR_LIMITS = "limits";
  /** Enabled / Disabled */
  private final String ATTR_ENABLED = "enabled";
  /** Default size: real */
  private final String ATTR_SIZE_REAL = "size-real";
  /** Default size: integer */
  private final String ATTR_SIZE_INTEGER = "size-integer";

  /** Theoretical floating point arithmetic performance */
  private float theoretical_flop_performance;
  /** Memory throughput calculation mode setting */
  private MEM_THROUGHPUT_CALC_MODE memThroughtputCalcMode;
  /** BF calculation unit */
  private BF_CALC_TYPE BFCalcType;
  /** Default size: real (byte) */
  private int defaultSizeReal;
  /** Default size: integer (byte) */
  private int defaultSizeInteger;
  /** Request Byte / FLOP setting list */
  private List<RequiredBF> listReqBF = new ArrayList<RequiredBF>();
  /** Default request Byte / FLOP setting */
  private RequiredBFProperties defaultProperties;

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public RequiredBFProperties() throws Exception {
    // Default value
    this.memThroughtputCalcMode = MEM_THROUGHPUT_CALC_MODE.AUTO;
    this.BFCalcType = BF_CALC_TYPE.BYTE_FLOP;
    this.defaultSizeReal = DEFUALT_DATASIZE;
    this.defaultSizeInteger = DEFUALT_DATASIZE;
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
   * Read the request Byte / FLOP configuration property from the configuration file.
   *
   * @param propertiesFile Request Byte / FLOP configuration property configuration file
   * @throws Exception Property read error
   */
  public void loadProperties(File propertiesFile) throws Exception {

    if (!propertiesFile.exists()) {
      throw (new Exception(
          Message.getString(
              "propertiesbase.exeption.notexist"))); // Request Byte / FLOP property file does not
      // exist.
    }

    // Read resource file
    InputStream stream = new FileInputStream(propertiesFile);

    // Parsing the XML file
    loadProperties(stream);
  }

  /**
   * Read the request Byte / FLOP configuration property from the configuration file.
   *
   * @param stream Configuration file stream
   * @throws Exception Property read error
   */
  public void loadProperties(InputStream stream) throws Exception {
    // Parsing the XML file
    List<RequiredBF> list = parseRequiredBF(stream, "//" + ELEM_REQUIRED_BF);
    if (list != null && list.size() > 0) {
      this.listReqBF = list;
    }
  }

  /**
   * Get the request Byte / FLOP setting
   *
   * @param stream XML input stream
   * @param path Request Byte / FLOP setting XPATH
   * @throws Exception Request Byte / FLOP setting parsing error
   */
  public List<RequiredBF> parseRequiredBF(InputStream stream, String path) throws Exception {

    List<RequiredBF> list = new ArrayList<RequiredBF>();

    // XML
    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = dbfactory.newDocumentBuilder();
    org.w3c.dom.Document document = builder.parse(stream);

    XPathFactory factory = XPathFactory.newInstance();
    XPath xpath = factory.newXPath();

    // Get the request BF element
    XPathExpression expr = xpath.compile(path);

    Object result = expr.evaluate(document, XPathConstants.NODESET);

    NodeList nodelist = (NodeList) result;
    if (nodelist == null) return null;

    for (int i = 0; i < nodelist.getLength(); i++) {
      try {
        Node node = nodelist.item(i);
        RequiredBF reqbf = null;

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode;
        String value;
        // Key
        attrnode = attrs.getNamedItem(ATTR_KEY);
        if (attrnode == null) continue;
        String key = attrnode.getNodeValue();
        // Floating point arithmetic performance
        if (KEY_FLOP_PERFORMANCE.equalsIgnoreCase(key)) {
          attrnode = attrs.getNamedItem(ATTR_VALUE);
          value = attrnode.getNodeValue();
          if (StringUtils.isFloat(value)) {
            this.setFlopPerformance(Float.parseFloat(value));
          }
          continue;
        }
        // Memory throughput calculation mode
        else if (KEY_MEM_THROUGHPUT_CALC_MODE.equalsIgnoreCase(key)) {
          attrnode = attrs.getNamedItem(ATTR_VALUE);
          value = attrnode.getNodeValue();
          try {
            this.setMemThroughputCalcMode(MEM_THROUGHPUT_CALC_MODE.valueOf(value.toUpperCase()));
          } catch (Exception ex) {
            ex.printStackTrace();
          }
          continue;
        }
        // BF calculation unit
        else if (KEY_BF_CALC_TYPE.equalsIgnoreCase(key)) {
          attrnode = attrs.getNamedItem(ATTR_VALUE);
          value = attrnode.getNodeValue();
          try {
            this.setCalcType(BF_CALC_TYPE.valueOf(value.toUpperCase()));
          } catch (Exception ex) {
            ex.printStackTrace();
          }
          continue;
        }
        // Default size
        else if (KEY_DEFAULT_SIZE.equalsIgnoreCase(key)) {
          // real
          attrnode = attrs.getNamedItem(ATTR_SIZE_REAL);
          value = attrnode.getNodeValue();
          if (StringUtils.isNumeric(value)) {
            this.setDefaultSizeReal(Integer.parseInt(value));
          }
          // integer
          attrnode = attrs.getNamedItem(ATTR_SIZE_INTEGER);
          value = attrnode.getNodeValue();
          if (StringUtils.isNumeric(value)) {
            this.setDefaultSizeInteger(Integer.parseInt(value));
          }
          continue;
        }
        // Memory
        else if (ACCESSMEMORY_TYPE.MEMORY.getKey().equalsIgnoreCase(key)) {
          reqbf = new RequiredBF(ACCESSMEMORY_TYPE.MEMORY);
        }
        // L1 cache
        else if (ACCESSMEMORY_TYPE.L1_CACHE.getKey().equalsIgnoreCase(key)) {
          reqbf = new RequiredBF(ACCESSMEMORY_TYPE.L1_CACHE);
        }
        // L2 cache
        else if (ACCESSMEMORY_TYPE.L2_CACHE.getKey().equalsIgnoreCase(key)) {
          reqbf = new RequiredBF(ACCESSMEMORY_TYPE.L2_CACHE);
        }
        // Register
        else if (ACCESSMEMORY_TYPE.REGISTER.getKey().equalsIgnoreCase(key)) {
          reqbf = new RequiredBF(ACCESSMEMORY_TYPE.REGISTER);
        }
        // custom
        else if (ACCESSMEMORY_TYPE.CUSTOM.getKey().equalsIgnoreCase(key)) {
          reqbf = new RequiredBF(ACCESSMEMORY_TYPE.CUSTOM);
        }
        if (reqbf == null) continue;

        // Access name
        attrnode = attrs.getNamedItem(ATTR_NAME);
        if (attrnode != null) {
          String res = attrnode.getNodeValue();
          reqbf.setName(res);
        }

        // Access background color
        attrnode = attrs.getNamedItem(ATTR_BACKGROUND_COLOR);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          Color res = StringUtils.parseColor(value);
          reqbf.setBackColor(res);
        }

        // Memory Throughput: With Store
        attrnode = attrs.getNamedItem(ATTR_MEM_THROUGHPUT_STORE);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          if (StringUtils.isFloat(value)) {
            reqbf.setMemThroughputStore(Float.parseFloat(value));
          }
        }

        // Memory Throughput: No Store
        attrnode = attrs.getNamedItem(ATTR_MEM_THROUGHPUT_NOSTORE);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          if (StringUtils.isFloat(value)) {
            reqbf.setMemThroughputNostore(Float.parseFloat(value));
          }
        }

        // Coefficient
        attrnode = attrs.getNamedItem(ATTR_COEFFICIENT);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          if (StringUtils.isFloat(value)) {
            reqbf.setCoef(Float.parseFloat(value));
          }
        }

        // Request B / F calculation flag
        attrnode = attrs.getNamedItem(ATTR_REQUIRED_BF);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          boolean res = Boolean.parseBoolean(value);
          reqbf.setRequiredBF(res);
        }

        // rate-determining flag
        attrnode = attrs.getNamedItem(ATTR_LIMITS);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          boolean res = Boolean.parseBoolean(value);
          reqbf.setLimiting(res);
        }

        // Valid / Invalid
        attrnode = attrs.getNamedItem(ATTR_ENABLED);
        if (attrnode != null) {
          value = attrnode.getNodeValue();
          boolean res = Boolean.parseBoolean(value);
          reqbf.setEnabled(res);
        }

        list.add(reqbf);

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
                  "requiredbfproperties.document.comment")); // Request Byte / FLOP property
      node.appendChild(comment);
    }

    if (this.listReqBF == null || this.listReqBF.size() <= 0) return;

    // Floating point arithmetic performance GFLOPS
    {
      org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
      org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
      attrKey.setValue(KEY_FLOP_PERFORMANCE);
      elem.setAttributeNode(attrKey);

      float value = this.getFlopPerformance();
      org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
      attrValue.setNodeValue(String.valueOf(value));
      elem.setAttributeNode(attrValue);

      // Add node
      node.appendChild(elem);
    }
    // Memory throughput calculation mode
    {
      org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
      org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
      attrKey.setValue(KEY_MEM_THROUGHPUT_CALC_MODE);
      elem.setAttributeNode(attrKey);

      MEM_THROUGHPUT_CALC_MODE value = this.getMemThroughputCalcMode();
      org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
      attrValue.setNodeValue(value.toString().toLowerCase());
      elem.setAttributeNode(attrValue);

      // Add node
      node.appendChild(elem);
    }
    // Calculation unit of request BF
    {
      org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
      org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
      attrKey.setValue(KEY_BF_CALC_TYPE);
      elem.setAttributeNode(attrKey);

      BF_CALC_TYPE value = this.getBFCalcType();
      org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
      attrValue.setNodeValue(value.toString().toLowerCase());
      elem.setAttributeNode(attrValue);

      // Add node
      node.appendChild(elem);
    }
    // Default size
    {
      org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
      org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
      attrKey.setValue(KEY_DEFAULT_SIZE);
      elem.setAttributeNode(attrKey);
      // real
      int real = this.getDefaultSizeReal();
      org.w3c.dom.Attr attrReal = document.createAttribute(ATTR_SIZE_REAL);
      attrReal.setNodeValue(String.valueOf(real));
      elem.setAttributeNode(attrReal);
      // integer
      int integer = this.getDefaultSizeInteger();
      org.w3c.dom.Attr attrInteger = document.createAttribute(ATTR_SIZE_INTEGER);
      attrInteger.setNodeValue(String.valueOf(integer));
      elem.setAttributeNode(attrInteger);

      // Add node
      node.appendChild(elem);
    }
    // Request Byte / FLOP setting
    for (RequiredBF bind : this.listReqBF) {
      org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);

      // key name
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute(ATTR_KEY);
        attr.setValue(bind.getType().getKey());
        elem.setAttributeNode(attr);
      }
      // Access name
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute(ATTR_NAME);
        attr.setValue(bind.getName());
        elem.setAttributeNode(attr);
      }
      // Access background color
      {
        Color color = bind.getBackColor();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_BACKGROUND_COLOR);
        attr.setNodeValue(StringUtils.parseColorCode(color));
        elem.setAttributeNode(attr);
      }

      // Memory throughput calculation mode: with store
      {
        float value = bind.getMemThroughputStore();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_MEM_THROUGHPUT_STORE);
        attr.setNodeValue(String.valueOf(value));
        elem.setAttributeNode(attr);
      }
      // Memory throughput calculation mode: No store
      {
        float value = bind.getMemThroughputNostore();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_MEM_THROUGHPUT_NOSTORE);
        attr.setNodeValue(String.valueOf(value));
        elem.setAttributeNode(attr);
      }
      // Coefficient
      {
        float value = bind.getCoef();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_COEFFICIENT);
        attr.setNodeValue(String.valueOf(value));
        elem.setAttributeNode(attr);
      }
      // Request B / F calculation (valid / invalid)
      {
        boolean value = bind.isRequiredBF();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_REQUIRED_BF);
        attr.setNodeValue(String.valueOf(value));
        elem.setAttributeNode(attr);
      }
      // rate-determining
      {
        boolean value = bind.isLimiting();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_LIMITS);
        attr.setNodeValue(String.valueOf(value));
        elem.setAttributeNode(attr);
      }
      // Enabled / disabled
      {
        boolean value = bind.isEnabled();
        org.w3c.dom.Attr attr = document.createAttribute(ATTR_ENABLED);
        attr.setNodeValue(String.valueOf(value));
        elem.setAttributeNode(attr);
      }

      // Add node
      node.appendChild(elem);
    }
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /**
   * Get the request Byte / FLOP setting list.
   *
   * @return Request Byte / FLOP setting list
   */
  public List<RequiredBF> getListRequiredBF() {
    return this.listReqBF;
  }

  /**
   * Get the number of requested Byte / FLOP settings.
   *
   * @return Request Byte / FLOP setting number
   */
  public int getRequiredBFCount() {
    if (listReqBF == null || listReqBF.size() <= 0) {
      return 0;
    }
    return listReqBF.size();
  }

  /**
   * Get the request Byte / FLOP setting.
   *
   * @param index index
   * @return Request Byte / FLOP setting
   */
  public RequiredBF getRequiredBF(int index) {
    if (listReqBF == null || listReqBF.size() <= 0) {
      return null;
    }
    if (listReqBF.size() <= index) {
      return null;
    }
    return listReqBF.get(index);
  }

  /**
   * Set the request Byte / FLOP setting.
   *
   * @param index index
   * @param keyword Request Byte / FLOP setting
   */
  public void setRequiredBF(int index, RequiredBF reqbf) {
    if (listReqBF == null || listReqBF.size() <= 0) {
      return;
    }
    if (listReqBF.size() <= index) {
      return;
    }
    listReqBF.set(index, reqbf);
  }

  /**
   * Add request Byte / FLOP settings.
   *
   * @param keyword Request Byte / FLOP setting
   */
  public void addRequiredBF(RequiredBF reqbf) {
    if (listReqBF == null) {
      listReqBF = new ArrayList<RequiredBF>();
    }
    listReqBF.add(reqbf);
  }

  /**
   * Delete the request Byte / FLOP setting.
   *
   * @param keyword Request Byte / FLOP setting
   */
  public void removeRequiredBF(RequiredBF reqbf) {
    if (listReqBF == null) return;
    listReqBF.remove(reqbf);
  }

  /**
   * Delete the request Byte / FLOP setting.
   *
   * @param index index
   */
  public void removeRequiredBF(int index) {
    if (listReqBF == null) return;
    listReqBF.remove(index);
  }

  /** Clear the request Byte / FLOP setting list. */
  public void clearRequiredBF() {
    listReqBF = new ArrayList<RequiredBF>();
  }

  /**
   * Get the request Byte / FLOP setting
   *
   * @param type Request Byte / FLOP type
   * @return Keyword information
   */
  public RequiredBF getRequiredBF(ACCESSMEMORY_TYPE type) {
    if (type == null) return null;

    for (RequiredBF reqbf : listReqBF) {
      ACCESSMEMORY_TYPE srctype = reqbf.getType();
      if (srctype == type) {
        return reqbf;
      }
    }

    return null;
  }

  /**
   * Get theoretical floating point arithmetic performance
   *
   * @return Computation performance
   */
  public float getFlopPerformance() {
    return this.theoretical_flop_performance;
  }

  /**
   * Set theoretical floating point arithmetic performance.
   *
   * @param performance Computational performance
   */
  public void setFlopPerformance(float performance) {
    this.theoretical_flop_performance = performance;
  }

  /**
   * Get the default request Byte / FLOP setting.
   *
   * @return Default request Byte / FLOP setting
   */
  public RequiredBFProperties getDefaultProperties() {
    return this.defaultProperties;
  }

  /**
   * Set the default request Byte / FLOP setting.
   *
   * @return Default request Byte / FLOP setting
   */
  public void setDefaultProperties(RequiredBFProperties properties) {
    this.defaultProperties = properties;
  }

  /**
   * Get the throughput calculation mode setting.
   *
   * @return Throughput calculation mode setting
   */
  public MEM_THROUGHPUT_CALC_MODE getMemThroughputCalcMode() {
    return memThroughtputCalcMode;
  }

  /**
   * Set the throughput calculation mode
   *
   * @param memThroughputCalcMode Throughput calculation mode setting
   */
  public void setMemThroughputCalcMode(MEM_THROUGHPUT_CALC_MODE memThroughputCalcMode) {
    this.memThroughtputCalcMode = memThroughputCalcMode;
  }

  /**
   * Get the unit of request BF calculation.
   *
   * @return Calculation unit
   */
  public BF_CALC_TYPE getBFCalcType() {
    return BFCalcType;
  }

  /**
   * Set the unit for calculating the request BF.
   *
   * @param BFCalcType Calculation unit
   */
  public void setCalcType(BF_CALC_TYPE BFCalcType) {
    this.BFCalcType = BFCalcType;
  }

  /**
   * Default size: Get real.
   *
   * @return Default size: real
   */
  public int getDefaultSizeReal() {
    return this.defaultSizeReal;
  }

  /**
   * Default size: Set real.
   *
   * @param size Default size: real
   */
  public void setDefaultSizeReal(int size) {
    this.defaultSizeReal = size;
  }

  /**
   * Default size: Get integer.
   *
   * @return Default size: integer
   */
  public int getDefaultSizeInteger() {
    return this.defaultSizeInteger;
  }

  /**
   * Default size: Set integer.
   *
   * @param size Default size: integer
   */
  public void setDefaultSizeInteger(int size) {
    this.defaultSizeInteger = size;
  }

  /**
   * Throughput calculation mode: Get with store.
   *
   * @return Throughput: With store
   */
  public float getMemThroughputStore() {
    int count = getRequiredBFCount();
    float value = 0.0F;
    for (int i = 0; i < count; i++) {
      RequiredBF mem = getRequiredBF(i);
      float throughput = mem.getMemThroughputStore();
      float coef = mem.getCoef();
      value += throughput * coef;
    }
    return value;
  }

  /**
   * Throughput calculation mode: Get with store.
   *
   * @return Throughput: With store
   */
  public float getMemThroughputNostore() {
    int count = getRequiredBFCount();
    float value = 0.0F;
    for (int i = 0; i < count; i++) {
      RequiredBF mem = getRequiredBF(i);
      float throughput = mem.getMemThroughputNostore();
      float coef = mem.getCoef();
      value += throughput * coef;
    }
    return value;
  }
}
