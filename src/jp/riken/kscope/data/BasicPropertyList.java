package jp.riken.kscope.data;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class BasicPropertyList {

  private List<BasicProperty> list = new ArrayList<BasicProperty>();

  /**
   * Reaplace FileInputStream with WatchedStream
   *
   * @param stream
   * @param path
   * @throws XPathExpressionException
   * @throws ParserConfigurationException
   * @throws SAXException
   * @throws IOException
   */
  public BasicPropertyList(InputStream stream, String path)
      throws XPathExpressionException, ParserConfigurationException, SAXException, IOException {

    // XML
    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = dbfactory.newDocumentBuilder();
    org.w3c.dom.Document document = builder.parse(stream);

    XPathFactory factory = XPathFactory.newInstance();
    XPath xpath = factory.newXPath();

    XPathExpression expr = xpath.compile(path);

    Object result = expr.evaluate(document, XPathConstants.NODESET);

    NodeList nodelist = (NodeList) result;
    if (nodelist == null) return;

    for (int i = 0; i < nodelist.getLength(); i++) {
      try {
        Node node = nodelist.item(i);

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode_key, attrnode_value;
        String key = null;
        String value = null;
        // Property name
        attrnode_key = attrs.getNamedItem("key");
        if (attrnode_key != null) {
          key = attrnode_key.getNodeValue();
        }
        // Property value
        attrnode_value = attrs.getNamedItem("value");
        if (attrnode_value != null) {
          value = attrnode_value.getNodeValue();
        }
        if (key != null || value != null) {
          BasicProperty property = new BasicProperty(key, value);
          this.list.add(property);
        }
      } catch (Exception ex) {
        ex.printStackTrace();
      }
    }
  }

  /**
   * Get the value of the Property that matches the search Key
   *
   * @param key Property key (name)
   * @return Property value
   */
  public String getPropertyValue(String key) {
    if (key == null || key.isEmpty()) return null;

    for (BasicProperty property : this.list) {
      String k = property.getKey();
      if (k.equalsIgnoreCase(key)) {
        return property.getValue();
      }
    }
    return null;
  }

  /**
   * Get array of keys from the list
   *
   * @return Array of keys as a String[]
   */
  public String[] getKeys() {
    List<String> keys_list = new ArrayList<String>();
    for (BasicProperty bp : this.list) {
      keys_list.add(bp.getKey());
    }
    return keys_list.toArray(new String[keys_list.size()]);
  }

  /**
   * Get array of key:value pairs from the list
   *
   * @return Array of pairs key:value as a String[]
   */
  public String[] getPairs() {
    List<String> keys_list = new ArrayList<String>();
    for (BasicProperty bp : this.list) {
      keys_list.add(bp.getKey() + ":" + bp.getValue());
    }
    return keys_list.toArray(new String[keys_list.size()]);
  }

  /**
   * Set value of property with the given key. If property with the given key not found in the list,
   * add new property.
   *
   * @param key
   * @param value
   */
  public void setProperty(String key, String value) {
    for (BasicProperty bp : this.list) {
      String bp_key = bp.getKey();
      if (bp_key.equalsIgnoreCase(key)) {
        bp.setValue(value);
        return;
      }
    }
    this.list.add(new BasicProperty(key, value));
  }

  /**
   * Class for storing pair of Strings: key & value.
   *
   * @author peterbryzgalov
   */
  private class BasicProperty {
    private String key;
    private String value;

    public BasicProperty(String key, String value) {
      this.key = key;
      this.value = value;
    }

    public String getKey() {
      return this.key;
    }

    public String getValue() {
      return this.value;
    }

    public void setValue(String value) {
      this.value = value;
    }
  }
}
