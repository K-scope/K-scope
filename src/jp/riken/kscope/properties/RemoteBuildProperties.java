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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.RemoteBuildData;
import jp.riken.kscope.utils.ResourceUtils;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.yaml.snakeyaml.*;

/** @author Peter Bryzgalov */
public class RemoteBuildProperties extends PropertiesBase {
  /**
   * These files are necessary for building source code on remote server by the corresponding
   * program. If these files are present in the current directory (with kscope.jar), we set flags
   * haveDockerIaaS and haveSSHconnect to TRUE.
   */
  private static Boolean debug = (System.getenv("DEBUG") != null);

  private static boolean debug_l2 = false;

  public static String REMOTE_UTILS_DIR = "utils";
  public static String DOCKER_IAAS_FILE = "connect.sh";
  public static String SSHCONNECT_FILE = "SSHconnect.jar";
  public static String REMOTE_SETTINGS_DIR = "properties" + File.separator + "remote";

  public static String SETTINGS_PATH_SEPARATOR = "/"; // symbol to use instead
  // of "/" in paths of
  // settings files
  /**
   * File with settings for building on server Remote build settings file in
   * "<service><settigns_path_separator><filename>" format.
   */
  public static String SETTINGS_FILE = "settings_file";

  public static String LOCAL_PATH = "local_path";

  // Remote build service names
  // These names are used in directory names for configuration files
  public static String REMOTE_SERVICE_DOCKERIAAS = "dockeriaas";
  public static String REMOTE_SERVICE_SSHCONNECT = "sshconnect";

  // SSHconnect specific settings
  public static String FILE_FILTER = "file_filter";
  public static String PREPROCESS_FILES = "preprocess_files";

  private static final long serialVersionUID = 1L;

  /** Keyword (highlight) setting list */
  private List<RemoteBuildData> RB_data_list = new ArrayList<RemoteBuildData>();

  private InputStream is = null;

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public RemoteBuildProperties() throws Exception {
    if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    loadProperties();
    checkDockerIaaS();
    checkSSHconnect();
  }

  /**
   * Read source configuration properties from the default configuration file.
   *
   * @throws Exception Property read error
   */
  public void loadProperties() throws Exception {
    is = null;
    // Read resource file
    is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE); // properties.xml
    loadProperties(is);
  }

  /**
   * Read source configuration properties from the configuration file.
   *
   * @param propertiesFile Source settings property settings file
   * @throws Exception Property read error
   */
  public void loadProperties(File propertiesFile) throws Exception {

    if (!propertiesFile.exists()) {
      throw (new Exception(
          Message.getString(
              "propertiesbase.exeption.notexist"))); // Source settings properties file does not
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
    RB_data_list = parseRBProperty(stream, "//project");
    // TODO: For backward compatibility
    // if (!checkRBdata) {

    // }
  }

  /** Clear the keyword (highlight) list. */
  public void clearList() {
    RB_data_list = new ArrayList<RemoteBuildData>();
  }

  /**
   * Get keywords
   *
   * @param stream XML input stream
   * @param path Keyword XPATH
   * @return keyword list
   * @throws Exception Keyword parsing error
   */
  public List<RemoteBuildData> parseRBProperty(InputStream stream, String path) throws Exception {

    List<RemoteBuildData> list = new ArrayList<RemoteBuildData>();

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
        RemoteBuildData rbdata = new RemoteBuildData();

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode_key, attrnode_value, attrnode_description;
        String key = null;
        String value = null;
        String commandline_option = null;
        String description = null;

        attrnode_value = attrs.getNamedItem("commandline_option");
        if (attrnode_value == null) continue;

        if (attrnode_value != null) {
          commandline_option = attrnode_value.getNodeValue();
        }

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

        // Description
        attrnode_description = attrs.getNamedItem("description");
        if (attrnode_description != null) {
          description = attrnode_description.getNodeValue();
        }

        if (key != null || value != null) {
          rbdata.setProperty(key, value, commandline_option, i, description);
        }

        list.add(rbdata);

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
          document.createComment(Message.getString("remotebuildproperties.document.comment"));
      node.appendChild(comment);
    }

    if (this.RB_data_list == null || this.RB_data_list.size() <= 0) return;

    // RB property set
    for (RemoteBuildData rb_data : this.RB_data_list) {
      org.w3c.dom.Element elem = document.createElement("server");

      {
        org.w3c.dom.Attr attr = document.createAttribute("key");
        attr.setValue(rb_data.getKey());
        elem.setAttributeNode(attr);
      }
      {
        org.w3c.dom.Attr attr = document.createAttribute("value");
        attr.setNodeValue(rb_data.getValue());
        elem.setAttributeNode(attr);
      }

      {
        org.w3c.dom.Attr attr = document.createAttribute("description");
        attr.setValue(rb_data.getDescription());
        elem.setAttributeNode(attr);
      }
      // Add node
      node.appendChild(elem);
    }
  }

  /**
   * Returns parameters as a Map from YAML file
   *
   * @param settings_file
   * @return Map
   * @throws FileNotFoundException
   */
  public static Map<String, String> getSettingsFromFile(String settings_file)
      throws FileNotFoundException {
    InputStream input = new FileInputStream(new File(locateRemoteSettingsFile(settings_file)));
    Yaml yaml = new Yaml();
    @SuppressWarnings("unchecked")
    Map<String, String> map = (Map<String, String>) yaml.load(input);
    return map;
  }

  /**
   * Get filename with directory for settings file
   *
   * @param settings file
   * @return remote/<settings file>.yml
   */
  public static String locateRemoteSettingsFile(String str) {
    String filename = REMOTE_SETTINGS_DIR + File.separator + str + ".yml";
    // System.out.println("File name is "+ filename);
    // System.out.println("Looking in " + new File(System.getProperty("user.dir")));
    return filename;
  }

  /** Prints out data in RB_data_list */
  public void checkData() {
    System.out.println("RB data: ");
    for (RemoteBuildData rbdata : this.RB_data_list) {
      System.out.println(
          rbdata.getKey()
              + "="
              + rbdata.getValue()
              + ", "
              + rbdata.getDescription()
              + " "
              + rbdata.getCommandlineOption());
    }
  }

  public int count() {
    if (RB_data_list == null || RB_data_list.size() <= 0) {
      return 0;
    }
    return RB_data_list.size();
  }

  /**
   * Get property key
   *
   * @param index
   * @return
   */
  public String getKey(int index) {
    if (RB_data_list == null || RB_data_list.size() <= 0) {
      return null;
    }
    if (RB_data_list.size() <= index) {
      return null;
    }

    return RB_data_list.get(index).getKey();
  }

  /**
   * Get property value
   *
   * @param index
   * @return
   */
  public String getValue(int index) {
    if (RB_data_list == null || RB_data_list.size() <= 0) {
      return null;
    }
    if (RB_data_list.size() <= index) {
      return null;
    }

    return RB_data_list.get(index).getValue();
  }

  /**
   * Get property description
   *
   * @param index
   * @return
   */
  public String getDescription(int index) {
    if (RB_data_list == null || RB_data_list.size() <= 0) {
      return null;
    }
    if (RB_data_list.size() <= index) {
      return null;
    }

    return RB_data_list.get(index).getDescription();
  }

  /**
   * Get property order number
   *
   * @param index
   * @return
   */
  public int getOrder(int index) {
    if (RB_data_list == null || RB_data_list.size() <= 0) {
      return -1;
    }
    if (RB_data_list.size() <= index) {
      return -1;
    }

    return RB_data_list.get(index).getOrder();
  }

  /**
   * Set "value" filed of RemoteBuildData entity from RB_data_list. Entity defined by index.
   *
   * @param index
   * @param value
   */
  public void setValue(int index, String value) {
    RemoteBuildData rb_data = RB_data_list.get(index);
    rb_data.setValue(value);
  }

  /**
   * Set "value" filed of RemoteBuildData entity from RB_data_list. Entity is defined by the key.
   * Function exits after first assignment. Keys are search ignoring character case.
   *
   * @param key
   * @param value
   */
  private void setValueByKey(String key, String value) {
    for (RemoteBuildData rbd : RB_data_list) {
      if (rbd.getKey().equalsIgnoreCase(key)) {
        rbd.setValue(value);
        return;
      }
    }
  }

  /**
   * Return value from RB_data_list with given key
   *
   * @param key
   * @return value
   */
  public String getValueByKey(String key) {
    for (RemoteBuildData rbd : RB_data_list) {
      if (rbd.getKey().equalsIgnoreCase(key)) {
        return rbd.getValue();
      }
    }
    return null;
  }

  /**
   * Set remote settings file path.
   *
   * @param filter
   */
  public void setSettingsFile(String path) {
    setValueByKey(RemoteBuildProperties.SETTINGS_FILE, path);
  }

  /** @return settings file in format <service><settigns_path_separator><filename> */
  public String getSettingsFile() {
    return getValueByKey(RemoteBuildProperties.SETTINGS_FILE);
  }

  /**
   * Set local path. Similar to setBuildCommand function.
   *
   * @param absolutePath
   */
  public void setLocalPath(String absolutePath) {
    setValueByKey(RemoteBuildProperties.LOCAL_PATH, absolutePath);
  }

  /**
   * Set file filter. Similar to setBuildCommand function.
   *
   * @param filter
   */
  public void setFileFilter(String filter) {
    setValueByKey(RemoteBuildProperties.FILE_FILTER, filter);
  }

  /**
   * Set preprocess files. Similar to setBuildCommand function.
   *
   * @param files
   */
  public void setPreprocessFiles(String files) {
    setValueByKey(RemoteBuildProperties.PREPROCESS_FILES, files);
  }

  /**
   * Static method for extracting service name from settings file path
   *
   * @param settings_file
   * @return
   */
  public static String getRemoteService(String settings_file) {
    int pos = settings_file.indexOf(RemoteBuildProperties.SETTINGS_PATH_SEPARATOR);
    String service = settings_file.substring(0, pos);
    return service;
  }

  /** True if we can use connect.sh with Docker IaaS tools for remote code build */
  private static boolean checkDockerIaaS() {
    File f = new File(REMOTE_UTILS_DIR + File.separator + DOCKER_IAAS_FILE);
    if (f.exists()) {
      if (debug_l2) System.out.println("checkDockerIaaS() " + f.getAbsolutePath());
      return true;
    }
    return false;
  }

  /** True if we can use SSHconnect for remote code build */
  private static boolean checkSSHconnect() {
    File f = new File(REMOTE_UTILS_DIR + File.separator + SSHCONNECT_FILE);
    if (f.exists()) {
      if (debug_l2) System.out.println("checkSSHconnect() " + f.getAbsolutePath());
      return true;
    }
    return false;
  }

  @Override
  public void firePropertyChange() {}

  public String getRemoteService() {
    String settings_file = getSettingsFile();
    if (settings_file == null) {
      System.err.println("No remote settings file.");
      return null;
    }
    int pos = settings_file.indexOf(SETTINGS_PATH_SEPARATOR);
    String service = settings_file.substring(0, pos);
    return service;
  }

  public static String[] getRemoteSettings() {
    List<String> list = new ArrayList<String>();
    String[] list_ar = null;
    List<String> ignore = new ArrayList<String>();
    ignore.add("(\\.).*");
    if (!checkDockerIaaS()) {
      ignore.add(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS + "*");
    }
    if (!checkSSHconnect()) {
      ignore.add(RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT + "*");
    }
    File dir = new File(REMOTE_SETTINGS_DIR);
    try {
      String[] s = new String[ignore.size()];
      list = getFiles(dir, list, "", ignore.toArray(s));
      list_ar = new String[list.size()];
    } catch (IOException e) {
      System.err.println("Error reading settings files from remote directory");
      e.printStackTrace();
    }
    return list.toArray(list_ar);
  }

  /*
   * Return list of files in directory with subdirectories.
   *
   * @dir - starting directory
   *
   * @list - list of files found before (empty for the first call)
   *
   * @path_prefix - path from starting directory to current directory ignore -
   * pattern for ignoring files and directories names
   */
  private static List<String> getFiles(
      File dir, List<String> list, String path_prefix, String[] ignore) throws IOException {
    File[] flist = dir.listFiles();
    if (flist == null || flist.length < 1) {
      return list;
    }
    Boolean trunk_extensions = true; // remove extensions from file names
    for (File f : flist) {
      boolean ignore_me = false;
      for (String p : ignore) {
        if (f.getName().matches(p)) {
          ignore_me = true;
          break;
        }
      }
      if (ignore_me) continue;
      if (f.isFile()) {
        String name = f.getName();
        if (trunk_extensions) {
          int pos = name.lastIndexOf(".");
          if (pos > 0) {
            name = name.substring(0, pos);
          }
        }
        list.add(path_prefix + name);
      } else if (f.isDirectory()) {
        list =
            getFiles(f, list, f.getName() + RemoteBuildProperties.SETTINGS_PATH_SEPARATOR, ignore);
      }
    }
    return list;
  }
}
