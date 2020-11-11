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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.BasicPropertyList;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.yaml.snakeyaml.Yaml;

/**
 * Project property setting class
 *
 * @author RIKEN
 */
public class ProjectProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  // Property key
  /** Project title property key */
  public static final String PRJ_TITLE = "project_title";
  /** Build command */
  public static final String BUILD_COMMAND = "build_command";

  public static final String CLEAN_COMMAND = "clean_command";

  public static String LOCAL_PATH = "local_path";

  // Intermediate code generation
  public static final String GENERATE_XML = "generate-XML";

  // full mode
  public static final String FULL_PROJECT = "full-project";

  /** Property setting list */
  private List<ProjectPropertyValue> listProperty = new ArrayList<ProjectPropertyValue>();
  /** Project hidden properties */
  private BasicPropertyList listHiddenProperty = null;

  /*
   * Two flags show if we have external programs necessary to build code on remote server
   * */
  private boolean haveDockerIaaS = false;
  private boolean haveSSHconnect = false;

  public boolean remote_settings_found = false; // True if files with remote
  // settings are found

  private static HashMap<String, String> options_map;

  static {
    options_map = new HashMap<String, String>(12, 1);
    options_map.put("server_address", "-h");
    options_map.put("port", "-p");
    options_map.put("user", "-u");
    options_map.put("password", "-pw");
    options_map.put("key", "-k");
    options_map.put("passphrase", "-ph");
    options_map.put("add_path", "-a");
    options_map.put("remote_path", "-rp");
    options_map.put("local_path", "-l");
    options_map.put("build-command", "-m");
    options_map.put("product_pattern", "-dp");
    options_map.put("command_pattern", "-cp");
  }

  private static HashMap<String, String> options_map_docker;

  static {
    options_map_docker = new HashMap<String, String>(8, 1);
    options_map_docker.put("server_address", "-h");
    options_map_docker.put("port", "-p");
    options_map_docker.put("user", "-u");
    options_map_docker.put("key", "-k");
    options_map_docker.put("add_path", "-a");
    options_map_docker.put("local_path", "-l");
    options_map_docker.put("build-command", "-m");
  }

  /**
   * Constructor
   *
   * @throws Exception Property read error
   */
  public ProjectProperties() throws Exception {
    if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    loadProperties();
    // set Remote Build is possible Flag to TRUE if either SSHconnect or connect.sh for DockerIaaS
    // are present
    this.haveDockerIaaS = checkDockerIaaS();
    this.haveSSHconnect = checkSSHconnect();
    this.remote_settings_found = (this.haveDockerIaaS || this.haveSSHconnect);
  }

  /**
   * Load project configuration properties from the default configuration file.
   *
   * @throws Exception Property read error
   */
  public void loadProperties() throws Exception {
    InputStream is = null;

    // Read resource file
    is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
    loadProperties(is);

    is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
    loadPropertiesOther(is);
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

    stream = new FileInputStream(propertiesFile);
    loadPropertiesOther(stream);
  }

  /**
   * Load project configuration properties from the configuration file.
   *
   * @param stream Configuration file stream
   * @throws Exception Property read error
   */
  public void loadProperties(InputStream stream) throws Exception {
    // Parsing the XML file
    List<ProjectPropertyValue> list = null;
    list = parseProjectProperty(stream, "//project");
    if (list != null) {
      this.listProperty = list;
    }
  }

  /**
   * Load project configuration properties from the configuration file.
   *
   * @param stream Configuration file stream
   * @throws Exception Property read error
   */
  public void loadPropertiesOther(InputStream stream) throws Exception {
    // Read project hidden properties
    this.listHiddenProperty = new BasicPropertyList(stream, "//project_other");
  }

  /**
   * Get keywords
   *
   * @param stream XML input stream
   * @param path Keyword XPATH
   * @return keyword list
   * @throws Exception Keyword parsing error
   */
  public List<ProjectPropertyValue> parseProjectProperty(InputStream stream, String path)
      throws Exception {

    List<ProjectPropertyValue> list = new ArrayList<ProjectPropertyValue>();

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

        // Get attributes
        NamedNodeMap attrs = node.getAttributes();
        Node attrnode;
        String type = "text";
        String key = "";
        String value = "";
        String name = "";
        String message = "";
        String commandline_option = null;

        // Type
        attrnode = attrs.getNamedItem("type");
        if (attrnode != null) {
          type = attrnode.getNodeValue();
        }
        // Key
        attrnode = attrs.getNamedItem("key");
        if (attrnode != null) {
          key = attrnode.getNodeValue();
          if (StringUtils.isNullOrEmpty(key)) continue;
        }
        // value
        attrnode = attrs.getNamedItem("value");
        if (attrnode != null) {
          if ("reference".equalsIgnoreCase(type)) {
            value = "";
          } else {
            value = attrnode.getNodeValue();
          }
        }
        // name
        attrnode = attrs.getNamedItem("name");
        if (attrnode != null) {
          name = attrnode.getNodeValue();
        }
        // Message
        attrnode = attrs.getNamedItem("message");
        if (attrnode != null) {
          message = attrnode.getNodeValue();
        }

        attrnode = attrs.getNamedItem("commandline_option");
        if (attrnode != null) {
          commandline_option = attrnode.getNodeValue();
        }

        list.add(new ProjectPropertyValue(key, type, name, value, message, commandline_option, i));

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
   * Set the property value.
   *
   * @param value Property value
   */
  public void setPropertyValue(ProjectPropertyValue value) {
    if (value == null) {
      return;
    }

    for (int i = 0; i < this.listProperty.size(); i++) {
      ProjectPropertyValue property = this.listProperty.get(i);
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
  public ProjectPropertyValue getPropertyValue(String key) {
    if (key == null) {
      System.err.println("Key " + key + " is null");
      return null;
    }

    for (ProjectPropertyValue property : this.listProperty) {
      if (key.equalsIgnoreCase(property.getKey())) {
        return property;
      }
    }
    System.err.println("Property " + key + " not found");
    return null;
  }

  public String getHiddenPropertyValue(String key) {
    return listHiddenProperty.getPropertyValue(key);
  }

  public void setHiddenPropertyValue(String key, String value) {
    listHiddenProperty.setProperty(key, value);
  }

  public String[] getHiddenPropertyPairs() {
    return listHiddenProperty.getPairs();
  }

  /**
   * Output properties to DOM node
   *
   * @param node Output node
   * @param projectFolder Project folder (= output folder)
   */
  public void writeProperties(org.w3c.dom.Element node) {

    // Get documentation
    org.w3c.dom.Document document = node.getOwnerDocument();

    // add comment
    {
      org.w3c.dom.Comment comment =
          document.createComment(
              Message.getString("projectproperties.document.comment")); // Project properties
      node.appendChild(comment);
    }

    for (ProjectPropertyValue value : this.listProperty) {
      org.w3c.dom.Element elem = document.createElement("project");

      String key = "";
      // key
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("key");
        key = value.getKey();
        attr.setNodeValue(key);
        elem.setAttributeNode(attr);
      }
      // name
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("name");
        attr.setNodeValue(value.getName());
        elem.setAttributeNode(attr);
      }
      // type
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("type");
        attr.setNodeValue(value.getType());
        elem.setAttributeNode(attr);
      }
      // value
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("value");
        String attr_value = value.getValue();
        attr_value = StringUtils.escapeFilePath(attr_value);
        attr.setNodeValue(attr_value);
        elem.setAttributeNode(attr);
      }

      // message
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("message");
        attr.setNodeValue(value.getMessage());
        elem.setAttributeNode(attr);
      }

      // command line options
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("commandline_option");
        attr.setNodeValue(value.getCommandlineOption());
        elem.setAttributeNode(attr);
      }

      node.appendChild(elem);
    }
    // add comment
    {
      org.w3c.dom.Comment comment =
          document.createComment("Project hidden properties"); // Project hidden properties
      node.appendChild(comment);
    }

    for (String key : this.listHiddenProperty.getKeys()) {
      org.w3c.dom.Element elem = document.createElement("project_other");

      // key
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("key");
        attr.setNodeValue(key);
        elem.setAttributeNode(attr);
      }
      // value
      {
        org.w3c.dom.Attr attr;
        attr = document.createAttribute("value");
        String attr_value = this.listHiddenProperty.getPropertyValue(key);
        attr_value = StringUtils.escapeFilePath(attr_value);
        attr.setNodeValue(attr_value);
        elem.setAttributeNode(attr);
      }
      node.appendChild(elem);
    }
  }

  /**
   * Format command line options for remote build call
   *
   * @param Remote build settings file path
   * @return string with command line options
   */
  public String[] getCommandLineOptions(String RS) {
    if (debug) {
      checkData();
    }
    String service = getRemoteService();
    List<String> command_options = new ArrayList<String>();
    String value;
    String key;
    for (ProjectPropertyValue pproperty : this.listProperty) {
      String commandline_option = pproperty.getCommandlineOption();
      if (commandline_option == null) continue;
      if (commandline_option.length() <= 0) continue;
      // Get other properties from RemoteBuildProperties
      value = pproperty.getValue();
      key = pproperty.getKey();
      if (debug_l2)
        System.out.println("CL options " + commandline_option + " " + key + ":" + value);
      try {
        if (value.length() > 0) {
          if (key.equalsIgnoreCase(RemoteBuildProperties.SETTINGS_FILE)) {
            // Add CLI options from file
            String settings_file = value;
            try {
              // Add all values from YAML file to command_option
              // prefixed with CLI options from options Map

              Map<String, String> map = getSettingsFromFile(settings_file);
              Iterator<java.util.Map.Entry<String, String>> iterator = map.entrySet().iterator();
              while (iterator.hasNext()) {
                Map.Entry<String, String> entry = (Map.Entry<String, String>) iterator.next();
                command_options = addCLoption(command_options, entry);
                if (debug_l2) {
                  System.out.println(
                      "CL option from file "
                          + entry.getKey()
                          + ":"
                          + String.valueOf(entry.getValue()));
                }
              }
            } catch (FileNotFoundException e) {
              System.out.println(settings_file + " not found");
              // return null;
            }
          } else {
            if (service.indexOf(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS) >= 0) {
              if (key.equalsIgnoreCase(RemoteBuildProperties.PREPROCESS_FILES)
                  || key.equalsIgnoreCase(RemoteBuildProperties.FILE_FILTER)) {
                System.out.println(
                    "Option " + key + " is not used for " + service + ". Option is ignored.");
                continue;
              }
            }
            command_options.add(commandline_option);
            command_options.add("\"" + value + "\"");
          }
        }
      } catch (NullPointerException e) {
        System.out.println("Remote build option " + pproperty.getKey() + " is null.");
      }
    }
    String[] result = command_options.toArray(new String[0]);
    System.out.println("Command line options: " + Arrays.toString(result));
    return result;
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
   * @return Map with new command option added
   * @param command_options list
   * @param new option to be added to map
   * @param new option value
   */
  private List<String> addCLoption(List<String> command_options, Map.Entry<String, String> entry) {
    String value = null;
    Object v = entry.getValue();
    try {
      value = (String) v;
    } catch (ClassCastException e) {
      try {
        value = String.valueOf(v);
      } catch (ClassCastException ex) {
        System.err.println("Undefined type of parameter value: " + v);
        ex.printStackTrace();
        return command_options;
      }
    }
    if (debug) {
      System.out.println("entry: " + entry.getKey() + " = " + value);
    }
    // Ignore description
    if (entry.getKey().equalsIgnoreCase("description")) {
      return command_options;
    }
    if (value == null || value.equalsIgnoreCase("null")) {
      System.out.println("Entry ignored");
      return command_options;
    }
    String option = getCLIoption(entry.getKey());
    if (option == null) return command_options;
    command_options.add(option);
    command_options.add(value);
    return command_options;
  }

  /**
   * Get CLI option for given parameter name
   *
   * @param key -- parameter name, key for map options_map.
   * @return CLI option
   */
  private String getCLIoption(String key) {

    String option = "";
    String service = getRemoteService();
    if (debug_l2) {
      System.out.println("RS " + service);
    }
    if (service.equalsIgnoreCase(RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT)) {
      option = options_map.get(key);
    } else if (service.equalsIgnoreCase(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS)) {
      option = options_map_docker.get(key);
      if (option == null) {
        System.out.println("Option " + key + " not used for Docker IaaS tools. Option is ignored.");
      }
    }
    return option;
  }

  /**
   * Get filename with directory for settings file
   *
   * @param settings file
   * @return remote/<settings file>.yml
   */
  public static String locateRemoteSettingsFile(String str) {
    String filename = RemoteBuildProperties.REMOTE_SETTINGS_DIR + File.separator + str + ".yml";
    // System.out.println("File name is "+ filename);
    if (debug_l2)
      System.out.println(
          "Looking for " + filename + " in " + new File(System.getProperty("user.dir")));
    return filename;
  }

  /** Prints out data in list */
  public void checkData() {
    System.out.println("Project Property data: ");
    for (ProjectPropertyValue pproperty : this.listProperty) {
      System.out.println(pproperty.toString());
    }
  }

  /*
  public int count() {
  	if (RB_data_list == null || RB_data_list.size() <= 0) {return 0;}
         return RB_data_list.size();
  }
  */

  /**
   * Get the property value list.
   *
   * @return Property value list
   */
  public ProjectPropertyValue[] getPropertyValues() {
    return this.listProperty.toArray(new ProjectPropertyValue[0]);
  }

  /**
   * Setting the build command
   *
   * @param command make command
   */
  public void setBuildCommand(String build_command) {
    setValueByKey(BUILD_COMMAND, build_command);
  }

  /**
   * Save clean command to project properties class instance
   *
   * @param cc - clean command
   */
  public void setCleanCommand(String cc) {
    setValueByKey(CLEAN_COMMAND, cc);
  }

  /**
   * Project title setting
   *
   * @param title Project title
   */
  public void setProjectTitle(String title) {
    setValueByKey(PRJ_TITLE, title);
  }

  /**
   * Set local path. Similar to setBuildCommand function.
   *
   * @param absolutePath
   */
  public void setSettingsFile(String name) {
    setValueByKey(RemoteBuildProperties.SETTINGS_FILE, name);
  }

  /** @return settings file in format <service><settigns_path_separator><filename> */
  public String getSettingsFile() {
    return getValueByKey(RemoteBuildProperties.SETTINGS_FILE);
  }

  /**
   * Return value from RB_data_list with given key
   *
   * @param key
   * @return value
   */
  public String getValueByKey(String key) {
    for (ProjectPropertyValue pp_value : listProperty) {
      if (pp_value.getKey().equalsIgnoreCase(key)) {
        return pp_value.getValue();
      }
    }
    return null;
  }

  /**
   * Set local path. Similar to setBuildCommand function.
   *
   * @param absolutePath
   */
  public void setLocalPath(String absolutePath) {
    setValueByKey(LOCAL_PATH, absolutePath);
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
    if (settings_file == null || settings_file.length() == 0) {
      System.err.println(
          "Call to ProjectProperties/getRemoteService with empty settings file name.");
      return null;
    }
    int pos = settings_file.indexOf(RemoteBuildProperties.SETTINGS_PATH_SEPARATOR);
    String service = settings_file.substring(0, pos);
    return service;
  }

  /** True if we can use connect.sh with Docker IaaS tools for remote code build */
  private static boolean checkDockerIaaS() {
    File f =
        new File(
            RemoteBuildProperties.REMOTE_UTILS_DIR + "/" + RemoteBuildProperties.DOCKER_IAAS_FILE);
    if (f.exists()) {
      if (debug_l2) System.out.println("PP.checkDockerIaaS() " + f.getAbsolutePath());
      return true;
    }
    return false;
  }

  /** True if we can use SSHconnect for remote code build */
  private static boolean checkSSHconnect() {
    File f =
        new File(
            RemoteBuildProperties.REMOTE_UTILS_DIR + "/" + RemoteBuildProperties.SSHCONNECT_FILE);
    if (f.exists()) {
      if (debug_l2) System.out.println("PP.checkSSHconnect() " + f.getAbsolutePath());
      return true;
    }
    return false;
  }

  /*
   * Use these functions to check if remote build is possible
   * */
  public boolean haveDockerIaaS() {
    return this.haveDockerIaaS;
  }

  public boolean haveSSHconnect() {
    return this.haveSSHconnect;
  }

  public boolean remoteBuildPossible() {
    return this.remote_settings_found;
  }

  public boolean useRemoteBuild() {
    return this.remote_settings_found && this.useServer();
  }

  public String getRemoteService() {
    String settings_file = getSettingsFile();
    if (settings_file == null) {
      System.err.println("No remote settings file.");
      return null;
    }
    int pos = settings_file.indexOf(RemoteBuildProperties.SETTINGS_PATH_SEPARATOR);
    String service = settings_file.substring(0, pos);
    return service;
  }

  public static String[] getRemoteSettings() {
    return getRemoteSettings(null);
  }

  /**
   * Return list of settings files for given remote service (sshconnect/dockeriaas)
   *
   * @return
   */
  public static String[] getRemoteSettings(String service) {
    boolean dockeriaas = false;
    boolean sshconnect = false;
    if (service != null) {
      if (service.equalsIgnoreCase(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS))
        dockeriaas = true;
      else if (service.equalsIgnoreCase(RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT))
        sshconnect = true;
    } else {
      dockeriaas = true;
      sshconnect = true;
    }
    List<String> list = new ArrayList<String>();
    String[] list_ar = null;
    List<String> ignore = new ArrayList<String>();
    ignore.add("(\\.).*");
    if (!checkDockerIaaS() || !dockeriaas) {
      if (debug) System.out.println("Ignoring dockeriass service files");
      ignore.add(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS + "*");
    }
    if (!checkSSHconnect() || !sshconnect) {
      if (debug) System.out.println("Ignoring sshconnect service files");
      ignore.add(RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT + "*");
    }
    File dir = new File(RemoteBuildProperties.REMOTE_SETTINGS_DIR);
    try {
      String[] s = new String[ignore.size()];
      list = getFiles(dir, list, "", ignore.toArray(s));
      list_ar = new String[list.size()];
      if (debug) {
        for (String file : list.toArray(list_ar)) {
          System.out.println(
              "file=" + file + " service=" + ProjectProperties.getRemoteService(file));
        }
      }
    } catch (IOException e) {
      System.err.println("Error reading settings files from remote directory");
      e.printStackTrace();
    }
    return list.toArray(list_ar);
  }

  /**
   * Specify the key to set the property
   *
   * @param key key
   * @param value value
   */
  private void setValueByKey(String key, String value) {
    if (StringUtils.isNullOrEmpty(key)) return;
    for (ProjectPropertyValue v : this.listProperty) {
      if (v == null) continue;
      if (key.equals(v.getKey())) {
        v.setValue(value);
        break;
      }
    }
  }

  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /** Get BUILD COMMAND */
  public String getBuildCommand() {
    ProjectPropertyValue bc = getPropertyValue(BUILD_COMMAND);
    String bcs = bc.getValue();
    return bcs;
  }

  /** Get SETTINGS_FILE */
  public String getRemoteSettingsFile() {
    ProjectPropertyValue ppv = getPropertyValue(RemoteBuildProperties.SETTINGS_FILE);
    String s = ppv.getValue();
    return s;
  }

  /**
   * True if project Rebuild action can be performed. Depends on "full_project" and "generate_xml"
   * properties from properties.xml
   *
   * @return
   */
  public boolean canRebuild() {
    return testValue(getHiddenPropertyValue(ProjectProperties.FULL_PROJECT))
        && testValue(getHiddenPropertyValue(ProjectProperties.GENERATE_XML));
  }

  /**
   * True if project is full - i.e. includes intermediate code.
   *
   * @return
   */
  public boolean isFullProject() {
    return testValue(getHiddenPropertyValue(ProjectProperties.FULL_PROJECT));
  }

  /**
   * True if need to build the project (locally or remotely) to generate intermediate code.
   *
   * @return
   */
  public boolean generateXML() {
    return testValue(getHiddenPropertyValue(ProjectProperties.GENERATE_XML));
  }

  /**
   * Return boolean representation of String value ("true" or not "true") of project hidden
   * property.
   *
   * @param s
   * @return
   */
  private boolean testValue(String s) {
    if (s == null) {
      noHiddenProperties();
      return true;
    }
    return (s.equalsIgnoreCase("true"));
  }

  /** Display error and print out hidden properties list */
  private void noHiddenProperties() {
    System.err.println(
        "Project hidden properties not set. Check properties.xml file.\n"
            + "List of hidden properties:\n");
    String[] hidden_pproperties = getHiddenPropertyPairs();
    for (String s : hidden_pproperties) {
      System.err.println(s);
    }
  }

  // NO MORE NEED IN REBUILD FLAG. "REBUILD" MENU DEPENDS ONLY ON GENERATEXML AND FULL_PROJECT
  // PROPERTIES
  // DELETED : public void setRebuildFlag(boolean flag) {}

  /**
   * Return true if project uses server for building Fortran project. Returns boolean "true" if
   * property use-server is set to String "true".
   *
   * @return
   */
  public boolean useServer() {
    String rs_file = getPropertyValue(RemoteBuildProperties.SETTINGS_FILE).getValue();
    return (rs_file != null && rs_file.length() > 0);
  }

  /*
   * Return list of files in directory with subdirectories.
   * @dir - starting directory
   * @list - list of files found before (empty for the first call)
   * @path_prefix - path from starting directory to current directory
   * ignore - pattern for ignoring files and directories names
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

  public String toString() {
    String s = "";
    String separator = "";
    for (ProjectPropertyValue property : this.listProperty) {
      s += separator + property.getKey() + "=" + property.getValue();
      if (separator == "") separator = ", ";
    }
    return s;
  }
}
