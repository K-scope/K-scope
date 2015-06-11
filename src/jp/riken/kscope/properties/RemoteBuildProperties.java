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
import java.io.FileReader;
import java.io.FileWriter;
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

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.RemoteBuildData;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.ResourceUtils;

import org.yaml.snakeyaml.*;

/**
 * 
 * @author Peter Bryzgalov
 *
 */
public class RemoteBuildProperties extends PropertiesBase {
	/**
	 * These files are necessary for building source code on remote server
	 * by the corresponding program.
	 * If these files are present in the current directory (with kscope.jar), 
	 * we set flags haveDockerIaaS and haveSSHconnect to TRUE.
	 */
	private static String docker_iaas_file = "makeRemote.sh";
	private static String sshconnect_file = "SSHconnect.jar";
	public static String REMOTE_SETTINGS_DIR = "remote";
	
	public static String settigns_path_separator="/"; // symbol to use instead of "/" in paths of settings files
	public static String SETTINGS_FILE = "settings_file";  // remote build settings file in "<service><settigns_path_separator><filename>" format
	public static String LOCAL_PATH = "local_path";
	
	// Remote build service names 
	// These names are used in directory names for configuration files
	public static String remote_service_dockeriaas = "dockeriaas";
	public static String remote_service_sshconnect = "sshconnect";
	
	// SSHconnect specific settings
	public static String FILE_FILTER = "file_filter";
    public static String PREPROCESS_FILES = "preprocess_files";
	
	private boolean remote_build_possible = false; // Can project be built on a remote server or not?
	public boolean remote_settings_found = false; // True if files with remote settings are found
	private boolean use_remote_build = false; // True if user checked checkUseRemote button on New Project dialog
	/*
     * Two flags show if we have external programs necessary to build code on remote server
     * */
    private boolean haveDockerIaaS = false;
    private boolean haveSSHconnect = false;
    
    private static final long serialVersionUID = 1L;

	/** キーワード(ハイライト)設定リスト */
    private List<RemoteBuildData> RB_data_list = new ArrayList<RemoteBuildData>();
	
	private InputStream is=null;
	private AppController controller; // Used to get other properties (ProjectProperties)
	
	private static HashMap<String,String> options_map;
    static {
    	options_map = new HashMap<String, String>();
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
    
    private static HashMap<String,String> options_map_docker;
    static {
    	options_map_docker = new HashMap<String, String>();
    	options_map_docker.put("server_address", "-h");
    	options_map_docker.put("user", "-u");
    	options_map_docker.put("key", "-k");
    	options_map_docker.put("add_path", "-a");
    	options_map_docker.put("local_path", "-l");
    	options_map_docker.put("build-command", "-m");
    }
    
    private static Boolean debug=(System.getenv("DEBUG")!= null);
    
   	/**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public RemoteBuildProperties(AppController controller) throws Exception {
    	this.controller = controller;
        loadProperties();   
        // set Remote Build is possible Flag to TRUE if either SSHconnect or makeRemote for DockerIaaS are present
		this.haveDockerIaaS = checkDockerIaaS();
		this.haveSSHconnect = checkSSHconnect();
		this.remote_build_possible = (this.haveDockerIaaS || this.haveSSHconnect); 
    }
    
    /**
     * ソース設定プロパティをデフォルト設定ファイルから読み込む。
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
        is = null;
        // リソースファイルの読込
        is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
        loadProperties(is);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     * @param  propertiesFile 		ソース設定プロパティ設定ファイル
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw(new Exception(Message.getString("propertiesbase.exeption.notexist"))); //ソース設定プロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // XMLファイルのパース
        loadProperties(stream);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(InputStream stream ) throws Exception {
        // XMLファイルのパース
        RB_data_list = parseRBProperty(stream, "//remotebuild");
    }


    /**
     * キーワード(ハイライト)設定リストを取得する。
     * @return		ハイライト設定リスト
     */
    public List<RemoteBuildData> getList() {
        return RB_data_list;
    }

    
    /**
     * キーワード(ハイライト)リストをクリアする。
     */
    public void clearList() {
        RB_data_list = new ArrayList<RemoteBuildData>();
    }

    /**
     * 検索Keyと一致するRemoteBuildData情報を取得する
     * @param key	Property key (name)
     * @return		Property情報
     */
    public RemoteBuildData getPropertySet(String key) {
        if (key == null || key.isEmpty()) return null;

        for (RemoteBuildData rb_property_set : RB_data_list) {
        	String k = rb_property_set.getKey();
        	if (k.equalsIgnoreCase(key)) {
        		return rb_property_set;
        	}
        }
        return null;
    }

    /**
     * キーワードを取得する
     * @param stream		XML入力ストリーム
     * @param path		キーワードXPATH
     * @return		キーワードリスト
     * @throws Exception 		キーワードパースエラー
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

        for (int i=0; i<nodelist.getLength(); i++) {
            try {
                Node node = nodelist.item(i);
                RemoteBuildData rbdata = new RemoteBuildData();
                
                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode_key,attrnode_value,attrnode_type,attrnode_description;
                String key = null;
                String value = null;
                String commandline_option = null;
                String type = null;
                String description = null;
                
                // プロパティ名
                attrnode_key = attrs.getNamedItem("key");
                if (attrnode_key != null) {
                    key = attrnode_key.getNodeValue();
                }
                // プロパティ値
                attrnode_value = attrs.getNamedItem("value");
                if (attrnode_value != null) {
                    value = attrnode_value.getNodeValue();                    
                }
                attrnode_value = attrs.getNamedItem("commandline_option");
                if (attrnode_value != null) {
                    commandline_option = attrnode_value.getNodeValue();                    
                }
                // プロパティType
                attrnode_type = attrs.getNamedItem("type");
                if (attrnode_type != null) {
                    type = attrnode_type.getNodeValue();                    
                }
                
                //Description
                attrnode_description = attrs.getNamedItem("description");
                if (attrnode_description != null) {
                    description = attrnode_description.getNodeValue();                    
                }                
                
                if (key != null || value != null) {
                	rbdata.setProperty(key, value, commandline_option, type, i, description);
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
     * プロパティをDOMノードに出力する
     * @param node		出力ノード
     */
    public void writeProperties(org.w3c.dom.Node node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("remotebuildproperties.document.comment")); 
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
                org.w3c.dom.Attr attr = document.createAttribute("type");
                attr.setValue(rb_data.getType());
                elem.setAttributeNode(attr);
            }
            {
                org.w3c.dom.Attr attr = document.createAttribute("description");
                attr.setValue(rb_data.getDescription());
                elem.setAttributeNode(attr);
            }
            // ノード追加
            node.appendChild(elem);
        }
    }

    /**
     * Format command line options for remote build call
     * @param Remote build settings file path
     * @return string with command line options
     */
    public String[] getCommandLineOptions(String RS) {
    	if (debug) {
    		checkData();
    	}
    	String service = getRemoteService();
    	ProjectProperties pp = this.controller.getPropertiesProject();
    	List<String> command_options = new ArrayList<String>();
    	String value = pp.getBuildCommand();
    	try {
			if (value.length() > 0) {
				command_options.add("-m");
				command_options.add("\""+value+"\"");
			} else {
				System.err.println("Build command is empty.");
			}
		} catch (NullPointerException e) {
			System.err.println("Build command is null.");
		} 
    	for (RemoteBuildData rbdata : this.RB_data_list) {
    		String commandline_option = rbdata.getCommandlineOption();
    		value = null;
    		// Get other properties from RemoteBuildProperties
			value = rbdata.getValue();
    		try {
    			if (value.length() > 0) {    				
    				if (rbdata.getKey().equalsIgnoreCase(RemoteBuildProperties.SETTINGS_FILE)) {
    					// Add CLI options from file
    					String settings_file = value;    					
    					try {	    					
    						// Add all values from YAML file to command_option 
	    				    // prefixed with CLI options from options Map
    						
    						Map<String, String> map = getSettingsFromFile(settings_file);
	    				    Iterator<java.util.Map.Entry<String, String>> iterator = map.entrySet().iterator();	    				    
	    				    while (iterator.hasNext()) {
	    				    	Map.Entry<String, String> entry = (Map.Entry<String, String>)iterator.next();
	    				    	command_options = addCLoption(command_options, entry);
	    				    }
    					}
    					catch (FileNotFoundException e) {
    						System.out.println(settings_file+ " not found");
    						//return null;
    					}    					
    				}    				
    				else {
    					if (service.indexOf("docker") >= 0) {
    						String key = rbdata.getKey(); 
    						if (key.equalsIgnoreCase(PREPROCESS_FILES) || key.equalsIgnoreCase(FILE_FILTER)) {
    							System.out.println("Otion "+ key + " is not used in "+ service+ ". Option is ignored.");
    							continue;
    						}
        				}
    					command_options.add(commandline_option);
    					command_options.add("\""+value+"\"");
    				}
    			} 
    		} catch (NullPointerException e) {
    			System.out.println("Remote build option "+rbdata.getKey()+" is null.");
    		} 
    	}
    	String[] result=command_options.toArray(new String[0]);
    	System.out.println("Command line options: " + Arrays.toString(result));
    	return result;
    }

	/**
	 * Returns parameters as a Map from YAML file
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
	private List<String> addCLoption(List<String> command_options,	Map.Entry<String, String> entry) {
		String value = null;
		Object v = entry.getValue();
		try {
			value = (String)v;
		}
		catch (ClassCastException e) {
			try {
				value = String.valueOf(v);
			}
			catch (ClassCastException ex) {
				System.err.println("Undefined type of parameter value: " + v);
				ex.printStackTrace();
				return command_options;
			}
		}			
		if (debug) {
			System.out.println("entry: " + entry.getKey() + " = "+value);
		}
		// Ignore description
		if (entry.getKey().equalsIgnoreCase("description")) {
			return command_options;
		}
		if (value == null || value.equalsIgnoreCase("null")) {
			System.out.println("Entry ignored");
			return command_options;
		}
		String option =  getCLIoption(entry.getKey());
		if (option == null) return command_options;
		command_options.add(option);
		command_options.add(value);
		return command_options;
	}

	/**
	 * Get CLI option for given parameter name
	 * @param key -- parameter name, key for map options_map.
	 * @return CLI option
	 */
	private String getCLIoption(String key) {
		
		String option = "";
		String service = getRemoteService();
		if (debug) {
			System.out.println("RS "+ service);
		}
		if (service.indexOf("sshconnect") >= 0) {
			option = options_map.get(key);
		} 
		else if (service.indexOf("docker") >= 0) {
			option = options_map_docker.get(key);
			if (option == null) {
				System.err.println("Option " + key+ " not used for Docker IaaS tools. Option is ignored.");
			}
		}
		return option;
	}

	/**
	 * Get filename with directory for settings file
	 * @param settings file
	 * @return remote/<settings file>.yml
	 */
	public static String locateRemoteSettingsFile(String str) {
		String filename = "remote/"+str+".yml";
		//System.out.println("File name is "+ filename);
		//System.out.println("Looking in " + new File(System.getProperty("user.dir")));
		return filename;
	}

	/**
	 * Prints out data in RB_data_list
	 */
	public void checkData() {
		System.out.println("RB data: ");
		for (RemoteBuildData rbdata : this.RB_data_list) {
			System.out.println(rbdata.getKey()+"="+rbdata.getValue()+", "+rbdata.getDescription() + " " + rbdata.getCommandlineOption()+" " + rbdata.getType());
		}
	}
    
      
	public int count() {
		if (RB_data_list == null || RB_data_list.size() <= 0) {return 0;}
        return RB_data_list.size();
	}

	/**
	 * Get property key
	 * @param index
	 * @return
	 */
	public String getKey(int index) {
		if (RB_data_list == null || RB_data_list.size() <= 0) {return null;}
        if (RB_data_list.size() <= index) {return null;}

        return RB_data_list.get(index).getKey();
	}

	/**
	 * Get property value
	 * @param index
	 * @return
	 */
	public String getValue(int index) {
		if (RB_data_list == null || RB_data_list.size() <= 0) {return null;}
        if (RB_data_list.size() <= index) {return null;}

        return RB_data_list.get(index).getValue();
	}
	
	/**
	 * Get property description
	 * @param index
	 * @return
	 */
	public String getDescription(int index) {
		if (RB_data_list == null || RB_data_list.size() <= 0) {return null;}
        if (RB_data_list.size() <= index) {return null;}

        return RB_data_list.get(index).getDescription();
	}
	
	/**
	 * Get property order number
	 * @param index
	 * @return
	 */
	public int getOrder(int index) {
		if (RB_data_list == null || RB_data_list.size() <= 0) {return -1;}
        if (RB_data_list.size() <= index) {return -1;}

        return RB_data_list.get(index).getOrder();
	}

	/**
	 * Set "value" filed of RemoteBuildData entity from RB_data_list. Entity defined by index.
	 * @param index
	 * @param value
	 */
	public void setValue(int index, String value) {
		RemoteBuildData rb_data = RB_data_list.get(index);
		rb_data.setValue(value);
	}
		
	/**
	 * Set "value" filed of RemoteBuildData entity from RB_data_list. Entity is defined by the key. 
	 * Function exits after first assignment.
	 * Keys are search ignoring character case.
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
     * @param filter
     */
    public void setSettingsFile(String path) {
    	setValueByKey(RemoteBuildProperties.SETTINGS_FILE, path);
    }
	
    /**
     * 
     * @return settings file in format <service><settigns_path_separator><filename>
     */
    public String getSettingsFile() {
    	return getValueByKey(RemoteBuildProperties.SETTINGS_FILE);
    }
    
    
	/**
	 * Set local path. Similar to setBuildCommand function.
	 * @param absolutePath
	 */
	public void setLocalPath(String absolutePath) {
		setValueByKey(RemoteBuildProperties.LOCAL_PATH, absolutePath);		
	}

	/**
     * Set file filter. Similar to setBuildCommand function.
     * @param filter
     */
    public void setFileFilter(String filter) {
            setValueByKey(RemoteBuildProperties.FILE_FILTER, filter);
    }

    /**
     * Set preprocess files. Similar to setBuildCommand function.
     * @param files
     */
    public void setPreprocessFiles(String files) {
            setValueByKey(RemoteBuildProperties.PREPROCESS_FILES, files);
    }
	
    /**
     * Static method for extracting service name from settings file path
     * @param settings_file
     * @return
     */
    public static String getRemoteService(String settings_file) {    	
		int pos = settings_file.indexOf(RemoteBuildProperties.settigns_path_separator);
		String service = settings_file.substring(0, pos);
		return service;
	}

    /**
     * True if we can use makeRemote with Docker IaaS tools for remote code build 
     * */
    private static boolean checkDockerIaaS() {
        File f = new File(docker_iaas_file);
        if (f.exists()) {
            System.out.println(f.getAbsolutePath());
            return true;
        }
        return false;
    }

    /** 
     * True if we can use SSHconnect for remote code build
     */
    private static boolean checkSSHconnect() {
        File f = new File(sshconnect_file);
        if (f.exists()) {
            System.out.println(f.getAbsolutePath());
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
    	return this.remote_build_possible && this.remote_settings_found;
    }
    
    public boolean useRemoteBuild() {
    	return this.remote_build_possible && this.remote_settings_found && this.use_remote_build;
    }
    
    public void setRemoteBuild(boolean useRB) {
    	this.use_remote_build = useRB;
    }
    
	@Override
	public void firePropertyChange() {}

	public String getRemoteService() {
		String settings_file = getSettingsFile();
		if (settings_file == null) {
			System.err.println("No remote settings file.");
			return null;
		}
		int pos = settings_file.indexOf(RemoteBuildProperties.settigns_path_separator);
		String service = settings_file.substring(0, pos);
		return service;
	}
	
	
	public static String[] getRemoteSettings() {
    	List<String> list = new ArrayList<String>();
    	String[] list_ar = null;
    	List<String> ignore = new ArrayList<String>(); 
    	ignore.add("(\\.).*");
    	if (!checkDockerIaaS()) {
    		ignore.add(RemoteBuildProperties.remote_service_dockeriaas + "*");
    	}
    	if (!checkSSHconnect()) {
    		ignore.add(RemoteBuildProperties.remote_service_sshconnect + "*");
    	}
		File dir = new File(REMOTE_SETTINGS_DIR);
		try {
			String[] s = new String[ignore.size()];
			list = getFiles(dir, list, "", ignore.toArray(s));			
			list_ar=new String[list.size()];
		}
		catch (IOException e) {
			System.err.println("Error reading settings files from remote directory");
			e.printStackTrace();			
		}
		return list.toArray(list_ar);
	}
	
	/*
     * Return list of files in directory with subdirectories.
     * @dir - starting directory
     * @list - list of files found before (empty for the first call)
     * @path_prefix - path from starting directory to current directory
     * ignore - pattern for ignoring files and directories names
     */
	private static List<String> getFiles(File dir, List<String> list, String path_prefix, String[] ignore) throws IOException {
		File [] flist = dir.listFiles();
		if (flist == null || flist.length < 1) {
			return list;
		}
		Boolean trunk_extensions = true;  // remove extensions from file names
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
				list.add(path_prefix+name);
			} 
			else if (f.isDirectory()) {
				list = getFiles(f,list,f.getName()+RemoteBuildProperties.settigns_path_separator, ignore);
			}
		}
		return list;		
	}
	
}
