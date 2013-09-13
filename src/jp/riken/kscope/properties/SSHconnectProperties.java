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

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.SSHconnectData;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * SSHconnectプロパティ設定クラス
 * @author peterbryzgalov
 *
 */
public class SSHconnectProperties extends PropertiesBase {
	
	public static String ADD_PATH = "add_path";
	public static String HOST = "server_address";
	public static String PORT = "port";
	public static String USER = "user";
	public static String PASSWORD = "build_command";
	public static String KEY = "key";
	public static String PASSPHRASE = "passphrase";
	public static String REMOTE_PATH = "remote_path";
	public static String BUILD_COMMAND = "build_command";
	public static String LOCAL_PATH = "local_path";
	public static String FILE_FILTER = "file_filter";
	public static String PREPROCESS_FILES = "preprocess_files";
	
	public boolean haveSSHconnect = false; // Possible to use SSHconnect for making project or not
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** キーワード(ハイライト)設定リスト */
    private List<SSHconnectData> listSSH = new ArrayList<SSHconnectData>();
	
	private InputStream is=null;
	private AppController controller; // Used to get other properties (ProjectProperties)
    
   	/**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public SSHconnectProperties(AppController controller) throws Exception {
    	this.controller = controller;
        loadProperties();        
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
        listSSH = parseSSHProperty(stream, "//sshconnect");
    }


    /**
     * キーワード(ハイライト)設定リストを取得する。
     * @return		ハイライト設定リスト
     */
    public List<SSHconnectData> getList() {
        return listSSH;
    }

    
    /**
     * キーワード(ハイライト)リストをクリアする。
     */
    public void clearList() {
        listSSH = new ArrayList<SSHconnectData>();
    }

    /**
     * 検索Keyと一致するSSHconnectData情報を取得する
     * @param key	Property key (name)
     * @return		Property情報
     */
    public SSHconnectData getPropertySet(String key) {
        if (key == null || key.isEmpty()) return null;

        for (SSHconnectData ssh_property_set : listSSH) {
        	String k = ssh_property_set.getKey();
        	if (k.equalsIgnoreCase(key)) {
        		return ssh_property_set;
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
    public List<SSHconnectData> parseSSHProperty(InputStream stream, String path) throws Exception {

        List<SSHconnectData> list = new ArrayList<SSHconnectData>();

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
                SSHconnectData sshdata = new SSHconnectData();
                
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
                	sshdata.setProperty(key, value, commandline_option, type, i, description);
                }
                
                list.add(sshdata);

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
            org.w3c.dom.Comment comment = document.createComment(Message.getString("sshconnectproperties.document.comment")); 
            node.appendChild(comment);
        }

        if (this.listSSH == null || this.listSSH.size() <= 0) return;

        // SSHconnect property set
        for (SSHconnectData sshdata : this.listSSH) {
            org.w3c.dom.Element elem = document.createElement("sshconnect");

            {
                org.w3c.dom.Attr attr = document.createAttribute("key");
                attr.setValue(sshdata.getKey());
                elem.setAttributeNode(attr);
            }
            {
                org.w3c.dom.Attr attr = document.createAttribute("value");
                attr.setNodeValue(sshdata.getValue());
                elem.setAttributeNode(attr);
            }
            {
                org.w3c.dom.Attr attr = document.createAttribute("commandline_option");
                attr.setNodeValue(sshdata.getCommandlineOption());
                elem.setAttributeNode(attr);
            }
            {
                org.w3c.dom.Attr attr = document.createAttribute("type");
                attr.setValue(sshdata.getType());
                elem.setAttributeNode(attr);
            }
            {
                org.w3c.dom.Attr attr = document.createAttribute("description");
                attr.setValue(sshdata.getDescription());
                elem.setAttributeNode(attr);
            }
            // ノード追加
            node.appendChild(elem);
        }
    }

    /**
     * Format command line options for SSHconnect call
     * @return
     */
    public String[] getCommandLineOptions() {
    	List<String> command_options = new ArrayList<String>();
    	for (SSHconnectData sshdata : this.listSSH) {
    		String commandline_option = sshdata.getCommandlineOption();
    		String value = null;
    		if (sshdata.getKey().equalsIgnoreCase(SSHconnectProperties.BUILD_COMMAND)) {
    			// Get build command from Project Properties
    			ProjectProperties pp = this.controller.getPropertiesProject();
    			value = pp.getBuildCommand();   
    		} else {
    			// Get other properties from SSHconnect Properties
    			value = sshdata.getValue();
    		}
    		try {
    			if (value.length() > 0) {
    				command_options.add(commandline_option);
    				command_options.add(value);
    			} else {
    				System.err.println("SSHconnect option "+sshdata.getKey()+" is empty.");
    			}
    		} catch (NullPointerException e) {
    			System.err.println("SSHconnect option "+sshdata.getKey()+" is null.");
    		} 
    	}
    	return command_options.toArray(new String[0]);
    }
    
      
	public int count() {
		if (listSSH == null || listSSH.size() <= 0) {return 0;}
        return listSSH.size();
	}

	/**
	 * Get property key
	 * @param index
	 * @return
	 */
	public String getKey(int index) {
		if (listSSH == null || listSSH.size() <= 0) {return null;}
        if (listSSH.size() <= index) {return null;}

        return listSSH.get(index).getKey();
	}

	/**
	 * Get property value
	 * @param index
	 * @return
	 */
	public String getValue(int index) {
		if (listSSH == null || listSSH.size() <= 0) {return null;}
        if (listSSH.size() <= index) {return null;}

        return listSSH.get(index).getValue();
	}
	
	/**
	 * Get property description
	 * @param index
	 * @return
	 */
	public String getDescription(int index) {
		if (listSSH == null || listSSH.size() <= 0) {return null;}
        if (listSSH.size() <= index) {return null;}

        return listSSH.get(index).getDescription();
	}
	
	/**
	 * Get property order number
	 * @param index
	 * @return
	 */
	public int getOrder(int index) {
		if (listSSH == null || listSSH.size() <= 0) {return -1;}
        if (listSSH.size() <= index) {return -1;}

        return listSSH.get(index).getOrder();
	}

	/**
	 * Set "value" filed of SSHconnectData entity from listSSH. Entity defined by index.
	 * @param index
	 * @param value
	 */
	public void setValue(int index, String value) {
		SSHconnectData sshdata = listSSH.get(index);
		sshdata.setValue(value);
	}
		
	/**
	 * Set "value" filed of SSHconnectData entity from listSSH. Entity is defined by the key. 
	 * Function exits after first assignment.
	 * Keys are search ignoring character case.
	 *   
	 * @param key
	 * @param value
	 */
	private void setValueByKey(String key, String value) {
		for (SSHconnectData sshproperty : listSSH) {
			if (sshproperty.getKey().equalsIgnoreCase(key)) {
				sshproperty.setValue(value);
				return;
			}
		}		
	}	
	
	/**
	 * USE ProjectProperties.getBuildCommand() instead
	 * 
	 * Set build command.
	 * Assigns command to the "value" filed of a member of listSSH with "key" field 
	 * equal to this.build_command String.
	 * @param command
	 */
	/*public void setBuildCommand(String command) {
		setValueByKey(SSHconnectProperties.build_command, command);
	}*/	

	/**
	 * Set local path. Similar to setBuildCommand function.
	 * @param absolutePath
	 */
	public void setLocalPath(String absolutePath) {
		setValueByKey(SSHconnectProperties.LOCAL_PATH, absolutePath);		
	}

	/**
	 * Set file filter. Similar to setBuildCommand function.
	 * @param filter
	 */
	public void setFileFilter(String filter) {
		setValueByKey(SSHconnectProperties.FILE_FILTER, filter);
	}

	/**
	 * Set preprocess files. Similar to setBuildCommand function.
	 * @param files
	 */
	public void setPreprocessFiles(String files) {
		setValueByKey(SSHconnectProperties.PREPROCESS_FILES, files);		
	}

	@Override
	public void firePropertyChange() {}
}
