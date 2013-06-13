package jp.riken.kscope.properties;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

/**
 * SSHconnectプロパティ設定クラス
 * @author peterbryzgalov
 *
 */
public class SSHconnectProperties {
	
	public static final String SSH_PROPERTIES_FILE = "sshconnect_conf.txt";
	
	/** プロパティ設定リスト */
	
	/*// Initializing parameters with default values
    String host = ""; // host IP
    
    String user = ""; // username for SSH connection  
    String password = ""; // password for SSH connection 
    int port; // default SSH port
    String key="",passphrase="";
    
	// local project folder. Must contain Makefile and all files necessary for building.  
    String default_local_path = ""; 
    String remote_path = ""; 
    // makefiles to look for replacement placeholders
    String default_makefiles_process = ""; 
    String default_make = "";
    String file_filter = "";*/
    
    Properties prop;
    String[] property_names;
    
    public void setProperty(String name, String value) {
		prop.setProperty(name, value);
	}
	
	/**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public SSHconnectProperties() throws Exception {
        loadProperties();
    }

    
    /**
     * プロジェクト設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
    	
    	// Read parameters from configuration file
		prop = new Properties();
		try {
			prop.load(new FileInputStream(SSH_PROPERTIES_FILE));
		} catch (FileNotFoundException e) {
			// TODO Add feature to read from alternative location, like in:  is = ResourceUtils.getPropertiesFile(PROPERTIES_FILE);
			//prop.load(new FileInputStream(resource_path + conf_filename));
		}
		
		property_names = prop.stringPropertyNames().toArray(new String[0]);
		/*System.out.println("SSH settings:");
		for (String name : property_names) {
			System.out.println(name + " : " + getProperty(name));
		}*/
		/*
		default_make = updateProperty(prop,"make");
		default_local_path = updateProperty(prop, "local_path");  
		host = updateProperty(prop, "host");
		user = updateProperty(prop, "user");
		if (user.length() < 1) throw new InvalidPreferencesFormatException("'user' property not found in "+SSH_PROPERTIES_FILE+". This is a required propery. Set ssh user name for connecting to remote server.");
		password = updateProperty(prop, "password"); // If password == "" authenticate with key.
		key = updateProperty(prop,"key");
		passphrase = updateProperty(prop,"passphrase");
		
		try {
			port = Integer.parseInt(updateProperty(prop, "port"));
		} catch (NumberFormatException e) {
			port = 22;
			System.err.println("'port' propery not found or not a number in "+SSH_PROPERTIES_FILE+". Default port 22 is used.");
		}
		  			
		remote_path = updateProperty(prop, "remote_path");     
		if (remote_path.length() < 1) throw new InvalidPreferencesFormatException("'remote_path' property not found in "+SSH_PROPERTIES_FILE+". This is a required propery. Set remote path on server to create temporary directories.");
		
		String ff = updateProperty(prop, "file_filter");
		// *.origin - reserved for original copies of edited make files.
		if (ff != null && ff.length() > 1) file_filter = ff +",*.origin";
		else System.err.println("'file_filter' property not found in "+SSH_PROPERTIES_FILE+". Default is used: "+ file_filter);
		
		// Makefiles to look into for replacement pattern 
		default_makefiles_process = updateProperty(prop, "makefiles");
		*/
    }
	
    public int count() {
		return prop.size();		
	}

	public String getProperty(int i) {
		String name = property_names[i];
		return getProperty(name);		
	}
	
	public String getProperty(String name) {
		return prop.getProperty(name);		
	}

	public String getDescription(int i) {
		String name = property_names[i];
		return getDescription(name);
	}
	
	public String getDescription(String name) {
		return prop.getProperty(name);
	}

	public String getPropertyName(int i) {
		return property_names[i];
	}

	public void store() {
		try {
			prop.store(new FileOutputStream(SSH_PROPERTIES_FILE), "SSHconnect settings");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
