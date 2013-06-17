package jp.riken.kscope.properties;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import jp.riken.kscope.utils.ResourceUtils;

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
    String properties_file = ""; // path to the tile, used to read properties
    private InputStream is=null;
    
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
			is = ResourceUtils.getPropertiesFile(SSH_PROPERTIES_FILE);
			// properties_file = ResourceUtils.PROPERTIES_FILE_USED; // Use configuration file stored near K-scope property file.
			prop.load(is);
		}
		
		property_names = prop.stringPropertyNames().toArray(new String[0]);
		
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

	
	/**
	 * Save parameters to configuration file.
	 * Use java.util.Properties store() method.
	 * DO NOT try to save to the same file, properties were read from. Always save to working dir (near jars), so that SSHconnect can find it
	 * If not possible, store to SSH_PROPERTIES_FILE in SSHconnect.jar & kscope.jar folder.
	 */
	public void store() {
		if (properties_file.length() > 1) {
			try {
				FileOutputStream fos = new FileOutputStream(properties_file);
				prop.store(fos, "SSHconnect settings");

			} catch (IOException e) {
				properties_file = ""; // Couldn't write to this file. Forget it.
				e.printStackTrace();
			}
		} 
		else  
		{
			try {
				FileOutputStream fos = new FileOutputStream(SSH_PROPERTIES_FILE);
				prop.store(fos, "SSHconnect settings");
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
