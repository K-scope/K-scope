package jp.riken.kscope.properties;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import jp.riken.kscope.utils.ResourceUtils;

/**
 * SSHconnectプロパティ設定クラス
 * @author peterbryzgalov
 *
 */
public class SSHconnectProperties {
	
	public static final String SSH_PROPERTIES_FILE = "sshconnect_conf.txt";
	
	public String[] BUILD_COMMAND = {"-m",""}; 
	public String[] LOCAL_PATH = {"-lp",""};
	public String[] FILE_FILTER = {"-ff",""};
	public String[] PREPROCESS_FILES = {"-pf",""};
	public final String[][] BUILD_PROPERTIES = {BUILD_COMMAND,LOCAL_PATH,FILE_FILTER,PREPROCESS_FILES}; // List of parameters needed for build
	
	Properties system_prop; // System properties
	Properties project_prop = new Properties(); // Project properties
    String[] property_names;
    String properties_file = ""; // path to the tile, used to read properties
    private InputStream is=null;
    
    public void setProperty(String name, String value) {
    	system_prop.setProperty(name, value);
    }

	/**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public SSHconnectProperties() throws Exception {
        loadSystemProperties();        
    }

    /**
     * Option setter
     * @param build_command
     */
    public void setBuildCommand(String build_command) {
    	this.BUILD_COMMAND[1] = "'"+build_command+"'";
    }
    
    /**
     * Option setter
     * @param local_path
     */
    public void setLocalPath(String local_path) {
    	this.LOCAL_PATH[1] = "'"+local_path + "'";
    }
    
    /**
     * Option setter
     * @param file_filter
     */
    public void setFileFilter(String file_filter) {
    	this.FILE_FILTER[1] = file_filter;
    }
    /**
     * Option setter
     * @param preprocess_files
     */
    public void setPreprocessFiles(String preprocess_files) {
    	this.PREPROCESS_FILES[1] = preprocess_files;
    }
    
    /**
     * Format command line options for SSHconnect call
     * @return
     */
    public String[] getCommandLineOptions() {
    	List<String> command_options = new ArrayList<String>();
    	for (String[] option : BUILD_PROPERTIES) {
    		if (option[1].length() > 0) {
    			command_options.add(option[0]);
    			command_options.add(option[1]);
    		} 
    	}
    	return command_options.toArray(new String[0]);
    }
    
    /**
     * プロジェクト設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadSystemProperties() throws Exception {
    	
    	// Read parameters from configuration file
		system_prop = new Properties();
		try {
			system_prop.load(new FileInputStream(SSH_PROPERTIES_FILE));
		} catch (FileNotFoundException e) {
			is = ResourceUtils.getPropertiesFile(SSH_PROPERTIES_FILE);
			// properties_file = ResourceUtils.PROPERTIES_FILE_USED; // Use configuration file stored near K-scope property file.
			system_prop.load(is);
		}
		
		property_names = system_prop.stringPropertyNames().toArray(new String[0]);
		
    }
	
    public int count() {
		return system_prop.size();		
	}

	public String getProperty(int i) {
		String name = property_names[i];
		return getProperty(name);		
	}
	
	public String getProperty(String name) {
		return system_prop.getProperty(name);		
	}

	public String getDescription(int i) {
		String name = property_names[i];
		return getDescription(name);
	}
	
	public String getDescription(String name) {
		return system_prop.getProperty(name);
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
				system_prop.store(fos, "SSHconnect settings");

			} catch (IOException e) {
				properties_file = ""; // Couldn't write to this file. Forget it.
				e.printStackTrace();
			}
		} 
		else  
		{
			try {
				FileOutputStream fos = new FileOutputStream(SSH_PROPERTIES_FILE);
				system_prop.store(fos, "SSHconnect settings");
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
