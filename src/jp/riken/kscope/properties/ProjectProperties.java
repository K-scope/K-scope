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
import jp.riken.kscope.data.BasicPropertyList;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * プロジェクトプロパティ設定クラス
 * @author RIKEN
 */
public class ProjectProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    // プロパティキー
    /** プロジェクトタイトルプロパティキー */
    public static final String PRJ_TITLE = "project-title";
    /** Build command */
    public static final String BUILD_COMMAND = "build-command";
       
    public static String LOCAL_PATH = "local_path";

    /** New properties for building on server */
    public static final String USE_SERVER = "remote-build";
    
    public static final String SETTINGS_FILE = "settings-file";
    
    //中間コードの生成
    public static final String GENERATE_XML = "generate-XML";

    //フルモード
    public static final String FULL_PROJECT = "full-project";

    /** プロパティ設定リスト */
    private List<ProjectPropertyValue> listProperty = new ArrayList<ProjectPropertyValue>();
    /** Project hidden properties */
    private BasicPropertyList listHiddenProperty = null;

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public ProjectProperties() throws Exception {
        loadProperties();
    }

    /**
     * プロジェクト設定プロパティをデフォルト設定ファイルから読み込む。
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
        InputStream is = null;

        // リソースファイルの読込
        is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
        loadProperties(is);
        
        is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
        loadPropertiesOther(is);
    }

    /**
     * ソース設定プロパティを設定ファイルから読み込む。
     *
     * @param propertiesFile ソース設定プロパティ設定ファイル
     * @throws Exception プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {
        if (!propertiesFile.exists()) {
            throw (new Exception(Message.getString("propertiesbase.exeption.notexist"))); //プロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);
        // ソース設定プロパティを設定ファイルから読み込む。
        loadProperties(stream);
        
        stream = new FileInputStream(propertiesFile);
        loadPropertiesOther(stream);
    }

    /**
     * プロジェクト設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(InputStream stream ) throws Exception {
        // XMLファイルのパース
    	List<ProjectPropertyValue> list = null;
    	list = parseProjectProperty(stream, "//project");
    	if (list != null) {
    		this.listProperty = list;
    	}
    }
    
    /**
     * プロジェクト設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadPropertiesOther(InputStream stream ) throws Exception {
    	// Read project hidden properties
    	this.listHiddenProperty = new BasicPropertyList(stream, "//project_other");
    }
        
     /**
     * キーワードを取得する
     * @param stream		XML入力ストリーム
     * @param path		キーワードXPATH
     * @return		キーワードリスト
     * @throws Exception 		キーワードパースエラー
     */
    public List<ProjectPropertyValue> parseProjectProperty(InputStream stream, String path) throws Exception {

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

        for (int i=0; i<nodelist.getLength(); i++) {
            try {
                Node node = nodelist.item(i);

                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode;
                String type = "text";
                String key = "";
                String value = "";
                String name = "";
                String message = "";

                // タイプ
                attrnode = attrs.getNamedItem("type");
                if (attrnode != null) {
                	type = attrnode.getNodeValue();
                }
                // キー
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
                	}
                	else {
                		value = attrnode.getNodeValue();
                	}
                }
                // 名前
                attrnode = attrs.getNamedItem("name");
                if (attrnode != null) {
                    name = attrnode.getNodeValue();
                }
                // メッセージ
                attrnode = attrs.getNamedItem("message");
                if (attrnode != null) {
                	message = attrnode.getNodeValue();
                }

                list.add(new ProjectPropertyValue(type, key, value, name, message));

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
     * プロパティ値を設定する.
     *
     * @param value	プロパティ値
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

        // 新規追加
        this.listProperty.add(value);
    }

    /**
     * プロパティ値を取得する.
     * @param key	キー
     * @return   プロパティ値
     */
    public ProjectPropertyValue getPropertyValue(String key) {
        if (key == null) {
        	System.err.println("Key "+key+" is null");
            return null;
        }

        for (ProjectPropertyValue property : this.listProperty) {
            if (key.equalsIgnoreCase(property.getKey())) {
                return property;
            }
        }
        System.err.println("Property "+key+" not found");
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
     * プロパティをDOMノードに出力する
     * @param node	出力ノード
     * @param projectFolder	プロジェクトフォルダ(=出力フォルダ)
     */
    public void writeProperties(org.w3c.dom.Element node, File projectFolder) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("projectproperties.document.comment")); //プロジェクトプロパティ
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
        		/*if (MAKEFILE_PATH.equalsIgnoreCase(key)) {
        			if (StringUtils.isNullOrEmpty(attr_value)) {
        				attr_value = "";
        			}
        			else if (FileUtils.isAbsolutePath(attr_value)) {
        				File f = new File(value.getValue());
        				attr_value = FileUtils.getRelativePath(f, projectFolder);
        			}
        		}*/
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

        	node.appendChild(elem);
        }
        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment("Project hidden properties"); // Project hidden properties
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
     * プロパティ値リストを取得する.
     * @return    プロパティ値リスト
     */
    public ProjectPropertyValue[] getPropertyValues() {
    	return this.listProperty.toArray(new ProjectPropertyValue[0]);
    }
    
    /**
     * build コマンドの設定
     * @param  command    makeコマンド
     */    
    public void setBuildCommand(String build_command) {
    	setValueByKey(BUILD_COMMAND, build_command);
	}

    /**
     * プロジェクトタイトルの設定
     * @param   title    プロジェクトタイトル
     */
    public void setProjectTitle(String title) {
    	setValueByKey(PRJ_TITLE, title);
    }
    
    /**
	 * Set local path. Similar to setBuildCommand function.
	 * @param absolutePath
	 */
	public void setLocalPath(String absolutePath) {
		setValueByKey(LOCAL_PATH, absolutePath);		
	}

	/**
	 * Set local path. Similar to setBuildCommand function.
	 * @param absolutePath
	 */
	public void setSettingsFile(String name) {
		setValueByKey(SETTINGS_FILE, name);		
	}
	
	
    /**
	 * キーを指定してプロパティを設定
	 * @param  key    キー
	 * @param  value  値
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

    /**
     * Get BUILD COMMAND
     */
    public String getBuildCommand() {
    	ProjectPropertyValue bc = getPropertyValue(BUILD_COMMAND);
    	String bcs = bc.getValue();
    	return bcs;
    }
    
    /**
     * Get SETTINGS_FILE
     */
    public String getRemoteSettingsFile() {
    	ProjectPropertyValue ppv = getPropertyValue(SETTINGS_FILE);
    	String s = ppv.getValue();
    	return s;
    }

    /** 
     * True if project Rebuild action can be performed.
     * Depends on "full_project" and "generate_xml" properties from properties.xml
     * @return
     */
	public boolean canRebuild() {
		return testValue(getHiddenPropertyValue(ProjectProperties.FULL_PROJECT)) && testValue(getHiddenPropertyValue(ProjectProperties.GENERATE_XML));		
	}

	/**
     * True if project is full – i.e. includes intermediate code.
     * @return 
     */
    public boolean isFullProject() {
    	return testValue(getHiddenPropertyValue(ProjectProperties.FULL_PROJECT));    	
	}
    
    /**
     * True if need to build the project (locally or remotely) to generate intermediate code.
     * @return
     */
    public boolean generateXML() {
    	return testValue(getHiddenPropertyValue(ProjectProperties.GENERATE_XML));    	
	}
    
    /**
	 * Return boolean representation of String value ("true" or not "true") of project hidden property.
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
	
    /**
	 *  Display error and print out hidden properties list
	 */
	private void noHiddenProperties() {
		System.err.println("Project hidden properties not set. Check properties.xml file.\nList of hidden properties:\n");
		String[] hidden_pproperties = getHiddenPropertyPairs();
		for (String s : hidden_pproperties) {
			System.err.println(s);
		}
	}
	
	// NO MORE NEED IN REBUILD FLAG. "REBUILD" MENU DEPENDS ONLY ON GENERATEXML AND FULL_PROJECT PROPERTIES
	// DELETED : public void setRebuildFlag(boolean flag) {}
		
	/**
	 * Return true if project uses server for building Fortran project.
	 * Returns boolean "true" if property use-server is set to String "true". 
	 * @return
	 */
	public boolean useServer() {
		return getPropertyValue(USE_SERVER).getValue().equalsIgnoreCase("true");
	}
}
