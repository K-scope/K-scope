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
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * プロジェクトプロパティ設定クラス
 * @author riken
 */
public class ProjectProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    // プロパティキー
    /** makeコマンドプロパティキー */
    public static final String MAKE_COMMAND = "make-command";
    /** Makefileパスプロパティキー */
    public static final String MAKEFILE_PATH = "makefile-path";
    /** プロジェクトタイトルプロパティキー */
    public static final String PRJ_TITLE = "project-title";

    /** プロパティ設定リスト */
    private List<ProjectPropertyValue> listProperty = new ArrayList<ProjectPropertyValue>();

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
    		listProperty = list;
    	}
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
            return null;
        }

        for (ProjectPropertyValue property : this.listProperty) {
            if (key.equalsIgnoreCase(property.getKey())) {
                return property;
            }
        }
        return null;
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
        		if (MAKEFILE_PATH.equalsIgnoreCase(key)) {
        			if (StringUtils.isNullOrEmpty(attr_value)) {
        				attr_value = "";
        			}
        			else if (FileUtils.isAbsolutePath(attr_value)) {
        				File f = new File(value.getValue());
        				attr_value = FileUtils.getRelativePath(f, projectFolder);
        			}
        		}
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
    }


    /**
     * プロパティ値リストを取得する.
     * @return    プロパティ値リスト
     */
    public ProjectPropertyValue[] getPropertyValues() {
    	return this.listProperty.toArray(new ProjectPropertyValue[0]);
    }

    /**
     * make コマンドの設定
     * @param  command    makeコマンド
     */
    public void setMakeCommand(String command) {
    	setValueByKey(MAKE_COMMAND, command);
    }

    /**
     * makefileパスの設定
     * @param   path    makefileパス
     */
    public void setMakefilePath(String path) {
    	setValueByKey(MAKEFILE_PATH, path);
    }

    /**
     * プロジェクトタイトルの設定
     * @param   title    プロジェクトタイトル
     */
    public void setProjectTitle(String title) {
    	setValueByKey(PRJ_TITLE, title);
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
     * makeコマンドリストを取得します.
     * @return    makeコマンドリスト
     */
    public String[] getMakeCommandList() {
    	String command = getPropertyValue(MAKE_COMMAND).getValue();
    	if (command == null) return null;
    	String[] commands = StringUtils.tokenizerDelimit(command, " ");
    	String makefile = getPropertyValue(MAKEFILE_PATH).getValue();
    	if (makefile == null) {
    		return commands;
    	}
    	String name = new File(makefile).getName();
    	List<String> list = new ArrayList<String>();
    	list.addAll(Arrays.asList(commands));
    	list.add("-f");
    	list.add(name);
    	return list.toArray(new String[0]);
    }

    /**
     * makeコマンドを取得します.
     * @return    makeコマンド
     */
    public String getMakeCommand() {
    	String[] commands = getMakeCommandList();
    	if (commands == null) return null;
    	String command = "";
    	for (int i=0; i<commands.length; i++) {
    		if (!command.isEmpty()) command += " ";
    		command += commands[i];
    	}
    	return command;
    }

    /**
     * makeコマンドパスを取得します.
     * @return    makeコマンドパス
     */
    public String getMakefileFolder() {
    	String makefile = getPropertyValue(MAKEFILE_PATH).getValue();
    	if (makefile == null) return null;
    	String path = new File(makefile).getParent();
    	return path;
    }

    /**
     * makeコマンドパスを取得します.
     * @param projectFolder    プロジェクトフォルダ
     * @return    makeコマンドパス
     */
    public String getMakefileFolder(File projectFolder) {
    	String makefile = getPropertyValue(MAKEFILE_PATH).getValue();
    	String path = null;
    	if (makefile == null) return null;
    	if (new File(makefile).isAbsolute()) {
        	path = new File(makefile).getParent();
        	return path;
    	}
    	else {
    		try {
    			File file = new File(projectFolder.getAbsolutePath() + File.separator + makefile);
            	path = file.getCanonicalFile().getParent();
			} catch (IOException e) {}
    	}
    	return path;
    }


}
