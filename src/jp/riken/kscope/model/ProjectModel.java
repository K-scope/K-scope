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

package jp.riken.kscope.model;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * プロジェクト情報クラス
 * @author RIKEN
 */
public class ProjectModel  {

    /** プロジェクトタイトル */
    private String projectTitle;

    /** プロジェクトフォルダ */
    private File projectFolder;
    /** プロジェクトファイルタイプ */
    private FILE_TYPE fileType;
    /** プロジェクトファイルリスト */
    private List<SourceFile> listXmlFile;
    /** 作成日付 */
    private String createDate;
    /** 更新日付 */
    private String updateDate;
    /** プロジェクト:中間コード選択フォルダ、ファイルリスト */
    private List<File> listSearchPath;

    /**
     * コンストラクタ
     */
    public ProjectModel() {
    	listXmlFile = new ArrayList<SourceFile>();
    	listSearchPath = new ArrayList<File>();
    }

    /**
     * プロジェクトタイトルを取得する。
     * @return projectTitle		プロジェクトタイトル
     */
    public String getProjectTitle() {
        return projectTitle;
    }

    /**
     * プロジェクトタイトルを設定する
     * @param projectTitle 		プロジェクトタイトル
     */
    public void setProjectTitle(String projectTitle) {
        this.projectTitle = projectTitle;
    }

    /**
     * プロジェクトフォルダを取得する
     * @return projectFolder		プロジェクトフォルダ
     */
    public File getProjectFolder() {
        return projectFolder;
    }

    /**
     * プロジェクトフォルダを設定する。
     * @param projectFolder 		プロジェクトフォルダ
     */
    public void setProjectFolder(File projectFolder) {
        this.projectFolder = projectFolder;
    }

    /**
     * プロジェクトXMLファイルリストを取得する
     * @return listXmlFile		プロジェクトXMLファイルリスト
     */
    public List<SourceFile> getListSelectedFile() {
        return listXmlFile;
    }

    /**
     * プロジェクトXMLファイルリストを設定する
     * @param listXmlFile 		プロジェクトXMLファイルリスト
     */
    public void setListXmlFile(List<SourceFile> listXmlFile) {
        this.listXmlFile = listXmlFile;
    }

    /**
     * プロジェクトXMLファイルリストを設定する
     * @param listXmlFile 		プロジェクトXMLファイルリスト
     */
    public void setListXmlFile(SourceFile[] listXmlFile) {
        if (listXmlFile == null || listXmlFile.length <=0) return;

        this.listXmlFile.clear();
        for (int i=0; i<listXmlFile.length; i++) {
            this.listXmlFile.add(listXmlFile[i]);
        }
    }

    /**
     * プロジェクトXMLファイルを追加する
     * @param xmlFile 		プロジェクトXMLファイル
     */
    public void addProjectSelectedFile(SourceFile xmlFile) {
        if (this.listXmlFile == null || this.listXmlFile.size() <=0) {
            this.listXmlFile = new ArrayList<SourceFile>();
        }
        this.listXmlFile.add(xmlFile);
    }

    /**
     * 作成日付を取得する.
     * @return		作成日付
     */
    public String getCreateDate() {
        return createDate;
    }

    /**
     * 作成日付を設定する
     * @param createDate		作成日付
     */
    public void setCreateDate(String createDate) {
        this.createDate = createDate;
    }

    /**
     * 更新日付を取得する.
     * @return		更新日付
     */
    public String getUpdateDate() {
        return updateDate;
    }

    /**
     * 更新日付を設定する
     * @param updateDate		更新日付
     */
    public void setUpdateDate(String updateDate) {
        this.updateDate = updateDate;
    }

    /**
     * プロジェクトモデルをクリアする。
     */
    public void clearProjectModel() {

        this.createDate = null;
        this.updateDate = null;
        this.projectFolder = null;
        this.projectTitle = null;

        if (this.listXmlFile == null) {
            this.listXmlFile = new ArrayList<SourceFile>();
        }
        this.listXmlFile.clear();
    }

    /**
     * プロジェクトXMLファイル出力する.
     * @param node		出力ノード
     */
    public void writeProjectModel(org.w3c.dom.Node node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // 作成日付
        {
            if (this.createDate == null || this.createDate.isEmpty()) {
                // 作成日付
                SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
                this.createDate = format.format(new Date());
            }
            org.w3c.dom.Element elem = document.createElement("createdate");
            node.appendChild(elem);
            elem.appendChild(document.createTextNode(this.createDate));
        }
        // 更新日付:現在日付
        {
            // 更新日付
            SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
            this.updateDate = format.format(new Date());

            org.w3c.dom.Element elem = document.createElement("updatedate");
            node.appendChild(elem);
            elem.appendChild(document.createTextNode(this.updateDate));
        }

        // プロジェクトタイトル
        {
            org.w3c.dom.Element elem = document.createElement("title");
            node.appendChild(elem);
            elem.appendChild(document.createTextNode(this.getProjectTitle()));
        }
        // 選択ファイルタイプ
        {
            String typeAttr = null;
            if (this.fileType == FILE_TYPE.XCODEML_XML) {
                typeAttr = "xcodeml";
            }
            else if (this.fileType == FILE_TYPE.FORTRANLANG) {
                typeAttr = "fortran";
            }
            org.w3c.dom.Element elem = document.createElement("filetype");
            node.appendChild(elem);
            org.w3c.dom.Attr attribute = document.createAttribute("type");
            attribute.setValue(typeAttr);
            elem.setAttributeNode(attribute);
        }

        // 読込XMLファイル出力
        if (this.getListSelectedFile() != null && this.getListSelectedFile().size() > 0) {

            List<SourceFile> xmlfiles = new ArrayList<SourceFile>();
            List<SourceFile> srcfiles = new ArrayList<SourceFile>();
            for (SourceFile file : this.getListSelectedFile()) {
                if (FILE_TYPE.isFortranFile(file.getFile())) {
                    srcfiles.add(file);
                }
                else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
                    xmlfiles.add(file);
                }
            }

            if (xmlfiles.size() > 0) {
                // xcodeml要素
                org.w3c.dom.Element elem = document.createElement("xcodeml");
                String typeAttr = "f_front";
                String fileAttr = "xml";
                node.appendChild(elem);
                org.w3c.dom.Attr attribute = document.createAttribute("type");
                attribute.setValue(typeAttr);
                elem.setAttributeNode(attribute);

                for (SourceFile xml : xmlfiles) {
                    File file = xml.getFile();
                    String path = FileUtils.getRelativePath(file, this.getProjectFolder());
                    path = StringUtils.escapeFilePath(path);
                    if (path == null) continue;

                    // file要素
                    org.w3c.dom.Element elemFile = document.createElement("file");
                    org.w3c.dom.Attr attrFile = document.createAttribute("type");
                    attrFile.setValue(fileAttr);
                    elemFile.setAttributeNode(attrFile);
                    // XMLファイル名
                    elemFile.appendChild(document.createTextNode(path));

                    elem.appendChild(elemFile);
                }
            }

            if (srcfiles.size() > 0) {
                // source要素
                org.w3c.dom.Element elem = document.createElement("source");
                String typeAttr = "fortran";
                String fileAttr = "fortran";
                node.appendChild(elem);
                org.w3c.dom.Attr attribute = document.createAttribute("type");
                attribute.setValue(typeAttr);
                elem.setAttributeNode(attribute);

                for (SourceFile xml : this.getListSelectedFile()) {
                    File file = xml.getFile();
                    String path = FileUtils.getRelativePath(file, this.getProjectFolder());
                    path = StringUtils.escapeFilePath(path);
                    if (path == null) continue;
                    // file要素
                    org.w3c.dom.Element elemFile = document.createElement("file");
                    org.w3c.dom.Attr attrFile = document.createAttribute("type");
                    attrFile.setValue(fileAttr);
                    elemFile.setAttributeNode(attrFile);
                    // XMLファイル名
                    elemFile.appendChild(document.createTextNode(path));

                    elem.appendChild(elemFile);
                }
            }
        }

        // 選択フォルダ・ファイル出力
        if (this.getListSearchPath() != null && this.getListSearchPath().size() > 0) {
            List<File> list = this.getListSearchPath();

            // searchpath要素
            org.w3c.dom.Element elem = document.createElement("searchpath");
            node.appendChild(elem);

            for (File file : list) {
                String path = FileUtils.getRelativePath(file, this.getProjectFolder());
                path = StringUtils.escapeFilePath(path);
                if (path == null) continue;
                // path要素
                org.w3c.dom.Element elemFile = document.createElement("path");
                // 選択フォルダ・ファイル名
                elemFile.appendChild(document.createTextNode(path));

                elem.appendChild(elemFile);
            }
        }
    }


    /**
     * プロジェクト設定ファイルの読込を行う.
     * @param loadFile		読込設定ファイル
     * @throws Exception 		読込エラー
     */
    public void loadProjectModel(File loadFile) throws Exception {

        if (!loadFile.exists()) {
            throw(new Exception("Project Configuration is not exist.")); //プロジェクト設定ファイルが存在しません。
        }

        // リソースファイルの読込
        XmlUtils xml = new XmlUtils(loadFile);

        // 作成日付
        this.createDate = xml.getString("/project/createdate");
        // 更新日付
        this.updateDate = xml.getString("/project/updatedate");
        // タイトル
        this.projectTitle = xml.getString("/project/title");
        // プロジェクトフォルダ
        this.projectFolder = loadFile.getParentFile();
        // 選択ファイルリスト
        String type = xml.getString("/project/filetype/@type");
        if ("xcodeml".equalsIgnoreCase(type)) {
            this.fileType = FILE_TYPE.XCODEML_XML;
        }
        else if ("fortran".equalsIgnoreCase(type)) {
            // ファイルタイプ
            this.fileType = FILE_TYPE.FORTRANLANG;
        }
        else {
            this.fileType = FILE_TYPE.XCODEML_XML;
        }
        // XMLファイルリスト
        List<String> list = xml.getList("/project/xcodeml/file");
        if (list != null && list.size() > 0) {

	        for (String value : list) {
	            if (value == null) continue;

	            File file = new File(value);
	            if (!file.isAbsolute()) {
	                // 相対パスであるので、プロジェクトフォルダを付ける
	                file = new File(this.projectFolder.getAbsoluteFile() + File.separator + value);
	            }
	            if (file.exists()) {
	                SourceFile source = new SourceFile(file);
	                addProjectSelectedFile(source);
	            }
	        }
        }
        // ソースファイルリスト
        List<String> sourcelist = xml.getList("/project/source/file");
        if (sourcelist != null && sourcelist.size() > 0) {
            for (String value : sourcelist) {
                if (value == null) continue;

                File file = new File(value);
                if (!file.isAbsolute()) {
                    // 相対パスであるので、プロジェクトフォルダを付ける
                    file = new File(this.projectFolder.getAbsoluteFile() + File.separator + value);
                }
                if (file.exists()) {
                    SourceFile source = new SourceFile(file);
                    this.addProjectSelectedFile(source);
                }
            }
        }

        // 選択フォルダ・ファイルリスト
        List<String> listSelect = xml.getList("/project/searchpath/path");
        if (listSelect != null && listSelect.size() > 0) {

	        for (String value : listSelect) {
	            if (value == null) continue;

	            File file = new File(value);
	            if (!file.isAbsolute()) {
	                // 相対パスであるので、プロジェクトフォルダを付ける
	                file = new File(this.projectFolder.getAbsoluteFile() + File.separator + value);
	            }
	            addSearchPath(file);
	        }
        }
    }

    /**
     * プロジェクトの中間コード選択フォルダ・ファイルリストに追加する.
     * プロジェクト新規作成の中間コード選択時のフォルダ・ファイルを保持する.
     * @param file		中間コード選択フォルダ・ファイル
     */
    private void addSearchPath(File path) {
    	if (this.listSearchPath.contains(path)) {
    		return;
    	}
    	this.listSearchPath.add(path);
	}

    /**
     * プロジェクトの中間コード選択フォルダ・ファイルリストを取得する.
     * プロジェクト新規作成の中間コード選択時のフォルダ・ファイルを保持する.
     * @return		中間コード選択フォルダ・ファイルリスト
     */
    public List<File> getListSearchPath() {
    	return this.listSearchPath;
	}

    /**
     * プロジェクトの中間コード選択フォルダ・ファイルリストを取得する.
     * プロジェクト新規作成の中間コード選択時のフォルダ・ファイルを保持する.
     * @return		中間コード選択フォルダ・ファイルリスト
     */
    public void setListSearchPath(List<File> list) {
    	this.listSearchPath.clear();
    	this.listSearchPath.addAll(list);
	}

	/**
     * プロジェクトファイルタイプ を設定する
     * @param type		プロジェクトファイルタイプ :FILE_TYPE.XCODEML_XML or FILE_TYPE.FORTRANLANG
     */
    public void setFileType(FILE_TYPE type) {
        this.fileType = type;
    }

    /**
     * プロジェクトファイルタイプを取得する
     * @return		プロジェクトファイルタイプ
     */
    public FILE_TYPE getFileType() {
        return this.fileType;
    }

    /**
     * プロジェクト設定が有効であるかチェックします.
     * プロジェクトフォルダが設定されているかチェックする.
     * @return		true=プロジェクト設定が有効
     */
    public boolean isVaildProject() {
    	return (this.projectFolder!= null);
    }
}


