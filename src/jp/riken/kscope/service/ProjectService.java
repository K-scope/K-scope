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
package jp.riken.kscope.service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.MemorybandProperties;
import jp.riken.kscope.properties.OperandProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.SSHconnectProperties;
import jp.riken.kscope.properties.SourceProperties;


/**
 * プロジェクトの管理を行うサービスクラス
 * @author riken
 */
public class ProjectService extends BaseService {

    /** プロジェクトモデル */
    private ProjectModel projectModel;
    /** キーワードプロパティ */
    private KeywordProperties propertiesKeyword;
    /** 外部ツールプロパティ */
    private ProgramProperties propertiesExtension;
    /** 演算カウントプロパティ */
    private OperandProperties propertiesOperand;
    /** ソースビュー設定 */
    private SourceProperties propertiesSource;
    /** プロファイラプロパティ設定 */
    private ProfilerProperties propertiesProfiler;
    /** プロジェクト設定 */
    private ProjectProperties propertiesProject;
    /** 要求Byte/FLOP設定プロパティ */
    private MemorybandProperties propertiesMemory;
    
    private SSHconnectProperties propertiesSSH;


    /**
     * コンストラクタ
     */
    public ProjectService() {
        setProjectModel(new ProjectModel());
    }

    /**
     * コンストラクタ
     * @param model		プロジェクトモデル
     */
    public ProjectService(ProjectModel model) {
        this.setProjectModel(model);
    }

    /**
     * 新規プロジェクトを作成する
     * @param title			プロジェクトタイトル
     * @param projectFolder		プロジェクトフォルダ
     * @param list			    ファイルリスト
     * @param type				ファイルタイプ
     * @return			プロジェクト情報モデル
     */
    public ProjectModel createProject(String title, File projectFolder, List<File> list, FILE_TYPE type) {
        if (this.projectModel == null) {
            setProjectModel(new ProjectModel());
        }

        // プロジェクトタイトル
        this.projectModel.setProjectTitle(title);
        // プロジェクトフォルダ
        this.projectModel.setProjectFolder(projectFolder);
        // ファイルタイプ
        this.projectModel.setFileType(type);
        // プロジェクト選択ファイル
        if (list != null) {
            SourceFile[] listFile = getSourceFiles(list.toArray(new File[0]), type, true);
            this.projectModel.setListXmlFile(listFile);
        }
        // プロジェクト選択フォルダ・ファイル
        this.projectModel.setListSearchPath(list);

//        this.propertiesProject.setProjectFolder(projectFolder.toString());

        return projectModel;
    }

    /**
     * プロジェクトにXMLフォルダを追加する。
     * @param listAddFile		追加ファイルリスト
     * @return		成否
     */
    public boolean addProjectSelectedFile(List<File> listAddFile) {

        if (listAddFile == null || listAddFile.size() <= 0) return false;

        // 追加XMLファイルの取得
        SourceFile[] listAdd = getSourceFiles(listAddFile.toArray(new File[0]), FILE_TYPE.XCODEML_XML, true);
        if (listAdd == null) return false;

        // 重複ファイルチェック
        List<SourceFile> listXml = this.projectModel.getListSelectedFile();
        for (SourceFile addfile : listAdd) {
            if (!listXml.contains(addfile)) {
                listXml.add(addfile);
            }
        }

        return true;
    }

    /**
     * プロジェクトにFortranフォルダを追加する。
     * @param listAddFile		追加ファイルリスト
     * @return		成否
     */
    public boolean addProjectFortranFile(List<File> listAddFile) {

        if (listAddFile == null || listAddFile.size() <= 0) return false;

        // 追加XMLファイルの取得
        SourceFile[] listAdd = getSourceFiles(listAddFile.toArray(new File[0]), FILE_TYPE.FORTRANLANG, true);
        if (listAdd == null) return false;

        // 重複ファイルチェック
        List<SourceFile> listXml = this.projectModel.getListSelectedFile();
        for (SourceFile addfile : listAdd) {
            if (!listXml.contains(addfile)) {
                listXml.add(addfile);
            }
        }

        return true;
    }

    /**
     * プロジェクトからXMLファイルを削除する。
     * @param listDeleteFile		削除ファイルリスト
     * @return		成否
     */
    public boolean deleteProjectSelectedFile(List<SourceFile> listDeleteFile) {

        // 登録XMLファイルリスト
        List<SourceFile> listXml = this.projectModel.getListSelectedFile();

        // ファイルチェック
        for (SourceFile delfile : listDeleteFile) {
            if (listXml.contains(delfile)) {
                listXml.remove(delfile);
            }
        }

        return true;
    }

    /**
     * サブディレクトリからファイル一覧を取得する。
     *
     * @param dir            選択ファイルリスト
     * @param ftype            選択言語タイプ
     * @param subDir            サブディレクトリを検索するかどうかのフラグ（true:サブディレクトリ検索/false:検索しない)
     * @return サブディレクトリのファイルリスト
     */
    private File[] searchFiles(File dir, FILE_TYPE ftype, boolean subDir) {
    	if (dir == null) return null;
    	// settings.ppaフォルダは追加しない。
    	if (dir.isDirectory() && KscopeProperties.SETTINGS_FOLDER.equalsIgnoreCase(dir.getName())) {
    		return null;
    	}

        ArrayList<File> sublist = new ArrayList<File>();
        FileFilter filter = ftype.getFileFilter();
        // ディレクトリ内のファイル一覧を取得する。
        File[] fileList = dir.listFiles();
        for (int i = 0; i < fileList.length; i++) {
            // サブディレクトリ検索フラグがtrueの場合のみ、サブディレクトリを検索する。
            if (subDir && fileList[i].isDirectory()) {
                File[] files = searchFiles(fileList[i], ftype, subDir);
                if (files != null && files.length > 0) {
                    sublist.addAll(java.util.Arrays.asList(files));
                }
            } else if (fileList[i].isFile()) {
                // ファイルフィルタがnullの場合は、無条件追加
                if (filter == null) {
                    sublist.add(fileList[i]);
                } else if (filter.accept(fileList[i])) {
                    sublist.add(fileList[i]);
                }
            }
        }
        if (sublist.size() == 0) return null;

        return (File[]) sublist.toArray(new File[0]);
    }

    /**
     * ファイルからSourceFileオブジェクトを作成して、一覧を取得する。
     * @param files            選択ファイルリスト
     * @param ftype            ファイルタイプ
     * @param subDir            サブディレクトリ検索フラグ
     * @return SourceFileリスト
     */
    public SourceFile[] getSourceFiles(File files[], FILE_TYPE ftype,
            boolean subDir) {

        ArrayList<File> filelist = new ArrayList<File>();
        ArrayList<SourceFile> sourcelist = new ArrayList<SourceFile>();
        for (int i = 0; i < files.length; i++) {
            File sourceFiles[] = null;
            if (files[i].isDirectory()) {
                sourceFiles = searchFiles(files[i], ftype, subDir);
            } else if (files[i].isFile()) {
                sourceFiles = new File[1];
                sourceFiles[0] = files[i];
            }

            if (sourceFiles == null) continue;
            for (int j = 0; j < sourceFiles.length; j++) {
                if (!filelist.contains(sourceFiles[j])) {
                	filelist.add(sourceFiles[j]);
                }
            }
        }

        for (int j = 0; j < filelist.size(); j++) {
            // 自動判定の場合は、ファイル拡張子からファイルタイプを取得する。
            FILE_TYPE type = FILE_TYPE.getFileType(filelist.get(j));
            if (type != FILE_TYPE.UNKNOWN) {
                SourceFile src = new SourceFile(filelist.get(j), type);
                sourcelist.add(src);
            }
        }
        if (sourcelist.size() == 0) return null;

        return (SourceFile[]) sourcelist.toArray(new SourceFile[0]);
    }

    /**
     * ディレクトリからSourceFileオブジェクトを作成して、一覧を取得する。
     *
     * @param dir            選択ディレクトリ
     * @param ftype            言語タイプ
     * @param subDir            サブディレクトリ検索フラグ
     * @return SourceFileリスト
     */
    public SourceFile[] getSourceFiles(File dir, FILE_TYPE ftype, boolean subDir) {
        File files[] = { dir };
        return getSourceFiles(files, ftype, subDir);
    }

    /**
     * プロジェクトモデルを取得する。
     * @return projectModel		プロジェクトモデル
     */
    public ProjectModel getProjectModel() {
        return projectModel;
    }

    /**
     * プロジェクトモデルを設定する
     * @param projectModel 		プロジェクトモデル
     */
    public void setProjectModel(ProjectModel projectModel) {
        this.projectModel = projectModel;
    }


    /**
     * プロジェクトを保存する
     * @param	saveFolder	プロジェクトフォルダ
     * @throws Exception   プロジェクト保存エラー
     */
    public void saveProject(File saveFolder) throws Exception {

        try {
            // プロジェクトXMLファイル出力
            writeProjectModel(saveFolder);

            // プロパティのXMLファイル出力
            writeProperties(saveFolder);

        } catch (Exception ex) {
            // エラーメッセージ出力
            this.addErrorInfo(ex.getMessage());

            throw(ex);
        }
    }


    /**
     * プロジェクトXMLファイル出力する
     * @param saveFolder    保存フォルダ
     * @throws Exception 		プロパティ出力エラー
     */
    private void writeProjectModel(File saveFolder) throws Exception {

        // ドキュメント作成
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docbuilder = dbfactory.newDocumentBuilder();
        org.w3c.dom.Document document = docbuilder.newDocument();

        // project要素:ルート要素
        org.w3c.dom.Element root = document.createElement("project");
        document.appendChild(root);

        // プロジェクトモデル情報のノード出力
        this.projectModel.writeProjectModel(root);

        // 出力プロジェクトファイル
        File saveFile = new File(saveFolder.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);

        // XML ファイル出力
        writeXmlFile(saveFile, document);

    }


    /**
     * プロパティのXMLファイル出力する
     * @param saveFolder    保存フォルダ
     * @throws Exception 		プロパティ出力エラー
     */
    private void writeProperties(File saveFolder) throws Exception {

        // ドキュメント作成
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docbuilder = dbfactory.newDocumentBuilder();
        org.w3c.dom.Document document = docbuilder.newDocument();

        // properties要素:ルート要素
        org.w3c.dom.Element root = document.createElement("properties");
        document.appendChild(root);

        // settings要素
        org.w3c.dom.Element elemSettings = document.createElement("settings");
        root.appendChild(elemSettings);

        // ソースビュー設定出力
        this.propertiesSource.writeProperties(elemSettings);
        // キーワードプロパティ設定出力
        this.propertiesKeyword.writeProperties(elemSettings);
        // 外部ツールプロパティ設定出力
        this.propertiesExtension.writeProperties(elemSettings);
        // 演算カウントプロパティ設定出力
        this.propertiesOperand.writeProperties(elemSettings);
        // プロファイラプロパティ設定出力
        this.propertiesProfiler.writeProperties(elemSettings);
        // プロジェクトプロパティ設定出力
        this.propertiesProject.writeProperties(elemSettings, this.projectModel.getProjectFolder());
        // 要求Byte/FLOP設定プロパティ設定出力
        this.propertiesMemory.writeProperties(elemSettings);
        this.propertiesSSH.writeProperties(elemSettings);

        // settingsフォルダ作成
        File settingsFolder = new File(saveFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
        if (!settingsFolder.exists()) {
            settingsFolder.mkdir();
        }

        // 出力ファイル
        File settingsXml = new File(settingsFolder.getAbsoluteFile() + File.separator + KscopeProperties.PROPERTIES_FILE);

        // XML ファイル出力
        writeXmlFile(settingsXml, document);

    }


    /**
     * XMLドキュメントをファイル出力する
     * @param output		出力ファイル
     * @param document		XMLドキュメント
     * @throws Exception    XML出力エラー
     */
    private void writeXmlFile(File output, org.w3c.dom.Document document) throws Exception {

        // DOM出力
        TransformerFactory transFactory = TransformerFactory.newInstance();
        transFactory.setAttribute("indent-number", 4);
        Transformer transformer = transFactory.newTransformer();

        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.METHOD, "xml");

        DOMSource source = new DOMSource(document);

        FileOutputStream os = new FileOutputStream(output);
        StreamResult result = new StreamResult(new OutputStreamWriter(os, "utf-8"));
        transformer.transform(source, result);

    }


    /**
     * キーワードプロパティを設定する
     * @param propertiesKeyword		キーワードプロパティ
     */
    public void setPropertiesKeyword(KeywordProperties propertiesKeyword) {
        this.propertiesKeyword = propertiesKeyword;
    }

    /**
     * 外部ツールプロパティを設定する
     * @param propertiesExtension		外部ツールプロパティ
     */
    public void setPropertiesExtension(ProgramProperties propertiesExtension) {
        this.propertiesExtension = propertiesExtension;
    }

    /**
     * 演算カウントプロパティを設定する
     * @param propertiesOperand			演算カウントプロパティ
     */
    public void setPropertiesOperand(OperandProperties propertiesOperand) {
        this.propertiesOperand = propertiesOperand;
    }

    /**
     * ソースビュー設定を設定する
     * @param propertiesSource		ソースビュー設定
     */
    public void setPropertiesSource(SourceProperties propertiesSource) {
        this.propertiesSource = propertiesSource;
    }

    /**
     * プロファイラプロパティ設定を設定する
     * @param propertiesProfiler		プロファイラプロパティ設定
     */
    public void setPropertiesProfiler(ProfilerProperties propertiesProfiler) {
        this.propertiesProfiler = propertiesProfiler;
    }

    /**
     * プロジェクト設定を設定する
     * @param propertiesProject		プロジェクト設定
     */
    public void setPropertiesProject(ProjectProperties propertiesProject) {
    	this.propertiesProject = propertiesProject;
    }

    /**
     * 要求Byte/FLOP設定プロパティ設定を設定する
     * @param propertiesMemory		要求Byte/FLOP設定プロパティ
     */
    public void setPropertiesMemory(MemorybandProperties propertiesMemory) {
    	this.propertiesMemory = propertiesMemory;
    }
    
    public void setPropertiesSSH(SSHconnectProperties propertiesSSH) {
    	this.propertiesSSH = propertiesSSH;
    }

    /**
     * プロジェクトを開く
     * @param openFolder		プロジェクトフォルダ
     * @throws Exception        プロジェクトオープンエラー
     */
    public void openProject(File openFolder) throws Exception {
        try {
            // プロジェクト設定ファイル
            File projectFile = new File(openFolder.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);
            // プロジェクト設定ファイルのロード
            this.projectModel.loadProjectModel(projectFile);

            // プロジェクトプロパティのロード
            // settingsフォルダ
            File settingsFolder = new File(openFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);

            // 出力ファイル
            File settingsXml = new File(settingsFolder.getAbsoluteFile() + File.separator + KscopeProperties.PROPERTIES_FILE);

            // ソースビュー設定出力
            this.propertiesSource.loadProperties(settingsXml);
            // キーワードプロパティ設定出力
            this.propertiesKeyword.loadProperties(settingsXml);
            // 外部ツールプロパティ設定出力
            this.propertiesExtension.loadProperties(settingsXml);
            // 演算カウントプロパティ設定出力
            this.propertiesOperand.loadProperties(settingsXml);
            // プロファイラプロパティ設定出力
            this.propertiesProfiler.loadProperties(settingsXml);
            // プロジェクト設定出力
            this.propertiesProject.loadProperties(settingsXml);
            // 要求Byte/FLOP設定プロパティ
            this.propertiesMemory.loadProperties(settingsXml);
            this.propertiesSSH.loadProperties(settingsXml);

        } catch (Exception ex) {
            // エラーメッセージ出力
            this.addErrorInfo(ex.getMessage());

            throw(ex);
        }

    }


    /**
     * プロジェクトのプロパティを設定する
     * @param model			プロパティモデル
     */
    public void setProperties(PropertiesTableModel model) {

        String[] items = {
            Message.getString("projectservice.properties.name"), //プロジェクト名
            Message.getString("projectservice.properties.folder"), //プロジェクトフォルダ
            Message.getString("projectservice.properties.createdate"), //作成日時
            Message.getString("projectservice.properties.updatedate") //更新日時
        };
        String[] values = new String[4];
        if (this.projectModel != null) {
            // プロジェクト名
            values[0] = this.projectModel.getProjectTitle();
            if (this.projectModel.getProjectFolder() != null) {
                // プロジェクトフォルダ
                values[1] = this.projectModel.getProjectFolder().getAbsolutePath();
            }
            // 作成日時
            values[2] = this.projectModel.getCreateDate();
            // 更新日時
            values[3] = this.projectModel.getUpdateDate();
        }

        // プロパティモデルに設定する
        // プロパティパネルへの通知はObserverにて通知される。
        model.setTitle(Message.getString("projectservice.properties.title")); //プロジェクトプロパティ
        model.setProperties(items, values);

    }

    /**
     * プロパティ設定が完了しているか確認
     * @return    true=プロパティ設定完了
     */
    public boolean existAllProperties() {
    	if(this.propertiesExtension == null) return false;
    	if(this.propertiesKeyword == null) return false;
    	if(this.propertiesMemory == null) return false;
    	if(this.propertiesOperand == null) return false;
    	if(this.propertiesProfiler == null) return false;
    	if(this.propertiesProject == null) return false;
    	if(this.propertiesSource == null) return false;
    	return true;
    }

}


