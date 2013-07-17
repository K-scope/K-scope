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
package jp.riken.kscope.action;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.FileProjectNewDialog;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.SSHconnectProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectMakeService;
import jp.riken.kscope.service.ProjectService;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * プロジェクトの新規作成アクション
 * @author riken
 */
public class FileProjectNewAction extends ActionBase {
	/** 中間コードを生成するクラス */
	private ProjectMakeService makeService;
    /** プロジェクトのクリアアクション */
    private ProjectClearLanguageAction clearAction;
    /** データベース構築サービス */
    private LanguageService languageService;
    /** プロジェクト構築サービス */
    private ProjectService projectService;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileProjectNewAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
	public boolean validateAction() {
        // スレッドタスクの実行状態をチェックする
        return this.controller.isThreadTaskDone();
	}


    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        final String message = Message.getString("mainmenu.file.newproject"); //プロジェクトの新規作成
        SSHconnectProperties sshc_properties = null;
        
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 最終アクセスフォルダ
        String currentFolder = this.controller.getLastAccessFolder();

        // プロジェクトの新規作成ダイアログを表示する。
        FileProjectNewDialog dialog = new FileProjectNewDialog(frame, true);
        dialog.setLastAccessFolder(currentFolder);
        // 除外パス名を設定する
        dialog.addExcludeName(KscopeProperties.SETTINGS_FOLDER);
        // プロジェクトプロパティにmakeコマンドがある場合はその文字列を表示
        dialog.setMakeCommand(this.controller.getPropertiesProject().getPropertyValue(ProjectProperties.BUILD_COMMAND).getValue());
        // タイトル, Makefile, 保存フラグの設定削除 at 2013/05/30 by @hira
        // プロジェクトプロパティにタイトル設定がある場合その文字列を表示
        // dialog.setProjectTitle(this.controller.getPropertiesProject().getPropertyValue(ProjectProperties.PRJ_TITLE).getValue());
        // プロジェクトプロパティにMakefileパスがある場合はそのパスを表示
        // dialog.setMakefilePath(this.controller.getPropertiesProject().getPropertyValue(ProjectProperties.MAKEFILE_PATH).getValue());
        // プロジェクト作成直後にプロジェクトを保存するかどうかを設定
        //　dialog.setSaveFlag(this.controller.getPropertiesApplication().getSaveProjectAfterCreate());

        int result = dialog.showDialog();

        if (result != Constant.OK_DIALOG) {
            Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
            return;
        }

        // 最終アクセスフォルダ
        this.controller.setLastAccessFolder(new File(dialog.getProjectFolder()));
        // 中間コードの生成を行うか否か
        boolean genCode = dialog.isGenerateIntermediateCode();
        
        boolean useSSHconnect = dialog.useSSHconnect();
        
        // 選択ソース
        boolean selectedXml = dialog.isSelectedXml();
        FILE_TYPE type = FILE_TYPE.XCODEML_XML;
        if (!genCode && !selectedXml) {
            // Fortranソースファイル
            type = FILE_TYPE.FORTRANLANG;
        }
        try {
        	//String makeCom = dialog.getMakeCommand();  // make command as set in New Project dialog. Full path if executable file. 
        	//String makefilePath = dialog.getMakefilePath(); // path to makefile as set in New Project dialog. If set " " (space), makefilePath = " ".
        	String build_command = dialog.getBuildCommand(); // build command as set in text field in New Project dialog.
        	
            // プロジェクトを閉じる
            FileProjectCloseAction closeAction = new FileProjectCloseAction(this.controller);
            closeAction.clearProject();

            // プロジェクト情報
            ProjectModel project = this.controller.getProjectModel();
            // プロジェクトの新規作成
            projectService = new ProjectService(project);
            projectService.createProject(dialog.getPeojectTitle(), new File(dialog.getProjectFolder()), dialog.getProjectXmlList(), type);

            // エラーモデルの作成
            ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
            projectService.setErrorInfoModel(errorModel);

            // フォートランデータベースをクリアする
            clearAction = new ProjectClearLanguageAction(this.controller);
            clearAction.clearFortranLanguage();

            /** 構造解析関連情報設定 */
            // フォートランデータベース
            Fortran fortran = this.controller.getFortranLanguage();
            // XMLパーサの作成
            XcodeMLParserStax xmlParser = new XcodeMLParserStax();
            // ソースツリーモデル
            FileTreeModel fileModel = this.controller.getSourceTreeModel();
            // 構造ツリーモデル
            LanguageTreeModel languageModel = this.controller.getLanguageTreeModel();
            // モジュールツリーモデル
            ModuleTreeModel moduleModel = this.controller.getModuleTreeModel();
            // データベースの構築、探索を行うクラス
            languageService = new LanguageService(fortran);
            // パーサの設定
            languageService.setPerser(xmlParser);
            // 構造ツリーモデルを設定する
            languageService.setLanguageTreeModel(languageModel);
            // モジュールツリーモデルを設定する
            languageService.setModuleTreeModel(moduleModel);
            // エラー情報モデルを設定する。
            languageService.setErrorInfoModel(errorModel);
            // ソースツリーモデルを設定する。
            languageService.setSourceTreeModel(fileModel);
            // プロジェクトフォルダを設定する
            languageService.setProjectFolder(this.controller.getProjectModel().getProjectFolder());

            /** プロパティ情報設定　ファイル保存時必須 */
            // キーワードプロパティの設定
            projectService.setPropertiesKeyword(this.controller.getPropertiesKeyword());
            // 外部ツールプロパティ設定
            projectService.setPropertiesExtension(this.controller.getPropertiesExtension());
            // 演算カウントプロパティ設定
            projectService.setPropertiesOperand(this.controller.getPropertiesOperand());
            // ソースビュープロパティ設定
            projectService.setPropertiesSource(this.controller.getPropertiesSource());
            // プロファイラプロパティ設定
            projectService.setPropertiesProfiler(this.controller.getPropertiesProfiler());
            // プロジェクトプロパティ設定
            projectService.setPropertiesProject(this.controller.getPropertiesProject());
            // 要求Byte/FLOP設定プロパティ
            projectService.setPropertiesMemory(this.controller.getPropertiesMemory());
            
            projectService.setPropertiesSSH(this.controller.getPropertiesSSH());

            // Make関連情報
            //List<String> commands = new ArrayList<String>();
            File work = null;

            // 中間コードの生成を行う
            if (genCode) {
                // コンソールを表示
            	this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.CONSOLE);

                work = project.getProjectFolder();
                
                // プロジェクトプロパティ設定
                //this.controller.getPropertiesProject().setMakeCommand(makeCom);
                //this.controller.getPropertiesProject().setMakefilePath(makefilePath);
                this.controller.getPropertiesProject().setBuildCommand(build_command);
                System.out.println("BUILD COMMAND: "+ this.controller.getPropertiesProject().getBuildCommand());
                
                if (useSSHconnect) { // Set command line options for SSHconnect call
                	sshc_properties = this.controller.getPropertiesSSH();
                	sshc_properties.setUseSSHconnect(true);  // set to use SSHconncet for building this project
                	sshc_properties.setBuildCommand(build_command);
                	sshc_properties.setLocalPath(work.getAbsolutePath());
                	sshc_properties.setFileFilter(dialog.getFileFilter());
                	sshc_properties.setPreprocessFiles(dialog.getPreprocessFiles());
                }
            }
            // 中間コードの生成を行わない
            else
            {
	            if (project.getListSelectedFile().size() < 1) {
	            	Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
	            	String filetype = "XML";
	            	if (type == FILE_TYPE.FORTRANLANG)
	            		filetype = "Fortran";
	            	String msg = Message.getString("fileprojectnewaction.createprojecterr.dialog.message", filetype); //指定したフォルダには  filetype ファイルがありません。...
	            	JOptionPane.showMessageDialog(frame, msg,
	            			Message.getString("fileprojectnewaction.createprojecterr.dialog.title"), //プロジェクト作成エラー
	            			JOptionPane.ERROR_MESSAGE);
	            	return;
	            }
            }
            // プロジェクトプロパティ設定
            this.controller.getPropertiesProject().setProjectTitle(dialog.getPeojectTitle());

            /** 新規作成実行 */
            execMake(build_command, work, dialog.getProjectXmlList(), project, dialog.isBuild(), dialog.isSave(), genCode, sshc_properties, (project.getFileType() == FILE_TYPE.XCODEML_XML));

            // ソースビューにプロジェクトフォルダを設定する
            this.controller.getMainframe().getPanelSourceView().setProjectFolder(project.getProjectFolder());
            if (dialog.isBuild()) {
            	this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.LANGUAGE);
            }
            else {
	            if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
	                // XMLタブをアクティブにする
	                this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.XML);
	            }
	            else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
	                // ソースタブをアクティブにする
	                this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.SOURCE);
	            }
            }


        } catch (Exception ex) {

            ex.printStackTrace();
            // エラーメッセージ
            JOptionPane.showMessageDialog(
                    frame,
                    Message.getString("fileprojectnewaction.newprojecterr.dialog.message"), //プロジェクトの新規作成エラー
                    message + Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);

            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
        }

        //Application.status.setMessageMain(message + Message.getString("action.common.done.status")); //:完了

    }

    /**
     * プロジェクトの新規作成を実行する
     * @param build_command
     * @param work
     * @param xmls
     * @param model
     * @param build
     * @param save
     * @param make
     * @param mode
     */
    private void execMake(String build_command, File work, List<File> xmls, ProjectModel model, boolean build, boolean save, boolean make, SSHconnectProperties sshc_properties, boolean mode) {
        final String message = Message.getString("mainmenu.file.newproject"); //プロジェクトの新規作成
        if (sshc_properties!=null) makeService = new ProjectMakeService(build_command, work, sshc_properties);
        else makeService = new ProjectMakeService(build_command, work);
    	final ConsolePanel console = this.controller.getMainframe().getPanelAnalysisView().getPanelConsole();
		console.clearConsole();
		OutputStream out = console.getOutputStream();
    	makeService.setOutputStream(out);
    	final boolean bMake = make;
    	final boolean bBuild = build;
    	final boolean bSave = save;
    	final boolean bModeXML = mode;
    	final File prjFolder = model.getProjectFolder();
    	final File settingFolder = new File(prjFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
    	final ProjectModel prjModel = model;
    	final List<File> sourceFiles = xmls;

        // スレッドタスクサービスの生成を行う。
        FutureService<Integer> future = new FutureService<Integer>(
                /**
                 * スレッド呼出クラス
                 */
                new Callable<Integer>() {
                    /**
                     * スレッド実行を行う
                     */
                    @Override
					public Integer call() {
                        try {
                            // 解析実行
                        	if (bMake) {
                        		console.disable_horizontal_scroll = true; // disable scroll
                        		boolean result = makeService.executeMakeCommand();
                        		if (!result) {
                        			return Constant.CANCEL_RESULT;
                        		}
                        		console.disable_horizontal_scroll = false; // enable scroll
                        	}
                        	Application.status.setMessageStatus("Set files...");
                        	// 中間コードをまたはFortranファイルをソースリストに追加
                        	FILE_TYPE filter = FILE_TYPE.XCODEML_XML;
                        	FileTreeModel treeModel = controller.getXmlTreeModel();
                        	if (!bModeXML) {
                        		filter = FILE_TYPE.FORTRANLANG;
                        		treeModel = controller.getSourceTreeModel();
                        	}
                        	SourceFile [] srcs = projectService.getSourceFiles(sourceFiles.toArray(new File[0]), filter, true);
                        	ArrayList<SourceFile> ls = new ArrayList<SourceFile>(Arrays.asList(srcs));

                        	prjModel.setListXmlFile(ls);
                        	languageService.setSourceFiles(ls.toArray(new SourceFile[0]));
            	            // XMLツリーの更新
            	            if (treeModel != null) {
            	            	treeModel.setProjectFolder(prjFolder);
            	                treeModel.setSourceFile(ls.toArray(new SourceFile[0]));
            				}

                        	if (bBuild) {
                        		if (languageService.canParse()) {
                        			languageService.parseSourceFile();
                        		}
                        		else {
                        			return Constant.ERROR_RESULT;
                        		}
                        	}
                        	if (bSave) {
                        		if (projectService.existAllProperties()) {
	                        		projectService.saveProject(prjFolder);
	                        		languageService.writeLanguage(settingFolder);
                        		}
                        		else {
                        			return Constant.ERROR_RESULT;
                        		}
                        	}

                            return Constant.SUCCESS_RESULT;
                        } catch (Exception e) {
                            e.printStackTrace();
                            return Constant.ERROR_RESULT;
                        }
                    }
                }
                ) {
                    /**
                     * スレッド実行完了.<br/>
                     * キャンセルされた時の後処理を行う。
                     */
                    @Override
                    protected void done() {
                        // キャンセルによる終了であるかチェックする。
                        if (this.isCancelled()) {
                            Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
                        }
                        else {
                            Application.status.setMessageMain(message + Message.getString("action.common.done.status")); //:完了
                        }
                        // サービス実行の停止
                        if (makeService != null) {
                        	makeService.cancelRunning();
                        }
                        if (languageService != null) {
                        	languageService.cancelRunning();
                        }

                        super.done();
                    }
        };
        // ステータスメッセージクリア
        Application.status.setMessageStatus(null);

        // スレッドタスクにコントローラをリスナ登録する：スレッド完了時の呼出の為
        future.addPropertyChangeListener(this.controller);
        this.controller.setThreadFuture(future);

        // プログレスダイアログを表示する
        WindowProgressAction progress = new WindowProgressAction(this.controller);
        progress.showProgressDialog();
        Application.status.setProgressStart(true);

        // スレッド起動
        new Thread(future).start();
    }

}
