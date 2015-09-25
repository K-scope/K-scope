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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
//import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectService;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * プロジェクトを開くアクション
 * @author RIKEN
 */
public class FileProjectOpenAction extends ActionBase {

	private static boolean debug = (System.getenv("DEBUG")!= null);
	private static boolean debug_l2 = false;
	
    /** データベースの構築、探索を行うクラス */
    private LanguageService serviceLanguage;
    /** 変数アクセス先メモリサービス */
    private AnalysisMemoryService serviceMemory;
    /** プロジェクトのクリアアクション */
    private ProjectClearLanguageAction clearAction;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileProjectOpenAction(AppController controller) {
        super(controller);
        if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
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
        String message = Message.getString("mainmenu.file.openproject"); //プロジェクトを開く
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 最終アクセスフォルダ
        String currentFolder = this.controller.getLastAccessFolder();
        if (currentFolder == null) {
            currentFolder = System.getProperty("user.dir");
        }

        // プロジェクトフォルダ選択ダイアログを表示する。
        File selected = SwingUtils.showOpenProjectDialog(
    			frame,
    			Message.getString("dialog.common.selectproject.title"), //プロジェクトフォルダの選択
    			currentFolder);
        if (selected == null) {
        		Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
        		return;
        }
        // プロジェクト設定ファイルであるか
        File projectFolder = null;		// プロジェクトフォルダ
        File projectFile = null;		// プロジェクト設定ファイル
        if (selected.isFile() && KscopeProperties.PROJECT_FILE.equalsIgnoreCase(selected.getName())) {
        	projectFolder = selected.getParentFile();
        	projectFile = selected;
        	// 最終アクセスフォルダ
        	this.controller.setLastAccessFolder(projectFolder);
        }
        else {
        	projectFolder = selected;
        	projectFile = new File(selected.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);
        	// 最終アクセスフォルダ
        	this.controller.setLastAccessFolder(projectFolder.getParentFile());
        }

        // プロジェクトフォルダのチェック
        // プロジェクト設定ファイル
        if (!projectFile.exists()) {
            // エラーメッセージ
            JOptionPane.showMessageDialog(frame,
                    Message.getString("dialog.common.selectproject.notprojecterr.message"), //プロジェクトフォルダではありません。
                    Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
            return;
        }

        try {
            // プロジェクトのクリア
            FileProjectCloseAction actionClose = new FileProjectCloseAction(this.controller);
            actionClose.clearProject();

            // プロジェクトモデル
            ProjectModel modelProject = this.controller.getProjectModel();
            // プロジェクトサービス
            ProjectService service = new ProjectService(modelProject);
            // キーワードプロパティ
            service.setPropertiesKeyword(this.controller.getPropertiesKeyword());
            // 外部ツールプロパティ
            service.setPropertiesExtension(this.controller.getPropertiesExtension());
            // 演算カウントプロパティ
            service.setPropertiesOperand(this.controller.getPropertiesOperation());
            // ソースビュー設定プロパティ
            service.setPropertiesSource(this.controller.getPropertiesSource());
            // プロファイラプロパティ設定プロパティ
            service.setPropertiesProfiler(this.controller.getPropertiesProfiler());
            // プロジェクトプロパティ
            service.setPropertiesProject(this.controller.getPropertiesProject());
            // 要求Byte/FLOP設定プロパティ
            service.setPropertiesMemory(this.controller.getPropertiesMemory());
            
            service.setRBproperties(this.controller.getRBproperties());
            // エラーモデル
            service.setErrorInfoModel(this.controller.getErrorInfoModel());

            // プロジェクトを開く
            service.openProject(projectFolder);

            // XMLツリーに選択XMLファイルを表示する。
            List<SourceFile> listSource = modelProject.getListSelectedFile();
            List<SourceFile> xmlfiles = new ArrayList<SourceFile>();
            List<SourceFile> srcfiles = new ArrayList<SourceFile>();
            if (listSource != null && listSource.size() > 0) {
                for (SourceFile file : listSource) {
                    if (FILE_TYPE.isFortranFile(file.getFile())) {
                        srcfiles.add(file);
                    }
                    else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
                        xmlfiles.add(file);
                    }
                }
            }

            // ソースビューにプロジェクトフォルダを設定する
            this.controller.getMainframe().getPanelSourceView().setProjectFolder(modelProject.getProjectFolder());

            // プロパティの更新
            this.controller.updateProperties();
            // プロジェクトプロパティにタイトルを設定
            this.controller.getPropertiesProject().setProjectTitle(modelProject.getProjectTitle());

            // Languageクラスのデシリアライズを行う
            // settingsフォルダ
            File settingsFolder = new File(projectFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
            readLanguage(settingsFolder, xmlfiles.toArray(new SourceFile[0]), srcfiles.toArray(new SourceFile[0]));
            if(xmlfiles.size() <= 0)  {
            	this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.SOURCE);
            	this.controller.getErrorInfoModel().addErrorInfo(Message.getString("fileprojectopenaction.build.noxmlerr.errinfo")); //XMLファイルが存在しませんので、新たに構造解析実行はできません。
             }

        } catch (Exception e) {
            if (debug) e.printStackTrace();
            // エラーメッセージ
            JOptionPane.showMessageDialog(frame,
                    Message.getString("fileprojectopenaction.openproject.openerr.dialog.message"), //プロジェクトのオープンエラー
                    message + Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);

            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
        }
    }

    /**
     * Languageクラスのデシリアライズを行う
     * @param folder		Languageクラスのシリアライズフォルダ
     * @param xmlfiles		XMLファイルリスト
     * @param  srcfiles     ソースファイルリスト
     */
    public void readLanguage(final File folder, final SourceFile[] xmlfiles, final SourceFile[] srcfiles) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.file.openproject"); //プロジェクトを開く

        // フォートランデータベースをクリアする
        clearAction = new ProjectClearLanguageAction(this.controller);
        clearAction.clearFortranLanguage();

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        // ソースツリーモデル
        final FileTreeModel fileModel = this.controller.getSourceTreeModel();
        // Xmlツリーモデル
        final FileTreeModel xmlModel = this.controller.getXmlTreeModel();
        // 構造ツリーモデル
        LanguageTreeModel languageModel = this.controller.getLanguageTreeModel();
        // モジュールツリーモデル
        ModuleTreeModel moduleModel = this.controller.getModuleTreeModel();
        // XMLパーサの作成
        XcodeMLParserStax xmlParser = new XcodeMLParserStax();

        ProjectModel model = this.controller.getProjectModel();
        final File prjFolder = model.getProjectFolder();

        // 構造解析サービス
        serviceLanguage = new LanguageService(xmlfiles, fortran, xmlParser);
        // エラー情報モデルを設定する。
        serviceLanguage.setErrorInfoModel(errorModel);
        // ソースツリーモデルを設定する。
        serviceLanguage.setSourceTreeModel(fileModel);
        // Xmlツリーモデルを設定する。
        serviceLanguage.setXmlTreeModel(xmlModel);
        // 構造ツリーモデルを設定する
        serviceLanguage.setLanguageTreeModel(languageModel);
        // モジュールツリーモデルを設定する
        serviceLanguage.setModuleTreeModel(moduleModel);

        // 変数アクセス先メモリサービス
        serviceMemory = new AnalysisMemoryService();
        // 変数アクセス先メモリ設定
        serviceMemory.setPropertiesVariableMemory(this.controller.getPropertiesVariable());

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
                        	// ツリー更新
                        	Application.status.setMessageStatus("Set files...");
                        	xmlModel.setProjectFolder(prjFolder);
                        	xmlModel.setSourceFile(xmlfiles);
                        	fileModel.setProjectFolder(prjFolder);
                        	fileModel.setSourceFile(srcfiles);

                            // デシリアライズ実行
                        	serviceLanguage.readLanguage(folder);
                        	controller.setFortranLanguage(serviceLanguage.getFortranLanguage());

                        	// アクセス先メモリの設定している変数の取得
                        	serviceMemory.createVariableMemoryProperties(serviceLanguage.getFortranLanguage());

                            return Constant.SUCCESS_RESULT;

                        } catch (LanguageException lang_ex) {
                            return Constant.ERROR_RESULT;
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
                        // フォートラン構文解析結果格納データベースが新規にオブジェクト生成されたので、
                        // アプリケーションコントローラにセットする
                        Fortran value = serviceLanguage.getFortranLanguage();
                        if (value != null) {
                            controller.setFortranLanguage(value);
                        }

                        // キャンセルによる終了であるかチェックする。
                        if (this.isCancelled()) {
                            // フォートランデータベースをクリアする
                            clearAction.clearFortranLanguage();
                            Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
                        }
                        else {
                            Application.status.setMessageMain(message + Message.getString("action.common.done.status")); //:完了
                        }

                        // サービス実行の停止
                        if (serviceLanguage != null) {
                        	serviceLanguage.cancelRunning();
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
        WindowProgressAction progress = new WindowProgressAction(FileProjectOpenAction.this.controller);
        progress.showProgressDialog();
        Application.status.setProgressStart(true);

        // スレッド起動
        new Thread(future).start();
    }


}
