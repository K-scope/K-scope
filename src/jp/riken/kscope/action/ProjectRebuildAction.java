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
import java.util.concurrent.Callable;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectMakeService;
import jp.riken.kscope.utils.StringUtils;

/**
 * 構造解析再実行アクション
 * @author riken
 */
public class ProjectRebuildAction extends ActionBase {
	/** makeコマンド実行サービス */
	private ProjectMakeService serviceMake;
	/** 構造解析サービス */
	private LanguageService serviceLang;
	/** エクスプローラビューの更新フラグ */
	private boolean updateView;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
	public ProjectRebuildAction(AppController controller){
		super(controller);
	}

	/**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
	@Override
	public boolean validateAction() {
		ProjectProperties properties = this.controller.getPropertiesProject();
		if (properties == null) return false;
		/*
		 *  MAKE_COMMAND and MAKEFILE_PATH are not used anymore 
		// makeコマンドが設定されていない場合はfalse
		ProjectPropertyValue value = properties.getPropertyValue(ProjectProperties.MAKE_COMMAND);
		if (value == null) return false;
		if (StringUtils.isNullOrEmpty(value.getValue())) return false;
		// makefileが設定されていない場合はfalse
		value = properties.getPropertyValue(ProjectProperties.MAKEFILE_PATH);
		*/
		ProjectPropertyValue value = properties.getPropertyValue(ProjectProperties.BUILD_COMMAND);
		if (value == null) return false;
		if (StringUtils.isNullOrEmpty(value.getValue())) return false;

        // スレッドタスクの実行状態をチェックする
        return this.controller.isThreadTaskDone();
	}

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
	@Override
	public void actionPerformed(ActionEvent event) {
		final String message = Message.getString("mainmenu.project.restertanalysis"); //構造解析再実行
		Application.status.setMessageMain(message);

		Frame frame = getWindowAncestor(event);
        // 確認メッセージを表示する。
        int result = JOptionPane.showConfirmDialog(
                frame,
                Message.getString("projectrebuildaction.confirmdialog.message"), //構造解析を再実行しますか？
                message, //構造解析再実行
                JOptionPane.OK_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE);

		if (result != Constant.OK_DIALOG) {
			Application.status.setMessageMain(message +
					Message.getString("action.common.cancel.status")); //:キャンセル
			return;
		}

		// 構造解析再実行
		rebuild();

		return;
	}

	/**
	 * 構造解析再実行を行う.
	 * makeコマンドを実行し、データベースの再構成を行う。
	 */
	public void rebuild() {
		//構造解析再実行
		final String message = Message.getString("mainmenu.project.restertanalysis");
        Application.status.setMessageMain(message);

        // プロジェクトモデル
        ProjectModel projectModel = this.controller.getProjectModel();
		// makeコマンド
		ProjectProperties project =  this.controller.getPropertiesProject();
		String command = project.getBuildCommand();
		//String path = project.getMakefileFolder(projectModel.getProjectFolder());

		// コンソール
		this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.CONSOLE);
		ConsolePanel console = this.controller.getMainframe().getPanelAnalysisView().getPanelConsole();
		console.clearConsole();
		OutputStream out = console.getOutputStream();

		// エラーモデル
		ErrorInfoModel modelError = this.controller.getErrorInfoModel();
		modelError.clearErrorList();

		// LanguageTreeModel
		LanguageTreeModel modelTree = this.controller.getLanguageTreeModel();
		this.updateView = !(modelTree.isSetLanguageTree());

		// makeコマンド実行サービス
		serviceMake = new ProjectMakeService(command, projectModel.getProjectFolder());
		serviceMake.setOutputStream(out);
        // エラー情報モデルを設定する。
		serviceMake.setErrorInfoModel(modelError);
        // フォートランデータベース
		serviceMake.setFortranLanguage(this.controller.getFortranLanguage());
        // XMLファイル検索パス
		serviceMake.setListSearchPath(this.controller.getProjectModel().getListSearchPath());
        // 差替結果モデルを設定する。
		serviceMake.setReplaceModel(this.controller.getReplaceTableModel());
        // プロジェクトモデル
		serviceMake.setProjectModel(this.controller.getProjectModel());

        // 構造解析サービス
        serviceLang = new LanguageService(this.controller.getFortranLanguage());
        // 構造ツリーモデルを設定する
        serviceLang.setLanguageTreeModel(this.controller.getLanguageTreeModel());
        // モジュールツリーモデルを設定する
        serviceLang.setModuleTreeModel(this.controller.getModuleTreeModel());
        // エラー情報モデルを設定する。
        serviceLang.setErrorInfoModel(modelError);
        // ソースツリーモデルを設定する。
        serviceLang.setSourceTreeModel(this.controller.getSourceTreeModel());
        // XMLツリーモデルを設定する。
        serviceLang.setXmlTreeModel(this.controller.getXmlTreeModel());

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
                        	boolean result = serviceMake.executeMakeCommand();
                        	if (!result) {
                        		return Constant.CANCEL_RESULT;
                        	}
                            // 分析ビューのクリアを行う。
                            clearAnalysisView();
                            // 解析実行
                        	if (serviceMake.rebuild()) {
                                // エクスプローラビュー、ソースビューのクリアを行う。
                                clearExplorerView();
                            	// エクスプローラビューの更新
                            	serviceLang.setExplorerView();
                        	}

                            // 差替結果タブをアクティブにする
                        	if (serviceMake.getReplaceModel() != null && serviceMake.getReplaceModel().getReplacementResultCount() > 0) {
	                            controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.REPLACE);
                        	}
                        	else {
                        		controller.getMainframe().getPanelAnalysisView().closeTab(ANALYSIS_PANEL.REPLACE);
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
                        if (serviceMake != null) {
                        	serviceMake.cancelRunning();
                        }
                        if (serviceLang != null) {
                        	serviceLang.cancelRunning();
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

        // スレッド起動
        Thread thread =  new Thread(future);
        thread.start();

	}

    /**
     * 分析ビューのクリアを行う。
     */
    private void clearAnalysisView() {
        // 分析情報クリア
        this.controller.getMainframe().getPanelAnalysisView().clearModels();
    }

    /**
     * エクスプローラビュー、ソースビューのクリアを行う。
     */
    private void clearExplorerView() {
        // ツリーモデル
        this.controller.getMainframe().getPanelExplorerView().clearTreeModel();
        // ソースビュークリア
        this.controller.getMainframe().getPanelSourceView().closeAllTabs();
    }
}
