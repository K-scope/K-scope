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
import java.io.IOException;
import java.util.concurrent.Callable;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.FileExportSourceFileDialog;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;

/**
 * ソースファイルエクスポートアクション.
 * @author riken
 */
public class FileExportSourceFileAction extends ActionBase {
	/** プロジェクトフォルダ */
	private File prjFolder;
	/** ソースフォルダ */
	private File srcFolder;
	/** 出力フォルダ */
	private File outFolder;
	/** ステータスメッセージ */
	private String message = "";

	/**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
	public FileExportSourceFileAction(AppController controller) {
		super(controller);
		message = Message.getString("fileexportsourcefileaction.exportsource.status"); // ソースファイルエクスポート
	}

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        // プロジェクトが作成済みであること
        ProjectModel model = this.controller.getProjectModel();
        if (model == null) return false;
        if (model.getProjectTitle() == null) return false;
        if (model.getProjectFolder() == null || !model.getProjectFolder().exists()) return false;

        return true;
    }

	@Override
	public void actionPerformed(ActionEvent event) {
		// 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );
        Application.status.setMessageMain(message);

		FileExportSourceFileDialog dialog = new FileExportSourceFileDialog(frame, true);
		dialog.setProjectFolder(controller.getProjectModel().getProjectFolder());
		dialog.setExcludeFile(controller.getPropertiesApplication().getSourceExportExclude());

		int res = dialog.showDialog();
		if (res != Constant.OK_DIALOG) {
			Application.status.setMessageMain(message +
					Message.getString("action.common.cancel.status")); //:キャンセル
			return;
		}

		// 出力フォルダ取得
		outFolder = new File(dialog.getOutputFolder());
		// コピー先ディレクトリチェック
		if (!outFolder.exists()) return;
		if (!outFolder.isDirectory()) return;

		// プロジェクトパス
		String prjRoot = this.controller.getProjectModel().getProjectFolder().getAbsolutePath();
		// プロジェクトフォルダ
		prjFolder = new File(prjRoot);
		// ソースルートパス（プロジェクトフォルダの外にソースがある場合）
		String sourceRoot = this.controller.getSourceTreeModel().getRootFolder();
		// ソースルートフォルダ
		srcFolder = null;
		if (!StringUtils.isNullOrEmpty(sourceRoot) && !FileUtils.isChildPath(prjRoot, sourceRoot)) {
			srcFolder = new File(sourceRoot);
		}

		// 除外ファイルパターン文字列
		String exclude = dialog.getExcludeFilePattern();

		// アプリケーションシステムの除外パスリスト
		String [] excludePaths = new String [] {
				FileUtils.joinFilePath(prjFolder, KscopeProperties.SETTINGS_FOLDER).getAbsolutePath(),
				FileUtils.joinFilePath(prjFolder, KscopeProperties.PROJECT_FILE).getAbsolutePath()
		};

		Application.status.setMessageMain(message +
				Message.getString("action.common.process.status")); //:処理中

		copyFiles(exclude, excludePaths, dialog.isExportOtherFile());

	}

	/**
	 * ソースファイルをコピーする.
	 * @param exclude			除外ファイル拡張子カンマ区切りリスト
	 * @param excludePaths		除外ファイルパスリスト
	 * @param other				true=ソースファイル以外をコピーする
	 */
    private void copyFiles(String exclude, String [] excludePaths, boolean other) {
        final String excludeStr = exclude;
        final String [] excludePathArr = excludePaths;
        final boolean otherFlag = other;

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
                        	Application.status.setProgressStart(true);
                        	File[] cpFiles = null;
                            if (otherFlag) {
//                             	プロジェクトフォルダ配下を走査
                    			File[] files = FileUtils.getChildren(prjFolder, excludeStr, excludePathArr);

                    			// ソースフォルダがプロジェクトフォルダ配下に無い場合はソースフォルダ配下も走査
                    			if(srcFolder != null && !FileUtils.isChildPath(prjFolder.getAbsolutePath(), srcFolder.getAbsolutePath())) {
                    				File [] srcFiles = FileUtils.getChildren(srcFolder, excludeStr, excludePathArr);
                    				if (srcFiles != null && srcFiles.length > 0) {
                    					if (files != null && files.length > 0) {
                    						cpFiles = new File[files.length + srcFiles.length];
                    						for (int i=0; i<files.length; i++) {
                    							cpFiles[i] = files[i];
                    						}
                    						for (int i=files.length; i<cpFiles.length; i++) {
                    							cpFiles[i] = srcFiles[i - files.length];
                    						}
                    					}
                    					else {
                    						cpFiles = srcFiles;
                    					}
                    				}
                    				else {
                    					cpFiles = files;
                    				}
                    			}
                    			else {
                    				cpFiles = files;
                    			}
                            }
                            else {
                    			SourceFile[] sourceFiles = controller.getSourceTreeModel().getAllSourceFiles();
                    			cpFiles = new File[sourceFiles.length];
                    			for (int i=0; i<sourceFiles.length; i++) {
                    				cpFiles[i] = sourceFiles[i].getFile();
                    			}
                            }

                    		// コピー対象が存在しない場合は終了
                    		if (cpFiles == null) return Constant.SUCCESS_RESULT;
                    		if (cpFiles.length < 1) return Constant.SUCCESS_RESULT;

                    		File outPrj = FileUtils.joinFilePath(outFolder, prjFolder.getName());
                    		if (srcFolder == null || otherFlag) {
                    			outPrj.mkdir();
                    		}

                    		// ソースファイルフォルダコピー先のルート
                    		File outSrc = null;
                    		if (srcFolder != null) {
                    			outSrc = FileUtils.joinFilePath(outFolder, srcFolder.getName());
                    			outSrc.mkdir();
                    		}

                    		int cnt = 0;
                    		for (File f : cpFiles) {
                    			if (FileUtils.isChildPath(prjFolder.getAbsolutePath(), f.getAbsolutePath())) {
                    				String relPath = FileUtils.getRelativePath(f, prjFolder);
                    				if (relPath.startsWith("."))
                    					relPath = relPath.substring(2);

                    				File newPath = FileUtils.joinFilePath(outPrj, relPath);
                    				try {
                    					FileUtils.copyFile(f, outPrj, newPath);
                    					cnt++;
                    					Application.status.setMessageStatus("Exporting (" + cnt + "/" + cpFiles.length + ")");
                    				} catch (IOException e) {
                    					// TODO Auto-generated catch block
                    					e.printStackTrace();
                    				}
                    			}
                    			else if (FileUtils.isChildPath(srcFolder.getAbsolutePath(), f.getAbsolutePath())) {
                    				String relPath = FileUtils.getRelativePath(f, srcFolder);
                    				if (relPath.startsWith("."))
                    					relPath = relPath.substring(2);

                    				File newPath = FileUtils.joinFilePath(outSrc, relPath);
                    				try {
                    					FileUtils.copyFile(f, outSrc, newPath);
                    					cnt++;
                    					Application.status.setMessageStatus("Exporting (" + cnt + "/" + cpFiles.length + ")");
                    				} catch (IOException e) {
                    					// TODO Auto-generated catch block
                    					e.printStackTrace();
                    				}
                    			}
                    		}
                    		Application.status.setProgressStart(false);
                            return Constant.SUCCESS_RESULT;
                        } catch (Exception e) {
                            e.printStackTrace();
                            Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
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
        new Thread(future).start();

    }

}
