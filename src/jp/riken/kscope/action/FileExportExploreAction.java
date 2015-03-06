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

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.gui.ITreeComponent;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * エクスポートアクションクラス
 * @author RIKEN
 *
 */
public class FileExportExploreAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileExportExploreAction(AppController controller) {
        super(controller);
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

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
    	
    	final String message = Message.getString("fileexportexploreaction.exportexploer.status"); //ツリー情報のエクスポート
    	Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 選択分析情報のタブの取得
        ITreeComponent tab = this.controller.getMainframe().getPanelExplorerView().getSelectedPanel();

        // プロジェクトフォルダ
        File projectfolder = this.controller.getProjectModel().getProjectFolder();
        String folder = projectfolder != null ? projectfolder.getAbsolutePath() : null;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();

        // 出力プロシージャリスト
        List<String> procedure_names = new ArrayList<String>();
        String filename = null;
        if (tab.getEnumPanel() == EXPLORE_PANEL.LANGUAGE || tab.getEnumPanel() == EXPLORE_PANEL.MODULE) {
            // 構造ツリー、モジュールツリーの場合は、フォートランデータベースから出力する。
            IBlock[] blocks = tab.getSelectedBlocks();

            // 出力プロシージャチェック
            if (blocks != null) {
                for (IBlock block : blocks) {
                    if (block instanceof Procedure && ((Procedure) block).get_name() != null) {
                        procedure_names.add(((Procedure) block).get_name());
                    }
                }
            }

            // 出力プロシージャが存在しないので、ルートから検索する
            if (procedure_names.size() <= 0) {
                if (fortran.getMainName() != null) {
                    procedure_names.add(fortran.getMainName());
                }
            }
            if (procedure_names.size() <= 0) {
                // エラーメッセージ
                JOptionPane.showMessageDialog(null,
                        Message.getString("fileexportexploreaction.exportexploer.dialog.message"), //出力プロシージャが存在しません。
                        message, 
                        JOptionPane.ERROR_MESSAGE);
                Application.status.setMessageMain(message +
                		Message.getString("action.common.error.status")); //:エラー
                return;
            }
            filename = procedure_names.get(0) + ".txt";
        }
        else if  (tab.getEnumPanel() == EXPLORE_PANEL.SOURCE) {
            filename = "sourcetree.txt";
        }
        else if (tab.getEnumPanel() == EXPLORE_PANEL.XML) {
            filename = "xmltree.txt";
        }

        // ファイル保存ダイアログを表示する。
        File file = SwingUtils.showSaveFileDialog(frame, message, folder, filename);
        if (file == null) {
        	Application.status.setMessageMain(message + 
        			Message.getString("action.common.cancel.status")); // キャンセル
        	return; 
        }

        if (tab.getEnumPanel() == EXPLORE_PANEL.LANGUAGE) {
            // 構造ツリー、モジュールツリーの場合は、フォートランデータベースから出力する。
            IBlock[] blocks = tab.getSelectedBlocks();

            // エラー情報モデル
            ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

            // 構造解析サービス
            LanguageService service = new LanguageService(fortran);
            // エラー情報モデルを設定する。
            service.setErrorInfoModel(errorModel);

            // ファイル出力する
            service.exportLanguage(file, blocks);
        }
        else {
            // ツリー情報のエクスポートを行う
            tab.export(file);
        }

        Application.status.setMessageMain(message + 
        		Message.getString("action.common.done.status")); //完了 
    }

}
