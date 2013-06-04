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

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.EprofStatementDialog;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.ProfilerMeasureModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProfilerService;

/**
 * プロファイラ:Eprofの測定区間挿入アクションクラス
 * @author riken
 */
public class ProfilerAddEprofAction extends ActionBase {

    /** 測定区間取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param view 			測定区間取得先ビュー
     */
    public ProfilerAddEprofAction(AppController controller, FRAME_VIEW view) {
        super(controller);
        this.view = view;
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        if (this.view == FRAME_VIEW.SOURCE_VIEW) {
            CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
            return (line != null);
        }
        else if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
            IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
            if (blocks == null || blocks.length <= 0) return false;
            // 同一ファイルであるかチェックする
            SourceFile srcfile = null;
            for (IBlock block : blocks) {
                SourceFile file = block.getStartCodeLine().getSourceFile();
                if (srcfile == null) {
                    srcfile = file;
                }
                else if (!srcfile.equals(file)) {
                    return false;
                }
                // サブルーチン、関数宣言文には測定区間は設定不可
                if (block instanceof Procedure) {
                	return false;
                }
            }
            // 同一階層であるかチェックする
            int length = -1;
            DefaultMutableTreeNode[] nodes = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
            for (DefaultMutableTreeNode node : nodes) {
                TreeNode[] paths = node.getPath();
                if (paths == null) {
                    return false;
                }
                else if (length == -1) {
                    length = paths.length;
                }
                else if (length != paths.length) {
                    return false;
                }
            }
        }
        return true;
    }
    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // ステータスメッセージ
    	final String message = event.getActionCommand();
        Application.status.setMessageMain(message);
        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // グループ名の入力ダイアログを表示する
        ProfilerProperties properties = this.controller.getPropertiesProfiler();
        EprofStatementDialog dialog = new EprofStatementDialog(frame, true);
        dialog.setProperties(properties);
        int result = dialog.showDialog();
        if (result != Constant.OK_DIALOG) {
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }

        // グループ名
        String goupname = dialog.getGroupname();
        String number = dialog.getNumber();
        String level = dialog.getLevel();

        // プロファイラサービス
        ProfilerService service = new ProfilerService();
        service.setErrorInfoModel(this.controller.getErrorInfoModel());
        // 測定区間モデル
        ProfilerMeasureModel model = this.controller.getMainframe().getPanelAnalysisView().getPanelProfilerMeasure().getModel();
        service.setMeasureModel(model);
        // プロファイラ情報
        service.setProfilerInfo(this.controller.getProfilerInfo());
        // プロジェクトフォルダ
        service.setProjectFolder(this.controller.getProjectModel().getProjectFolder());

        if (this.view == FRAME_VIEW.SOURCE_VIEW) {
            CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedArea();
            service.addProfilerMeasureInfo(line, goupname, number, level);
        }
        else if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
            IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
            if (blocks != null && blocks.length > 0) {
                service.addProfilerMeasureInfo(blocks, goupname, number, level);
            }
        }

        // 測定区間タブをアクティブにする
        this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.EPROF_MEASURE);
        
        Application.status.setMessageMain(message +
        		Message.getString("action.common.done.status")); //完了
    }

}
