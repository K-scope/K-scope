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

import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.dialog.InformationDialog;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.service.AppController;


/**
 * 付加情報編集アクションクラス
 * @author RIKEN
 *
 */
public class EditInformationEditAction extends ActionBase {

    /** 付加情報取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param view 			付加情報取得先ビュー
     */
    public EditInformationEditAction(AppController controller, FRAME_VIEW view) {
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
        return isSelectedInformation();
/*        IInformation info = getSelectedInformation();
        if (info == null) return false;
        return true;*/ // 2012/10/30 Marge yabe
    }

    /**
     * 付加情報編集イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 実行チェック
        if (!validateAction()) return;

        // ステータスメッセージ
        final String message = Message.getString("mainmenu.edit.info"); //付加情報編集
        Application.status.setMessageMain(message);

        // 選択付加情報を取得する
        IInformation infoNode = getSelectedInformation();
        if (infoNode == null) return;

        // 付加情報の編集を行う
        if (!editInformation(infoNode)) {
            // 付加情報の編集のキャンセル
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status")); //:キャンセル
            return;
        }

        // 付加情報パネルをアクティブにする
        this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.INFORMATION);
        Application.status.setMessageMain(message +
        		Message.getString("action.common.done.status")); //:完了
        return;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    private IInformation getSelectedInformation() {
        IInformation info = null;
        if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
            DefaultMutableTreeNode[] node = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
            if (node == null) return null;
            Object sobj = node[0].getUserObject();
            Object eobj = node[node.length - 1].getUserObject();
            if (sobj == eobj) {
                if (sobj instanceof IInformation) {
                    info = (IInformation)sobj;
                }
            } else {
                if (sobj != null && eobj != null) {
                    if (sobj instanceof IInformation && eobj instanceof IInformation) {
                        // 複数範囲の付加情報から付加情報クラスを取得する.
                        info = getProgramInformation(new IInformation[]{(IInformation) sobj, (IInformation)eobj});
                        /*Program fortran = this.controller.getFortranLanguage();
                        if (fortran != null) {
                            info = fortran.getInformation((IInformation) sobj, (IInformation) eobj);
                        }*/ // 2012/10/30 Marge yabe
                    }
                }
            }
        }
        else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
            info = this.controller.getMainframe().getPanelAnalysisView().getSelectedInformation();
            if (info == null) return null;
        }
        return info;
    }


    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    private boolean isSelectedInformation() {
        if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
            DefaultMutableTreeNode[] node = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
            if (node == null) return false;
            Object sobj = node[0].getUserObject();
            Object eobj = node[node.length - 1].getUserObject();
            if (sobj != null && eobj != null) {
                if (sobj instanceof IInformation && eobj instanceof IInformation) {
                    return true;
                }
            }
            else if (sobj != null) {
                if (sobj instanceof IInformation) {
                    return true;
                }
            }
            return false;
        }
        else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
            IInformation info = this.controller.getMainframe().getPanelAnalysisView().getSelectedInformation();
            if (info == null) return false;
            return true;
        }

        return false;
    }

    /**
     * 付加情報の編集を行う
     * @param infoNode		付加情報
     * @return    付加情報の編集の可否
     */
    public boolean editInformation(IInformation infoNode) {
        return editInformation(infoNode, null, true);
    }

    /**
     * 付加情報の編集を行う
     * @param infoNode		付加情報
     * @param editable		付加情報編集の可否
     * @return    付加情報の編集の可否
     */
    /*
    public boolean editInformation(IInformation infoNode, boolean editable) {
        return editInformation(infoNode, null, editable);
    }
     */

    /**
     * 付加情報の編集を行う
     * @param infoNode		付加情報
     * @param addinfo		追加情報
     * @return    付加情報の編集の可否
     */
    public boolean editInformation(IInformation infoNode, String addinfo) {
        return editInformation(infoNode, addinfo, true);
    }

    /**
     * 付加情報の編集を行う
     * @param infoNode		付加情報
     * @param addinfo		追加情報
     * @param editable		付加情報編集の可否
     * @return    付加情報の編集の可否
     */
    public boolean editInformation(IInformation infoNode, String addinfo, boolean editable) {
        if (infoNode == null) return false;

        // ステータスメッセージ
        final String message = Message.getString("mainmenu.edit.info"); //付加情報編集
        Application.status.setMessageMain(message);

        // メインフレーム
        Frame frame = this.controller.getMainframe();

        // プロジェクトフォルダ
        File projectFolder = this.controller.getProjectModel().getProjectFolder();

        // 付加情報
        TextInfo info = infoNode.getInformation();
        if (info == null) {
            info = new TextInfo();
        }
        // 付加情報
        String content = info.getContent();
        if (content == null) {
            content = "";
        }
        if (addinfo != null && !addinfo.isEmpty()) {
            if (content != null && !content.isEmpty()) {
                content += "\n";
            }
            content += addinfo;
        }
        // 付加情報編集ダイアログの表示を行う。
        InformationDialog dialog = new InformationDialog(frame, true);
        dialog.setProjectFolder(projectFolder);
        dialog.setInformation(content);
        dialog.setBlockName(infoNode.toString());
        dialog.setEditable(editable);

        // ダイアログを表示する
        int result = dialog.showDialog();
        if (result == Constant.CANCEL_DIALOG) {
            // 付加情報の編集のキャンセル
        	Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
        	return false;
        }

        // 更新された付加情報を設定する
        content = dialog.getInformation();
        info.setContent(content);
        // ノードに付加情報の設定
        infoNode.setInformation(info);

        // 付加情報パネルモデルの設定
        InformationModel infoModel = this.controller.getMainframe().getPanelAnalysisView().getPanelInformation().getModel();
        // プロジェクトフォルダの設定
        infoModel.setProjectFolder(projectFolder);

        // 付加情報の設定
        infoModel.setTitle(infoNode.toString());
        infoModel.setInformation(infoNode, info);

        // エクスプローラビューの再描画を行う
        this.controller.getMainframe().getPanelExplorerView().fireSelectNodeChanged();

        // 変数特性一覧情報を更新する
        this.controller.refreshInformation();

        Application.status.setMessageMain(message + Message.getString("action.common.done.status")); //:完了
        return true;
    }


    /**
     * 複数範囲の付加情報から付加情報クラスを取得する.
     * Programクラスにて付加情報範囲のInformationBlockを生成して返す. <br/>
     * 付加情報範囲が１つだけの場合は、先頭の付加情報を返す.
     * @param infos		付加情報範囲
     * @return			付加情報
     */
    private IInformation getProgramInformation(IInformation[] infos) {
        if (infos == null) return null;
        Program fortran = this.controller.getFortranLanguage();
        if (fortran == null) return null;
        if (infos.length == 0) return null;
        // 単一
        if (infos.length == 1) {
            return infos[0];
        }
        // 複数範囲
        IInformation start = infos[0];
        IInformation end = infos[infos.length-1];
        if (start != null && end != null) {
            IInformation info = fortran.getInformation(start, end);
            return info;
        }

        return start;
    }
}
