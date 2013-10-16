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
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProjectService;

/**
 * XMLファイル削除アクションクラス
 * @author riken
 *
 */
public class ProjectDeleteFileAction extends ActionBase {

    /** アクションエクスプローラパネル */
    private EXPLORE_PANEL panelExplore;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param panel			アクションエクスプローラパネル
     */
    public ProjectDeleteFileAction(AppController controller, EXPLORE_PANEL panel) {
        super(controller);
        panelExplore = panel;
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // メニューのイネーブル切替
        // プロジェクト情報
        ProjectModel project = this.controller.getProjectModel();
        if (project == null) return false;
        // プロジェクトにXMLファイルが追加済みであるか？
        if (project.getListSelectedFile() == null || project.getListSelectedFile().size() <= 0) {
            // XMLファイル未登録
            return false;
        }

        if (project.getFileType() == FILE_TYPE.XCODEML_XML
            && panelExplore == EXPLORE_PANEL.XML) {
            return true;
        }
        if (project.getFileType() == FILE_TYPE.FORTRANLANG
            && panelExplore == EXPLORE_PANEL.SOURCE) {
            return true;
        }
        boolean enable = false;
        if (panelExplore == null || panelExplore == EXPLORE_PANEL.UNKNOWN) {
            // メインフレームからのアクション
            EXPLORE_PANEL activePanel = this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel();
            if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
                // XMLツリーパネルが表示されている場合のみイネーブルとする。
                enable = (activePanel == EXPLORE_PANEL.XML);
            }
//            if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
//                // ソースツリーパネルが表示されている場合のみイネーブルとする。
//                enable = (activePanel == EXPLORE_PANEL.SOURCE);
//            }
        }
        return enable;
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );
        // プロジェクト情報
        ProjectModel project = this.controller.getProjectModel();
        // ステータスメッセージ
        String message = Message.getString("projectdeletefileaction.xml.delete.status"); //XMLファイル削除
        if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
        	message = Message.getString("projectdeletefileaction.source.delete.status"); //ソースファイル削除
        }
        Application.status.setMessageMain(message);

        // 選択ファイルの選択（選択ファイル OR 選択フォルダの子ファイルを取得する）
        SourceFile[] selectedFiles = null;
        String title = Message.getString("projectdeletefileaction.xml.confirmdialog.title"); // XMLファイル／フォルダの削除
        String confirm =Message.getString("projectdeletefileaction.xml.confirmdialog.message"); //選択されたXMLファイル／フォルダを削除しますが、よろしいですか？
        if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
            selectedFiles = this.controller.getMainframe().getPanelExplorerView().getPanelXmlTree().getSelectChildSourceFiles();
        }
        else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
            selectedFiles = this.controller.getMainframe().getPanelExplorerView().getPanelSourceTree().getSelectChildSourceFiles();
            title = Message.getString("projectdeletefileaction.source.confirmdialog.title"); // ソースファイル／フォルダの削除
            confirm =Message.getString("projectdeletefileaction.source.confirmdialog.message"); //選択されたソースファイル／フォルダを削除しますが、よろしいですか？
        }
        if (selectedFiles == null) {
            JOptionPane.showMessageDialog(frame,
            		Message.getString("projectdeletefileaction.selectdialog.message"), //削除するファイル／フォルダを選択してください。
            		Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);

            Application.status.setMessageMain(message +
            		Message.getString("action.common.error.status")); //エラー
            return;
        }

        // 確認メッセージを表示する。
        int option = JOptionPane.showConfirmDialog(frame,
        		  confirm, //選択ファイル／フォルダを削除しますが、よろしいですか？
                  title, //ファイル／フォルダの削除
                  JOptionPane.OK_CANCEL_OPTION,
                  JOptionPane.WARNING_MESSAGE);
        if (option != JOptionPane.OK_OPTION) {
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status")); //キャンセル
            return;
        }

        // プロジェクト管理サービス
        ProjectService service = new ProjectService(project);
        List<SourceFile> list = java.util.Arrays.asList(selectedFiles);
        service.deleteProjectSelectedFile(list);

        // プロジェクトから削除後のXMLファイルリスト
        list = project.getListSelectedFile();

        // XMLツリーに選択XMLファイルを表示する。
        List<SourceFile> xmlfiles = new ArrayList<SourceFile>();
        List<SourceFile> srcfiles = new ArrayList<SourceFile>();
        if (list != null && list.size() > 0) {
            for (SourceFile file : list) {
                if (FILE_TYPE.isFortranFile(file.getFile())) {
                    srcfiles.add(file);
                }
                else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
                    xmlfiles.add(file);
                }
            }
        }
        FileTreeModel treeModel = null;
        List<SourceFile> setfiles = null;
        if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
            // XMLツリーに選択XMLファイルを表示する。
            treeModel = this.controller.getXmlTreeModel();
            setfiles = xmlfiles;
        }
        else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
            // Fortranファイルの追加
            treeModel = this.controller.getSourceTreeModel();
            setfiles = srcfiles;
        }
        if (treeModel != null) {
            if (setfiles != null && setfiles.size() > 0) {
                treeModel.setSourceFile(setfiles.toArray(new SourceFile[0]));
            }
            else {
                treeModel.clearTreeModel();
            }
        }

        // ソースビューの閉じたファイルを閉じる
        for (int i=0; i<selectedFiles.length; i++) {
            String filename = selectedFiles[i].getPath();
            this.controller.getMainframe().getPanelSourceView().closeSourceFile(filename);
        }

        Application.status.setMessageMain(message +
        		Message.getString("action.common.done.status")); //完了

        return;
    }

}


