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

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProjectService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * XMLファイル追加アクションクラス
 * @author riken
 *
 */
public class ProjectAddFileAction extends ActionBase {

    /** アクションエクスプローラパネル */
    private EXPLORE_PANEL panelExplore;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param panel			アクションエクスプローラパネル
     */
    public ProjectAddFileAction(AppController controller, EXPLORE_PANEL panel) {
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
        ProjectModel project = this.controller.getProjectModel();
        if (project == null) return false;
        if (project.getFileType() == FILE_TYPE.XCODEML_XML
            && panelExplore == EXPLORE_PANEL.XML) {
            return true;
        }
        if (project.getFileType() == FILE_TYPE.FORTRANLANG
            && panelExplore == EXPLORE_PANEL.SOURCE) {
            return true;
        }

        // メニューのイネーブル切替
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
    	// プロジェクト情報
        ProjectModel project = this.controller.getProjectModel();

        // ステータスメッセージ
    	String message = null;
    	if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
    		message = Message.getString("mainmenu.project.addxmlfile"); //XMLファイルの追加
        }
        else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
        	message = Message.getString("projectaddfileaction.addfile.fortran.status"); //Fortranファイルの追加
        }
    	Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        String projectFolder = null;
        if (project.getProjectFolder() != null) {
            projectFolder = project.getProjectFolder().getAbsolutePath();
        }
        if (projectFolder == null) {
            projectFolder = System.getProperty("user.dir");
        }

        // ファイル選択ダイアログのタイトル
        String title = null;
        SwingUtils.ExtFileFilter filter = null;
        if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
            title = Message.getString("projectaddfileaction.selectfiledialog.xml.title"); //"XMLファイルの選択"
            // XMLファイルフィルタ
            // FILE_TYPEからフィルタ作成に変更 at 2013/05/14 by @hira
            // String description = Message.getString("projectaddfileaction.selectfiledialog.xml.filterdescription"); //XcodeMLファイル(*.xml)
            // String[] exts = {"xml"};
            // filter = new SwingUtils().new ExtFileFilter(description, exts);
            filter = new SwingUtils().new ExtFileFilter(FILE_TYPE.getXcodemlFilter());
        }
        else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
            title = Message.getString("projectaddfileaction.selectfiledialog.fortran.title"); //Fortranファイルの選択
            // ソースファイルフィルタ
            // FILE_TYPEからフィルタ作成に変更 at 2013/05/14 by @hira
            // String description = Message.getString("projectaddfileaction.selectfiledialog.fortran.filterdescription"); //ソースファイル(*.f,*.f90)
            // String[] exts = {"f", "f90"};
            // filter = new SwingUtils().new ExtFileFilter(description, exts);
            filter = new SwingUtils().new ExtFileFilter(FILE_TYPE.getFortranFilter());
        }

        // ファイル選択ダイアログを表示する。
        File[] selected = SwingUtils.showOpenFileDialog(frame, title, projectFolder, filter, true);
        if (selected == null || selected.length <= 0) {
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }

        // 選択ファイルを追加する
        List<File> list = java.util.Arrays.asList(selected);

        // プロジェクトにフォルダ追加
        ProjectService service = new ProjectService(project);
        service.addProjectSelectedFile(list);

        // XMLツリーに選択XMLファイルを表示する。
        List<SourceFile> listSource = project.getListSelectedFile();
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

        Application.status.setMessageMain(message +
    			Message.getString("action.common.done.status")); //完了

        return;
    }

}


