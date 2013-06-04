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

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProfilerService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロファイラ測定区間フォルダ保存アクションクラス
 * @author riken
 *
 */
public class ProfilerSaveFolderAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ProfilerSaveFolderAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        // プロジェクト情報
        ProjectModel project = this.controller.getProjectModel();
        String projectFolder = null;
        if (project.getProjectFolder() != null) {
            projectFolder = project.getProjectFolder().getAbsolutePath();
        }
        if (projectFolder == null) {
            return false;
        }
        // 測定区間情報
        if (this.controller.getProfilerInfo() == null) {
            return false;
        }
        if (this.controller.getProfilerInfo().getMeasureInfo() == null) {
            return false;
        }
        ProfilerMeasureInfo measureInfo = this.controller.getProfilerInfo().getMeasureInfo();
        if (measureInfo.getMeasureDataCount() <= 0) {
            return false;
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
        final String message = Message.getString("mainmenu.profiler.savefolder-mesuermentrange"); //測定区間:フォルダ保存
        Application.status.setMessageMain(message);
        // メインフレーム
        Frame frame = this.controller.getMainframe();

        // プロジェクト情報
        ProjectModel project = this.controller.getProjectModel();
        File projectFolder = project.getProjectFolder();

        String title = Message.getString("profilersavefolderaction.savefolder.selectdialog.title"); //測定区間:フォルダ保存の選択
        // フォルダ選択ダイアログを表示する。
        File[] selected = SwingUtils.showSaveFolderDialog(frame, title, projectFolder.getAbsolutePath(), false);
        if (selected == null || selected.length <= 0) return;

        // 測定区間情報
        ProfilerMeasureInfo measureInfo = this.controller.getProfilerInfo().getMeasureInfo();
        // プロファイラサービス
        ProfilerService service = new ProfilerService();
        service.setErrorInfoModel(this.controller.getErrorInfoModel());
        service.setMeasureInfo(measureInfo);
        service.setProjectFolder(projectFolder);
        // プロファイラプロパティ
        service.setPropertiesProfiler(this.controller.getPropertiesProfiler());

        try {
            // 上書き保存実行
            service.saveMeasureFile(selected[0]);
        } catch (Exception ex) {
            ex.printStackTrace();
            Application.status.setMessageMain(message+
            		Message.getString("action.common.failed.status") //:失敗
            		);
            return;
        }

        Application.status.setMessageMain(message+
        		Message.getString("action.common.done.status") //:完了
        		);

        return;
    }
}


