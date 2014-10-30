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
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.ProfilerTableBaseModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProfilerService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロファイラデータ読込アクションクラス
 * @author RIKEN
 *
 */
public class ProfilerOpenFileAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ProfilerOpenFileAction(AppController controller) {
        super(controller);
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.profiler.read"); //"プロファイラデータ読込"
        Application.status.setMessageMain(message);

        // メインフレーム
        Frame frame = this.controller.getMainframe();

        // プロジェクト情報
        ProjectModel project = this.controller.getProjectModel();
        String projectFolder = null;
        if (project.getProjectFolder() != null) {
            projectFolder = project.getProjectFolder().getAbsolutePath();
        }
        if (projectFolder == null) {
            projectFolder = System.getProperty("user.dir");
        }

        // プロファイラデータフィルタ
        String description = Message.getString("profileropenfileaction.selectdialog.filterdescription"); //"プロファイラデータ(*.*)"
        SwingUtils.ExtFileFilter filter = new SwingUtils().new ExtFileFilter(description, null);

        // ファイル選択ダイアログを表示する。
        File[] selected = SwingUtils.showOpenFileDialog(frame, 
        		Message.getString("profileropenfileaction.selectdialog.title"), //"プロファイラデータの選択"
        		projectFolder, filter, true);
        if (selected == null || selected.length <= 0) {
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }

        // プロファイラサービス
        ProfilerService service = new ProfilerService();
        service.setErrorInfoModel(this.controller.getErrorInfoModel());
        // プロファイラモデル
        ProfilerTableBaseModel[] models = this.controller.getMainframe().getPanelAnalysisView().getProfilerModels();
        service.setProfilerModels(models);
        // ツリー上のソースファイル
        SourceFile[] files = this.controller.getMainframe().getPanelExplorerView().getPanelSourceTree().getAllSourceFiles();
        service.setSourceFiles(files);
        // プロファイラ情報
        service.setProfilerInfo(this.controller.getProfilerInfo());
        // データベース
        service.setFortranLanguage(this.controller.getFortranLanguage());
        // プロファイラプロパティ
        service.setPropertiesProfiler(this.controller.getPropertiesProfiler());
        // プロファイラファイルから読込
        service.loadProfilerDataFile(selected[0]);

        // 読込データの分析タブをアクティブにする。
        String fileType = service.getFileType();
        String paEventName = service.getPaEventName();
        setSelectedPanel(fileType, paEventName);

    	Application.status.setMessageMain(message +
    			Message.getString("action.common.done.status")); //完了

        return;
    }

    /**
     * 読込データのタブをアクティブ(開く)にする。
     * @param fileType  読込ファイルタイプ
     * @param paEventName PAイベント指定値(EPRFのみ)
     */
    private void setSelectedPanel(String fileType, String paEventName) {

        ANALYSIS_PANEL panel = null;
        if ("DPRF".equalsIgnoreCase(fileType)) {
            // コスト情報パネル、コールグラフパネルすべて開く
            this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.COST_PROCEDURE);
            this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.COST_LOOP);
            this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.COST_LINE);
            this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.CALLGRAPH);
            // アクティブパネルはコスト情報:手続とする
            panel = ANALYSIS_PANEL.COST_PROCEDURE;
        }
        else if ("EPRF".equalsIgnoreCase(fileType)) {
            if (PROFILERINFO_TYPE.EVENTCOUNTER_CACHE.getName().indexOf(paEventName) >= 0) {
                panel = ANALYSIS_PANEL.EVENTCOUNTER_CACHE;
            }
            else if (PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS.getName().indexOf(paEventName) >= 0) {
                panel = ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS;
            }
            else if (PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS.getName().indexOf(paEventName) >= 0) {
                panel = ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS;
            }
            else if (PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE.getName().indexOf(paEventName) >= 0) {
                panel = ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE;
            }
            else if (PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS.getName().indexOf(paEventName) >= 0) {
                panel = ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS;
            }
        }
        if (panel != null) {
            this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(panel);
        }

        return;
    }

}


