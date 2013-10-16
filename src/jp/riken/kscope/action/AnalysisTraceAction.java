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

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.TraceChooserDialog;
import jp.riken.kscope.gui.TraceResultPanel;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.service.AnalysisTraceService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.FileUtils;

/**
 * トレースアクション
 * @author riken
 *
 */
public class AnalysisTraceAction extends ActionBase {

    /** トレース方向 */
    private TRACE_DIR tracedir;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param dir			トレース方向
     */
    public AnalysisTraceAction(AppController controller, TRACE_DIR dir) {
        super(controller);
        this.tracedir = dir;
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        if (tracedir == TRACE_DIR.START) {
            // トレース開始
            // ソースコードの選択行を取得する
            CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
            if (line == null) return false;

            // 選択文字列
            String statement = line.getStatement();
            if (statement == null || statement.isEmpty()) return false;
        }
        else if (tracedir == TRACE_DIR.UP || tracedir == TRACE_DIR.DOWN
                || tracedir == TRACE_DIR.IN || tracedir == TRACE_DIR.OUT
                || tracedir == TRACE_DIR.FORWARD
                || tracedir == TRACE_DIR.END || tracedir == TRACE_DIR.REFRESH) {
            // トレース
            TraceResultModel[] models = this.controller.getMainframe().getPanelAnalysisView().getTraceResultModels();
            if (models == null || models.length <= 0) return false;
            for (TraceResultModel model : models) {
                if (model.getTraceWord() == null || model.getTraceWord().isEmpty()) {
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
        final String message = Message.getString("mainmenu.window.analysis.trace"); //トレース

        if (tracedir == TRACE_DIR.START) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-start.status")); //:開始

            // 選択文字列からトレースを開始する.
            startTrace();
        }
        else if (tracedir == TRACE_DIR.UP) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-up.satatus")); //:アップ

            // トレース:アップを行う
            traceUp();
        }
        else if (tracedir == TRACE_DIR.DOWN) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-down.status"));//:ダウン

            // トレース:ダウンを行う
            traceDown();
        }
        else if (tracedir == TRACE_DIR.IN) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-in.status"));//:イン

            // トレース:インを行う
            traceIn();
        }
        else if (tracedir == TRACE_DIR.OUT) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-out.status"));//:アウト

            // トレース:アウトを行う
            traceOut(false);
        }
        else if (tracedir == TRACE_DIR.FORWARD) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-forword.status"));//:フォワード

            // トレース:フォワードを行う
            traceOut(true);
        }
        else if (tracedir == TRACE_DIR.REFRESH) {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-refresh.status"));//:リフレッシュ

            // トレースタブをアクティブにする
            if (this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel() == null) {
                this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.TRACE);
            }
        }

        if (tracedir != TRACE_DIR.END) {
            // キーワードリストを取得する
            setTraceKeywords();
        }
        else {
            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("analysistraceaction.trace-clear.status"));//:クリア

            // トレース結果をクリアする
            clearTrace();
            // ソースビューからトレースキーワードの削除
            this.controller.getMainframe().getPanelSourceView().clearSearchWords(KEYWORD_TYPE.TRACE);
        }

    }

    /**
     * 選択文字列からトレースを開始する.
     */
    private void startTrace() {

        // ソースコードの選択行を取得する
        CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
        if (line == null) return;

        // ソースファイルをプロジェクトフォルダの相対パスに変更する
        SourceFile srcfile = line.getSourceFile();
        if (srcfile.getFile().isAbsolute()) {
            File projectfolder = this.controller.getProjectModel().getProjectFolder();
            String relpath = FileUtils.getRelativePath(srcfile.getFile(), projectfolder);
            if (relpath != null && !relpath.isEmpty()) {
                line = new CodeLine(line);
                SourceFile relfile = new SourceFile(relpath);
                relfile.setFileType(srcfile.getFileType());
                line.setSourceFile(relfile);
            }
        }

        // 選択文字列
        String statement = line.getStatement();
        if (statement == null || statement.isEmpty()) return;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 分析:トレースサービス
        AnalysisTraceService service = new AnalysisTraceService(fortran);
        service.setErrorInfoModel(errorModel);
        // トレース対象変数名を設定する
        service.setTraceWord(statement);

        // トレース結果を取得する
        TraceResultModel modelTrace = service.analysisTraceStart(line);
        if (modelTrace == null) {
            // ステータスメッセージ
            Application.status.setMessageMain(Message.getString("analysistraceaction.trace-start-no-target.status"));//トレース:開始[対象なし]
            return;
        }

        // トレースパスを追加する
        IBlock[] paths = {getRootBlock(modelTrace)};
        modelTrace.setTracePath(paths);

        // トレース結果を表示する
        this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(modelTrace);

    }


    /**
     * トレース結果をクリアする
     */
    private void clearTrace() {
        // トレース結果をクリアする
        // トレースタブすべてを閉じる。
        this.controller.getMainframe().getPanelAnalysisView().clearTrace();
    }

    /**
     * トレース:アップを行う
     */
    private void traceUp() {
        // トレースパネルで上のノード探索を行う
        TraceResultPanel panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
        if (panel == null) return;

        // トレース:アップを行う
        panel.traceUp();
    }


    /**
     * トレース:ダウンを行う
     */
    private void traceDown() {
        // トレースパネルで上のノード探索を行う
        TraceResultPanel panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
        if (panel == null) return;

        // トレース:ダウンを行う
        panel.traceDown();
    }

    /**
     * トレース:インを行う
     */
    private void traceIn() {
        // 選択トレースパネル
        TraceResultPanel panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
        if (panel == null) return;
        // 選択トレースモデル
        TraceResultModel model = panel.getModel();
        // トレース変数
        String statement = model.getTraceWord();
        // 現在の選択ブロック
        IBlock selectedBlock = model.getSelectedBlock();
        if (statement == null || selectedBlock == null) return;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 分析:トレースサービス
        AnalysisTraceService service = new AnalysisTraceService(fortran);
        service.setErrorInfoModel(errorModel);
        // トレース対象変数名を設定する
        service.setTraceWord(statement);

        // トレース:イン結果を取得する
        TraceResultModel[] modelTraces = service.analysisTraceIn(selectedBlock);
        if (modelTraces == null || modelTraces.length <= 0 || modelTraces[0] == null) {
            // ステータスメッセージ
            Application.status.setMessageMain(Message.getString("analysistraceaction.trace-in-no-target.status"));//トレース:イン[対象なし]
            return;
        }

        // トレース先
        TraceResultModel selectTrace = modelTraces[0];
        if (modelTraces.length > 1) {
            // トレース先選択ダイアログを表示して、トレース先を選択する。
            selectTrace = showTraceChooserDialog(modelTraces);
        }
        if (selectTrace == null) return;

        // トレースパスを追加する
        List<IBlock> list = new ArrayList<IBlock>();
        IBlock[] paths = model.getTracePath();
        if (paths != null && paths.length > 0) {
            list.addAll(Arrays.asList(paths));
        }
        list.add(selectedBlock);
        list.add(getRootBlock(selectTrace));
        selectTrace.setTracePath(list.toArray(new IBlock[0]));

        // トレース結果を表示する
        this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(selectTrace);

    }


    /**
     * トレース:アウトを行う
     * @param  forward		フォワードを行う。
     */
    private void traceOut(boolean forward) {

        // 選択トレースパネル
        TraceResultPanel panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
        if (panel == null) return;
        // 選択トレースモデル
        TraceResultModel model = panel.getModel();
        // トレース変数
        String statement = model.getTraceWord();
        // 現在のルートブロック
        IBlock rootBlock = model.getRootBlock();
        if (statement == null || rootBlock == null) return;
        // トレースパス
        IBlock[] tracePath = null;
        if (forward) {
            // トレース：フォワード
            tracePath = model.getTracePath();
        }

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 分析:トレースサービス
        AnalysisTraceService service = new AnalysisTraceService(fortran);
        service.setErrorInfoModel(errorModel);
        // トレース対象変数名を設定する
        service.setTraceWord(statement);

        // トレース:アウト結果を取得する
        TraceResultModel[] modelTraces = service.analysisTraceOut(rootBlock, tracePath);
        if (modelTraces == null || modelTraces.length <= 0 || modelTraces[0] == null) {
            // ステータスメッセージ
            Application.status.setMessageMain(Message.getString("analysistraceaction.trace-out-no-target.status"));//トレース:アウト[対象なし]
            return;
        }

        // トレース先
        TraceResultModel selectTrace = modelTraces[0];
        if (modelTraces.length > 1) {
            // トレース先選択ダイアログを表示して、トレース先を選択する。
            selectTrace = showTraceChooserDialog(modelTraces);
        }
        if (selectTrace == null) return;

        // トレースパスを追加する
        List<IBlock> list = new ArrayList<IBlock>();
        if (forward) {
            // トレース：フォワード
            tracePath = model.getTracePath();
            if (tracePath != null && tracePath.length > 0) {
                list.addAll(Arrays.asList(tracePath));
                if (list.size() > 2) {
                    // 最後ノードの削除
                    list.remove(list.size()-1);
                    list.remove(list.size()-1);
                }
            }
        }
        else {
            list.add(getRootBlock(selectTrace));
        }
        //list.add(getRootBlock(selectTrace));
        selectTrace.setTracePath(list.toArray(new IBlock[0]));

        // トレース結果を表示する
        this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(selectTrace);

    }


    /**
     * トレース先選択ダイアログを表示する
     * @return		トレース結果モデル
     */
    private TraceResultModel showTraceChooserDialog(TraceResultModel[] traces) {
        TraceChooserDialog dialog = new TraceChooserDialog(this.controller.getMainframe(), true);
        dialog.setTraceResultModel(traces);

        // 該当個所を開くアクションの設定
        dialog.setViewOpenAnalysisLineAction(new ViewOpenAnalysisLineAction(this.controller));

        // トレース選択ダイアログを開く
        int result = dialog.showDialog();
        if (result == Constant.CANCEL_DIALOG) return null;

        return dialog.getTraceResultModel();
    }

    /**
     * トレースキーワードを設定する
     */
    public void setTraceKeywords() {
        // キーワードリストを取得する
        this.controller.setTraceKeywords();
    }


    /**
     * ルートブロックを取得する
     * @param modelTrace		トレースモデル
     * @return   ルートブロック
     */
    private IBlock getRootBlock(TraceResultModel modelTrace) {
        if (modelTrace == null) return null;
        DefaultMutableTreeNode root = modelTrace.getRootNode();
        if (root == null) return null;
        if (root.getUserObject() == null) return null;
        if (root.getUserObject() instanceof IBlock) {
            return (IBlock)root.getUserObject();
        }

        return null;
    }
}
