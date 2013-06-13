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

package jp.riken.kscope.menu;

import java.awt.event.ActionListener;
import java.util.EventListener;
import java.util.List;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.AnalysisMemoryAction;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.AnalysisTraceAction;
import jp.riken.kscope.action.AnalysisVariableAction;
import jp.riken.kscope.action.EditClipboardCopyAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.action.ErrorOpenFileAction;
import jp.riken.kscope.action.FileExitAction;
import jp.riken.kscope.action.FileExportAnalysisAction;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.action.FileExportSourceFileAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.FileProjectCloseAction;
import jp.riken.kscope.action.FileProjectNewAction;
import jp.riken.kscope.action.FileProjectOpenAction;
import jp.riken.kscope.action.FileProjectSaveAction;
import jp.riken.kscope.action.HelpVersionAction;
import jp.riken.kscope.action.ProfilerAddEprofAction;
import jp.riken.kscope.action.ProfilerClearAction;
import jp.riken.kscope.action.ProfilerOpenFileAction;
import jp.riken.kscope.action.ProfilerSaveFileAction;
import jp.riken.kscope.action.ProfilerSaveFolderAction;
import jp.riken.kscope.action.ProfilerViewRulerAction;
import jp.riken.kscope.action.ProjectAddFileAction;
import jp.riken.kscope.action.ProjectAddFolderAction;
import jp.riken.kscope.action.ProjectBuildAction;
import jp.riken.kscope.action.ProjectClearLanguageAction;
import jp.riken.kscope.action.ProjectDeleteFileAction;
import jp.riken.kscope.action.ProjectPropertyAction;
import jp.riken.kscope.action.ProjectRebuildAction;
import jp.riken.kscope.action.ProjectSettingKeywordAction;
import jp.riken.kscope.action.ProjectSettingMemoryAction;
import jp.riken.kscope.action.ProjectSettingOperandAction;
import jp.riken.kscope.action.ProjectSettingProfilerAction;
import jp.riken.kscope.action.ProjectSettingProjectAction;
import jp.riken.kscope.action.ProjectSettingSSHAction;
import jp.riken.kscope.action.ProjectSettingToolsAction;
import jp.riken.kscope.action.ProjectSettingViewAction;
import jp.riken.kscope.action.SearchFindAction;
import jp.riken.kscope.action.SearchGrepAction;
import jp.riken.kscope.action.SearchResultAction;
import jp.riken.kscope.action.SearchTreeAction;
import jp.riken.kscope.action.ThreadCancelAction;
import jp.riken.kscope.action.TreeCollapseAllAction;
import jp.riken.kscope.action.TreeExpandAllAction;
import jp.riken.kscope.action.TreeExpandSelectAction;
import jp.riken.kscope.action.ViewCloseAction;
import jp.riken.kscope.action.ViewLangugeFilterAction;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.action.ViewOpenExploreBlockAction;
import jp.riken.kscope.action.ViewOpenLanguageTreeAction;
import jp.riken.kscope.action.ProfilerViewBargraphAction;
import jp.riken.kscope.action.WindowProfilerLegendAction;
import jp.riken.kscope.action.WindowProgressAction;
import jp.riken.kscope.action.WindowSourceAction;
import jp.riken.kscope.action.WindowViewAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.service.AppController;

/**
 * メインメニューバークラス
 * @author riken
 *
 */
public class MainMenu extends JMenuBar implements  MenuListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** プロジェクトメニュー */
    private JMenu menuProject;
    /** プロジェクト:XMLファイル削除 */
    private JMenuItem menuProjectDeleteXmlFile;
    /** 構造解析キャンセルメニュー */
    private JMenuItem menuProjectCancel;
    /** ウィンドウメニュー */
    private JMenu menuWindow;
    /** ウィンドウ:進捗状況 */
    private JMenuItem menuWindowProgress;
    /** ウィンドウ:エクスプローラビュー */
    private JMenu menuWindowExplore;
    /** ウィンドウ:ソースビュー */
    private JMenu menuWindowSource;
    /** アプリケーションコントローラ */
    private AppController controller;
    /** ウィンドウ:エクスプローラビュー:構造 */
    private JMenuItem menuWindowExploreLanguage;
    /** ウィンドウ:分析ビュー:トレース */
    private JMenuItem menuWindowAnalysisTrace;
    /** ウィンドウ:分析ビュー */
    private JMenu menuWindowAnalysis;
    /** 表示 */
    private JMenu menuView;
    /** 表示:構造フィルタ */
    private JMenu menuViewFilter;
    /** プロファイラバーグラフ表示 */
    private JCheckBoxMenuItem menuProfilerBarVisibled;
    /** プロファイラコストルーラ表示 */
    private JCheckBoxMenuItem menuProfilerRulerVisibled;
    /** 付加情報編集アクション */
    private EditInformationEditAction actionEditInformation;
    /** 分析情報エクスポートアクション */
    private FileExportAnalysisAction actionExportAnalysis;
    /** エラー箇所を開くアクション */
    private ErrorOpenFileAction actionErrorOpenFile;
    /** 分析結果該当個所を開くアクション */
    private ViewOpenAnalysisLineAction actionOpenAnalysisLine;
    /** 分析:変数特性一覧アクション */
    private AnalysisVariableAction actionAnalysisVariable;
    /** 分析:演算カウントアクション */
    private AnalysisOperandAction actionAnalysisOperand;
    /** すべて収納アクション */
    private TreeCollapseAllAction actionTreeCollapseAll;
    /** 新規構造ツリーアクション */
    private ViewOpenLanguageTreeAction actionOpenLanguageTree;

    /**
     * コンストラクタ
     */
    public MainMenu() {
        // メニューの作成を行う。
        initialize();
    }


    /**
     * コンストラクタ
     * @param controller		アプリケーションコントローラ
     */
    public MainMenu(AppController controller) {
        this.controller = controller;

        // メニューの作成を行う。
        initialize();
    }

    /**
     * メニューの作成を行う。
     */
    private void initialize() {

        // ファイル
        JMenu menuFile = new JMenu(Message.getString("mainmenu.file")); //ファイル
        this.add(menuFile);
        menuFile.addMenuListener(this);
        // ファイル:プロジェクトの新規作成
        JMenuItem menuFileProjectNew = new JMenuItem(Message.getString("mainmenu.file.newproject")); //プロジェクトの新規作成
        menuFileProjectNew.addActionListener(new FileProjectNewAction(this.controller));

        menuFile.add(menuFileProjectNew);
        // ファイル:プロジェクトを開く
        JMenuItem menuFileProjectOpen = new JMenuItem(Message.getString("mainmenu.file.openproject")); //プロジェクトを開く
        menuFile.add(menuFileProjectOpen);
        menuFileProjectOpen.addActionListener(new FileProjectOpenAction(this.controller));

        // ファイル:プロジェクトを閉じる
        JMenuItem menuFileProjectClose = new JMenuItem(Message.getString("mainmenu.file.closeproject")); //プロジェクトを閉じる
        menuFile.add(menuFileProjectClose);
        menuFileProjectClose.addActionListener(new FileProjectCloseAction(this.controller));

        // ファイル:プロジェクトの保存
        JMenuItem menuFileProjectSave = new JMenuItem(Message.getString("mainmenu.file.saveproject")); //プロジェクトの保存
        menuFile.add(menuFileProjectSave);
        menuFileProjectSave.addActionListener(new FileProjectSaveAction(this.controller));

        // セパレータ
        menuFile.addSeparator();
        // ファイル:構造情報の差替 : delete 構造情報の差替 at 2013/04/10 by @hira
        // JMenuItem menuFileImport = new JMenuItem(Message.getString("mainmenu.file.replaceproject")); //構造情報の差替
        // menuFileImport.addActionListener(new FileProjectImportAction(this.controller));
        // menuFile.add(menuFileImport);

        // ファイル:エクスポート
        JMenu menuFileExport = new JMenu(Message.getString("mainmenu.file.export")); //エクスポート
        menuFile.add(menuFileExport);
        // ファイル:エクスポート:構造情報(TEXT)
        JMenuItem menuFileExportLanguage = new JMenuItem(Message.getString("mainmenu.file.export.structure")); //構造情報(TEXT)
        menuFileExport.add(menuFileExportLanguage);
        menuFileExportLanguage.addActionListener(new FileExportExploreAction(this.controller));
        // ファイル:エクスポート:分析情報
        JMenuItem menuFileExportAnalysis = new JMenuItem(Message.getString("mainmenu.file.export.analysis")); //分析情報
        menuFileExport.add(menuFileExportAnalysis);
        actionExportAnalysis = new FileExportAnalysisAction(this.controller);
        menuFileExportAnalysis.addActionListener(actionExportAnalysis);
        // ファイル：エクスポート：ソースファイル
        JMenuItem menuFileExportSourceFile = new JMenuItem(Message.getString("mainmenu.file.export.source")); //ソースファイル
        menuFileExport.add(menuFileExportSourceFile);
        menuFileExportSourceFile.addActionListener(new FileExportSourceFileAction(controller));

        menuFile.add(menuFileExport);

        // ファイル:外部ツールでソースファイルを開く
        JMenuItem menuFileOpenSourceFile = new JMenuItem(Message.getString("mainmenu.file.program")); //外部ツールで開く
        menuFile.add(menuFileOpenSourceFile);
        menuFileOpenSourceFile.addActionListener(new FileOpenSourceFileAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // セパレータ
        menuFile.addSeparator();
        // ファイル:終了
        JMenuItem menuFileExit = new JMenuItem(Message.getString("mainmenu.file.close")); //終了
        menuFile.add(menuFileExit);
        menuFileExit.addActionListener(new FileExitAction(this.controller));

        // 編集
        JMenu menuEdit = new JMenu(Message.getString("mainmenu.edit")); //編集
        this.add(menuEdit);
        menuEdit.addMenuListener(this);
        // 編集:切り取り
        JMenuItem menuEditCut = new JMenuItem(Message.getString("mainmenu.edit.cut")); //切り取り
        menuEdit.add(menuEditCut);
        // 編集:切り取り機能は未使用の為DisEnableにする。
        menuEditCut.setEnabled(false);

        // 編集:コピー
        JMenuItem menuEditCopy = new JMenuItem(Message.getString("mainmenu.edit.copy")); //コピー
        menuEdit.add(menuEditCopy);
        menuEditCopy.addActionListener(new EditClipboardCopyAction(this.controller, FRAME_VIEW.SOURCE_VIEW));
        // 編集:貼り付け
        JMenuItem menuEditPaste = new JMenuItem(Message.getString("mainmenu.edit.paste")); //貼り付け
        menuEdit.add(menuEditPaste);
        // 編集:貼り付け機能は未使用の為DisEnableにする。
        menuEditPaste.setEnabled(false);

        // セパレータ
        menuEdit.addSeparator();

        // 編集:付加情報編集
        JMenuItem menuEditInformationEdit = new JMenuItem(Message.getString("mainmenu.edit.info")); //付加情報編集
        menuEdit.add(menuEditInformationEdit);
        actionEditInformation = new EditInformationEditAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
        menuEditInformationEdit.addActionListener(actionEditInformation);

        // 検索
        JMenu menuSearch = new JMenu(Message.getString("mainmenu.search")); //検索
        this.add(menuSearch);
        menuSearch.addMenuListener(this);
        // 検索:ソース検索
        JMenuItem menuSearchFind = new JMenuItem(Message.getString("mainmenu.search.source")); //ソース検索
        menuSearch.add(menuSearchFind);
        menuSearchFind.addActionListener(new SearchFindAction(this.controller));
        // 検索:ファイル検索...
        JMenuItem menuSearchGrep = new JMenuItem(Message.getString("mainmenu.search.file")); //ファイル検索
        menuSearch.add(menuSearchGrep);
        menuSearchGrep.addActionListener(new SearchGrepAction(this.controller));
        // 検索:ツリー検索...
        JMenuItem menuSearchTree = new JMenuItem(Message.getString("mainmenu.search.tree")); //ツリー検索
        menuSearch.add(menuSearchTree);
        menuSearchTree.addActionListener(new SearchTreeAction(this.controller));
        // セパレータ
        menuSearch.addSeparator();
        // 検索:前を検索
        JMenuItem menuSearchUp = new JMenuItem(Message.getString("mainmenu.search.backward")); //前を検索
        menuSearch.add(menuSearchUp);
        menuSearchUp.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.UP));
        // 検索:次を検索
        JMenuItem menuSearchBackward = new JMenuItem(Message.getString("mainmenu.search.forward")); //次を検索
        menuSearch.add(menuSearchBackward);
        menuSearchBackward.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.DOWN));
        // 検索:リフレッシュ
        JMenuItem menuSearchRefresh = new JMenuItem(Message.getString("mainmenu.search.refresh"));//検索結果更新
        menuSearch.add(menuSearchRefresh);
        menuSearchRefresh.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.REFRESH));
        // 検索:検索結果クリア
        JMenuItem menuSearchClear = new JMenuItem(Message.getString("mainmenu.search.clear"));//検索結果クリア
        menuSearch.add(menuSearchClear);
        menuSearchClear.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.END));

        // プロジェクト
        this.menuProject = new JMenu(Message.getString("mainmenu.project"));//プロジェクト
        this.add(menuProject);
        menuProject.addMenuListener(this);

        // プロジェクト:構造解析実行
        JMenuItem menuProjectBuild = new JMenuItem(Message.getString("mainmenu.project.startanalysis"));//構造解析実行
        menuProject.add(menuProjectBuild);
        menuProjectBuild.addActionListener(new ProjectBuildAction((this.controller)));
        // プロジェクト:構造解析キャンセル
        menuProjectCancel = new JMenuItem(Message.getString("mainmenu.project.endanalysis"));//構造解析キャンセル
        menuProject.add(menuProjectCancel);
        menuProjectCancel.addActionListener(new ThreadCancelAction((this.controller)));

        // プロジェクト:構造解析クリア
        JMenuItem menuProjectClear = new JMenuItem(Message.getString("mainmenu.project.clearanalysis"));//構造解析クリア
        menuProject.add(menuProjectClear);
        menuProjectClear.addActionListener(new ProjectClearLanguageAction((this.controller)));

        // プロジェクト：構造解析再実行
        JMenuItem menuProjectRebuild = new JMenuItem(Message.getString("mainmenu.project.restertanalysis"));//構造解析再実行
        menuProject.add(menuProjectRebuild);
        menuProjectRebuild.addActionListener(new ProjectRebuildAction((this.controller)));

        // セパレータ
        menuProject.addSeparator();
        // プロジェクト:フォルダ追加...
        JMenuItem menuProjectAddXmlFolder = new JMenuItem(Message.getString("mainmenu.project.addxmlfolder"));//フォルダ追加...
        menuProject.add(menuProjectAddXmlFolder);
        menuProjectAddXmlFolder.addActionListener(new ProjectAddFolderAction(this.controller, null));
        // プロジェクト:ファイル追加...
        JMenuItem menuProjectAddXmlFile = new JMenuItem(Message.getString("mainmenu.project.addxmlfile"));//ファイル追加...
        menuProject.add(menuProjectAddXmlFile);
        menuProjectAddXmlFile.addActionListener(new ProjectAddFileAction(this.controller, null));
        // プロジェクト:ファイル削除...
        this.menuProjectDeleteXmlFile = new JMenuItem(Message.getString("mainmenu.project.removexmlfile"));//ファイル削除...
        menuProject.add(menuProjectDeleteXmlFile);
        menuProjectDeleteXmlFile.addActionListener(new ProjectDeleteFileAction(this.controller, null));
        // セパレータ
        menuProject.addSeparator();

        // プロジェクト:プロパティ
        JMenuItem menuProjectProperty = new JMenuItem(Message.getString("mainmenu.project.property"));//プロパティ
        menuProject.add(menuProjectProperty);
        menuProjectProperty.addActionListener(new ProjectPropertyAction(this.controller));

        // プロジェクト:設定
        JMenu menuProjectSetting = new JMenu(Message.getString("mainmenu.project.config"));//設定
        menuProject.add(menuProjectSetting);

        // プロジェクト：設定：プロジェクト
        JMenuItem menuProjectSettingProject = new JMenuItem(Message.getString("mainmenu.project.config.project")); // プロジェクト
        menuProjectSetting.add(menuProjectSettingProject);
        menuProjectSettingProject.addActionListener(new ProjectSettingProjectAction(this.controller));

        // プロジェクト:設定:キーワード
        JMenuItem menuProjectSettingKeyword = new JMenuItem(Message.getString("mainmenu.project.config.keyword"));//キーワード
        menuProjectSetting.add(menuProjectSettingKeyword);
        menuProjectSettingKeyword.addActionListener(new ProjectSettingKeywordAction(this.controller));

        // プロジェクト:設定:ソースビュー
        JMenuItem menuProjectSettingView = new JMenuItem(Message.getString("mainmenu.project.config.display"));//ソースビュー
        menuProjectSetting.add(menuProjectSettingView);
        menuProjectSettingView.addActionListener(new ProjectSettingViewAction(this.controller));

        // プロジェクト:設定:演算カウント
        JMenuItem menuProjectSettingOperation = new JMenuItem(Message.getString("mainmenu.project.config.operation"));//演算カウント
        menuProjectSetting.add(menuProjectSettingOperation);
        menuProjectSettingOperation.addActionListener(new ProjectSettingOperandAction(this.controller));

        // プロジェクト:設定:外部ツール
        JMenuItem menuProjectSettingTools = new JMenuItem(Message.getString("mainmenu.project.config.program"));//外部ツール
        menuProjectSetting.add(menuProjectSettingTools);
        menuProjectSettingTools.addActionListener(new ProjectSettingToolsAction(this.controller));

        // プロジェクト:設定:プロファイラ設定
        JMenuItem menuProjectSettingProfiler = new JMenuItem(Message.getString("mainmenu.project.config.profiler"));//プロファイラ
        menuProjectSetting.add(menuProjectSettingProfiler);
        menuProjectSettingProfiler.addActionListener(new ProjectSettingProfilerAction(this.controller));

        // プロジェクト:設定:要求Bye/FLOP設定
        JMenuItem menuProjectSettingMemory = new JMenuItem(Message.getString("mainmenu.project.config.memoryband"));//要求Bye/FLOP
        menuProjectSetting.add(menuProjectSettingMemory);
        menuProjectSettingMemory.addActionListener(new ProjectSettingMemoryAction(this.controller));
        
        // セパレータ
        menuProjectSetting.addSeparator();
        // SSHconnect 設定
        JMenuItem menuProjectSettingSSH = new JMenuItem("SSHconnect");
        menuProjectSetting.add(menuProjectSettingSSH);
        menuProjectSettingSSH.addActionListener(new ProjectSettingSSHAction(this.controller));

        // 分析
        JMenu menuAnalysis = new JMenu(Message.getString("mainmenu.analysis"));//分析
        this.add(menuAnalysis);
        menuAnalysis.addMenuListener(this);

        // 分析:変数特性一覧
        JMenuItem menuAnalysisVariable = new JMenuItem(Message.getString("mainmenu.analysis.valiableproperty"));//変数特性一覧
        menuAnalysis.add(menuAnalysisVariable);
        actionAnalysisVariable = new AnalysisVariableAction(this.controller);
        menuAnalysisVariable.addActionListener(actionAnalysisVariable);
        this.controller.setActionVariable(actionAnalysisVariable);

        // 分析:演算カウント
        JMenuItem menuAnalysisCount = new JMenuItem(Message.getString("mainmenu.analysis.operation"));//演算カウント
        actionAnalysisOperand = new AnalysisOperandAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
        menuAnalysis.add(menuAnalysisCount);
        menuAnalysisCount.addActionListener(actionAnalysisOperand);

        // 分析:変数有効域
        JMenuItem menuAnalysisValid = new JMenuItem(Message.getString("mainmenu.analysis.valiablescope"));//変数有効域
        menuAnalysis.add(menuAnalysisValid);
        menuAnalysisValid.addActionListener(new AnalysisScopeAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // セパレータ
        menuAnalysis.addSeparator();

        // 分析:宣言・定義・参照
        JMenuItem menuAnalysisReference = new JMenuItem(Message.getString("mainmenu.analysis.dec-def-ref"));//宣言・定義・参照
        menuAnalysis.add(menuAnalysisReference);
        menuAnalysisReference.addActionListener(new AnalysisReferenceAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // セパレータ
        menuAnalysis.addSeparator();
        // 分析:トレース：開始
        JMenuItem menuAnalysisTraceStart = new JMenuItem(Message.getString("mainmenu.analysis.starttrace"));//トレース：開始
        menuAnalysis.add(menuAnalysisTraceStart);
        menuAnalysisTraceStart.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.START));

        // 分析:トレース：更新
        JMenuItem menuAnalysisTraceRefresh = new JMenuItem(Message.getString("mainmenu.analysis.updatetrace"));//トレース：更新
        menuAnalysis.add(menuAnalysisTraceRefresh);
        menuAnalysisTraceRefresh.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.REFRESH));

        // 分析:トレース：クリア
        JMenuItem menuAnalysisTraceClear = new JMenuItem(Message.getString("mainmenu.analysis.cleartrace"));//トレース：クリア
        menuAnalysis.add(menuAnalysisTraceClear);
        menuAnalysisTraceClear.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.END));

        // セパレータ
        menuAnalysis.addSeparator();

        // 分析:トレース：前へ
        JMenuItem menuAnalysisUp = new JMenuItem(Message.getString("mainmenu.analysis.up"));//トレース：アップ
        menuAnalysis.add(menuAnalysisUp);
        menuAnalysisUp.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.UP));

        // 分析:トレース：次へ
        JMenuItem menuAnalysisDown = new JMenuItem(Message.getString("mainmenu.analysis.down"));//トレース：ダウン
        menuAnalysis.add(menuAnalysisDown);
        menuAnalysisDown.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.DOWN));

        // 分析:トレース：イン
        JMenuItem menuAnalysisInside = new JMenuItem(Message.getString("mainmenu.analysis.in"));//トレース：イン
        menuAnalysis.add(menuAnalysisInside);
        menuAnalysisInside.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.IN));

        // 分析:トレース：アウト
        JMenuItem menuAnalysisOutside = new JMenuItem(Message.getString("mainmenu.analysis.out"));//トレース：アウト
        menuAnalysis.add(menuAnalysisOutside);
        menuAnalysisOutside.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.OUT));

        // 分析:トレース：フォワード
        JMenuItem menuAnalysisForward = new JMenuItem(Message.getString("mainmenu.analysis.forward"));//トレース：フォワード
        menuAnalysis.add(menuAnalysisForward);
        menuAnalysisForward.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.FORWARD));

        // セパレータ
        menuAnalysis.addSeparator();

        // 分析:アクセス先設定
        JMenuItem menuAnalysisMemoryAccess = new JMenuItem(Message.getString("mainmenu.analysis.access"));  // 変数アクセス先設定
        menuAnalysis.add(menuAnalysisMemoryAccess);
        menuAnalysisMemoryAccess.addActionListener(
        				new AnalysisMemoryAction(
        							this.controller,
        							AnalysisMemoryAction.ACTION_MODE.ACCESS_SETTING,
        							FRAME_VIEW.EXPLORE_VIEW));

        // 分析:要求Byte/FLOP算出
        JMenuItem menuAnalysisMemoryCalculate = new JMenuItem(Message.getString("mainmenu.analysis.calculate"));  // 要求Byte/FLOP算出
        menuAnalysis.add(menuAnalysisMemoryCalculate);
        menuAnalysisMemoryCalculate.addActionListener(
						new AnalysisMemoryAction(
									this.controller,
									AnalysisMemoryAction.ACTION_MODE.MEMORY_CALCULATE,
									FRAME_VIEW.EXPLORE_VIEW));

        // プロファイラ
        JMenu menuProfiler = new JMenu(Message.getString("mainmenu.project.config.profiler"));
        this.add(menuProfiler);
        menuProfiler.addMenuListener(this);

        // プロファイラ：プロファイラの読込
        JMenuItem menuProfilerOpen = new JMenuItem(Message.getString("mainmenu.profiler.read"));
        menuProfiler.add(menuProfilerOpen);
        menuProfilerOpen.addActionListener(new ProfilerOpenFileAction(this.controller));

        // プロファイラ：プロファイラのクリア
        JMenuItem menuProfilerClear = new JMenuItem(Message.getString("mainmenu.profiler.clear"));
        menuProfiler.add(menuProfilerClear);
        menuProfilerClear.addActionListener(new ProfilerClearAction(this.controller));

        // プロファイラ：コスト表示
        this.menuProfilerBarVisibled = new JCheckBoxMenuItem(Message.getString("mainmenu.profiler.cost"));
        menuProfiler.add(menuProfilerBarVisibled);
        menuProfilerBarVisibled.addActionListener(new ProfilerViewBargraphAction(this.controller));

        // プロファイラ：コストルーラ表示
        this.menuProfilerRulerVisibled = new JCheckBoxMenuItem(Message.getString("mainmenu.profiler.costruler"));
        menuProfiler.add(menuProfilerRulerVisibled);
        menuProfilerRulerVisibled.addActionListener(new ProfilerViewRulerAction(this.controller));

        // セパレータ
        menuProfiler.addSeparator();

        // プロファイラ：測定区間設定
        JMenuItem menuProfilerAddEprofArea = new JMenuItem(Message.getString("mainmenu.profiler.set-mesuermentrange"));
        menuProfiler.add(menuProfilerAddEprofArea);
        menuProfilerAddEprofArea.addActionListener(new ProfilerAddEprofAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));
        // プロファイラ：測定区間上書き保存
        JMenuItem menuProfilerSavefile = new JMenuItem(Message.getString("mainmenu.profiler.save-mesuermentrange"));
        menuProfiler.add(menuProfilerSavefile);
        menuProfilerSavefile.addActionListener(new ProfilerSaveFileAction(this.controller));
        // プロファイラ：測定区間フォルダ保存
        JMenuItem menuProfilerSavefolder = new JMenuItem(Message.getString("mainmenu.profiler.savefolder-mesuermentrange"));
        menuProfiler.add(menuProfilerSavefolder);
        menuProfilerSavefolder.addActionListener(new ProfilerSaveFolderAction(this.controller));

        // 表示
        menuView = new JMenu(Message.getString("mainmenu.view"));//表示
        this.add(menuView);
        menuView.addMenuListener(this);

        // 表示:ツリー収納・展開
        JMenu menuViewTreeNode = new JMenu(Message.getString("mainmenu.view.collapse-expand"));//ツリー収納・展開
        menuView.add(menuViewTreeNode);
        // 表示:ツリー収納・展開:すべて収納
        JMenuItem menuViewTreeNodeAllCollapse = new JMenuItem(Message.getString("mainmenu.view.collapse-expand.collapse-all"));//すべて収納
        menuViewTreeNode.add(menuViewTreeNodeAllCollapse);
        this.actionTreeCollapseAll = new TreeCollapseAllAction(this.controller);
        menuViewTreeNodeAllCollapse.addActionListener(this.actionTreeCollapseAll);

        // 表示:ツリー収納・展開:すべて収納
        JMenuItem menuViewTreeNodeAllExpand = new JMenuItem(Message.getString("mainmenu.view.collapse-expand.expand-all"));//すべて展開
        menuViewTreeNode.add(menuViewTreeNodeAllExpand);
        menuViewTreeNodeAllExpand.addActionListener(new TreeExpandAllAction(this.controller));
        // 表示:ツリー収納・展開:展開
        JMenuItem menuViewTreeNodeExpand = new JMenuItem(Message.getString("mainmenu.view.collapse-expand.selective"));//選択展開
        menuViewTreeNode.add(menuViewTreeNodeExpand);
        menuViewTreeNodeExpand.addActionListener(new TreeExpandSelectAction(this.controller));

        // 表示:新規構造ツリー
        JMenuItem menuViewOpenTree = new JMenuItem(Message.getString("mainmenu.view.newtree"));//新規構造ツリー
        menuView.add(menuViewOpenTree);
        this.actionOpenLanguageTree = new ViewOpenLanguageTreeAction(this.controller);
        menuViewOpenTree.addActionListener(this.actionOpenLanguageTree);

        // 表示:ファイルを開く
        JMenuItem menuViewOpenFile = new JMenuItem(Message.getString("mainmenu.view.openfile"));//ファイルを開く
        menuView.add(menuViewOpenFile);
        menuViewOpenFile.addActionListener(new ViewOpenExploreBlockAction(this.controller));
        // 表示:閉じる
        JMenuItem menuViewClose = new JMenuItem(Message.getString("mainmenu.view.closefile"));//ファイルを閉じる
        menuView.add(menuViewClose);
        menuViewClose.addActionListener(new ViewCloseAction(this.controller));
        // 表示:すべて閉じる
        JMenuItem menuViewAllClose = new JMenuItem(Message.getString("mainmenu.view.close-all-file"));//すべて閉じる
        menuView.add(menuViewAllClose);
        menuViewAllClose.addActionListener(new ViewCloseAction(this.controller, true));

        // セパレータ
        menuView.addSeparator();
        // 表示:構造フィルタ
        menuViewFilter = new JMenu(Message.getString("mainmenu.view.filter"));//構造フィルタ
        menuView.add(menuViewFilter);
        // 表示:構造フィルタ:すべて表示
        JCheckBoxMenuItem menuViewFilterAll = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.viewall"));//すべて表示
        menuViewFilter.add(menuViewFilterAll);
        menuViewFilterAll.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.ALL));
        // 表示:構造フィルタ:サブルーチン・関数
        JCheckBoxMenuItem menuViewFilterSubroutine = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.subroutine-function"));//サブルーチン・関数
        menuViewFilter.add(menuViewFilterSubroutine);
        menuViewFilterSubroutine.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.PROCEDURE));
        // 表示:構造フィルタ:CALL文・関数呼出文
        JCheckBoxMenuItem menuViewFilterCall = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.call"));//CALL文・関数呼出文
        menuViewFilter.add(menuViewFilterCall);
        menuViewFilterCall.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.PROCEDUREUSAGE));
        // 表示:構造フィルタ:DO文
        JCheckBoxMenuItem menuViewFilterDo = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.do"));//DO文
        menuViewFilter.add(menuViewFilterDo);
        menuViewFilterDo.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.REPETITION));
        // 表示:構造フィルタ:IF文
        JCheckBoxMenuItem menuViewFilterIf = new JCheckBoxMenuItem(FILTER_TYPE.SELECTION_IF.getName());
        menuViewFilter.add(menuViewFilterIf);
        menuViewFilterIf.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.SELECTION_IF));
        // 表示:構造フィルタ:SELECT文
        JCheckBoxMenuItem menuViewFilterSelect = new JCheckBoxMenuItem(FILTER_TYPE.SELECTION_SELECT.getName());
        menuViewFilter.add(menuViewFilterSelect);
        menuViewFilterSelect.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.SELECTION_SELECT));
        // 表示:構造フィルタ:代入文
        JCheckBoxMenuItem menuViewFilterAssignment = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.expression-array"));//代入文
        menuViewFilter.add(menuViewFilterAssignment);
        menuViewFilterAssignment.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.SUBSTITUTION));
        // 表示:構造フィルタ:フロー制御文
        JCheckBoxMenuItem menuViewFilterFlow = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.other-flow-control"));//フロー制御文
        menuViewFilter.add(menuViewFilterFlow);
        menuViewFilterFlow.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.FLOW));
        // 表示:構造フィルタ:ディレクティブ文
        JMenu menuViewFilterDirective = new JMenu(Message.getString("mainmenu.view.filter.directive"));//ディレクティブ文
        JCheckBoxMenuItem menuViewFilterDirectiveOmp = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.openmp"));//OPENMP
        JCheckBoxMenuItem menuViewFilterDirectiveOcl = new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.ocl"));//OCL
        menuViewFilterDirective.add(menuViewFilterDirectiveOmp);
        menuViewFilterDirective.add(menuViewFilterDirectiveOcl);
        menuViewFilter.add(menuViewFilterDirective);
        menuViewFilterDirectiveOmp.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.DIRECTIVE_OPENML));
        menuViewFilterDirectiveOcl.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.DIRECTIVE_OCL));

        // セパレータ
        menuViewFilter.addSeparator();

        // 表示:構造フィルタ:デフォルト
        JMenuItem menuViewFilterDefault = new JMenuItem(Message.getString("mainmenu.view.filter.default"));//デフォルトに戻す
        menuViewFilter.add(menuViewFilterDefault);
        menuViewFilterDefault.addActionListener(new ViewLangugeFilterAction(this.controller, FILTER_TYPE.DEFAULT));

        // ウィンドウ
        menuWindow = new JMenu(Message.getString("mainmenu.window"));//ウィンドウ
        this.add(menuWindow);
        menuWindow.addMenuListener(this);

        // ウィンドウ:エクスプローラビュー
        menuWindowExplore = new JMenu(Message.getString("mainmenu.window.explore"));//エクスプローラビュー
        menuWindow.add(menuWindowExplore);
        // ウィンドウ:エクスプローラビュー:構造
        menuWindowExploreLanguage = new JMenuItem(Message.getString("mainmenu.window.explore.structure"));//構造
        menuWindowExplore.add(menuWindowExploreLanguage);
        menuWindowExploreLanguage.addActionListener(new WindowViewAction(this.controller, EXPLORE_PANEL.LANGUAGE));

        // ウィンドウ:エクスプローラビュー:モジュール
        JMenuItem menuWindowExploreModule = new JMenuItem(Message.getString("mainmenu.window.explore.module"));//モジュール
        menuWindowExplore.add(menuWindowExploreModule);
        menuWindowExploreModule.addActionListener(new WindowViewAction(this.controller, EXPLORE_PANEL.MODULE));

        // ウィンドウ:エクスプローラビュー:ソース
        JMenuItem menuWindowExploreSource = new JMenuItem(Message.getString("mainmenu.window.explore.source"));//ソース
        menuWindowExplore.add(menuWindowExploreSource);
        menuWindowExploreSource.addActionListener(new WindowViewAction(this.controller, EXPLORE_PANEL.SOURCE));

        // ウィンドウ:エクスプローラビュー:XML
        JMenuItem menuWindowExploreXml = new JMenuItem(Message.getString("mainmenu.window.explore.xml"));//XML
        menuWindowExplore.add(menuWindowExploreXml);
        menuWindowExploreXml.addActionListener(new WindowViewAction(this.controller, EXPLORE_PANEL.XML));

        // ウィンドウ:ソースビュー
        menuWindowSource = new JMenu(Message.getString("mainmenu.window.source"));//ソースビュー
        menuWindow.add(menuWindowSource);

        // ウィンドウ:ソースビュー:ソースファイル
        // menuSelectedイベントで開いているファイルを追加する。

        // ウィンドウ:分析ビュー
        menuWindowAnalysis = new JMenu(Message.getString("mainmenu.window.analysis"));//分析ビュー
        menuWindow.add(menuWindowAnalysis);
        // ウィンドウ:分析ビュー:付加情報
        JMenuItem menuWindowAnalysisInformation = new JMenuItem(Message.getString("mainmenu.window.analysis.information"));//付加情報
        menuWindowAnalysis.add(menuWindowAnalysisInformation);
        menuWindowAnalysisInformation.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.INFORMATION));

        // ウィンドウ:分析ビュー:検索結果
        JMenuItem menuWindowAnalysisSearch = new JMenuItem(Message.getString("mainmenu.window.analysis.search"));//検索結果
        menuWindowAnalysis.add(menuWindowAnalysisSearch);
        menuWindowAnalysisSearch.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.SEARCHRESULT));

        // ウィンドウ:分析ビュー:変数特性一覧
        JMenuItem menuWindowAnalysisVariable = new JMenuItem(Message.getString("mainmenu.analysis.valiableproperty"));//変数特性一覧
        menuWindowAnalysis.add(menuWindowAnalysisVariable);
        menuWindowAnalysisVariable.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.VALIABLE));

        // ウィンドウ:分析ビュー:演算カウント
        JMenuItem menuWindowAnalysisCount = new JMenuItem(Message.getString("mainmenu.analysis.operation"));//演算カウント
        menuWindowAnalysis.add(menuWindowAnalysisCount);
        menuWindowAnalysisCount.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.OPERAND));

        // ウィンドウ:分析ビュー:要求B/F算出結果
        JMenuItem menuWindowAnalysisRequired = new JMenuItem(Message.getString("mainmenu.window.analysis.byteflop"));//要求B/F算出結果
        menuWindowAnalysis.add(menuWindowAnalysisRequired);
        menuWindowAnalysisRequired.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.REQUIRED));

        // ウィンドウ:分析ビュー:参照一覧
        JMenuItem menuWindowAnalysisReference = new JMenuItem(Message.getString("mainmenu.analysis.dec-def-ref"));//参照一覧
        menuWindowAnalysis.add(menuWindowAnalysisReference);
        menuWindowAnalysisReference.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.REFERENCE));

        // ウィンドウ:分析ビュー:トレース
        menuWindowAnalysisTrace = new JMenuItem(Message.getString("mainmenu.window.analysis.trace"));//トレース
        menuWindowAnalysis.add(menuWindowAnalysisTrace);
        menuWindowAnalysisTrace.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.TRACE));

        // ウィンドウ:分析ビュー:変数有効域
        JMenuItem menuWindowAnalysisScope = new JMenuItem(Message.getString("mainmenu.analysis.valiablescope"));//変数有効域
        menuWindowAnalysis.add(menuWindowAnalysisScope);
        menuWindowAnalysisScope.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.SCOPE));

        // ウィンドウ:分析ビュー:構造情報差替え
        JMenuItem menuWindowReplace = new JMenuItem(Message.getString("mainmenu.window.analysis.structureinfo"));//構造情報差替え
        menuWindowAnalysis.add(menuWindowReplace);
        menuWindowReplace.addActionListener(new WindowViewAction(
                this.controller, ANALYSIS_PANEL.REPLACE));

        // ウィンドウ:分析ビュー:プロパティ
        JMenuItem menuWindowAnalysisProparty = new JMenuItem(Message.getString("mainmenu.project.property"));//分析プロパティ
        menuWindowAnalysis.add(menuWindowAnalysisProparty);
        menuWindowAnalysisProparty.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.PROPARTIES));

        // ウィンドウ:分析ビュー:エラー箇所
        JMenuItem menuWindowAnalysisError = new JMenuItem(Message.getString("mainmenu.window.analysis.error"));//エラー箇所
        menuWindowAnalysis.add(menuWindowAnalysisError);
        menuWindowAnalysisError.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.ERROR));

        // ウィンドウ:分析ビュー:コンソール
        JMenuItem menuWindowAnalysisConsole = new JMenuItem(Message.getString("mainmenu.window.analysis.console"));//コンソール
        menuWindowAnalysis.add(menuWindowAnalysisConsole);
        menuWindowAnalysisConsole.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.CONSOLE));

        // ウィンドウ:分析ビュー:コスト情報
        {
            JMenu menuViewProfilerInfo = new JMenu(Message.getString("mainmenu.window.analysis.profiler-info"));
            menuWindowAnalysis.add(menuViewProfilerInfo);
            // ウィンドウ:分析ビュー:コスト情報:手続
            JMenuItem menuWindowAnalysisCostProcedure = new JMenuItem(ANALYSIS_PANEL.COST_PROCEDURE.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisCostProcedure);
            menuWindowAnalysisCostProcedure.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.COST_PROCEDURE));
            // ウィンドウ:分析ビュー:コスト情報:ループ
            JMenuItem menuWindowAnalysisCostLoop = new JMenuItem(ANALYSIS_PANEL.COST_LOOP.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisCostLoop);
            menuWindowAnalysisCostLoop.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.COST_LOOP));
            // ウィンドウ:分析ビュー:コスト情報:ライン
            JMenuItem menuWindowAnalysisCostLine = new JMenuItem(ANALYSIS_PANEL.COST_LINE.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisCostLine);
            menuWindowAnalysisCostLine.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.COST_LINE));
            // ウィンドウ:分析ビュー:コールグラフ情報
            JMenuItem menuWindowAnalysisCallGraph = new JMenuItem(ANALYSIS_PANEL.CALLGRAPH.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisCallGraph);
            menuWindowAnalysisCallGraph.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.CALLGRAPH));
            // ウィンドウ:分析ビュー:プロファイラ:イベントカウンタ情報:Eprof:CACHE
            JMenuItem menuWindowAnalysisEprofCache = new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_CACHE.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisEprofCache);
            menuWindowAnalysisEprofCache.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_CACHE));
            // ウィンドウ:分析ビュー:プロファイラ:イベントカウンタ情報:Eprof:INSTRUCTIONS
            JMenuItem menuWindowAnalysisEprofInstructions = new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisEprofInstructions);
            menuWindowAnalysisEprofInstructions.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS));
            // ウィンドウ:分析ビュー:プロファイラ:イベントカウンタ情報:Eprof:MEM_ACCESS
            JMenuItem menuWindowAnalysisEprofMemaccess = new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisEprofMemaccess);
            menuWindowAnalysisEprofMemaccess.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS));
            // ウィンドウ:分析ビュー:プロファイラ:イベントカウンタ情報:Eprof:PERFORMANCE
            JMenuItem menuWindowAnalysisEprofPerformance = new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisEprofPerformance);
            menuWindowAnalysisEprofPerformance.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE));
            // ウィンドウ:分析ビュー:プロファイラ:イベントカウンタ情報:Eprof:STATISTICS
            JMenuItem menuWindowAnalysisEprofStatistics = new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisEprofStatistics);
            menuWindowAnalysisEprofStatistics.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS));
            // ウィンドウ:分析ビュー:タイマ情報
            JMenuItem menuWindowAnalysisEprofMeasure = new JMenuItem(ANALYSIS_PANEL.EPROF_MEASURE.getTabName());
            menuViewProfilerInfo.add(menuWindowAnalysisEprofMeasure);
            menuWindowAnalysisEprofMeasure.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.EPROF_MEASURE));
        }

        // ウィンドウ:進捗状況
        menuWindowProgress = new JMenuItem(Message.getString("mainmenu.window.analysis.progress"));//進捗状況
        menuWindow.add(menuWindowProgress);
        menuWindowProgress.addActionListener(new WindowProgressAction(this.controller));

        // ウィンドウ:プロファイラ凡例
        JMenuItem menuWindowSourcePreview = new JMenuItem(Message.getString("mainmenu.window.analysis.profiler-legend"));//プロファイラ凡例
        menuWindow.add(menuWindowSourcePreview);
        menuWindowSourcePreview.addActionListener(new WindowProfilerLegendAction(this.controller));

        // ヘルプ
        JMenu menuHelp = new JMenu(Message.getString("mainmenu.help"));//ヘルプ
        this.add(menuHelp);
        JMenuItem menuHelpVersion = new JMenuItem(Message.getString("mainmenu.help.about"));//バージョン情報...
        menuHelp.add(menuHelpVersion);
        menuHelpVersion.addActionListener(new HelpVersionAction(this.controller));

        // メニュー非表示
        // エラー箇所を開く
        actionErrorOpenFile = new ErrorOpenFileAction(this.controller);
        actionOpenAnalysisLine = new ViewOpenAnalysisLineAction(this.controller);

        // メニュー選択状態の初期化
        this.clearSelectedMenu();
    }


    /**
     * 分析情報エクスポートアクション
     * @return		分析情報エクスポートアクション
     */
    public FileExportAnalysisAction getActionExportAnalysis() {
        return actionExportAnalysis;
    }


    /**
     * メニュー選択アクション<br/>
     * メニュー表示、イネーブルの切替を行う。
     * @param event		イベント情報
     */
    @Override
    public void menuSelected(MenuEvent event) {

        // ウィンドウ:ソースビュー
        if (event.getSource() == this.menuWindow) {
            // 開いているファイルの一覧を取得する
            SourceFile[] list = this.controller.getMainframe().getPanelSourceView().getOpenedSourceFile();
            // ソースビューのイネーブルの切替
            menuWindowSource.setEnabled((list != null));

            // 表示ソースファイルのクリア
            menuWindowSource.removeAll();

            // ソースファイル名の追加
            if (list != null) {
                for (SourceFile file : list) {
                    // ウィンドウ:ソースビュー:ソースファイル
                    JMenuItem menuWindowSourceFile = new JMenuItem(file.getFile().getName());
                    WindowSourceAction action = new WindowSourceAction(this.controller, file);
                    menuWindowSourceFile.addActionListener(action);
                    menuWindowSource.add(menuWindowSourceFile);
                }
            }

            // 構造メニュー
            {
                int pos = getMenuItemPos(menuWindowExplore, menuWindowExploreLanguage);
                if (pos >= 0) {
                    // 開いている構造タブの一覧を取得する
                    LanguageTreeModel[] models = this.controller.getMainframe().getPanelExplorerView().getLanguageModels();
                    // 構造タブのプロシージャ名の追加
                    if (models != null && models.length > 0) {
                        menuWindowExplore.remove(menuWindowExploreLanguage);
                        menuWindowExploreLanguage = new JMenu(Message.getString("mainmenu.window.explore.structure"));//構造
                        menuWindowExplore.insert(menuWindowExploreLanguage, pos);
                        for (LanguageTreeModel model : models) {
                            // ウィンドウ:エクスプローラビュー:構造
                            DefaultMutableTreeNode root = model.getRootNode();
                            String menuname = EXPLORE_PANEL.LANGUAGE.getTabName();
                            if (root != null && root.getChildCount() > 0) {
                                menuname = root.toString();
                                DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
                                Object obj = child.getUserObject();
                                if (obj != null && obj instanceof Procedure) {
                                    menuname = ((Procedure)obj).get_name();
                                }
                            }

                            JMenuItem menuLanguage = new JMenuItem(menuname);
                            WindowViewAction action = new WindowViewAction(this.controller, model);
                            menuLanguage.addActionListener(action);
                            menuWindowExploreLanguage.add(menuLanguage);
                        }
                    }
                    else {
                        menuWindowExplore.remove(menuWindowExploreLanguage);
                        menuWindowExploreLanguage = new JMenuItem(Message.getString("mainmenu.window.explore.structure"));//構造
                        menuWindowExplore.insert(menuWindowExploreLanguage, pos);
                        menuWindowExploreLanguage.addActionListener(new WindowViewAction(this.controller, EXPLORE_PANEL.LANGUAGE));
                    }
                }
            }
            // トレースメニュー
            {
                int pos = getMenuItemPos(menuWindowAnalysis, menuWindowAnalysisTrace);
                if (pos >= 0) {
                    // 開いているトレースタブの一覧を取得する
                    TraceResultModel[] models = this.controller.getMainframe().getPanelAnalysisView().getTraceResultModels();
                    // トレースタブのプロシージャ名の追加
                    if (models != null && models.length > 0) {
                        menuWindowAnalysis.remove(menuWindowAnalysisTrace);
                        menuWindowAnalysisTrace = new JMenu(Message.getString("mainmenu.window.analysis.trace"));//トレース
                        menuWindowAnalysis.insert(menuWindowAnalysisTrace, pos);
                        for (TraceResultModel model : models) {
                            IBlock block = model.getRootBlock();
                            String word = model.getTraceWord();
                            String menuname = ANALYSIS_PANEL.TRACE.getTabName();
                            if (word != null && block != null) {
                                if (block instanceof Procedure) {
                                    menuname = word + " : " + ((Procedure)block).get_name();
                                }
                                else {
                                    menuname = word + " : " + block.toString();
                                }
                            }
                            JMenuItem menuTrace = new JMenuItem(menuname);
                            WindowViewAction action = new WindowViewAction(this.controller, model);
                            menuTrace.addActionListener(action);
                            menuWindowAnalysisTrace.add(menuTrace);
                        }
                    }
                    else {
                        menuWindowAnalysis.remove(menuWindowAnalysisTrace);
                        menuWindowAnalysisTrace = new JMenuItem(ANALYSIS_PANEL.TRACE.getTabName());
                        menuWindowAnalysis.insert(menuWindowAnalysisTrace, pos);
                        menuWindowAnalysisTrace.addActionListener(new WindowViewAction(this.controller, ANALYSIS_PANEL.TRACE));
                    }
                }
            }

        }
        // 表示:構造フィルタ
        if (event.getSource() == this.menuView) {
            // 構造ツリーフィルタ取得
            List<FILTER_TYPE> filters = this.controller.getListLanguageFilter();
            if (filters != null && filters.size() > 0) {
                int count = this.menuViewFilter.getMenuComponentCount();
                for (int i=0; i<count; i++) {
                    Object obj = this.menuViewFilter.getMenuComponent(i);
                    if (!(obj instanceof JCheckBoxMenuItem)) continue;
                    JCheckBoxMenuItem submenu = (JCheckBoxMenuItem)obj;
                    ActionListener[] actions = submenu.getActionListeners();
                    if (actions == null) continue;
                    for (ActionListener action : actions) {
                        if (action instanceof ViewLangugeFilterAction) {
                            FILTER_TYPE filter = ((ViewLangugeFilterAction)action).getFilter();
                            boolean checked = filters.contains(filter);
                            submenu.setSelected(checked);
                        }
                    }
                }
            }
        }

        // アクションが実行可能かチェックする
        JMenu menu = (JMenu) event.getSource();
        validateAction(menu);
    }

    /**
     * アクションが実行可能かチェックする
     * @param menu		チェックメニュー
     */
    private void validateAction(JMenu menu) {

        int count = menu.getMenuComponentCount();
        int disenabled = 0;
        for (int i=0; i<count; i++) {
            Object obj = menu.getMenuComponent(i);
            if (obj instanceof JMenu) {
                validateAction((JMenu)obj);
            }
            if (!(obj instanceof JMenuItem)) continue;
            JMenuItem submenu = (JMenuItem)obj;
            ActionListener[] actions = submenu.getActionListeners();
            if (actions == null) continue;
            for (ActionListener action : actions) {
                if (action instanceof ActionBase) {
                    boolean enabled = ((ActionBase)action).validateAction();
                    submenu.setEnabled(enabled);
                    if (!enabled) {
                        disenabled++;
                    }
                }
            }
        }
        // すべてディスイネーブルであれば、親メニューもディスイネーブルにする
        menu.setEnabled((count != disenabled));

    }


    /**
     * メニュー項目の位置を取得する.<br/>
     * メニュー項目が存在しない場合は、-1を返す.
     * @param menu		親メニュー
     * @param item		メニュー項目
     * @return			メニュー項目位置
     */
    private int getMenuItemPos(JMenu menu, JMenuItem item) {
        int count = menu.getMenuComponentCount();
        if (count <= 0) return -1;
        for (int i=0; i<count; i++) {
            if (menu.getMenuComponent(i) == item) {
                return i;
            }
        }
        return -1;
    }


    /**
     * メニュー選択解除アクション
     * @param event		イベント情報
     */
    @Override
    public void menuDeselected(MenuEvent event) { }


    /**
     * メニューキャンセルアクション
     * @param event		イベント情報
     */
    @Override
    public void menuCanceled(MenuEvent event) { }


    /**
     * エラー箇所を開くアクションを取得する
     * @return		エラー箇所を開くアクション
     */
    public EventListener getActionErrorOpenFile() {
        return this.actionErrorOpenFile;
    }


    /**
     * 分析結果該当個所を開くアクションを取得する
     * @return		分析結果該当個所を開くアクション
     */
    public EventListener getActionOpenAnalysisLine() {
        return this.actionOpenAnalysisLine;
    }

    /**
     * 付加情報編集アクションを取得する
     * @return		付加情報編集アクション
     */
    public EditInformationEditAction getActionEditInformation() {
        return actionEditInformation;
    }

    /**
     * 演算カウントアクションを取得する
     * @return		演算カウントアクション
     */
    public AnalysisOperandAction getActionAnalysisOperand() {
        return this.actionAnalysisOperand;
    }

    /**
     * トレースアクションを取得する
     * @param dir		トレース方向
     * @return			トレースアクション
     */
    public AnalysisTraceAction getActionAnalysisTrace(TRACE_DIR dir) {
        return new AnalysisTraceAction(this.controller, dir);
    }

    /**
     * 新規構造ツリーアクションを取得する
     * @return		新規構造ツリーアクション
     */
    public ViewOpenLanguageTreeAction getActionOpenLanguageTree() {
        return actionOpenLanguageTree;
    }

    /**
     * 検索結果アクションを取得する
     * @param dir		検索結果方向
     * @return			検索結果アクション
     */
    public SearchResultAction getActionSearchResult(TRACE_DIR dir) {
        return new SearchResultAction(this.controller, dir);
    }

    /**
     * メニュー表示（コンボボックス）を初期化する
     */
    public void clearSelectedMenu() {
        // プロファイラ：コスト表示
    	if (menuProfilerBarVisibled != null) {
	        boolean visibleBar = ProfilerProperties.INITIALIZE_VISIBLE_BARGRAPH;
	        menuProfilerBarVisibled.setSelected(visibleBar);
    	}
        // プロファイラ：コストルーラ表示
    	if (menuProfilerRulerVisibled != null) {
	        boolean visibleRuler = ProfilerProperties.INITIALIZE_VISIBLE_RULER;
	        menuProfilerRulerVisibled.setSelected(visibleRuler);
    	}
    }
}


