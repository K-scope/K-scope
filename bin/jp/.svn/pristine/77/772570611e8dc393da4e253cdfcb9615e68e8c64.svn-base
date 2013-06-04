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
package jp.riken.kscope.service;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisVariableAction;
import jp.riken.kscope.action.WindowProgressAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.ProfilerLegendDialog;
import jp.riken.kscope.gui.AnalysisView;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.gui.ExploreView;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.gui.LanguageTreePanel;
import jp.riken.kscope.gui.MainFrame;
import jp.riken.kscope.gui.ProfilerTablePanel;
import jp.riken.kscope.gui.SourceView;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.model.ReplacementResultTableModel;
import jp.riken.kscope.model.RequiredByteFlopModel;
import jp.riken.kscope.model.ScopeModel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.profiler.ProfilerInfo;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.ApplicationProperties;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.MemorybandProperties;
import jp.riken.kscope.properties.OperandProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;

/**
 * アプリケーションコントローラクラス
 * @author riken
 *
 */
public class AppController implements PropertyChangeListener {

    /** メインフレーム */
    private MainFrame mainframe;

    /** プロジェクト情報 */
    private ProjectModel projectModel;

    /** プロジェクト設定 */
    private ProjectProperties propertiesProject;

    /** ソースビュー設定 */
    private SourceProperties propertiesSource;

    /** ハイライト設定 */
    private KeywordProperties propertiesKeyword;

    /** 外部ツール設定プロパティ */
    private ProgramProperties propertiesProgram;

    /** 演算カウントプロパティ */
    private OperandProperties propertiesOperand;

    /** プロファイラプロパティ */
    private ProfilerProperties propertiesProfiler;

    /** 要求Byte/FLOP設定プロパティ */
    private MemorybandProperties propertiesMemory;

    /** アプリケーションプロパティ */
    private ApplicationProperties propertiesApplication;

    /** 変数アクセス先メモリ設定 */
    private VariableMemoryProperties propertiesVariable;

    /** フォートランデータベース */
    private Fortran fortranLanguage;

    /** スレッドタスク */
    private FutureService<Integer> threadFuture;

    /** 構造ツリーフィルタ */
    private List<FILTER_TYPE> listLanguageFilter;

    /** 最終アクセスフォルダ */
    private String lastAccessFolder;

    /** プロファイラ情報クラス */
    private ProfilerInfo profilerInfo;

    /** 最終変数特性一覧情報 ブロック */
    private List<IBlock> lastBlocks;

    /** 最終変数特性一覧情報 変数リスト */
    private List<VariableDefinition> lastVars;

    /** 変数特性一覧アクション（更新用） */
    private AnalysisVariableAction actionVariable = null;

    /**
     * コンストラクタ
     */
    public AppController() {
    }

    /**
     * 初期化を行う。
     * @throws Exception     初期起動エラー
     */
    public void initialize() throws Exception {
        // 初期化を行う。
        initialize(this.mainframe);
    }

    /**
     * 初期化を行う。
     * @param  frame		メインフレーム
     * @throws Exception     初期起動エラー
     */
    public void initialize(MainFrame frame) throws Exception {
        this.mainframe = frame;

        // プロジェクト情報
        projectModel = new ProjectModel();

        /** プロパティ設定の生成：作成済みの場合はクリアしない. */
    	createProperties(false);

        /** プロパティ設定変更リスナの登録 */
        // エクスプローラビュー：ソースビュー設定プロパティ
        {
	        ExploreView view = mainframe.getPanelExplorerView();
	        propertiesSource.addPropertyChangeListener(view);
        }
        // ソースビュー：ソースビュー設定プロパティ
        {
	        SourceView view = mainframe.getPanelSourceView();
	        propertiesSource.addPropertyChangeListener(view);
	        propertiesKeyword.addPropertyChangeListener(view);
	        propertiesProfiler.addPropertyChangeListener(view);
	        propertiesMemory.addPropertyChangeListener(view);
	        propertiesVariable.addPropertyChangeListener(view);
        }

        // 付加情報モデル：外部ツール設定プロパティ
        InformationModel modelInformation = this.getInformationModel();
        if (modelInformation != null) {
            propertiesProgram.addPropertyChangeListener(modelInformation);
            modelInformation.setPropertiesExtension(propertiesProgram);
        }

        // トレースパネル：ソースビュー設定プロパティ
        AnalysisView analysis = mainframe.getPanelAnalysisView();
        propertiesSource.addPropertyChangeListener(analysis);
        propertiesSource.firePropertyChange();
        // プロファイラパネル:プロファイラプロパティ
        propertiesProfiler.addPropertyChangeListener(analysis);
        propertiesProfiler.firePropertyChange();

        // 構造ツリーフィルタデフォルト設定
        FILTER_TYPE[] filters = KscopeProperties.LANGUGE_DEFAULTFILTERS;
        this.setListLanguageFilter(filters);

        // フォートランデータベースの作成
        fortranLanguage = new Fortran();
        // プロファイラ情報クラス
        profilerInfo = new ProfilerInfo();

        // プロファイラ凡例ダイアログを閉じる
    	ProfilerLegendDialog dialog = this.getMainframe().getDialogProfilerLegend();
    	if (dialog != null) {
    		dialog.setVisible(false);
    	}

        // 最終変数特性一覧情報
        lastBlocks = null;
        lastVars = null;
    }


    /**
     * メインフレームを取得する
     * @return mainframe		メインフレーム
     */
    public MainFrame getMainframe() {
        return mainframe;
    }

    /**
     * プロジェクト設定を取得する
     * @return	プロジェクト設定
     */
    public ProjectProperties getPropertiesProject() {
    	return propertiesProject;
    }

    /**
     * ソースビュー設定を取得する。
     * @return		ソースビュー設定
     */
    public SourceProperties getPropertiesSource() {
        return propertiesSource;
    }

    /**
     * ハイライト設定を取得する
     * @return		ハイライト設定
     */
    public KeywordProperties getPropertiesKeyword() {
        return propertiesKeyword;
    }


    /**
     * 外部ツール設定プロパティを取得する
     * @return		外部ツール設定プロパティ
     */
    public ProgramProperties getPropertiesExtension() {
        return propertiesProgram;
    }

    /**
     * 演算カウントプロパティを取得する
     * @return		演算カウントプロパティ
     */
    public OperandProperties getPropertiesOperand() {
        return this.propertiesOperand;
    }

    /**
     * プロファイラカウントプロパティを取得する
     * @return		プロファイラプロパティ
     */
    public ProfilerProperties getPropertiesProfiler() {
        return this.propertiesProfiler;
    }

    /**
     * 要求Byte/FLOP設定プロパティを取得する
     * @return		要求Byte/FLOP設定プロパティ
     */
    public MemorybandProperties getPropertiesMemory() {
        return this.propertiesMemory;
    }

    /**
     * アプリケーションプロパティを取得する
     * @return		アプリケーションプロパティ
     */
    public ApplicationProperties getPropertiesApplication() {
    	return this.propertiesApplication;
    }

    /**
     * 変数アクセス先メモリを取得する
     * @return		変数アクセス先メモリ
     */
    public VariableMemoryProperties getPropertiesVariable() {
        return propertiesVariable;
    }

    /**
     * プロジェクト情報を取得する
     * @return projectModel		プロジェクト情報
     */
    public ProjectModel getProjectModel() {
        return projectModel;
    }

    /**
     * プロパティ設定を更新する。
     */
    public void updateProperties() {
    	// プロジェクト設定
    	propertiesProject.firePropertyChange();
        // ソースビュー設定
        propertiesSource.firePropertyChange();
        // ハイライト設定
        propertiesKeyword.firePropertyChange();
        // 外部ツール設定プロパティ
        propertiesProgram.firePropertyChange();
        // プロファイラ設定プロパティ
        propertiesProfiler.firePropertyChange();
        // 要求Byte/FLOP設定プロパティ
        propertiesMemory.firePropertyChange();
        // 変数アクセス先メモリ設定
        propertiesVariable.firePropertyChange();

        return;
    }

    /**
     * 付加情報モデルを取得する
     * @return		付加情報モデル
     */
    public InformationModel getInformationModel() {
        if (this.mainframe.getPanelAnalysisView() == null) return null;
        if (this.mainframe.getPanelAnalysisView().getPanelInformation() == null) return null;
        return this.mainframe.getPanelAnalysisView().getPanelInformation().getModel();
    }


    /**
     * プロパティテーブルモデルを取得する
     * @return		プロパティテーブルモデル
     */
    public PropertiesTableModel getPropertiesTableModel() {
        if (this.mainframe.getPanelAnalysisView() == null) return null;
        if (this.mainframe.getPanelAnalysisView().getPanelPropertiesTable() == null) return null;
        return this.mainframe.getPanelAnalysisView().getPanelPropertiesTable().getModel();
    }


    /**
     * エラー情報モデルを取得する
     * @return		エラー情報モデル
     */
    public ErrorInfoModel getErrorInfoModel() {
        if (this.mainframe.getPanelAnalysisView() == null) return null;
        if (this.mainframe.getPanelAnalysisView().getPanelError() == null) return null;
        // エラー情報モデル
        ErrorInfoModel model = this.mainframe.getPanelAnalysisView().getPanelError().getModel();
        if (model == null) return null;

        // プロジェクトフォルダを設定する
        if (this.getProjectModel() != null) {
            model.setProjectFolder(this.getProjectModel().getProjectFolder());
        }
        return model;
    }

    /**
     * 変数特性情報一覧モデルを取得する
     * @return		変数特性情報一覧モデル
     */
    public VariableTableModel getVariableTableModel() {
        // 変数特性情報一覧モデル
        VariableTableModel model = this.mainframe.getPanelAnalysisView().getPanelVariable().getModel();

        return model;
    }

    /**
     * 参照一覧モデルを取得する
     * @return		参照一覧モデル
     */
    public ReferenceModel getReferenceModel() {
        // 参照一覧モデル
        ReferenceModel model = this.mainframe.getPanelAnalysisView().getPanelReference().getModel();
        return model;
    }

    /**
     * 検索結果モデルを取得する
     * @return		検索結果モデル
     */
    public SearchResultModel getSearchResultModel() {
        // 検索一覧モデル
        SearchResultModel model = this.mainframe.getPanelAnalysisView().getPanelSearchResult().getModel();
        return model;
    }


    /**
     * 変数有効域モデルを取得する
     * @return		変数有効域モデル
     */
    public ScopeModel getScopeModel() {
        // 変数有効域モデル
        ScopeModel model = this.mainframe.getPanelAnalysisView().getPanelScope().getModel();
        return model;
    }


    /**
     * フォートランデータベースを取得する
     * @return		フォートランデータベース
     */
    public Fortran getFortranLanguage() {
        return fortranLanguage;
    }

    /**
     * フォートランデータベースを設定する
     * @param  value		フォートランデータベース
     */
    public void setFortranLanguage(Fortran value) {
        this.fortranLanguage = value;
    }

    /**
     * ソースファイルビューにソースファイルを表示する
     * @param file		ソースファイル
     * @throws Exception  		ファイルオープンエラー
     */
    public void openSourceFile(SourceFile file) throws Exception {
        openSourceFile(new CodeLine(file, file.getPath()));
    }

    /**
     * ソースファイルビューにソースファイルを表示する
     * @param line		ソースファイル
     * @throws Exception  		ファイルオープンエラー
     */
    public void openSourceFile(CodeLine line) throws Exception {

        // ソースファイルを開く
        this.getMainframe().getPanelSourceView().viewSource(line);

        // キーワード設定等のプロパティ設定を適用する。
        this.updateProperties();

        // プロファイラバーグラフをソースビューに設定する
        setProfilerBargraph();
    }

    /**
     * ソースファイルビュー指定行を選択表示する
     * @param lines		選択表示行情報
     */
    public void setSelectedBlock(CodeLine[] lines) {
        // ソースファイルを開く
        this.getMainframe().getPanelSourceView().setSelectedBlock(lines);
    }

    /**
     * 構造ツリーモデルを取得する
     * @return		構造ツリーモデル
     */
    public LanguageTreeModel getLanguageTreeModel() {
        // 構造パネル
        LanguageTreePanel panel = this.mainframe.getPanelExplorerView().getPanelLanguageTree();
        if (panel == null) {
            // 構造パネルが存在しないので作成する
            this.mainframe.getPanelExplorerView().createLanguageTreePanel();
            // フィルタを適用する
            applyLanguageTreeFilter();
        }
        // 構造ツリーモデル
        LanguageTreeModel model = this.mainframe.getPanelExplorerView().getPanelLanguageTree().getModel();

        return model;
    }

    /**
     * モジュールツリーモデルを取得する
     * @return		モジュールツリーモデル
     */
    public ModuleTreeModel getModuleTreeModel() {
        // モジュールツリーモデル
        ModuleTreeModel model = this.mainframe.getPanelExplorerView().getPanelModuleTree().getModel();

        return model;
    }

    /**
     * ソースツリーモデルを取得する
     * @return		ソースツリーモデル
     */
    public FileTreeModel getSourceTreeModel() {
        // エラー情報モデル
        FileTreeModel model = this.mainframe.getPanelExplorerView().getPanelSourceTree().getModel();

        // プロジェクトフォルダを設定する
        if (this.getProjectModel() != null) {
            model.setProjectFolder(this.getProjectModel().getProjectFolder());
        }
        return model;
    }


    /**
     * XMLツリーモデルを取得する
     * @return		Xmlツリーモデル
     */
    public FileTreeModel getXmlTreeModel() {
        // エラー情報モデル
        FileTreeModel model = this.mainframe.getPanelExplorerView().getPanelXmlTree().getModel();

        // プロジェクトフォルダを設定する
        if (this.getProjectModel() != null) {
            model.setProjectFolder(this.getProjectModel().getProjectFolder());
        }
        return model;
    }

    /**
     * フォートランデータベースをクリアする
     */
    public void clearFortranLanguage() {
        this.fortranLanguage = new Fortran();
        // 変数アクセス先メモリプロパティクリア
        this.propertiesVariable.clearVariableMemory();
    }

    /**
     * スレッドタスクの終了通知を受け取り、メッセージボックスを表示する.<br/>
     * <pre>
     * 終了コード : SUCCESS_RESULT = 正常終了
     *            CANCEL_RESULT = キャンセルによる中断
     *            CANCEL_RESULT = 異常終了
     *            null = メッセージボックスなし終了
     * </pre>
     * @param result		終了コード
     */
    public void finishThreadFuture(Integer result) {

        // プログレスダイアログを閉じる
        WindowProgressAction progress = new WindowProgressAction(this);
        progress.closeProgressDialog();
        // プログレスバーのクリア
        Application.status.setProgressStart(false);

		// コンソール
		ConsolePanel console = this.getMainframe().getPanelAnalysisView().getPanelConsole();
		if (console != null) {
			console.flush();
		}

        if (result == null) {
            return;
        }

        if (result == Constant.ERROR_RESULT) {
        	String errmsg = null;
        	if (this.threadFuture != null) {
        		errmsg = this.threadFuture.getMessage();
        	}
        	//エラーにより終了しました。
        	String msg = Message.getString("appcontroller.thread.message.error");
        	if (errmsg != null) {
        		msg += "\n[" + errmsg + "]";
        	}
            JOptionPane.showMessageDialog(this.mainframe,
            		msg,
                    Message.getString("appcontroller.thread.title"), // 処理メッセージ
                    JOptionPane.INFORMATION_MESSAGE);
        }
        else if (result == Constant.CANCEL_RESULT) {
            JOptionPane.showMessageDialog(this.mainframe,
            		Message.getString("appcontroller.thread.message.cancel"), //キャンセルにより中断しました。
                    Message.getString("appcontroller.thread.title"), // 処理メッセージ
                    JOptionPane.INFORMATION_MESSAGE);
        }
        else if (result != null) {
            JOptionPane.showMessageDialog(this.mainframe,
            		Message.getString("appcontroller.thread.message.success"), //実行終了しました。
                    Message.getString("appcontroller.thread.title"), // 処理メッセージ
                    JOptionPane.INFORMATION_MESSAGE);
        }

    }


    /**
     * スレッドタスクを取得する
     * @return threadFuture		スレッドタスク
     */
    public FutureService<Integer> getThreadFuture() {
        return threadFuture;
    }


    /**
     * スレッドタスクを設定する
     * @param threadFuture 	スレッドタスク
     */
    public void setThreadFuture(FutureService<Integer> threadFuture) {
        this.threadFuture = threadFuture;
    }


    /**
     * プロパティ変更通知.<br/>
     * スレッドタスクの終了通知をPropertyChangeSupportによって通知、受信する
     * @param  event		イベント情報
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {

        if (Constant.PROPERTYNAME_THREADDONE.equals(event.getPropertyName())) {
            // スレッド終了イベント
            Integer result = (Integer)event.getNewValue();

            // スレッドタスクの終了通知を受け取り、メッセージボックスを表示する
            finishThreadFuture(result);
        }
    }

    /**
     * スレッドタスクの終了状態をチェックする
     * @return		true=スレッドタスク終了
     */
    public boolean isThreadTaskDone() {
        if (this.threadFuture == null) { return true; }
        return this.threadFuture.isDone();
    }

    /**
     * 分析ビューの指定タブをアクティブにする
     * @param panel			選択タブ
     */
    public void setSelectedAnalysisPanel(ANALYSIS_PANEL panel) {
        this.getMainframe().getPanelAnalysisView().setSelectedPanel(panel);
    }


    /**
     * 演算カウントテーブルモデルを取得する
     * @return		演算カウントテーブルモデル
     */
    public OperandTableModel getOperandTableModel() {
        // 演算カウントテーブルモデル
        OperandTableModel model = this.mainframe.getPanelAnalysisView().getPanelOperand().getModel();

        return model;
    }

    /**
     * 要求Byte/FLOP算出結果モデルを取得する
     * @return		要求Byte/FLOP算出結果モデル
     */
    public RequiredByteFlopModel getRequiredByteFlopModel() {
        // 要求Byte/FLOP算出結果モデル
    	RequiredByteFlopModel model = this.mainframe.getPanelAnalysisView().getPanelRequiredByteFlop().getModel();

        return model;
    }

    /**
     * 構造情報差替結果テーブルモデルを取得する
     * @return 構造情報差替結果テーブルモデル
     */
    public ReplacementResultTableModel getReplaceTableModel() {
        // 差替結果テーブルモデル
        ReplacementResultTableModel model = this.mainframe.getPanelAnalysisView().getPanelReplace().getModel();

        return model;
    }

    /**
     * プロジェクトをクリアする
     * @throws Exception     プロジェクトクリアエラー
     */
    public void clearProject() throws Exception {
        // プロパティ設定の生成
    	boolean clear = false;
    	if (this.projectModel != null && this.projectModel.isVaildProject()) {
    		clear = true;
    	}
    	createProperties(clear);

        // 初期化を行う
        initialize();
    }

    /**
     * プロパティをクリアする
     */
    private void clearProperties() {
        /** プロパティ設定をクリアする:initializeで生成*/
        this.propertiesProject = null;
        this.propertiesSource = null;
        this.propertiesKeyword = null;
        this.propertiesProgram = null;
        this.propertiesOperand = null;
        this.propertiesApplication = null;
        this.propertiesProfiler = null;
        this.propertiesMemory = null;
        this.propertiesVariable = null;
    }

    /**
     * プロパティ設定を生成する.
     * @param   clear    true=プロパティ設定をクリアする
     * @throws Exception		プロパティ読込エラー
     */
    private void createProperties(boolean clear) throws Exception {
    	// プロパティ設定をクリアする
    	if (clear) {
    		clearProperties();
    	}
        /** プロパティ設定 */
        if (this.propertiesProject == null) {
        	this.propertiesProject = new ProjectProperties();
        }
        if (this.propertiesSource == null) {
        	this.propertiesSource = new SourceProperties();
        }
        if (this.propertiesKeyword == null) {
        	this.propertiesKeyword = new KeywordProperties();
        }
        if (this.propertiesProgram == null) {
        	this.propertiesProgram = new ProgramProperties();
        }
        if (this.propertiesOperand == null) {
        	this.propertiesOperand = new OperandProperties();
        }
        if (this.propertiesApplication == null) {
        	this.propertiesApplication = new ApplicationProperties();
        }
        // メニュー表示選択をコピーする
        this.mainframe.getMenuMain().clearSelectedMenu();
        if (this.propertiesProfiler == null) {
        	this.propertiesProfiler = new ProfilerProperties(this.propertiesProfiler);
        }
        else {
        	this.propertiesProfiler.setVisibleProperties(this.propertiesProfiler);
        }
        // バーグラフの色設定
        PROFILERINFO_TYPE.setProfilerProperties(this.propertiesProfiler);
        // 要求Byte/FLOP設定プロパティ:デフォルト設定を保持する為に生成済みの場合は、newしない。
        if (this.propertiesMemory == null) {
        	this.propertiesMemory = new MemorybandProperties();
        	// デフォルト設定を設定する.
        	MemorybandProperties defaultProperties = new MemorybandProperties();
        	this.propertiesMemory.setDefaultProperties(defaultProperties);
        }
        // 要求Byte/FLOP変数設定データ
        this.propertiesVariable = new VariableMemoryProperties(this.propertiesMemory);

    }

    /**
     * トレースキーワードを設定する
     */
    public void setTraceKeywords() {
        // キーワードリストを取得する
        Keyword[] words = this.getMainframe().getPanelAnalysisView().getTraceKeywords();
        // キーワードを設定する
        this.getMainframe().getPanelSourceView().setSearchWords(words);
    }

    /**
     * 検索キーワードを設定する
     */
    public void setSearchKeywords() {
        // キーワードリストを取得する
        Keyword[] words = this.getMainframe().getPanelAnalysisView().getSearchKeywords();
        // キーワードを設定する
        this.getMainframe().getPanelSourceView().setSearchWords(words);
    }

    /**
     * 構造ツリーフィルタリストを取得する
     * @return		構造ツリーフィルタリスト
     */
    public List<FILTER_TYPE> getListLanguageFilter() {
        return listLanguageFilter;
    }

    /**
     * 構造ツリーフィルタリストを設定する
     * @param list			構造ツリーフィルタリスト
     */
    public void setListLanguageFilter(FILTER_TYPE[] list) {
        this.listLanguageFilter = new ArrayList<FILTER_TYPE>();
        this.listLanguageFilter.addAll(java.util.Arrays.asList(list));

        // 構造ツリーにフィルタを設定する
        applyLanguageTreeFilter();
    }

    /**
     * 構造ツリーフィルタを追加する
     * @param filter			構造ツリーフィルタ
     */
    public void addListLanguageFilter(FILTER_TYPE filter) {
        if (filter == null) return;
        if (this.listLanguageFilter == null) {
            this.listLanguageFilter = new ArrayList<FILTER_TYPE>();
        }
        if (this.listLanguageFilter.contains(filter)) return;
        this.listLanguageFilter.add(filter);

        // 構造ツリーにフィルタを設定する
        applyLanguageTreeFilter();
    }

    /**
     * 構造ツリーフィルタを削除する
     * @param filter			構造ツリーフィルタ
     */
    public void removeListLanguageFilter(FILTER_TYPE filter) {
        if (filter == null) return;
        if (this.listLanguageFilter == null) return;
        if (this.listLanguageFilter.size() <= 0) return;

        // フィルタ削除
        this.listLanguageFilter.remove(filter);

        // 構造ツリーにフィルタを設定する
        applyLanguageTreeFilter();
    }

    /**
     * 構造ツリーにフィルタを適用する
     */
    public void applyLanguageTreeFilter() {
        FILTER_TYPE[] filters = null;
        if (this.listLanguageFilter != null) {
            filters = this.listLanguageFilter.toArray(new FILTER_TYPE[0]);
        }
        this.mainframe.getPanelExplorerView().setLanguageTreeFilter(filters);
    }

    /**
     * 最終アクセスフォルダを取得する
     * @return		最終アクセスフォルダ
     */
    public String getLastAccessFolder() {
        return lastAccessFolder;
    }

    /**
     * 最終アクセスフォルダを設定する
     * @param folder		最終アクセスフォルダ
     */
    public void setLastAccessFolder(File folder) {
        if (folder == null) {
            this.lastAccessFolder = null;
            return;
        }
        // フォルダであるかチェックする
        if (folder.isDirectory()) {
            this.lastAccessFolder = folder.getAbsolutePath();
        }
        else {
            this.lastAccessFolder = folder.getParent();
        }
    }

    /**
     * 最終アクセスフォルダを設定する
     * @param folder		最終アクセスフォルダ
     */
    public void setLastAccessFolder(String folder) {
        this.lastAccessFolder = folder;
    }

    /**
     * プロファイラ情報クラス
     * @return プロファイラ情報クラス
     */
    public ProfilerInfo getProfilerInfo() {
        return profilerInfo;
    }

    /**
     * プロファイラ情報クラス
     * @param info プロファイラ情報クラス
     */
    public void setProfilerInfo(ProfilerInfo info) {
        this.profilerInfo = info;
    }


    /**
     * プロファイラバーグラフをソースビューに設定する
     */
    public void setProfilerBargraph() {
        PROFILERINFO_TYPE type = null;
        // 表示タブから表示プロファイラ情報タイプを取得する
        IAnalisysComponent panel = this.getMainframe().getPanelAnalysisView().getSelectedPanel();
        if (panel == null) return;
        if (panel.getEnumPanel() == ANALYSIS_PANEL.COST_PROCEDURE) {
            type = PROFILERINFO_TYPE.COST_PROCEDURE;
        }
        else if (panel.getEnumPanel() == ANALYSIS_PANEL.COST_LOOP) {
            type = PROFILERINFO_TYPE.COST_LOOP;
        }
        else if (panel.getEnumPanel() == ANALYSIS_PANEL.COST_LINE) {
            type = PROFILERINFO_TYPE.COST_LINE;
        }
        if (type == null) return;

        this.setProfilerBargraph(type);

        return;
    }

    /**
     * プロファイラバーグラフをソースビューに設定する
     * @param type    プロファイラ情報タイプ
     */
    public void setProfilerBargraph(PROFILERINFO_TYPE type) {
        if (type == null) return;
        ProfilerTablePanel panel = this.mainframe.getPanelAnalysisView().getSelectedProfilerCostPanel();
        if (panel == null) return;
        if (panel.getModel() == null) return;
        ISourceBargraph[] bargraph = panel.getModel().getSelectedBargraph();
        if (bargraph == null) return;
        this.mainframe.getPanelSourceView().setProfilerBargraph(bargraph);

        return;
    }


    /**
     * プロファイラ情報をクリアする
     */
    public void clearProfilerInfo() {
        if (this.profilerInfo != null) {
            this.profilerInfo.clearProfilerData();
        }
        this.mainframe.getPanelAnalysisView().clearProfilerInfo();
        this.mainframe.getPanelSourceView().clearBargraphData();
    }

    /**
     * 最終変数特性一覧情報を取得する
     * @return 選択ツリーのブロック
     */
    public List<IBlock> getLastVariableBlocks() {
    	return lastBlocks;
    }

    /**
     * 最終変数特性一覧情報を取得する
     * @return 変数リスト
     */
    public List<VariableDefinition> getLastVariableVars() {
    	return lastVars;
    }

    /**
     * 最終変数特性一覧をセットする
     * @param blocks      最終変数特性一覧:ブロック
     * @param vars        最終変数特性一覧:変数リスト
     */
    public void setLastVariable(List<IBlock> blocks, List<VariableDefinition> vars) {
    	lastBlocks = blocks;
    	lastVars = vars;
    }

    /**
     * 変数特性一覧アクションを設定する
     * @param actionVar		変数特性一覧アクション
     */
    public void setActionVariable(AnalysisVariableAction actionVar) {
    	this.actionVariable = actionVar;
    }

    /**
     * 付加情報の編集により付加情報表示の分析ビューを更新する.
     * 変数特性情報一覧を更新する.
     */
    public void refreshInformation() {
    	if (this.actionVariable != null) {
    		this.actionVariable.refresh();
    	}
    	// 要求Byte/FLOP算出結果モデル
    	RequiredByteFlopModel requiredModel = this.getRequiredByteFlopModel();
    	requiredModel.notifyModel();

    }

    /**
     * エラー情報を設定する
     * @param error    エラー情報
     */
	public void setErrorInfo(ErrorInfo error) {
        // エラー情報モデル
        ErrorInfoModel errorModel = this.getErrorInfoModel();
        errorModel.addErrorInfo(error);
	}
}


