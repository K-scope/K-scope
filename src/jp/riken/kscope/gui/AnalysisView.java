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
package jp.riken.kscope.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.event.ChangeListener;

import jp.riken.kscope.action.AnalysisTabChangeAction;
import jp.riken.kscope.action.AnalysisTraceAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.ProfilerPopupMenu;
import jp.riken.kscope.model.ProfilerTableBaseModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.SourceProperties;

/**
 * 解析ビュークラス.<br/>
 * 参照一覧、トレース、検索結果をタブを配置する。
 * @author RIKEN
 *
 */
public class AnalysisView  extends ClosableTabbedPane implements PropertyChangeListener {

    /** シリアル番号  */
    private static final long serialVersionUID = 1L;

    /** 変数特性一覧パネル */
    private VariableTablePanel panelVariable;
    /** 付加情報パネル */
    private InformationPanel panelInformation;
    /** 演算カウントパネル */
    private OperandTablePanel panelOperand;
    /** 要求Byte/FLOP算出結果パネル */
    private RequiredByteFlopPanel panelRequiredByteFlop;
    /** プロパティテーブルパネル */
    private PropertiesTablePanel panelPropertiesTable;
    /** コンソールパネル */
    private ConsolePanel panelConsole;
    /** エラー箇所パネル */
    private ErrorInfoPanel panelError;
    /** 検索結果パネル */
    private SearchResultPanel panelSearchResult;
    /** 参照一覧パネル */
    private ReferencePanel panelReference;
    /** 変数有効域パネル */
    private ScopePanel panelScope;
    /** トレースパネル */
    private TraceResultPanel panelTrace;
    /** プロファイラ:情報パネル */
    private List<ProfilerTablePanel> panelProfilerList;
    /** プロファイラ:測定区間情報テーブルパネル */
    private ProfilerMeasurePanel panelProfilerMeasure;

    // トレースパネルを新規作成する時に必要な情報。
    /** メインメニュー */
    private MainMenu mainMenu;
    /** ソース設定プロパティ */
    private SourceProperties propertiesSource;
    /** プロファイラプロパティ */
    private ProfilerProperties propertiesProfiler;
    /** トレースクローズアクション */
    private AnalysisTraceAction closeTraceAction;

    /**
     * コンストラクタ
     */
    public AnalysisView() {
        super(FRAME_VIEW.ANALYSIS_VIEW);
        initGUI();
    }

    /**
     * 初期化を行う.<br/>
     * 参照一覧、トレース、検索結果をタブを配置する。
     */
    private void initGUI() {
        try {

            // 分析タブの追加
            // 付加情報
            panelInformation = new InformationPanel(ANALYSIS_PANEL.INFORMATION);
            panelInformation.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.INFORMATION.getTabName(),  panelInformation);

            // 検索結果
            panelSearchResult = new SearchResultPanel(ANALYSIS_PANEL.SEARCHRESULT);
            panelSearchResult.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.SEARCHRESULT.getTabName(),  panelSearchResult);

            // 変数特性一覧パネル
            panelVariable = new VariableTablePanel(ANALYSIS_PANEL.VARIABLE);
            panelVariable.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.VARIABLE.getTabName(), panelVariable);

            // 演算カウント
            panelOperand = new OperandTablePanel(ANALYSIS_PANEL.OPERAND);
            panelOperand.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.OPERAND.getTabName(),  panelOperand);

            // 要求Byte/FLOP算出結果パネル
            panelRequiredByteFlop = new RequiredByteFlopPanel(ANALYSIS_PANEL.REQUIRED);
            panelRequiredByteFlop.setParentComponent(this);

            // 宣言・定義・参照
            panelReference = new ReferencePanel(ANALYSIS_PANEL.REFERENCE);
            panelReference.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.REFERENCE.getTabName(),  panelReference);

            // トレース
            panelTrace = new TraceResultPanel(ANALYSIS_PANEL.TRACE);
            panelTrace.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.TRACE.getTabName(),  panelTrace);

            // 変数有効域パネル
            panelScope = new ScopePanel(ANALYSIS_PANEL.SCOPE);
            panelScope.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.SCOPE.getTabName(),  panelScope);

            // プロファイラパネルリスト
            if (panelProfilerList == null) {
                panelProfilerList = new ArrayList<ProfilerTablePanel>();
            }
            panelProfilerList.clear();
            // プロファイラコスト情報(手続)パネル
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.COST_PROCEDURE);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラコスト情報(ループ)パネル
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.COST_LOOP);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラコスト情報(ライン)パネル
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.COST_LINE);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:コールグラフ情報
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.CALLGRAPH);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:イベントカウンタ情報:Eprof_CACHE
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_CACHE);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:イベントカウンタ情報:Eprof_INSTRUCTIONS
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:イベントカウンタ情報:Eprof_MEM_ACCESS
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:イベントカウンタ情報:Eprof_PERFORMANCE
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:イベントカウンタ情報:Eprof_STATISTICS
            {
                ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS);
                panelProfiler.setParentComponent(this);
                panelProfilerList.add(panelProfiler);
            }
            // プロファイラ:測定区間情報テーブルパネル
            panelProfilerMeasure = new ProfilerMeasurePanel(ANALYSIS_PANEL.EPROF_MEASURE);
            panelProfilerMeasure.setParentComponent(this);
            // this.addTab(ANALYSIS_PANEL.TIMERINFO.getTabName(),  panelTimerTable);

            // プロパティパネル
            panelPropertiesTable = new PropertiesTablePanel(ANALYSIS_PANEL.PROPARTIES);
            panelPropertiesTable.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.PROPARTIES.getTabName(),  panelPropertiesTable);

            // エラー箇所パネル
            panelError = new ErrorInfoPanel(ANALYSIS_PANEL.ERROR);
            panelError.setParentComponent(this);
            this.addTab(ANALYSIS_PANEL.ERROR.getTabName(),  panelError);

            // コンソールパネル
            panelConsole = new ConsolePanel(ANALYSIS_PANEL.CONSOLE);
            panelConsole.setParentComponent(this);
            //this.addTab(ANALYSIS_PANEL.CONSOLE.getTabName(),  panelConsole);

            this.setMaximumSize(new Dimension(400, 100));
            this.setMinimumSize(new Dimension(400, 50));
            this.setPreferredSize(new Dimension(400, 100));
            this.setSize(new Dimension(400, 50));

            // 初期表示を付加情報タブとする。
            this.setSelectedIndex(0);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * タブを閉じる
     */
    @Override
    public void closeTabComponent() {
        int index = this.getSelectedIndex();
        // タブを閉じる
        closeTab(index);
    }

    /**
     * タブを閉じる
     * @param index		閉じるタブインデックス
     */
    @Override
    protected void closeTab(int index) {
        if (index < 0) return;

        // 閉じるタブ
        Component tab = this.getComponentAt(index);
        tab.setVisible(false);
        if (tab instanceof IAnalisysComponent) {
            ((IAnalisysComponent)tab).closeTab();
        }
        this.remove(index);

        // トレースキーワードを再設定する
        this.closeTraceAction.setTraceKeywords();
    }

    /**
     * 付加情報パネルを取得する
     * @return		付加情報パネル
     */
    public InformationPanel getPanelInformation() {
        return panelInformation;
    }

    /**
     * 参照一覧パネルを取得する
     * @return		参照一覧パネル
     */
    public ReferencePanel getPanelReference() {
        return this.panelReference;
    }

    /**
     * 検索結果パネルを取得する
     * @return		検索結果パネル
     */
    public SearchResultPanel getPanelSearchResult() {
        return this.panelSearchResult;
    }

    /**
     * 変数特性一覧パネルを取得する
     * @return		変数特性一覧パネル
     */
    public VariableTablePanel getPanelVariable() {
        return panelVariable;
    }

    /**
     * 変数有効域パネルを取得する
     * @return		変数有効域パネル
     */
    public ScopePanel getPanelScope() {
        return this.panelScope;
    }

    /**
     * コンソールパネルを取得する
     * @return		コンソールパネル
     */
    public ConsolePanel getPanelConsole() {
        return panelConsole;
    }

    /**
     * エラー箇所パネルを取得する
     * @return		エラー箇所パネル
     */
    public ErrorInfoPanel getPanelError() {
        return panelError;
    }

    /**
     * プロパティテーブルパネルを取得する
     * @return		プロパティテーブルパネル
     */
    public PropertiesTablePanel getPanelPropertiesTable() {
        return panelPropertiesTable;
    }

    /**
     * プロファイラパネルを取得する
     * @param  type   パネル識別子
     * @return		プロファイラパネル
     */
    public ProfilerTablePanel getPanelProfiler(ANALYSIS_PANEL type) {
        for (ProfilerTablePanel panel : this.panelProfilerList) {
            if (panel.getEnumPanel() == type) {
                return panel;
            }
        }
        return null;
    }

    /**
     * 測定区間情報テーブルパネルを取得する
     * @return		測定区間情報テーブルパネル
     */
    public ProfilerMeasurePanel getPanelProfilerMeasure() {
        return this.panelProfilerMeasure;
    }

    /**
     * 現在選択されている分析情報パネルの取得を行う。
     * @return		選択分析情報パネル
     */
    public IAnalisysComponent getSelectedPanel() {
        int index = this.getSelectedIndex();
        //
        if (index < 0) {
            return null;
        }
        IAnalisysComponent tab = (IAnalisysComponent)this.getComponentAt(index);

        return tab;
    }

    /**
     * 指定分析情報パネルをアクティブにする.<br/>
     * 閉じている場合は開く
     * @param panel      選択分析情報パネル
     */
    public void setSelectedPanel(ANALYSIS_PANEL panel) {

        Component viewpanel = null;
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof IAnalisysComponent) {
                if ( ((IAnalisysComponent)comp).getEnumPanel() == panel) {
                    viewpanel = comp;
                    this.setSelectedIndex(i);
                    return;
                }
            }
        }

        // タブが表示されていない。
        if (viewpanel == null) {
            /** 変数特性一覧パネル */
            if (panelVariable.getEnumPanel() == panel) {
                viewpanel = panelVariable;
            }
            /** 付加情報パネル */
            if (panelInformation.getEnumPanel() == panel) {
                viewpanel = panelInformation;
            }
            /** 演算カウントパネル */
            if (panelOperand.getEnumPanel() == panel) {
                viewpanel = panelOperand;
            }
            /** 要求Byte/FlOP算出結果パネル */
            if (panelRequiredByteFlop.getEnumPanel() == panel) {
                viewpanel = panelRequiredByteFlop;
            }
            /** プロパティテーブルパネル */
            if (panelPropertiesTable.getEnumPanel() == panel) {
                viewpanel = panelPropertiesTable;
            }
            /** コンソールパネル */
            if (panelConsole.getEnumPanel() == panel) {
                viewpanel = panelConsole;
            }
            /** エラー箇所パネル */
            if (panelError.getEnumPanel() == panel) {
                viewpanel = panelError;
            }
            /** 検索結果パネル */
            if (panelSearchResult.getEnumPanel() == panel) {
                viewpanel = panelSearchResult;
            }
            /** 参照一覧パネル */
            if (panelReference.getEnumPanel() == panel) {
                viewpanel = panelReference;
            }
            /** 変数有効域パネル */
            if (panelScope.getEnumPanel() == panel) {
                viewpanel = panelScope;
            }
            // トレース
            if (ANALYSIS_PANEL.TRACE == panel) {
                TraceResultPanel panelTrace = createTraceResultPanel();
                viewpanel = panelTrace;
            }
            /** プロファイラ:コスト情報パネル(手続) */
            ProfilerTablePanel profilerpanel = getPanelProfiler(panel);
            if (profilerpanel != null) {
                viewpanel = profilerpanel;
            }
            /** プロファイラ:測定区間情報テーブルパネル */
            if (panelProfilerMeasure.getEnumPanel() == panel) {
                viewpanel = panelProfilerMeasure;
            }
        }
        if (viewpanel == null) return;

        // タブが表示されていないので、追加する
        this.addTab(panel.getTabName(),  viewpanel);
        this.setSelectedIndex(this.getTabCount()-1);

        // 表示プロパティを設定する
        if (viewpanel instanceof IAnalisysComponent) {
        	((IAnalisysComponent)viewpanel).setSourceProperties(this.propertiesSource);
        }
        // プロファイラプロパティを設定する
        if ( viewpanel instanceof ProfilerTablePanel) {
            ((ProfilerTablePanel)viewpanel).setProfilerProperties(this.propertiesProfiler);
        }

        return;
    }

    /**
     * パネルにアクションリスナを設定する.<br/>
     * メニューバーに作成済みのアクションリスナをパネルボタンに割り当てる。
     * @param menu		メインメニュー
     */
    public void setActionListener(MainMenu menu) {

        /** パネルリスト */
        IAnalisysComponent[] listPanel = {panelVariable, panelInformation,
    						    		panelOperand, panelRequiredByteFlop,
    						    		panelPropertiesTable,
    						    		panelConsole, panelError,
    						    		panelSearchResult, panelReference,
    						    		panelScope, panelProfilerMeasure, panelTrace};
        for (IAnalisysComponent panel : listPanel) {
        	if (panel != null) {
                panel.setActionListener(menu);
            }
        }

        // プロファイラパネル
        for (ProfilerTablePanel panel : this.panelProfilerList) {
            panel.setActionListener(menu);
        }

        // トレースクローズアクション
        this.closeTraceAction = menu.getActionAnalysisTrace(TRACE_DIR.REFRESH);

        this.mainMenu = menu;
    }

    /**
     * 分析情報をクリアする
     */
    public void clearModels() {

        /** 付加情報パネル */
        this.panelInformation.clearModel();
        /** 変数特性一覧パネル */
        this.panelVariable.clearModel();
        /** 演算カウントパネル */
        this.panelOperand.clearModel();
        /** 要求B/F算出結果パネル */
        this.panelRequiredByteFlop.clearModel();
        /** プロパティテーブルパネル */
        this.panelPropertiesTable.clearModel();
        /** エラー箇所パネル */
        this.panelError.clearModel();
        /** 検索結果パネル */
        this.panelSearchResult.clearModel();
        /** 参照一覧パネル */
        this.panelReference.clearModel();
        /** 変数有効域パネル */
        this.panelScope.clearModel();
        /** プロファイラパネル */
        for (ProfilerTablePanel panel : this.panelProfilerList) {
            panel.clearModel();
        }
        /** プロファイラ:測定区間情報テーブルパネル */
        panelProfilerMeasure.clearModel();

        // トレース結果をクリアする
        clearTrace();

    }

    /**
     * トレース結果をクリアする.<br/>
     * トレースタブすべてを閉じる.
     */
    public void clearTrace() {

        // トレース結果パネルを閉じる
        int count = this.getTabCount();
        boolean blankPanel = false;
        for (int i=count-1; i>=0; i--) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof TraceResultPanel) {
                TraceResultPanel panel = (TraceResultPanel)comp;
                if (!blankPanel) {
                    // 最初のトレースパネルはブランクパネルとする。
                    panel.clearModel();
                    blankPanel = true;
                    setTracePanelTabname(i, panel.getModel());
                }
                else {
                    this.closeTab(i);
                }
            }
        }
    }


    /**
     * 演算カウントパネルを取得する
     * @return		演算カウントパネル
     */
    public OperandTablePanel getPanelOperand() {
        return this.panelOperand;
    }

    /**
     * 要求Byte/FLOP算出結果パネル
     * @return		要求Byte/FLOP算出結果パネル
     */
    public RequiredByteFlopPanel getPanelRequiredByteFlop() {
        return this.panelRequiredByteFlop;
    }

    /**
     * 指定分析情報パネルのインデックスを取得する.<br/>
     * 閉じている場合は-1を返す
     * @param panel 		指定分析情報パネル識別子
     * @return 		タブインデックス
     */
    public int getTabIndex(ANALYSIS_PANEL panel) {

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof IAnalisysComponent) {
                if ( ((IAnalisysComponent)comp).getEnumPanel() == panel) {
                    return i;
                }
            }
        }
        return -1;
    }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    public CodeLine getSelectedCodeLine() {
        IAnalisysComponent tab = (IAnalisysComponent)this.getSelectedComponent();
        return tab.getSelectedCodeLine();
    }


    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    public IBlock getSelectedBlock() {
        IAnalisysComponent tab = (IAnalisysComponent)this.getSelectedComponent();
        return tab.getSelectedBlock();
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    public IInformation getSelectedInformation() {
        IAnalisysComponent tab = (IAnalisysComponent)this.getSelectedComponent();
        return tab.getSelectedInformation();
    }

    /**
     * トレース結果を表示する
     * @param modelTrace		トレース結果モデル
     */
    public void viewAnalysisTrace(TraceResultModel modelTrace) {

        if (modelTrace == null) return;

        int count = this.getTabCount();
        int index = -1;
        TraceResultPanel panelTrace = null;
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof TraceResultPanel) {
                TraceResultPanel panel = (TraceResultPanel)comp;
                TraceResultModel panelModel = panel.getModel();
                // ブランクモデルであるか
                if (panelModel.getTraceWord() == null || panelModel.getRootBlock() == null) {
                    index = i;
                    panelTrace = panel;
                    break;
                }
                // 同一トレースモデルであるかチェックする
                if (panelModel.equalsTrace(modelTrace)) {
                    index = i;
                    panelTrace = panel;
                    break;
                }
            }
        }

        if (panelTrace == null) {
            int lastindex = -1;
            for (int i=0; i<count; i++) {
                Component comp = this.getComponentAt(i);
                if ( comp instanceof TraceResultPanel) {
                    lastindex = i;
                }
            }
            // トレースパネル挿入位置
            lastindex++;
            // ブランクモデル,同一トレースモデルが存在しないので、新しいトレースパネルを作成する
            panelTrace = createTraceResultPanel();
            if (lastindex > 0 && lastindex <= count-1) {
                this.insertTab(ANALYSIS_PANEL.TRACE.getTabName(),  panelTrace, lastindex);
                index = lastindex;
            }
            else {
                this.addTab(ANALYSIS_PANEL.TRACE.getTabName(),  panelTrace);
                index = this.getTabCount() - 1;
            }
        }

        // トレースモデルを設定する
        panelTrace.setModel(modelTrace);

        // トレース結果タブ名を設定する
        setTracePanelTabname(index, modelTrace);

        // トレースタブをアクティブにする
        this.setSelectedIndex(index);
    }


    /**
     * 新しいトレースパネルを作成する
     * @return		 作成トレースパネル
     */
    private TraceResultPanel createTraceResultPanel() {
        TraceResultPanel panelTrace = new TraceResultPanel(ANALYSIS_PANEL.TRACE);

        // アクションリスナ設定
        panelTrace.setActionListener(this.mainMenu);
        // ソースプロパティ設定
        panelTrace.setSourceProperties(this.propertiesSource);

        return panelTrace;
    }

    /**
     * トレース結果タブ名を設定する.<br/>
     * トレース変数と行番号とする
     * @param index		タブインデックス
     * @param modelTrace	トレースモデル
     */
    private void setTracePanelTabname(int index, TraceResultModel modelTrace) {
        if (index >= this.getTabCount()) return;

        // トレース変数と行番号とする
        String msg = null;
        if (modelTrace != null) {
            String  word = modelTrace.getTraceWord();
            IBlock block = modelTrace.getRootBlock();
            if (word != null && block != null) {
                String name = block.toString();
                if (block instanceof Procedure) {
                    name = ((Procedure)block).get_name();
                }
                msg = "( " + word + ":" + name + ")";
            }
        }
        String tabname = ANALYSIS_PANEL.TRACE.getTabName();
        if (msg != null) {
            tabname += " " + msg;
        }

        // タブ名を設定する
        setTabTitle(index, tabname);
    }


    /**
     * 現在選択されている分析情報:トレースパネルの取得を行う.<br/>
     * トレースパネルが未選択の場合は、nullを返す。
     * @return		選択分析情報トレースパネル
     */
    public TraceResultPanel getSelectedTracePanel() {
        int index = this.getSelectedIndex();
        IAnalisysComponent tab = (IAnalisysComponent)this.getComponentAt(index);
        if (tab.getEnumPanel() == ANALYSIS_PANEL.TRACE) {
            return (TraceResultPanel)tab;
        }

        return null;
    }

    /**
     * 現在選択されている分析情報:プロファイラ:コスト情報パネルの取得を行う.<br/>
     * プロファイラパネルが未選択の場合は、nullを返す。
     * @return		選択プロファイラ:コスト情報パネル
     */
    public ProfilerTablePanel getSelectedProfilerCostPanel() {
        int index = this.getSelectedIndex();
        IAnalisysComponent tab = (IAnalisysComponent)this.getComponentAt(index);
        if (tab.getEnumPanel() == ANALYSIS_PANEL.COST_LINE
            || tab.getEnumPanel() == ANALYSIS_PANEL.COST_LOOP
            || tab.getEnumPanel() == ANALYSIS_PANEL.COST_PROCEDURE) {
            return (ProfilerTablePanel)tab;
        }

        return null;
    }

    /**
     * プロパティ変更イベント
     * @param event		イベント情報
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {

        // ソース表示フォント、フォント色等のソースビュープロパティの変更
        if (event.getNewValue() instanceof SourceProperties) {
            propertiesSource = (SourceProperties)event.getNewValue();

            // 分析ビュータブにソース表示プロパティを設定する。
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
                Component comp = this.getComponentAt(i);
                if ( comp instanceof IAnalisysComponent) {
                    ((IAnalisysComponent)comp).setSourceProperties(propertiesSource);
                }
            }
        }
        // プロファイラプロパティの変更
        else if (event.getNewValue() instanceof ProfilerProperties) {
            ProfilerProperties newProp = (ProfilerProperties)event.getNewValue();
            int newvalue = newProp.getCostinfoMaxCount();
            if (this.propertiesProfiler != null) {
                int oldvalue = this.propertiesProfiler.getCostinfoMaxCount();
                if (newvalue == oldvalue) {
                    return;
                }
            }
            this.propertiesProfiler = (ProfilerProperties)((ProfilerProperties)event.getNewValue()).clone();
            // プロファイラタブにプロファイラプロパティを設定する。
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
                Component comp = this.getComponentAt(i);
                if ( comp instanceof ProfilerTablePanel) {
                    ((ProfilerTablePanel)comp).setProfilerProperties(propertiesProfiler);
                }
            }
        }
    }


    /**
     * トレース結果のキーワードリストを取得する
     * @return		トレースキーワードリスト
     */
    public Keyword[] getTraceKeywords() {

        // キーワードリスト
        List<Keyword> list = new ArrayList<Keyword>();

        /***** アクティブトレースタブののキーワードリストを取得する  ******
        // トレース結果のキーワードリストを取得する
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof TraceResultPanel) {
                TraceResultPanel panel = (TraceResultPanel)comp;
                Keyword[] words = panel.getTraceKeywords();
                if (words != null) {
                    list.addAll(Arrays.asList(words));
                }
            }
        }
        ****************************************/
        IAnalisysComponent comp = getSelectedPanel();
        if ( comp instanceof TraceResultPanel) {
            TraceResultPanel panel = (TraceResultPanel)comp;
            Keyword[] words = panel.getTraceKeywords();
            if (words != null) {
                list.addAll(Arrays.asList(words));
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new Keyword[0]);
    }

    /**
     * トレースパネルモデルを取得する
     * @return		トレースパネルモデル
     */
    public TraceResultModel[] getTraceResultModels() {

        List<TraceResultModel> list = new ArrayList<TraceResultModel>();
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof TraceResultPanel) {
                TraceResultPanel panel = (TraceResultPanel)comp;
                TraceResultModel panelModel = panel.getModel();
                // ブランクモデル以外を取得する
                if (panelModel.getTraceWord() != null && panelModel.getRootBlock() != null) {
                    list.add(panelModel);
                }
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new TraceResultModel[0]);
    }

    /**
     * 検索結果のキーワードリストを取得する
     * @return		検索キーワードリスト
     */
    public Keyword[] getSearchKeywords() {

        // キーワードリスト
        List<Keyword> list = new ArrayList<Keyword>();

        // 検索結果のキーワードリストを取得する
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof SearchResultPanel) {
                SearchResultPanel panel = (SearchResultPanel)comp;
                Keyword[] words = panel.getSearchKeywords();
                if (words != null) {
                    list.addAll(Arrays.asList(words));
                }
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new Keyword[0]);
    }

    /**
     * 分析タブ変更イベント
     * @param action        タブの変更アクション
     */
    public void addTabChangeListener(AnalysisTabChangeAction action) {
        this.addChangeListener(action);
    }

    /**
     * 分析タブ変更に伴う処理を行う。
     */
    public void changeAnalisysTab() {
        ChangeListener[] listeners = this.getChangeListeners();
        if (listeners == null || listeners.length <= 0) return;
        for (ChangeListener l : listeners) {
            if (l instanceof AnalysisTabChangeAction) {
                ((AnalysisTabChangeAction)l).changeAnalisysTab();
            }
        }
    }

    /**
     * プロファイラポップアップメニューを設定する
     * @param profilerPopup		プロファイラポップアップメニュー
     */
    public void setProfilerPopupMenu(ProfilerPopupMenu profilerPopup) {
        /** プロファイラパネル */
        for (ProfilerTablePanel panel : this.panelProfilerList) {
            panel.setPopupMenu(profilerPopup);
        }
    }

    /**
     * プロファイラモデルを取得するする
     * @return       プロファイラモデル
     */
    public ProfilerTableBaseModel[] getProfilerModels() {
        List<ProfilerTableBaseModel> list = new ArrayList<ProfilerTableBaseModel>();
        /** プロファイラパネル */
        for (ProfilerTablePanel panel : this.panelProfilerList) {
            if (panel.getModel() != null) {
                list.add(panel.getModel());
            }
        }
        if (list.size() <= 0) return null;
        return list.toArray(new ProfilerTableBaseModel[0]);
    }

    /**
     * プロファイラ情報をクリアする
     */
    public void clearProfilerInfo() {
        /** プロファイラパネル */
        for (ProfilerTablePanel panel : this.panelProfilerList) {
            if (panel.getModel() != null) {
                panel.clearModel();
            }
        }
    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    public void copyClipboard() {
        IAnalisysComponent panel = getSelectedPanel();
        panel.copyClipboard();
    }


    /**
     * 指定分析情報パネルを閉じる.
     * @param panel      閉じる分析情報パネル
     */
    public void closeTab(ANALYSIS_PANEL panel) {
        int count = this.getTabCount();
        for (int i=count-1; i>=0; i--) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof IAnalisysComponent) {
                if ( ((IAnalisysComponent)comp).getEnumPanel() == panel) {
                    this.closeTab(i);
                }
            }
        }
    }
}
