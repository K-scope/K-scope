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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.BevelBorder;
import javax.swing.border.LineBorder;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisTabChangeAction;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.action.FileExitAction;
import jp.riken.kscope.action.HelpVersionAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.dialog.ManageSettingsFilesDialog;
import jp.riken.kscope.dialog.ProfilerLegendDialog;
import jp.riken.kscope.dialog.ProgressDialog;
import jp.riken.kscope.dialog.SearchFindDialog;
import jp.riken.kscope.dialog.SearchGrepDialog;
import jp.riken.kscope.dialog.SearchTreeDialog;
import jp.riken.kscope.menu.ProfilerPopupMenu;
import jp.riken.kscope.menu.LanguageTreePopupMenu;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.ModuleTreePopupMenu;
import jp.riken.kscope.menu.ReplacePopupMenu;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.menu.SourceTreePopupMenu;
import jp.riken.kscope.menu.VariablePopupMenu;
import jp.riken.kscope.menu.XmlTreePopupMenu;
import jp.riken.kscope.service.AppController;

/**
 * メインフレーム
 * @author RIKEN
 */
public class MainFrame extends javax.swing.JFrame implements ITabComponent {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** アプリケーションコントローラ */
    AppController controller;

    /** ステータスビュー */
    private StatusBarPanel panelStatusBar;
    /** ソースビュー */
    private SourceView panelSourceView;
    /** 分析情報ビュー */
    private AnalysisView panelAnalysisView;
    /** エクスプローラビュー */
    private ExploreView panelExplorerView;
    /** プログレスバーダイアログ */
    private ProgressDialog dialogProgress;

    /** ソース検索ダイアログ */
    private SearchFindDialog dialogSearchFind;
    /** ファイル検索ダイアログ */
    private SearchGrepDialog dialogSearchGrep;
    /** ツリー検索ダイアログ */
    private SearchTreeDialog dialogSearchTree;
    /** プロファイラプレビューダイアログ */
    private ProfilerLegendDialog dialogProfilerLegend;

    /**
     * タブフォーカスリスナ
     * 最終アクセス、アクティブパネルの監視用リスナ
     */
    private TabFocusListener focusListener;
    /** メインメニュー */
    private MainMenu menuMain;

	/**
     * コンストラクタ
     */
    public MainFrame() {
        super();
    }

    /**
     * 初期化を行う
     * @param   controller 			アプリケーションコントローラ
     */
    public void initialize(AppController controller) {
        this.controller = controller;
        initGUI();
    }

    /**
     * メインフレームの初期化を行う.
     */
    private void initGUI() {
        this.setTitle(Message.getString("mainframe.title")); //K-scope

        final int SPLIT_DIVIDERSIZE = 5;        // 仕切線サイズ
        focusListener = new TabFocusListener();

        BorderLayout frameLayout = new BorderLayout();
        Dimension screen_size = getToolkit().getScreenSize();
        this.setBounds(0, 0, screen_size.width, screen_size.height);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.getContentPane().setLayout(frameLayout);
        {
            panelStatusBar = new StatusBarPanel();
            this.getContentPane().add(panelStatusBar, BorderLayout.SOUTH);
            panelStatusBar.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
        }
        {
            JSplitPane splitHorizontal = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
            this.getContentPane().add(splitHorizontal, BorderLayout.CENTER);
            splitHorizontal.setResizeWeight(0.1);
            splitHorizontal.setDividerSize(SPLIT_DIVIDERSIZE);
            {
                JSplitPane splitVertical = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
                splitHorizontal.add(splitVertical, JSplitPane.RIGHT);
                splitVertical.setResizeWeight(0.8);
                splitVertical.setDividerSize(SPLIT_DIVIDERSIZE);

                // 分析ビューの追加
                {
                    // MacOSXで何故かJTabbedPaneの高さが広がる対策
                    // 分析ビューをJSplitPaneに直接追加はしない
                    // JPanelに分析ビューを追加してJPanelをJSplitPaneに追加する。
                    panelAnalysisView = new AnalysisView();
                    //splitVertical.add(panelAnalysisInfoView, JSplitPane.RIGHT);
                    JPanel panel = new JPanel();
                    splitVertical.add(panel, JSplitPane.RIGHT);
                    panel.setLayout(new BorderLayout());
                    panel.setBorder(new LineBorder(Color.GRAY, 1));
                    panel.add(panelAnalysisView);

                    // 親フォームの設定
                    panelAnalysisView.setParentComponent(this);
                    // フォーカスリスナの設定
                    panelAnalysisView.addTabFocusListener(focusListener);

                }
                // ソースビューの追加
                {
                    panelSourceView = new SourceView();
                    splitVertical.add(panelSourceView, JSplitPane.LEFT);

                    // 親フォームの設定
                    panelSourceView.setParentComponent(this);
                    // フォーカスリスナの設定
                    panelSourceView.addTabFocusListener(focusListener);
                }
            }
            // エクスプローラビューの追加
            {
                panelExplorerView = new ExploreView();
                splitHorizontal.add(panelExplorerView, JSplitPane.LEFT);
            }
        }

        // メインメニュー
        menuMain = new MainMenu(controller);
        this.setJMenuBar(menuMain);

        // ソースファイルパネルコンテキストメニュー
        SourcePanelPopupMenu menuSourcePanel = new SourcePanelPopupMenu(controller);
        this.panelSourceView.setSourcePanelPopupMenu(menuSourcePanel);

        // 構造ツリーエクスプローラコンテキストメニュー
        LanguageTreePopupMenu menuLanguageTree = new LanguageTreePopupMenu(controller);
        panelExplorerView.setLanguagePopupMenu(menuLanguageTree);

        // モジュールツリーエクスプローラコンテキストメニュー
        ModuleTreePopupMenu menuModuleTree = new ModuleTreePopupMenu(controller);
        panelExplorerView.setModulePopupMenu(menuModuleTree);

        // ソースファイルエクスプローラコンテキストメニュー
        SourceTreePopupMenu menuSourceTree = new SourceTreePopupMenu(controller);
        panelExplorerView.setSourcePopupMenu(menuSourceTree);

        // ツリーの変更イベント
        ExploreTreeChangeAction treeChangeAction = new ExploreTreeChangeAction(controller);
        panelExplorerView.addTreeSelectionListener(treeChangeAction);

        // XMLファイルエクスプローラコンテキストメニュー
        XmlTreePopupMenu menuXml = new XmlTreePopupMenu(controller);
        panelExplorerView.setXmlPopupMenu(menuXml);

        // メニューのアクションリスナをパネルのボタンに割り当てる。
        panelAnalysisView.setActionListener(menuMain);

        // 変数特性一覧のコンテキストメニュー
        panelAnalysisView.getPanelVariable().setPopupMenu(new VariablePopupMenu(controller));

        // プロファイラコスト情報のコンテキストメニュー
        ProfilerPopupMenu profilerPopup = new ProfilerPopupMenu(controller);
        panelAnalysisView.setProfilerPopupMenu(profilerPopup);

        // タブの変更イベント
        AnalysisTabChangeAction tabChangeAction = new AnalysisTabChangeAction(controller);
        panelAnalysisView.addTabChangeListener(tabChangeAction);

        // 起動時の画面サイズ
        this.setSize(new Dimension(screen_size.width, screen_size.height));

        // 終了イベント
        this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new FileExitAction(this.controller));

        // プログレスバーダイアログ
        dialogProgress = new ProgressDialog(this, false);
        // ソース検索ダイアログ
        dialogSearchFind = new SearchFindDialog(this, true);
        // ファイル検索ダイアログ
        dialogSearchGrep = new SearchGrepDialog(this, true);
        // ツリー検索ダイアログ
        dialogSearchTree = new SearchTreeDialog(this, true);

        // ステータスバーをステータス通知登録
        Application.addStatus(panelStatusBar);
        // プログレスバーダイアログをステータス通知登録
        Application.addStatus(dialogProgress);

    }

    /**
     * ソースビューを取得する
     * @return		ソースビュー
     */
    public SourceView getPanelSourceView() {
        return panelSourceView;
    }

    /**
     * 分析情報ビューを取得する。
     * @return		分析情報ビュー
     */
    public AnalysisView getPanelAnalysisView() {
        return panelAnalysisView;
    }

    /**
     * エクスプローラビューを取得する
     * @return		エクスプローラビュー
     */
    public ExploreView getPanelExplorerView() {
        return panelExplorerView;
    }

    /**
     * 親コンポーネントを取得する.
     * @return		親コンポーネント
     */
    @Override
    public ITabComponent getParentComponent() { return null; }

    /**
     * 親コンポーネントを設定する.
     * @param component		親コンポーネント
     */
    @Override
    public void setParentComponent(ITabComponent component) { }

    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {}

    /**
     * タブを閉じる
     */
    @Override
    public void closeTabComponent() {
        ITabComponent forcus = this.focusListener.getLastTabComponent();
        if (forcus != null) {
            forcus.closeTabComponent();
        }
        return;
    }

    /**
     * アクティブなビュー識別子を取得する
     * @return		アクティブなビュー識別子
     */
    public FRAME_VIEW getEnumView() {
        ITabComponent forcus = this.focusListener.getLastTabComponent();
        if (forcus == null) return null;
        ITabComponent parent = forcus.getParentComponent();
        ClosableTabbedPane panel = null;
        if (parent != null && parent instanceof ClosableTabbedPane) {
            panel = (ClosableTabbedPane)parent;
        }
        else if (forcus instanceof ClosableTabbedPane) {
            panel = (ClosableTabbedPane)forcus;
        }

        if (panel == null) return null;

        return panel.getViewType();
    }


    /**
     * コントローラを取得する。
     * @return		アプリケーションコントローラ
     */
    public AppController getController() {
        return this.controller;
    }

    /**
     * プログレスバーダイアログを取得する
     * @return		プログレスバーダイアログ
     */
    public ProgressDialog getDialogProgress() {
        return dialogProgress;
    }

    /**
     * バージョン情報ダイアログを表示する
     */
    public void showAboutDialog() {
        HelpVersionAction action = new HelpVersionAction(this.controller);
        action.showAboutDialog();
    }
    
    /**
     * アプリケーションを終了する
     */
    public void exitApplication() {
        FileExitAction action = new FileExitAction(this.controller);
        action.exitApplication(this);
    }

    /**
     * ソース検索ダイアログを取得する
     * @return		ソース検索ダイアログ
     */
    public SearchFindDialog getDialogSearchFind() {
        return this.dialogSearchFind;
    }

    /**
     * ファイル検索ダイアログを取得する
     * @return		ファイル検索ダイアログ
     */
    public SearchGrepDialog getDialogSearchGrep() {
        return this.dialogSearchGrep;
    }

    /**
     * ツリー検索ダイアログを取得する
     * @return		ツリー検索ダイアログ
     */
    public SearchTreeDialog getDialogSearchTree() {
        return this.dialogSearchTree;
    }

    /**
     * プロファイラプレビューダイアログを取得する
     * @return     プロファイラプレビューダイアログ
     */
	public ProfilerLegendDialog getDialogProfilerLegend() {
		return dialogProfilerLegend;
	}

    /**
     * プロファイラプレビューダイアログを設定する
     * @param   dialog     プロファイラプレビューダイアログ
     */
	public void setDialogProfilerLegend(ProfilerLegendDialog dialog) {
		this.dialogProfilerLegend = dialog;
	}

	/**
	 * メインメニューの取得を行う.
	 * @return		メインメニュー
	 */
    public MainMenu getMenuMain() {
		return menuMain;
	}

}


