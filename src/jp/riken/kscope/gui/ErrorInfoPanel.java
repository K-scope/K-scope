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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * エラーパネルクラス
 * @author RIKEN
 *
 */
public class ErrorInfoPanel extends AnalisysPanelBase implements Observer, IAnalisysComponent {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** エラーテーブル */
    private JTable tableError;
    /** クリアボタン */
    private JButton btnClear;
    /** エクスポートボタン */
    private JButton btnExport;
    /** ファイルを開く */
    private JButton btnOpenFile;
    /** エラーラベル */
    private JLabel label;

    /** エラーテーブルモデル */
    private ErrorInfoModel model;

    /**
     * コンストラクタ
     */
    public ErrorInfoPanel() {
        super();

        // 初期化を行う。
        initialize();

    }

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     */
    public ErrorInfoPanel(ANALYSIS_PANEL panel) {
        super(panel);

        // 初期化を行う。
        initialize();

    }

    /**
     * 初期化を行う。
     */
    private void initialize() {

        // モデルの生成を行う
        model = new ErrorInfoModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();
    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);
//            setPreferredSize(new Dimension(400, 64));

            // 上部の情報ラベル、ボタンの配置パネル
            {
                JPanel panelTop = new JPanel();
                panelTop.setLayout(new BorderLayout());
                this.add(panelTop, BorderLayout.NORTH);
                panelTop.setBorder(new CompoundBorder(
                                            new LineBorder(Color.BLACK, 1),
                                            BorderFactory.createEmptyBorder(0, 5, 0, 20)));
                // ボタン配置パネル
                {
                    JPanel panelButtons = new JPanel();
                    panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
                    panelTop.add(panelButtons, BorderLayout.EAST);

                    java.awt.Dimension buttonSize = new java.awt.Dimension(24, 24);
                    // クリアボタン
                    {
                        Icon icon = ResourceUtils.getIcon("removeall.gif");
                        btnClear = new JButton(icon);
                        panelButtons.add(btnClear);
                        btnClear.setContentAreaFilled(false);
                        btnClear.setBorderPainted(false);
                        btnClear.setPreferredSize(buttonSize);
                        btnClear.setMinimumSize(buttonSize);
                        btnClear.setMaximumSize(buttonSize);
                        btnClear.addActionListener( new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                // モデルクリア
                                clearModel();
                            }
                        });
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("openfile.gif");
                        btnOpenFile = new JButton(icon);
                        btnOpenFile.setContentAreaFilled(false);
                        btnOpenFile.setBorderPainted(false);
                        btnOpenFile.setPreferredSize(buttonSize);
                        btnOpenFile.setMinimumSize(buttonSize);
                        btnOpenFile.setMaximumSize(buttonSize);
                        panelButtons.add(btnOpenFile);
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("save.gif");
                        btnExport = new JButton(icon);
                        btnExport.setContentAreaFilled(false);
                        btnExport.setBorderPainted(false);
                        btnExport.setPreferredSize(buttonSize);
                        btnExport.setMinimumSize(buttonSize);
                        btnExport.setMaximumSize(buttonSize);
                        panelButtons.add(btnExport);
                    }
                }

                // ラベル配置
                {
                    label = new JLabel();
                    panelTop.add(label, BorderLayout.CENTER);
                    label.setText("");
                }
            }
            {
                {
                    // エラーテーブル
                    tableError = new JStripeTable();
                    tableError.setModel(model.getTableModel());

                    tableError.setAutoCreateColumnsFromModel(false);
                    tableError.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                    tableError.setColumnSelectionAllowed(false);

                    // テーブル列モデル
                    DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableError.getColumnModel();
                    TableColumn column = null;

                    // 1列目はコード行情報：非表示
                    column = columnModel.getColumn(0);
                    column.setPreferredWidth(0);
                    column.setMinWidth(0);
                    column.setMaxWidth(0);
                    column.setResizable(false);

                    // エラーメッセージ列
                    column = columnModel.getColumn(1);
                    column.setPreferredWidth(360);
                    column.setMinWidth(160);

                    // ファイル名
                    column = columnModel.getColumn(2);
                    column.setPreferredWidth(240);
                    column.setMinWidth(120);

                    // 行番号
                    column = columnModel.getColumn(3);
                    column.setPreferredWidth(80);
                    column.setMinWidth(80);

                    // スクロールパイン
                    JScrollPane scrollTable = new JScrollPane(tableError);
                    scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.getViewport().setBackground(Color.WHITE);

                    add(scrollTable);

                }

            }

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnOpenFile.setToolTipText(Message.getString("errorInfopanel.tooltip.open")); //エラー箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * エラーモデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // テーブルモデル
        ErrorInfoModel observer = (ErrorInfoModel)o;
        tableError.setModel(observer.getTableModel());

        // パネルタイトル
        this.label.setText(observer.getTitle());

        // エラー発生状況
        if (observer.getErrorListCount() > 0) {
            // エラー発生であるので、自身タブをアクティブにする
            AnalysisView tab = (AnalysisView)this.getParentComponent();
            tab.setSelectedPanel(this.getEnumPanel());
        }
    }

    /**
     * エラーテーブルモデルを取得する
     * @return		エラーテーブルモデル
     */
    public ErrorInfoModel getModel() {
        return model;
    }


    /**
     * タブフォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);
        // 子コンポーネントにもフォーカスリスナを設定する
        if (this.tableError != null) {
            this.tableError.addFocusListener(listener);
            this.btnClear.addFocusListener(listener);
            this.btnExport.addFocusListener(listener);
            this.btnOpenFile.addFocusListener(listener);
        }
    }


    /**
     * エクスポートを行う
     */
    @Override
    public void export(File file) {
        if (this.model == null) return;
        
        model.writeFile(file);
    }

    /**
     * パネルにアクションリスナを設定する.<br/>
     * メニューバーに作成済みのアクションリスナをパネルボタンに割り当てる。
     * @param menu		メニューバー
     */
    @Override
    public void setActionListener(MainMenu menu) {
        // 分析情報エクスポートアクション
        this.btnExport.addActionListener(menu.getActionExportAnalysis());
        // エラー箇所を開く
        this.btnOpenFile.addActionListener((ActionListener) menu.getActionErrorOpenFile());
        this.tableError.addMouseListener((MouseListener) menu.getActionErrorOpenFile());
    }

    /**
     * 選択行のエラーメッセージを取得する.<br/>
     * テーブルモデルの2列目がエラーメッセージが設定されている。
     * @return		コード情報
     */
    public String getSelectedErrorMessage() {

        //  選択行
        int row = this.tableError.getSelectedRow();
        if (row < 0) return null;

        // ２列目はエラーメッセージ
        DefaultTableModel tableModel = (DefaultTableModel) this.tableError.getModel();
        Object obj = tableModel.getValueAt(row, 1);
        if (obj == null) return null;

        return obj.toString();
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearErrorList();
    }

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        //  選択行
        int row = this.tableError.getSelectedRow();
        if (row < 0) return null;

        // 1列目がCodeLine情報
        DefaultTableModel tableModel = (DefaultTableModel) this.tableError.getModel();
        Object obj = tableModel.getValueAt(row, 0);
        if (obj == null) return null;
        if (obj instanceof CodeLine) {
            return (CodeLine)obj;
        }

        return null;
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        //  選択行
        int row = this.tableError.getSelectedRow();
        if (row < 0) return null;

        // 1列目がCodeLine情報
        DefaultTableModel tableModel = (DefaultTableModel) this.tableError.getModel();
        Object obj = tableModel.getValueAt(row, 0);
        if (obj == null) return null;
        if (obj instanceof IBlock) {
            return (IBlock)obj;
        }

        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        return null;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {}


    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.tableError == null) return;
        String text = SwingUtils.toCsvOfSeletedRows(this.tableError);
        if (text == null) return;

        // クリップボードにコピーする
        SwingUtils.copyClipboard(text);
    }

    /**
     * エクスポート可能か否か
     */
	@Override
	public boolean isExportable() {
		if (this.model == null) return false;
		return (!this.model.isEmpty());
	}
}


