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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableColumnModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ProfilerMeasureModel;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 測定区間情報パネルクラス
 * @author RIKEN
 */
public class ProfilerMeasurePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, ActionListener, MouseListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** クリアボタン */
    private JButton btnClear;
    /** 削除ボタン */
    private JButton btnRemove;
    /** エクスポートボタン */
    private JButton btnExport;
    /** 該当個所を開く */
    private JButton btnOpenFile;
    /** 測定区間情報ラベル */
    private JLabel label;
    /** スクロールパイン */
    private JScrollPane scrollPane;
    /** 測定区間情報テーブル */
    private JTable tableMeasure;
    /** 測定区間情報テーブルモデル */
    private ProfilerMeasureModel model;
    /** 該当個所を開くアクション */
    private ViewOpenAnalysisLineAction actionOpenAnalysis;

    /**
     * コンストラクタ
     */
    public ProfilerMeasurePanel() {
        super();

        // モデルの生成を行う
        model = new ProfilerMeasureModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();

    }

    /**
     * コンストラクタ
     * @param proparties		分析情報パネル識別子
     */
    public ProfilerMeasurePanel(ANALYSIS_PANEL proparties) {
        super(proparties);

        // モデルの生成を行う
        model = new ProfilerMeasureModel();
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
                        btnClear.setPreferredSize(buttonSize);
                        btnClear.setMinimumSize(buttonSize);
                        btnClear.setMaximumSize(buttonSize);
                        btnClear.setContentAreaFilled(false);
                        btnClear.setBorderPainted(false);
                        btnClear.addActionListener( new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                // 確認メッセージを表示する。
                                int option = JOptionPane.showConfirmDialog(null,
                                		  Message.getString("profilermeasurepanel.confirmdialog.clearall.message"), //登録済みのすべての測定区間を削除します。\nよろしいですか？
                                          Message.getString("profilermeasurepanel.confirmdialog.clearall.title"), //測定区間のクリア
                                          JOptionPane.OK_CANCEL_OPTION,
                                          JOptionPane.WARNING_MESSAGE);
                                if (option != JOptionPane.OK_OPTION) {
                                    return;
                                }
                                // モデルクリア
                                clearModel();
                            }
                        });
                    }
                    // 削除ボタン
                    {
                        Icon icon = ResourceUtils.getIcon("remove.gif");
                        btnRemove = new JButton(icon);
                        panelButtons.add(btnRemove);
                        btnRemove.setPreferredSize(buttonSize);
                        btnRemove.setMinimumSize(buttonSize);
                        btnRemove.setMaximumSize(buttonSize);
                        btnRemove.setContentAreaFilled(false);
                        btnRemove.setBorderPainted(false);
                        btnRemove.addActionListener( new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                // 確認メッセージを表示する。
                                int option = JOptionPane.showConfirmDialog(null,
                                		  Message.getString("profilermeasurepanel.confirmdialog.clear.message"), //選択測定区間を削除します。\nよろしいですか？
                                		  Message.getString("profilermeasurepanel.confirmdialog.clear.title"), //測定区間の削除
                                          JOptionPane.OK_CANCEL_OPTION,
                                          JOptionPane.WARNING_MESSAGE);
                                if (option != JOptionPane.OK_OPTION) {
                                    return;
                                }
                                // 測定区間削除
                                removeMeasureData();

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
                    label.setText("Operand");
                }
            }
            {
                {
                    // 測定区間情報テーブル
                    tableMeasure = new JStripeTable();
                    tableMeasure.setModel(this.model.getTableModel());
                    tableMeasure.setAutoCreateColumnsFromModel(false);
                    tableMeasure.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                    tableMeasure.setColumnSelectionAllowed(false);

                    // テーブル列幅設定
                    DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableMeasure.getColumnModel();
                    model.setTableColumnWidth(columnModel);

                    // スクロールパイン
                    scrollPane = new JScrollPane();
                    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setViewportView(tableMeasure);
                    scrollPane.getViewport().setBackground(Color.WHITE);

                    add(scrollPane);
                }
            }

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnRemove.setToolTipText(Message.getString("profilermeasurepanel.tooltip.delete")); //選択削除
            btnOpenFile.setToolTipText(Message.getString("profilermeasurepanel.tooltip.open")); //選択箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート

            // イベントリスナ設定
            btnOpenFile.addActionListener(this);
            tableMeasure.addMouseListener(this);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 測定区間情報モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 測定区間情報モデル
        ProfilerMeasureModel observer = (ProfilerMeasureModel)o;

        // テーブルモデル
        tableMeasure.setModel(observer.getTableModel());

        // パネルタイトル
        this.label.setText(observer.getTitle());

    }


    /**
     * 測定区間情報モデルを取得する
     * @return		測定区間情報モデル
     */
    public ProfilerMeasureModel getModel() {
        return model;
    }


    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);
        // 子コンポーネントにもフォーカスリスナを設定する
        SwingUtils.addChildFocusListener(this, listener);
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
        // 該当箇所を開く
        this.actionOpenAnalysis = (ViewOpenAnalysisLineAction) menu.getActionOpenAnalysisLine();
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearModel();
    }

    /**
     * 選択されている測定区間を削除する
     */
    private void removeMeasureData() {
        //  選択行
        ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
        // 測定区間データのクリア
        this.model.removeMeasureData(data);
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
        ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
        if (data == null) return null;
        return data.getMeasureArea();
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        //  選択行
         ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
         if (data == null) return null;
         IBlock[] blocks = data.getMeasureBlocks();
         if (blocks == null) return null;
         return blocks[0];
    }

    /**
     * 選択行の測定区間データを取得する
     * @return		選択測定区間データ
     */
    private ProfilerMeasureInfo.MeasureData getSelectedMeasureData() {
        int selection = this.tableMeasure.getSelectedRow();
        if (selection < 0) return null;
        // テーブル・モデルの行数に変換
        int modelRow = this.tableMeasure.convertRowIndexToModel(selection);
        if (modelRow < 0) return null;
        Object cell = this.tableMeasure.getModel().getValueAt(modelRow, 0);
        if (cell == null) return null;
        if (cell instanceof ProfilerMeasureInfo.MeasureData) {
            return (ProfilerMeasureInfo.MeasureData)cell;
        }
        return null;
    }


    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() { return null; }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {}

    /**
     * マウスクリックイベント
     * @param event		マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {

        // クリックチェック
        if (SwingUtilities.isLeftMouseButton(event)) {
            // ダブルクリック
            if (event.getClickCount() == 2) {
                // 該当個所を開く
                this.btnOpenFile.doClick();
            }
        }
    }


    /**
     * マウスボタンダウンイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}

    /**
     * ボタンのクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // 該当行を開く
        if (event.getSource() == this.btnOpenFile) {
            ProfilerMeasureInfo.MeasureData data = getSelectedMeasureData();
            if (data == null) return;
            if (data.getMeasureBlocks() != null) {
                this.actionOpenAnalysis.viewSelectedArea(data.getMeasureBlocks());
            }
            else if (data.getMeasureCodeLine() != null) {
                this.actionOpenAnalysis.clearSourceBlock();
                this.actionOpenAnalysis.viewSelectedSourceBlock(data.getMeasureCodeLine());
            }
        }
    }


    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.tableMeasure == null) return;
        String text = SwingUtils.toCsvOfSeletedRows(this.tableMeasure);
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


