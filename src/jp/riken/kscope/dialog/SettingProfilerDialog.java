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

package jp.riken.kscope.dialog;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロファイラ設定ダイアログ
 * @author RIKEN
 */
public class SettingProfilerDialog extends javax.swing.JDialog implements ActionListener, TreeSelectionListener {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** 適用ボタン */
    private JButton btnApply;
    /** OKボタン */
    private JButton btnOk;
    /** プロファイラ設定リスト */
    private JTree treeProperties;
    /** プロファイラ設定リストデータ */
    private DefaultTreeModel modelProperties;
    /** プロファイラ設定パネル */
    private JPanel panelProperty;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** プロファイラプロパティ */
    private ProfilerProperties properities;
    /** コンテンツパネル */
    private JPanel panelContent;
    /** 設定パネル */
    private JPanel panelSettings;


    /** パネルタイプ */
    private enum PANEL_TYPE {
        /** コストバーグラフ色、最大表示行数 */
        PROFILER_VIEW,
        /** コストルーラ色 */
        PROFILER_RULER,
        /** EPROF測定区間説明 */
        EPROF_STATEMENT_DISCRIPTION,
        /** EPROF測定区間:開始 */
        EPROF_STATEMENT_START,
        /** EPROF測定区間:終了 */
        EPROF_STATEMENT_END
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public SettingProfilerDialog(Frame frame) {
        super(frame);
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SettingProfilerDialog(Frame frame, boolean modal) {
        super(frame, modal);
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities		プロファイラ設定プロパティ
     */
    public SettingProfilerDialog(Frame frame, boolean modal, ProfilerProperties properities) {
        super(frame, modal);
        this.properities = properities;
        initGUI();
    }

    /**
     * プロファイラ設定を設定する。
     * @param properities		プロファイラ設定プロパティ
     */
    public void setProfilerProperties(ProfilerProperties properities) {
        this.properities = properities;

        return;
    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            thisLayout.setHgap(5);
            thisLayout.setVgap(5);
            getContentPane().setLayout(thisLayout);

            // ボタンパネル
            {
                JPanel panelButtons = new JPanel();
                FlowLayout jPanel1Layout = new FlowLayout();
                jPanel1Layout.setHgap(10);
                jPanel1Layout.setVgap(10);
                panelButtons.setLayout(jPanel1Layout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                panelButtons.setPreferredSize(new java.awt.Dimension(390, 46));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnApply = new JButton();
                    btnApply.setText(Message.getString("dialog.common.button.apply")); //適用
                    btnApply.setPreferredSize(buttonSize);
                    btnApply.addActionListener(this);
                    panelButtons.add(btnApply);
                }
                {
                    btnOk = new JButton();
                    btnOk.setText(Message.getString("dialog.common.button.ok")); //OK
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                    panelButtons.add(btnOk);
                }
                {
                    btnCancel = new JButton();
                    btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                    btnCancel.setPreferredSize(buttonSize);
                    btnCancel.addActionListener(this);
                    btnCancel.setMargin(new Insets(5, 5, 5, 5));
                    panelButtons.add(btnCancel);
                }
            }

            // コンテンツパネル
            {
                panelContent = new JPanel();
                BorderLayout panelContentLayout = new BorderLayout();
                getContentPane().add(panelContent, BorderLayout.CENTER);
                Border border = new EmptyBorder(7,7,0,7);
                panelContent.setBorder(border);
                panelContent.setLayout(panelContentLayout);

                // プロパティリスト
                {
                    JPanel panelList = new JPanel();
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.WEST);
                    {
                        JLabel lblList = new JLabel();
                        panelList.add(lblList, BorderLayout.NORTH);
                        lblList.setText(Message.getString("settingprofilerdialog.label.propertieslist")); //プロパティ設定リスト
                    }
                    {
                        JScrollPane scrollList = new JScrollPane();
                        scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
                        scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelList.add(scrollList, BorderLayout.CENTER);
                        {
                            this.modelProperties = createTreeModel();
                            this.treeProperties = new JTree(this.modelProperties);
                            scrollList.setViewportView(this.treeProperties);
                            //this.treeProperties.setRootVisible(false);

                            for(int i=0;i<this.treeProperties.getRowCount();i++) {
                                this.treeProperties.expandRow(i);
                            }

                            DefaultTreeCellRenderer r = (DefaultTreeCellRenderer)this.treeProperties.getCellRenderer();
                            r.setLeafIcon(null);
                            r.setOpenIcon(null);
                            r.setClosedIcon(null);
                        }
                        Dimension size = new Dimension(240, 400);
                        scrollList.setPreferredSize(size);
                    }
                }
                // 設定パネル
                {
                    this.panelSettings = new JPanel();
                    BorderLayout panelSettingsLayout = new BorderLayout();
                    panelContent.add(panelSettings, BorderLayout.CENTER);
                    Border borderSettings = new EmptyBorder(0,7,0,0);
                    panelSettings.setBorder(borderSettings);
                    panelSettings.setLayout(panelSettingsLayout);
                    {
                        JLabel lblSettings = new JLabel();
                        lblSettings.setText(Message.getString("mainmenu.project.config")); //設定
                        panelSettings.add(lblSettings, BorderLayout.NORTH);
                    }
                    this.panelProperty = new JPanel();
                    panelSettings.add(this.panelProperty, BorderLayout.CENTER);
                    panelSettings.add(panelProperty, BorderLayout.CENTER);

                    // 設定パネル枠
                    EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
                    Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
                    panelProperty.setBorder(borderKeyword);

                }
            }
            setTitle(Message.getString("projectsettingprofileraction.setup.status")); //プロファイラ設定
            setSize(680, 420);

            // イベント
            this.treeProperties.addTreeSelectionListener(this);
            this.treeProperties.setSelectionRow(0);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * プロファイラ設定ツリーモデルの作成を行う。
     * @return   プロファイラ設定ツリーモデル
     */
    private DefaultTreeModel createTreeModel() {

        DefaultMutableTreeNode root = new DefaultMutableTreeNode(Message.getString("projectsettingprofileraction.setup.status")); //プロファイラ設定
        root.setUserObject(new TopTitlePanel());

        // コスト情報表示設定
        {
            DefaultMutableTreeNode node = new DefaultMutableTreeNode();
            root.add(node);
            node.setUserObject(new ProfilerViewPanel(this.properities, PANEL_TYPE.PROFILER_VIEW));
        }
        // コストルーラ表示設定
        {
            DefaultMutableTreeNode node = new DefaultMutableTreeNode();
            root.add(node);
            node.setUserObject(new ProfilerRulerPanel(this.properities, PANEL_TYPE.PROFILER_RULER));
        }

        // 測定区間設定
        {
            DefaultMutableTreeNode node = new DefaultMutableTreeNode(); //測定区間設定
            root.add(node);
            node.setUserObject(new EprofStatementTitlePanel());
            DefaultMutableTreeNode start = new DefaultMutableTreeNode(); //開始ステートメント
            node.add(start);
            start.setUserObject(new EprofStatementPanel(this.properities, PANEL_TYPE.EPROF_STATEMENT_START));
            DefaultMutableTreeNode end = new DefaultMutableTreeNode(); //終了ステートメント
            node.add(end);
            end.setUserObject(new EprofStatementPanel(this.properities, PANEL_TYPE.EPROF_STATEMENT_END));
        }
        DefaultTreeModel model = new DefaultTreeModel(root);
        return model;
    }

    /**
     * プロファイラ設定パネルインターフェイス
     */
    private interface ProfilerPropertiesPanel {
        /**
         * プロパティ設定を更新する
         */
        public void updateProperties();
    }

    /**
     * プロファイラ設定パネル
     */
    private class TopTitlePanel extends javax.swing.JPanel implements ProfilerPropertiesPanel {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * コンストラクタ
         */
        public TopTitlePanel() {
            initGUI();
        }

        /**
         * GUI初期化を行う。
         */
        private void initGUI() {

            BorderLayout panelSettingsLayout = new BorderLayout();
            this.setLayout(panelSettingsLayout);
            {
                JLabel lblSettings = new JLabel(Message.getString("projectsettingprofileraction.setup.status")); //プロファイラ設定
                this.add(lblSettings, BorderLayout.NORTH);
            }
            JPanel panelProperty = new JPanel();
            this.add(panelProperty, BorderLayout.CENTER);
            GridBagLayout panelPropertyLayout = new GridBagLayout();
            panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
            panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
            panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
            panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
            panelProperty.setLayout(panelPropertyLayout);
            panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

            // 設定パネル枠
            EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
            Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
            panelProperty.setBorder(borderKeyword);

            {
            	JLabel label = new JLabel(Message.getString("settingprofilerdialog.discription"));
                panelProperty.add(label, new GridBagConstraints(0, 0, 3, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }

        /**
         * プロパティ設定を更新する
         */
        @Override
        public void updateProperties() { }

        @Override
        public String toString() {
            return Message.getString("projectsettingprofileraction.setup.status"); //プロファイラ設定
        }
    }


    /**
     * 測定ステートメント設定タイトルパネル
     */
    private class EprofStatementTitlePanel extends javax.swing.JPanel implements ProfilerPropertiesPanel {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;
        /** タイトル、ノード表示 */
        private String title = Message.getString("settingprofilerdialog.label.setupmesurementrange.title"); //測定区間設定

        /**
         * コンストラクタ
         */
        public EprofStatementTitlePanel() {
            initGUI();
        }

        /**
         * GUI初期化を行う。
         */
        private void initGUI() {

            BorderLayout panelSettingsLayout = new BorderLayout();
            this.setLayout(panelSettingsLayout);
            {
                JLabel lblSettings = new JLabel(this.title);
                this.add(lblSettings, BorderLayout.NORTH);
            }
            JPanel panelProperty = new JPanel();
            this.add(panelProperty, BorderLayout.CENTER);
            GridBagLayout panelPropertyLayout = new GridBagLayout();
            panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
            panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
            panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
            panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
            panelProperty.setLayout(panelPropertyLayout);
            panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

            // 設定パネル枠
            EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
            Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
            panelProperty.setBorder(borderKeyword);

            {
            	JLabel label = new JLabel(Message.getString("settingprofilerdialog.discription.statement"));
                panelProperty.add(label, new GridBagConstraints(0, 0, 3, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }

        /**
         * プロパティ設定を更新する
         */
        @Override
        public void updateProperties() { }

        @Override
        public String toString() {
            return this.title;
        }
    }

    /**
     * 測定ステートメント設定パネル
     */
    private class EprofStatementPanel extends javax.swing.JPanel implements ProfilerPropertiesPanel {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;
        /** プロファイラ設定 */
        private ProfilerProperties properties;
        /** パネルタイプ */
        private PANEL_TYPE type;
        /** 測定ステートメント:関数名*/
        private JTextField eprofFunctionname;
        /** 測定ステートメント:測定ステートメント文 */
        private JTextArea eprofMeasureStatement;
        /** タイトル、ノード表示 */
        private String title;

        /**
         * コンストラクタ
         * @param properties	プロファイラ
         * @param type			パネルタイプ
         */
        public EprofStatementPanel(ProfilerProperties properties, PANEL_TYPE type) {
            this.properties = properties;
            this.type = type;
            initGUI();
        }


        /**
         * GUI初期化を行う。
         */
        private void initGUI() {
            BorderLayout panelSettingsLayout = new BorderLayout();
            this.setLayout(panelSettingsLayout);
            String functionname = null;
            String statement = null;
            if (this.type == PANEL_TYPE.EPROF_STATEMENT_START) {
                this.title = Message.getString("settingprofilerdialog.label.mesuermentstatement") + //測定ステートメント
                		" : " + Message.getString("settingprofilerdialog.label.start"); //:開始
                functionname = this.properties.getEprofFunctionStart();
                statement = this.properties.getEprofStatementStart();
            }
            else if (this.type == PANEL_TYPE.EPROF_STATEMENT_END) {
                this.title = Message.getString("settingprofilerdialog.label.mesuermentstatement") + //測定ステートメント
                		" : " + Message.getString("settingprofilerdialog.label.end"); //:終了
                functionname = this.properties.getEprofFunctionEnd();
                statement = this.properties.getEprofStatementEnd();
            }
            {
                JLabel lblSettings = new JLabel(this.title);
                this.add(lblSettings, BorderLayout.NORTH);
            }
            JPanel panelProperty = new JPanel();
            this.add(panelProperty, BorderLayout.CENTER);
            GridBagLayout panelPropertyLayout = new GridBagLayout();
            panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
            panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
            panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
            panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
            panelProperty.setLayout(panelPropertyLayout);
            panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

            // 設定パネル枠
            EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
            Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
            panelProperty.setBorder(borderKeyword);

            Dimension textSize = new Dimension(50, 20);
            // 測定ステートメント:関数名
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.mesurementfuncname")); //測定関数名
                panelProperty.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.eprofFunctionname = new JTextField(20);
                this.eprofFunctionname.setMinimumSize(textSize);
                this.eprofFunctionname.setPreferredSize(textSize);
                panelProperty.add(this.eprofFunctionname, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                this.eprofFunctionname.setText(functionname);
            }
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.statement")); //測定ステートメント
                panelProperty.add(lblName, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.eprofMeasureStatement = new JTextArea();
                this.eprofMeasureStatement.setBorder(new EtchedBorder(EtchedBorder.LOWERED));
                //this.eprofMeasureStatement.setMinimumSize(textSize);
                //this.eprofMeasureStatement.setPreferredSize(textSize);
                panelProperty.add(this.eprofMeasureStatement, new GridBagConstraints(2, 1, 1, 3, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                this.eprofMeasureStatement.setText(statement);
            }
            {
            	JLabel label = new JLabel(Message.getString("settingprofilerdialog.discription.macros"));
                panelProperty.add(label, new GridBagConstraints(2, 4, 3, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }

        /**
         * プロパティ設定を更新する
         */
        @Override
        public void updateProperties() {
            if (this.type == PANEL_TYPE.EPROF_STATEMENT_START) {
                // 測定区間ステートメント:開始関数名
                this.properties.setEprofFunctionStart(this.eprofFunctionname.getText());
                // 測定区間ステートメント:開始ステートメント
                this.properties.setEprofStatementStart(this.eprofMeasureStatement.getText());
            } else if (this.type == PANEL_TYPE.EPROF_STATEMENT_END) {
                // 測定区間ステートメント:終了関数名
                this.properties.setEprofFunctionEnd(this.eprofFunctionname.getText());
                // 測定区間ステートメント:終了ステートメント
                this.properties.setEprofStatementEnd(this.eprofMeasureStatement.getText());
            }
        }

        @Override
        public String toString() {
            return this.title;
        }
    }

    /**
     * コスト情報表示設定パネル
     */
    private class ProfilerViewPanel extends javax.swing.JPanel implements ProfilerPropertiesPanel, ActionListener {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;
        /** プロファイラ設定 */
        private ProfilerProperties properties;
        /** パネルタイプ */
        @SuppressWarnings("unused")
        private PANEL_TYPE type;
        /** コスト情報表示最大数 */
        private JTextField maxCount;
        /** コスト情報表示色ボタン:手続 */
        private JColorButton btnColorProcedure;
        /** コスト情報表示色ボタン:ループ */
        private JColorButton btnColorLoop;
        /** コスト情報表示色ボタン:ライン */
        private JColorButton btnColorLine;
        /** タイトル、ノード表示 */
        private final String title = Message.getString("settingprofilerdialog.label.profilerview.title"); //コスト情報表示

        /**
         * コンストラクタ
         * @param properties		プロファイラ設定
         * @param type				パネルタイプ
         */
        public ProfilerViewPanel(ProfilerProperties properties, PANEL_TYPE type) {
            this.properties = properties;
            this.type = type;
            initGUI();
        }


        /**
         * GUI初期化を行う。
         */
        private void initGUI() {

            BorderLayout panelSettingsLayout = new BorderLayout();
            this.setLayout(panelSettingsLayout);
            {
                JLabel lblSettings = new JLabel();
                lblSettings.setText(this.title);
                this.add(lblSettings, BorderLayout.NORTH);
            }
            JPanel panelProperty = new JPanel();
            this.add(panelProperty, BorderLayout.CENTER);
            GridBagLayout panelPropertyLayout = new GridBagLayout();
            panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
            panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 7};
            panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
            panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 1};
            panelProperty.setLayout(panelPropertyLayout);
            panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

            // 設定パネル枠
            EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
            Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
            panelProperty.setBorder(borderKeyword);

            Dimension textSize = new Dimension(50, 20);
            // プロパティ名
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.maxlinenumber")); //テーブル表示最大行数
                panelProperty.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.maxCount = new JTextField(5);
                this.maxCount.setMinimumSize(textSize);
                this.maxCount.setPreferredSize(textSize);
                panelProperty.add(this.maxCount, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.maxCount.setText(String.valueOf(this.properties.getCostinfoMaxCount()));
            }
            {
                JLabel lblName = new JLabel(
                		Message.getString("settingprofilerdialog.label.costinfocolor") + //コスト情報表示色
                		":" +
                		Message.getString("profileinfo_type.enum.procedure")); //手続
                panelProperty.add(lblName, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnColorProcedure = new JColorButton();
                panelProperty.add(this.btnColorProcedure, new GridBagConstraints(2, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnColorProcedure.addActionListener(this);
                this.btnColorProcedure.setColor(this.properties.getCostinfoBarcolorProcedure());
            }
            {
                JLabel lblName = new JLabel(
                		Message.getString("settingprofilerdialog.label.costinfocolor") + //コスト情報表示色
                		":" +
                		Message.getString("profileinfo_type.enum.loop")); //ループ
                panelProperty.add(lblName, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnColorLoop = new JColorButton();
                panelProperty.add(this.btnColorLoop, new GridBagConstraints(2, 2, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnColorLoop.addActionListener(this);
                this.btnColorLoop.setColor(this.properties.getCostinfoBarcolorLoop());
            }
            {
                JLabel lblName = new JLabel(
                		Message.getString("settingprofilerdialog.label.costinfocolor") + //コスト情報表示色
                		":" +
                		Message.getString("profileinfo_type.enum.line")); //ライン
                panelProperty.add(lblName, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnColorLine = new JColorButton();
                panelProperty.add(this.btnColorLine, new GridBagConstraints(2, 3, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnColorLine.addActionListener(this);
                this.btnColorLine.setColor(this.properties.getCostinfoBarcolorLine());
            }
        }


        /**
         * プロパティ設定を更新する
         */
        @Override
        public void updateProperties() {
            // テーブル表示最大行数
            this.properties.setCostinfoMaxCount(Integer.parseInt(this.maxCount.getText()));
            // コスト情報表示色:手続
            this.properties.setCostinfoBarcolorProcedure(this.btnColorProcedure.getColor());
            // コスト情報表示色:ループ
            this.properties.setCostinfoBarcolorLoop(this.btnColorLoop.getColor());
            // コスト情報表示色:ライン
            this.properties.setCostinfoBarcolorLine(this.btnColorLine.getColor());
            // バーグラフの色設定
            PROFILERINFO_TYPE.setProfilerProperties(this.properties);
        }

        @Override
        public String toString() {
            return this.title;
        }


        /**
         * 色選択ダイアログを表示する
         * @param event		イベント情報
         */
        @Override
        public void actionPerformed(ActionEvent event) {

            // コスト情報表示色
            if (event.getSource() instanceof JColorButton) {
                JColorButton button = (JColorButton) event.getSource();
                // 色選択ダイアログ
                Color color = JColorChooser.showDialog(this,
                		Message.getString("settingprofilerdialog.colorchooserdialog.title"), //色の選択
                		button.getColor());
                if(color != null){
                    // ボタンにカラーを設定する
                    button.setColor(color);
                }

                return;
            }
        }
    }


    /**
     * コストルーラ表示設定パネル
     */
    private class ProfilerRulerPanel extends javax.swing.JPanel implements ProfilerPropertiesPanel, ActionListener, ChangeListener {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;
        /** プロファイラ設定 */
        private ProfilerProperties properties;
        /** パネルタイプ */
        @SuppressWarnings("unused")
        private PANEL_TYPE type;
        /** コストルーラ:最小色 */
        private JColorButton btnRulerMin;
        /** コストルーラ:最大色 */
        private JColorButton btnRulerMax;
        /** コストルーラ:コード枠色 */
        private JColorButton btnBorder;
        /** コストルーラ:コード背景色 */
        private JColorButton btnBackcolor;
        /** コストルーラ:コード背景色:透過色 */
        private JSlider sliderAlpha;
        /** タイトル、ノード表示 */
        private final String title = Message.getString("settingprofilerdialog.label.costruler"); //コストルーラ表示

        /**
         * コンストラクタ
         * @param properties		プロファイラ設定
         * @param type				パネルタイプ
         */
        public ProfilerRulerPanel(ProfilerProperties properties, PANEL_TYPE type) {
            this.properties = properties;
            this.type = type;
            initGUI();
        }


        /**
         * GUI初期化を行う。
         */
        private void initGUI() {

            BorderLayout panelSettingsLayout = new BorderLayout();
            this.setLayout(panelSettingsLayout);
            {
                JLabel lblSettings = new JLabel();
                lblSettings.setText(this.title);
                this.add(lblSettings, BorderLayout.NORTH);
            }
            JPanel panelProperty = new JPanel();
            this.add(panelProperty, BorderLayout.CENTER);
            GridBagLayout panelPropertyLayout = new GridBagLayout();
            panelPropertyLayout.columnWidths = new int[] {120, 7, 40, 120, 7};
            panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 7};
            panelPropertyLayout.columnWeights = new double[] {0, 0, 0, 1.0, 0};
            panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 1};
            panelProperty.setLayout(panelPropertyLayout);
            panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

            // 設定パネル枠
            EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
            Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
            panelProperty.setBorder(borderKeyword);

            // コード表示枠色
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.framecolor")); //KEY32=コード表示枠色
                panelProperty.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnBorder = new JColorButton();
                panelProperty.add(this.btnBorder, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnBorder.setColor(this.properties.getRulerPanelBorderColor());
                this.btnBorder.addActionListener(this);
            }
            // コード表示背景色
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.background")); //KEY33=コード表示背景色
                panelProperty.add(lblName, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnBackcolor = new JColorButton();
                panelProperty.add(this.btnBackcolor, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnBackcolor.setColor(this.properties.getRulerPanelBackColor());
                this.btnBackcolor.addActionListener(this);
            }
            {
            	JPanel panel = new JPanel();
            	panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
            	JLabel label = new JLabel(Message.getString("settingprofilerdialog.label.transparency")); //KEY36=透明度
            	panel.add(label);
            	this.sliderAlpha = new JSlider(0, 255);
            	panel.add(this.sliderAlpha);
                panelProperty.add(panel, new GridBagConstraints(3, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 10, 0, 0), 0, 0));
                this.sliderAlpha.setPreferredSize(new Dimension(120, 24));
                this.sliderAlpha.addChangeListener(this);
                Color back = this.properties.getRulerPanelBackColor();
                this.sliderAlpha.setValue(back.getAlpha());
            }
            // コストルーラ:最小色
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.costminimun")); //KEY34=コストルーラ:最小色
                panelProperty.add(lblName, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnRulerMin = new JColorButton();
                panelProperty.add(this.btnRulerMin, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnRulerMin.setColor(this.properties.getRulerColorMin());
                this.btnRulerMin.addActionListener(this);
            }
            // コストルーラ:最大色
            {
                JLabel lblName = new JLabel(Message.getString("settingprofilerdialog.label.costmaximun")); //KEY34=コストルーラ:最大色
                panelProperty.add(lblName, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            {
                this.btnRulerMax = new JColorButton();
                panelProperty.add(this.btnRulerMax, new GridBagConstraints(2, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                this.btnRulerMax.setColor(this.properties.getRulerColorMax());
                this.btnRulerMax.addActionListener(this);
            }
            // グラデーションパネル
            {
                // グラデーション色
                JPanel panelGradient = new JPanel() {
                    /**
                     * シリアル番号
                     */
                    private static final long serialVersionUID = 1L;

                    /**
                     * プロファイラルーラのグラデーションを描画する.
                     */
                    @Override
                    protected void paintComponent(Graphics g) {
                        super.paintComponent(g);
                        Graphics2D g2 = (Graphics2D) g;
                        // 描画領域
                        Rectangle rectDraw = this.getBounds();
                        int step = 256;
                        float width = (float) rectDraw.width / (float) step;
                        Color minColor = ProfilerRulerPanel.this.btnRulerMin.getColor();
                        Color maxColor = ProfilerRulerPanel.this.btnRulerMax.getColor();
                        for (int i = 0; i < step; i++) {
                            float pos_x = (float) i * width;
                            Rectangle2D.Float rect2d = new Rectangle2D.Float(pos_x, 0, width, 40);
                            float ratio = (float) i / (float) step;
                            Color valueColor = SwingUtils.getGradientHsbColor(ratio, minColor, maxColor);
                            g2.setColor(valueColor);
                            g2.fill(rect2d);
                        }
                    }
                };
                panelGradient.setPreferredSize(new Dimension(60, 25));
                panelGradient.setMinimumSize(new Dimension(30, 25));
                panelGradient.setBorder(new LineBorder(Color.BLACK, 1));
                panelProperty.add(panelGradient, new GridBagConstraints(3, 2, 1, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 10, 0, 0), 0, 0));
            }
        }


        /**
         * プロパティ設定を更新する
         */
        @Override
        public void updateProperties() {
            // コード表示枠色
            this.properties.setRulerPanelBorderColor(this.btnBorder.getColor());
            // コード表示背景色
            Color backcolor = this.btnBackcolor.getColor();
            if (backcolor != null) {
            	Color value = new Color(backcolor.getRed(), backcolor.getGreen(), backcolor.getBlue(), this.sliderAlpha.getValue());
                this.properties.setRulerPanelBackColor(value);
            }
            // コストルーラ:最小色
            this.properties.setRulerColorMin(this.btnRulerMin.getColor());
            // コストルーラ:最大色
            this.properties.setRulerColorMax(this.btnRulerMax.getColor());
        }

        @Override
        public String toString() {
            return this.title;
        }


        /**
         * 色選択ダイアログを表示する
         * @param event		イベント情報
         */
        @Override
        public void actionPerformed(ActionEvent event) {

            // コスト情報表示色
            if (event.getSource() instanceof JColorButton) {
                JColorButton button = (JColorButton) event.getSource();
                // 色選択ダイアログ
                Color color = JColorChooser.showDialog(this,
                		Message.getString("settingprofilerdialog.colorchooserdialog.title"), //色の選択
                		button.getColor());
                if (color == null) {
                	return;
                }

            	// ボタンにカラーを設定する
                if (event.getSource() == this.btnBackcolor) {
                	// 背景色の場合Alphaを設定する
                	Color back = new Color(color.getRed(), color.getGreen(), color.getBlue(), this.sliderAlpha.getValue());
                    button.setColor(back);
                }
                else {
                    button.setColor(color);
                }
                this.repaint();
                return;
            }
        }

        /**
         * 背景透過色のスライダ変更イベント
         * @param event    イベント情報
         */
		@Override
		public void stateChanged(ChangeEvent event) {
			if (event.getSource() == this.sliderAlpha) {
				Color color = this.btnBackcolor.getColor();
				Color back = new Color(color.getRed(), color.getGreen(), color.getBlue(), this.sliderAlpha.getValue());
				this.btnBackcolor.setColor(back);
			}
		}
    }

    /**
     * ダイアログを表示する。
     * @return    ダイアログの閉じた時のボタン種別
     */
    public int showDialog() {

        // 親フレーム中央に表示する。
        this.setLocationRelativeTo(this.getOwner());

        // ダイアログ表示
        this.setVisible(true);

        return this.result;
    }

    /**
     * ボタンクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 適用・登録
        if (event.getSource() == this.btnOk
            || event.getSource() == this.btnApply) {
            // コーディング規約設定を更新する。
            for(int i=0;i<this.treeProperties.getRowCount();i++) {
                TreePath path = this.treeProperties.getPathForRow(i);
                DefaultMutableTreeNode node = (DefaultMutableTreeNode)path.getLastPathComponent();
                if (node == null) continue;
                if (node.getUserObject() instanceof ProfilerPropertiesPanel) {
                    ((ProfilerPropertiesPanel)node.getUserObject()).updateProperties();
                }
            }
            // 変更イベントを発生
            this.properities.firePropertyChange();

            // ダイアログを閉じる。
            if (event.getSource() == this.btnOk) {
                this.result = Constant.OK_DIALOG;
                dispose();
            }
            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
    }

    /**
     * 設定項目ツリーの選択変更イベント
     * @param event		イベント情報
     */
    @Override
    public void valueChanged(TreeSelectionEvent event) {

        DefaultMutableTreeNode node = (DefaultMutableTreeNode)this.treeProperties.getLastSelectedPathComponent();
        if (node.getUserObject() == null) return;
        if (!(node.getUserObject() instanceof JPanel)) return;
        JPanel panel = (JPanel)node.getUserObject();
        this.panelSettings.removeAll();
        this.panelSettings.add(panel, BorderLayout.CENTER);
        this.panelContent.updateUI();
    }


}

