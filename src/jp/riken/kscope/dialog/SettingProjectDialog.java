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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.properties.ProjectProperties;
//import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロジェクト設定ダイアログ
 * @author RIKEN
 */
public class SettingProjectDialog extends javax.swing.JDialog implements ActionListener, ListSelectionListener  {

    /** シリアル番号 */
	private static final long serialVersionUID = 1L;
	
	private static boolean debug = (System.getenv("DEBUG")!= null);
	private static boolean debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));

    /** 最終アクセスフォルダ */
    private String lastAccessFolder;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** 登録ボタン */
    private JButton btnReg;
    /** プロジェクト設定リスト */
    private JTable tblProperties;
    /** プロジェクト設定リストデータ */
    private DefaultTableModel modelProperties;
    /** プロジェクト設定パネル */
    private JPanel panelProperty;
    
    //private DefaultComboBoxModel<String> list_model;
    private JButton manage_settings_files;
    private JComboBox<String> settings_list;

	/** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** プロジェクトプロパティ */
    ProjectProperties properties;

    
    //TODO: Copy messages to message_ja.properties
    /** 列名 */
    private final String[] COLUMN_HEADER = {
    		"",
    	Message.getString("settingprojectdialog.column_header.key"), //キー
    	Message.getString("settingprojectdialog.column_header.type"), //タイプ
    	Message.getString("settingprogramdialog.label.name"), //名前
    	Message.getString("settingprojectdialog.column_header.value"), //値
    	Message.getString("settingprojectdialog.column_header.message"),  //メッセージ
    	Message.getString("settingprojectdialog.column_header.clo"),  //command line option
    	Message.getString("settingprojectdialog.column_header.order")  //order
    };

    /** 選択ボタン */
    private JButton btnSelect;
    /** 値入力テキストフィールド */
    private JTextField txtValue;

    /** 選択プロパティ */
    private ProjectPropertyValue selectedvalue;

	/**
     * コンストラクタ
     * @param owner		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
	public SettingProjectDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
	}

	/**
     * コンストラクタ
     * @param frame		親フレーム
     */
	public SettingProjectDialog(Frame frame) {
		super(frame);
		initGUI();
	}

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities		プロジェクト設定プロパティ
     * @wbp.parser.constructor
     */
    public SettingProjectDialog(Frame frame, boolean modal, ProjectProperties properities) {
        super(frame, modal);
        initGUI();
        setProjectProperties(properities);
    }

    /**
     * プロジェクト設定を設定する。
     * @param properties		ソース設定プロパティ
     */
    public void setProjectProperties(ProjectProperties properties) {

        ProjectPropertyValue[] values = properties.getPropertyValues();

        // テーブルに追加する
        for (ProjectPropertyValue value : values) {
            // "PropertyValue", "キー", "タイプ", "名前", "値", "メッセージ"
            Object[] rowData = new Object[8];
            rowData[0] = value;
            rowData[1] = value.getKey();
            rowData[2] = value.getType();
            rowData[3] = Message.getString(value.getName());
            rowData[4] = value.getValue();
            rowData[5] = Message.getString(value.getMessage());
            rowData[6] = value.getCommandlineOption();
            rowData[7] = value.getOrder();
            if (debug) {
            	System.out.println("Add row: " 
            			+ rowData[1]+" : "
            			+ rowData[2]+" : "
            			+ rowData[3]+" : "
            			+ rowData[4]+" : "
            			+ rowData[5]+" : "
            			+ rowData[6]+" : "
            			+ rowData[7]);
            }
            this.modelProperties.addRow(rowData);
        }

        this.properties = properties;

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
                    btnOk.setText(Message.getString("dialog.common.button.ok"));//OK
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                    panelButtons.add(btnOk);
                }
                {
                    btnCancel = new JButton();
                    btnCancel.setText(Message.getString("dialog.common.button.cancel"));//キャンセル
                    btnCancel.setPreferredSize(buttonSize);
                    btnCancel.addActionListener(this);
                    btnCancel.setMargin(new Insets(5, 5, 5, 5));
                    panelButtons.add(btnCancel);
                }
            }

            // コンテンツパネル
            {
                JPanel panelContent = new JPanel();
                BorderLayout panelContentLayout = new BorderLayout();
                getContentPane().add(panelContent, BorderLayout.CENTER);
                Border border = new EmptyBorder(7,7,0,7);
                panelContent.setBorder(border);
                panelContent.setLayout(panelContentLayout);

                // プロパティリスト
                {
                    JPanel panelList = new JPanel();
                    panelList.setPreferredSize(new Dimension(300, 250));
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.WEST);
                    {
                        JLabel lblList = new JLabel();
                        panelList.add(lblList, BorderLayout.NORTH);
                        lblList.setText(Message.getString("settingprojectdialog.label.setupprojectlist"));// Project settings / プロジェクト設定リスト
                    }
                    {
                        JScrollPane scrollList = new JScrollPane();
                        scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelList.add(scrollList, BorderLayout.CENTER);
                        {
                            modelProperties = new DefaultTableModel();
                            modelProperties.setColumnCount(COLUMN_HEADER.length);
                            // ヘッダー列名
                            String[] columns = COLUMN_HEADER;
                            modelProperties.setColumnIdentifiers(columns);
                            tblProperties = new JTable();
                            scrollList.setViewportView(tblProperties);
                            tblProperties.setModel(modelProperties);
                            tblProperties.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                            tblProperties.getSelectionModel().addListSelectionListener(this);
                            tblProperties.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
                            tblProperties.setDefaultRenderer(Object.class, new PropertiesTableRenderer());
                            tblProperties.setColumnSelectionAllowed(false);
                            tblProperties.setDefaultEditor(Object.class, null);

                            // 列幅設定
                            // 1列目:PropertyValue:非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(0);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 2列目:キー:非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(1);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 3列目:タイプ:非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(2);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 4列目:名前
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(3);
                                col.setResizable(true);
                                col.setMinWidth(130);
                            }
                            // 5列目:値
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(4);
                                col.setResizable(true);
                                col.setMinWidth(160);
                            }
                            // 6列目:メッセージ: 非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(5);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 7列目:メッセージ: 非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(6);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                        }
                    }
                }
                // 設定パネル
                {
                    JPanel panelSettings = new JPanel();
                    BorderLayout panelSettingsLayout = new BorderLayout();
                    panelContent.add(panelSettings, BorderLayout.CENTER);
                    Border borderSettings = new EmptyBorder(0,7,0,0);
                    panelSettings.setBorder(borderSettings);
                    panelSettings.setLayout(panelSettingsLayout);
                    {
                        JLabel lblSettings = new JLabel();
                        lblSettings.setText(Message.getString("mainmenu.project.config")); //設定 / Configuration
                        panelSettings.add(lblSettings, BorderLayout.NORTH);
                    }
                    this.panelProperty = new JPanel();
                    GridBagLayout panelPropertyLayout = new GridBagLayout();
                    panelPropertyLayout.columnWidths = new int[] {80, 100, 7, 7};
                    panelPropertyLayout.rowHeights = new int[] {7, 30, 30, 7, 7};
                    panelPropertyLayout.columnWeights = new double[] {0, 1.0, 0, 0};
                    panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 1};
                    panelSettings.add(this.panelProperty, BorderLayout.CENTER);
                    this.panelProperty.setLayout(panelPropertyLayout);
                    this.panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

                    // 設定パネル枠
                    EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
                    Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
                    panelProperty.setBorder(borderKeyword);

                    // プロパティ名
                    {
                        JLabel lblName = new JLabel();
                        this.panelProperty.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblName.setText(Message.getString("settingprogramdialog.label.name")); //名前
                    }
                    {
                        JLabel txtName = new JLabel();
                        this.panelProperty.add(txtName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }

                    // キーワード追加削除ボタンパネル
                    JPanel panelAddButtons = new JPanel();
                    FlowLayout panelAddButtonsLayout = new FlowLayout(FlowLayout.RIGHT);
                    Border borderAddButtons = new EmptyBorder(0,7,0,0);
                    panelAddButtons.setBorder(borderAddButtons);
                    panelAddButtons.setLayout(panelAddButtonsLayout);
                    panelSettings.add(panelAddButtons, BorderLayout.SOUTH);

                    java.awt.Dimension minSize = new java.awt.Dimension(60, 22);
                    Insets minInsets = new Insets(3, 3, 3, 3);
                    {
                        btnReg = new JButton();
                        panelAddButtons.add(btnReg);
                        btnReg.setText(Message.getString("dialog.common.button.update")); //更新
                        btnReg.setPreferredSize(minSize);
                        btnReg.setMargin(minInsets);
                        btnReg.addActionListener(this);
                    }
                }
            }
            setTitle(Message.getString("projectsettingprojectaction.setup.status")); //プロジェクト設定
            setSize(640, 300);

        } catch (Exception e) {
            e.printStackTrace();
        }
	}

    /**
     * ボタンクリックイベント
     * @param event		イベント情報
     */
	@Override
	public void actionPerformed(ActionEvent event) {
		if (debug_l2) System.out.println("actionPerformed() of FileProjectNewDialog started");
        // 登録
        if (event.getSource() == this.btnOk) {
            this.result = Constant.OK_DIALOG;

            // 変更内容をソースプロパティに更新する。
            setProperties();

            // 変更イベントを発生
            this.properties.firePropertyChange();

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 適用
        if (event.getSource() == this.btnApply) {
            this.result = Constant.OK_DIALOG;

            // 変更内容をソースプロパティに更新する。
            setProperties();

            // 変更イベントを発生
            this.properties.firePropertyChange();

            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 選択
        else if (event.getSource() == this.btnSelect) {
        	// カレントフォルダ
            String currentFolder = this.txtValue.getText();
            if (currentFolder == null || currentFolder.isEmpty()) {
                if (this.lastAccessFolder != null) {
                    currentFolder = this.lastAccessFolder;
                }
                else {
                    currentFolder = System.getProperty("user.dir");
                }
            }
        	String dlgTitle = "";
        	if ("file".equalsIgnoreCase(selectedvalue.getType())) {
        		File[] selected = null;
        		dlgTitle = Message.getString("informationdialog.selectfiledialog.title"); //ファイルの選択
                /*if (selectedvalue != null
                	&& ProjectProperties.MAKEFILE_PATH.equalsIgnoreCase(selectedvalue.getKey())) {
            		// Makefile選択ダイアログを表示する。
                    selected = SwingUtils.showOpenMakefileDialog(this, dlgTitle, currentFolder, false);
                }
                else*/ 
        		{
	        		// ファイル選択ダイアログを表示する。
	                selected = SwingUtils.showOpenFileDialog(this, dlgTitle, currentFolder, null, true);
                }
                if (selected == null || selected.length <= 0) return;
                txtValue.setText(selected[0].toString());
        	}
        	else if ("folder".equalsIgnoreCase(selectedvalue.getType())) {
        		dlgTitle = Message.getString("settingprojectdialog.selectfolderdialog.title"); //フォルダの選択
        		// フォルダ選択ダイアログを表示する。
                File[] selected = SwingUtils.showOpenFolderDialog(this, dlgTitle, currentFolder, false);
                if (selected == null || selected.length <= 0) return;
                txtValue.setText(selected[0].toString());
        	}

            return;
        }
        // 更新
        else if (event.getSource() == this.btnReg ) {
            // 設定値をテーブルに設定する
            // 選択行を取得する。
            int selectedrow = this.tblProperties.getSelectedRow();
            if (selectedrow < 0) return;
            if (selectedvalue == null) return;

            int col = 4;

            this.modelProperties.setValueAt(txtValue.getText(), selectedrow, col);

        }
        else if (event.getSource() == this.manage_settings_files) {
        	if (debug) {
        		System.out.println("Button manage_settings_files pressed");
        	}
        	this.setModal(false);
        	ManageSettingsFilesDialog manage_files_dialog = new ManageSettingsFilesDialog();
        	manage_files_dialog.showDialog();   
        	refreshSettingsList();
        }
        else if (event.getSource() == this.settings_list) {
        	if (debug_l2) {
        		System.out.println("Action on jComboBox settings_list");
        	}
        	String selection = settings_list.getItemAt(settings_list.getSelectedIndex());
        	if (debug_l2) System.out.println("\tSelection="+ selection);
        	
        	// Should change ProjectProperties only after "Apply" or "OK" buttons pressed.
        	//properties.setSettingsFile(selection);
        	//if (debug) System.out.println("\tProject settings file set to "+ properties.getSettingsFile());
        	
        	// Update GUI table
        	int selectedrow = this.tblProperties.getSelectedRow();
            if (selectedrow < 0) return;
            if (selectedvalue == null) return;
            int col = 4;
            this.modelProperties.setValueAt(selection, selectedrow, col);
        }
        if (debug_l2) System.out.println("actionPerformed() of SettingsProjectDialog exited");
	}

    /**
     * 最終アクセスフォルダを設定する
     * @param folder		最終アクセスフォルダ
     */
    public void setLastAccessFolder(String folder) {
        if (folder == null) return;
        this.lastAccessFolder = folder;
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
     * 文字列設定パネルを設定する
     * @param value		文字列設定プロパティ
     */
    private void setTextPanel(ProjectPropertyValue value) {

        if (!("text".equalsIgnoreCase(value.getType()))) return;

        // 設定値
        String str = value.getValue();
        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));//名前
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(value.getName());
        }

        // 値
        {
            JLabel lblValue = new JLabel();
            this.panelProperty.add(lblValue, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblValue.setText(Message.getString("settingprojectdialog.column_header.value"));//値
        }
        {
            txtValue = new JTextField();
            this.panelProperty.add(txtValue, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            txtValue.setText(str);
        }
        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(value.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 2, 2, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }
    }

    /**
     * ファイル設定パネルを設定する
     * @param value		ファイル設定プロパティ
     */
    private void setFilePanel(ProjectPropertyValue value) {

        if (!("file".equalsIgnoreCase(value.getType())) &&
        		!("folder".equalsIgnoreCase(value.getType()))) return;

        String valueLabel = "";
        if ("file".equalsIgnoreCase(value.getType())) {
        	valueLabel = Message.getString("settingprojectdialog.label.file-colon"); //ファイル
        }
        else if ("folder".equalsIgnoreCase(value.getType())) {
        	valueLabel = Message.getString("settingprojectdialog.label.folder-colon"); //フォルダ
        }

        // 設定ファイル
        String filename = value.getValue();
        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));//名前
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(value.getName());
        }

        // ファイル／フォルダ
        {
            JLabel lblFile = new JLabel();
            this.panelProperty.add(lblFile, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblFile.setText(valueLabel);
        }
        {
            txtValue = new JTextField();
            this.panelProperty.add(txtValue, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            txtValue.setText(filename);
        }
        {
            Dimension sizeButton = new Dimension(46, 22);
            btnSelect = new JButton();
            this.panelProperty.add(btnSelect, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            btnSelect.setText(Message.getString("dialog.common.button.refer"));//参照
            btnSelect.setMargin(new Insets(5, 0, 5, 0));
            btnSelect.addActionListener(this);
            btnSelect.setMaximumSize(sizeButton);
            btnSelect.setMinimumSize(sizeButton);
            btnSelect.setPreferredSize(sizeButton);
//            btnFont.setSize(szieButton);
        }

        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(value.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 2, 2, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }
    }
    
    /**
     * Drop-down list for selecting remote settings files
     * @param ppvalue
     */
    private void setFixedFilePanel(ProjectPropertyValue ppvalue) {
    	if (debug) System.out.println("Value="+ppvalue.getValue());
    	if (!("fixed-file".equalsIgnoreCase(ppvalue.getType()))) return;
    	
        String valueLabel = "";
        valueLabel = Message.getString("settingprojectdialog.label.file-colon"); //ファイル
        
        // 設定ファイル
        String filename = ppvalue.getValue();
        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));// Name
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(ppvalue.getName());
        }

        // ファイル／フォルダ
        {
            JLabel lblFile = new JLabel();
            this.panelProperty.add(lblFile, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblFile.setText(valueLabel);
        }
        /*{
            txtValue = new JTextField();
            this.panelProperty.add(txtValue, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            txtValue.setText(filename);
        }*/
        {        	
        	String[] selections = ProjectProperties.getRemoteSettings(); 
        	if (selections.length < 1) {
        		System.out.println("No remote connection settings files found in "+ ProjectProperties.REMOTE_SETTINGS_DIR);    		
        	} else {    		
        		System.out.println("Have remote connection settings in "+ ProjectProperties.REMOTE_SETTINGS_DIR);
            	//list_model = new DefaultComboBoxModel<String>(selections);
        		settings_list = new JComboBox<String>(selections);
            	settings_list.setEnabled(true);
            	if (debug) {
            		System.out.println("Check objects comparison in JComboBox");
            		for (int i=0; i < settings_list.getItemCount(); i++) {
            			String item = settings_list.getItemAt(i);
            			if (item.equalsIgnoreCase(filename)) {
            				System.out.println("Match item "+ i + " value: "+ filename);
            			}
            		}
            	}
            	settings_list.setSelectedItem(ppvalue.getValue());
            	manage_settings_files = new JButton(Message.getString("fileprojectnewdialog.kindpanel.button.manage_settings_files"));
            	manage_settings_files.setEnabled(true);
            	this.panelProperty.add(settings_list, new GridBagConstraints(1, 1, 3, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            	this.panelProperty.add(manage_settings_files, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            	settings_list.addActionListener(this);
            	manage_settings_files.addActionListener(this);
        	}
        		
        }       

        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(ppvalue.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 3, 3, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }
    }
    
    
    
    

    /**
     * 固定文字列表示パネルを設定する
     * @param value		固定文字列設定プロパティ
     */
    private void setFixedTextPanel(ProjectPropertyValue value) {

        if (!"fixed-text".equalsIgnoreCase(value.getType())) return;

        // 設定値
        String str = value.getValue();
        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));//名前
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(value.getName());
        }

        // 値
        {
            JLabel lblValue = new JLabel();
            this.panelProperty.add(lblValue, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblValue.setText(Message.getString("settingprojectdialog.column_header.value"));//値
        }
        {
        	JLabel txtValue = new JLabel();
            this.panelProperty.add(txtValue, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            txtValue.setText(str);
            txtValue.setBorder(BorderFactory.createEtchedBorder());
            //txtValue.setEditable(false);
        }
        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(value.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 2, 2, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }
    }

    /**
     * メッセージラベルコンポーネントを作成する
     * @param  message   表示メッセージ
     * @return			メッセージラベルコンポーネント
     */
    private JComponent createMessageLabel(String  message) {
        if (message == null) return null;

        JTextPane lblMassage = new JTextPane();
        SimpleAttributeSet attr = new SimpleAttributeSet();
        //StyleConstants.setLineSpacing(attr, -0.2f);
        //lblMassage.setParagraphAttributes(attr, true);
        lblMassage.setForeground(UIManager.getColor("Label.foreground"));
        lblMassage.setOpaque(false);
        lblMassage.setEditable(false);
        lblMassage.setFocusable(false);
        lblMassage.setText(message);

        return lblMassage;
    }

    /**
     * プロパティ設定パネルを設定する
     * @param value		設定プロパティ
     */
    private void setPropertyPanel(ProjectPropertyValue value) {
    	if (value == null) return;

        if ("text".equalsIgnoreCase(value.getType())) {
            // テキスト設定
        	setTextPanel(value);
        	btnReg.setEnabled(true);
        }
        else if ("file".equalsIgnoreCase(value.getType()) ||
        		"folder".equalsIgnoreCase(value.getType())) {
        	// ファイル設定／フォルダ設定
        	setFilePanel(value);
        	btnReg.setEnabled(true);
        }
        else if ("fixed-text".equalsIgnoreCase(value.getType())) {
        	// テキスト表示のみ
        	setFixedTextPanel(value);
        	btnReg.setEnabled(false);
        }
        else if ("fixed-file".equalsIgnoreCase(value.getType())) {
        	// テキスト表示のみ
        	setFixedFilePanel(value);
        	btnReg.setEnabled(false);
        }

        this.panelProperty.revalidate();
        this.panelProperty.repaint();
    }

	@Override
	public void valueChanged(ListSelectionEvent event) {
		if (event.getSource() == this.tblProperties.getSelectionModel()) {
            // 選択行を取得する。
            int selectedrow = this.tblProperties.getSelectedRow();
            if (selectedrow < 0) return;
            if (debug) System.out.println("Selected row "+selectedrow+ " key="+this.modelProperties.getValueAt(selectedrow, 1)+ " type="+this.modelProperties.getValueAt(selectedrow, 2));
            
            // "PropertyValue", "キー", "タイプ", "名前", "値", "メッセージ", CLO, order
            ProjectPropertyValue value = new ProjectPropertyValue(
            			(String)this.modelProperties.getValueAt(selectedrow, 1), //key
                        (String)this.modelProperties.getValueAt(selectedrow, 2), //type
                        (String)this.modelProperties.getValueAt(selectedrow, 3), //name
                        (String)this.modelProperties.getValueAt(selectedrow, 4), //value
                        (String)this.modelProperties.getValueAt(selectedrow, 5),  //message
                        (String)this.modelProperties.getValueAt(selectedrow, 6),  //command line option
                        (int)   this.modelProperties.getValueAt(selectedrow, 7)  //order
                    );
            // 選択プロパティ
            this.selectedvalue = value;
            if (debug) System.out.println("Selected value: "+value);
            // 設定パネルに表示する
            this.setPropertyPanel(value);
        }
	}

    /**
     * プロジェクトプロパティを設定する。
     */
    private void setProperties() {

        int rows = this.modelProperties.getRowCount();

        // "PropertyValue", "キー", "タイプ", "名前", "値", "メッセージ"}
        for (int i=0; i<rows; i++) {
            ProjectPropertyValue value = (ProjectPropertyValue) this.modelProperties.getValueAt(i, 0);
            value.setValue((String)this.modelProperties.getValueAt(i, 4));
        }

    }
    
    /***
     * Refresh contents of settings_list JComboBox
     */
    private void refreshSettingsList() {
    	if (debug) System.out.println("Refireshin settings_list contents.");
    	String[] selections = ProjectProperties.getRemoteSettings();
    	this.settings_list.removeAllItems();
    	for (String s : selections) {
    		settings_list.addItem(s);
    	}
    }

    /**
     * プロパティテーブル描画クラス
     * @author RIKEN
     *
     */
    private class PropertiesTableRenderer extends DefaultTableCellRenderer {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * セルの描画コンポーネントを取得する
         * @param table			描画テーブル
         * @param value			セルデータ
         * @param isSelected	選択状態
         * @param hasFocus		フォーカス
         * @param row			行インデックス
         * @param column		列インデックス
         * @return		描画コンポーネント
         */
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus,
                int row, int column) {

            Object cellValue = value;
            if (value != null) cellValue = value.toString();
            else cellValue = "";

            super.getTableCellRendererComponent(table, cellValue, isSelected, hasFocus, row, column);

            return this;
        }
    }
}
