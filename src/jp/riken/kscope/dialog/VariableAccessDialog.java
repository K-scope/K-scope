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
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JBackgroundComboBox;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;

/**
 * 変数アクセス先設定ダイアログ
 * @author RIKEN
 */
public class VariableAccessDialog  extends javax.swing.JDialog implements ActionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** 算出ボタン */
    private JButton btnCalc;
    /** 変更ボタン */
    private JButton btnChange;
    /** すべて変更:選択 */
	private JCheckBox chkSelect;
    /** すべて変更:array */
    private JCheckBox chkArray;
    /** すべて変更:scalar */
    private JCheckBox chkScalar;
    /** すべて設定アクセス先メモリコンボボックス */
    private JBackgroundComboBox cmbAllAccess;
    /** メモリアクセス先リスト */
    private JTable tblAccess;
    /** メモリアクセス先リストデータ */
    private DefaultTableModel modelAccess;
    /** 選択ブロック */
    private IBlock[] selectedblocks;
    /** 変数アクセス先メモリ設定 */
    private VariableMemoryProperties propertiesVariable;
    /** 要求Byte/FLOP設定プロパティ設定 */
    private RequiredBFProperties propertiesMemoryband;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
	/** メモリ性能算出結果ダイアログ */
	private RequiredBFDialog nextDialog;
	/** 親ダイアログフラグ true=最初に呼び出されたダイアログ, false=他のダイアログから呼び出された */
	private boolean ownerDialog;
    /** 算出パネル */
	private JPanel panelCalculate;
	/** メモリアクセス先テーブル列ヘッダー設定 */
    private final String[] COLUMN_HEADER = { "",
    		                                 Message.getString("variableaccessdialog.header.variable"),  // 変数
    		                                 Message.getString("settingprojectdialog.column_header.type"),   // タイプ
    		                                 Message.getString("variableaccessdialog.header.datatype"),   // データ型
    		                                 Message.getString("settingrequiredbfdialog.label.access"),  // アクセス先
    										 ""
                                             };
    /** メモリアクセス先テーブル列最小幅 */
    private final int[] COLUMN_MINWIDTH = {0,  80, 60, 240, 120, 0};
    /** メモリアクセス先テーブル列最大幅 */
    private final int[] COLUMN_MAXWIDTH = {-1, 0, 60, 0, 120, -1};
    /** メモリアクセス先テーブル列数 */
    private final int COLUMN_COUNT = 6;
    /** ダイアログ初期高さ */
    private final int DEFAULT_HEIGHT = 360;
    /** メモリアクセス先テーブル行高さ */
    private final int TABLE_ROWHEIGHT = 22;
    /** アクセス先メモリコンボボックス列 */
    private final int MEMORYACCESS_COLUMN = 4;
    /** タイプ列 */
    private final int ARRAYSCALAR_COLUMN = 2;
    /** アクセス先メモリ文字列の退避先列 */
    private final int ACCESSMEMORYTYPE_COLUMN = 5;

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public VariableAccessDialog(JFrame frame) {
        super(frame);
        nextDialog = null;
        this.ownerDialog = true;
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public VariableAccessDialog(Frame frame, boolean modal) {
        super(frame, modal);
        nextDialog = null;
        this.ownerDialog = true;
        initGUI();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
        	getContentPane().setLayout(new BorderLayout());
            // ボタンパネル
            {
                JPanel panelButtons = new JPanel();
                FlowLayout jPanel1Layout = new FlowLayout();
                jPanel1Layout.setHgap(10);
                jPanel1Layout.setVgap(10);
                panelButtons.setLayout(jPanel1Layout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                // panelButtons.setPreferredSize(new java.awt.Dimension(390, 45));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnApply = new JButton();
                    panelButtons.add(btnApply);
                    btnApply.setText(Message.getString("dialog.common.button.apply")); //適用
                    btnApply.setPreferredSize(buttonSize);
                    btnApply.addActionListener(this);
                }
                {
                	String textOk = Message.getString("dialog.common.button.ok");
                    btnOk = new JButton();
                    panelButtons.add(btnOk);
                    btnOk.setText(textOk);
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                }
                {
                    btnCancel = new JButton();
                    panelButtons.add(btnCancel);
                    btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                    btnCancel.setPreferredSize(buttonSize);
                    btnCancel.setMargin(new Insets(5, 5, 5, 5));
                    btnCancel.addActionListener(this);
                }
            }
            // コンテンツパネル
            {
                JPanel panelContent = new JPanel();
                panelContent.setLayout(new BoxLayout(panelContent, BoxLayout.Y_AXIS));
                getContentPane().add(panelContent, BorderLayout.CENTER);

                JPanel panelTable = new JPanel();
                panelContent.add(panelTable);
                GridBagLayout layoutTable = new GridBagLayout();
                layoutTable.rowWeights = new double [] {0.0, 1.0, 0.0};
                layoutTable.rowHeights = new int [] {24, 100, 7};
                layoutTable.columnWeights = new double [] {0.0, 1.0, 0.0};
                layoutTable.columnWidths = new int [] {7, 520, 7};
                panelTable.setLayout(layoutTable);
                String title = Message.getString("variableaccessdialog.table.title"); //変数アクセス先リスト
                TitledBorder insideborder = new TitledBorder(new EtchedBorder(), title);
                Border outsideBorder = new EmptyBorder(7,7,7,7);
                CompoundBorder border = new CompoundBorder(outsideBorder, insideborder);
                panelTable.setBorder(border);

                // すべて設定
                {
                    JPanel panelAll = new JPanel();
                    FlowLayout layout = new FlowLayout(FlowLayout.RIGHT);
                    layout.setHgap(10);
                    panelAll.setLayout(layout);
                    panelTable.add(panelAll, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));

                    // まとめて設定
                    String titleAll = Message.getString("variableaccessdialog.all.title"); //アクセス先一括設定
                    JLabel labelAll = new JLabel(titleAll + " : ");
                    panelAll.add(labelAll);

                    // すべて：選択
                    String select = Message.getString("settingviewdialog.label.select"); //選択
                    this.chkSelect = new JCheckBox(select, true);
                    panelAll.add(chkSelect);

                    // すべて：Array
                    this.chkArray = new JCheckBox("array");
                    panelAll.add(chkArray);
                    // すべて：Scalar
                    this.chkScalar = new JCheckBox("scalar");
                    panelAll.add(chkScalar);
                    // メモリアクセス先
                    this.cmbAllAccess = new JBackgroundComboBox();
                    panelAll.add(this.cmbAllAccess);
                    // 変更ボタン
                    btnChange = new JButton();
                    panelAll.add(btnChange);
                    btnChange.setText(Message.getString("dialog.common.button.change")); //変更ボタン
                    btnChange.setMargin(new Insets(0, 2, 2, 2));
                    btnChange.addActionListener(this);
                }
                // アクセス先変数リスト
                {

	                JScrollPane scrollList = new JScrollPane();
	                panelTable.add(scrollList, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
	                scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	                scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
	                {
	            		this.modelAccess = createVariableTableModel();
	            		this.tblAccess = createVariableTable(this.modelAccess);
	                    scrollList.setViewportView(tblAccess);
	                }
                }
                // 算出:メモリ性能算出結果ダイアログからの表示の場合は算出はなし。
                if (this.ownerDialog) {
	                panelCalculate = new JPanel();
	                panelContent.add(panelCalculate);
	                FlowLayout layout = new FlowLayout(FlowLayout.RIGHT);
	                layout.setHgap(24);
	                panelCalculate.setLayout(layout);

	                // 要求Byte/FLOP算出
	                JLabel label = new JLabel(Message.getString("mainmenu.analysis.calculate")); //要求Byte/FLOP算出 
	                panelCalculate.add(label);
	                // 算出
	                {
	                    btnCalc = new JButton();
	                    panelCalculate.add(btnCalc);
	                    btnCalc.setText(Message.getString("variableaccessdialog.calculate.button")); //算出する
	                    btnCalc.setMargin(new Insets(2, 2, 2, 2));
	                    btnCalc.addActionListener(this);
	                }
                }
            }
            // プロパティデータの更新
            updateProperties();

            setTitle(Message.getString("mainmenu.analysis.access")); //変数アクセス先設定
            this.pack();

            Dimension size = new Dimension(this.getSize().width+5, DEFAULT_HEIGHT);
            this.setSize(size);

        } catch (Exception e) {
            e.printStackTrace();
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
     * すべてのメモリアクセス先コンボボックスを作成する.
     * @return		すべてのメモリアクセス先コンボボックス
     */
    @SuppressWarnings("unused")
    private JBackgroundComboBox createAllAccessCombobox() {
//    	JComboBox cmb = new JComboBox(listmemory);
//		ComboColorRenderer renderer = new ComboColorRenderer();
//		cmb.setRenderer(renderer);
		JBackgroundComboBox cmb = new JBackgroundComboBox();
    	return cmb;
    }


	/**
	 * ボタンクリックイベント
     * @param event		イベント情報
	 */
	@Override
	public void actionPerformed(ActionEvent event) {

        // OK
        if (event.getSource() == this.btnOk) {
            this.result = Constant.OK_DIALOG;
        	// 変数にアクセス先メモリを設定する.
        	setVariableAccessMemory(false);    // アクセス先適用

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 適用
        else if (event.getSource() == this.btnApply) {
            this.result = Constant.OK_DIALOG;
        	// 変数にアクセス先メモリを設定する.
        	setVariableAccessMemory(false);    // アクセス先適用

            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 算出
        else if (event.getSource() == this.btnCalc) {
        	if (this.nextDialog != null) {
            	// 変数にアクセス先メモリを一時的に設定する.
            	setVariableAccessMemory(true);
	            // メモリ性能算出結果ダイアログを表示する。
	            this.setVisible(false);
	            this.nextDialog.setOwnerDialog(false);
	            this.nextDialog.showDialog();
	            // 再表示
	            this.setVisible(true);
        	}
            return;
        }
        // すべてのアクセス先を変更する
        else if (event.getSource() == this.btnChange) {
        	setAllAccessMemory();
        }
	}

	/**
	 * セルを描画用コンボボックス
	 * @author RIKEN
	 */
	class RendererComboBox extends JBackgroundComboBox implements TableCellRenderer {
		/** シリアル番号 */
		private static final long serialVersionUID = 1L;

		/**
		 * コンストラクタ
		 */
		public RendererComboBox() {
			super();
		}

		/**
		 * セルを描画するのに使用されるコンポーネントを返します。
		 */
		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
				boolean hasFocus, int row, int column) {

			Color selectColor = Color.white;
			ACCESSMEMORY_TYPE type = ACCESSMEMORY_TYPE.getAccessMemoryType(value.toString());
			if (type != null) {
				RequiredBF mem = VariableAccessDialog.this.propertiesMemoryband.getRequiredBF(type);
				if (mem != null) {
					selectColor = mem.getBackColor();
				}
			}
			this.removeAllItems();
			this.addItem(value.toString(), selectColor);
			setSelectedItem(value);
			return this;
		}
	}

	/**
	 * コンボボックスセルエディタ
	 * @author RIKEN
	 */
	class ComboBoxCellEditor extends DefaultCellEditor {
		/** シリアル番号 */
		private static final long serialVersionUID = 1L;

		/**
		 * コンストラクタ
		 */
		public ComboBoxCellEditor() {
			super(new JBackgroundComboBox());
		}

		/**
		 * エディタの編集コンポーネントを返します。
		 */
		@Override
		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {

			if (super.getComponent() instanceof JBackgroundComboBox) {
				JBackgroundComboBox combo = (JBackgroundComboBox)super.getComponent();
				combo.removeAllItems();
				// 選択項目
				String cellValue = (String)table.getValueAt(row, ACCESSMEMORYTYPE_COLUMN);
				ACCESSMEMORY_TYPE[] listmemory = ACCESSMEMORY_TYPE.values();
				ACCESSMEMORY_TYPE type = null;
				try {
					type = ACCESSMEMORY_TYPE.getAccessMemoryType(cellValue);
				} catch (Exception ex) {}
				for (int i=0; i<listmemory.length; i++) {
					RequiredBF mem = propertiesMemoryband.getRequiredBF(listmemory[i]);
					combo.addItem(listmemory[i].getName(), mem!=null?mem.getBackColor():null);
				}
				if (type == null) {
					combo.addItem(cellValue, null);
				}
				combo.setSelectedItem(value);
			}
			return super.getTableCellEditorComponent(table, value, isSelected, row, column);
		}
	}

	/**
	 * 変数アクセス先メモリテーブルモデルを作成する.
	 * @return   変数アクセス先メモリテーブルモデル
	 */
	private DefaultTableModel createVariableTableModel() {
		DefaultTableModel model = new DefaultTableModel();
        // ヘッダー列名
		model.setColumnCount(COLUMN_COUNT);
        String[] columns = COLUMN_HEADER;
        model.setColumnIdentifiers(columns);

        // 変数データ
        if (this.selectedblocks == null) return model;
        List<Variable> list = new ArrayList<Variable>();
        for (IBlock block : this.selectedblocks) {
        	Set<Variable> vars = block.getAllVariables();
        	if (vars != null) {
	        	list.addAll(vars);
        	}
        }
        List<Variable> listMemory = getAccessVariables(list);
        // 変数のソート
        java.util.Collections.sort(listMemory,
                new java.util.Comparator<Variable>() {
                    public int compare(Variable o1, Variable o2) {
                    	Variable src1 = (Variable) o1;
                    	Variable src2 = (Variable) o2;
                    	if (src1.getDefinition() == null) return -1;
                    	if (src2.getDefinition() == null) return -1;
                    	boolean isscalar1 = (src1.getDefinition().get_dimension_size() <= 0);
                    	boolean isscalar2 = (src2.getDefinition().get_dimension_size() <= 0);
                    	// scalar && array
                    	if (isscalar1 && !isscalar2) return -1;
                    	// array && scalar
                    	else if (!isscalar1 && isscalar2) return 1;
                    	// 変数名比較
                    	String text1 = src1.getVariableString();
                    	String text2 = src2.getVariableString();
                    	if (text1 == null) return -1;
                        return text1.compareTo(text2);
                    }
                });

        if (listMemory == null || listMemory.size() <= 0)  return model;
		Object[][] tabledata = new Object[listMemory.size()][columns.length];
		for (int row=0; row<listMemory.size(); row++) {
			Variable var = listMemory.get(row);
			if (var == null) continue;
			if (var.getDefinition() == null) continue;
			int col = 0;
			tabledata[row][col] = getVariables(list, var);  // List<Variable>
			tabledata[row][++col] = var.getVariableString();
			if (var.getDefinition().get_dimension_size() > 0) {
				tabledata[row][++col] = "array";
			}
			else {
				tabledata[row][++col] = "scalar";
			}
			tabledata[row][++col] = var.getDefinition().toString();
			// 選択メモリアクセス先
			String memText = getAccessMemoryString(list, var);
			tabledata[row][++col] = memText;     // コンボボックスセル表示用
			tabledata[row][++col] = memText;     // コンボボックスリスト追加用
		}
		model.setDataVector(tabledata, columns);
		return model;
	}


	/**
	 * 選択ブロックを取得する
	 * @return 選択ブロック
	 */
	public IBlock[] getSelectedBlocks() {
		return this.selectedblocks;
	}

	/**
	 * 選択ブロックを設定する.
	 * @param blocks 選択ブロック
	 */
	public void setSelectedblocks(IBlock[] blocks) {
		this.selectedblocks = blocks;
		this.modelAccess = createVariableTableModel();
		createVariableTable(this.modelAccess);
	}


	/**
	 * 変数アクセス先メモリテーブルを作成する.
	 * @param  model    テーブルモデル
	 * @return   変数アクセス先メモリテーブル
	 */
	private JTable createVariableTable(DefaultTableModel model) {
		if (this.tblAccess == null) {
			tblAccess = new JTable();
		}
		if (model == null) return this.tblAccess;
		this.tblAccess.setModel(model);
		this.tblAccess.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
		this.tblAccess.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		this.tblAccess.setColumnSelectionAllowed(false);
		this.tblAccess.setDefaultEditor(Object.class, null);
		this.tblAccess.getTableHeader().setReorderingAllowed(false);
		this.tblAccess.setAutoCreateRowSorter(true);

        // 列幅設定
        for (int i=0; i<this.tblAccess.getColumnModel().getColumnCount(); i++) {
            TableColumn col = this.tblAccess.getColumnModel().getColumn(i);
            // 列幅を設定する。
            if (i<COLUMN_MINWIDTH.length) {
                col.setMinWidth(COLUMN_MINWIDTH[i]);
                // 最大列幅が設定されている(>0)場合は、最大列幅を設定して固定列幅とする。
                if (COLUMN_MAXWIDTH[i] > 0) {
                    col.setMaxWidth(COLUMN_MAXWIDTH[i]);
                    col.setResizable(false);
                }
                // 最大列幅が-1で設定されている場合は、非表示列(=0)とする。
                else if (COLUMN_MAXWIDTH[i] < 0) {
                    col.setMaxWidth(0);
                    col.setResizable(false);
                }
            }
            // 列幅が設定されていない列は非表示とする。
            else {
                col.setMinWidth(0);
                col.setMaxWidth(0);
            }
        }

        // アクセス先コンボボックスの作成、表示
        TableColumn col = this.tblAccess.getColumnModel().getColumn(MEMORYACCESS_COLUMN);
        ComboBoxCellEditor editor = new ComboBoxCellEditor();
        RendererComboBox renderer = new RendererComboBox();
        col.setCellEditor(editor);
        col.setCellRenderer(renderer);
        this.tblAccess.setRowHeight(TABLE_ROWHEIGHT);

		return this.tblAccess;
	}


	/**
	 * メモリアクセス先変数リストを取得する
	 * @param listVariable		変数一覧
	 * @return			メモリアクセス先変数リスト
	 */
	private List<Variable> getAccessVariables(List<Variable> listVariable) {
        List<Variable> list = new ArrayList<Variable>();
        for (Variable var : listVariable) {
        	// 実数、配列であること
        	if (!var.isMemoryAccess()) continue;
        	if (!containsVariables(list, var)) {
        		list.add(var);
        	}
        }

		return list;
	}

	/**
	 * 変数リストに変数が含まれているかチェックする.
	 * @param list		変数リスト
	 * @param value		変数
	 * @return			true=変数リスト
	 */
	private boolean containsVariables(List<Variable> list, Variable value) {
		List<Variable> vars = getVariables(list, value);
		return (vars != null && vars.size() > 0);
	}

	/**
	 * 変数リストから変数と同じ変数リストを取得する
	 * @param listVar		変数リスト
	 * @param value		変数
	 * @return			true=変数リスト
	 */
	private List<Variable> getVariables(List<Variable> listVar, Variable value) {
		if (listVar == null || listVar.size() <= 0) return null;
		if (value == null) return null;
		if (value.getVariableString() == null) return null;
		List<Variable> listEquals = new ArrayList<Variable>();
		for (Variable var : listVar) {
			if (var == null) continue;
			// 定義が同じであること
			if (value.getDefinition() != var.getDefinition()) continue;
			if (value.equalsVariable(var)) {
				listEquals.add(var);
			}
		}
		if (listEquals.size() > 0) {
			return listEquals;
		}

		return null;
	}

	/**
	 * アクセス先メモリ文字列を取得する.
	 * @param listSelected		変数リスト
	 * @param var			変数
	 * @return		アクセス先メモリ文字列
	 */
	private String getAccessMemoryString(List<Variable> listSelected, Variable var) {
		if (var == null || var.getDefinition() == null) return ACCESSMEMORY_TYPE.getDefaultType().getName();
		ACCESSMEMORY_TYPE defaulttype = ACCESSMEMORY_TYPE.getDefaultType(var.getDefinition());

		// 選択範囲内の他のアクセス先設定リストを取得する.
		List<Variable> varsSelected = getVariables(listSelected, var);
		if (varsSelected == null || varsSelected.size() <= 0) return defaulttype.getName();
		List<ACCESSMEMORY_TYPE> listMemSelected = getAccessMemory(varsSelected);
		String buf = getAccessMemoryString(listMemSelected);
		if (buf != null && !buf.isEmpty()) {
			return buf;
		}

		// 存在しないので、設定済みリストから取得する.
		List<Variable> varsSave = getVariables(this.propertiesVariable.getListVariable(), var);
		if (varsSave == null || varsSave.size() <= 0) return defaulttype.getName();
		List<ACCESSMEMORY_TYPE> listMemSave = getAccessMemory(varsSave);
		buf = getAccessMemoryString(listMemSave);
		if (buf != null && !buf.isEmpty()) {
			return buf;
		}

		return buf;
	}

	/**
	 * アクセス先メモリ文字列を取得する.
	 * @param listMem		アクセス先メモリリスト
	 * @return		アクセス先メモリ文字列
	 */
	private String getAccessMemoryString(List<ACCESSMEMORY_TYPE> listMem) {
		if (listMem == null || listMem.size() <= 0) return null;
		if (listMem.size() == 1) {
			return listMem.get(0).getName();
		}
		// 複数のアクセス先が存在するのでカンマ区切りで略称を結合する.
		String buf = "";
		for (ACCESSMEMORY_TYPE type : listMem) {
			if (!buf.isEmpty()) buf += ",";
			buf += type.getShortname();
		}
		return buf;
	}

	/**
	 * アクセス先メモリリストを取得する.
	 * @param listVar		変数リスト
	 * @return		アクセス先メモリリスト
	 */
	private List<ACCESSMEMORY_TYPE> getAccessMemory(List<Variable> listVar) {
		if (listVar == null || listVar.size() <= 0) return null;
		List<ACCESSMEMORY_TYPE> listMem = new ArrayList<ACCESSMEMORY_TYPE>();
		for (Variable item : listVar) {
			if (item == null) continue;
			ACCESSMEMORY_TYPE type = item.getMemoryType();
			if (type == null) continue;
			if (!listMem.contains(type)) {
				listMem.add(type);
			}
		}
		if (listMem.size() <= 0) return null;
		return listMem;
	}

	/**
	 * アクセス先メモリの一括変更を行う。
	 */
	private void setAllAccessMemory() {
		// すべてに適用するアクセス先メモリ
    	Object selobj = this.cmbAllAccess.getSelectedItem();
    	String value = null;
    	if (selobj instanceof String) {
    		value = (String)selobj;
    	}
    	else if (selobj instanceof RequiredBF) {
    		value = ((RequiredBF)selobj).getName();
    	}

    	if (this.chkSelect.isSelected()) {
	    	int[] rows = this.tblAccess.getSelectedRows();
	    	for (int row : rows) {
	    		// テーブルセルの値を変更する.
	    		setTableCellValueAt(value, row, MEMORYACCESS_COLUMN);
	    	}
    	}
    	if (this.chkArray.isSelected() || this.chkScalar.isSelected()) {
	    	for (int row=0; row<this.tblAccess.getRowCount(); row++) {
	    		String type = (String)this.tblAccess.getValueAt(row, ARRAYSCALAR_COLUMN);
	    		if (this.chkArray.isSelected()) {
	    			if (this.chkArray.getText().equalsIgnoreCase(type)) {
	    	    		setTableCellValueAt(value, row, MEMORYACCESS_COLUMN);
	    				continue;
	    			}
	    		}
	    		if (this.chkScalar.isSelected()) {
	    			if (this.chkScalar.getText().equalsIgnoreCase(type)) {
	    	    		setTableCellValueAt(value, row, MEMORYACCESS_COLUMN);
	    				continue;
	    			}
	    		}
	    	}

    	}
        return;
	}

	/**
	 * テーブルセルの値を変更する.
	 * @param value		変更値
	 * @param row		行インデックス
	 * @param col		列インデックス
	 */
	private void setTableCellValueAt(String value, int row, int col) {
		Object obj = this.tblAccess.getValueAt(row, col);
		Object objEditor = this.tblAccess.getCellEditor(row, col);
		Object objRenderer = this.tblAccess.getCellRenderer(row, col);
		if (obj instanceof JComboBox) {
			((JComboBox<?>)obj).setSelectedItem(value);
		}
		else {
			this.tblAccess.setValueAt(value, row, col);
		}
		if (objEditor instanceof ComboBoxCellEditor) {
			Component cmp = ((ComboBoxCellEditor)objEditor).getComponent();
			if (cmp instanceof JComboBox) {
    			((JComboBox<?>)cmp).setSelectedItem(value);
			}
		}
		if (objRenderer instanceof JComboBox) {
			((JComboBox<?>)objRenderer).setSelectedItem(value);
		}
	}

	/**
	 * テーブルセルの値を取得する.
	 * @param row		行インデックス
	 * @param col		列インデックス
	 * @return  		テーブルセル値
	 */
	private String getTableCellValueAt(int row, int col) {
		String value = null;
		Object obj = this.tblAccess.getValueAt(row, col);
		Object objEditor = this.tblAccess.getCellEditor(row, col);
		if (obj instanceof JComboBox) {
			value = (String)((JComboBox<?>)obj).getSelectedItem();
		}
		else {
			value = (String)this.tblAccess.getValueAt(row, col);
		}
		return value;
	}


	/**
	 * 変数にアクセス先メモリを設定する.
	 * @param  temporary    一時設定フラグ : true=ソースビューへの適用は行わない.
	 */
	@SuppressWarnings("unchecked")
	private void setVariableAccessMemory(boolean temporary) {
		for (int row=0; row<this.tblAccess.getRowCount(); row++) {
    		// 変数オブジェクト
    		Variable var = null;
    		List<Variable> list = null;
    		Object obj = this.tblAccess.getValueAt(row, 0);
    		if (obj == null) continue;
    		if (obj instanceof Variable) {
    			var = (Variable)obj;
    		}
    		else if (obj instanceof List) {
    			list = (List<Variable>)obj;
    		}
    		if (var == null && list == null) continue;
    		if (list == null) {
    			list = new ArrayList<Variable>();
    			list.add(var);
    		}
    		String value = getTableCellValueAt(row, MEMORYACCESS_COLUMN);
    		ACCESSMEMORY_TYPE type = ACCESSMEMORY_TYPE.getAccessMemoryType(value);
    		if (type == null) continue;
    		for (Variable data : list) {
    			if (temporary) {
	    			// 一時設定アクセス先メモリに設定する.
	    			data.setTemporaryMemoryType(type);
    			}
    			else if (type == ACCESSMEMORY_TYPE.DEFAULT) {
    				// アクセス先メモリの設定を解除する
	    			data.clearMemoryType();
	        		// 変数アクセス先メモリプロパティに変数を追加する
	        		this.propertiesVariable.removeVariable(data);
    			}
    			else {
	        		// アクセス先メモリを設定する
	    			data.setMemoryType(type);
	    			// 一時設定アクセス先メモリをクリアする.
	    			data.setTemporaryMemoryType(null);
	        		// 変数アクセス先メモリプロパティに変数を追加する
	        		this.propertiesVariable.addVariable(data);
    			}
    		}
    	}

		if (!temporary) {
	    	// 変数アクセス先メモリプロパティの適用を行う
	    	this.propertiesVariable.firePropertyChange();
		}
	}


    /**
     * 変数アクセス先メモリプロパティを取得する
     * @return		変数アクセス先メモリプロパティ
     */
    public VariableMemoryProperties getPropertiesVariable() {
        return propertiesVariable;
    }

    /**
     * 変数アクセス先メモリプロパティを取得する
     * @param  properties		変数アクセス先メモリプロパティ
     */
    public void setPropertiesVariable(VariableMemoryProperties properties) {
        this.propertiesVariable = properties;
    }

    /**
     * メモリ性能算出結果ダイアログを設定する.
     * @param dialog		メモリ性能算出結果ダイアログ
     */
    public void setMemoryPerformanceDialog(RequiredBFDialog dialog) {
    	this.nextDialog = dialog;
    }

    /**
     * 親ダイアログフラグを設定する.
     * @param owner		親ダイアログフラグ
     */
    public void setOwnerDialog(boolean owner) {
    	this.ownerDialog = owner;
    	// 算出パネルを非表示にする
    	panelCalculate.setVisible(this.ownerDialog);
    	// OKボタンのテキスト変更
    	String text = Message.getString("dialog.common.button.ok"); //OK
    	if (!owner) {
    		text = Message.getString("dialog.common.button.recalculate"); //再計算
    	}
    	this.btnOk.setText(text);
    }

	/**
	 * 要求Byte/FLOP設定プロパティを設定する.
	 * @param properties    要求Byte/FLOP設定プロパティ
	 */
	public void setPropertiesMemoryband(RequiredBFProperties properties) {
		this.propertiesMemoryband = properties;
		updateProperties();
	}

	/**
	 * アクセス先メモリプロパティを更新する.
	 */
	private void updateProperties() {

		if (this.propertiesMemoryband != null) {
	    	// アクセス先メモリ
	    	ACCESSMEMORY_TYPE[] listmemory = ACCESSMEMORY_TYPE.values();
			if (this.propertiesMemoryband != null) {
				this.cmbAllAccess.removeAllItems();
				for (ACCESSMEMORY_TYPE type : listmemory) {
					RequiredBF mem = this.propertiesMemoryband.getRequiredBF(type);
					this.cmbAllAccess.addItem(
										type.getName(),
										mem != null?mem.getBackColor():null);
				}
				this.cmbAllAccess.setSelectedIndex(0);
			}
		}
	}
}


