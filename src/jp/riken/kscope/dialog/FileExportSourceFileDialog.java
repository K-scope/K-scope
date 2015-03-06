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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * ソースファイルエクスポートダイアログクラス
 * @author RIKEN
 */
public class FileExportSourceFileDialog extends javax.swing.JDialog implements
		ActionListener {
	/** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** エクスポートボタン */
    private JButton btnExport;

    /** 出力フォルダ */
    private JTextField txtExportFolder;
    /** 参照ボタン */
    private JButton btnRef;
    /** ソース以外のファイルも出力する ラベル */
    private JLabel lblOtherFiles;
    /** ソース以外のファイルも出力する チェックボックス */
    private JCheckBox chxOtherFiles;
    /** 除外ファイル　*/
    private JTextField txtExcludeFiles;

    /** プロジェクトフォルダ */
    private File projectFolder;
    /** エラーメッセージ */
    private String errMsg;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
    /** 画面幅 */
	private final int DEFAULT_WIDTH = 480;

	 /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal     モーダルモード
     */
	public FileExportSourceFileDialog(Frame frame, boolean modal) {
		super(frame, modal);
		initGUI();
	}

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
	public FileExportSourceFileDialog(JFrame frame) {
		super(frame);
		initGUI();
	}

	/**
	 * GUI初期化
	 */
	private void initGUI() {
		try {

			// ボタンパネル
			{
				JPanel panelButtons = new JPanel();
                FlowLayout jPanel1Layout = new FlowLayout();
                jPanel1Layout.setHgap(10);
                jPanel1Layout.setVgap(20);
                panelButtons.setLayout(jPanel1Layout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                panelButtons.setPreferredSize(new java.awt.Dimension(200, 55));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);

                {
                	btnExport = new JButton();
                    panelButtons.add(btnExport);
                    String text = "";
                    text = Message.getString("mainmenu.file.export"); //Export
                    btnExport.setText(text);
                    btnExport.setPreferredSize(buttonSize);
                    btnExport.setMargin(new Insets(5, 5, 5, 5));
                    btnExport.addActionListener(this);
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
				JPanel panelContent = new JPanel(new BorderLayout());
                getContentPane().add(panelContent, BorderLayout.CENTER);
                GridBagLayout layoutContent = new GridBagLayout();
                layoutContent.rowWeights = new double[] {0.0, 0.0, 0.0, 1.0};
                layoutContent.rowHeights = new int[] {32, 32, 32, 7};
                layoutContent.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
                layoutContent.columnWidths = new int[] {14, 32, 220, 50};
                panelContent.setLayout(layoutContent);

                // 出力フォルダパネル
                {
                	panelContent.add(new JLabel(Message.getString("fileexportsourcefiledialog.label.outputfolder")), //出力先フォルダ
                			new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(20, 0, 0, 8), 0, 0));
                	txtExportFolder = new JTextField();
                	panelContent.add(txtExportFolder,
                			new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(20, 0, 0, 0), 0, 0));
                	btnRef = new JButton();
                	btnRef.setText(Message.getString("dialog.common.button.refer")); //参照
                	btnRef.setPreferredSize(new java.awt.Dimension(48, 22));
                	btnRef.setMaximumSize(new java.awt.Dimension(48, 22));
                	btnRef.setMargin(new Insets(0, 3, 0, 3));
                	btnRef.addActionListener(this);
                	panelContent.add(btnRef,
                			new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(20, 8, 0, 8), 0, 0));
                }
                // ソースファイル以外のファイルパネル
                {
                	lblOtherFiles = new JLabel(Message.getString("fileexportsourcefiledialog.label.excludefile")); //除外ファイル
                	txtExcludeFiles = new JTextField();
                	txtExcludeFiles.setToolTipText(Message.getString("fileexportsourcefiledialog.tooltip.excludefile")); //カンマ区切りで除外するファイル名を列記します。...
                	chxOtherFiles = new JCheckBox(
                			Message.getString("fileexportsourcefiledialog.checkbox.excludefile"), //ソース以外のファイルも出力する
                			false){
						/** シリアル番号 */
						private static final long serialVersionUID = 1L;
						@Override
						protected void fireStateChanged() {
							lblOtherFiles.setEnabled(this.isSelected());
							txtExcludeFiles.setEnabled(this.isSelected());
						}
                	};
                	lblOtherFiles.setEnabled(chxOtherFiles.isSelected());
                	txtExcludeFiles.setEnabled(chxOtherFiles.isSelected());
                	panelContent.add(chxOtherFiles,
                			new GridBagConstraints(1, 1, 3, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(10, 8, 0, 8), 0, 0));
                	panelContent.add(lblOtherFiles,
                			new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                	panelContent.add(txtExcludeFiles,
                			new GridBagConstraints(2, 2, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 8, 0, 8), 0, 0));
                }
			}

            this.setTitle(Message.getString("fileexportsourcefiledialog.dialog.title")); //ソースファイルエクスポート
            this.pack();

            Dimension size = new Dimension(DEFAULT_WIDTH, this.getSize().height);
            this.setSize(size);
		}
		catch(Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * 設定値エラーチェック
	 * @return	true=問題なし
	 */
	private boolean chkParams() {
		// 出力先未設定　存在しないorディレクトリでない場合はＮＧ
		String str = txtExportFolder.getText();
		if (StringUtils.isNullOrEmpty(str)) {
			errMsg = Message.getString("fileexportsourcefiledialog.errdlg.msg.outputfolderunset"); //出力先フォルダーが設定されていません。
			return false;
		}
		File f = new File(str);
		if (!f.exists()) {
			errMsg = Message.getString("fileexportsourcefiledialog.errdlg.msg.outputfoldernotexist"); //指定された出力先フォルダーは存在しません。
			return false;
		}
		if (!f.isDirectory()) {
			errMsg = Message.getString("fileexportsourcefiledialog.errdlg.msg.outputfoldernotdir"); //指定された出力先フォルダーはディレクトリではありません。
			return false;
		}

		return true;
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		if (event.getSource() == this.btnExport) {
			if (!chkParams()) {
				JOptionPane.showMessageDialog(this, errMsg,
						Message.getString("dialog.common.error"), //エラー
						JOptionPane.ERROR_MESSAGE);
			}
			else {
				result = Constant.OK_DIALOG;
				dispose();
			}
			return;
		}
		else if (event.getSource() == this.btnCancel) {
			result = Constant.CANCEL_DIALOG;
			dispose();
			return;
		}
		else if (event.getSource() == this.btnRef) {
			// フォルダ選択ダイアログを表示する。
            File[] selected = SwingUtils.showOpenFolderDialog(this,
                    Message.getString("fileexportsourcefiledialog.outputfolderselectdialog.title"), //出力先フォルダの選択
                    this.projectFolder.getAbsolutePath(), false);
            if (selected == null || selected.length <= 0) return;
            this.txtExportFolder.setText(selected[0].getAbsolutePath());
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
     * プロジェクトフォルダを設定する
     * @param	projectFolder
     */
    public void setProjectFolder(File projectFolder) {
    	if (projectFolder != null && projectFolder.exists() && projectFolder.isDirectory()) {
    		this.projectFolder = projectFolder;
    	}
    	else {
    		String usrFolderPath = System.getProperty("user.dir");
    		this.projectFolder = new File(usrFolderPath);
    	}
    }

    /**
     * 出力先フォルダパスを取得する
     * @return	プロジェクトフォルダの絶対パス
     */
    public String getOutputFolder() {
    	return this.txtExportFolder.getText();
    }

    /**
     * 除外ファイル文字列を設定
     * @param	exclude          除外ファイル文字列
     */
    public void setExcludeFile(String exclude) {
    	txtExcludeFiles.setText(exclude);
    }

    /**
     * 除外ファイルパターンを取得
     * @return	除外ファイルパターン
     */
    public String getExcludeFilePattern() {
    	return this.txtExcludeFiles.getText();
    }

    /**
     * ソースファイル以外も出力するか否か
     * @return	true=ソースファイル以外も出力する
     */
    public boolean isExportOtherFile(){
    	return this.chxOtherFiles.isSelected();
    }
}
