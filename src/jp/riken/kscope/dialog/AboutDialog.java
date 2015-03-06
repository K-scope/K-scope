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
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.event.HyperlinkListener;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * バージョンダイアログ
 *
 * @author RIKEN
 *
 */
public class AboutDialog extends javax.swing.JDialog implements ActionListener, HyperlinkListener {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 閉じるボタン */
    private JButton btnClose;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
    /** ダイアログサイズ */
    private Dimension DIALOG_SIZE = new Dimension(320, 480);

    /**
     * コンストラクタ
     *
     * @param owner	親フレーム
     * @param modal	true=モーダルダイアログを表示する
     */
    public AboutDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
    }

    /**
     * コンストラクタ
     *
     * @param frame 親フレーム
     */
    public AboutDialog(JFrame frame) {
        super(frame);
        initGUI();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            this.setResizable(true);
            this.setTitle(Message.getString("mainmenu.help.about")); //About
            this.getContentPane().setLayout(new BorderLayout());

            // ボタンパネル
            {
                JPanel panelButtons = new JPanel();
                FlowLayout jPanel1Layout = new FlowLayout();
                jPanel1Layout.setHgap(10);
                jPanel1Layout.setVgap(10);
                panelButtons.setLayout(jPanel1Layout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);

                // クローズボタン
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                btnClose = new JButton();
                panelButtons.add(btnClose);
                btnClose.setText(Message.getString("dialog.common.button.close")); //閉じる
                btnClose.setPreferredSize(buttonSize);
                btnClose.addActionListener(this);
            }
            //ロゴアイコン
            JLabel lblIcon = new JLabel();
            Icon icon1 = ResourceUtils.getIcon("logo.png");
            lblIcon.setHorizontalAlignment(JLabel.CENTER);
            lblIcon.setIcon(icon1);
            this.getContentPane().add(lblIcon, BorderLayout.NORTH);

            {
                JPanel panelContent = new JPanel();
                getContentPane().add(panelContent, BorderLayout.CENTER);
                GridBagLayout layoutTable = new GridBagLayout();
                layoutTable.rowWeights = new double [] {0.0, 1.0, 0.0};
                layoutTable.rowHeights = new int [] {24, 200, 7};
                layoutTable.columnWeights = new double [] {0.0, 1.0, 0.0};
                layoutTable.columnWidths = new int [] {7, 320, 7};
                panelContent.setLayout(layoutTable);

                //アプリケーション名＋ヴァージョン
                JLabel lblAppName = new JLabel();
                lblAppName.setBounds(18, 110, 300, 15);
                lblAppName.setText(KscopeProperties.APPLICATION_NAMEJP
                        + "  ("
                        + Message.getString("aboutdialog.version.title") //VERSION :
                        + KscopeProperties.APPLICATION_VERSION
                        + ")");
                panelContent.add(lblAppName, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));

                //テキストエリア(スクロール付き)
                // "This prototype software provides structure of source code to support tuning and analysis. <Disclaimer>: ... <Licence>: ..."
                String text = Message.getString("kscope.license");
                JTextPane textarea = new JTextPane();
                textarea.setContentType("text/html");
                textarea.putClientProperty(JTextPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);	//setFont()が有効になる
                textarea.setText(text);
                textarea.setCaretPosition(0);
                textarea.setEditable(false);
                JScrollPane scroller = new JScrollPane(textarea,
                                                       ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                       ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                panelContent.add(scroller, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHEAST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                // リンククリックイベント
                textarea.addHyperlinkListener(this);
            }

            this.pack();
            this.setSize(DIALOG_SIZE);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * ダイアログを表示する。
     *
     * @return ダイアログの閉じた時のボタン種別
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
     *
     * @param event	イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 閉じる
        if (event.getSource() == this.btnClose) {
            this.result = Constant.CLOSE_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
    }

    /**
     * 説明文のリンクタグクリックイベント
     */
	@Override
	public void hyperlinkUpdate(HyperlinkEvent e) {
		if (e.getEventType() == EventType.ACTIVATED) {	//クリックされた時
			URL url = e.getURL();
			//デフォルトのブラウザーを使ってリンク先を表示
			Desktop dp = Desktop.getDesktop();
			try {
				dp.browse(url.toURI());
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}
}


