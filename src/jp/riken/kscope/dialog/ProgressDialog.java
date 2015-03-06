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
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JButton;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.service.FutureService;


/**
 * 進捗状況表示ダイアログ
 * @author RIKEN
 */
public class ProgressDialog extends javax.swing.JDialog implements ActionListener, Observer {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** プログレスバー */
    private JProgressBar progressBar;
    /** 進捗メッセージ */
    private JLabel lblMessageStatus;
    /** キャンセルボタン */
    private JButton btnCancel;
    /** 閉じるボタン */
    private JButton btnClose;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** スレッドタスクサービス */
    private FutureService<Integer> threadService;

    /**
     * プログレスバー不確定：インターバル時間(ms).<br/>
     * PROGRESSBAR_CYCLETIME / PROGRESSBAR_INTERVAL = 偶数であること
     */
    private final int PROGRESSBAR_INTERVAL = 200;
    /** プログレスバー不確定：最大時間(ms) */
    private final int PROGRESSBAR_CYCLETIME = 4000;

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public ProgressDialog(JFrame frame) {
        super(frame);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param owner		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public ProgressDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            getContentPane().setLayout(null);
            this.setResizable(false);
            {
                progressBar = new JProgressBar();
                getContentPane().add(progressBar);
                progressBar.setBounds(40, 40, 360, 20);
            }
            {
                lblMessageStatus = new JLabel();
                getContentPane().add(lblMessageStatus);
                lblMessageStatus.setBounds(40, 70, 360, 20);
            }
            {
                btnClose = new JButton();
                getContentPane().add(btnClose);
                btnClose.setText(Message.getString("progressdialog.button.invisible")); //非表示
                btnClose.setBounds(94, 112, 100, 22);
                btnClose.addActionListener(this);
            }
            {
                btnCancel = new JButton();
                getContentPane().add(btnCancel);
                btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                btnCancel.setBounds(235, 112, 101, 22);
                btnCancel.addActionListener(this);
            }

            this.setTitle(Message.getString("progressdialog.dialog.title")); //しばらくお待ちください
            this.setSize(440, 180);

            progressBar.setMinimum(0);
            progressBar.setMaximum(100);
            progressBar.setIndeterminate(true);

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
     * ボタンクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        if (event.getSource() == this.btnClose) {
            this.result = Constant.OK_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.OK_DIALOG;

            // スレッドをキャンセルする
            if (this.threadService != null) {
                this.threadService.cancel(true);
            }

            // ダイアログを閉じる。
            dispose();
            return;
        }
    }

    /**
     * ステータスメッセージを設定する
     * @param message		ステータスメッセージ
     */
    private void setMessageStatus(final String message) {

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                ProgressDialog.this.lblMessageStatus.setText(message);
            }
        });

    }

    /**
     * 進捗状況の更新通知
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {

        Application.StatusPrint status = (Application.StatusPrint)o;

        // ステータスメッセージ
        String statusMessage = status.getMessageStatus();
        setMessageStatus(statusMessage);

        // プログレスバー
        if (status.isProgressStart()) {
            // プログレスバー開始
            Integer value = status.getProgressValue();
            Integer min = status.getProgressMin();
            Integer max = status.getProgressMax();
            if (value != null && min != null && max != null) {
                progressBar.setMinimum(min);
                progressBar.setMaximum(max);
                progressBar.setValue(value);
            }
            else {
                progressBar.setIndeterminate(true);
                UIManager.put("ProgressBar.repaintInterval", new Integer(PROGRESSBAR_INTERVAL));
                UIManager.put("ProgressBar.cycleTime", new Integer(PROGRESSBAR_CYCLETIME));
            }
        }
        else {
            // プログレスバー停止
            progressBar.setMinimum(0);
            progressBar.setMaximum(0);
            progressBar.setValue(0);
            progressBar.setIndeterminate(false);
        }

    }

    /**
     * スレッドタスクサービスを設定する
     * @param threadService		スレッドタスクサービス
     */
    public void setThreadService(FutureService<Integer> threadService) {
        this.threadService = threadService;
    }

}



