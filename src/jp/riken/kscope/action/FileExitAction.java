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
package jp.riken.kscope.action;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JOptionPane;

import jp.riken.kscope.Message;
import jp.riken.kscope.service.AppController;

/**
 * 終了アクション
 * @author RIKEN
 */
public class FileExitAction extends ActionBase implements WindowListener {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileExitAction(AppController controller) {
        super(controller);
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 確認メッセージを表示し、アプリケーションを終了する。
        exitApplication(frame);

    }

    /**
     * 確認メッセージを表示し、アプリケーションを終了する。
     * @param frame			親フレーム
     */
    public void exitApplication(Frame frame) {

        // 確認メッセージを表示する。
        int option = JOptionPane.showConfirmDialog(
                frame,
                Message.getString("fileexitaction.exit.dialog.message"), //K-scopeを終了しますか？
                Message.getString("fileexitaction.exit.dialog.title"), //K-scopeの終了
                JOptionPane.OK_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE);
        if (option != JOptionPane.OK_OPTION) {
            return;
        }

        // アプリケーションの終了
        System.exit(0);
    }


    /**
     * ウィンドウの終了前イベント
     * @param event		イベント情報
     */
    @Override
    public void windowClosing(WindowEvent event) {

        Frame frame = (Frame)event.getWindow();

        // 確認メッセージを表示し、アプリケーションを終了する。
        exitApplication(frame);
    }

    /**
     * ウィンドウのオープンイベント
     * @param event		イベント情報
     */
    @Override
    public void windowOpened(WindowEvent event) {}

    /**
     * ウィンドウの終了イベント
     * @param event		イベント情報
     */
    @Override
    public void windowClosed(WindowEvent event) {}

    /**
     * ウィンドウのアイコン化イベント
     * @param event		イベント情報
     */
    @Override
    public void windowIconified(WindowEvent event) {}

    /**
     * ウィンドウのアイコン化イベント
     * @param event		イベント情報
     */
    @Override
    public void windowDeiconified(WindowEvent event) {}

    /**
     * ウィンドウのアクティブイベント
     * @param event		イベント情報
     */
    @Override
    public void windowActivated(WindowEvent event) {}

    /**
     * ウィンドウの非アクティブイベント
     * @param event		イベント情報
     */
    @Override
    public void windowDeactivated(WindowEvent event) {}

}
