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

import java.awt.Component;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.service.AppController;

/**
 * エラー箇所を開くアクション
 * @author RIKEN
 *
 */
public class ErrorOpenFileAction extends ActionBase implements MouseListener {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ErrorOpenFileAction(AppController controller) {
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

        // エラー箇所を開く
        openErrorFile(frame);

    }

    /**
     * エラー箇所を開く
     * @param	parent     親コンポーネント
     */
    private void openErrorFile(Component parent) {

        CodeLine line = null;
        try {
            // エラー箇所のコード情報の取得を行う
            line = this.controller.getMainframe().getPanelAnalysisView().getSelectedCodeLine();
            // エラーメッセージ
            String errorMessage = this.controller.getMainframe().getPanelAnalysisView().getPanelError().getSelectedErrorMessage();
            if (line == null && errorMessage == null) return;

            if (line == null || line.getSourceFile() == null || line.getSourceFile().getFile() == null) {
                // エラーメッセージ
                JOptionPane.showMessageDialog(
                        parent,
                        errorMessage,
                        Message.getString("erroropenfileaction.errorinfo.dialog.title"), //エラー情報
                        JOptionPane.INFORMATION_MESSAGE);
                return;
            }

            // エラー箇所の情報をセットして開く
            this.controller.openSourceFile(line);

            // 選択コード行を選択状態にする
            this.controller.getMainframe().getPanelSourceView().setSelectedLine(line);

        } catch (HeadlessException ex) {
            ex.printStackTrace();
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }


    /**
     * マウスクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // ダブルクリックチェック
        if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
            if (event.getSource() instanceof JTable) {
                // エラー箇所を開く
                openErrorFile(this.controller.getMainframe());
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


}
