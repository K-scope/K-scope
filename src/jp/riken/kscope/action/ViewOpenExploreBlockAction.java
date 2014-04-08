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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.service.AppController;

/**
 * ファイルを開くアクションクラス
 * @author riken
 */
public class ViewOpenExploreBlockAction extends ActionBase implements MouseListener {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ViewOpenExploreBlockAction(AppController controller) {
        super(controller);
    }


    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        // 選択ソースコード行情報を取得する
        CodeLine[] line = this.controller.getMainframe().getPanelExplorerView().getSelectedCodeLines();
        if (line == null) return false;

        return true;
    }


    /**
     * 選択ファイルを開く
     * AllAnalysisMemoryActionクラスのためにprivateからpublicに変更(2014/4/8 ohichi)
     */
    public void openFile() {

        // 選択ソースコード行情報を取得する
        CodeLine[] line = this.controller.getMainframe().getPanelExplorerView().getSelectedCodeLines();
        if (line == null) return;

        // 選択ソースコード行情報からファイルを開く
        for (int i=0; i<line.length; i++) {
            if (line[i].getSourceFile() == null || line[i].getSourceFile().getFile() == null) {
            	Frame frame = this.controller.getMainframe();
            	JOptionPane.showMessageDialog(frame,
            			Message.getString("viewopenexploreblockaction.errdialog.notsetsource.message", line[i].getStatement()), //のソースファイルが設定されていません。
            			Message.getString("dialog.common.error"), //エラー
            			JOptionPane.ERROR_MESSAGE);
            	continue;
            }

            try {
                this.controller.openSourceFile(line[i]);
            } catch (Exception ex) {
                ex.printStackTrace();
                this.controller.getErrorInfoModel().addErrorInfo(line[i], ex.toString());
            }
        }

        // 選択コード行を選択状態にする
        // ソースファイルの一覧作成
        List<SourceFile> files = new ArrayList<SourceFile>();
        for (int i=0; i<line.length; i++) {
            if (line[i].getSourceFile() == null) continue;
            if (line[i].getSourceFile().getFile() == null) continue;

            if (!files.contains(line[i].getSourceFile())) {
                files.add(line[i].getSourceFile());
            }
        }

        // 選択コード行リストの作成
        for (SourceFile file : files) {
            List<CodeLine> lines = new ArrayList<CodeLine>();
            for (int i=0; i<line.length; i++) {
                if (line[i].getSourceFile() == null) continue;
                if (file.equals(line[i].getSourceFile())) {
                    lines.add(line[i]);
                }
            }
            if (lines.size() > 0) {
                // 選択コード行を選択状態にする
                this.controller.setSelectedBlock(lines.toArray(new CodeLine[0]));
            }
        }

        return;
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
    	// ステータスバー
    	Application.status.setMessageMain(
    			Message.getString("mainmenu.view.openfile")); //ファイルを開く
        // 選択ファイルを開く
        openFile();
    }

    /**
     * マウスクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // ダブルクリックチェック
        if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
            if (event.getSource() instanceof JTree) {
                // 選択ファイルを開く
                openFile();
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
