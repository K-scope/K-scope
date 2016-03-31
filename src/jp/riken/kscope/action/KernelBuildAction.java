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
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
//import java.util.Arrays;
import java.util.List;

import javax.swing.JOptionPane;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.properties.KernelProperties;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.KernelService;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * カーネル抽出アクションクラス
 * @author RIKEN
 *
 */
public class KernelBuildAction extends ActionBase {

    /** カーネル取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller    アプリケーションコントローラ
     * @param view             カーネル取得先ビュー
     */
    public KernelBuildAction(AppController controller, FRAME_VIEW view) {
        super(controller);
        this.view = view;
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return        true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        IBlock[] blocks = this.getSelectedBlocks();
        if (blocks == null) return false;
        if (blocks == null || blocks.length <= 0) {
            return false;
        }
        return true;
    }


    /**
     * 現在選択されているブロックを取得する
     * @return    選択ブロック
     */
    private IBlock[] getSelectedBlocks() {
        if (this.view != FRAME_VIEW.EXPLORE_VIEW) return null;

        IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
        if (blocks == null || blocks.length <= 0) {
            return null;
        }

        return blocks;
    }


    /**
     * ソースファイルを開くイベント
     * @param event        イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // ステータスメッセージ
        final String message = Message.getString("mainmenu.kernel.create");   // カーネル抽出
        Application.status.setMessageMain(message);

        // 選択ブロック
        IBlock[] blocks = this.getSelectedBlocks();
        if (blocks == null || blocks.length <= 0) {
            return;
        }

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 最終アクセスフォルダ
        String currentFolder = this.controller.getLastAccessFolder();
        if (currentFolder == null) {
            currentFolder = System.getProperty("user.dir");
        }

        String  action_message = null;
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        errorModel.clearErrorList();
        try {
            // カーネル構築サービス
            KernelService service = new KernelService();
            // フォートランデータベース
            service.setFortranDb(this.controller.getFortranLanguage());

            // エラー情報モデル
            service.setErrorInfoModel(errorModel);

            // カーネルブロックのチェック、取得
            List<IBlock> kernel_list = service.getKernelBlocks(Arrays.asList(blocks), false);

            if (kernel_list != null && kernel_list.size() > 0) {
                // カーネル出力フォルダ選択ダイアログを表示する。
                File[] selected = SwingUtils.showOpenFolderDialog(
                        frame,
                        Message.getString("dialog.common.kernelfolder.title"), //カーネル出力フォルダの選択
                        currentFolder,
                        false);
                if (selected == null || selected.length <= 0) {
                        Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
                        return;
                }
                File selected_folder = selected[0];

                // 既存出力であるかチェックする
                File output_folder = service.getOutputDirectory(kernel_list, selected_folder);
                if (output_folder != null && output_folder.exists()) {
                    // 上書確認
                    // カーネルは既に存在します。既存のカーネルを置き換えますか?
                    String confirm_message = Message.getString("kernel.overwrite.message");
                    int confirm = JOptionPane.showConfirmDialog(frame,
                                    confirm_message,  // カーネルは既に存在します。既存のカーネルを置き換えますか?
                                    Message.getString("swingutils.savefiledialog.overwrite.title"), // 上書き確認
                                    JOptionPane.OK_CANCEL_OPTION,
                                    JOptionPane.WARNING_MESSAGE);
                    if (confirm != JOptionPane.OK_OPTION) {
                        return;
                    }
                }

                // カーネル抽出実行
                service.buildKernel(kernel_list, selected_folder);

                if (!service.existsErrorInfo()) {
                    // 実行終了しました。
                    action_message = Message.getString("appcontroller.thread.message.success");
                }
                else {
                    // エラーにより終了しました。
                    action_message = Message.getString("appcontroller.thread.message.error");
                }
            }
            else {
                // エラーにより終了しました。
                action_message = Message.getString("appcontroller.thread.message.error");
            }

        } catch (Exception ex) {
            // エラーにより終了しました。
            action_message = Message.getString("appcontroller.thread.message.error");
            errorModel.addErrorInfo(ex);
            ex.printStackTrace();
        }

        JOptionPane.showMessageDialog(frame,
                action_message,  // 実行終了しました。|エラーにより終了しました。
                Message.getString("appcontroller.thread.title"), // 処理メッセージ
                JOptionPane.INFORMATION_MESSAGE);

        Application.status.setMessageMain(message +
                Message.getString("action.common.done.status")); //完了
        return;
    }


}
