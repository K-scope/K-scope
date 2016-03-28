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

import java.awt.event.ActionEvent;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.LanguageService;

/**
 * 新規構造ツリーを開くアクションクラス
 * @author RIKEN
 */
public class ViewOpenLanguageTreeAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ViewOpenLanguageTreeAction(AppController controller) {
        super(controller);
    }


    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // 現在の選択ノードの取得
        IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
        if (blocks == null) return false;
        IBlock selectedProcedure = null;
        for (IBlock block : blocks) {
            if (block instanceof Procedure) {
                selectedProcedure = block;
                break;
            }
        }
        if (selectedProcedure == null) return false;

        return true;
    }


    /**
     * 構造ツリーを開く
     * @param   block			新規ルートブロック
     */
    public void openLanguageTree(IBlock block) {
        // ルートブロックにできるのは、Procedureのみ
        if (!(block instanceof Procedure)) {
            return;
        }

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        // 構造ツリーモデルを新規に生成する
        LanguageTreeModel languageModel = new LanguageTreeModel();

        // 構造解析サービス
        LanguageService service = new LanguageService(fortran, false);
        // 構造ツリーモデルを設定する
        service.setLanguageTreeModel(languageModel);
        // エラー情報モデルを設定する。
        service.setErrorInfoModel(errorModel);

        // 構造ツリーを生成する
        if (!service.writeTree(block)) {
            return;
        }

        // 構造ツリー生成済みの構造ツリーモデルにて新規ツリータブを作成する
        this.controller.getMainframe().getPanelExplorerView().viewLanguageTree(languageModel);

        // 構造ツリーフィルタの適用を行う
        this.controller.applyLanguageTreeFilter();

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
    			Message.getString("mainmenu.view.newtree")); //新規構造ツリー

        // 現在の選択ノードの取得
        IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
        if (blocks == null) return;
        IBlock selectedProcedure = null;
        for (IBlock block : blocks) {
            if (block instanceof Procedure) {
                selectedProcedure = block;
                break;
            }
        }
        if (selectedProcedure == null) return;

        // 構造ツリーを開く
        openLanguageTree(selectedProcedure);

    }


}
