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
import java.util.ArrayList;
import java.util.List;


import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.dialog.RequiredBFDialog;
import jp.riken.kscope.dialog.VariableAccessDialog;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;
import jp.riken.kscope.service.AppController;


/**
 * 要求Byte/FLOPアクションクラス
 * @author RIKEN
 */
public class AnalysisMemoryAction extends ActionBase {
	/** アクションモード */
	public enum ACTION_MODE {
		/** 変数アクセス先メモリ設定 */
		ACCESS_SETTING,
		/** 要求Byte/FLOP算出 */
		MEMORY_CALCULATE
	};

	/** 要求Byte/FLOPアクションモード */
	private ACTION_MODE mode;
    /** アクセス先変数取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param mode 			アクションモード
     * @param view 			アクセス先変数取得先ビュー
     */
    public AnalysisMemoryAction(AppController controller, ACTION_MODE mode, FRAME_VIEW view) {
        super(controller);
        this.mode = mode;
        this.view = view;
    }


    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        // 選択ブロックを取得する
        IBlock[] blocks = getSelectedBlocks();
        if (blocks == null) return false;
        return true;
    }


    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 実行チェック
        if (!validateAction()) return;

        // 選択ブロックを取得する
        IBlock[] blocks = getSelectedBlocks();
        if (blocks == null) return;

        // ソースコードの選択範囲をクリアし、選択背景色に変更する
        if (this.view == FRAME_VIEW.SOURCE_VIEW) {
        	setSelectedBlockNoCaret();
        }
        // ステータスメッセージ
        String message = "";
        if (this.mode == ACTION_MODE.ACCESS_SETTING) {
        	message = Message.getString("mainmenu.analysis.access");
        }
        else if (this.mode == ACTION_MODE.MEMORY_CALCULATE) {
        	message = Message.getString("mainmenu.analysis.calculate");
        }
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );
        // 要求Byte/FLOP設定プロパティ
        RequiredBFProperties properitiesMemory = this.controller.getPropertiesMemory();
        // 変数アクセス先メモリ設定
        VariableMemoryProperties properitiesVariable = this.controller.getPropertiesVariable();
        // メモリアクセス先設定ダイアログ
        VariableAccessDialog dialogVariable = new VariableAccessDialog(frame, true);        // メモリアクセス性能ダイアログ
        RequiredBFDialog dialogPerformance = new RequiredBFDialog(frame, true);
        // プロパティ設定
        dialogVariable.setPropertiesVariable(properitiesVariable);
        dialogVariable.setPropertiesMemoryband(properitiesMemory);
        dialogPerformance.setPropertiesMemoryband(properitiesMemory);
        dialogPerformance.setPropertiesVariable(properitiesVariable);
        // 選択ブロック
        dialogVariable.setSelectedblocks(blocks);
        dialogPerformance.setSelectedblocks(blocks);
        // 表示ダイアログ
        dialogVariable.setMemoryPerformanceDialog(dialogPerformance);
        dialogPerformance.setVariableAccessDialog(dialogVariable);

        // 要求Byte/FLOP算出サービス
        RequiredBFModel modelRequired = this.controller.getRequiredByteFlopModel();
        LanguageTreeModel modelLanguage = this.controller.getLanguageTreeModel();
        modelRequired.setModelLanguageTree(modelLanguage);
        AnalysisMemoryService serviceMemory = new AnalysisMemoryService();
        serviceMemory.setBlocks(blocks);
        serviceMemory.setProperitiesRequiredBF(properitiesMemory);
        serviceMemory.setPropertiesOperand(this.controller.getPropertiesOperand());
        serviceMemory.setModelRequiredBF(modelRequired);
        serviceMemory.setPropertiesVariableMemory(properitiesVariable);
        dialogPerformance.setServiceMemory(serviceMemory);

        if (this.mode == ACTION_MODE.ACCESS_SETTING) {
	        // 変数アクセス先設定ダイアログを表示する。
	        int result = dialogVariable.showDialog();
	        if (result == Constant.CANCEL_DIALOG) {
	        	return;
	        }
        }
        else if (this.mode == ACTION_MODE.MEMORY_CALCULATE) {
	        // メモリアクセス性能ダイアログクラスを表示する。
	        int result = dialogPerformance.showDialog();
	        if (result == Constant.CANCEL_DIALOG) {
	        	return;
	        }
        }

        return;

    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    private IBlock[] getSelectedBlocks() {
    	IBlock[] blocks = null;
        if (this.view == FRAME_VIEW.SOURCE_VIEW) {
        	// ソースビュー
        	IBlock[] codeblocks = getSelectedCodeLines();
            if (codeblocks == null || codeblocks.length <= 0) return null;
            // ソースビューからの場合、親ブロックとしてブロックリストを作成する.
            if (codeblocks.length > 1) {
	            BlockList parent = new BlockList(codeblocks);
	            blocks = new IBlock[codeblocks.length + 1];
	            blocks[0] = parent;
	            for (int i=0; i<codeblocks.length; i++) {
	            	blocks[i+1] = codeblocks[i];
	            }
            }
            else {
            	blocks = codeblocks;
            }
        }
        else {
        	// 構造エクスプローラービューから選択ブロックを取得する
            blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
        }
        if (blocks == null) return null;

        // IBlockをチェックする
        List<IBlock> list = new ArrayList<IBlock>();
        for (IBlock block : blocks) {
        	// 宣言文以外を追加する
        	if (block.getBlockType() != BlockType.VARIABLEDEFINITION) {
        		list.add(block);
        	}
        }

        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }

    /**
     * ソースビューから選択ブロックを取得する
     * @return		選択ブロック
     */
    private IBlock[] getSelectedCodeLines() {

    	// ソースコードの選択範囲を取得する
	    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedArea();
	    LanguageUtils utils = new LanguageUtils(this.controller.getFortranLanguage());
	    IBlock[] blocks = utils.getCodeLineBlocks(line);
	    return blocks;
    }

    /**
     * ソースコードの選択範囲をクリアし、選択背景色に変更する.
     */
    private void setSelectedBlockNoCaret() {
    	// ソースコードの選択範囲を取得する
	    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedArea();
	    this.controller.getMainframe().getPanelSourceView().setSelectedBlockNoCaret(line);
    }
}
