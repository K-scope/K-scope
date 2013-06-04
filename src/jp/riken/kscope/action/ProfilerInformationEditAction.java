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
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.gui.ProfilerTablePanel;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.service.AppController;


/**
 * プロファイラ情報の付加情報追加アクションクラス
 * @author riken
 *
 */
public class ProfilerInformationEditAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ProfilerInformationEditAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        return isSelectedInformation();
    }

    /**
     * 付加情報編集イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 実行チェック
        if (!validateAction()) return;

        // ステータスメッセージ
        final String message = Message.getString("mainmenu.edit.info"); //"付加情報編集"
        Application.status.setMessageMain(message);

        // 選択付加情報を取得する
        IAnalisysComponent panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
        if (!(panel instanceof ProfilerTablePanel)) return;
        ProfilerBaseData value = ((ProfilerTablePanel)panel).getModel().getSelectedInfo();
        if (value == null) return;
        // 追加プロファイラ情報の取得
        String addinfo = ((ProfilerTablePanel)panel).getModel().getSelectedText();

        // 付加情報の編集を行う
        if (!editInformations(value, addinfo)) {
            // 付加情報の編集のキャンセル
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status"));
            return;
        }

        // 付加情報パネルをアクティブにする
        this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.INFORMATION);
        Application.status.setMessageMain(message +
    			Message.getString("action.common.done.status"));
        return;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    private boolean isSelectedInformation() {
        IAnalisysComponent panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
        if (!(panel instanceof ProfilerTablePanel)) return false;
        ProfilerBaseData value = ((ProfilerTablePanel)panel).getModel().getSelectedInfo();
        if (value == null) return false;

        IInformation info = getInformationBlock(value);
        if (info == null) return false;
        return true;
    }

    /**
     * 付加情報の編集を行う
     * @param infoNodes		付加情報範囲
     * @param addText		追加付加情報
     * @return    付加情報の編集の可否
     */
    public boolean editInformations(IInformation[] infoNodes, String addText) {
        if (infoNodes == null) return false;
        // 複数範囲の付加情報から付加情報クラスを取得する.
        IInformation info = getProgramInformation(infoNodes);

        // 付加情報ダイアログを表示する
        EditInformationEditAction action = new EditInformationEditAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        return action.editInformation(info, addText);
    }

    /**
     * 複数範囲の付加情報から付加情報クラスを取得する.
     * Programクラスにて付加情報範囲のInformationBlockを生成して返す. <br/>
     * 付加情報範囲が１つだけの場合は、先頭の付加情報を返す.
     * @param infos		付加情報範囲
     * @return			付加情報
     */
    private IInformation getProgramInformation(IInformation[] infos) {
        if (infos == null) return null;
        Program fortran = this.controller.getFortranLanguage();
        if (fortran == null) return null;
        if (infos.length == 0) return null;
        // 単一
        if (infos.length == 1) {
            return infos[0];
        }
        // 複数範囲
        IInformation start = infos[0];
        IInformation end = infos[infos.length-1];
        if (start != null && end != null) {
            IInformation info = fortran.getInformation(start, end);
            return info;
        }

        return start;
    }

    /**
     * 付加情報の編集を行う
     * @param value		    プロファイラデータ
     * @param text		追加付加情報
     * @return    付加情報の編集の可否
     */
	public boolean editInformations(ProfilerBaseData value, String text) {
	    IInformation info = getInformationBlock(value);
	    if (info == null) return false;

        // 付加情報ダイアログを表示する
        EditInformationEditAction action = new EditInformationEditAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        return action.editInformation(info, text);
	}

	/**
	 * プロファイラデータから付加情報ブロックを取得する.
	 * @param value		プロファイラデータ
	 * @return			付加情報ブロック
	 */
	private IInformation getInformationBlock(ProfilerBaseData value) {
		if (value == null) return null;
		// コード行情報
		CodeLine line = value.getCodeLine();
		IBlock block = value.getBlock();
		if (block == null) {
			LanguageUtils utils = new LanguageUtils(this.controller.getFortranLanguage());
		    IBlock[] blocks = utils.getCodeLineBlocks(line);
		    if (blocks == null || blocks.length <= 0) return null;
		    if (!(blocks[0] instanceof IInformation)) return null;
		    block = blocks[0];
		    value.setBlock(block);
		}
		if (block == null) return null;
	    IInformation info = (IInformation)block;

	    return info;
	}
}



