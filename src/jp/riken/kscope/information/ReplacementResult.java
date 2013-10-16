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

package jp.riken.kscope.information;

import jp.riken.kscope.language.IInformation;

/**
 * 新旧開始／終了位置とユーザが設定した情報との関連を表現するクラス。
 *
 * @author RIKEN
 *
 */
public class ReplacementResult {

	/** 付加情報差替結果 */
	public enum RESULT_STATUS {
		/** 失敗 */
		FAILURE,
		/** 成功 */
		SUCCESS,
		/** 不確実 */
		UNSURE
	}
	/** 付加情報差替結果 */
	private RESULT_STATUS status;
	/** 付加情報 */
    private InformationBase information;
    /** 新しいの開始位置 */
    private IInformation newStartPosition;
    /** 新しいの終了位置 */
    private IInformation newEndPosition;
    /** 古いの開始位置 */
    private IInformation oldStartPosition;
    /** 古いの終了位置 */
    private IInformation oldEndPosition;

    /**
     * コンストラクタ。
     * @param status    差替結果
     * @param info
     *           情報
     * @param newStartPos
     *           新しい（現在）の開始位置
     * @param newEndPos
     *           新しい（現在）の終了位置
     * @param oldStartPos
     *           古い（一つ前）の開始位置
     * @param oldEndPos
     *           古い（一つ前）の終了位置
     */
    public ReplacementResult(RESULT_STATUS status,
    		InformationBase info,
            IInformation newStartPos, IInformation newEndPos,
            IInformation oldStartPos, IInformation oldEndPos) {
    	this.status = status;
        this.information = info;
        this.newStartPosition = newStartPos;
        this.newEndPosition = newEndPos;
        this.oldStartPosition = oldStartPos;
        this.oldEndPosition = oldEndPos;
    }

    /**
     * コンストラクタ。
     *
     * @param info
     *           情報
     * @param newStartPos
     *           新しい（現在）の開始位置
     * @param newEndPos
     *           新しい（現在）の終了位置
     * @param oldStartPos
     *           古い（一つ前）の開始位置
     * @param oldEndPos
     *           古い（一つ前）の終了位置
     */
    public ReplacementResult(
    		InformationBase info,
            IInformation newStartPos, IInformation newEndPos,
            IInformation oldStartPos, IInformation oldEndPos) {
        this.information = info;
        this.newStartPosition = newStartPos;
        this.newEndPosition = newEndPos;
        this.oldStartPosition = oldStartPos;
        this.oldEndPosition = oldEndPos;
    }

    /**
     * ユーザが設定した情報を取得する。
     *
     * @return ユーザが設定した情報
     */
    public InformationBase getInformation() {
        return this.information;
    }

    /**
     * 新しい（現在）の開始位置を取得する。
     *
     * @return 新しい（現在）の開始位置
     */
    public IInformation getCurrentStartPosition() {
        return this.newStartPosition;
    }

    /**
     * 新しい（現在）の終了位置を取得する。
     *
     * @return 新しい（現在）の終了位置
     */
    public IInformation getCurrentEndPosition() {
        return this.newEndPosition;
    }

    /**
     * 古い（一つ前）の開始位置を取得する。
     *
     * @return 古い（一つ前）の開始位置
     */
    public IInformation getOldStartPosition() {
        return this.oldStartPosition;
    }

    /**
     * 古い（一つ前）の終了位置を取得する。
     *
     * @return 古い（一つ前）の終了位置
     */
    public IInformation getOldEndPosition() {
        return this.oldEndPosition;
    }

    /**
     * 差替結果を取得する
     * @return		差替結果
     */
	public RESULT_STATUS getStatus() {
		return status;
	}

    /**
     * 差替結果を設定する
     * @param	status		差替結果
     */
	public void setStatus(RESULT_STATUS status) {
		this.status = status;
	}

    /**
     * 新しい（現在）のIInformationを取得する。
     * @return 新しい（現在）IInformation
     */
    public IInformation getCurrentInformation() {
    	IInformation info = null;
    	if (this.newStartPosition == null) {
    		return null;
    	}
    	if (this.newStartPosition == this.newEndPosition) {
    		info = this.newStartPosition;
    	}
    	else {
    		info = new InformationBlock(this.information, this.newStartPosition, this.newEndPosition);
    	}
    	return info;
    }
}
