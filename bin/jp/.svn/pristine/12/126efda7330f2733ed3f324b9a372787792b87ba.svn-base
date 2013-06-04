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

package jp.riken.kscope.language;

import java.io.Serializable;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
/**
 * ソースコード中の行情報を保持するクラス。
 *
 * @author RIKEN
 *
 */
public class Statement implements Serializable {
	/** シリアル番号 */
	private static final long serialVersionUID = 6565980998601169008L;
    /** コード行ラベルデフォルト値 */
    public final static String NO_LABEL = "no_label";
    /** コード行ラベル */
    private String label = NO_LABEL;

    /**
     * コード行情報 ソースコード行文字列、ファイル開始・終了行番号、ソースファイルの情報を持つ。
     */
    CodeLine lineInfo;

    /**
     * コンストラクタ
     *
     * @param lineInfo
     *            コード行情報
     */
    public Statement(CodeLine lineInfo) {
        this.lineInfo = lineInfo;
        label = NO_LABEL;
    }

    protected void set_label(String str) {
        label = str;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * コード行文字列を取得する。
     *
     * @return コード行文字列
     */
    public String get_statement() {
        return this.lineInfo.getStatement();
    }

    /**
     * コード行開始ファイル行番号を取得する。
     *
     * @return コード行開始ファイル行番号
     */
    public Integer get_pos() {
        return this.lineInfo.getStartLine();
    }

    /**
     * コード行開始ファイル行番号を取得する。
     *
     * @return コード行開始ファイル行番号
     */
    public Integer get_start_pos() {
        return this.lineInfo.getStartLine();
    }

    /**
     * コード行終了ファイル行番号を取得する。
     *
     * @return コード行終了ファイル行番号
     */
    public Integer get_end_pos() {
        return this.lineInfo.getEndLine();
    }

    /**
     * ソースファイルを取得する。
     *
     * @return ソースファイル
     */
    public SourceFile get_sourcefile() {
        return this.lineInfo.getSourceFile();
    }

    /**
     * 行ラベルを取得する
     *
     * @return 行ラベル
     */
    public String get_label() {
        return label;
    }

    /**
     * 行ラベルが存在しているかチェックする。
     *
     * @return true:行ラベルあり/false:行ラベルなし
     */
    protected boolean is_labeled() {
        if (label == null) return false;
        return (!label.equals(NO_LABEL));
    }

	/**
	 * コード行情報を取得する.
	 * @return lineInfo		コード行情報
	 */
	public CodeLine getLineInfo() {
		return this.lineInfo;
	}

}
