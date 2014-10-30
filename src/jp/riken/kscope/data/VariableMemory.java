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
package jp.riken.kscope.data;

import java.awt.Color;
import java.util.List;

import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * ソースコードの変数メモリデータクラス
 * @author RIKEN
 *
 */
public class VariableMemory extends Keyword {

	/** 変数 */
	private Variable variable;
	/** アクセス先メモリ */
	private RequiredBF requiredbf;

    /**
     * コンストラクタ
     * @param variable		変数
     * @param bf    アクセス先メモリ
     */
    public VariableMemory(Variable variable, RequiredBF bf) {
    	super(KEYWORD_TYPE.VARIABLE);
        this.variable = variable;
        this.requiredbf = bf;
        // 大文字・小文字の区別を設定する
        setSensitivecase(false);
        // 正規表現を設定する
        setRegex(true);
        // 単語検索を設定する
        setSearchWord(false);
        // 変数検索を設定する
        setSearchVariable(true);
        // 変数の正規表現
        String regex = createRegexPattern(variable);
        setKeyword(regex);
    }

	/**
	 * 変数を取得する.
	 * @return 変数
	 */
	public Variable getVariable() {
		return variable;
	}

	/**
	 * 変数を設定する
	 * @param variable 変数
	 */
	public void setVariable(Variable variable) {
		this.variable = variable;
	}

	/**
	 * 変数の文字列表現を正規表現に変換する.
	 * 変数の文字列表現に空白正規表現を追加する.
	 * @param variable		変数
	 * @return		変数の正規表現
	 */
	private String createRegexPattern(Variable variable) {
		if (variable == null) return null;
		String statement = variable.getVariableString();
		List<String> list = StringUtils.tokenizer(statement);
		if (list == null || list.size() <= 0) return null;
		String buf = "";
		for (String word : list) {
			if (word.isEmpty()) continue;
			if (word.trim().isEmpty()) continue;
			if (!buf.isEmpty()) buf += "[ ]*";
			if (KscopeProperties.DELIMITER_CHARS.indexOf(word) >= 0) {
				buf += "\\" + word;
			}
			else {
				buf += word;
			}
		}
		return buf;
	}

	/**
	 * 変数の背景色を取得する
	 */
	@Override
	public Color getBackgroundcolor() {
		if (requiredbf == null) return null;
		return requiredbf.getBackColor();
	}

	/**
	 * 変数のコード行情報を取得する.
	 */
	@Override
	public CodeLine getSearchLine() {
		if (this.variable == null) return null;
		IBlock parent = this.variable.getParentStatement();
		if (parent == null) return null;
		CodeLine start = parent.getStartCodeLine();
		CodeLine end = parent.getEndCodeLine();
		CodeLine line = new CodeLine(start, end);
		return line;
	}

	/**
	 * アクセス先メモリを取得する.
	 * @return アクセス先メモリ
	 */
	public RequiredBF getMemoryband() {
		return requiredbf;
	}

	/**
	 * アクセス先メモリを設定する
	 * @param bf    アクセス先メモリ
	 */
	public void setMemoryband(RequiredBF bf) {
		this.requiredbf = bf;
	}

}
