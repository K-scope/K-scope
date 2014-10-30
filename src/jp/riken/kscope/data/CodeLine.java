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

import java.io.Serializable;

import jp.riken.kscope.utils.StringUtils;

/**
 * ソースコードクラス
 *
 * @author RIKEN
 *
 */
public class CodeLine implements Comparable<CodeLine>, Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = 4602122606378044455L;

    /**
     * COMMENT:注釈・注釈文 STATEMENT:コード文 UNKNOWN:不明
     */
    public enum CODE_TYPE {
        /** コメント文 */
        COMMENT,
        /** ステータス文 */
        STATEMENT,
        /** 不明 */
        UNKNOWN
    };
    /** ソースファイル */
    private SourceFile m_sourceFile;
    /** ソースコード文 */
    private String m_statement;
    /** ファイル行番号（開始）:<=0の場合、行番号設定なし */
    private int m_startline;
    /** ファイル行番号（終了）:<=0の場合、行番号設定なし */
    private int m_endline;
    /** コード文タイプ */
    private CODE_TYPE m_type;
    /** ソースファイル（存在しなくてもＸＭＬ情報から格納） */
    private String m_strSourceFile;

    /**
     * コンストラクタ
     *
     * @param sourceFile
     *            ソースファイル
     * @param statement
     *            コード行
     * @param startline
     *            ファイル行番号（開始）
     * @param endline
     *            ファイル行番号（終了）
     * @param strSourceFile
     *            ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(SourceFile sourceFile, String statement, int startline, int endline, String strSourceFile) {
        m_sourceFile = sourceFile;
        m_statement = statement;
        m_startline = startline;
        m_endline = endline;
        m_type = CODE_TYPE.STATEMENT;
        m_strSourceFile = strSourceFile;
    }

    /**
     * コンストラクタ
     *
     * @param sourceFile
     *            ソースファイル
     * @param statement
     *            コード行
     * @param line
     *            ファイル行番号
     * @param strSourceFile
     *            ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(SourceFile sourceFile, String statement, int line, String strSouceFile) {
        this(sourceFile, statement, line, line, strSouceFile);
    }

    /**
     * コンストラクタ
     *
     * @param sourceFile          ソースファイル
     * @param statement           コード行
     * @param line            ファイル行番号
     * @param type            コードタイプ
     * @param strSourceFile		ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(SourceFile sourceFile, String statement, int line, CODE_TYPE type, String strSourceFile) {
        this(sourceFile, statement, line, strSourceFile);
        m_type = type;
    }

    /**
     * コンストラクタ
     *
     * @param statement
     *            コード行
     * @param strSourceFile
     *            ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(String statement) {
        this(null, statement, -1, null);
    }

    /**
     * コンストラクタ
     *
     * @param statement
     *            コード行
     * @param line
     *            ファイル行番号
     * @param strSourceFile
     *            ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(String statement, int line) {
        this(null, statement, line, null);
    }

    /**
     * コンストラクタ
     *
     * @param sourceFile        ソースファイル
     * @param startline         ファイル行番号（開始）
     * @param endline           ファイル行番号（終了）
     * @param strSourceFile		ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(SourceFile sourceFile, int startline, int endline, String strSourceFile) {
        m_sourceFile = sourceFile;
        m_statement = null;
        m_startline = startline;
        m_endline = endline;
        m_type = CODE_TYPE.UNKNOWN;
        m_strSourceFile = strSourceFile;
    }

    /**
     * コンストラクタ
     *
     * @param sourceFile        ソースファイル
     * @param strSourceFile		ソースファイル（存在しなくてもＸＭＬ情報から格納）
     */
    public CodeLine(SourceFile sourceFile, String strSourceFile) {
        m_sourceFile = sourceFile;
        m_statement = null;
        m_startline = 0;
        m_endline = 0;
        m_type = CODE_TYPE.UNKNOWN;
        m_strSourceFile = strSourceFile;
    }


    /**
     * コピーコンストラクタ
     * @param code        ソースコード
     */
    public CodeLine(CodeLine code) {
        m_sourceFile = new SourceFile(code.m_sourceFile);
        m_statement = code.m_statement;
        m_startline = code.m_startline;
        m_endline = code.m_endline;
        m_type = code.m_type;
        m_strSourceFile = code.m_strSourceFile;
    }

    /**
     * コピーコンストラクタ
     * @param start        開始コード行
     * @param end          終了コード行
     */
    public CodeLine(CodeLine start, CodeLine end) {
        this(start);
        if (end != null) {
            if (m_startline > end.getStartLine()) {
                m_startline = end.getStartLine();
            }
            if (m_endline < end.getEndLine()) {
                this.m_endline = end.getEndLine();
            }
        }
    }


    /**
     * ソースファイルを取得する。
     *
     * @return ソースファイル
     */
    public SourceFile getSourceFile() {
        return m_sourceFile;
    }

    /**
     * ソースファイルを設定する。
     *
     * @param sourceFile
     *            ソースファイル
     */
    public void setSourceFile(SourceFile sourceFile) {
        m_sourceFile = sourceFile;
    }

    /**
     * ソースコード行を取得する。
     *
     * @return ソースコード行
     */
    public String getStatement() {
        return m_statement;
    }

    /**
     * ソースコード行を設定する。
     *
     * @param statement
     *            ファイル行番号
     */
    public void setStatement(String statement) {
        m_statement = statement;
    }

    /**
     * ファイル行番号（開始）を取得する。
     *
     * @return ファイル行番号（開始）
     */
    public int getStartLine() {
        return m_startline;
    }

    /**
     * ファイル行番号（開始）を設定する。
     *
     * @param line
     *            ファイル行番号（開始）
     */
    public void setLine(int line) {
        m_startline = line;
    }

    /**
     * ファイル行番号（終了）を取得する。
     *
     * @return ファイル行番号（終了）
     */
    public int getEndLine() {
        return m_endline;
    }

    /**
     * ファイル行番号（終了）を設定する。
     *
     * @param line
     *            ファイル行番号（終了）
     */
    public void setEndLine(int line) {
        m_endline = line;
    }

    /**
     * 注釈・注釈文であるコードタイプを設定する。
     */
    public void setCommentType() {
        m_type = CODE_TYPE.COMMENT;
    }

    /**
     * 注釈・注釈文タイプであるかチェックする。
     *
     * @return true:注釈・注釈文タイプ/false:注釈・注釈文タイプではない。
     */
    public boolean isCommentType() {
        return (m_type == CODE_TYPE.COMMENT);
    }

    /**
     * コード文タイプであるかチェックする。
     *
     * @return true:コード文タイプ/false:コード文タイプではない。
     */
    public boolean isStatementType() {
        return (m_type == CODE_TYPE.STATEMENT);
    }

    /**
     * コード行に追加する。
     *
     * @param code
     *            追加コード
     * @param lineno
     *            追加コード行番号
     */
    public void appendLine(String code, int lineno) {
        if (code == null)
            return;
        if (code.trim().length() == 0)
            return;

        m_statement += code;
        m_endline = lineno;
    }

    /**
     * コード行をソートする。 ファイル開始行順にソートする。 同一行にコメントとコードが存在している場合は、コードを優先する。
     *
     * @param code
     *            コードオブジェクト
     * @return -1:行が小さい/0:行が同じ/1:行が大きい
     */
    @Override
    public int compareTo(CodeLine code) {
        if (this.m_startline < code.m_startline) {
            return -1;
        } else if (this.m_startline > code.m_startline) {
            return 1;
        }

        if (this.m_type == CODE_TYPE.STATEMENT
                && code.m_type == CODE_TYPE.COMMENT) {
            return -1;
        }
        if (this.m_type == CODE_TYPE.COMMENT
                && code.m_type == CODE_TYPE.STATEMENT) {
            return 1;
        }

        return 0;
    }

    /**
     * コード行の情報を文字列として返す。
     *
     * @return コード行の情報
     */
    @Override
	public String toString() {

        String text = this.getStatement();
        String prefix = String.valueOf(this.getStartLine());
        text = prefix + " : " + text;

        return text;
    }

    /**
     * コード行の詳細情報を文字列として返す。
     *
     * @return コード行の情報
     */
    public String toDetailString() {
        StringBuilder buf = new StringBuilder();

        if (m_startline > 0) {
            buf.append("[code ");
            buf.append(m_startline);
            if (m_startline < m_endline) {
                buf.append(":");
                buf.append(m_endline);
                buf.append("] ");
            }
            buf.append("] ");
        } else {
            buf.append("[code] ");
        }
        // ソースコード文
        buf.append(m_statement);

        // ソースファイル
        if (m_sourceFile != null) {
            buf.append("\n");
            buf.append("[file] ");
            buf.append(m_sourceFile.toString());
        }
        else {
        	buf.append("\n");
            buf.append("[file(not exist)] ");
            buf.append(m_strSourceFile);
        }

        return buf.toString();
    }

    /**
     * コード行の情報を文字列として返す。
     *
     * @return コード行の情報
     */
    public String getLineInfo() {
        StringBuilder buf = new StringBuilder();

        buf.append("[");
        // ソースファイル
        if (m_sourceFile != null) {
            String file = m_sourceFile.toString();
            file = String.format("%-16s", file);
            buf.append(file);
            buf.append(":");
        }
        else {
        	String file = m_strSourceFile;
            file = String.format("%-16s", file);
            file += "(not exist)";
            buf.append(file);
            buf.append(":");
        }

        if (m_startline >= 0) {
            String lineno = String.format("%06d", m_startline);
            buf.append(lineno);
        }
        buf.append("] ");

        // ソースコード文
        buf.append(m_statement);

        return buf.toString();
    }

    /**
     * 開始、終了行番号の情報を文字列として返す。
     *
     * @return 開始、終了行番号
     */
    public String getLineno() {
        StringBuilder buf = new StringBuilder();

        if (m_startline > 0) {
            buf.append(m_startline);
        }
        if (m_endline > 0) {
            buf.append(":");
            buf.append(m_endline);
        }

        return buf.toString();
    }

    /**
     * 同じコード行であるかチェックする
     * @param code		チェック対象コード行
     * @return			true=一致
     */
    @Override
    public boolean equals(Object code) {
        if (!(code instanceof CodeLine)) return false;
        if (this.m_sourceFile != null) {
            if (!this.m_sourceFile.equals(((CodeLine)code).m_sourceFile)) {
                return false;
            }
        }
        if (this.m_startline != ((CodeLine)code).m_startline) {
            return false;
        }
        if (this.m_endline != ((CodeLine)code).m_endline) {
            return false;
        }
        if (this.m_statement != null) {
            if (!this.m_statement.equalsIgnoreCase(((CodeLine)code).m_statement)) {
                return false;
            }
        }

        return true;
    }

    /**
     * ソースファイル名を取得する
     * @return			ソースファイル名（存在しないファイルでも設定があればかえす）
     */
    public String getStrSourceFile() {
    	if (m_statement != null) {
			if (m_statement.startsWith("COMMON")){
    			String [] array = m_statement.split("/", 0);
    			return StringUtils.join(array, "/", array.length - 1);
			}
    	}
    	if (m_sourceFile != null && m_sourceFile.getPath() != null) {
    		return m_sourceFile.getPath();
    	}
    	return m_strSourceFile;
    }

    /**
     * ソースファイル名を設定する
     * @param			ソースファイル名（ヘッダーなどの存在しないファイルでも設定があれば設定する）
     */
    public void setStrSourceFile(String strSourceFile) {
    	m_strSourceFile = strSourceFile;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += this.m_startline;
        hash += this.m_endline;
        return hash;
    }

    /**
     * 開始、終了行が重なりあうかチェックする.
     * @param start		開始行
     * @param end		終了行
     * @return			true=重なりあう
     */
    public boolean isOverlap(CodeLine start, CodeLine end) {
		if (this.getSourceFile() == null) return false;
		if (start == null) return false;
		if (end == null) return false;
		if (!this.getSourceFile().equals(start.getSourceFile())) return false;
		if (!this.getSourceFile().equals(end.getSourceFile())) return false;

		if ( (this.getStartLine() >= start.getStartLine()
			&& this.getStartLine() <= start.getEndLine())
			|| (this.getEndLine() >= end.getStartLine()
					&& this.getEndLine() <= end.getEndLine()) ) {
			return true;
		}
		else if (this.getStartLine() <= start.getStartLine()
				&& this.getEndLine() >= end.getEndLine()) {
			return true;
		}
		return false;
    }
}
