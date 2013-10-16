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

package jp.riken.kscope.utils;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * 言語構文分割クラス
 * 
 * @author riken
 * 
 */
public class LanguageTokenizer {

	/**
	 * 読込ソースコードリーダー
	 */
	private Reader reader = null;

	/**
	 * 分割文字格納バッファ
	 */
	private StringBuffer buf = new StringBuffer();

	/**
	 * 区切り文字リスト
	 */
	private ArrayList<String> m_delimList = new ArrayList<String>();

	/**
	 * 次の読込フラグ
	 */
	private int peekc = NEED_CHAR;
	private static final int NEED_CHAR = Integer.MAX_VALUE;
	private static final int SKIP_LF = Integer.MAX_VALUE - 1;

	/** ライン番号 */
	private int LINENO = 1;

	/**
	 * EOL(改行)区切りフラグ true:EOL(改行)を区切りとする false:EOL(改行)を区切りとしない。
	 */
	private boolean eolIsSignificantP = false;

	/** C言語コメント("//")をコメントとする */
	private boolean slashSlashCommentsP = false;
	/** C言語コメント("/*")をコメントとする */
	private boolean slashStarCommentsP = false;
	/** 小括弧 '(',')'を対の区切り文字とする */
	private boolean parenthesisQuote = false;

	/**
	 * ACSII文字の文節設定リスト CT_WHITESPACE,CT_ALPHA,CT_QUOTE,CT_COMMENTがOR値で設定する。
	 */
	private byte ctype[] = new byte[256];
	/** スペースに置き換える文字 */
	private static final byte CT_WHITESPACE = 1;
	/** 単語とする文字 */
	private static final byte CT_ALPHA = 4;
	/** 囲み単語とする文字 */
	private static final byte CT_QUOTE = 8;
	/** コメントとする文字（未使用） */
	private static final byte CT_COMMENT = 16;

	/**
	 * 現在文字の文節設定
	 */
	public int ttype = LT_NOTHING;

	/** EOF（読込リーダーの終端） */
	public static final int LT_EOF = -1;

	/** EOL（読込行末） */
	public static final int LT_EOL = '\n';

	/** 単語文字列 */
	public static final int LT_WORD = -3;

	/** 未定 */
	private static final int LT_NOTHING = -4;

	/** 囲み単語 */
	public static final int LT_QUOTE = -9;

	/** 区切り文字 */
	public static final int LT_DELIM = -10;

	/** 括弧区切り */
	public static final int LT_PARENTHESIS = -11;

	/**
	 * 文節区切りによる文字列
	 */
	public String sval;

	/**
	 * コンストラクタ
	 */
	private LanguageTokenizer() {
		clearToken();
		// wordChars('a', 'z');
		// wordChars('A', 'Z');
		// wordChars(128 + 32, 255);
		whitespaceChars(0, ' ' - 1);
		quoteChar('"');
		quoteChar('\'');
		eolIsSignificant(true);
	}

	/**
	 * コンストラクタ
	 * 
	 * @param r
	 *            読込ソースリーダー
	 */
	public LanguageTokenizer(Reader r) {
		this();
		if (r == null) {
			throw new NullPointerException();
		}
		reader = r;
	}

	/**
	 * コンストラクタ
	 * 
	 * @param str
	 *            読込ソースコード文字列
	 */
	public LanguageTokenizer(String str) {
		this();
		if (str == null) {
			throw new NullPointerException();
		}
		reader = new StringReader(str);
	}

	/**
	 * 文節文字をリセットする
	 */
	public void resetSyntax() {
		for (int i = ctype.length; --i >= 0;)
			ctype[i] = 0;
	}

	/**
	 * 文節文字をデフォルト設定にする
	 */
	public void clearToken() {
		for (int i = ctype.length; --i >= 0;)
			ctype[i] = CT_ALPHA;
	}

	/**
	 * Specifies that all characters <i>c</i> in the range
	 * <code>low&nbsp;&lt;=&nbsp;<i>c</i>&nbsp;&lt;=&nbsp;high</code> are word
	 * constituents. A word token consists of a word constituent followed by
	 * zero or more word constituents or number constituents.
	 * 
	 * @param low
	 *            the low end of the range.
	 * @param hi
	 *            the high end of the range.
	 */
	public void wordChars(int low, int hi) {
		if (low < 0)
			low = 0;
		if (hi >= ctype.length)
			hi = ctype.length - 1;
		while (low <= hi) {
			ctype[low++] &= ~CT_WHITESPACE;
			ctype[low++] |= CT_ALPHA;
		}
	}

	/**
	 * 単語文字を設定する
	 * 
	 * @param low
	 *            単語文字コード
	 */
	public void wordChar(int low) {
		if (low < 0)
			low = 0;
		if (low >= ctype.length)
			low = ctype.length - 1;
		ctype[low] &= ~CT_WHITESPACE;
		ctype[low] |= CT_ALPHA;
	}

	/**
	 * スペース文字範囲を設定する
	 * 
	 * @param low
	 *            下限スペース文字コード
	 * @param hi
	 *            上限スペース文字コード
	 */
	public void whitespaceChars(int low, int hi) {
		if (low < 0)
			low = 0;
		if (hi >= ctype.length)
			hi = ctype.length - 1;
		while (low <= hi)
			ctype[low++] = CT_WHITESPACE;
	}

	/**
	 * スペース文字を設定する
	 * 
	 * @param ch
	 *            スペース文字コード
	 */
	public void whitespaceChar(int ch) {
		if (ch >= 0 && ch < ctype.length)
			ctype[ch] |= CT_WHITESPACE;
	}

	/**
	 * 文節文字としない文字範囲を設定する
	 * 
	 * @param low
	 *            下限設定なし文字コード
	 * @param hi
	 *            上限設定なし文字コード
	 */
	public void ordinaryChars(int low, int hi) {
		if (low < 0)
			low = 0;
		if (hi >= ctype.length)
			hi = ctype.length - 1;
		while (low <= hi)
			ctype[low++] = 0;
	}

	/**
	 * 文節文字としない文字を設定する
	 * 
	 * @param ch
	 *            設定なし文字コード
	 */
	public void ordinaryChar(int ch) {
		if (ch >= 0 && ch < ctype.length)
			ctype[ch] = 0;
	}

	/**
	 * コメント文字を設定する
	 * 
	 * @param ch
	 *            コメント文字
	 */
	public void commentChar(int ch) {
		if (ch >= 0 && ch < ctype.length)
			ctype[ch] = CT_COMMENT;
	}

	/**
	 * 引用符文字を設定する。 通常("), (')を設定する。
	 * 
	 * @param ch
	 *            引用符文字
	 */
	public void quoteChar(int ch) {
		if (ch >= 0 && ch < ctype.length)
			ctype[ch] = CT_QUOTE;
	}

	/**
	 * 引用符文字の設定を解除する。
	 * 
	 * @param ch
	 *            引用符文字
	 */
	public void unQuoteChar(int ch) {
		if (ch >= 0 && ch < ctype.length)
			ctype[ch] &= ~CT_QUOTE;
	}

	/**
	 * 改行文字を文節区切りとするか設定する
	 * 
	 * @param flag
	 *            true=改行文字を文節区切りとする
	 */
	public void eolIsSignificant(boolean flag) {
		eolIsSignificantP = flag;
	}

	/**
	 * C言語コメント("/*")をコメントとするか設定する
	 * 
	 * @param flag
	 *            true=C言語コメント("/*")をコメントとする
	 */
	public void slashStarComments(boolean flag) {
		slashStarCommentsP = flag;
	}

	/**
	 * C言語コメント("/*")をコメントとするか設定する
	 * 
	 * @param flag
	 *            true=C言語コメント("/*")をコメントとする
	 */
	public void slashSlashComments(boolean flag) {
		slashSlashCommentsP = flag;
	}

	/**
	 * Read the next character
	 * 
	 * @return 読込文字コード
	 */
	private int read() throws IOException {
		if (reader != null)
			return reader.read();
		else
			throw new IllegalStateException();
	}

	/**
	 * 次の文節区切り文字を取得する
	 * 
	 * @return 文節区切り文字コード
	 * @throws IOException
	 *             読込エラー
	 */
	public int nextToken() throws IOException {

		byte ct[] = ctype;
		sval = null;

		int c = peekc;
		if (c < 0)
			c = NEED_CHAR;
		if (c == SKIP_LF) {
			c = read();
			if (c < 0)
				return ttype = LT_EOF;
			if (c == '\n')
				c = NEED_CHAR;
		}
		if (c == NEED_CHAR) {
			c = read();
			if (c < 0)
				return ttype = LT_EOF;
		}
		ttype = c; /* Just to be safe */

		/*
		 * Set peekc so that the next invocation of nextToken will read another
		 * character unless peekc is reset in this invocation
		 */
		peekc = NEED_CHAR;

		// 読込文字の設定を取得する。
		int ctype = c < 256 ? ct[c] : CT_ALPHA;

		// CT_WHITESPACEのチェックを行う。
		// 設定文字を削除(スペース)とする。
		boolean existWhite = false;
		while ((ctype & CT_WHITESPACE) != 0) {
			existWhite = true;
			if (c == '\r') {
				LINENO++;
				if (eolIsSignificantP) {
					peekc = SKIP_LF;
					return ttype = LT_EOL;
				}
				c = read();
				if (c == '\n')
					c = read();
			} else {
				if (c == '\n') {
					LINENO++;
					if (eolIsSignificantP) {
						return ttype = LT_EOL;
					}
				}
				c = read();
			}
			if (c < 0)
				return ttype = LT_EOF;
			ctype = c < 256 ? ct[c] : CT_ALPHA;
		}

		// WHITESPACEが存在した場合、１つのスペースを挿入する。
		if (existWhite) {
			appendBuffer(' ');
		}

		// CT_ALPHAのチェックを行う。
		// １つの文字列として扱う
		if ((ctype & CT_ALPHA) != 0) {
			do {
				appendBuffer(c);
				c = read();
				ctype = c < 0 ? CT_WHITESPACE : c < 256 ? ct[c] : CT_ALPHA;
			} while ((ctype & (CT_ALPHA)) != 0);
			peekc = c;
			sval = buf.toString();
			buf.delete(0, buf.length());
			// 文字列区切りの終了
			return ttype = LT_WORD;
		}

		// 小括弧を対の区切文字とする場合
		if (parenthesisQuote && c == '(') {
			appendBuffer(c);
			int paren = 0;
			int d = read();
			while (d >= 0 && d != '\n' && d != '\r') {
				ctype = d < 256 ? ct[d] : CT_ALPHA;
				if ((ctype & CT_QUOTE) != 0) {
					// 引用符に囲まれた文字を取得する。
					String quote = getQuote(d);
					appendBuffer(quote);
				} else if (d == '\\') {
					// バックスラッシュが存在した場合、次の文字は見ない。
					appendBuffer(d);
					c = read();
					appendBuffer(c);
				} else {
					c = d;
					// 右括弧のチェックを行う。
					if (d == ')') {
						// 左括弧個数と右括弧個数が一致したら終了する。
						if (paren <= 0) {
							break;
						}
						// 括弧カウントをデクリメントする。
						paren--;
					}
					// 左括弧のチェックを行う。
					if (d == '(') {
						// 再度左括弧が出現したので、括弧カウントをインクリメントする。
						paren++;
					}
					appendBuffer(c);
				}
				d = read();
			}
			appendBuffer(d);

			peekc = NEED_CHAR;
			sval = buf.toString();
			buf.delete(0, buf.length());
			return LT_PARENTHESIS;
		}

		// CT_QUOTEのチェックを行う。
		if ((ctype & CT_QUOTE) != 0) {
			// 引用符に囲まれた文字を取得する。
			String quote = getQuote(c);

			peekc = NEED_CHAR;
			sval = quote;
			buf.delete(0, buf.length());
			return LT_QUOTE;
		}

		// 区切り文字列をチェックする。
		String delim = null;
		if ((delim = isDelimiter(c)) != null) {
			sval = delim;
			peekc = NEED_CHAR;
			buf.delete(0, buf.length());
			return LT_DELIM;
		}

		if (c == '/' && (slashSlashCommentsP || slashStarCommentsP)) {
			c = read();
			if (c == '*' && slashStarCommentsP) {
				int prevc = 0;
				while ((c = read()) != '/' || prevc != '*') {
					if (c == '\r') {
						LINENO++;
						c = read();
						if (c == '\n') {
							c = read();
						}
					} else {
						if (c == '\n') {
							LINENO++;
							c = read();
						}
					}
					if (c < 0)
						return ttype = LT_EOF;
					prevc = c;
				}
				return nextToken();
			} else if (c == '/' && slashSlashCommentsP) {
				while ((c = read()) != '\n' && c != '\r' && c >= 0)
					;
				peekc = c;
				return nextToken();
			} else {
				/* Now see if it is still a single line comment */
				if ((ct['/'] & CT_COMMENT) != 0) {
					while ((c = read()) != '\n' && c != '\r' && c >= 0)
						;
					peekc = c;
					return nextToken();
				} else {
					peekc = c;
					return ttype = '/';
				}
			}
		}

		sval = String.valueOf((char) c);
		return ttype = c;
	}

	/**
	 * Return the current line number.
	 * 
	 * @return the current line number of this stream tokenizer.
	 */
	public int lineno() {
		return LINENO;
	}

	@Override
	public String toString() {
		String ret;
		switch (ttype) {
		case LT_EOF:
			ret = "EOF";
			break;
		case LT_EOL:
			ret = "EOL";
			break;
		case LT_WORD:
			ret = sval;
			break;
		case LT_NOTHING:
			ret = "NOTHING";
			break;
		default: {
			/*
			 * ttype is the first character of either a quoted string or is an
			 * ordinary character. ttype can definitely not be less than 0,
			 * since those are reserved values used in the previous case
			 * statements
			 */
			if (ttype < 256 && ((ctype[ttype] & CT_QUOTE) != 0)) {
				ret = sval;
				break;
			}

			char s[] = new char[3];
			s[0] = s[2] = '\'';
			s[1] = (char) ttype;
			ret = new String(s);
			break;
		}
		}
		return "Token[" + ret + "], line " + LINENO;
	}

	/**
	 * デリミタの設定を行う。
	 * 
	 * @param delim
	 *            デリミタ
	 */
	public void useDelimiter(String delim) {
		// デリミタの先頭文字はタイプからクリアする。
		char ch = delim.charAt(0);
		ordinaryChar(ch);
		m_delimList.add(delim);
	}

	/**
	 * デリミタの設定を行う。
	 * 
	 * @param delims
	 *            デリミタ文字リスト
	 */
	public void useDelimiters(String delims) {
		for (int i = 0; i < delims.length(); i++) {
			String delim = delims.substring(i, i + 1);
			useDelimiter(delim);
		}
	}

	/**
	 * デリミタの設定解除を行う。
	 * 
	 * @param delim
	 *            デリミタ
	 */
	public void unusedDelimiter(String delim) {
		// デリミタの先頭文字はWORD文字とする。
		char ch = delim.charAt(0);
		wordChar(ch);
		m_delimList.remove(delim);
	}

	/**
	 * デリミタ文字列と一致しているかチェックする。
	 * 
	 * @param c
	 *            現在読込文字
	 * @return デリミタ文字列と一致している場合デリミタ文字列を返す。/一致していない場合、nullを返す。
	 */
	private String isDelimiter(int c) {

		try {
			String delim = null;
			Iterator<String> itr = m_delimList.iterator();
			while (itr.hasNext()) {
				delim = itr.next();
				char ch = delim.charAt(0);
				if (c == ch) {
					// 先読みするためにマークをする。
					reader.mark(delim.length());
					boolean match = true;
					for (int i = 1; i < delim.length(); i++) {
						if (reader.read() != delim.charAt(i)) {
							match = false;
							break;
						}
					}
					if (!match) {
						// マーク位置に戻す。
						reader.reset();
						continue;
					} else {
						return delim;
					}
				}
			}
			return null;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * 小括弧'(',')'を対の区切り文字として設定する。
	 * 
	 * @param paren
	 *            true:小括弧'(',')'を対の区切り文字とする。
	 */
	public void setParenthesisQuote(boolean paren) {
		parenthesisQuote = paren;
		if (paren) {
			ctype['('] = 0;
			ctype[')'] = 0;
		} else {
			ctype['('] = CT_ALPHA;
			ctype[')'] = CT_ALPHA;
		}
	}

	/**
	 * 引用符文字列を取得する。
	 * 
	 * @param c
	 *            キャラクターコード
	 * @return 引用符文字列
	 */
	private String getQuote(int c) throws IOException {

		// 読込文字の設定を取得する。
		byte ct[] = ctype;
		int ctype = c < 256 ? ct[c] : CT_ALPHA;
		if ((ctype & CT_QUOTE) == 0)
			return null;

		// 引用符格納バッファー
		StringBuffer quote_buf = new StringBuffer();
		appendBuffer(quote_buf, c);

		// 引用符文字
		int quote_type = c;

		// 次の文字の取得
		int d = read();
		// 次に引用符、改行文字まで取得を行う。
		while (d >= 0 && d != quote_type && d != '\n' && d != '\r') {
			if (d == '\\') {
				// バックスラッシュが存在した場合、次の文字は見ない。
				appendBuffer(quote_buf, c);
				c = read();
				d = read();

			} else {
				c = d;
				d = read();
			}
			appendBuffer(quote_buf, c);
		}
		appendBuffer(quote_buf, d);

		return quote_buf.toString();
	}

	/**
	 * 分割バッファーにコードを追加する。
	 * 
	 * @param c
	 *            追加文字コード
	 */
	private void appendBuffer(int c) {
		appendBuffer(this.buf, c);
	}

	/**
	 * 分割バッファーにコードを追加する。
	 * 
	 * @param c
	 *            追加文字コード
	 */
	private void appendBuffer(StringBuffer add_buf, int c) {
		if (c <= 0)
			return;

		if (c != '\n' && c != '\r') {
			add_buf.appendCodePoint(c);
		}
	}

	/**
	 * 分割バッファーに文字列を追加する。
	 * 
	 * @param str
	 *            追加文字列
	 */
	private void appendBuffer(String str) {
		if (str == null || str.isEmpty())
			return;
		buf.append(str);
	}

}
