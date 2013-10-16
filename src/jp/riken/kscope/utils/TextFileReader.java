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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.Charset;

/**
 * テキストファイルリーダクラス.<br/>
 * 文字コードを判別して読み込む
 * 
 * @author riken
 */
public class TextFileReader {
	/** バッファーサイズ(KB)*1024 = 1MB */
	private final int FILEIO_BUFFERSIZE = 1000;
	/** 読込ファイル */
	private File m_file = null;
	/** 読込バッファー */
	private byte[] m_buffer = null;
	/** ファイルの文字セット */
	private Charset m_charset = null;
	/** 次の読込バッファーのインデックス */
	private int m_nextIdx = 0;

	/**
	 * コンストラクタ
	 * 
	 * @param path
	 *            ファイルパス
	 */
	public TextFileReader(String path) {
		if (path == null)
			return;
		this.m_file = new File(path);
	}

	/**
	 * コンストラクタ
	 * 
	 * @param file
	 *            読込ファイル
	 */
	public TextFileReader(File file) {
		this.m_file = file;
	}

	/**
	 * ファイルから読み込み、ファイルデータを文字列で返す。
	 * 
	 * @throws IOException
	 *             ファイル読込例外
	 */
	public void readFile() throws IOException {
		if (m_file == null)
			return;

		int bufferSize = FILEIO_BUFFERSIZE * 1024;

		// ソースファイルを読み込む
		m_buffer = new byte[(int) m_file.length()];
		FileInputStream fis = new FileInputStream(m_file);
		BufferedInputStream bis = new BufferedInputStream(fis, bufferSize);
		bis.read(m_buffer);
		fis.close();

		// 読込データの文字コードチェック
		String enc = StringUtils.getDetectedCharset(m_buffer);

		if (enc == null) {
			m_charset = Charset.defaultCharset();
		} else {
			m_charset = Charset.forName(enc);
		}
		return;
	}

	/**
	 * 読込ファイルのすべてのテキストデータを取得する。
	 * 
	 * @return テキストデータ
	 * @throws IOException
	 *             ファイル読込エラー
	 */
	public String getFileText() throws IOException {
		if (m_buffer == null || m_charset == null) {
			readFile();
		}
		if (m_buffer == null || m_charset == null)
			return null;
		return new String(m_buffer, m_charset);
	}

	/**
	 * 読込ファイルから１行ずつテキストデータを取得する。
	 * 
	 * @return １行テキストデータ
	 * @throws IOException
	 *             ファイル読込エラー
	 */
	public String readLine() throws IOException {
		if (m_buffer == null || m_charset == null) {
			readFile();
		}

		if (m_buffer == null || m_charset == null)
			return null;
		int len = m_buffer.length;
		if (len <= m_nextIdx)
			return null;

		int startIdx = m_nextIdx;
		int nextIdx = len - 1;
		for (int i = m_nextIdx; i < len; i++) {
			byte c = m_buffer[i];
			if (c == '\n') {
				nextIdx = i;
				m_nextIdx = i + 1;
				break;
			} else if (c == '\r') {
				nextIdx = i;
				m_nextIdx = i + 1;
				if (len > i + 1 && m_buffer[i + 1] == '\n') {
					m_nextIdx++;
				}
				break;
			} else {
				m_nextIdx = i + 1;
			}
		}

		if (startIdx >= nextIdx)
			return new String();

		return new String(m_buffer, startIdx, nextIdx - startIdx, m_charset);
	}

	/**
	 * 読込バッファーを取得する。
	 * 
	 * @return 読込バッファー
	 */
	public byte[] getBuffer() {
		return m_buffer;
	}

	/**
	 * ファイルの文字セット
	 * 
	 * @return 文字セット
	 */
	public Charset getCharset() {
		return m_charset;
	}
}
