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

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 * ログフォーマッター
 * 
 * @author riken
 * 
 */
public class LogFormatter extends Formatter {

	/**
	 * ログ出力メッセージのフォーマットを設定する。
	 */
	@Override
	public String format(LogRecord record) {
		return String.format("[%tF %tT] %s : %s%n", record.getMillis(),
				record.getMillis(), record.getLevel().getName(),
				record.getMessage());
	}
}
