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

/**
 * プロジェクトプロパティ設定値クラス
 * @author RIKEN
 */
public class ProjectPropertyValue {
	private String type;
	private String key;
	private String value;
	private String name;
	private String message;
	private String commandline_option;
	private int order;
	
	/**
	 * コンストラクタ
	 * @param type		タイプ
	 * @param key		キー
	 * @param value		値
	 * @param name		名前
	 * @param message	メッセージ
	 */
	public ProjectPropertyValue(String key, String type,  String name, String value, String message, String CLO, int order) {
		this.type = type;
		this.key = key;
		this.value = value;
		this.name = name;
		this.message = message;
		this.commandline_option = CLO;
		this.order = order;
	}
	
	public void setProperty(String k,String v, String co, int order) {
		this.key = k;
		this.value = v;
		this.commandline_option = co;
		this.order = order;
	}

	/**
	 * タイプを取得する.
	 * @return		タイプ
	 */
	public String getType() {
		return type;
	}

	/**
	 * タイプを設定する.
	 * @param type		タイプ
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * キーを取得する.
	 * @return		キー
	 */
	public String getKey() {
		return key;
	}

	/**
	 * キーを設定する.
	 * @param key		キー
	 */
	public void setKey(String key) {
		this.key = key;
	}

	/**
	 * 値を取得する.
	 * @return		値
	 */
	public String getValue() {
		return value;
	}

	/**
	 * 値を設定する.
	 * @param value		値
	 */
	public void setValue(String value) {
		this.value = value;
	}

	/**
	 * 名前を取得する.
	 * @return		名前
	 */
	public String getName() {
		return name;
	}

	/**
	 * 名前を設定する.
	 * @param name		名前
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * メッセージを取得する.
	 * @return		メッセージ
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * メッセージを設定する.
	 * @param message		メッセージ
	 */
	public void setMessage(String message) {
		this.message = message;
	}
	
	public String getCommandlineOption() {
		return this.commandline_option;
	}
	
	
	public int getOrder() {
		return this.order;
	}
	
	public String toString() {
		return "K:"+ this.key + ", T:" + this.type + ", N:" + this.name + ", V:" + this.value 
				+ ", M:" + this.message	+ ", CL:" + this.commandline_option + ", O:" + this.order;
	}
	
}
