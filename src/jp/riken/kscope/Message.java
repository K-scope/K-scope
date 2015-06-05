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
package jp.riken.kscope;

import java.util.Enumeration;

/**
 * 英語、日本語メッセージクラス.
 * シングルトンクラス
 */
public class Message {
	/** プロパティファイル */
	private static final String PROPERTIES_FILE = "jp.riken.kscope.message";
	/** プロパティリソース */
	private java.util.ResourceBundle bundle = null;
	/** Messageクラスインスタンス */
    private static Message instance = new Message();
    private static boolean debug=(System.getenv("DEBUG")!= null && System.getenv("DEBUG").equals("high"));

    /**
     * コンストラクタ
     */
    private Message() {
    	this.bundle = java.util.ResourceBundle.getBundle(PROPERTIES_FILE);
    }

    /**
     * Messageクラスインスタンスを取得する.
     * @return		Messageクラスインスタンス
     */
    public static Message getInstance() {
        return instance;
    }

    /**
     * プロパティファイルからリソースを作成する.
     * @return   プロパティフリソース
     */
	private static java.util.ResourceBundle getBundle() {
        try {
    		if (instance == null) instance = new Message();
    		if (debug) {
	    		System.out.println("ResourceBundle: ");
	    		for (Enumeration<String> e=instance.bundle.getKeys(); e.hasMoreElements();) {
	    			System.out.println(e.nextElement());
	    		}
    		}
            return instance.bundle;
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;
	}

	/**
	 * キーのメッセージを取得する.
	 * @param key	キー
	 * @return		メッセージ文字列
	 */
	public static String getString(String key) {
		if (key == null || key.isEmpty()) return null;
		if (getBundle() == null) return null;
		try {
			String msg = getBundle().getString(key);
			return msg;
		} catch (Exception ex) {
			System.err.println("Error getting message for key "+ key);
            StackTraceElement[] ste= ex.getStackTrace();
            int maxelements=10;
            for (int i=0; i< maxelements; i++) {
            	System.err.println(ste[i].toString());
            }
			//ex.printStackTrace();
			return key;
		}
	}

	/**
	 * キーの書式文字列からメッセージする.
	 * @param key	キー
	 * @param args		書式パラメータ
	 * @return		メッセージ文字列
	 */
	public static String getString(String key, Object... args) {
		if (key == null || key.isEmpty()) return null;
		if (getBundle() == null) return null;
		try {
			String format = getBundle().getString(key);
			String msg = String.format(format, (Object[])args);
			return msg;
		} catch (Exception ex) {
            ex.printStackTrace();
			return key;
		}
	}

	/**
	 * メッセージのキーを取得する.
	 * @param message	メッセージ文字列
	 * @return		キー
	 */
	public static String getKey(String message) {
		if (message == null || message.isEmpty()) return null;
		if (getBundle() == null) return null;
		try {
			Enumeration<String> keys = getBundle().getKeys();
			while (keys.hasMoreElements()) {
			    String key = keys.nextElement();
				String value = getBundle().getString(key);
			    if (message.equals(value)) {
			    	return key;
			    }
			}
		} catch (Exception ex) {
            ex.printStackTrace();
		}
		return null;
	}

	/**
	 * メッセージのキーが存在するかチェックする.
	 * @param key	キー
	 * @return		true=キーが存在する
	 */
	public static boolean containsKey(String key) {
		if (key == null || key.isEmpty()) return false;
		if (getBundle() == null) return false;
		return getBundle().containsKey(key);
	}
}


