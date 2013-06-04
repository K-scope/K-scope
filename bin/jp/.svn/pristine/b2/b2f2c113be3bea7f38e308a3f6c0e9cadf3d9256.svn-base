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

import jp.riken.kscope.common.ACCESSMEMORY_TYPE;


/**
 * 要求Byte/FLOPデータクラス
 * @author riken
 */
public class Memoryband {

    /** アクセス先タイプ */
    private ACCESSMEMORY_TYPE type;
    /** アクセス先名称 */
    private String name;
    /** アクセス先背景色 */
    private Color backColor;
    /** スループット:ストアあり */
    private float throughput_store;
    /** スループット:ストアなし */
    private float throughput_nonestore;
    /** 係数 */
    private float coef;
    /** 要求B/F算出 */
    private boolean required;
    /** 律速 */
    private boolean limiting;
    /** 有効/無効フラグ */
    private boolean enabled;

    /**
     * コンストラクタ
     * @param type		アクセス先タイプ
     */
    public Memoryband(ACCESSMEMORY_TYPE type) {
        this.setType(type);
    }

    /**
     * アクセス先背景色を取得する.
     * @return アクセス先背景色
     */
    public Color getBackColor() {
        return backColor;
    }

    /**
     * アクセス先背景色を設定する
     * @param color アクセス先背景色
     */
    public void setBackColor(Color color) {
        this.backColor = color;
    }

    /**
     * スループット:ストアありを取得する.
     * @return		スループット:ストアあり
     */
	public float getThroughputStore() {
		return throughput_store;
	}

	/**
	 * スループット:ストアありを設定する
	 * @param throughput_store	スループット:ストアあり
	 */
	public void setThroughputStore(float throughput_store) {
		this.throughput_store = throughput_store;
	}

	/**
	 * スループット:ストアなしを取得する.
	 * @return		スループット:ストアなし
	 */
	public float getThroughputNonestore() {
		return throughput_nonestore;
	}

	/**
	 * スループット:ストアなしを設定する.
	 * @param throughput_nonestore		スループット:ストアなし
	 */
	public void setThroughputNonestore(float throughput_nonestore) {
		this.throughput_nonestore = throughput_nonestore;
	}

	/**
	 * 係数を取得する.
	 * @return		係数
	 */
	public float getCoef() {
		return coef;
	}

	/**
	 * 係数を設定する.
	 * @param coef		係数
	 */
	public void setCoef(float coef) {
		this.coef = coef;
	}

	/**
	 * 要求B/F算出フラグを取得する.
	 * @return	要求B/F算出フラグ
	 */
	public boolean isRequired() {
		return required;
	}

	/**
	 * 要求B/F算出フラグを設定する.
	 * @param required	要求B/F算出フラグ
	 */
	public void setRequired(boolean required) {
		this.required = required;
	}

	/**
	 * 律速フラグを取得する.
	 * @return		律速フラグ
	 */
	public boolean isLimiting() {
		return limiting;
	}

	/**
	 * 律速フラグを設定する.
	 * @param limiting		律速フラグ
	 */
	public void setLimiting(boolean limiting) {
		this.limiting = limiting;
	}

	/**
	 * アクセス先タイプを取得する
	 * @return		アクセス先タイプ
	 */
	public ACCESSMEMORY_TYPE getType() {
		return type;
	}

	/**
	 * アクセス先タイプを設定する
	 * @param type		アクセス先タイプ
	 */
	public void setType(ACCESSMEMORY_TYPE type) {
		this.type = type;
	}

	/**
	 * 有効/無効フラグを取得する.
	 * @return		有効/無効フラグ
	 */
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * 有効/無効フラグを設定する.
	 * @param enabled		有効/無効フラグ
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * アクセス先名称を設定する
	 * @return		アクセス先名称
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * アクセス先名称を取得する
	 * @return		アクセス先名称
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * アクセス先名称
	 */
	@Override
	public String toString() {
		return getName();
	}

}


