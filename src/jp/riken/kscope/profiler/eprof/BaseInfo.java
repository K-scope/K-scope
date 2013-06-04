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
package jp.riken.kscope.profiler.eprof;

/**
 * 基本情報
 * @author riken
 */
public class BaseInfo {
    /** カウンタの呼び出し回数	int */
    private int callCount;
    /** 経過時間	float	(ms) */
    private float elapsTime;
    /** ユーザCPU時間	float	(ms) */
    private float userTime;
    /** システムＣＰＵ時間	float	(ms) */
    private float systemTime;
    
    
    /**
     * コンストラクタ
     */
    public BaseInfo() {
    }
    
    
    /**
     * カウンタの呼び出し回数
     * @return		カウンタの呼び出し回数
     */
	public int getCallCount() {
		return callCount;
	}
	
	/**
	 * カウンタの呼び出し回数
	 * @param count		カウンタの呼び出し回数
	 */
	public void setCallCount(int count) {
		this.callCount = count;
	}
	
	/**
	 * 経過時間
	 * @return		経過時間
	 */
	public float getElapsTime() {
		return elapsTime;
	}
	/**
	 * 経過時間
	 * @param time		経過時間
	 */
	public void setElapsTime(float time) {
		this.elapsTime = time;
	}
	
	/**
	 * ユーザCPU時間
	 * @return		ユーザCPU時間
	 */
	public float getUserTime() {
		return userTime;
	}
	
	/**
	 * ユーザCPU時間
	 * @param time		ユーザCPU時間
	 */
	public void setUserTime(float time) {
		this.userTime = time;
	}
	
	/**
	 * システムＣＰＵ時間
	 * @return		システムＣＰＵ時間
	 */
	public float getSystemTime() {
		return systemTime;
	}
	
	/**
	 * システムＣＰＵ時間
	 * @param time		システムＣＰＵ時間
	 */
	public void setSystemTime(float time) {
		this.systemTime = time;
	}


}



