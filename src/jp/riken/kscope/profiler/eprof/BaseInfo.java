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
 * Basic information
 * @author RIKEN
 */
public class BaseInfo {
    /** Counter call count int */
    private int callCount;
    /** Elapsed time float (ms) */
    private float elapsTime;
    /** User CPU time float (ms) */
    private float userTime;
    /** System CPU time float (ms) */
    private float systemTime;
    
    
    /**
     * Constructor
     */
    public BaseInfo() {
    }
    
    
    /**
     * Number of counter calls
     * @return Number of counter calls
     */
	public int getCallCount() {
		return callCount;
	}
	
	/**
* Number of counter calls
* @param count Number of counter calls
*/
	public void setCallCount(int count) {
		this.callCount = count;
	}
	
	/**
	 * elapsed time
* @return elapsed time
*/
	public float getElapsTime() {
		return elapsTime;
	}
	/**
	 * elapsed time
* @param time elapsed time
*/
	public void setElapsTime(float time) {
		this.elapsTime = time;
	}
	
	/**
* User CPU time
* @return user CPU time
*/
	public float getUserTime() {
		return userTime;
	}
	
	/**
* User CPU time
* @param time User CPU time
*/
	public void setUserTime(float time) {
		this.userTime = time;
	}
	
	/**
* System CPU time
* @return system CPU time
*/
	public float getSystemTime() {
		return systemTime;
	}
	
	/**
* System CPU time
* @param time System CPU time
*/
	public void setSystemTime(float time) {
		this.systemTime = time;
	}


}



