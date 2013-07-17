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
 * SSHconnect property data
 * 
 * @author peterbryzgalov
 *
 */
public class SSHconnectData {

	private String key;
	private String value;
	private String commandline_option;
	private boolean type;
	
	static boolean SYSTEM_TYPE = true;
	static boolean PROJECT_TYPE = false;
	static String SYSTEM = "system";
	static String PROJECT = "project";
	
	public void setProperty(String k,String v, String co, String t) {
		this.key = k;
		this.value = v;
		this.commandline_option = co;
		if (t.equalsIgnoreCase(SYSTEM)) this.type = SYSTEM_TYPE;
		else if (t.equalsIgnoreCase(PROJECT)) this.type = PROJECT_TYPE;
		else System.err.println("Invalid value for type of SSHconnectData property. It must be either "+SYSTEM+" or " + PROJECT+". Had: "+t+". Value is not set.");
	}
	
	public void setValue(String new_value) {
		this.value = new_value;
	}
	
	public String getKey() {
		return this.key;
	}
	
	public String getValue() {
		return this.value;
	}
	
	public String getCommandlineOption() {
		return this.commandline_option;
	}
	
	public String getType() {
		if (this.type == SSHconnectData.SYSTEM_TYPE) return SSHconnectData.SYSTEM;
		return SSHconnectData.PROJECT;
	}
	
}