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

import java.util.List;

/**
 * イベントカウンタ情報
 * @author RIKEN
 */
public class EventCounterInfo {
    /** イベントカウンタ数 */
    private int eventcount;
    /** カウンタグループリスト */
    private List<EventCounterGroup> eventGroupList;

    /**
     * コンストラクタ
     */
    public EventCounterInfo() {
    }
    
    
    /**
     * イベントカウンタ数
     * @return		イベントカウンタ数
     */
	public int getEventcount() {
		return eventcount;
	}
	
	/**
	 * イベントカウンタ数
	 * @param count			イベントカウンタ数
	 */
	public void setEventcount(int count) {
		this.eventcount = count;
	}
	
	/**
	 * カウンタグループリスト
	 * @return		カウンタグループリスト
	 */
	public List<EventCounterGroup> getEventGroupList() {
		return eventGroupList;
	}
	
	/**
	 * カウンタグループリスト
	 * @param list			カウンタグループリスト
	 */
	public void setEventGroupList(List<EventCounterGroup> list) {
		this.eventGroupList = list;
	}
    
}
