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
 * Event counter information
 *
 * @author RIKEN
 */
public class EventCounterInfo {
  /** Number of event counters */
  private int eventcount;
  /** Counter group list */
  private List<EventCounterGroup> eventGroupList;

  /** Constructor */
  public EventCounterInfo() {}

  /**
   * Number of event counters
   *
   * @return Number of event counters
   */
  public int getEventcount() {
    return eventcount;
  }

  /**
   * Number of event counters
   *
   * @param count Number of event counters
   */
  public void setEventcount(int count) {
    this.eventcount = count;
  }

  /**
   * Counter group list
   *
   * @return Counter group list
   */
  public List<EventCounterGroup> getEventGroupList() {
    return eventGroupList;
  }

  /**
   * Counter group list
   *
   * @param list Counter group list
   */
  public void setEventGroupList(List<EventCounterGroup> list) {
    this.eventGroupList = list;
  }
}
