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

package jp.riken.kscope.profiler.dprof;

import java.util.ArrayList;
import java.util.List;

/**
 * Symbol information list class
 *
 * @author RIKEN
 */
public class SymbolList {
  /** Symbol information list */
  private List<SymbolInfo> list;

  /** Constructor */
  public SymbolList() {
    list = new ArrayList<SymbolInfo>();
  }

  /**
   * Add symbol information
   *
   * @param symbol Symbol information
   * @return true = success
   */
  public boolean addSymbolInfo(SymbolInfo symbol) {
    return this.list.add(symbol);
  }

  /**
   * Get the symbol information list
   *
   * @return Symbol information list
   */
  public List<SymbolInfo> getSymbolList() {
    return this.list;
  }

  /**
   * Get symbol information
   *
   * @param symbolid Symbol index
   * @return Symbol information
   */
  public SymbolInfo getSymbolInfo(int symbolid) {
    if (list == null) return null;
    if (list.size() <= symbolid) return null;
    return list.get(symbolid);
  }

  /**
   * Get the number of symbols
   *
   * @return Number of symbols
   */
  public int getSymbolInfoCount() {
    if (list == null) return 0;
    return list.size();
  }
}
