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

package jp.riken.kscope.model;

import java.util.Comparator;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.profiler.ProfilerDprofData;

/**
 * Sort by row of profiler cost information (ascending order) class
 *
 * @author RIKEN
 */
public class ProfilerCostTableModelComparator implements Comparator<ProfilerDprofData> {

  public enum SORT_MODE {
    BY_LINE,
    BY_COST;
  }

  private SORT_MODE mode = SORT_MODE.BY_LINE;

  public ProfilerCostTableModelComparator(SORT_MODE mode) {
    this.mode = mode;
  }

  @Override
  public int compare(ProfilerDprofData arg0, ProfilerDprofData arg1) {
    if (this.mode == SORT_MODE.BY_COST) {
      return compareByCost(arg0, arg1);
    } else {
      return compareByLine(arg0, arg1);
    }
  }

  private int compareByLine(ProfilerDprofData arg0, ProfilerDprofData arg1) {
    CodeLine code0 = arg0.getCodeLine();
    CodeLine code1 = arg1.getCodeLine();
    int ln0 = code0.getStartLine();
    int ln1 = code1.getStartLine();

    if (ln0 > ln1) {
      return 1;
    } else if (ln0 == ln1) {
      return 0;
    } else {
      return -1;
    }
  }

  private int compareByCost(ProfilerDprofData arg0, ProfilerDprofData arg1) {
    float ratio0 = arg0.getRatio();
    float ratio1 = arg1.getRatio();

    if (ratio0 > ratio1) {
      return -1;
    } else if (ratio0 == ratio1) {
      return 0;
    } else {
      return 1;
    }
  }
}
