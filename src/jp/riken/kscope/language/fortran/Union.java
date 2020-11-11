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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * union type class.
 *
 * @author RIKEN
 */
public class Union implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = -3660335230300380344L;

  private Set<jp.riken.kscope.language.fortran.Map> maps =
      new HashSet<jp.riken.kscope.language.fortran.Map>();

  /**
   * Added map statement.
   *
   * @param map map statement
   */
  public void add(jp.riken.kscope.language.fortran.Map map) {
    if (map != null) {
      maps.add(map);
    }
  }

  /**
   * Acquisition of map set in the common body.
   *
   * @return map set
   */
  public Set<jp.riken.kscope.language.fortran.Map> getMaps() {
    return maps;
  }
}
