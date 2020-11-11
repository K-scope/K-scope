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

package jp.riken.kscope.language;

import java.io.Serializable;

/**
 * A class that represents an array subscript of a variable declaration.
 *
 * @author RIKEN
 */
public class VariableDimension implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = 1309109368271186155L;

  private DimensionIndex[] indices = null;

  /**
   * Constructor.
   *
   * @param inds Subscript array
   */
  public VariableDimension(DimensionIndex[] inds) {
    this.indices = inds;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected DimensionIndex getIndex(int i) {
    return this.indices[i];
  }

  public DimensionIndex[] getIndex() {
    return this.indices;
  }

  protected void set_index_size(int size) {
    indices = new DimensionIndex[size];
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected void set_index_start(int i, Expression index_start) {
    indices[i].set_start(index_start);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected void set_index_end(int i, Expression index_end) {
    indices[i].set_end(index_end);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected int get_index_size() {
    return (indices.length);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected Expression get_index_start(int i) {
    return (indices[i].get_start());
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected Expression get_index_end(int i) {
    return (indices[i].get_end());
  }

  @Override
  public String toString() {
    StringBuilder st = new StringBuilder();
    st.append("(");
    for (int i = 0; i < indices.length; i++) {
      st.append(this.get_index_start(i).toString());
      st.append(":");
      st.append(this.get_index_end(i).toString());
      st.append(",");
    }
    st.replace(st.length() - 1, st.length(), ")");

    return st.toString();
  }
}
