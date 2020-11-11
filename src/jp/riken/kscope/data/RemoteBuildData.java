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
 * Remote Build property data
 *
 * @author Peter Bryzgalov
 */
public class RemoteBuildData {

  private String key;
  private String value;
  private String commandline_option;
  private int order;
  private String description;

  public void setProperty(String k, String v, String co, int order, String description) {
    this.key = k;
    this.value = v;
    this.commandline_option = co;
    this.order = order;
    this.description = description;
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

  public int getOrder() {
    return this.order;
  }

  public String getDescription() {
    return this.description;
  }
}
