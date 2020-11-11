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

package jp.riken.kscope.information;

/**
 * Additional information class.
 *
 * @author RIKEN
 */
public class TextInfo extends InformationBase {

  /** Serial number */
  private static final long serialVersionUID = -4345153798646974378L;
  /** Additional information */
  private String content;

  /** Default constructor. */
  public TextInfo() {}

  /**
   * Constructor.
   *
   * @param text Additional information text
   */
  public TextInfo(String text) {
    this.content = text;
  }

  /**
   * Additional information text settings.
   *
   * @param text Additional information text
   */
  @Override
  public void setContent(String text) {
    content = text;
  }

  /**
   * Get additional information text.
   *
   * @return Additional information text
   */
  @Override
  public String getContent() {
    return content;
  }

  @Override
  public String toString() {
    return content;
  }

  @Override
  public String getAbstract() {
    String br = System.getProperty("line.separator");
    String[] lines = this.content.split(br);
    return lines[0];
  }
}
