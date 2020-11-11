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
package jp.riken.kscope.xcodeml.xml;

/**
 * Statement interface class
 *
 * @author RIKEN
 */
public interface IDefBaseStatement {
  /**
   * Get the starting line number
   *
   * @return Start line number
   */
  public String getLineno();

  /**
   * Set the start line number
   *
   * @param value Start line number
   */
  public void setLineno(String value);

  /**
   * Get the end line number
   *
   * @return End line number
   */
  public String getEndlineno();

  /**
   * Set the end line number
   *
   * @param value End line number
   */
  public void setEndlineno(String value);

  /**
   * Get the line number
   *
   * @return line number
   */
  public String getRawlineno();

  /**
   * Set the line number
   *
   * @param value Line number
   */
  public void setRawlineno(String value);

  /**
   * Get the file name
   *
   * @return filename
   */
  public String getFile();

  /**
   * Set the file name
   *
   * @param value filename
   */
  public void setFile(String value);
}
