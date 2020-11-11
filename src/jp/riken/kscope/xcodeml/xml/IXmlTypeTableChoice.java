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
 * Data type interface class
 *
 * @author RIKEN
 */
public interface IXmlTypeTableChoice {

  /**
   * Get the data type name
   *
   * @return data type name
   */
  String getType();

  /**
   * Set the data type name
   *
   * @param type data type name
   */
  void setType(String type);

  /**
   * Check public attribute
   *
   * @return public attribute
   */
  Boolean isIsPublic();

  /**
   * Set public attribute
   *
   * @param isPublic public attribute
   */
  void setIsPublic(Boolean isPublic);

  /**
   * Check private attribute
   *
   * @return private attribute
   */
  Boolean isIsPrivate();

  /**
   * Set private attribute
   *
   * @param isPrivate private attribute
   */
  void setIsPrivate(Boolean isPrivate);

  /**
   * Set the data type to a string
   *
   * @return Data type string
   */
  @Override
  String toString();
}
