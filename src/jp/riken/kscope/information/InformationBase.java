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

import java.io.Serializable;

/**
 * An abstract class for expressing additional information.
 *
 * @author RIKEN
 */
public abstract class InformationBase implements Serializable {

  /** Serial number */
  private static final long serialVersionUID = -1857663347004192172L;

  /**
   * Additional information text settings.
   *
   * @param cn Additional information text
   */
  public abstract void setContent(String cn);

  /**
   * Get additional information text.
   *
   * @return Additional information text
   */
  public abstract String getContent();

  /**
   * Returns the first sentence of the additional information text.
   *
   * @return First sentence of additional information
   */
  public abstract String getAbstract();
}
