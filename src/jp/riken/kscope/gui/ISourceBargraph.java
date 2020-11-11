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
package jp.riken.kscope.gui;

import java.awt.Color;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;

/**
 * Source view bar graph interface class
 *
 * @author RIKEN
 */
public interface ISourceBargraph {

  /**
   * Get the value of the bar graph. Max = 1.0 ~ Min = 0.0
   *
   * @return Bar graph value
   */
  public float getBarValue();

  /**
   * Get the character string to be displayed next to the bar graph
   *
   * @return Display string
   */
  public String getBarText();

  /**
   * Get the target row information of the bar graph
   *
   * @return Target line information
   */
  public CodeLine getCodeLine();

  /**
   * Get the target source file for the bar graph
   *
   * @return Target source file
   */
  public SourceFile getSourceFile();

  /**
   * Get the display color of the bar graph
   *
   * @return Bar graph display color
   */
  public Color getBarColor();

  /**
   * Get the bar graph type name
   *
   * @return Bar graph type name
   */
  public String getTypeName();
}
