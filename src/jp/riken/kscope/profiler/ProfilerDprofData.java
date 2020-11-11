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

package jp.riken.kscope.profiler;

import java.awt.Color;
import java.math.BigDecimal;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.properties.ProfilerProperties;

/**
 * Basic Profiler (DProf) data class. Dprof profiler data
 *
 * @author RIKEN
 */
public class ProfilerDprofData extends ProfilerBaseData implements ISourceBargraph {

  /** Symbol name / counter group name: Dprof */
  private String symbol;
  // Dprof
  /** Nest level: Dprof */
  private int nestLevel;
  /** Sampling count: Dprof */
  private float sampling;
  /** Percentage to total: Dprof */
  private float ratio;
  /** Cumulative sampling count: Dprof */
  private float sumSampling;

  /** Constructor */
  public ProfilerDprofData() {
    super();
    this.symbol = null;
    this.sampling = 0.0F;
    this.ratio = 0.0F;
  }

  /**
   * Get the symbol name
   *
   * @return Symbol name
   */
  public String getSymbol() {
    return symbol;
  }

  /**
   * Set the symbol name
   *
   * @param name Symbol name
   */
  public void setSymbol(String name) {
    this.symbol = name;
  }

  /**
   * Get the number of samplings
   *
   * @return Sampling count
   */
  public float getSampling() {
    return sampling;
  }

  /**
   * Set the number of samplings
   *
   * @param count Sampling count
   */
  public void setSampling(float count) {
    this.sampling = count;
  }

  /**
   * Set the ratio to the whole
   *
   * @return Percentage of total
   */
  public float getRatio() {
    return ratio;
  }

  /**
   * Set the ratio to the whole
   *
   * @param value Percentage of the whole
   */
  public void setRatio(float value) {
    this.ratio = value;
  }

  /**
   * Get the character string to be displayed next to the bar graph
   *
   * @return Display string
   */
  @Override
  public String getBarText() {
    // Display 2 digits after the decimal point

    float value =
        new BigDecimal(String.valueOf(this.getRatio() * 100))
            .setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP)
            .floatValue();
    int scale = ProfilerProperties.COST_RATIO_SCALE;
    if (value >= 100.0) {
      scale = 1;
    }
    String format = "%.0" + scale + "f%%";
    String text = String.format(format, value);
    return text;
  }

  /**
   * Get the value of the bar graph. Max = 1.0 ~ Min = 0.0
   *
   * @return Bar graph value
   */
  @Override
  public float getBarValue() {
    return this.ratio;
  }

  /**
   * Get the target source file for the bar graph
   *
   * @return Target source file
   */
  @Override
  public SourceFile getSourceFile() {
    if (this.getCodeLine() == null) return null;
    return this.getCodeLine().getSourceFile();
  }

  /**
   * Get the display color of the bar graph
   *
   * @return Bar graph display color
   */
  @Override
  public Color getBarColor() {
    return this.getInfoType().getBarColor();
  }

  /**
   * Nest level: Dprof
   *
   * @return Nest level: Dprof
   */
  public int getNestLevel() {
    return nestLevel;
  }

  /**
   * Nest level: Dprof
   *
   * @param nest Nest level: Dprof
   */
  public void setNestLevel(int nest) {
    this.nestLevel = nest;
  }

  /**
   * Cumulative sampling count
   *
   * @return Cumulative sampling count
   */
  public float getSumSampling() {
    return sumSampling;
  }

  /**
   * Cumulative sampling count
   *
   * @param count Cumulative sampling count
   */
  public void setSumSampling(float count) {
    this.sumSampling = count;
  }

  /**
   * Get the bar graph type name
   *
   * @return Bar graph type name
   */
  @Override
  public String getTypeName() {
    return this.getInfoType().getShortName();
  }
}
