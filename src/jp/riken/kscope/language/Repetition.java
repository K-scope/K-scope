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

/**
 * A class that expresses iterative processing. <br>
 * Corresponds to the DO loop in Fortran.
 *
 * @author RIKEN
 */
public class Repetition extends Block {
  /** Serial number */
  private static final long serialVersionUID = -2221953302518033528L;
  /** DO statement: index variable */
  private Variable iterator;
  /** DO statement: initial value */
  private Expression initIterator;
  /** DO statement: maximum value */
  private Expression endCondition;
  /** DO statement: Step interval */
  private Expression step;

  /** Constructor. */
  Repetition() {
    super();
  }

  /**
   * Constructor.
   *
   * @param mama Parent block
   */
  Repetition(Block mama) {
    super(mama);
  }

  /**
   * Constructor.
   *
   * @param itrtr Loop control variable
   * @param initItrtr Open price
   * @param endCndtn closing price
   * @param stp step size
   */
  public Repetition(Variable itrtr, Expression initItrtr, Expression endCndtn, Expression stp) {
    this();
    this.iterator = itrtr;
    this.initIterator = initItrtr;
    this.endCondition = endCndtn;
    this.step = stp;
  }
  /**
   * Get block type.
   *
   * @return BlockType.REPETITION
   */
  public BlockType getBlockType() {
    return BlockType.REPETITION;
  }

  /**
   * Loop control variable settings.
   *
   * @param itrtr Loop control variable
   */
  protected void setIterator(Variable itrtr) {
    iterator = itrtr;
  }
  /**
   * Get loop control variables.
   *
   * @return Loop control variable
   */
  public Variable getIterator() {
    return iterator;
  }

  /**
   * Open price setting.
   *
   * @param initItrtr Open price
   */
  protected void setInitIterator(Expression initItrtr) {
    initIterator = initItrtr;
  }
  /**
   * Get the opening price.
   *
   * @return Open price
   */
  public Expression getInitIterator() {
    return initIterator;
  }

  /**
   * Setting the closing price.
   *
   * @param endCndtn closing price
   */
  protected void setEndCondition(Expression endCndtn) {
    endCondition = endCndtn;
  }
  /**
   * Get closing price.
   *
   * @return closing price
   */
  public Expression getEndCondition() {
    return endCondition;
  }

  /**
   * Setting the step size.
   *
   * @param stp step size
   */
  protected void setStep(Expression stp) {
    step = stp;
  }
  /**
   * Get step size.
   *
   * @return step size
   */
  public Expression getStep() {
    return step;
  }

  /**
   * Member variable settings.
   *
   * @param itrtr Loop control variable
   * @param initItrtr Open price
   * @param endCndtn Closing price
   * @param stp Step width
   */
  protected void setProperty(
      Variable itrtr, Expression initItrtr, Expression endCndtn, Expression stp) {
    iterator = itrtr;
    initIterator = initItrtr;
    endCondition = endCndtn;
    step = stp;
    // Set the parent DO statement
    if (iterator != null) {
      iterator.setParentStatement(this);
    }
    if (initIterator != null) {
      initIterator.setParentStatement(this);
    }
    if (endCondition != null) {
      endCondition.setParentStatement(this);
    }
    if (step != null) {
      step.setParentStatement(this);
    }
  }
}
