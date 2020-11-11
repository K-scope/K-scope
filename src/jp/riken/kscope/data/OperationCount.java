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
 * Calculation count
 *
 * @author RIKEN
 */
public class OperationCount {

  /** Operator: + Count */
  private Integer add;
  /** Operator: -Count */
  private Integer sub;
  /** Operator: * Count */
  private Integer mul;
  /** Operator: / Count */
  private Integer div;
  /** intrinsic count */
  private Integer intrinsic;
  /** Load count */
  private Integer load;
  /** Store count */
  private Integer store;
  /** F: Count */
  private Integer f;
  /** F/(L+S) or (L+S)/F */
  private Float fls;

  /**
   * Count name. <br>
   * Built-in function name or loop, block name
   */
  private String name;

  /** Constructor */
  public OperationCount() {}

  /**
   * Operator: + Get count
   *
   * @return operator: + count
   */
  public Integer getAdd() {
    return add;
  }

  /**
   * Operator: + Set count
   *
   * @param add operator: + count
   */
  public void setAdd(Integer add) {
    this.add = add;
  }

  /**
   * Operator:-Get count
   *
   * @return operator:-count
   */
  public Integer getSub() {
    return sub;
  }

  /**
   * Operator:-Set count
   *
   * @param sub operator:-count
   */
  public void setSub(Integer sub) {
    this.sub = sub;
  }

  /**
   * Operator: * Get count
   *
   * @return operator: * count
   */
  public Integer getMul() {
    return mul;
  }

  /**
   * Operator: * Set count
   *
   * @param mul operator: * Count
   */
  public void setMul(Integer mul) {
    this.mul = mul;
  }

  /**
   * Operator: / Get count
   *
   * @return operator: / count
   */
  public Integer getDiv() {
    return div;
  }

  /**
   * Operator: / Set count
   *
   * @param div operator: / count
   */
  public void setDiv(Integer div) {
    this.div = div;
  }

  /**
   * Get intrinsic count
   *
   * @return intrinsic count
   */
  public Integer getIntrinsic() {
    return intrinsic;
  }

  /**
   * Set intrinsic count
   *
   * @param intrinsic intrinsic count
   */
  public void setIntrinsic(Integer intrinsic) {
    this.intrinsic = intrinsic;
  }

  /**
   * Get the load count
   *
   * @return load count
   */
  public Integer getLoad() {
    return load;
  }

  /**
   * Set load count
   *
   * @param load load count
   */
  public void setLoad(Integer load) {
    this.load = load;
  }

  /**
   * Get store count
   *
   * @return store count
   */
  public Integer getStore() {
    return store;
  }

  /**
   * Set store count
   *
   * @param store Store count
   */
  public void setStore(Integer store) {
    this.store = store;
  }

  /**
   * Get F (L + S)
   *
   * @return F (L + S)
   */
  public Float getFls() {
    return this.fls;
  }

  /**
   * Set F (L + S)
   *
   * @param fls F (L + S)
   */
  public void setFls(Float fls) {
    this.fls = fls;
  }

  /**
   * Get the count name
   *
   * @return Count name
   */
  public String getName() {
    return name;
  }

  /**
   * Set the count name
   *
   * @param name Count name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * F: Get the count
   *
   * @return F: Count
   */
  public Integer getF() {
    return f;
  }

  /**
   * F: Set the count
   *
   * @param f F: Count
   */
  public void setF(Integer f) {
    this.f = f;
  }
}
