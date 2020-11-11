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

import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.properties.RequiredBFProperties.BF_CALC_TYPE;
import jp.riken.kscope.properties.RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE;

/**
 * Request Byte / FLOP calculation result
 *
 * @author RIKEN
 */
public class RequiredBFResult {
  /** Calculation block */
  private IBlock block;
  /** Calculation of Load variable Byte */
  private int load;
  /** Calculation of Store variable Byte */
  private int store;
  /** Number of operations (FLOP) = add (F) + mul (F) + intrinsic (F) */
  private int op;
  /** Request Byte / FLOP = (Load + Store) / FLOP */
  private float requiredBF; // /<
  /** Request FLOP / Byte = FLOP / (Load + Store) */
  private float requiredFB;
  /** Throughput (GB / s) */
  private float throughput;
  /** Effective Byte / FLOP = Throughput (GB / s) / Performance (GFLOPS) */
  private float effectiveBF;
  /** Effective FLOP / Byte = Performance (GFLOPS) / Throughput (GB / s) */
  private float effectiveFB;
  /** Peak performance ratio Byte / FLOP (%) = Requested Byte / FLOP / Effective Byte / FLOP */
  private float peakBF;
  /** Peak performance ratio FLOP / Byte (%) = Required FLOP / Byte / Effective Byte / FLOP */
  private float peakFB;
  /** Total variables of access destination Memory */
  private int memoryCount;
  /** Sum of variables in access destination L1 */
  private int l1Count;
  /** Sum of variables in access destination L2 */
  private int l2Count;
  /** Sum of variables in the accessed Register */
  private int registerCount;
  /** Sum of variables of access destination Custom */
  private int customCount;
  /** Number of additions (+) to variables of floating point data type */
  private int addCount;
  /** Number of subtractions (-) for variables of floating point data type */
  private int subCount;
  /** Number of multiplications (*) on variables of floating point data type */
  private int mulCount;
  /** Number of divisions (/) on variables of floating point data type */
  private int divCount;
  /**
   * Exponentiation of variables of floating point data type, addition of built-in functions (+) +
   * multiplication (*)
   */
  private int intrinsicCount;
  /** Computational performance */
  private float performance;
  /** Calculation source Memory throughput value (GB / s) (with or without store) */
  private float memoryMBW;
  /** Calculation source L1 throughput value (GB / s) (with or without store) */
  private float l1MBW;
  /** Calculation source L2 throughput value (GB / s) (with or without store) */
  private float l2MBW;
  /** Calculated source Register throughput value (GB / s) (with or without store) */
  private float registerMBW;
  /** Calculation source Custom throughput value (GB / s) (with or without store) */
  private float customMBW;
  /** Calculation source Memory coefficient */
  private float memoryCoef;
  /** Calculation source L2 coefficient */
  private float l1Coef;
  /** Calculation source L2 coefficient */
  private float l2Coef;
  /** Register coefficient of calculation source */
  private float registerCoef;
  /** Calculation source Custom coefficient */
  private float customCoef;
  /** Throughput store settings */
  private MEM_THROUGHPUT_CALC_MODE storeMode;
  /** Calculation unit */
  private BF_CALC_TYPE unitType;

  /**
   * Get the calculation block.
   *
   * @return Calculation block
   */
  public IBlock getBlock() {
    return block;
  }
  /**
   * Set the calculation block.
   *
   * @param block Calculation block
   */
  public void setBlock(IBlock block) {
    this.block = block;
  }

  /**
   * Get the calculated Byte of the Load variable.
   *
   * @return Load Variable calculation Byte
   */
  public int getLoad() {
    return load;
  }
  /**
   * Set the calculation byte of the Load variable.
   *
   * @param load Load Variable calculation Byte
   */
  public void setLoad(int load) {
    this.load = load;
  }
  /**
   * Get the calculation byte of the Store variable.
   *
   * @return Store variable calculation Byte
   */
  public int getStore() {
    return store;
  }
  /**
   * Set the calculation byte of the Store variable.
   *
   * @param store Store Variable calculation Byte
   */
  public void setStore(int store) {
    this.store = store;
  }
  /**
   * Get the number of operations (FLOP). Number of operations (FLOP) = add (F) + mul (F) +
   * intrinsic (F)
   *
   * @return Number of operations (FLOP)
   */
  public int getOperand() {
    return op;
  }
  /**
   * Set the number of operations (FLOP). Number of operations (FLOP) = add (F) + mul (F) +
   * intrinsic (F)
   *
   * @param op Number of operations (FLOP)
   */
  public void setOperation(int op) {
    this.op = op;
  }
  /**
   * Get the request Byte / FLOP. Request Byte / FLOP = (Load + Store) / FLOP
   *
   * @return Request Byte / FLOP
   */
  public float getRequiredBF() {
    return requiredBF;
  }
  /**
   * Set the request Byte / FLOP. Request Byte / FLOP = (Load + Store) / FLOP
   *
   * @param required BF Request Byte / FLOP
   */
  public void setRequiredBF(float requiredBF) {
    this.requiredBF = requiredBF;
  }
  /**
   * Get request FLOP / Byte. Request FLOP / Byte = FLOP / (Load + Store)
   *
   * @return Request FLOP / Byte
   */
  public float getRequiredFB() {
    return requiredFB;
  }
  /**
   * Set request FLOP / Byte. Request FLOP / Byte = FLOP / (Load + Store)
   *
   * @param requiredFB Request FLOP / Byte
   */
  public void setRequiredFB(float requiredFB) {
    this.requiredFB = requiredFB;
  }
  /**
   * Get throughput (GB / s).
   *
   * @return Throughput (GB / s)
   */
  public float getThroughput() {
    return throughput;
  }
  /**
   * Set the throughput (GB / s).
   *
   * @param throughput Throughput (GB / s)
   */
  public void setThroughput(float throughput) {
    this.throughput = throughput;
  }

  /**
   * Get the effective Byte / FLOP. Effective Byte / FLOP = Throughput (GB / s) / Performance
   * (GFLOPS)
   *
   * @return Effective Byte / FLOP
   */
  public float getEffectiveBF() {
    return effectiveBF;
  }
  /**
   * Set the effective Byte / FLOP. Effective Byte / FLOP = Throughput (GB / s) / Performance
   * (GFLOPS)
   *
   * @param effectiveBF Effective Byte / FLOP
   */
  public void setEffectiveBF(float effectiveBF) {
    this.effectiveBF = effectiveBF;
  }
  /**
   * Get effective FLOP / Byte. Effective FLOP / Byte = Performance (GFLOPS) / Throughput (GB / s)
   *
   * @return effectiveFB
   */
  public float getEffectiveFB() {
    return effectiveFB;
  }
  /**
   * Set effective FLOP / Byte. Effective FLOP / Byte = Performance (GFLOPS) / Throughput (GB / s)
   *
   * @param effectiveFB Set effectiveFB
   */
  public void setEffectiveFB(float effectiveFB) {
    this.effectiveFB = effectiveFB;
  }

  /**
   * Get the peak performance ratio Byte / FLOP (%). Peak performance ratio Byte / FLOP (%) =
   * Requested Byte / FLOP / Effective Byte / FLOP
   *
   * @return Peak performance ratio Byte / FLOP (%)
   */
  public float getPeakBF() {
    return peakBF;
  }
  /**
   * Set the peak performance ratio Byte / FLOP (%). Peak performance ratio Byte / FLOP (%) =
   * Requested Byte / FLOP / Effective Byte / FLOP
   *
   * @param peakBF Peak performance ratio Byte / FLOP (%)
   */
  public void setPeakBF(float peak) {
    this.peakBF = peak;
  }
  /**
   * Get the peak performance ratio FLOP / Byte (%). Peak performance ratio FLOP / Byte (%) =
   * Required FLOP / Byte / Effective Byte / FLOP
   *
   * @return Peak performance ratio FLOP / Byte (%)
   */
  public float getPeakFB() {
    return peakFB;
  }
  /**
   * Set the peak performance ratio FLOP / Byte (%). Peak performance ratio FLOP / Byte (%) =
   * Required FLOP / Byte / Effective Byte / FLOP
   *
   * @param peakFB Peak performance ratio FLOP / Byte (%)
   */
  public void setPeakFB(float peak) {
    this.peakFB = peak;
  }
  /**
   * Get the total of the variables of the access destination Memory.
   *
   * @return Total variables of access destination Memory
   */
  public int getMemoryCount() {
    return memoryCount;
  }
  /**
   * Set the total of the variables of the access destination Memory.
   *
   * @param count Total of variables of access destination Memory
   */
  public void setMemoryCount(int count) {
    this.memoryCount = count;
  }
  /**
   * Get the total of variables of access destination L1. @ return l1Count Total variables of access
   * destination L1
   */
  public int getL1Count() {
    return l1Count;
  }
  /**
   * Set the total of the variables of the access destination L1.
   *
   * @param count Total of variables in access destination L1
   */
  public void setL1Count(int count) {
    this.l1Count = count;
  }
  /**
   * Get the total of variables of access destination L2.
   *
   * @return Total variables of access destination L2
   */
  public int getL2Count() {
    return l2Count;
  }
  /**
   * Set the total of variables of access destination L2.
   *
   * @param l2Count Total of variables in access destination L2
   */
  public void setL2Count(int count) {
    this.l2Count = count;
  }
  /**
   * Get the total of variables of the accessed Register
   *
   * @return Total variables of access destination Register
   */
  public int getRegisterCount() {
    return registerCount;
  }
  /**
   * Set the total of variables of the access destination Register.
   *
   * @param count Total of variables of access destination Register
   */
  public void setRegisterCount(int count) {
    this.registerCount = count;
  }

  /**
   * Get the total of variables of the access destination Custom
   *
   * @return Total variables of access destination Custom
   */
  public int getCustomCount() {
    return customCount;
  }
  /**
   * Set the total of the variables of the access destination Custom.
   *
   * @param count Total of variables of access destination Custom
   */
  public void setCustomCount(int count) {
    this.customCount = count;
  }

  /**
   * Get the number of additions (+) to a variable of floating point data type.
   *
   * @return Number of additions (+) to variables of floating point data type
   */
  public int getAddCount() {
    return addCount;
  }
  /**
   * Set the number of additions (+) for variables of floating point data type.
   *
   * @param count Number of additions (+) to variables of floating point data type
   */
  public void setAddCount(int count) {
    this.addCount = count;
  }

  /**
   * Get the number of subtractions (-) for variables of floating point data type.
   *
   * @return Number of subtractions (-) for variables of floating point data type
   */
  public int getSubCount() {
    return subCount;
  }
  /**
   * Set the number of subtractions (-) for variables of floating point data type.
   *
   * @param count Number of subtractions (-) for variables of floating point data type
   */
  public void setSubCount(int count) {
    this.subCount = count;
  }

  /**
   * Get the number of multiplications (*) for variables of floating point data type.
   *
   * @return Number of multiplications (*) on variables of floating point data type
   */
  public int getMulCount() {
    return mulCount;
  }

  /**
   * Set the number of multiplications (*) for variables of floating point data type.
   *
   * @param count Number of multiplications (*) on variables of floating point data type
   */
  public void setMulCount(int count) {
    this.mulCount = count;
  }

  /**
   * Get the number of divisions (/) for variables of floating point data type.
   *
   * @return Number of divisions (/) on variables of floating point data type
   */
  public int getDivCount() {
    return divCount;
  }
  /**
   * Set the number of divisions (/) for variables of floating point data type.
   *
   * @param count Number of divisions (/) on variables of floating point data type
   */
  public void setDivCount(int count) {
    this.divCount = count;
  }

  /**
   * Get powers for variables of floating point data type, addition (+) + multiplication (*) of
   * built-in functions.
   *
   * @return Exponentiation of variables of floating point data type, addition of built-in functions
   *     (+) + multiplication (*)
   */
  public int getIntrinsicCount() {
    return intrinsicCount;
  }
  /**
   * Set powers for variables of floating point data type, addition (+) + multiplication (*) of
   * built-in functions.
   *
   * @param count Exponentiation of variables of floating point data type, addition of built-in
   *     functions (+) + multiplication (*)
   */
  public void setIntrinsicCount(int count) {
    this.intrinsicCount = count;
  }
  /**
   * Get computing performance.
   *
   * @return Computation performance
   */
  public float getPerformance() {
    return performance;
  }
  /**
   * Set the calculation performance.
   *
   * @param performance Computational performance
   */
  public void setPerformance(float performance) {
    this.performance = performance;
  }
  /**
   * Get the throughput store settings.
   *
   * @return Throughput store settings
   */
  public MEM_THROUGHPUT_CALC_MODE getStoreMode() {
    return storeMode;
  }
  /**
   * Set the throughput store settings.
   *
   * @param storeMode Throughput store settings
   */
  public void setMemThroughputCalcMode(MEM_THROUGHPUT_CALC_MODE storeMode) {
    this.storeMode = storeMode;
  }
  /**
   * Get the memory throughput value (GB / s) of the calculation source.
   *
   * @return Memory throughput value (GB / s) from the calculation source
   */
  public float getMemoryMBW() {
    return memoryMBW;
  }
  /**
   * Set the memory throughput value (GB / s) of the calculation source.
   *
   * @param value Memory throughput value (GB / s) from the calculation source
   */
  public void setMemoryMBW(float value) {
    this.memoryMBW = value;
  }

  /**
   * Get the L1 throughput value (GB / s) of the calculation source.
   *
   * @return Calculated source L1 throughput value (GB / s)
   */
  public float getL1MBW() {
    return l1MBW;
  }

  /**
   * Set the L1 throughput value (GB / s) of the calculation source.
   *
   * @param value Calculated source L1 throughput value (GB / s)
   */
  public void setL1MBW(float value) {
    l1MBW = value;
  }

  /**
   * Get the L2 throughput value (GB / s) of the calculation source.
   *
   * @return Calculated source L2 throughput value (GB / s)
   */
  public float getL2MBW() {
    return l2MBW;
  }

  /**
   * Set the L2 throughput value (GB / s) of the calculation source.
   *
   * @param value Calculated source L2 throughput value (GB / s)
   */
  public void setL2MBW(float value) {
    l2MBW = value;
  }

  /**
   * Get the Register throughput value (GB / s) of the calculation source.
   *
   * @return Calculated source Register throughput value (GB / s)
   */
  public float getRegisterMBW() {
    return registerMBW;
  }

  /**
   * Set the Register throughput value (GB / s) of the calculation source.
   *
   * @param value Calculated source Register throughput value (GB / s)
   */
  public void setRegisterMBW(float value) {
    this.registerMBW = value;
  }

  /**
   * Get the custom throughput value (GB / s) of the calculation source.
   *
   * @return Custom throughput value (GB / s) from the calculation source
   */
  public float getCustomMBW() {
    return customMBW;
  }

  /**
   * Set the custom throughput value (GB / s) of the calculation source.
   *
   * @param value Custom throughput value (GB / s) from which it was calculated
   */
  public void setCustomMBW(float value) {
    this.customMBW = value;
  }
  /**
   * Get the Memory coefficient of the calculation source.
   *
   * @return Memory coefficient of calculation source
   */
  public float getMemoryCoef() {
    return memoryCoef;
  }
  /**
   * Set the Memory coefficient of the calculation source.
   *
   * @param coef Calculation source Memory coefficient
   */
  public void setMemoryCoef(float coef) {
    this.memoryCoef = coef;
  }

  /**
   * Get the L1 coefficient of the calculation source.
   *
   * @return L1 coefficient of calculation source
   */
  public float getL1Coef() {
    return l1Coef;
  }

  /**
   * Set the L1 coefficient of the calculation source.
   *
   * @param coef L1 coefficient of calculation source
   */
  public void setL1Coef(float l1Coef) {
    this.l1Coef = l1Coef;
  }

  /**
   * Get the L2 coefficient of the calculation source.
   *
   * @return L2 coefficient of calculation source
   */
  public float getL2Coef() {
    return l2Coef;
  }
  /**
   * Set the L2 coefficient of the calculation source.
   *
   * @param coef L2 coefficient of calculation source
   */
  public void setL2Coef(float coef) {
    this.l2Coef = coef;
  }

  /**
   * Get the Register coefficient of the calculation source.
   *
   * @return Register coefficient of calculation source
   */
  public float getRegisterCoef() {
    return registerCoef;
  }
  /**
   * Set the Register coefficient of the calculation source.
   *
   * @param coef Register coefficient of calculation source
   */
  public void setRegisterCoef(float coef) {
    this.registerCoef = coef;
  }

  /**
   * Get the Custom coefficient of the calculation source.
   *
   * @return Custom coefficient of calculation source
   */
  public float getCustomCoef() {
    return customCoef;
  }
  /**
   * Set the Custom coefficient of the calculation source.
   *
   * @param coef Custom coefficient of calculation source
   */
  public void setCustomCoef(float coef) {
    this.customCoef = coef;
  }
  /**
   * Get the calculation unit.
   *
   * @return Calculation unit
   */
  public BF_CALC_TYPE getUnitType() {
    return unitType;
  }
  /**
   * Set the calculation unit.
   *
   * @param unitType Calculation unit
   */
  public void setBFCalcType(BF_CALC_TYPE type) {
    this.unitType = type;
  }

  /**
   * Calculate request Byte / FLOP and request FLOP / Byte from the configured Load and Store.
   * Required = (Load + Store) / FLOP
   *
   * @return Calculation result: Request Byte / FLOP
   */
  public float calcRequiredBF() {
    float flop = 0.0F;
    if (this.op > 0) {
      flop = (float) (this.load + this.store) / (float) this.op;
    }
    this.requiredBF = flop;
    if (flop > 0.0F) {
      this.requiredFB = 1.0F / flop;
    } else {
      this.requiredFB = 0.0F;
    }

    return flop;
  }

  /**
   * Calculate effective Byte / FLOP and effective FLOP / Byte. Effective Byte / FLOP = Throughput /
   * Computational performance
   *
   * @param performance Computational performance
   * @return Effective Byte / FLOP
   */
  public float calcRequiredBF(float performance) {
    this.performance = performance;
    if (performance == 0 || this.throughput == 0) {
      this.effectiveBF = 0.0F;
      this.effectiveFB = 0.0F;
      return 0.0F;
    }
    this.effectiveBF = this.throughput / performance;
    this.effectiveFB = performance / this.throughput;
    return this.effectiveBF;
  }

  /**
   * Calculate the peak performance ratio. Peak performance ratio = Effective B / F / Required B / F
   *
   * @return Peak performance ratio
   */
  public float calcPeakPerformance() {
    if (this.requiredBF == 0.0F) {
      this.peakBF = 0.0F;
    } else {
      this.peakBF = this.effectiveBF / this.requiredBF;
    }
    if (this.requiredFB == 0.0F) {
      this.peakBF = 0.0F;
    } else {
      this.peakFB = this.requiredFB / this.effectiveFB;
    }
    return this.peakBF;
  }
}
