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
package jp.riken.kscope.profiler.dprof;

import jp.riken.kscope.profiler.common.PaDiscrimInfo;
import jp.riken.kscope.profiler.common.ProfConstant;

/**
 * Hold common information
 *
 * @author RIKEN
 */
public class CommonInfo {

  private int processNum;
  private int measureOption;
  private short execKind;
  private short threadNum;
  private int cpuClock;
  private String measureTimeInfo;
  private int recomMemory;
  private float sampInterval;
  private int logicDimention;
  private int logicShapeX;
  private int logicShapeY;
  private int logicShapeZ;
  private int logicCordinateX;
  private int logicCordinateY;
  private int logicCordinateZ;
  private int phisShapeX;
  private int phisShapeY;
  private int phisShapeZ;
  private int phisShapeA;
  private int phisShapeB;
  private int phisShapeC;
  private int phisCordinateX;
  private int phisCordinateY;
  private int phisCordinateZ;
  private int phisCordinateA;
  private int phisCordinateB;
  private int phisCordinateC;
  private String paEventVal;
  private PaDiscrimInfo paDiscrimInfo;

  /**
   * Get the measurement option "COLL_OPT_COM"
   *
   * @return COLL_OPT_COM is set: true Not set: false
   */
  public boolean isOptCom() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_COM) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_PA"
   *
   * @return COLL_OPT_PA is set: true Not set: false
   */
  public boolean isOptPa() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_PA) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_SAMPLING"
   *
   * @return COLL_OPT_SAMPLING is set: true Not set: false
   */
  public boolean isOptSampling() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_SAMPLING) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_REALTIME"
   *
   * @return COLL_OPT_REALTIME is set: true Not set: false
   */
  public boolean isOptRealTime() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_REALTIME) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_PA_RANGE"
   *
   * @return COLL_OPT_PA_RANGE is set: true Not set: false
   */
  public boolean isOptPARange() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_PA_RANGE) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_PA_EVENT"
   *
   * @return COLL_OPT_PA_EVENT is set: true Not set: false
   */
  public boolean isOptPAEvent() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_PA_EVENT) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_CALLGRAPH"
   *
   * @return COLL_OPT_CALLGRAPH is set: true Not set: false
   */
  public boolean isOptCallGraph() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_CALLGRAPH) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_MPIELAPS"
   *
   * @return COLL_OPT_MPIELAPS is set: true Not set: false
   */
  public boolean isOptMPIElaps() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_MPIELAPS) != 0);
  }

  /**
   * Get the measurement option "COLL_SAMP_RANGE"
   *
   * @return COLL_SAMP_RANGE is set: true Not set: false
   */
  public boolean isOptSampRange() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_SAMP_RANGE) != 0);
  }

  /**
   * Get the measurement option "COLL_USERFUNC"
   *
   * @return COLL_OPT_USERFUNC is set: true Not set: false
   */
  public boolean isOptUserFunc() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_USERFUNC) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_SAMP_RANGE_COST"
   *
   * @return COLL_OPT_SAMP_RANGE_COST is set: true Not set: false
   */
  public boolean isOptSampRangeCost() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_SAMP_RANGE_COST) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_ST_COM"
   *
   * @return COLL_OPT_ST_COM is set: true Not set: false
   */
  public boolean isOptStCom() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_ST_COM) != 0);
  }

  /**
   * Get the measurement option "COLL_OPT_SLEEP"
   *
   * @return COLL_OPT_SLEEP is set: true Not set: false
   */
  public boolean isOptSleep() {
    return ((measureOption & ProfConstant.DPRF_COLL_OPT_SLEEP) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_SERIAL"
   *
   * @return EXEC_KIND_SERIAL is set: true Not set: false
   */
  public boolean isExecSerial() {
    return ((execKind & ProfConstant.EXEC_KIND_SERIAL) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_MPI"
   *
   * @return EXEC_KIND_MPI is set: true Not set: false
   */
  public boolean isExecMPI() {
    return ((execKind & ProfConstant.EXEC_KIND_MPI) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_XPF"
   *
   * @return EXEC_KIND_XPF is set: true Not set: false
   */
  public boolean isExecXPF() {
    return ((execKind & ProfConstant.EXEC_KIND_XPF) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_FULL"
   *
   * @return EXEC_KIND_FULL is set: true Not set: false
   */
  public boolean isExecFull() {
    return ((execKind & ProfConstant.EXEC_KIND_FULL) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_LIMITED"
   *
   * @return EXEC_KIND_LIMITED is set: true Not set: false
   */
  public boolean isExecLimited() {
    return ((execKind & ProfConstant.EXEC_KIND_LIMITED) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_AUTO"
   *
   * @return EXEC_KIND_AUTO is set: true Not set: false
   */
  public boolean isExecAuto() {
    return ((execKind & ProfConstant.EXEC_KIND_AUTO) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_OMP"
   *
   * @return EXEC_KIND_OMP is set: true Not set: false
   */
  public boolean isExecOMP() {
    return ((execKind & ProfConstant.EXEC_KIND_OMP) != 0);
  }

  /**
   * Get the execution form "EXEC_KIND_UNKNOWN"
   *
   * @return EXEC_KIND_UNKNOWN is set: true Not set: false
   */
  public boolean isExecUnknown() {
    return (execKind == ProfConstant.EXEC_KIND_UNKNOWN);
  }

  /**
   * Get the number of processes
   *
   * @return Number of processes
   */
  public int getProcessNum() {
    return processNum;
  }

  /**
   * Get measurement options
   *
   * @return Measurement option
   */
  public int getMeasureOption() {
    return measureOption;
  }

  /**
   * Get the execution form
   *
   * @return Execution form
   */
  public short getRunStyle() {
    return execKind;
  }

  /**
   * Get the number of threads
   *
   * @return Number of threads
   */
  public short getThreadNum() {
    return threadNum;
  }

  /**
   * Get CPU clock frequency
   *
   * @return CPU clock frequency
   */
  public int getCpuClock() {
    return cpuClock;
  }

  /**
   * Get measurement time information
   *
   * @return Measurement time information
   */
  public String getMeasureTimeInfo() {
    return measureTimeInfo;
  }

  /**
   * Get the recommended memory value
   *
   * @return Recommended memory value
   */
  public int getRecomMemory() {
    return recomMemory;
  }

  /**
   * Get the sampling interval
   *
   * @return Sampling interval
   */
  public float getSampInterval() {
    return sampInterval;
  }

  /**
   * Get the number of logical dimensions
   *
   * @return Number of logical dimensions
   */
  public int getLogicDimention() {
    return logicDimention;
  }

  /**
   * Get logical shape-X
   *
   * @return Logical shape-X
   */
  public int getLogicShapeX() {
    return logicShapeX;
  }

  /**
   * Get logical shape-Y
   *
   * @return Logical shape-Y
   */
  public int getLogicShapeY() {
    return logicShapeY;
  }

  /**
   * Get logical shape-Z
   *
   * @return Logical shape-Z
   */
  public int getLogicShapeZ() {
    return logicShapeZ;
  }

  /**
   * Get logical coordinates -X
   *
   * @return Logical coordinates -X
   */
  public int getLogicCordinateX() {
    return logicCordinateX;
  }

  /**
   * Get logical coordinates -Y
   *
   * @return Logical coordinates -Y
   */
  public int getLogicCordinateY() {
    return logicCordinateY;
  }

  /**
   * Get logical coordinates-Z
   *
   * @return Logical coordinates -Z
   */
  public int getLogicCordinateZ() {
    return logicCordinateZ;
  }

  /**
   * Get physical shape-X
   *
   * @return Physical shape-X
   */
  public int getPhisShapeX() {
    return phisShapeX;
  }

  /**
   * Get physical shape-Y
   *
   * @return Physical shape-Y
   */
  public int getPhisShapeY() {
    return phisShapeY;
  }

  /**
   * Get physical shape-Z
   *
   * @return Physical shape-Z
   */
  public int getPhisShapeZ() {
    return phisShapeZ;
  }

  /**
   * Get physical shape-A
   *
   * @return Physical shape-A
   */
  public int getPhisShapeA() {
    return phisShapeA;
  }

  /**
   * Get physical shape-B
   *
   * @return Physical shape-B
   */
  public int getPhisShapeB() {
    return phisShapeB;
  }

  /**
   * Get physical shape-C
   *
   * @return Physical shape-C
   */
  public int getPhisShapeC() {
    return phisShapeC;
  }

  /**
   * Get physical coordinates-X
   *
   * @return Physical coordinates -X
   */
  public int getPhisCordinateX() {
    return phisCordinateX;
  }

  /**
   * Get physical coordinates-Y
   *
   * @return Physical coordinates -Y
   */
  public int getPhisCordinateY() {
    return phisCordinateY;
  }

  /**
   * Get physical coordinates-Z
   *
   * @return Physical coordinates-Z
   */
  public int getPhisCordinateZ() {
    return phisCordinateZ;
  }

  /**
   * Get physical coordinates-A
   *
   * @return Physical coordinates-A
   */
  public int getPhisCordinateA() {
    return phisCordinateA;
  }

  /**
   * Get physical coordinates-B
   *
   * @return Physical coordinates-B
   */
  public int getPhisCordinateB() {
    return phisCordinateB;
  }

  /**
   * Get physical coordinates-C
   *
   * @return Physical coordinates-C
   */
  public int getPhisCordinateC() {
    return phisCordinateC;
  }

  /**
   * Get PA identification information
   *
   * @return PA identification information
   */
  public PaDiscrimInfo getPaDiscrimInfo() {
    return paDiscrimInfo;
  }

  /**
   * Get the PA event specification value
   *
   * @return PA event specification value
   */
  public String getPaEventVal() {
    return paEventVal;
  }

  /**
   * Set the number of processes
   *
   * @param processNum Number of processes to set
   */
  public void setProcessNum(int processNum) {
    this.processNum = processNum;
  }

  /**
   * Set measurement options
   *
   * @param measureOption Measurement options to set
   */
  public void setMeasureOption(int measureOption) {
    this.measureOption = measureOption;
  }

  /**
   * Set the execution mode
   *
   * @param runStyle Execution mode to be set
   */
  public void setRunStyle(short runStyle) {
    this.execKind = runStyle;
  }

  /**
   * Set the number of threads
   *
   * @param threadNum Number of threads to set
   */
  public void setThreadNum(short threadNum) {
    this.threadNum = threadNum;
  }

  /**
   * Set CPU clock frequency
   *
   * @param cpuClock CPU clock frequency to set
   */
  public void setCpuClock(int cpuClock) {
    this.cpuClock = cpuClock;
  }

  /**
   * Set measurement time information
   *
   * @param measureTimeInfo Measurement time information to be set
   */
  public void setMeasureTimeInfo(String measureTimeInfo) {
    this.measureTimeInfo = measureTimeInfo;
  }

  /**
   * Set the recommended memory value
   *
   * @param recomMemory Recommended memory value to set
   */
  public void setRecomMemory(int recomMemory) {
    this.recomMemory = recomMemory;
  }

  /**
   * Set the sampling interval
   *
   * @param sampInterval Sampling interval to set
   */
  public void setSampInterval(float sampInterval) {
    this.sampInterval = sampInterval;
  }

  /**
   * Set the number of logical dimensions
   *
   * @param logicDimention Number of logical dimensions to set
   */
  public void setLogicDimention(int logicDimention) {
    this.logicDimention = logicDimention;
  }

  /**
   * Set logical shape-X
   *
   * @param logicShapeX Logical shape to set-X
   */
  public void setLogicShapeX(int logicShapeX) {
    this.logicShapeX = logicShapeX;
  }

  /**
   * Set logical shape -Y
   *
   * @param logicShapeY Logical shape to set -Y
   */
  public void setLogicShapeY(int logicShapeY) {
    this.logicShapeY = logicShapeY;
  }

  /**
   * Set logical shape-Z
   *
   * @param logicShapeZ Logical shape to set-Z
   */
  public void setLogicShapeZ(int logicShapeZ) {
    this.logicShapeZ = logicShapeZ;
  }

  /**
   * Set logical coordinates -X
   *
   * @param logicCordinateX Logical coordinates to set -X
   */
  public void setLogicCordinateX(int logicCordinateX) {
    this.logicCordinateX = logicCordinateX;
  }

  /**
   * Set logical coordinates -Y
   *
   * @param logicCordinateY Logical coordinates to set -Y
   */
  public void setLogicCordinateY(int logicCordinateY) {
    this.logicCordinateY = logicCordinateY;
  }

  /**
   * Set logical coordinates -Z
   *
   * @param logicCordinateZ Logical coordinates to set -Z
   */
  public void setLogicCordinateZ(int logicCordinateZ) {
    this.logicCordinateZ = logicCordinateZ;
  }

  /**
   * Physical shape-Set X
   *
   * @param phisShapeX Physical shape to set-X
   */
  public void setPhisShapeX(int phisShapeX) {
    this.phisShapeX = phisShapeX;
  }

  /**
   * Physical shape-Set Y
   *
   * @param phisShapeY Physical shape to set-Y
   */
  public void setPhisShapeY(int phisShapeY) {
    this.phisShapeY = phisShapeY;
  }

  /**
   * Physical shape-Set Z
   *
   * @param phisShapeZ Physical shape to set-Z
   */
  public void setPhisShapeZ(int phisShapeZ) {
    this.phisShapeZ = phisShapeZ;
  }

  /**
   * Physical shape-Set A
   *
   * @param phisShapeA Physical shape to set-A
   */
  public void setPhisShapeA(int phisShapeA) {
    this.phisShapeA = phisShapeA;
  }

  /**
   * Physical shape-Set B
   *
   * @param phisShapeB Physical shape to set-B
   */
  public void setPhisShapeB(int phisShapeB) {
    this.phisShapeB = phisShapeB;
  }

  /**
   * Physical shape-Set C
   *
   * @param phisShapeC Physical shape to set-C
   */
  public void setPhisShapeC(int phisShapeC) {
    this.phisShapeC = phisShapeC;
  }

  /**
   * Physical coordinates-Set X
   *
   * @param phisCordinateX Physical coordinates to set-X
   */
  public void setPhisCordinateX(int phisCordinateX) {
    this.phisCordinateX = phisCordinateX;
  }

  /**
   * Physical coordinates-Set Y
   *
   * @param phisCordinateY Physical coordinates to set-Y
   */
  public void setPhisCordinateY(int phisCordinateY) {
    this.phisCordinateY = phisCordinateY;
  }

  /**
   * Physical coordinates-Set Z
   *
   * @param phisCordinateZ Physical coordinates to set-Z
   */
  public void setPhisCordinateZ(int phisCordinateZ) {
    this.phisCordinateZ = phisCordinateZ;
  }

  /**
   * Physical coordinates-Set A
   *
   * @param phisCordinateA Physical coordinates to set-A
   */
  public void setPhisCordinateA(int phisCordinateA) {
    this.phisCordinateA = phisCordinateA;
  }

  /**
   * Physical coordinates-Set B
   *
   * @param phisCordinateB Physical coordinates to set-B
   */
  public void setPhisCordinateB(int phisCordinateB) {
    this.phisCordinateB = phisCordinateB;
  }

  /**
   * Physical coordinates-Set C
   *
   * @param phisCordinateC Physical coordinates to set-C
   */
  public void setPhisCordinateC(int phisCordinateC) {
    this.phisCordinateC = phisCordinateC;
  }

  /**
   * Set PA identification information
   *
   * @param paDiscrimInfo PA identification information to be set
   */
  public void setPaDiscrimInfo(PaDiscrimInfo paDiscrimInfo) {
    this.paDiscrimInfo = paDiscrimInfo;
  }

  /**
   * Set the PA event specification value
   *
   * @param paEventVal PA event specification value to be set
   */
  public void setPaEventVal(String paEventVal) {
    this.paEventVal = paEventVal;
  }
}
