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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.profiler.IProfilerReader;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.profiler.ProfilerEprofData;
import jp.riken.kscope.profiler.common.BaseReader;
import jp.riken.kscope.profiler.common.MagicKey;
import jp.riken.kscope.profiler.common.PaDiscrimInfo;
import jp.riken.kscope.profiler.utils.ProfilerReaderUtil;

/**
 * Read the DProfile file and retain the information
 *
 * @author RIKEN
 */
public class DProfReader extends BaseReader implements IProfilerReader {

  private final String FILE_ID_DPROF = "DPRF"; // File identifier representing the DProf file
  private final int FILE_ID_LENGTH = 4; // File identification character length
  private final int MEASURE_TIME_LENGTH = 32; // Length of measurement time information string
  private final short PROFILER_VERSION = 0x412;

  /* Size of PA information table for each PA event specification value */
  private final Map<String, Integer> MAP_PA_INFO_LENGTH =
      new HashMap<String, Integer>() {
        private static final long serialVersionUID = 1L;

        {
          put("Cache", 10);
          put("Instructions", 9);
          put("MEM_access", 10);
          put("Performance", 10);
          put("Statistics", 10);
        }
      };

  private MagicKey magicKey = null;
  private CommonInfo commonInfo = null;
  private ArrayList<ThreadInfo> threadInfoList = null;
  private OffSetInfo offSetInfo = null;
  private FileRecord fileInfoList = null;
  private SymbolRecord symbolInfoList = null;
  private ArrayList<ArrayList<LineInfo>> lineInfoList = null;
  private ArrayList<ArrayList<LoopInfo>> loopInfoList = null;
  private ArrayList<CallGraphInfo> callGraphInfoList = null;

  /** Endian settings when loading */
  private int endian;
  /** Read profiler file */
  private File profFile;

  /**
   * Read the information of the specified profiler file
   *
   * @param fDProf Profiler file to read
   * @param endian Endian setting LITTLE_ENDIAN: 0x00 BIG_ENDIAN: 0x01;
   * @throws Exception Read exception
   */
  @Override
  public void readFile(File fDProf, int endian) throws Exception {
    // set endian
    this.endian = endian;
    this.profFile = fDProf;

    long fileSize = fDProf.length();
    ByteBuffer byteBuf = ByteBuffer.allocate((int) fileSize);
    FileInputStream fis = new FileInputStream(fDProf);

    while (fis.available() > 0) {
      byteBuf.put((byte) fis.read());
    }
    byteBuf.flip();

    magicKey = readMagicKey(byteBuf);
    commonInfo = readCommonInfo(byteBuf);
    threadInfoList = readThreadInfo(byteBuf);
    offSetInfo = readOffSetInfo(byteBuf);
    fileInfoList = readFileInfo(byteBuf);
    symbolInfoList = readSymbolInfo(byteBuf);
    lineInfoList = readLineInfo(byteBuf);
    loopInfoList = readLoopInfo(byteBuf);
    callGraphInfoList = readCallGraphInfo(byteBuf);

    fis.close();
  }

  /**
   * Returns an instance of magic key information read from the profiler file. Returns null if
   * readProfile (File) is not executed
   *
   * @return An instance of the MagicKey class that stores magic key information. However, it is
   *     null if readProfile (File) is not executed.
   */
  public MagicKey getMagicKey() {
    return magicKey;
  }

  /**
   * Returns an instance of common information read from the profiler file. Returns null if
   * readProfile (File) is not executed
   *
   * @return An instance of the CommonInfo class that stores common information. However, it is null
   *     if readProfile (File) is not executed.
   */
  public CommonInfo getCommonInfo() {
    return commonInfo;
  }

  /**
   * Returns a list of instances of thread information read from the profiler file. If readProfile
   * (File) is not executed Returns null
   *
   * @return A list of instances of the ThreadInfo class that contain thread information. However,
   *     readProfile (File) Null if * is not running
   */
  public List<ThreadInfo> getThreadInfoList() {
    return threadInfoList;
  }

  /**
   * Returns a list of instances of offset information read from the profiler file. If readProfile
   * (File) is not executed Returns null
   *
   * @return An instance of the OffSetInfo class that stores offset information. However, it is null
   *     if readProfile (File) is not executed.
   */
  public OffSetInfo getOffSetInfo() {
    return offSetInfo;
  }

  /**
   * Returns a list of instances of symbol information read from the profiler file. If readProfile
   * (File) is not executed Returns null
   *
   * @return A list of instances of the SymbolInfo class that contain symbol information. However,
   *     readProfile (File) Null if * is not running
   */
  public SymbolRecord getSymbolInfoList() {
    return symbolInfoList;
  }

  /**
   * Returns a list of instances of line information read from the profiler file. If readProfile
   * (File) is not executed Returns null
   *
   * @return A list of instances of the LineInfo class that contain symbol information. However,
   *     readProfile (File) Null if * is not running
   */
  public ArrayList<ArrayList<LineInfo>> getLineInfoList() {
    return lineInfoList;
  }

  /**
   * Returns a list of instances of loop information read from the profiler file. If readProfile
   * (File) is not executed Returns null
   *
   * @return A list of instances of the LoopInfo class that contain symbol information. However,
   *     readProfile (File) Null if * is not running
   */
  public ArrayList<ArrayList<LoopInfo>> getLoopInfoList() {
    return loopInfoList;
  }

  /**
   * Returns a list of instances of file information read from the profiler file. If readProfile
   * (File) is not executed Returns null
   *
   * @return A list of instances of the FilelInfo class that contain file information. However, it
   *     is null if readProfile (File) is not executed.
   */
  public FileRecord getFileInfoList() {
    return fileInfoList;
  }

  /**
   * Returns a list of instances of call graph information read from the profiler file. If
   * readProfile (File) is not executed Returns null
   *
   * @return A list of instances of the CallGraphlInfo class that contains call graph information.
   *     However, readProfile (File) Null if * is not running
   */
  public List<CallGraphInfo> getCallGraphInfo() {
    return callGraphInfoList;
  }

  /* Reading magic key information */
  private MagicKey readMagicKey(ByteBuffer byteBuf) throws Exception {
    MagicKey newMagicKey = new MagicKey();
    String fileID = getString(byteBuf, FILE_ID_LENGTH);

    if (!FILE_ID_DPROF.equals(fileID)) {
      throw new Exception(
          Message.getString("dialog.common.error")
              + // error
              ": "
              + Message.getString("dprofreader.exception.notvalid")); // Not a valid DProf file.
    }
    newMagicKey.setId(fileID);
    newMagicKey.setAdd_mode(getShort(byteBuf));
    short version = getShort(byteBuf);
    if (version != PROFILER_VERSION) {
      throw new Exception(
          Message.getString("dialog.common.error")
              + // error
              ": "
              + Message.getString(
                  "dprofreader.exception.outside",
                  version,
                  PROFILER_VERSION)); // This is an unsupported DProf version. Read =% # 04X Support
      // =% # 04X
    }
    newMagicKey.setVer(version);
    return newMagicKey;
  }

  /* Reading common information */
  private CommonInfo readCommonInfo(ByteBuffer byteBuf) {
    CommonInfo newCommonInfo = new CommonInfo();
    newCommonInfo.setProcessNum(getInt(byteBuf));
    newCommonInfo.setMeasureOption(getInt(byteBuf));
    newCommonInfo.setRunStyle(getShort(byteBuf));
    newCommonInfo.setThreadNum(getShort(byteBuf));
    newCommonInfo.setCpuClock(getInt(byteBuf));
    newCommonInfo.setMeasureTimeInfo(getString(byteBuf, MEASURE_TIME_LENGTH));
    newCommonInfo.setRecomMemory(getInt(byteBuf));
    newCommonInfo.setSampInterval(getFloat(byteBuf));
    newCommonInfo.setLogicDimention(getInt(byteBuf));
    newCommonInfo.setLogicShapeX(getInt(byteBuf));
    newCommonInfo.setLogicShapeY(getInt(byteBuf));
    newCommonInfo.setLogicShapeZ(getInt(byteBuf));
    newCommonInfo.setLogicCordinateX(getInt(byteBuf));
    newCommonInfo.setLogicCordinateY(getInt(byteBuf));
    newCommonInfo.setLogicCordinateZ(getInt(byteBuf));
    newCommonInfo.setPhisShapeX(getInt(byteBuf));
    newCommonInfo.setPhisShapeY(getInt(byteBuf));
    newCommonInfo.setPhisShapeZ(getInt(byteBuf));
    newCommonInfo.setPhisShapeA(getInt(byteBuf));
    newCommonInfo.setPhisShapeB(getInt(byteBuf));
    newCommonInfo.setPhisShapeC(getInt(byteBuf));
    newCommonInfo.setPhisCordinateX(getInt(byteBuf));
    newCommonInfo.setPhisCordinateY(getInt(byteBuf));
    newCommonInfo.setPhisCordinateZ(getInt(byteBuf));
    newCommonInfo.setPhisCordinateA(getInt(byteBuf));
    newCommonInfo.setPhisCordinateB(getInt(byteBuf));
    newCommonInfo.setPhisCordinateC(getInt(byteBuf));

    if (newCommonInfo.isOptPa()) {
      PaDiscrimInfo paInfo = new PaDiscrimInfo();
      paInfo.setCpu(getShort(byteBuf));
      paInfo.setEvent_nbr(getShort(byteBuf));
      paInfo.setPa_ver(getShort(byteBuf));
      paInfo.setReserve(getShort(byteBuf));
      newCommonInfo.setPaDiscrimInfo(paInfo);

      int paEventLength = getInt(byteBuf);
      newCommonInfo.setPaEventVal(getString(byteBuf, paEventLength));
    }
    return newCommonInfo;
  }

  /* Read thread information */
  private ArrayList<ThreadInfo> readThreadInfo(ByteBuffer byteBuf) {
    ArrayList<ThreadInfo> newThreadInfoList = new ArrayList<ThreadInfo>();
    int threadNum = this.commonInfo.getThreadNum();

    for (int i = 0; i < threadNum; i++) {
      ThreadInfo newThrInfo = new ThreadInfo();
      newThrInfo.setThreadNo(getInt(byteBuf));
      newThrInfo.setElapsTime(getFloat(byteBuf));
      newThrInfo.setUserTime(getFloat(byteBuf));
      newThrInfo.setSystemTime(getFloat(byteBuf));
      newThrInfo.setTotalSampNum(getFloat(byteBuf));
      newThrInfo.setBarrierWaitSyncNum(getFloat(byteBuf));
      newThrInfo.setMpiLibCostNum(getFloat(byteBuf));
      newThrInfo.setMpiFuncElapsTime(getFloat(byteBuf));

      if (this.commonInfo.isOptPa()) {
        int paEventLength = MAP_PA_INFO_LENGTH.get(this.commonInfo.getPaEventVal());
        double[] paInfo = new double[paEventLength];

        for (int j = 0; j < paEventLength; j++) {
          paInfo[j] = getDouble(byteBuf);
        }
        newThrInfo.setPaInfo(paInfo);
      }
      newThreadInfoList.add(newThrInfo);
    }

    return newThreadInfoList;
  }

  /* Reading offset information */
  private OffSetInfo readOffSetInfo(ByteBuffer byteBuf) {
    OffSetInfo newOffsetInfo = new OffSetInfo();
    newOffsetInfo.setLineInfo(getInt(byteBuf));
    newOffsetInfo.setLoopInfo(getInt(byteBuf));
    newOffsetInfo.setCallGraphInfo(getInt(byteBuf));
    newOffsetInfo.setMpiFuncElapsTimeInfo(getInt(byteBuf));
    newOffsetInfo.setComInfo(getInt(byteBuf));
    newOffsetInfo.setSymbolInfo(getInt(byteBuf));
    return newOffsetInfo;
  }

  /* Read file information */
  private FileRecord readFileInfo(ByteBuffer byteBuf) {
    FileRecord newFileInfoList = new FileRecord();
    int fileNameNum = getInt(byteBuf);

    for (int i = 0; i < fileNameNum; i++) {
      FileInfo fileInfo = new FileInfo();
      int fileNameLength = getInt(byteBuf);
      fileInfo.setFileName(getString(byteBuf, fileNameLength));

      newFileInfoList.addFileInfo(fileInfo);
    }
    return newFileInfoList;
  }

  /* Reading symbol information */
  private SymbolRecord readSymbolInfo(ByteBuffer byteBuf) {
    SymbolRecord newSymbolInfoList = new SymbolRecord();
    int threadNum = this.commonInfo.getThreadNum();

    for (int i = 0; i < threadNum; i++) {

      if (byteBuf.position() >= this.offSetInfo.getLineInfo()) {
        break;
      }
      SymbolList threadSymbList = new SymbolList();
      int symbolNum = getInt(byteBuf);

      for (int j = 0; j < symbolNum; j++) {
        SymbolInfo newSymbInfo = new SymbolInfo();
        newSymbInfo.setSampNum(getFloat(byteBuf));
        newSymbInfo.setBarrierSyncWaitNum(getFloat(byteBuf));
        newSymbInfo.setMpiLibCostNum(getFloat(byteBuf));
        newSymbInfo.setLineSymbolStart(getInt(byteBuf));
        newSymbInfo.setLineSymbolEnd(getInt(byteBuf));
        newSymbInfo.setFileIndex(getInt(byteBuf));
        int symbNameLength = getInt(byteBuf);
        newSymbInfo.setSymbolName(getString(byteBuf, symbNameLength));

        threadSymbList.addSymbolInfo(newSymbInfo);
      }
      newSymbolInfoList.addSymbolList(threadSymbList);
    }
    return newSymbolInfoList;
  }

  /* Reading line information */
  private ArrayList<ArrayList<LineInfo>> readLineInfo(ByteBuffer byteBuf) {
    ArrayList<ArrayList<LineInfo>> newLineInfoList = new ArrayList<ArrayList<LineInfo>>();

    if (this.offSetInfo.getLineInfo() > 0) {
      int threadNum = this.commonInfo.getThreadNum();
      int offset = this.offSetInfo.getLineInfo();
      byteBuf.position(offset);

      for (int i = 0; i < threadNum; i++) {

        if (byteBuf.position() >= this.offSetInfo.getLoopInfo()) {
          break;
        }
        ArrayList<LineInfo> threadLineList = new ArrayList<LineInfo>();
        int symbolNum = getInt(byteBuf);

        for (int j = 0; j < symbolNum; j++) {
          LineInfo newLineInfo = new LineInfo();
          newLineInfo.setSampNum(getFloat(byteBuf));
          newLineInfo.setLineNo(getInt(byteBuf));
          newLineInfo.setSymbolIndex(getInt(byteBuf));
          newLineInfo.setFileIndex(getInt(byteBuf));

          threadLineList.add(newLineInfo);
        }
        newLineInfoList.add(threadLineList);
      }
    }
    return newLineInfoList;
  }

  /* Reading loop information */
  private ArrayList<ArrayList<LoopInfo>> readLoopInfo(ByteBuffer byteBuf) {
    ArrayList<ArrayList<LoopInfo>> newLoopInfoList = new ArrayList<ArrayList<LoopInfo>>();

    if (this.offSetInfo.getLoopInfo() > 0) {
      int threadNum = this.commonInfo.getThreadNum();
      int offset = this.offSetInfo.getLoopInfo();
      byteBuf.position(offset);

      for (int i = 0; i < threadNum; i++) {

        if (byteBuf.position() >= this.offSetInfo.getCallGraphInfo()) {
          break;
        }
        ArrayList<LoopInfo> threadLoopList = new ArrayList<LoopInfo>();
        int symbolNum = getInt(byteBuf);

        for (int j = 0; j < symbolNum; j++) {
          LoopInfo newLoopInfo = new LoopInfo();

          newLoopInfo.setSampNum(getFloat(byteBuf));
          newLoopInfo.setBarrierSyncWaitNum(getFloat(byteBuf));
          newLoopInfo.setMpiLibCostNum(getFloat(byteBuf));
          newLoopInfo.setLineLoopStart(getInt(byteBuf));
          newLoopInfo.setLineLoopEnd(getInt(byteBuf));
          newLoopInfo.setNestLevel(getInt(byteBuf));
          newLoopInfo.setLoopType(getShort(byteBuf));
          newLoopInfo.setParallelInfo(getShort(byteBuf));
          newLoopInfo.setSymbolIndex(getInt(byteBuf));
          newLoopInfo.setFileIndex(getInt(byteBuf));

          threadLoopList.add(newLoopInfo);
        }
        newLoopInfoList.add(threadLoopList);
      }
    }
    return newLoopInfoList;
  }

  /* Reading call graph information */
  private ArrayList<CallGraphInfo> readCallGraphInfo(ByteBuffer byteBuf) {
    ArrayList<CallGraphInfo> newCallGraphInfoList = new ArrayList<CallGraphInfo>();

    if (this.commonInfo.isOptCallGraph() && this.offSetInfo.getCallGraphInfo() > 0) {
      int threadNum = this.commonInfo.getThreadNum();
      int offset = this.offSetInfo.getCallGraphInfo();
      byteBuf.position(offset);

      for (int i = 0; i < threadNum; i++) {
        CallGraphInfo newCallGraphInfo = new CallGraphInfo();

        if (byteBuf.remaining() < ProfilerReaderUtil.SIZEOF_FLOAT) {
          break;
        }
        newCallGraphInfo.setTotalSumSampNum(getFloat(byteBuf));
        int stackNum = getInt(byteBuf);
        ArrayList<StackInfo> newStackInfoList = new ArrayList<StackInfo>();

        for (int j = 0; j < stackNum; j++) {
          StackInfo newStackInfo = new StackInfo();
          newStackInfo.setNestLevel(getInt(byteBuf));
          newStackInfo.setSampNum(getFloat(byteBuf));
          newStackInfo.setSumSampNum(getFloat(byteBuf));
          int symbNameLength = getInt(byteBuf);
          newStackInfo.setSymbolName(getString(byteBuf, symbNameLength));

          newStackInfoList.add(newStackInfo);
        }
        newCallGraphInfo.setStackInfo(newStackInfoList);
        newCallGraphInfoList.add(newCallGraphInfo);
      }
    }
    return newCallGraphInfoList;
  }

  /**
   * Read from profiler file
   *
   * @param profilerfile Profiler file
   * @throws IOException Read error
   */
  @Override
  public void readFile(File profilerfile) throws Exception {
    readFile(profilerfile, this.endian);
  }

  /**
   * Set endian
   *
   * @param endian Endian settings
   */
  @Override
  public void setEndian(int endian) {
    this.endian = endian;
  }

  /** Get endian */
  @Override
  public int getEndian() {
    return this.endian;
  }

  /**
   * Cost information list: Get the line
   *
   * @return Cost information list: Line
   */
  @Override
  public ProfilerDprofData[] getCostInfoLine() {
    if (this.lineInfoList == null) return null;

    // Perform thread integration
    Map<CodeLine, ProfilerDprofData> listCost = new LinkedHashMap<CodeLine, ProfilerDprofData>();
    int threadid = 0;
    for (List<LineInfo> list : lineInfoList) {
      for (LineInfo info : list) {
        float sampNum = info.getSampNum();
        int lineNo = info.getLineNo();
        int symbolIndex = info.getSymbolIndex();
        int fileIndex = info.getFileIndex();

        // Get the symbol name
        SymbolInfo symbol = this.symbolInfoList.getSymbolInfo(threadid, symbolIndex);
        String symbolname = symbol.getSymbolName();

        // get the file name
        String filename = null;
        if (fileIndex >= 0) {
          FileInfo file = this.fileInfoList.getFileInfo(fileIndex);
          filename = file.getFileName();
        }

        // Cost information
        ProfilerDprofData cost =
            createProfilerCostInfo(sampNum, symbolname, lineNo, lineNo, filename);
        // Add cost information
        addProfilerCostInfo(listCost, cost);
      }
      threadid++;
    }

    if (listCost.size() <= 0) return null;
    return listCost.values().toArray(new ProfilerDprofData[0]);
  }

  /**
   * Cost information list: Get a loop
   *
   * @return Cost information list: Loop
   */
  @Override
  public ProfilerDprofData[] getCostInfoLoop() {

    if (this.loopInfoList == null) return null;

    // Perform thread integration
    Map<CodeLine, ProfilerDprofData> listCost = new LinkedHashMap<CodeLine, ProfilerDprofData>();
    int threadid = 0;
    for (List<LoopInfo> list : loopInfoList) {
      for (LoopInfo info : list) {
        float sampNum = info.getSampNum();
        int linenoStart = info.getLineLoopStart();
        int linenoEnd = info.getLineLoopEnd();
        int symbolIndex = info.getSymbolIndex();
        int fileIndex = info.getFileIndex();
        int nest = info.getNestLevel();

        // Get the symbol name
        SymbolInfo symbol = this.symbolInfoList.getSymbolInfo(threadid, symbolIndex);
        String symbolname = symbol.getSymbolName();

        // get the file name
        String filename = null;
        if (fileIndex >= 0) {
          FileInfo file = this.fileInfoList.getFileInfo(fileIndex);
          filename = file.getFileName();
        }

        // Cost information
        ProfilerDprofData cost =
            createProfilerCostInfo(sampNum, symbolname, linenoStart, linenoEnd, filename);
        // Add cost information
        addProfilerCostInfo(listCost, cost);
      }
      threadid++;
    }

    if (listCost.size() <= 0) return null;
    return listCost.values().toArray(new ProfilerDprofData[0]);
  }

  /**
   * Generate cost information
   *
   * @param sampling Number of samplings
   * @param symbolname Symbol name
   * @param linenostart Start line number
   * @param linenoend End line number
   * @param filename File name
   * @return Generation cost information
   */
  private ProfilerDprofData createProfilerCostInfo(
      float sampling, String symbolname, int linenostart, int linenoend, String filename) {
    // Cost information
    ProfilerDprofData cost = new ProfilerDprofData();
    cost.setSampling(sampling);
    cost.setSymbol(symbolname);
    // Line of code
    CodeLine line = new CodeLine(null, linenostart, linenoend, filename);
    if (filename != null) {
      line.setSourceFile(new SourceFile(filename));
    }
    cost.setCodeLine(line);

    return cost;
  }

  /**
   * Add cost information. If added, add up the number of samplings
   *
   * @param listCost Additional cost information map
   * @param costinfo Additional cost information
   */
  private void addProfilerCostInfo(
      Map<CodeLine, ProfilerDprofData> listCost, ProfilerDprofData costinfo) {
    if (listCost == null) return;
    if (costinfo == null) return;

    // Cost information
    CodeLine line = costinfo.getCodeLine();
    String symbolname = costinfo.getSymbol();
    float sampling = costinfo.getSampling();
    if (line == null) return;

    boolean find = false;
    if (listCost.containsKey(line)) {
      ProfilerDprofData srccost = listCost.get(line);
      if (symbolname != null && symbolname.equals(srccost.getSymbol())) {
        // Match
        find = true;
        // Accumulate the number of samplings
        float srcsampling = srccost.getSampling();
        srcsampling += sampling;
        srccost.setSampling(srcsampling);
      }
    }
    if (!find) {
      // Add new
      listCost.put(line, costinfo);
    }

    return;
  }

  /**
   * Cost information list: Get the procedure
   *
   * @return Cost information list: Procedure
   */
  @Override
  public ProfilerDprofData[] getCostInfoProcedure() {
    if (this.symbolInfoList == null) return null;

    // Perform thread integration
    Map<CodeLine, ProfilerDprofData> listCost = new LinkedHashMap<CodeLine, ProfilerDprofData>();
    int threadid = 0;
    for (SymbolList list : this.symbolInfoList.getSymbolRecord()) {
      for (SymbolInfo info : list.getSymbolList()) {
        float sampNum = info.getSampNum();
        int linenoStart = info.getLineSymbolStart();
        int linenoEnd = info.getLineSymbolEnd();
        int fileIndex = info.getFileIndex();
        String symbolname = info.getSymbolName();

        // get the file name
        String filename = null;
        if (fileIndex >= 0) {
          FileInfo file = this.fileInfoList.getFileInfo(fileIndex);
          filename = file.getFileName();
        }

        // Cost information
        ProfilerDprofData cost =
            createProfilerCostInfo(sampNum, symbolname, linenoStart, linenoEnd, filename);
        // Add cost information
        addProfilerCostInfo(listCost, cost);
      }
      threadid++;
    }

    if (listCost.size() <= 0) return null;
    return listCost.values().toArray(new ProfilerDprofData[0]);
  }

  /**
   * Get call graph information
   *
   * @return Call graph information
   */
  @Override
  public ProfilerDprofData[] getDprofCallGraphInfo() {
    if (this.callGraphInfoList == null) return null;

    // Bring all call graphs of a thread together
    List<ProfilerDprofData> listCall = new ArrayList<ProfilerDprofData>();
    for (CallGraphInfo list : this.callGraphInfoList) {
      List<ProfilerDprofData> listThread = new ArrayList<ProfilerDprofData>();
      // Accumulate for each thread
      float sum = 0;
      for (StackInfo info : list.getStackInfo()) {
        float sampling = info.getSampNum();
        int nestLevel = info.getNestLevel();
        String symbolname = info.getSymbolName();
        sum += sampling;
        // Call graph information
        ProfilerDprofData callinfo = new ProfilerDprofData();
        callinfo.setSymbol(symbolname);
        callinfo.setSampling(sampling);
        callinfo.setNestLevel(nestLevel);

        // Add cost information
        listThread.add(callinfo);
      }
      // Set the integrated value
      for (ProfilerDprofData data : listThread) {
        data.setSumSampling(sum);
        data.setRatio(data.getSampling() / sum);
      }
      listCall.addAll(listThread);
    }

    if (listCall.size() <= 0) return null;
    return listCall.toArray(new ProfilerDprofData[0]);
  }

  /**
   * EProf: Get event counter information
   *
   * @return EProf: Event counter information
   */
  @Override
  public ProfilerEprofData[] getEprofEventCounterInfo() {
    return null;
  }

  /**
   * Read profiler file
   *
   * @return Read profiler file
   */
  @Override
  public File getProfFile() {
    return this.profFile;
  }

  /**
   * Get profiler magic key
   *
   * @return magic key
   */
  @Override
  public String getFileType() {
    return FILE_ID_DPROF;
  }

  /**
   * Get the PA event specification value (EPRF only). Cache Instructions MEM_access Performance
   * Statistics
   *
   * @return PA event specification value (EPRF only)
   */
  @Override
  public String getPaEventName() {
    return null;
  }
}
