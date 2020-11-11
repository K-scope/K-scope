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
package jp.riken.kscope.profiler.eprof;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.profiler.IProfilerReader;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.profiler.ProfilerEprofData;
import jp.riken.kscope.profiler.common.BaseReader;
import jp.riken.kscope.profiler.common.MagicKey;
import jp.riken.kscope.profiler.common.PaDiscrimInfo;

/**
 * Read the EProfile file and retain the information
 *
 * @author RIKEN
 */
public class EProfReader extends BaseReader implements IProfilerReader {

  private final String FILE_ID_EPROF = "EPRF"; // File identifier representing the EProf file
  private final int FILE_ID_LENGTH = 4; // File identification character length
  private final int MEASURE_TIME_LENGTH = 32; // Length of measurement time information string
  private final short PROFILER_VERSION = 0x402;
  /** Hardware monitor information (PA information) table: Cache table */
  private final String PA_EVENT_CACHE = "Cache";
  /** Hardware monitor information (PA information) table: Instructions table */
  private final String PA_EVENT_INSTRUCTIONS = "Instructions";
  /** Hardware monitor information (PA information) table: MEM_access table */
  private final String PA_EVENT_MEM_ACCESS = "MEM_access";
  /** Hardware monitor information (PA information) table: Performance table */
  private final String PA_EVENT_PERFORMANCE = "Performance";
  /** Hardware monitor information (PA information) table: Statistics table */
  private final String PA_EVENT_STATISTICS = "Statistics";

  /* Size of PA information table for each PA event specification value */
  private final Map<String, Integer> MAP_PA_INFO_LENGTH =
      new HashMap<String, Integer>() {
        private static final long serialVersionUID = 1L;

        {
          put(PA_EVENT_CACHE, 10);
          put(PA_EVENT_INSTRUCTIONS, 9);
          put(PA_EVENT_MEM_ACCESS, 10);
          put(PA_EVENT_PERFORMANCE, 10);
          put(PA_EVENT_STATISTICS, 10);
        }
      };

  /** Magic key */
  private MagicKey magicKey;
  /** Common information */
  private CommonInfo commonInfo;
  /** Event counter information */
  private EventCounterInfo eventInfo;
  /** Endian settings when loading */
  private int endian;
  /** Read profiler file */
  private File profFile;

  /**
   * Read the information of the specified profiler file
   *
   * @param fEProf Profiler file to read
   * @param endian Endian setting LITTLE_ENDIAN: 0x00 BIG_ENDIAN: 0x01;
   * @throws IOException File read error
   */
  @Override
  public void readFile(File fEProf, int endian) throws Exception {
    // set endian
    this.endian = endian;
    this.profFile = fEProf;

    long fileSize = fEProf.length();
    ByteBuffer byteBuf = ByteBuffer.allocate((int) fileSize);
    FileInputStream fis = new FileInputStream(fEProf);

    while (fis.available() > 0) {
      byteBuf.put((byte) fis.read());
    }
    byteBuf.flip();

    magicKey = readMagicKey(byteBuf);
    commonInfo = readCommonInfo(byteBuf);
    eventInfo = readEventCounterInfo(byteBuf);

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
  public EventCounterInfo getEventCounterInfo() {
    return this.eventInfo;
  }

  /* Reading magic key information */
  private MagicKey readMagicKey(ByteBuffer byteBuf) throws Exception {
    MagicKey newMagicKey = new MagicKey();
    String fileID = getString(byteBuf, FILE_ID_LENGTH);

    if (!FILE_ID_EPROF.equals(fileID)) {
      throw new Exception(
          Message.getString("dialog.common.error")
              + // error
              ": "
              + Message.getString("eprofreader.exception.notvalid")); // Not a valid EProf file.
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
                  "eprofreader.exception.outside",
                  version,
                  PROFILER_VERSION)); // This is an unsupported EProf version. Read =% # 04X Support
                                      // =% # 04X
    }
    newMagicKey.setVer(version);
    return newMagicKey;
  }

  /**
   * Reading common information
   *
   * @param byteBuf File byte buffer
   * @return Common information
   */
  private CommonInfo readCommonInfo(ByteBuffer byteBuf) {
    CommonInfo newCommonInfo = new CommonInfo();
    newCommonInfo.setProcessNum(getInt(byteBuf));
    newCommonInfo.setMeasureOption(getInt(byteBuf));
    newCommonInfo.setRunStyle(getShort(byteBuf));
    newCommonInfo.setThreadNum(getShort(byteBuf));
    newCommonInfo.setCpuClock(getInt(byteBuf));
    newCommonInfo.setMeasureTimeInfo(getString(byteBuf, MEASURE_TIME_LENGTH));
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

  /**
   * Reading event counter information
   *
   * @param byteBuf File byte buffer
   * @return Event counter information
   */
  private EventCounterInfo readEventCounterInfo(ByteBuffer byteBuf) {
    EventCounterInfo eventInfo = new EventCounterInfo();
    List<EventCounterGroup> groupList = new ArrayList<EventCounterGroup>();
    // Number of event counters
    int eventCount = getInt(byteBuf);
    eventInfo.setEventcount(eventCount);
    for (int i = 0; i < eventCount; i++) {
      EventCounterGroup group = new EventCounterGroup();
      // Counter group name length
      int length = getInt(byteBuf);
      // Counter group name
      group.setGroupname(getString(byteBuf, length));
      // Counter detail number
      group.setDetailno(getInt(byteBuf));
      // Basic information
      group.setBaseInfo(readBaseInfo(byteBuf));
      // MPI information
      if (this.commonInfo.isOptMpi()) {
        MpiInfo mpiInfo = readMpiInfo(byteBuf);
        group.setMpiInfo(mpiInfo);
      }
      // Hardware monitor information
      if (this.commonInfo.isOptPa()) {
        HardwareMonitorInfo hardwareInfo = readHardwareMonitorInfo(byteBuf);
        group.setHardwareInfo(hardwareInfo);
      }
      groupList.add(group);
    }
    if (groupList.size() > 0) {
      eventInfo.setEventGroupList(groupList);
    }
    return eventInfo;
  }

  /**
   * Reading basic information
   *
   * @param byteBuf File byte buffer
   * @return Basic information
   */
  private BaseInfo readBaseInfo(ByteBuffer byteBuf) {
    BaseInfo baseInfo = new BaseInfo();
    // Counter call count int
    baseInfo.setCallCount(getInt(byteBuf));
    // elapsed time float
    baseInfo.setElapsTime(getFloat(byteBuf));
    // User CPU time float
    baseInfo.setUserTime(getFloat(byteBuf));
    // System CPU time float
    baseInfo.setSystemTime(getFloat(byteBuf));

    return baseInfo;
  }

  /**
   * Reading MPI information
   *
   * @param byteBuf File byte buffer
   * @return MPI information
   */
  private MpiInfo readMpiInfo(ByteBuffer byteBuf) {
    MpiInfo mpiInfo = new MpiInfo();
    // Number of MPI functions
    int mpicount = getInt(byteBuf);
    mpiInfo.setMpiCount(mpicount);
    // MPI information: MPI function
    List<MpiFunction> mpiFunctionList = new ArrayList<MpiFunction>();
    for (int i = 0; i < mpicount; i++) {
      MpiFunction function = readMpiFunction(byteBuf);
      mpiFunctionList.add(function);
    }
    if (mpiFunctionList.size() > 0) {
      mpiInfo.setMpiFunctionList(mpiFunctionList);
    }
    return mpiInfo;
  }

  /**
   * Reading MPI functions
   *
   * @param byteBuf File byte buffer
   * @return MPI function
   */
  private MpiFunction readMpiFunction(ByteBuffer byteBuf) {
    MpiFunction function = new MpiFunction();
    // Index of MPI function
    function.setMpiIndex(getInt(byteBuf));
    // Number of calls
    function.setCallCount(getInt(byteBuf));
    // elapsed time
    function.setElapsTime(getFloat(byteBuf));
    // Wait time
    function.setWaitTime(getFloat(byteBuf));
    // Message length
    function.setMessageLength(getLong(byteBuf));
    // Number of times the message length is 0 bytes or more and less than 4 Kbytes
    function.setCountMessage4k(getInt(byteBuf));
    // Number of times the message length is 4Kbyte or more and less than 64Kbyte
    function.setCountMessage64k(getInt(byteBuf));
    // Number of times the message length is 64Kbyte or more and less than 1024Kbyte
    function.setCountMessage1024k(getInt(byteBuf));
    // Number of times when the message length is 1024Kbyte or more
    function.setCountMessage1024kOver(getInt(byteBuf));

    return function;
  }

  /**
   * Read hardware monitor information
   *
   * @param byteBuf File byte buffer
   * @return Hardware monitor information
   */
  private HardwareMonitorInfo readHardwareMonitorInfo(ByteBuffer byteBuf) {
    HardwareMonitorInfo hardwareInfo = new HardwareMonitorInfo();
    // Number of measurement threads
    int threadcount = getInt(byteBuf);
    hardwareInfo.setThreadCount(threadcount);
    // Hardware monitor information (PA information) table list
    List<HardwarePaTable> paInfo = new ArrayList<HardwarePaTable>();
    for (int i = 0; i < threadcount; i++) {
      HardwarePaTable table = readHardwarePaTable(byteBuf);
      paInfo.add(table);
    }
    if (paInfo.size() > 0) {
      hardwareInfo.setPaInfo(paInfo);
    }
    return hardwareInfo;
  }

  /**
   * Read hardware monitor information
   *
   * @param byteBuf File byte buffer
   * @return Hardware monitor information
   */
  private HardwarePaTable readHardwarePaTable(ByteBuffer byteBuf) {
    HardwarePaTable table = new HardwarePaTable();
    // Thread number
    table.setThreadno(getInt(byteBuf));
    // Hardware monitor information
    int paEventLength = MAP_PA_INFO_LENGTH.get(this.commonInfo.getPaEventVal());
    double[] pa = new double[paEventLength];
    for (int j = 0; j < paEventLength; j++) {
      pa[j] = getDouble(byteBuf);
    }
    table.setPaTable(pa);
    return table;
  }

  /**
   * Read from profiler file
   *
   * @param profilerfile Profiler file
   * @throws Exception Read error
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

  /**
   * Get endian
   *
   * @return endian
   */
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
    return null;
  }

  /**
   * Cost information list: Get a loop
   *
   * @return Cost information list: Loop
   */
  @Override
  public ProfilerDprofData[] getCostInfoLoop() {
    return null;
  }

  /**
   * Cost information list: Get the procedure
   *
   * @return Cost information list: Procedure
   */
  @Override
  public ProfilerDprofData[] getCostInfoProcedure() {
    return null;
  }

  /**
   * Get event counter information
   *
   * @return Event counter information
   */
  @Override
  public ProfilerEprofData[] getEprofEventCounterInfo() {
    if (this.eventInfo == null) return null;

    // Counter group list
    List<EventCounterGroup> eventGroupList = this.eventInfo.getEventGroupList();
    if (eventGroupList == null) return null;

    // Profiler data type
    PROFILERINFO_TYPE type = null;
    // Hardware monitor information (PA information) table type
    if (PA_EVENT_CACHE.equals(this.commonInfo.getPaEventVal())) {
      type = PROFILERINFO_TYPE.EVENTCOUNTER_CACHE;
    } else if (PA_EVENT_INSTRUCTIONS.equals(this.commonInfo.getPaEventVal())) {
      type = PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS;
    } else if (PA_EVENT_MEM_ACCESS.equals(this.commonInfo.getPaEventVal())) {
      type = PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS;
    } else if (PA_EVENT_PERFORMANCE.equals(this.commonInfo.getPaEventVal())) {
      type = PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE;
    } else if (PA_EVENT_STATISTICS.equals(this.commonInfo.getPaEventVal())) {
      type = PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS;
    }

    List<ProfilerEprofData> list = new ArrayList<ProfilerEprofData>();
    for (EventCounterGroup group : eventGroupList) {
      ProfilerEprofData info = new ProfilerEprofData();
      info.setSymbol(group.getGroupname());
      BaseInfo baseinfo = group.getBaseInfo();
      if (baseinfo == null) continue;
      // Number of counter calls
      info.setCallCount(baseinfo.getCallCount());
      // elapsed time
      info.setElapsTime(baseinfo.getElapsTime());
      // User CPU time
      info.setUserTime(baseinfo.getUserTime());
      // System CPU time
      info.setSystemTime(baseinfo.getSystemTime());
      // Hardware monitor information (PA information) table
      info.setHardwareInfo(group.getHardwareInfo());
      // Hardware monitor information (PA information) table type
      info.setInfoType(type);

      list.add(info);
    }
    if (list.size() <= 0) {
      return null;
    }
    return list.toArray(new ProfilerEprofData[0]);
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
   * Get call graph information
   *
   * @return Call graph information
   */
  @Override
  public ProfilerDprofData[] getDprofCallGraphInfo() {
    return null;
  }

  /**
   * Get profiler magic key
   *
   * @return magic key
   */
  @Override
  public String getFileType() {
    return FILE_ID_EPROF;
  }

  /**
   * Get the PA event specification value (EPRF only). Cache Instructions MEM_access Performance
   * Statistics
   *
   * @return PA event specification value (EPRF only)
   */
  @Override
  public String getPaEventName() {
    return commonInfo.getPaEventVal();
  }
}
