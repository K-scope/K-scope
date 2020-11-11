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
package jp.riken.kscope.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.JOptionPane;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.model.ProfilerMeasureModel;
import jp.riken.kscope.model.ProfilerTableBaseModel;
import jp.riken.kscope.profiler.IProfilerReader;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.profiler.ProfilerEprofData;
import jp.riken.kscope.profiler.ProfilerInfo;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.profiler.ProfilerMeasureInfo.MeasureData;
import jp.riken.kscope.profiler.dprof.DProfReader;
import jp.riken.kscope.profiler.eprof.EProfReader;
import jp.riken.kscope.profiler.utils.ProfilerReaderUtil;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;

/**
 * Profiler service
 *
 * @author RIKEN
 */
public class ProfilerService extends BaseService {
  /** Source file list */
  private SourceFile[] sourceFiles;
  /** Profiler information class */
  private ProfilerInfo profilerInfo;
  /** Profiler model list */
  private ProfilerTableBaseModel[] profilerModels;
  /** File types: DPRF, EPRF */
  private String fileType;
  /** PA event specification value: EPRF only */
  private String paEventName;
  /** Fortran parsing result storage database. */
  private Fortran fortranDb;
  /** Detailed profiler measurement interval information model */
  private ProfilerMeasureModel measureModel;
  /** Project folder */
  private File projectFolder;
  /** Profiler Properties */
  private ProfilerProperties properties;
  /** Measurement section information */
  private ProfilerMeasureInfo measureInfo;

  /**
   * Read profiler data.
   *
   * @param file Profiler data file
   */
  public void loadProfilerDataFile(File file) {
    // Generate a profiler reader
    IProfilerReader reader = factoryProfilerReader(file);
    if (reader == null) {
      this.addErrorInfo(
          Message.getString(
              "profilerservice.profilerdatafile.invalidfile")); // The profiler file could not be
      // identified.
      return;
    }

    // Read from a file
    try {
      reader.readFile(file);

      // Set the read data in the model
      if (reader instanceof DProfReader) {
        setDprofModel(reader);
      } else if (reader instanceof EProfReader) {
        setEprofModel(reader);
      }
      // File type
      fileType = reader.getFileType();
      // PA event specification value: EPRF only
      paEventName = reader.getPaEventName();

    } catch (IOException ex) {
      ex.printStackTrace();
      this.addErrorInfo(ex);
    } catch (Exception ex) {
      ex.printStackTrace();
      this.addErrorInfo(ex);
    }

    return;
  }

  /**
   * Set Dprof cost information in the model
   *
   * @param reader Profiler reader
   */
  private void setDprofModel(IProfilerReader reader) {
    // Read file name
    File file = reader.getProfFile();
    String key = file.getName();
    // Cost information: Get the line
    ProfilerDprofData[] costline = reader.getCostInfoLine();
    {
      // Set the cost information type and the ratio to the whole, and sort.
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.COST_LINE;
      setDprofInfo(costline, type);
      ProfilerTableBaseModel model = getProfilerModel(type);
      if (model != null) {
        model.setProfilerData(key, costline);
      }
    }

    // Cost info: Get the loop
    ProfilerDprofData[] costloop = reader.getCostInfoLoop();
    {
      // Set the cost information type and the ratio to the whole, and sort.
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.COST_LOOP;
      setDprofInfo(costloop, type);
      ProfilerTableBaseModel model = getProfilerModel(type);
      if (model != null) {
        model.setProfilerData(key, costloop);
      }
    }

    // Cost information: Get the procedure
    ProfilerDprofData[] costprocedure = reader.getCostInfoProcedure();
    {
      // Set the cost information type and the ratio to the whole, and sort.
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.COST_PROCEDURE;
      setDprofInfo(costprocedure, type);
      ProfilerTableBaseModel model = getProfilerModel(type);
      if (model != null) {
        model.setProfilerData(key, costprocedure);
      }
    }

    // Get call graph information
    ProfilerDprofData[] callgraph = reader.getDprofCallGraphInfo();
    {
      // Cost information type
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.CALLGRAPH;
      setDprofInfo(callgraph, type);
      ProfilerTableBaseModel model = getProfilerModel(type);
      if (model != null) {
        model.setProfilerData(key, callgraph);
      }
    }

    // Set Dprof information
    if (this.profilerInfo != null) {
      this.profilerInfo.putCostLine(key, costline);
      this.profilerInfo.putCostLoop(key, costloop);
      this.profilerInfo.putCostProcedure(key, costprocedure);
      this.profilerInfo.putCallgraph(key, callgraph);
    }

    return;
  }

  /**
   * Set Eprof event counter information in the model
   *
   * @param reader Profiler reader
   */
  private void setEprofModel(IProfilerReader reader) {
    // Read file name
    File file = reader.getProfFile();
    String key = file.getName();

    // Get Eprof event counter information
    ProfilerEprofData[] eventInfo = reader.getEprofEventCounterInfo();
    if (eventInfo == null || eventInfo.length <= 0) return;
    if (eventInfo[0] == null) return;

    // Get the measurement interval of Eprof.
    for (ProfilerEprofData data : eventInfo) {
      String groupname = data.getSymbol();
      // Search and return the CALL statement block from the subroutine name.
      List<IBlock[]> areas =
          searchProcedureUsage(
              groupname,
              this.properties.getEprofFunctionStart(),
              this.properties.getEprofFunctionEnd());
      if (areas != null) {
        data.setAreas(areas);
      }
    }

    PROFILERINFO_TYPE type = eventInfo[0].getInfoType();
    ProfilerTableBaseModel model = getProfilerModel(type);
    if (model != null) {
      model.setProfilerData(key, eventInfo);
    }

    // Set EProf information
    if (this.profilerInfo != null) {
      this.profilerInfo.putEventCounter(key, eventInfo);
    }
    return;
  }

  /**
   * Set the cost information type and the ratio to the whole in the cost information list and sort.
   *
   * @param costinfos Costs in the cost information list
   * @param type Cost information type
   */
  private void setDprofInfo(ProfilerDprofData[] costinfos, PROFILERINFO_TYPE type) {

    if (costinfos == null) return;

    // Set the cost information type
    for (ProfilerDprofData info : costinfos) {
      info.setInfoType(type);
      CodeLine code = info.getCodeLine();
      if (code == null) continue;
      // Set the source file
      SourceFile profsrcfile = info.getSourceFile();
      if (profsrcfile == null) continue;
      // Since the path of the source file at the time of reading the profiler is the path at the
      // time of creating the profiler,
      // Set the tool source file
      SourceFile toolfile = searchSourceFile(profsrcfile);
      if (toolfile == null) continue;
      info.getCodeLine().setSourceFile(toolfile);

      // Search and return the program unit to which it belongs from the code line information.
      // IBlock block = searchCodeLine(code);
      // if (block != null) {
      //    info.setBlock(block);
      // }
    }

    if (type != PROFILERINFO_TYPE.CALLGRAPH) {
      // Calculate the ratio to the whole. CALLGRAPF is already set by DProfReader ::
      // getDprofCallGraphInfo
      calculateRatio(costinfos);
      // Sort by sampling count
      sortCostInfo(costinfos);
    }
  }

  /**
   * Since the path of the source file at the time of reading the profiler is the path at the time
   * of creating the profiler, Set the tool source file
   *
   * @param profsrcfile Profiler source file
   * @return Tool source file
   */
  private SourceFile searchSourceFile(SourceFile profsrcfile) {
    if (this.sourceFiles == null || this.sourceFiles.length <= 0) return null;
    if (profsrcfile == null) return null;
    if (profsrcfile.getFile() == null) return null;

    // Search by file name only
    List<SourceFile> matchFiles = new ArrayList<SourceFile>();
    for (SourceFile file : this.sourceFiles) {
      String name = file.getFile().getName();
      String profname = profsrcfile.getFile().getName();
      // Compare filenames case-insensitive
      if (profname.equalsIgnoreCase(name)) {
        matchFiles.add(file);
      }
    }
    if (matchFiles.size() <= 0) return null;
    if (matchFiles.size() == 1) return matchFiles.get(0);

    // Since there are multiple, compare the paths
    // Compare paths and return the best matching file.
    File[] profpathlist = FileUtils.getPathList(profsrcfile.getFile());
    SourceFile findFile = null;
    int maxmatchpath = 0;
    for (SourceFile file : matchFiles) {
      File[] pathlist = FileUtils.getPathList(file.getFile());
      if (pathlist == null || pathlist.length <= 0) continue;
      int matchpath = 0;
      for (int i = pathlist.length - 1, profindex = profpathlist.length - 1;
          i >= 0 && profindex >= 0;
          i--, profindex--) {
        if (profpathlist[profindex].getName().equalsIgnoreCase(pathlist[i].getName())) {
          matchpath++;
        }
      }
      if (maxmatchpath < matchpath) {
        maxmatchpath = matchpath;
        findFile = file;
      }
    }

    return findFile;
  }

  /**
   * Calculate the ratio to the whole
   *
   * @param costinfos Cost information list
   */
  private void calculateRatio(ProfilerDprofData[] costinfos) {
    if (costinfos == null) return;
    float total = 0.0F;
    int count = 0;
    for (ProfilerDprofData cost : costinfos) {
      total += cost.getSampling();
      count++;
    }

    for (ProfilerDprofData cost : costinfos) {
      float value = cost.getSampling();
      value = value / total;
      cost.setRatio(value);
    }

    return;
  }

  /**
   * Source cost information by sampling count. Sort the number of samplings in descending order.
   *
   * @param costinfos Cost information
   * @return Sort cost information
   */
  private ProfilerDprofData[] sortCostInfo(ProfilerDprofData[] costinfos) {
    if (costinfos == null) return null;

    // Sort the source file list
    java.util.Arrays.sort(
        costinfos,
        new java.util.Comparator<ProfilerDprofData>() {
          public int compare(ProfilerDprofData o1, ProfilerDprofData o2) {
            ProfilerDprofData src1 = (ProfilerDprofData) o1;
            ProfilerDprofData src2 = (ProfilerDprofData) o2;
            float diff = src2.getSampling() - src1.getSampling();
            if (diff < 0.0) return -1;
            else if (diff > 0.0) return 1;
            return 0;
          }
        });
    return costinfos;
  }

  /**
   * Read the beginning of the file and create a reader to read
   *
   * @param file Read file
   * @return Profile reader
   */
  private IProfilerReader factoryProfilerReader(File file) {
    FileInputStream fis = null;
    try {
      fis = new FileInputStream(file);
      byte[] magickey = new byte[] {0x00, 0x00, 0x00, 0x00};
      fis.read(magickey, 0, 4);

      char[] data =
          new char[] {
            (char) magickey[0], (char) magickey[1], (char) magickey[2], (char) magickey[3]
          };
      char[] repdata =
          new char[] {
            (char) magickey[3], (char) magickey[2], (char) magickey[1], (char) magickey[0]
          };
      String key = String.valueOf(data);
      String repkey = String.valueOf(repdata);
      int endian = -1;
      // DPRF
      {
        final String MAGIC_ID = "DPRF";
        if (MAGIC_ID.equalsIgnoreCase(key)) {
          endian = ProfilerReaderUtil.BIG_ENDIAN;
        } else if (MAGIC_ID.equalsIgnoreCase(repkey)) {
          endian = ProfilerReaderUtil.LITTLE_ENDIAN;
        }
        if (endian != -1) {
          IProfilerReader reader = new DProfReader();
          reader.setEndian(endian);
          return reader;
        }
      }
      // EPRF
      {
        final String MAGIC_ID = "EPRF";
        if (MAGIC_ID.equalsIgnoreCase(key)) {
          endian = ProfilerReaderUtil.BIG_ENDIAN;
        } else if (MAGIC_ID.equalsIgnoreCase(repkey)) {
          endian = ProfilerReaderUtil.LITTLE_ENDIAN;
        }
        if (endian != -1) {
          IProfilerReader reader = new EProfReader();
          reader.setEndian(endian);
          return reader;
        }
      }

    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      if (fis != null) {
        try {
          fis.close();
        } catch (Exception e) {
        }
      }
    }

    return null;
  }

  /**
   * Set the source file list
   *
   * @param files Source file list
   */
  public void setSourceFiles(SourceFile[] files) {
    this.sourceFiles = files;
  }

  /**
   * Profiler information class
   *
   * @param info Profiler information class
   */
  public void setProfilerInfo(ProfilerInfo info) {
    this.profilerInfo = info;
  }

  /**
   * Set profiler model
   *
   * @param models Profiler model list
   */
  public void setProfilerModels(ProfilerTableBaseModel[] models) {
    this.profilerModels = models;
  }

  /**
   * Get profiler model
   *
   * @param type Profiler data type
   * @return Profiler model
   */
  private ProfilerTableBaseModel getProfilerModel(PROFILERINFO_TYPE type) {
    if (this.profilerModels == null) return null;
    for (ProfilerTableBaseModel model : this.profilerModels) {
      if (model.getEnumInfo() == type) {
        return model;
      }
    }
    return null;
  }

  /**
   * File type: DPRF, EPRF
   *
   * @return fileType File type: DPRF, EPRF
   */
  public String getFileType() {
    return fileType;
  }

  /**
   * Searches back the program unit to which it belongs from the code line information.
   *
   * @param line Code line information
   * @return Program unit. If not, it returns null.
   */
  @SuppressWarnings("unused")
  private IBlock searchCodeLine(CodeLine line) {
    if (line == null) return null;
    if (this.fortranDb == null) return null;
    SourceFile file = line.getSourceFile();
    if (file == null) return null;
    LanguageSearcher searcher = new LanguageSearcher(this.fortranDb, line);
    IBlock block = searcher.searchCodeLine();

    return block;
  }

  /**
   * Search and return the block from the subroutine name.
   *
   * @param groupname Group name
   * @param callstart Start subroutine name
   * @param callend End subroutine name
   * @return Subroutine CALL statement {start subroutine, end subroutine}. If not, it returns null.
   */
  private List<IBlock[]> searchProcedureUsage(String groupname, String callstart, String callend) {
    if (groupname == null) return null;
    if (callstart == null) return null;
    if (callend == null) return null;
    if (this.fortranDb == null) return null;

    List<IBlock[]> list = new ArrayList<IBlock[]>();
    // If all, the whole program
    if ("all".equalsIgnoreCase(groupname)) {
      String mainname = fortranDb.getMainName();
      Procedure proc = fortranDb.search_subroutine(mainname);
      if (proc != null) {
        list.add(new IBlock[] {proc, null});
        return list;
      }
    }
    IBlock[] callblocks = new IBlock[2];
    int index = 0;
    for (String callname : new String[] {callstart, callend}) {
      LanguageSearcher searcher = new LanguageSearcher(this.fortranDb, callname);
      IBlock[] blocks = searcher.searchProcedureUsage();
      if (blocks == null) continue;
      for (IBlock block : blocks) {
        if (block == null) continue;
        if (!(block instanceof ProcedureUsage)) continue;
        // Check if the group name exists in the actual argument
        List<Expression> args = ((ProcedureUsage) block).getArguments();
        if (args == null || args.size() <= 0) continue;
        for (Expression arg : args) {
          String argText = StringUtils.trimQuote(arg.getLine());
          if (groupname.equalsIgnoreCase(argText)) {
            callblocks[index] = block;
            if (index == 0) {
              list.add(new IBlock[] {block, null});
            } else {
              // set callend to null
              for (IBlock[] procs : list) {
                if (procs[1] == null) {
                  procs[1] = block;
                  break;
                }
              }
            }
          }
        }
      }
      index++;
    }
    if (list.size() <= 0) {
      return null;
    }
    return list;
  }

  /**
   * Set up the Fortran parsing result storage database
   *
   * @param db Fortran parsing result storage database
   */
  public void setFortranLanguage(Fortran db) {
    this.fortranDb = db;
  }

  /**
   * Set up a detailed profiler measurement interval information model
   *
   * @param model Detailed profiler measurement interval information model
   */
  public void setMeasureModel(ProfilerMeasureModel model) {
    this.measureModel = model;
  }

  /**
   * Add detailed profiler measurement interval
   *
   * @param code Measurement interval line
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   */
  public void addProfilerMeasureInfo(CodeLine code, String name, String number, String level) {
    ProfilerMeasureInfo info = this.profilerInfo.getMeasureInfo();
    if (info == null) {
      info = new ProfilerMeasureInfo();
      this.profilerInfo.setMeasureInfo(info);
    }
    CodeLine addcode = new CodeLine(code);
    if (this.projectFolder != null) {
      SourceFile file = addcode.getSourceFile();
      String path = FileUtils.getRelativePath(file.getFile(), this.projectFolder);
      if (path == null) {
        JOptionPane.showMessageDialog(
            null,
            Message.getString(
                "profilerservice.profilermeasureinfo.notexists",
                file.getFile()), // does not exist or is not a file.
            Message.getString("profilerservice.error"), // error
            JOptionPane.ERROR_MESSAGE);
        return;
      }
      file.setFile(new File(path));
    }
    info.addMeasureData(addcode, name, number, level);
    this.measureModel.setMeasureInfo(info);
  }

  /**
   * Add detailed profiler measurement interval
   *
   * @param blocks Measurement interval {start block to end block}
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   */
  public void addProfilerMeasureInfo(IBlock[] blocks, String name, String number, String level) {
    ProfilerMeasureInfo info = this.profilerInfo.getMeasureInfo();
    if (info == null) {
      info = new ProfilerMeasureInfo();
      this.profilerInfo.setMeasureInfo(info);
    }
    info.addMeasureData(blocks, name, number, level);
    this.measureModel.setMeasureInfo(info);
  }

  /**
   * Set the project folder
   *
   * @param folder Project folder
   */
  public void setProjectFolder(File folder) {
    this.projectFolder = folder;
  }

  /**
   * Get the PA event specification value (EPRF only). Cache Instructions MEM_access Performance
   * Statistics
   *
   * @return PA event specification value (EPRF only)
   */
  public String getPaEventName() {
    return this.paEventName;
  }

  /**
   * Set profiler properties
   *
   * @param properties Profiler properties
   */
  public void setPropertiesProfiler(ProfilerProperties properties) {
    this.properties = properties;
  }

  /**
   * Set measurement section information
   *
   * @param info Measurement section information
   */
  public void setMeasureInfo(ProfilerMeasureInfo info) {
    this.measureInfo = info;
  }

  /**
   * Save the measurement interval
   *
   * @param saveFolder Save folder
   * @return true = Saved successfully
   * @throws Exception File I / O error
   */
  public boolean saveMeasureFile(File saveFolder) throws Exception {
    if (this.measureInfo == null) {
      this.addErrorInfo(
          Message.getString(
              "profilerservice.measurefile.measureline.empty")); // Measurement interval is not set
      return false;
    }
    List<MeasureData> list = this.measureInfo.getMeasureList();
    if (list == null || list.size() <= 0) {
      this.addErrorInfo(
          Message.getString(
              "profilerservice.measurefile.measureline.empty")); // Measurement interval is not set
      return false;
    }

    // Create a line of code for the measurement section and organize it for each source file.
    Map<SourceFile, List<CodeLine>> mapLines = createMeasureLine(list);
    if (mapLines == null || mapLines.size() <= 0) {
      this.addErrorInfo(
          Message.getString(
              "profilerservice.measurefile.measureline.null")); // Could not get the measurement
      // interval
      return false;
    }
    Set<SourceFile> keySet = mapLines.keySet();
    for (SourceFile file : keySet) {
      List<CodeLine> lines = mapLines.get(file);
      FileService service = new FileService(this.getErrorInfoModel());

      // Read source code line
      CodeLine[] codes = service.readSourceFile(file, this.projectFolder);
      if (codes == null) {
        this.addErrorInfo(
            Message.getString("profilerservice.measurefile.sourcecode.invalidread")
                + // Failed to read the source code.
                "[file="
                + file.getPath()
                + "]");
        return false;
      }
      // Insert the measurement statement into the read code list
      List<CodeLine> sources = new ArrayList<CodeLine>();
      sources.addAll(Arrays.asList(codes));
      // Insert from the end
      for (int i = lines.size() - 1; i >= 0; i--) {
        sources.add(lines.get(i).getStartLine(), lines.get(i));
      }

      // File output
      File outpath = null;
      if (saveFolder == null) {
        outpath = file.getFile();
      } else if (file.getFile().isAbsolute()) {
        String path = FileUtils.getRelativePath(file.getFile(), this.projectFolder);
        if (path != null && (new File(path).isAbsolute())) {
          outpath = new File(saveFolder, file.getFile().getName());
        } else {
          outpath = new File(saveFolder, path);
        }
      } else {
        outpath = new File(saveFolder, file.getFile().getPath());
      }
      service.writeFile(outpath, sources.toArray(new CodeLine[0]));
    }

    return true;
  }

  /**
   * Create a line of code for the measurement section and organize it for each source file.
   *
   * @param list Measurement interval list
   * @return Measurement code line for each source
   */
  private Map<SourceFile, List<CodeLine>> createMeasureLine(List<MeasureData> list) {
    if (list == null || list.size() <= 0) return null;

    Map<SourceFile, List<CodeLine>> mapLines = new HashMap<SourceFile, List<CodeLine>>();
    for (MeasureData data : list) {
      // Measurement section
      CodeLine measureLine = data.getMeasureArea();

      // Insertion measurement statement
      SourceFile file = measureLine.getSourceFile();
      int start = measureLine.getStartLine();
      int end = measureLine.getEndLine();
      String groupname = data.getGroupname();
      String number = data.getNumber();
      String level = data.getLevel();
      // Create insertion measurement syntax from group name, detail number, level
      String startCode = this.properties.createEprofStatementStart(groupname, number, level);
      String endCode = this.properties.createEprofStatementEnd(groupname, number, level);
      String fn = null;
      if (file != null) {
        fn = file.getPath();
      }
      // Start code
      CodeLine startline = new CodeLine(file, startCode, start - 1, fn);
      // Exit code
      CodeLine endline = new CodeLine(file, endCode, end, fn);

      List<CodeLine> lines = null;
      if (mapLines.containsKey(file)) {
        lines = mapLines.get(file);
      } else {
        lines = new ArrayList<CodeLine>();
        mapLines.put(file, lines);
      }
      lines.add(startline);
      lines.add(endline);
    }

    // Sort lines of code by line number
    Set<SourceFile> keySet = mapLines.keySet();
    for (SourceFile key : keySet) {
      List<CodeLine> lines = mapLines.get(key);
      // Sort execution
      Collections.sort(
          lines,
          new Comparator<CodeLine>() {
            public int compare(CodeLine code1, CodeLine code2) {
              return code1.getStartLine() - code2.getStartLine();
            }
          });
    }
    if (mapLines.size() <= 0) return null;
    return mapLines;
  }
}
