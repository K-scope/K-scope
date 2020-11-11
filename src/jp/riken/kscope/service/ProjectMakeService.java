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
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.ValidateLanguage;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * Makefile execution class. Execute Makefile to rebuild the database.
 *
 * @author RIKEN
 */
public class ProjectMakeService extends BaseService {

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  /** make command execution folder */
  private File workdirectory;
  /** make command output stream */
  private OutputStream outStream;
  /** Project model */
  private ProjectModel projectModel;
  /** Fortran parsing result storage database: current database. */
  private Fortran currentDb = null;
  /** XML file search path list */
  private List<File> listSearchPath;
  /** Thread execution flag true: Continue execution / false: Cancel. */
  private boolean m_running = true;

  private AppController controller;

  /**
   * Constructor.
   *
   * @param BUILD_COMMAND Make command
   * @param work Make command execution folder
   * @param controller AddController
   */
  public ProjectMakeService(File work, AppController controller) {
    if (debug) {
      debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    }
    this.workdirectory = work;
    this.controller = controller;
  }

  /**
   * Re-execute the structural information.
   *
   * @return Success or failure
   */
  public boolean rebuild() {
    Application.status.setProgressStart(true);
    if (this.listSearchPath == null || this.listSearchPath.size() <= 0) {
      this.addErrorInfo("can not set search path list.");
      return false;
    }

    try {
      // Get XML file list
      ProjectService service = new ProjectService();
      SourceFile[] listBuildXml =
          service.getSourceFiles(
              this.listSearchPath.toArray(new File[0]), FILE_TYPE.XCODEML_XML, true);
      // Check if the source file exists.
      listBuildXml = validateXmlFiles(listBuildXml);
      if (listBuildXml == null || listBuildXml.length <= 0) {
        // There was no intermediate code (XML).
        String msg = Message.getString("projectmakeservice.rebuild.notexists.xml");
        this.addErrorInfo(msg);
        return false;
      }

      // Current file list
      List<SourceFile> listCurrentSrc = this.currentDb.getSourceFileList();
      // Current XML file list
      List<SourceFile> listCurrentXml = new ArrayList<SourceFile>();
      for (SourceFile src : listCurrentSrc) {
        SourceFile xml = src.getRelationFile();
        listCurrentXml.add(xml);
      }
      // Update file list
      SourceFile[] listUpdate =
          getUpdateFiles(listCurrentXml.toArray(new SourceFile[0]), listBuildXml);
      SourceFile[] listDelete =
          getDeleteFiles(listCurrentXml.toArray(new SourceFile[0]), listBuildXml);
      if ((listUpdate == null || listUpdate.length <= 0)
          && (listDelete == null || listDelete.length <= 0)) {
        // There was no update file.
        String msg = Message.getString("projectmakeservice.rebuild.notexists.updatefile");
        this.addErrorInfo(msg);
        return false;
      }

      // Parse the update file.
      Fortran buildDb = parseSourceFile(listUpdate);

      // Make a module copy of the database
      copyModules(buildDb, this.currentDb, listDelete);

      // Associate the variable definition.
      buildDb.analyseDB();

      // Validate the database.
      ValidateLanguage validate = new ValidateLanguage(buildDb);
      LanguageVisitor visitor = new LanguageVisitor(validate);
      visitor.entry();
      int error = validate.analyseTypes();
      if (error > 0) {
        this.getErrorInfoModel().addErrorInfos(validate.getErrorList());
        String msg = Message.getString("validatelanguage.final.error", error);
        this.addErrorInfo(msg);
      }

      // Copy the created database to the original database
      this.currentDb.copyShallow(buildDb);

      // Set the XML file list
      this.projectModel.setListXmlFile(listBuildXml);

      return true;

    } catch (LanguageException lang_ex) {
      Logger.error(lang_ex);
      Logger.error(lang_ex.getCodeInfo());
      lang_ex.printStackTrace();
      // Set the error location information
      this.addErrorInfo(lang_ex);
      return false;
    } catch (InterruptedException ex) {
      // End due to cancellation: projectmakeservice.rebuild.cancel = Canceled due to cancellation.
      String msg = Message.getString("projectmakeservice.rebuild.cancel");
      this.addErrorInfo(msg);
      Application.status.setMessageStatus(msg);
      return false;
    } catch (Exception ex) {
      Logger.error(ex);
      ex.printStackTrace();

      String error_message = ex.getMessage();
      if (error_message == null) {
        error_message = ex.toString();
      }
      // Set the error location information
      this.addErrorInfo(error_message);
      return false;
    } finally {
      Application.status.setProgressStart(false);
    }
  }

  /**
   * Validate the XML file. Check if the source file exists.
   *
   * @param xmlfiles
   * @return Fortran source file list
   */
  private SourceFile[] validateXmlFiles(SourceFile[] xmlfiles) {
    if (xmlfiles == null) return null;

    // Create XML parser
    XcodeMLParserStax fortranParser = new XcodeMLParserStax();
    List<SourceFile> list = new ArrayList<SourceFile>();
    for (SourceFile xml : xmlfiles) {
      try {
        if (xml.getRelationFile() != null && xml.getRelationFile().getFile() != null) {
          SourceFile file = xml.getRelationFile();
          if (file.getFile().exists()) {
            list.add(xml);
          }
          continue;
        }
        // Parse the XML file.
        fortranParser.readFile(xml);
        fortranParser.parseSourceFile();
        SourceFile source = fortranParser.getLanguageFile();
        if (source != null && source.getFile() != null) {
          if (source.getFile().exists()) {
            list.add(xml);
          }
        }

      } catch (LanguageException lang_ex) {
      }
    }

    if (list.size() <= 0) return null;
    return list.toArray(new SourceFile[0]);
  }

  /**
   * Get the list of deleted files.
   *
   * @param currents Current file list
   * @param builds Rebuild file list
   * @return Deleted file list
   */
  private SourceFile[] getDeleteFiles(SourceFile[] currents, SourceFile[] builds) {
    if (currents == null || currents.length <= 0) return null;
    if (builds == null || builds.length <= 0) return null;

    // Delete file list
    List<SourceFile> listDelete = new ArrayList<SourceFile>();
    List<SourceFile> listCurrent = Arrays.asList(currents);
    List<SourceFile> listBuild = Arrays.asList(builds);
    for (SourceFile file : listCurrent) {
      if (!listBuild.contains(file)) {
        listDelete.add(file);
      }
    }
    if (listDelete.size() <= 0) {
      return null;
    }

    return listDelete.toArray(new SourceFile[0]);
  }

  /**
   * Get the update file list. Get the files whose update date has been changed and the added files.
   *
   * @param currents Current file list
   * @param builds Rebuild file list
   * @return Update / additional file list
   */
  private SourceFile[] getUpdateFiles(SourceFile[] currents, SourceFile[] builds) {
    if (builds == null || builds.length <= 0) return null;
    if (currents == null || currents.length <= 0) return builds;

    // Update file list
    List<SourceFile> listUpdate = new ArrayList<SourceFile>();
    List<SourceFile> listCurrent = Arrays.asList(currents);
    List<SourceFile> listBuild = Arrays.asList(builds);
    for (SourceFile file : listBuild) {
      // Additional files
      if (!listCurrent.contains(file)) {
        listUpdate.add(file);
        continue;
      }
      // Update date check
      Date currentDate = null;
      Date buildDate = file.getModifyDate();
      int idx = listCurrent.indexOf(file);
      if (idx >= 0) {
        SourceFile currentFile = listCurrent.get(idx);
        if (currentFile != null) {
          currentDate = currentFile.getModifyDate();
        }
      }
      if (currentDate == null) {
        listUpdate.add(file);
      } else if (buildDate == null) {
        listUpdate.add(file);
      } else if (!currentDate.equals(buildDate)) {
        listUpdate.add(file);
      }
    }
    if (listUpdate.size() <= 0) {
      return null;
    }

    return listUpdate.toArray(new SourceFile[0]);
  }

  /** Cancel the thread execution. */
  public void cancelRunning() {
    m_running = false;
  }

  /**
   * Check if thread execution is cancelled
   *
   * @return true = Cancel
   */
  public boolean isCancel() {
    return !this.m_running;
  }

  /**
   * Set the make command output stream.
   *
   * @param out make command output stream
   */
  public void setOutputStream(OutputStream out) {
    this.outStream = out;
  }

  /**
   * Set up a Fortran database
   *
   * @param fortran Fortran database
   */
  public void setFortranLanguage(Fortran fortran) {
    this.currentDb = fortran;
  }

  /**
   * Execute clean command from ProjectProperties. Default is "make clean".
   *
   * @return true = Normal termination or continuous execution
   * @throws Exception
   */
  public boolean executeCleanCommand() throws Exception {
    ProjectProperties pproperties = this.controller.getPropertiesProject();
    String clean_command = pproperties.getValueByKey(ProjectProperties.CLEAN_COMMAND);
    if (clean_command == null || clean_command.length() <= 0) return true;
    // Status message
    Application.status.setProgressStart(true);
    if (debug) System.out.println("Executing " + clean_command);
    String[] exec_commands = null;
    Application.status.setMessageStatus(clean_command);

    // execute make command
    int result = -1;
    try {
      result = SwingUtils.processRun(clean_command.split(" "), this.workdirectory, this.outStream);
      if (result != 0) { // Clean command failed. Ask if we should
        // continue.
        if (JOptionPane.showConfirmDialog(
                null,
                Message.getString("projectmakeservice.executecleancommand.continue.message"),
                Message.getString("projectmakeservice.executecleancommand.error"),
                JOptionPane.YES_NO_OPTION)
            != JOptionPane.YES_OPTION) {
          // Status message
          Application.status.setProgressStart(false);
          return false;
        }
      }
      Application.status.setProgressStart(false);
      return true;
    } catch (Exception ex) {
      ex.printStackTrace();
      throw ex;
    }
  }

  /**
   * Run the make command.
   *
   * @return true = Normal termination or continuous execution
   * @throws Exception
   */
  public boolean executeMakeCommand() throws Exception {
    // Status message
    Application.status.setProgressStart(true);
    ProjectProperties pproperties = this.controller.getPropertiesProject();
    String build_command = pproperties.getBuildCommand();
    if (build_command == null || build_command.length() <= 0) return false;
    String[] exec_commands = null;
    if (debug) System.out.println("Use remote build is " + pproperties.useRemoteBuild());
    if (pproperties.useRemoteBuild()) {
      String RS =
          ProjectProperties.getRemoteService(
              pproperties.getPropertyValue(RemoteBuildProperties.SETTINGS_FILE).getValue());
      System.out.println("Remote service " + RS);
      if (useServer(pproperties)) {
        if (RS.equals(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS)) {
          // inject remote build command
          String[] diaas_cl = pproperties.getCommandLineOptions(RS);
          int formal_commands = 1;
          exec_commands = new String[formal_commands + diaas_cl.length];
          exec_commands[0] =
              RemoteBuildProperties.REMOTE_UTILS_DIR
                  + File.separator
                  + RemoteBuildProperties.DOCKER_IAAS_FILE;
          for (int i = 0; i < diaas_cl.length; i++) {
            exec_commands[i + formal_commands] = diaas_cl[i];
          }
        } else if (RS.equals(RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT)) {
          // inject remote build command
          String[] sshc_cl = pproperties.getCommandLineOptions(RS);
          int formal_commands = 3;
          exec_commands = new String[formal_commands + sshc_cl.length];
          exec_commands[0] = "java";
          exec_commands[1] = "-jar";
          exec_commands[2] =
              RemoteBuildProperties.REMOTE_UTILS_DIR
                  + File.separator
                  + RemoteBuildProperties.SSHCONNECT_FILE;
          for (int i = 0; i < sshc_cl.length; i++) {
            exec_commands[i + formal_commands] = sshc_cl[i];
          }
        } else {
          /*
           * This case should never happen. useServer() and
           * remote_build should only be set to TRUE in case either
           * connect.sh or SSHconnect are present.
           */
          Exception ex =
              new Exception(
                  "Unknown remote build service: "
                      + RS
                      + "\nIncosistent settings:\nProjectProperties.useServer() "
                      + pproperties.useServer()
                      + "\nRemoteBuildProperties.remote_build "
                      + pproperties.useRemoteBuild()
                      + "\nAppController.haveDIAAS() "
                      + pproperties.haveDockerIaaS()
                      + "\nAppController.haveSSHconnect() "
                      + pproperties.haveSSHconnect());
          throw ex;
        }
        // System.out.println("Executing command "+
        // Arrays.toString(exec_commands));
      }
    }
    Application.status.setMessageStatus(build_command);

    // execute make command
    int result = -1;
    try {
      if (useServer(pproperties))
        result =
            SwingUtils.processRun(
                exec_commands, new File(System.getProperty("user.dir")), this.outStream);
      else {
        if (debug) System.out.println("Running command locally: " + build_command);
        result =
            SwingUtils.processRun(build_command.split(" "), this.workdirectory, this.outStream);
      }
      if (result != 0) { // Check if the intermediate code generation fails to continue
        if (JOptionPane.showConfirmDialog(
                null,
                Message.getString("projectmakeservice.executemakecommand.continue.message"),
                Message.getString("projectmakeservice.executemakecommand.error"),
                JOptionPane.YES_NO_OPTION)
            != JOptionPane.YES_OPTION) {
          // Status message
          Application.status.setProgressStart(false);
          return false;
        }
      }
      Application.status.setProgressStart(false);
      return true;
    } catch (Exception ex) {
      ex.printStackTrace();
      throw ex;
    }
  }

  /**
   * @param pproperties
   * @return
   */
  private boolean useServer(ProjectProperties pproperties) {
    if (pproperties == null) return false;
    return pproperties.useRemoteBuild() && pproperties.useServer();
  }

  /**
   * Set the XML file search path list.
   *
   * @param list XML file search path list
   */
  public void setListSearchPath(List<File> list) {
    this.listSearchPath = list;
  }

  /**
   * Parse the XML file.
   *
   * @param updateFiles Update XML file list
   * @return Fortran database
   * @throws InterruptedException Interrupt exception
   */
  private Fortran parseSourceFile(SourceFile[] updateFiles) throws InterruptedException {

    // Fortran database
    Fortran fortranDb = new Fortran();
    if (updateFiles == null) return fortranDb;

    // Create XML parser
    XcodeMLParserStax fortranParser = new XcodeMLParserStax();
    ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
    for (SourceFile file : updateFiles) {
      try {
        String filename = file.toString();
        Application.status.setMessageStatus(filename);

        // Read the file from the source file
        fortranParser.readFile(file);

        // Parse the read code line.
        fortranParser.parseFile(fortranDb);

        // Get the original Fortran source file
        sourceFileList.add(fortranParser.getLanguageFile());

        // Get parse error
        if (fortranParser.getErrorInfos() != null) {
          this.addErrorInfos(fortranParser.getErrorInfos());
        }
      } catch (Exception lang_ex) {
        // Set the error location information
        this.addErrorInfo(lang_ex);
      }

      // Cancel check
      if (this.isCancel()) {
        return null;
      }
    }

    // Source file list settings
    fortranDb.setSourceFileList(sourceFileList);

    return fortranDb;
  }

  /**
   * Make a module copy.
   *
   * @param buildDb Build database
   * @param originalDb Original database
   * @param deletes Delete XML list
   * @return true = success
   */
  private boolean copyModules(Fortran buildDb, Fortran originalDb, SourceFile[] deletes) {
    if (buildDb == null) return false;
    if (originalDb == null) return false;
    Map<String, Module> buildModules = buildDb.getModules();
    Map<String, Module> currentModules = originalDb.getModules();
    if (currentModules == null || currentModules.size() <= 0) {
      return true;
    }
    // XML file list to be deleted
    List<SourceFile> listDelete = null;
    if (deletes != null && deletes.length > 0) {
      listDelete = Arrays.asList(deletes);
    }

    // Delete module list
    List<Module> deleteModules = new ArrayList<Module>();
    // Update module map <old module, new module>
    HashMap<Module, Module> mapUpdate = new HashMap<Module, Module>();

    // Copy the module from the current database to the build database.
    CURRENT_LOOP:
    for (String key : currentModules.keySet()) {
      Module module = currentModules.get(key);
      if (module == null) continue;
      // Get the module compilation XML file.
      SourceFile xml = getXmlFile(module, originalDb.getSourceFileList());
      // Is the module to be deleted?
      if (xml != null && listDelete != null && listDelete.contains(xml)) {
        deleteModules.add(module);
        continue;
      }
      // Is the module to be updated?
      if (xml != null) {
        for (String buildKey : buildModules.keySet()) {
          Module buildModule = buildModules.get(buildKey);
          if (buildModule == null) continue;
          // Get the module compilation XML file.
          SourceFile buildXml = getXmlFile(buildModule, buildDb.getSourceFileList());
          if (xml.equals(buildXml)) {
            // Update module
            mapUpdate.put(module, buildModule);
            continue CURRENT_LOOP;
          }
        }
      }
      // Does the same module name exist?
      Module updateModule = buildDb.module(module.get_name());
      if (updateModule != null) {
        if ("NO_MODULE".equalsIgnoreCase(updateModule.get_name())) {
          if (updateModule.isEmpty()) {
            updateModule = null;
          }
        }
      }
      if (updateModule != null) {
        // Update module
        mapUpdate.put(module, updateModule);
        continue;
      }
      // Module copy
      buildDb.addModule(module);
    }

    // Copy the main program name
    if (buildDb.getMainName() == null || buildDb.getMainName().isEmpty()) {
      buildDb.setMainName(originalDb.getMainName());
    }

    // Copy the source file list
    ArrayList<SourceFile> listOrgFile = originalDb.getSourceFileList();
    ArrayList<SourceFile> listBuildFile = buildDb.getSourceFileList();
    if (listOrgFile != null) {
      if (listBuildFile == null) {
        listBuildFile = new ArrayList<SourceFile>();
        buildDb.setSourceFileList(listBuildFile);
      }
      for (SourceFile file : listOrgFile) {
        SourceFile xml = file.getRelationFile();
        if (listDelete != null) {
          if (listDelete.contains(xml)) {
            continue;
          }
        }
        if (!listBuildFile.contains(file)) {
          listBuildFile.add(file);
        }
      }
    }

    // COMMON map
    Map<String, List<ProgramUnit>> mapCommon = originalDb.getCommonMap();
    if (mapCommon != null) {
      CURRENT_LOOP:
      for (String key : mapCommon.keySet()) {
        List<ProgramUnit> list = mapCommon.get(key);
        for (ProgramUnit unit : list) {
          // Are modules and subroutines included in the deleted module list?
          for (Module module : deleteModules) {
            if (module.containsChildren(unit)) {
              continue CURRENT_LOOP;
            }
          }
          // Are modules and subroutines included in the update module list?
          for (Module module : mapUpdate.keySet()) {
            if (module.containsChildren(unit)) {
              // Since it is included in the update module, it should have been added to the
              // commonMap by parsing.
              continue CURRENT_LOOP;
            }
          }
          // Not a delete, update module, subroutine, so it should exist in the module copy.
          boolean existsModule = false;
          for (String buildKey : buildModules.keySet()) {
            Module buildModule = buildModules.get(buildKey);
            if (buildModule == null) continue;
            if (buildModule.containsChildren(unit)) {
              existsModule = true;
              break;
            }
          }
          if (!existsModule) {
            // COMMON statement declaration module not found
            throw new LanguageException(
                "not found ProgramUnit of COMMON[name=" + key + "].", unit.getStartCodeLine());
          }

          // Add COMMON statement
          buildDb.addCommonMap(key, unit);
        }
      }
    }

    // Clear variable definitions and subroutine references
    // ClearDefinitions validate = new ClearDefinitions(buildDb);
    // validate.setListClearModule(mapUpdate);
    // LanguageVisitor visitor = new LanguageVisitor(validate);
    // visitor.entry();

    return true;
  }

  /**
   * Get the module compilation XML file.
   *
   * @param module module
   * @param list Source file list
   * @return XML file
   */
  private SourceFile getXmlFile(Module module, List<SourceFile> list) {
    if (module == null) return null;
    if (module.getStartCodeLine() == null) return null;
    SourceFile src = module.getStartCodeLine().getSourceFile();
    if (src == null) return null;
    SourceFile xml = src.getRelationFile();
    if (xml == null && list != null) {
      for (SourceFile file : list) {
        if (src.equals(file)) {
          xml = file.getRelationFile();
          break;
        }
      }
    }
    return xml;
  }

  /**
   * Get the ProgramUnit of additional information.
   *
   * @param info Additional copy information
   * @param destDb Copy destination module
   * @param srcDb Copy source module
   * @return Additional information ProgramUnit [programDest, programSrc]
   */
  @SuppressWarnings("unused")
  private ProgramUnit[] getInformationProgramUnits(
      IInformation info, Fortran destDb, Fortran srcDb) {
    if (info == null) return null;
    if (destDb == null) return null;
    if (srcDb == null) return null;

    Map<String, Module> srcModules = srcDb.getModules();
    if (srcModules == null || srcModules.size() <= 0) {
      return null;
    }
    ProgramUnit[] programInfos = null;
    for (String key : srcModules.keySet()) {
      Module srcModule = srcModules.get(key);
      if (srcModule == null) continue;
      Module destModule = destDb.module(key);
      if (destModule == null) continue;

      // (1) Is the module to be updated = [programDest, programSrc]
      programInfos = getInformationProgramUnits(info, destModule, srcModule);
      if (programInfos != null) {
        break;
      }
    }
    return programInfos;
  }

  /**
   * Get the ProgramUnit of additional information.
   *
   * @param info Additional copy information
   * @param destModule Copy destination module
   * @param srcModule Copy source module
   * @return Additional information ProgramUnit [programDest, programSrc]
   */
  private ProgramUnit[] getInformationProgramUnits(
      IInformation info, Module destModule, Module srcModule) {
    if (info == null) return null;
    if (destModule == null) return null;
    if (srcModule == null) return null;

    // Get the ProgramUnit of the namespace (module + subroutine)
    String namespace = info.getNamespace();
    if (namespace == null || namespace.isEmpty()) {
      return null;
    }
    ProgramUnit programDest = getProgramUnitByNamespace(destModule, namespace);
    ProgramUnit programSrc = getProgramUnitByNamespace(srcModule, namespace);

    if (programDest == null || programSrc == null) {
      return null;
    }
    return new ProgramUnit[] {programDest, programSrc};
  }

  /**
   * Get the ProgramUnit of the namespace (module + subroutine)
   *
   * @param destDb Search target database
   * @param namespace Search namespace
   * @return ProgramUnit
   */
  @SuppressWarnings("unused")
  private ProgramUnit getProgramUnitByNamespace(Fortran destDb, String namespace) {
    if (destDb == null) return null;
    Map<String, Module> modules = destDb.getModules();
    if (modules == null || modules.size() <= 0) {
      return null;
    }

    ProgramUnit unit = null;
    for (String key : modules.keySet()) {
      Module module = modules.get(key);
      unit = module.findProgramUnitBy(namespace);
      break;
    }
    return unit;
  }

  /**
   * Get the ProgramUnit of the namespace (module + subroutine)
   *
   * @param destModule Search target module
   * @param namespace Search namespace
   * @return ProgramUnit
   */
  private ProgramUnit getProgramUnitByNamespace(Module destModule, String namespace) {
    if (destModule == null) return null;
    ProgramUnit unit = destModule.findProgramUnitBy(namespace);
    return unit;
  }

  /**
   * Check if InformationBlocks are included in InformationBlocks.
   *
   * @param infos InformationBlocks (= InformationBlock list)
   * @param src Search Information Block
   * @return true = included
   */
  @SuppressWarnings("unused")
  private boolean containsInformationBlock(InformationBlocks infos, InformationBlock src) {
    if (infos == null) return false;
    if (src == null) return false;

    for (InformationBlock info : infos) {
      if (src.getStartBlock() != info.getStartBlock()) continue;
      if (src.getEndBlock() != info.getEndBlock()) continue;
      return true;
    }

    return false;
  }

  /**
   * Check if it is a DO, IF, SELECT statement.
   *
   * @param info Additional information block
   * @return DO, IF, SELECT statement
   */
  @SuppressWarnings("unused")
  private boolean isBlock(IInformation info) {
    if (!(info instanceof IBlock)) return false;
    if (((IBlock) info).getBlockType() == BlockType.SELECTION
        || ((IBlock) info).getBlockType() == BlockType.REPETITION
        || ((IBlock) info).getBlockType() == BlockType.CONDITION) {
      return true;
    }

    return false;
  }

  /**
   * Check if the block hierarchy is the same. Check if the parent block hierarchy is the same.
   * Check only the type of the parent block.
   *
   * @param destinfo Block 1
   * @param srcinfo Block 2
   * @return true = Same block structure
   */
  @SuppressWarnings("unused")
  private boolean equalsParentLayout(IInformation destinfo, IInformation srcinfo) {
    if (destinfo == null) return false;
    if (srcinfo == null) return false;
    if (!(destinfo instanceof IBlock)) return false;
    if (!(srcinfo instanceof IBlock)) return false;

    if (((IBlock) srcinfo).getBlockType() != ((IBlock) destinfo).getBlockType()) {
      return false;
    }

    IBlock srcparent = ((IBlock) srcinfo).getMotherBlock();
    IBlock destparent = ((IBlock) destinfo).getMotherBlock();

    // Check the block structure
    while (srcparent != null && destparent != null) {
      if (srcparent.getBlockType() != destparent.getBlockType()) {
        return false;
      }
      srcparent = srcparent.getMotherBlock();
      destparent = destparent.getMotherBlock();
      if (srcparent == null && destparent == null) {
        return true;
      } else if (srcparent == null || destparent == null) {
        return false;
      }
    }
    return true;
  }

  /**
   * Check if the block structure is the same. Check if the parent block structure is the same. The
   * syntax of the parent block is also the same.
   *
   * @param destinfo Block 1
   * @param srcinfo Block 2
   * @return true = Same block structure
   */
  @SuppressWarnings("unused")
  private boolean equalsParentBlock(IInformation destinfo, IInformation srcinfo) {
    if (destinfo == null) return false;
    if (srcinfo == null) return false;
    if (!(destinfo instanceof IBlock)) return false;
    if (!(srcinfo instanceof IBlock)) return false;

    if (!srcinfo.toString().equalsIgnoreCase(destinfo.toString())) {
      return false;
    }

    IBlock srcparent = ((IBlock) srcinfo).getMotherBlock();
    IBlock destparent = ((IBlock) destinfo).getMotherBlock();

    // Check the block structure
    while (srcparent != null && destparent != null) {
      if (!srcparent.toString().equalsIgnoreCase(destparent.toString())) {
        return false;
      }
      srcparent = srcparent.getMotherBlock();
      destparent = destparent.getMotherBlock();
      if (srcparent == null && destparent == null) {
        return true;
      } else if (srcparent == null || destparent == null) {
        return false;
      }
    }
    return true;
  }

  /**
   * Set the project model.
   *
   * @param model Project model
   */
  public void setProjectModel(ProjectModel model) {
    this.projectModel = model;
  }
}
