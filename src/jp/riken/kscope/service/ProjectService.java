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
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.SourceProperties;

/**
 * Service class that manages the project
 *
 * @author RIKEN
 */
public class ProjectService extends BaseService {

  /** Project model */
  private ProjectModel projectModel;
  /** Keyword Properties */
  private KeywordProperties propertiesKeyword;
  /** External tool properties */
  private ProgramProperties propertiesExtension;
  /** Arithmetic count property */
  private OperationProperties propertiesOperand;
  /** Source view settings */
  private SourceProperties propertiesSource;
  /** Profiler property settings */
  private ProfilerProperties propertiesProfiler;
  /** Project settings */
  private ProjectProperties propertiesProject;
  /** Request Byte / FLOP configuration property */
  private RequiredBFProperties propertiesMemory;

  private RemoteBuildProperties rb_properties;

  private Boolean debug = (System.getenv("DEBUG") != null);
  private Boolean debug_l2 = (debug && (System.getenv("DEBUG").equalsIgnoreCase("high")));
  private Boolean debug_l3 =
      (debug
          && (System.getenv("DEBUG").equalsIgnoreCase("high")
              || System.getenv("DEBUG").equalsIgnoreCase("extreme")));
  /** Constructor */
  public ProjectService() {
    setProjectModel(new ProjectModel());
  }

  /**
   * Constructor
   *
   * @param model Project model
   */
  public ProjectService(ProjectModel model) {
    this.setProjectModel(model);
  }

  /**
   * Create a new project
   *
   * @param title Project title
   * @param projectFolder project folder
   * @param list File list
   * @param type File type
   * @return Project information model
   */
  public ProjectModel createProject(
      String title, File projectFolder, List<File> list, FILE_TYPE type) {
    if (this.projectModel == null) {
      setProjectModel(new ProjectModel());
    }

    // Project title
    this.projectModel.setProjectTitle(title);
    // Project folder
    this.projectModel.setProjectFolder(projectFolder);
    // File type
    this.projectModel.setFileType(type);
    // Project selection file
    if (list != null) {
      SourceFile[] listFile = getSourceFiles(list.toArray(new File[0]), type, true);
      this.projectModel.setListXmlFile(listFile);
    }
    // Project selection folder / file
    this.projectModel.setListSearchPath(list);

    //        this.propertiesProject.setProjectFolder(projectFolder.toString());

    return projectModel;
  }

  /**
   * Add an XML folder to your project.
   *
   * @param listAddFile List of additional files
   * @return Success or failure
   */
  public boolean addProjectSelectedFile(List<File> listAddFile) {

    if (listAddFile == null || listAddFile.size() <= 0) return false;

    // Get additional XML file
    SourceFile[] listAdd =
        getSourceFiles(listAddFile.toArray(new File[0]), FILE_TYPE.XCODEML_XML, true);
    if (listAdd == null) return false;

    // Duplicate file check
    List<SourceFile> listXml = this.projectModel.getListSelectedFile();
    for (SourceFile addfile : listAdd) {
      if (!listXml.contains(addfile)) {
        listXml.add(addfile);
      }
    }

    return true;
  }

  /**
   * Add a Fortran folder to your project.
   *
   * @param listAddFile List of additional files
   * @return Success or failure
   */
  public boolean addProjectFortranFile(List<File> listAddFile) {

    if (listAddFile == null || listAddFile.size() <= 0) return false;

    // Get additional XML file
    SourceFile[] listAdd =
        getSourceFiles(listAddFile.toArray(new File[0]), FILE_TYPE.FORTRANLANG, true);
    if (listAdd == null) return false;

    // Duplicate file check
    List<SourceFile> listXml = this.projectModel.getListSelectedFile();
    for (SourceFile addfile : listAdd) {
      if (!listXml.contains(addfile)) {
        listXml.add(addfile);
      }
    }

    return true;
  }

  /**
   * Delete the XML file from the project.
   *
   * @param listDeleteFile Delete file list
   * @return Success or failure
   */
  public boolean deleteProjectSelectedFile(List<SourceFile> listDeleteFile) {

    // Registered XML file list
    List<SourceFile> listXml = this.projectModel.getListSelectedFile();

    // File check
    for (SourceFile delfile : listDeleteFile) {
      if (listXml.contains(delfile)) {
        listXml.remove(delfile);
      }
    }

    return true;
  }

  /**
   * Get a list of files from a subdirectory.
   *
   * @param dir Selected file list
   * @param ftype Selected language type
   * @param subDir Flag for whether to search subdirectories (true: subdirectory search / false: do
   *     not search)
   * @return Subdirectory file list
   */
  private File[] searchFiles(File dir, FILE_TYPE ftype, boolean subDir) {
    if (debug_l2) {
      System.out.println("Search files in " + dir);
      System.out.println("KscopeProperties.SETTINGS_FOLDER=" + KscopeProperties.SETTINGS_FOLDER);
    }
    if (dir == null) return null;
    // Do not add the settings.ppa folder.
    if (dir.isDirectory() && KscopeProperties.SETTINGS_FOLDER.equalsIgnoreCase(dir.getName())) {
      return null;
    }

    ArrayList<File> sublist = new ArrayList<File>();
    FileFilter filter = ftype.getFileFilter();
    if (debug_l2) {
      System.out.println("File filter: " + filter.toString());
    }
    // Get the list of files in the directory.
    File[] fileList = dir.listFiles();
    if (debug_l3) {
      System.out.println("File list:");
      for (File f : fileList) {
        System.out.println(f.toString());
      }
    }
    for (int i = 0; i < fileList.length; i++) {
      // Search subdirectories only if the subdirectory search flag is true.
      if (subDir && fileList[i].isDirectory()) {
        File[] files = searchFiles(fileList[i], ftype, subDir);
        if (files != null && files.length > 0) {
          sublist.addAll(java.util.Arrays.asList(files));
        }
      } else if (fileList[i].isFile()) {
        // If the file filter is null, add unconditionally
        if (filter == null) {
          sublist.add(fileList[i]);
        } else if (filter.accept(fileList[i])) {
          sublist.add(fileList[i]);
        } else {
          if (debug_l2) System.out.println("- " + fileList[i].toString() + "   filtered");
        }
      }
    }
    if (sublist.size() == 0) return null;

    return (File[]) sublist.toArray(new File[0]);
  }

  /**
   * Create a SourceFile object from the file and get the list.
   *
   * @param files Selected file list
   * @param ftype file type
   * @param subDir Subdirectory search flag
   * @return SourceFile list
   */
  public SourceFile[] getSourceFiles(File files[], FILE_TYPE ftype, boolean subDir) {

    if (debug_l2)
      System.out.println(
          "Call to ProjectService.getSourceFiles\n ftype=" + ftype + " subdir=" + subDir);
    ArrayList<File> filelist = new ArrayList<File>();
    ArrayList<SourceFile> sourcelist = new ArrayList<SourceFile>();
    for (int i = 0; i < files.length; i++) {
      File sourceFiles[] = null;
      if (files[i].isDirectory()) {
        sourceFiles = searchFiles(files[i], ftype, subDir);
      } else if (files[i].isFile()) {
        sourceFiles = new File[1];
        sourceFiles[0] = files[i];
      }

      if (sourceFiles == null) continue;
      for (int j = 0; j < sourceFiles.length; j++) {
        if (!filelist.contains(sourceFiles[j])) {
          filelist.add(sourceFiles[j]);
        }
      }
    }

    for (int j = 0; j < filelist.size(); j++) {
      // For automatic judgment, get the file type from the file extension.
      FILE_TYPE type = FILE_TYPE.getFileType(filelist.get(j));
      if (type != FILE_TYPE.UNKNOWN) {
        SourceFile src = new SourceFile(filelist.get(j), type);
        sourcelist.add(src);
      }
    }
    if (sourcelist.size() == 0) return null;

    return (SourceFile[]) sourcelist.toArray(new SourceFile[0]);
  }

  /**
   * Create a SourceFile object from the directory to get the list.
   *
   * @param dir Selected directory
   * @param ftype language type
   * @param subDir Subdirectory search flag
   * @return SourceFile list
   */
  public SourceFile[] getSourceFiles(File dir, FILE_TYPE ftype, boolean subDir) {
    File files[] = {dir};
    return getSourceFiles(files, ftype, subDir);
  }

  /**
   * Get the project model.
   *
   * @return projectModel Project model
   */
  public ProjectModel getProjectModel() {
    return projectModel;
  }

  /**
   * Set the project model
   *
   * @param projectModel Project model
   */
  public void setProjectModel(ProjectModel projectModel) {
    this.projectModel = projectModel;
  }

  /**
   * Save the project
   *
   * @param saveFolder project folder
   * @throws Exception Project save error
   */
  public void saveProject(File saveFolder) throws Exception {

    try {
      // Project XML file output
      writeProjectModel(saveFolder);

      // Property XML file output
      writeProperties(saveFolder);

    } catch (Exception ex) {
      // Error message output
      this.addErrorInfo(ex.getMessage());

      throw (ex);
    }
  }

  /**
   * Output project XML file
   *
   * @param saveFolder Save folder
   * @throws Exception Property output error
   */
  private void writeProjectModel(File saveFolder) throws Exception {

    // Document creation
    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder docbuilder = dbfactory.newDocumentBuilder();
    org.w3c.dom.Document document = docbuilder.newDocument();

    // project element: root element
    org.w3c.dom.Element root = document.createElement("project");
    document.appendChild(root);

    // Node output of project model information
    this.projectModel.writeProjectModel(root);

    // Output project file
    File saveFile =
        new File(saveFolder.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);

    // XML file output
    writeXmlFile(saveFile, document);
  }

  /**
   * Output property XML file
   *
   * @param saveFolder Save folder
   * @throws Exception Property output error
   */
  public void writeProperties(File saveFolder) throws Exception {

    // Document creation
    DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder docbuilder = dbfactory.newDocumentBuilder();
    org.w3c.dom.Document document = docbuilder.newDocument();

    // properties element: root element
    org.w3c.dom.Element root = document.createElement("properties");
    document.appendChild(root);

    // settings element
    org.w3c.dom.Element elemSettings = document.createElement("settings");
    root.appendChild(elemSettings);

    // Source view settings output
    this.propertiesSource.writeProperties(elemSettings);
    // Keyword property setting output
    this.propertiesKeyword.writeProperties(elemSettings);
    // External tool property setting output
    this.propertiesExtension.writeProperties(elemSettings);
    // Operation count property setting output
    this.propertiesOperand.writeProperties(elemSettings);
    // Profiler property setting output
    this.propertiesProfiler.writeProperties(elemSettings);
    // Project property setting output
    this.propertiesProject.writeProperties(elemSettings);
    // Request Byte / FLOP setting property setting output
    this.propertiesMemory.writeProperties(elemSettings);
    // this.rb_properties.writeProperties(elemSettings);

    // Create settings folder
    File settingsFolder =
        new File(saveFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
    if (!settingsFolder.exists()) {
      settingsFolder.mkdir();
    }

    // Output file
    File settingsXml =
        new File(
            settingsFolder.getAbsoluteFile() + File.separator + KscopeProperties.PROPERTIES_FILE);

    // XML file output
    writeXmlFile(settingsXml, document);
  }

  /**
   * Output XML document to a file
   *
   * @param output Output file
   * @param document XML document
   * @throws Exception XML output error
   */
  private void writeXmlFile(File output, org.w3c.dom.Document document) throws Exception {

    // DOM output
    TransformerFactory transFactory = TransformerFactory.newInstance();
    transFactory.setAttribute("indent-number", 4);
    Transformer transformer = transFactory.newTransformer();

    transformer.setOutputProperty(OutputKeys.INDENT, "yes");
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
    transformer.setOutputProperty(OutputKeys.METHOD, "xml");

    DOMSource source = new DOMSource(document);

    FileOutputStream os = new FileOutputStream(output);
    StreamResult result = new StreamResult(new OutputStreamWriter(os, "utf-8"));
    transformer.transform(source, result);
  }

  /**
   * Set keyword properties
   *
   * @param properties Keyword keyword property
   */
  public void setPropertiesKeyword(KeywordProperties propertiesKeyword) {
    this.propertiesKeyword = propertiesKeyword;
  }

  /**
   * Set external tool properties
   *
   * @param propertiesExtension External tool properties
   */
  public void setPropertiesExtension(ProgramProperties propertiesExtension) {
    this.propertiesExtension = propertiesExtension;
  }

  /**
   * Set the operation count property
   *
   * @param propertiesOperand Arithmetic count property
   */
  public void setPropertiesOperand(OperationProperties propertiesOperand) {
    this.propertiesOperand = propertiesOperand;
  }

  /**
   * Set source view settings
   *
   * @param propertiesSource Source view settings
   */
  public void setPropertiesSource(SourceProperties propertiesSource) {
    this.propertiesSource = propertiesSource;
  }

  /**
   * Set profiler property settings
   *
   * @param propertiesProfiler Profiler property settings
   */
  public void setPropertiesProfiler(ProfilerProperties propertiesProfiler) {
    this.propertiesProfiler = propertiesProfiler;
  }

  /**
   * Set project settings
   *
   * @param propertiesProject Project settings
   */
  public void setPropertiesProject(ProjectProperties propertiesProject) {
    this.propertiesProject = propertiesProject;
  }

  /**
   * Request Byte / FLOP setting property Set property setting
   *
   * @param propertiesMemory Request Byte / FLOP configuration properties
   */
  public void setPropertiesMemory(RequiredBFProperties propertiesMemory) {
    this.propertiesMemory = propertiesMemory;
  }

  public void setRBproperties(RemoteBuildProperties rb_properties) {
    this.rb_properties = rb_properties;
  }

  /**
   * Open the project
   *
   * @param openFolder project folder
   * @throws Exception Project open error
   */
  public void openProject(File openFolder) throws Exception {
    try {
      // Project configuration file
      File projectFile =
          new File(openFolder.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);
      // Load the project configuration file
      this.projectModel.loadProjectModel(projectFile);

      // Load project properties
      // settings folder
      File settingsFolder =
          new File(
              openFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);

      // Output file
      File settingsXml =
          new File(
              settingsFolder.getAbsoluteFile() + File.separator + KscopeProperties.PROPERTIES_FILE);

      // Source view settings output
      this.propertiesSource.loadProperties(settingsXml);
      // Keyword property setting output
      this.propertiesKeyword.loadProperties(settingsXml);
      // External tool property setting output
      this.propertiesExtension.loadProperties(settingsXml);
      // Operation count property setting output
      this.propertiesOperand.loadProperties(settingsXml);
      // Profiler property setting output
      this.propertiesProfiler.loadProperties(settingsXml);
      // Project setting output
      this.propertiesProject.loadProperties(settingsXml);
      // Request Byte / FLOP configuration property
      this.propertiesMemory.loadProperties(settingsXml);
      this.rb_properties.loadProperties(settingsXml);

    } catch (Exception ex) {
      // Error message output
      this.addErrorInfo(ex.getMessage());

      throw (ex);
    }
  }

  /**
   * Set project properties
   *
   * @param model Property model
   */
  public void setProperties(PropertiesTableModel model) {

    String[] items = {
      Message.getString("projectservice.properties.name"), // Project name
      Message.getString("projectservice.properties.folder"), // project folder
      Message.getString("projectservice.properties.createdate"), // Creation date and time
      Message.getString("projectservice.properties.updatedate") // Update date and time
    };
    String[] values = new String[4];
    if (this.projectModel != null) {
      // Project name
      values[0] = this.projectModel.getProjectTitle();
      if (this.projectModel.getProjectFolder() != null) {
        // Project folder
        values[1] = this.projectModel.getProjectFolder().getAbsolutePath();
      }
      // Creation date and time
      values[2] = this.projectModel.getCreateDate();
      // Update date and time
      values[3] = this.projectModel.getUpdateDate();
    }

    // Set in property model
    // Notification to the property panel is notified by Observer.
    model.setTitle(Message.getString("projectservice.properties.title")); // Project properties
    model.setProperties(items, values);
  }

  /**
   * Check if property setting is completed
   *
   * @return true = Property setting completed
   */
  public boolean existAllProperties() {
    if (this.propertiesExtension == null) return false;
    if (this.propertiesKeyword == null) return false;
    if (this.propertiesMemory == null) return false;
    if (this.propertiesOperand == null) return false;
    if (this.propertiesProfiler == null) return false;
    if (this.propertiesProject == null) return false;
    if (this.propertiesSource == null) return false;
    return true;
  }
}
