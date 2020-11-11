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

package jp.riken.kscope.model;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * Project information class
 *
 * @author RIKEN
 */
public class ProjectModel {

  /** Project title */
  private String projectTitle;

  /** Project folder */
  private File projectFolder;
  /** Project file type */
  private FILE_TYPE fileType;
  /** Project file list */
  private List<SourceFile> listXmlFile;
  /** date of creation */
  private String createDate;
  /** Update date */
  private String updateDate;
  /** Project: Intermediate code selection folder, file list */
  private List<File> listSearchPath;

  /** Constructor */
  public ProjectModel() {
    listXmlFile = new ArrayList<SourceFile>();
    listSearchPath = new ArrayList<File>();
  }

  /**
   * Get the project title.
   *
   * @return projectTitle Project title
   */
  public String getProjectTitle() {
    return projectTitle;
  }

  /**
   * Set the project title
   *
   * @param projectTitle Project title
   */
  public void setProjectTitle(String projectTitle) {
    this.projectTitle = projectTitle;
  }

  /**
   * Get the project folder
   *
   * @return projectFolder project folder
   */
  public File getProjectFolder() {
    return projectFolder;
  }

  /**
   * Set the project folder.
   *
   * @param projectFolder project folder
   */
  public void setProjectFolder(File projectFolder) {
    this.projectFolder = projectFolder;
  }

  /**
   * Get the project XML file list
   *
   * @return listXmlFile Project XML file list
   */
  public List<SourceFile> getListSelectedFile() {
    return listXmlFile;
  }

  /**
   * Set the project XML file list
   *
   * @param listXmlFile Project XML file list
   */
  public void setListXmlFile(List<SourceFile> listXmlFile) {
    this.listXmlFile = listXmlFile;
  }

  /**
   * Set the project XML file list
   *
   * @param listXmlFile Project XML file list
   */
  public void setListXmlFile(SourceFile[] listXmlFile) {
    if (listXmlFile == null || listXmlFile.length <= 0) return;

    this.listXmlFile.clear();
    for (int i = 0; i < listXmlFile.length; i++) {
      this.listXmlFile.add(listXmlFile[i]);
    }
  }

  /**
   * Add project XML file
   *
   * @param xmlFile Project XML file
   */
  public void addProjectSelectedFile(SourceFile xmlFile) {
    if (this.listXmlFile == null || this.listXmlFile.size() <= 0) {
      this.listXmlFile = new ArrayList<SourceFile>();
    }
    this.listXmlFile.add(xmlFile);
  }

  /**
   * Get the creation date.
   *
   * @return Creation date
   */
  public String getCreateDate() {
    return createDate;
  }

  /**
   * Set the creation date
   *
   * @param createDate Creation date
   */
  public void setCreateDate(String createDate) {
    this.createDate = createDate;
  }

  /**
   * Get the update date.
   *
   * @return Update date
   */
  public String getUpdateDate() {
    return updateDate;
  }

  /**
   * Set the update date
   *
   * @param updateDate Update date
   */
  public void setUpdateDate(String updateDate) {
    this.updateDate = updateDate;
  }

  /** Clear the project model. */
  public void clearProjectModel() {

    this.createDate = null;
    this.updateDate = null;
    this.projectFolder = null;
    this.projectTitle = null;

    if (this.listXmlFile == null) {
      this.listXmlFile = new ArrayList<SourceFile>();
    }
    this.listXmlFile.clear();
  }

  /**
   * Output the project XML file.
   *
   * @param node Output node
   */
  public void writeProjectModel(org.w3c.dom.Node node) {

    // Get documentation
    org.w3c.dom.Document document = node.getOwnerDocument();

    // date of creation
    {
      if (this.createDate == null || this.createDate.isEmpty()) {
        // date of creation
        SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
        this.createDate = format.format(new Date());
      }
      org.w3c.dom.Element elem = document.createElement("createdate");
      node.appendChild(elem);
      elem.appendChild(document.createTextNode(this.createDate));
    }
    // Updated date: Current date
    {
      // Update date
      SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
      this.updateDate = format.format(new Date());

      org.w3c.dom.Element elem = document.createElement("updatedate");
      node.appendChild(elem);
      elem.appendChild(document.createTextNode(this.updateDate));
    }

    // Project title
    {
      org.w3c.dom.Element elem = document.createElement("title");
      node.appendChild(elem);
      elem.appendChild(document.createTextNode(this.getProjectTitle()));
    }
    // Selected file type
    {
      String typeAttr = null;
      if (this.fileType == FILE_TYPE.XCODEML_XML) {
        typeAttr = "xcodeml";
      } else if (this.fileType == FILE_TYPE.FORTRANLANG) {
        typeAttr = "fortran";
      }
      org.w3c.dom.Element elem = document.createElement("filetype");
      node.appendChild(elem);
      org.w3c.dom.Attr attribute = document.createAttribute("type");
      attribute.setValue(typeAttr);
      elem.setAttributeNode(attribute);
    }

    // Read XML file output
    if (this.getListSelectedFile() != null && this.getListSelectedFile().size() > 0) {

      List<SourceFile> xmlfiles = new ArrayList<SourceFile>();
      List<SourceFile> srcfiles = new ArrayList<SourceFile>();
      for (SourceFile file : this.getListSelectedFile()) {
        if (FILE_TYPE.isFortranFile(file.getFile())) {
          srcfiles.add(file);
        } else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
          xmlfiles.add(file);
        }
      }

      if (xmlfiles.size() > 0) {
        // xcodeml element
        org.w3c.dom.Element elem = document.createElement("xcodeml");
        String typeAttr = "f_front";
        String fileAttr = "xml";
        node.appendChild(elem);
        org.w3c.dom.Attr attribute = document.createAttribute("type");
        attribute.setValue(typeAttr);
        elem.setAttributeNode(attribute);

        for (SourceFile xml : xmlfiles) {
          File file = xml.getFile();
          String path = FileUtils.getRelativePath(file, this.getProjectFolder());
          path = StringUtils.escapeFilePath(path);
          if (path == null) continue;

          // file element
          org.w3c.dom.Element elemFile = document.createElement("file");
          org.w3c.dom.Attr attrFile = document.createAttribute("type");
          attrFile.setValue(fileAttr);
          elemFile.setAttributeNode(attrFile);
          // XML file name
          elemFile.appendChild(document.createTextNode(path));

          elem.appendChild(elemFile);
        }
      }

      if (srcfiles.size() > 0) {
        // source element
        org.w3c.dom.Element elem = document.createElement("source");
        String typeAttr = "fortran";
        String fileAttr = "fortran";
        node.appendChild(elem);
        org.w3c.dom.Attr attribute = document.createAttribute("type");
        attribute.setValue(typeAttr);
        elem.setAttributeNode(attribute);

        for (SourceFile xml : this.getListSelectedFile()) {
          File file = xml.getFile();
          String path = FileUtils.getRelativePath(file, this.getProjectFolder());
          path = StringUtils.escapeFilePath(path);
          if (path == null) continue;
          // file element
          org.w3c.dom.Element elemFile = document.createElement("file");
          org.w3c.dom.Attr attrFile = document.createAttribute("type");
          attrFile.setValue(fileAttr);
          elemFile.setAttributeNode(attrFile);
          // XML file name
          elemFile.appendChild(document.createTextNode(path));

          elem.appendChild(elemFile);
        }
      }
    }

    // Select folder / file output
    if (this.getListSearchPath() != null && this.getListSearchPath().size() > 0) {
      List<File> list = this.getListSearchPath();

      // searchpath element
      org.w3c.dom.Element elem = document.createElement("searchpath");
      node.appendChild(elem);

      for (File file : list) {
        String path = FileUtils.getRelativePath(file, this.getProjectFolder());
        path = StringUtils.escapeFilePath(path);
        if (path == null) continue;
        // path element
        org.w3c.dom.Element elemFile = document.createElement("path");
        // Selected folder / file name
        elemFile.appendChild(document.createTextNode(path));

        elem.appendChild(elemFile);
      }
    }
  }

  /**
   * Read the project setting file.
   *
   * @param loadFile Load configuration file
   * @throws Exception Read error
   */
  public void loadProjectModel(File loadFile) throws Exception {

    if (!loadFile.exists()) {
      throw (new Exception(
          "Project Configuration is not exist.")); // The project configuration file does not exist.
    }

    // Read resource file
    XmlUtils xml = new XmlUtils(loadFile);

    // date of creation
    this.createDate = xml.getString("/project/createdate");
    // Update date
    this.updateDate = xml.getString("/project/updatedate");
    // Title
    this.projectTitle = xml.getString("/project/title");
    // Project folder
    this.projectFolder = loadFile.getParentFile();
    // Selected file list
    String type = xml.getString("/project/filetype/@type");
    if ("xcodeml".equalsIgnoreCase(type)) {
      this.fileType = FILE_TYPE.XCODEML_XML;
    } else if ("fortran".equalsIgnoreCase(type)) {
      // File type
      this.fileType = FILE_TYPE.FORTRANLANG;
    } else {
      this.fileType = FILE_TYPE.XCODEML_XML;
    }
    // XML file list
    List<String> list = xml.getList("/project/xcodeml/file");
    if (list != null && list.size() > 0) {

      for (String value : list) {
        if (value == null) continue;

        File file = new File(value);
        if (!file.isAbsolute()) {
          // Since it is a relative path, add a project folder
          file = new File(this.projectFolder.getAbsoluteFile() + File.separator + value);
        }
        if (file.exists()) {
          SourceFile source = new SourceFile(file);
          addProjectSelectedFile(source);
        }
      }
    }
    // Source file list
    List<String> sourcelist = xml.getList("/project/source/file");
    if (sourcelist != null && sourcelist.size() > 0) {
      for (String value : sourcelist) {
        if (value == null) continue;

        File file = new File(value);
        if (!file.isAbsolute()) {
          // Since it is a relative path, add a project folder
          file = new File(this.projectFolder.getAbsoluteFile() + File.separator + value);
        }
        if (file.exists()) {
          SourceFile source = new SourceFile(file);
          this.addProjectSelectedFile(source);
        }
      }
    }

    // Selected folder / file list
    List<String> listSelect = xml.getList("/project/searchpath/path");
    if (listSelect != null && listSelect.size() > 0) {

      for (String value : listSelect) {
        if (value == null) continue;

        File file = new File(value);
        if (!file.isAbsolute()) {
          // Since it is a relative path, add a project folder
          file = new File(this.projectFolder.getAbsoluteFile() + File.separator + value);
        }
        addSearchPath(file);
      }
    }
  }

  /**
   * Add to the intermediate code selection folder / file list of the project. Holds the folder /
   * file when the intermediate code for creating a new project is selected.
   *
   * @param file Intermediate code selection folder file
   */
  private void addSearchPath(File path) {
    if (this.listSearchPath.contains(path)) {
      return;
    }
    this.listSearchPath.add(path);
  }

  /**
   * Get the intermediate code selection folder / file list of the project. Holds the folder / file
   * when the intermediate code for creating a new project is selected.
   *
   * @return Intermediate code selection folder / file list
   */
  public List<File> getListSearchPath() {
    return this.listSearchPath;
  }

  /**
   * Get the intermediate code selection folder / file list of the project. Holds the folder / file
   * when the intermediate code for creating a new project is selected.
   *
   * @return Intermediate code selection folder / file list
   */
  public void setListSearchPath(List<File> list) {
    this.listSearchPath.clear();
    this.listSearchPath.addAll(list);
  }

  /**
   * Set the project file type
   *
   * @param type Project file type: FILE_TYPE.XCODEML_XML or FILE_TYPE.FORTRANLANG
   */
  public void setFileType(FILE_TYPE type) {
    this.fileType = type;
  }

  /**
   * Get the project file type
   *
   * @return project file type
   */
  public FILE_TYPE getFileType() {
    return this.fileType;
  }

  /**
   * Check if the project settings are valid. Check if the project folder is set.
   *
   * @return true = Project settings are valid
   */
  public boolean isVaildProject() {
    return (this.projectFolder != null);
  }
}
