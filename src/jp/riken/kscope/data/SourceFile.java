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

import java.io.File;
// import java.io.IOException;
import java.io.Serializable;
import java.util.Date;
import jp.riken.kscope.utils.FileUtils;

/**
 * Source file class Has a source file name and file type.
 *
 * @author RIKEN
 */
public class SourceFile implements Serializable {

  /** Serial number */
  private static final long serialVersionUID = -1930551091523251203L;
  /** source file */
  private File m_file;
  /** Language type */
  private FILE_TYPE m_fileType;
  /** Update date */
  private Date modifyDate;
  /** Related files: XML files for source files, source files for XML files */
  private SourceFile relationFile = null;

  /**
   * Constructor
   *
   * @param file Source file
   * @param fileType Language type
   */
  public SourceFile(File file, FILE_TYPE fileType) {
    this.m_file = file;
    this.m_fileType = fileType;
    try {
      setModifyDate(new Date(m_file.lastModified()));
    } catch (Exception e) {
    }
  }

  /**
   * Constructor
   *
   * @param filename File name
   */
  public SourceFile(String filename) {
    this.m_file = new File(filename);
    this.m_fileType = FILE_TYPE.FILE_AUTO;
    try {
      setModifyDate(new Date(m_file.lastModified()));
    } catch (Exception e) {
    }
  }

  /**
   * Constructor
   *
   * @param file Source file
   */
  public SourceFile(File file) {
    this.m_file = file;
    this.m_fileType = FILE_TYPE.FILE_AUTO;
    try {
      setModifyDate(new Date(m_file.lastModified()));
    } catch (Exception e) {
    }
  }

  /**
   * Copy constructor
   *
   * @param source Copy source source file
   */
  public SourceFile(SourceFile source) {
    if (source.m_file != null) {
      m_file = new File(source.m_file.getPath());
    }
    m_fileType = source.m_fileType;
    setModifyDate(source.modifyDate);
  }

  /**
   * Get the source file.
   *
   * @return source file
   */
  public File getFile() {
    return m_file;
  }

  /**
   * File type
   *
   * @return File type
   */
  public FILE_TYPE getFileType() {
    return m_fileType;
  }

  /**
   * File type
   *
   * @param type File type
   */
  public void setFileType(FILE_TYPE type) {
    m_fileType = type;
  }

  /** Returns only the filename of the source file. */
  @Override
  public String toString() {
    if (m_file == null) return "";
    return m_file.getName();
  }

  /**
   * Returns the pathname of the source file.
   *
   * @return source file pathname
   */
  public String getPath() {
    if (m_file == null) return null;
    return m_file.getPath();
  }

  /**
   * Check if the source files are equal. If m_files are equal, they are equal.
   *
   * @param obj source file
   * @return true: equal / false: not equal
   */
  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof SourceFile)) return false;
    SourceFile src = (SourceFile) obj;
    if (src == null || src.m_file == null) return false;
    if (this.m_file == null) return false;

    try {
      if (this.m_file.isAbsolute() && src.m_file.isAbsolute()) {
        return FileUtils.isEqualsFile(m_file, src.m_file);
      } else if (!this.m_file.isAbsolute() && !src.m_file.isAbsolute()) {
        return FileUtils.isEqualsFile(m_file, src.m_file);
      } else if (this.m_file.isAbsolute() && !src.m_file.isAbsolute()) {
        String thispath = trimPath(this.m_file.getPath());
        String srcpath = trimPath(src.m_file.getPath());
        return thispath.endsWith(srcpath);
      } else if (!this.m_file.isAbsolute() && src.m_file.isAbsolute()) {
        String thispath = trimPath(this.m_file.getPath());
        // String srcpath = src.m_file.getCanonicalPath();
        String srcpath = trimPath(src.m_file.getPath());
        return srcpath.endsWith(thispath);
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }
    return false;
  }

  /**
   * Remove ./ from the beginning of the path string
   *
   * @param path path string
   * @return'./' Delete path string
   */
  private String trimPath(String path) {
    if (path == null) return null;
    if (path.startsWith("./")) {
      return path.substring(2);
    }
    if (path.startsWith("." + File.separator)) {
      return path.substring(2);
    }
    return path;
  }

  /**
   * Output source file information
   *
   * @return Source file information
   */
  public String toStringInfo() {
    if (m_file != null) return m_file.getName() + " [" + m_fileType.toString() + "]";
    else return null;
  }

  /**
   * Set the source file
   *
   * @param file Source file
   */
  public void setFile(File file) {
    this.m_file = file;
    this.m_fileType = FILE_TYPE.FILE_AUTO;
    try {
      setModifyDate(new Date(m_file.lastModified()));
    } catch (Exception e) {
    }
  }

  /**
   * Returns the hash code of the file path string.
   *
   * @return hash code
   */
  @Override
  public int hashCode() {
    if (this.m_file == null) return 0;
    int hash = this.m_file.getPath().hashCode();
    return hash;
  }

  /**
   * Get update date
   *
   * @return Update date
   */
  public Date getModifyDate() {
    return modifyDate;
  }

  /**
   * Set the update date.
   *
   * @param modifyDate Modify date
   */
  public void setModifyDate(Date modifyDate) {
    this.modifyDate = modifyDate;
  }

  /**
   * Set the update date.
   *
   * @param file Update file
   */
  public void setModifyDate(File file) {
    try {
      setModifyDate(new Date(file.lastModified()));
    } catch (Exception e) {
    }
  }

  /**
   * Get related files. Get the XML file for the source file, and the source file for the XML file.
   *
   * @return Related files
   */
  public SourceFile getRelationFile() {
    return this.relationFile;
  }

  /**
   * Set related files. Set the XML file for the source file and the source file for the XML file.
   *
   * @param file Related files
   */
  public void setRelationFile(SourceFile file) {
    this.relationFile = file;
  }

  /**
   * Check if the update date is valid.
   *
   * @return true = Update date is valid
   */
  public boolean validateModifyDate() {
    if (this.modifyDate == null) return false;
    long time = this.modifyDate.getTime();
    return (time != 0);
  }

  /** Update the update date. */
  public void updateModifyDate() {
    if (m_file == null) return;
    try {
      setModifyDate(new Date(m_file.lastModified()));
    } catch (Exception e) {
    }
  }
}
