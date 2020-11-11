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

import java.io.Serializable;
import jp.riken.kscope.utils.StringUtils;

/**
 * Source code class
 *
 * @author RIKEN
 */
public class CodeLine implements Comparable<CodeLine>, Serializable {
  /** Serial number */
  private static final long serialVersionUID = 4602122606378044455L;

  /** COMMENT: Annotation / Annotation statement STATEMENT: Code statement UNKNOWN: Unknown */
  public enum CODE_TYPE {
    /** Comment text */
    COMMENT,
    /** Status statement */
    STATEMENT,
    /** unknown */
    UNKNOWN
  };
  /** source file */
  private SourceFile m_sourceFile;
  /** Source code statement */
  private String m_statement;
  /** File line number (start): If <= 0, no line number is set */
  private int m_startline;
  /** File line number (end): If <= 0, no line number is set */
  private int m_endline;
  /** Code statement type */
  private CODE_TYPE m_type;
  /** Source file (stored from XML information even if it does not exist) */
  private String m_strSourceFile;

  /**
   * Constructor
   *
   * @param sourceFile source file
   * @param statement Line of code
   * @param startline File line number (start)
   * @param endline File line number (end)
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(
      SourceFile sourceFile, String statement, int startline, int endline, String strSourceFile) {
    m_sourceFile = sourceFile;
    m_statement = statement;
    m_startline = startline;
    m_endline = endline;
    m_type = CODE_TYPE.STATEMENT;
    m_strSourceFile = strSourceFile;
  }

  /**
   * Constructor
   *
   * @param sourceFile source file
   * @param statement Line of code
   * @param line File line number
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(SourceFile sourceFile, String statement, int line, String strSouceFile) {
    this(sourceFile, statement, line, line, strSouceFile);
  }

  /**
   * Constructor
   *
   * @param sourceFile Source file
   * @param statement Line of code
   * @param line File line number
   * @param type Code type
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(
      SourceFile sourceFile, String statement, int line, CODE_TYPE type, String strSourceFile) {
    this(sourceFile, statement, line, strSourceFile);
    m_type = type;
  }

  /**
   * Constructor
   *
   * @param statement Line of code
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(String statement) {
    this(null, statement, -1, null);
  }

  /**
   * Constructor
   *
   * @param statement Line of code
   * @param line File line number
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(String statement, int line) {
    this(null, statement, line, null);
  }

  /**
   * Constructor
   *
   * @param sourceFile Source file
   * @param startline File line number (start)
   * @param endline File line number (end)
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(SourceFile sourceFile, int startline, int endline, String strSourceFile) {
    m_sourceFile = sourceFile;
    m_statement = null;
    m_startline = startline;
    m_endline = endline;
    m_type = CODE_TYPE.UNKNOWN;
    m_strSourceFile = strSourceFile;
  }

  /**
   * Constructor
   *
   * @param sourceFile Source file
   * @param strSourceFile Source file (stored from XML information even if it does not exist)
   */
  public CodeLine(SourceFile sourceFile, String strSourceFile) {
    m_sourceFile = sourceFile;
    m_statement = null;
    m_startline = 0;
    m_endline = 0;
    m_type = CODE_TYPE.UNKNOWN;
    m_strSourceFile = strSourceFile;
  }

  /**
   * Copy constructor
   *
   * @param code Source code
   */
  public CodeLine(CodeLine code) {
    m_sourceFile = new SourceFile(code.m_sourceFile);
    m_statement = code.m_statement;
    m_startline = code.m_startline;
    m_endline = code.m_endline;
    m_type = code.m_type;
    m_strSourceFile = code.m_strSourceFile;
  }

  /**
   * Copy constructor
   *
   * @param start Start code line
   * @param end Exit code line
   */
  public CodeLine(CodeLine start, CodeLine end) {
    this(start);
    if (end != null) {
      if (m_startline > end.getStartLine()) {
        m_startline = end.getStartLine();
      }
      if (m_endline < end.getEndLine()) {
        this.m_endline = end.getEndLine();
      }
    }
  }

  /**
   * Get the source file.
   *
   * @return source file
   */
  public SourceFile getSourceFile() {
    return m_sourceFile;
  }

  /**
   * Set the source file.
   *
   * @param sourceFile source file
   */
  public void setSourceFile(SourceFile sourceFile) {
    m_sourceFile = sourceFile;
  }

  /**
   * Get the source code line.
   *
   * @return Source code line
   */
  public String getStatement() {
    return m_statement;
  }

  /**
   * Set the source code line.
   *
   * @param statement File line number
   */
  public void setStatement(String statement) {
    m_statement = statement;
  }

  /**
   * Get the file line number (start).
   *
   * @return File line number (start)
   */
  public int getStartLine() {
    return m_startline;
  }

  /**
   * Set the file line number (start).
   *
   * @param line File line number (start)
   */
  public void setLine(int line) {
    m_startline = line;
  }

  /**
   * Get the file line number (end).
   *
   * @return File line number (end)
   */
  public int getEndLine() {
    return m_endline;
  }

  /**
   * Set the file line number (end).
   *
   * @param line File line number (end)
   */
  public void setEndLine(int line) {
    m_endline = line;
  }

  /** Annotation Set the code type that is the annotation text. */
  public void setCommentType() {
    m_type = CODE_TYPE.COMMENT;
  }

  /**
   * Check if it is an annotation / annotation text type.
   *
   * @return true: Annotation / annotation type / false: Not an annotation / annotation type.
   */
  public boolean isCommentType() {
    return (m_type == CODE_TYPE.COMMENT);
  }

  /**
   * Check if it is a code statement type.
   *
   * @return true: Code statement type / false: Not a code statement type.
   */
  public boolean isStatementType() {
    return (m_type == CODE_TYPE.STATEMENT);
  }

  /**
   * Add to line of code.
   *
   * @param code Additional code
   * @param lineno Additional code line number
   */
  public void appendLine(String code, int lineno) {
    if (code == null) return;
    if (code.trim().length() == 0) return;

    m_statement += code;
    m_endline = lineno;
  }

  /**
   * Sort lines of code. Sort in file start line order. If the comment and code are on the same
   * line, the code takes precedence.
   *
   * @param code Code object
   * @return -1: Small line / 0: Same line / 1: Large line
   */
  @Override
  public int compareTo(CodeLine code) {
    if (this.m_startline < code.m_startline) {
      return -1;
    } else if (this.m_startline > code.m_startline) {
      return 1;
    }

    if (this.m_type == CODE_TYPE.STATEMENT && code.m_type == CODE_TYPE.COMMENT) {
      return -1;
    }
    if (this.m_type == CODE_TYPE.COMMENT && code.m_type == CODE_TYPE.STATEMENT) {
      return 1;
    }

    return 0;
  }

  /**
   * Returns the code line information as a string.
   *
   * @return Line of code information
   */
  @Override
  public String toString() {

    String text = this.getStatement();
    String prefix = String.valueOf(this.getStartLine());
    text = prefix + " : " + text;

    return text;
  }

  /**
   * Returns the detailed information of the line of code as a string.
   *
   * @return Line of code information
   */
  public String toDetailString() {
    StringBuilder buf = new StringBuilder();

    if (m_startline > 0) {
      buf.append("[code ");
      buf.append(m_startline);
      if (m_startline < m_endline) {
        buf.append(":");
        buf.append(m_endline);
        buf.append("] ");
      }
      buf.append("] ");
    } else {
      buf.append("[code] ");
    }
    // Source code statement
    buf.append(m_statement);

    // source file
    if (m_sourceFile != null) {
      buf.append("\n");
      buf.append("[file] ");
      buf.append(m_sourceFile.toString());
    } else {
      buf.append("\n");
      buf.append("[file(not exist)] ");
      buf.append(m_strSourceFile);
    }

    return buf.toString();
  }

  /**
   * Returns the code line information as a string.
   *
   * @return Line of code information
   */
  public String getLineInfo() {
    StringBuilder buf = new StringBuilder();

    buf.append("[");
    // source file
    if (m_sourceFile != null) {
      String file = m_sourceFile.toString();
      file = String.format("%-16s", file);
      buf.append(file);
      buf.append(":");
    } else {
      String file = m_strSourceFile;
      file = String.format("%-16s", file);
      file += "(not exist)";
      buf.append(file);
      buf.append(":");
    }

    if (m_startline >= 0) {
      String lineno = String.format("%06d", m_startline);
      buf.append(lineno);
    }
    buf.append("] ");

    // Source code statement
    buf.append(m_statement);

    return buf.toString();
  }

  /**
   * Returns the start and end line number information as a character string.
   *
   * @return Start and end line numbers
   */
  public String getLineno() {
    StringBuilder buf = new StringBuilder();

    if (m_startline > 0) {
      buf.append(m_startline);
    }
    if (m_endline > 0) {
      buf.append(":");
      buf.append(m_endline);
    }

    return buf.toString();
  }

  /**
   * Check if it is the same line of code
   *
   * @param code Checked code line
   * @return true = match
   */
  @Override
  public boolean equals(Object code) {
    if (!(code instanceof CodeLine)) return false;
    if (this.m_sourceFile != null) {
      if (!this.m_sourceFile.equals(((CodeLine) code).m_sourceFile)) {
        return false;
      }
    }
    if (this.m_startline != ((CodeLine) code).m_startline) {
      return false;
    }
    if (this.m_endline != ((CodeLine) code).m_endline) {
      return false;
    }
    if (this.m_statement != null) {
      if (!this.m_statement.equalsIgnoreCase(((CodeLine) code).m_statement)) {
        return false;
      }
    }

    return true;
  }

  /**
   * Get the source file name
   *
   * @return Source file name (return if there is a setting even if the file does not exist)
   */
  public String getStrSourceFile() {
    if (m_statement != null) {
      if (m_statement.startsWith("COMMON")) {
        String[] array = m_statement.split("/", 0);
        return StringUtils.join(array, "/", array.length - 1);
      }
    }
    if (m_sourceFile != null && m_sourceFile.getPath() != null) {
      return m_sourceFile.getPath();
    }
    return m_strSourceFile;
  }

  /**
   * Set the source file name
   *
   * @param Source file name (set if there is a setting even for a file that does not have a header
   *     etc.)
   */
  public void setStrSourceFile(String strSourceFile) {
    m_strSourceFile = strSourceFile;
  }

  @Override
  public int hashCode() {
    int hash = 0;
    hash += this.m_startline;
    hash += this.m_endline;
    return hash;
  }

  /**
   * Check if the start and end lines overlap.
   *
   * @param start Start line
   * @param end End line
   * @return true = overlap
   */
  public boolean isOverlap(CodeLine start, CodeLine end) {
    if (this.getSourceFile() == null) return false;
    if (start == null) return false;
    if (end == null) return false;
    if (!this.getSourceFile().equals(start.getSourceFile())) return false;
    if (!this.getSourceFile().equals(end.getSourceFile())) return false;

    if ((this.getStartLine() >= start.getStartLine() && this.getStartLine() <= start.getEndLine())
        || (this.getEndLine() >= end.getStartLine() && this.getEndLine() <= end.getEndLine())) {
      return true;
    } else if (this.getStartLine() <= start.getStartLine()
        && this.getEndLine() >= end.getEndLine()) {
      return true;
    }
    return false;
  }
}
