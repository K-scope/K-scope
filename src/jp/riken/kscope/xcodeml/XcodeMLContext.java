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

package jp.riken.kscope.xcodeml;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.xcodeml.util.XcodeMLOption;
import jp.riken.kscope.xcodeml.xml.EnumError;
import jp.riken.kscope.xcodeml.xml.IDefBaseStatement;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.StatementLabel;

/**
 * XcodeML parser context class. <br>
 * Manage the information needed to parse XcodeML.
 *
 * @author RIKEN
 */
public class XcodeMLContext {

  /**
   * IXmlNode stack class
   *
   * @author RIKEN
   */
  public class InvokeNodeStack extends LinkedList<IXmlNode> {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /**
     * Output a string of stacked nodes
     *
     * @return string output
     */
    @Override
    public String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append("[Invoke Node Stack]\n");
      for (IXmlNode node : this.toArray(new IXmlNode[0])) {
        sb.append(node.getClass().getSimpleName());
        sb.append("\n");
      }
      return sb.toString();
    }
  }

  /** Error message */
  private String m_lastErrorMessage = null;
  /** Data type table management class */
  private XcodeMLTypeManager m_typeManager;
  /** Error exception */
  private Exception m_lastCause;
  /** Source code generation class */
  private CodeBuilder m_codebuilder;
  /** Fortran database registration class */
  private DbUpdater m_updater = null;
  ;
  /** Read XML file */
  private SourceFile m_sourceXmlFile;
  /** XML node stack */
  private InvokeNodeStack _invokeNodeStack;
  /** Last XML node */
  private IXmlNode _previousNode;
  /** XML node scan class */
  private XcodeMLVisitor _visitor;
  /** Reference folder for source files */
  private File baseFoleder;

  /** Constructor */
  public XcodeMLContext() {
    m_typeManager = new XcodeMLTypeManager();
    _invokeNodeStack = new InvokeNodeStack();
  }

  /**
   * Judge debug mode.
   *
   * @return true/false.
   */
  public boolean isDebugMode() {
    return XcodeMLOption.isDebugOutput();
  }

  /**
   * Return cause of last error.
   *
   * @return Exception
   */
  public Exception getLastCause() {
    return m_lastCause;
  }

  /**
   * Judge output line directive.
   *
   * @return true/false.
   */
  public boolean isOutputLineDirective() {
    return !XcodeMLOption.isSuppressLineDirective();
  }

  /**
   * Set output line directive flag.
   *
   * @param outputLineDirective Output line directive flag.
   */
  public void setOutputLineDirective(boolean outputLineDirective) {
    XcodeMLOption.setIsSuppressLineDirective(!outputLineDirective);
  }

  /**
   * Get writer for Fortran.
   *
   * @return Instance of XmfWriter.
   */
  public DbUpdater getDbUpdater() {
    return m_updater;
  }

  /**
   * Set writer for Fortran.
   *
   * @param writer XfTypeManager.
   */
  public void setDbUpdater(DbUpdater writer) {
    m_updater = writer;
  }

  /**
   * Get type manager.
   *
   * @return Instance of XfTypeManager.
   */
  public XcodeMLTypeManager getTypeManager() {
    return m_typeManager;
  }

  /**
   * Check if an error has occurred
   *
   * @return true = error occurred
   */
  public boolean hasError() {
    if (m_lastErrorMessage != null) {
      return true;
    }
    return false;
  }

  /**
   * Set error message
   *
   * @param message Error message
   */
  public void setLastErrorMessage(String message) {
    m_lastErrorMessage = message;
    m_lastCause = new Exception();
  }

  /**
   * Get error messages
   *
   * @return error message
   */
  public String getLastErrorMessage() {
    if (hasError() == false) {
      return EnumError.SUCCESS.message();
    }
    return m_lastErrorMessage;
  }

  /**
   * Debug output
   *
   * @param message Output message
   */
  public void debugPrint(String message) {
    if (isDebugMode() != false) {
      System.out.print("Debug: ");
      System.out.print(message);
    }
  }

  /**
   * Perform formatted debug output
   *
   * @param format format
   * @param args Output message
   */
  public void debugPrint(String format, Object... args) {
    if (isDebugMode() != false) {
      System.out.print("Debug: ");
      System.out.format(format, args);
    }
  }

  /**
   * Debug output
   *
   * @param message Output message
   */
  public void debugPrintLine(String message) {
    if (isDebugMode() != false) {
      System.out.print("Debug: ");
      System.out.println(message);
    }
  }

  /**
   * Get the source code generation class
   *
   * @return Source code generation class
   */
  public CodeBuilder getCodeBuilder() {
    return m_codebuilder;
  }

  /**
   * Set the source code generation class
   *
   * @param writer Source code generation class
   */
  public void setCodeBuilder(CodeBuilder writer) {
    m_codebuilder = writer;
  }

  /**
   * Get the read XML file
   *
   * @return Read XML file
   */
  public SourceFile getSourceXmlFile() {
    return m_sourceXmlFile;
  }

  /**
   * Set the read XML file
   *
   * @param sourceXmlFile Read XML file
   */
  public void setSourceXmlFile(SourceFile sourceXmlFile) {
    m_sourceXmlFile = sourceXmlFile;
  }

  /**
   * Get the XML node stack
   *
   * @return XML node stack
   */
  public InvokeNodeStack getInvokeNodeStack() {
    return _invokeNodeStack;
  }

  /**
   * Get the number of XML node stacks
   *
   * @return Number of XML node stacks
   */
  public int getInvokeNodeStackSize() {
    return _invokeNodeStack.size();
  }

  /**
   * Get the XML node of the specified index
   *
   * @param idx Get index
   * @return XML node
   */
  public IXmlNode getInvokeNode(int idx) {

    if (idx < 0) {
      throw new IllegalArgumentException();
    }

    if (idx >= getInvokeNodeStackSize()) {
      return null;
    }

    return _invokeNodeStack.get(idx);
  }

  /**
   * Get the first node from the XML node stack. (No deletion)
   *
   * @return First node
   */
  public IXmlNode peekInvokeNode() {
    return _invokeNodeStack.peek();
  }

  /**
   * Add an XML node to the last node in the XML node stack
   *
   * @param node Additional XML node
   */
  public void pushInvokeNode(IXmlNode node) {
    _invokeNodeStack.push(node);
  }

  /**
   * Get the first node from the XML node stack. (With deletion)
   *
   * @return First node
   */
  public IXmlNode popInvokeNode() {
    return _invokeNodeStack.pop();
  }

  /**
   * Check if the specified XML node class is included in the XML node stack
   *
   * @param clazz Specified XML node class
   * @return true = exists
   */
  public boolean isInvokeAncestorNodeOf(Class<? extends IXmlNode> clazz) {

    IXmlNode node = null;
    for (Iterator<IXmlNode> it = _invokeNodeStack.iterator(); it.hasNext(); ) {
      node = it.next();
      if (clazz.equals(node.getClass())) {
        return true;
      }
    }
    return false;
  }

  /**
   * Check if the specified XML node class matches the specified index on the XML node stack
   *
   * @param clazz Specified XML node class
   * @param parentRank Specified index
   * @return true = match
   */
  public boolean isInvokeNodeOf(Class<? extends IXmlNode> clazz, int parentRank) {
    IXmlNode node = getInvokeNode(parentRank);
    if (node == null) return false;
    return clazz.equals(node.getClass());
  }

  /**
   * Get the DefBaseStatement (filename, line number) class.
   *
   * @param visitable Current XML node class
   * @return DefBaseStatement (filename, line number) class
   */
  public IDefBaseStatement getInvokeBaseStatement(IXmlNode visitable) {

    // Determine if the XML node is currently in the DefBaseStatement (filename, line number) class.
    if (visitable instanceof IDefBaseStatement) {
      return (IDefBaseStatement) visitable;
    }

    // Get the file name, start line number, and end line number from the parent element of the
    // current element.
    IXmlNode node = null;
    IDefBaseStatement base_node = null;
    for (Iterator<IXmlNode> it = _invokeNodeStack.iterator(); it.hasNext(); ) {
      node = it.next();
      if (node instanceof IDefBaseStatement) {
        base_node = (IDefBaseStatement) node;
        break;
      }
    }

    return base_node;
  }

  /** @return m_visitor XML node scanning class */
  public XcodeMLVisitor getVisitor() {
    return _visitor;
  }

  /** @param visitor Set the XML node scan class. */
  public void setVisitor(XcodeMLVisitor visitor) {
    _visitor = visitor;
  }

  /**
   * Set the pre-node.
   *
   * @param node Pre-XML node
   */
  public void setPreviousNode(IXmlNode node) {
    this._previousNode = node;
  }

  /**
   * Get the Statement Label node. If the pre-XML node is a StatementLabel, return the pre-XML node.
   *
   * @return Statement Label node
   */
  public StatementLabel getStatementLabelNode() {
    if (_previousNode instanceof StatementLabel) {
      return (StatementLabel) _previousNode;
    }
    return null;
  }

  /**
   * Get the Statement Label node. If the pre-XML node is a StatementLabel, return the pre-XML node.
   *
   * @return Statement Label node
   */
  public String getStatementLabel() {
    if (_previousNode instanceof StatementLabel) {
      return ((StatementLabel) _previousNode).getLabelName();
    }
    return null;
  }

  /**
   * Get the reference folder of the source file
   *
   * @return Reference folder for source files
   */
  public File getBaseFolder() {
    return this.baseFoleder;
  }

  /**
   * Set the reference folder for source files
   *
   * @param folder Reference folder for source files
   */
  public void setBaseFolder(File folder) {
    this.baseFoleder = folder;
  }
}
