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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Stack;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.FcontainsStatement;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionDefinition;
import jp.riken.kscope.xcodeml.xml.gen.FmoduleDefinition;
import jp.riken.kscope.xcodeml.xml.gen.GlobalSymbols;
import jp.riken.kscope.xcodeml.xml.gen.Name;
import jp.riken.kscope.xcodeml.xml.gen.ObjectFactory;
import jp.riken.kscope.xcodeml.xml.gen.TypeTable;
import jp.riken.kscope.xcodeml.xml.gen.XcodeProgram;

/**
 * XcodeML parsing class
 *
 * <p>Generate XcodeProgram class by batch binding from XML file of XcodeML output. Create a line of
 * code from the generated XcodeProgram class and register it in the database.
 *
 * @author RIKEN
 */
public class XcodeMLParserStax extends XcodeMLParserCls {

  /** Factory for StAX */
  private XMLInputFactory factory = null;

  /** Factory for JAXB */
  private JAXBContext context = null;
  /** JAXB Ammershaller */
  private Unmarshaller unmarshaller = null;
  /** Currently processing node */
  protected Stack<IXmlNode> m_nodeStack = null;
  /** Fortran database */
  protected Fortran m_dbFortran = null;
  /** XcodeML perspective settings */
  protected XcodeMLContext m_xmodContext = null;
  /** XcodeML parser */
  protected XcodeMLVisitor m_xmodVisitor = null;
  // modify at 2013/03/01 by @hira Change from File to Source File
  /** Original source file (Fortran source file) */
  protected SourceFile languageFile = null;

  /** Constructor */
  public XcodeMLParserStax() {
    super();

    try {
      // Generate a factory for StAX
      factory = XMLInputFactory.newInstance();

      // Generate factory for JAXB
      context = JAXBContext.newInstance(ObjectFactory.class);
      // Generate unmarshaller
      unmarshaller = context.createUnmarshaller();

      // XcodeML perspective settings
      m_xmodContext = new XcodeMLContext();
      // Debug output
      // XcodeMLOption.setDebugOutput(true);

      //            String outputFilePath = "xcodeml_debug.txt";
      //            PrintWriter writer = new PrintWriter(new BufferedWriter(
      //                    new FileWriter(outputFilePath)));

      CodeBuilder fwriter = new CodeBuilder(m_xmodContext);
      //            fwriter.setWriter(writer);

      m_xmodContext.setCodeBuilder(fwriter);

      m_xmodVisitor = new XcodeMLVisitor(m_xmodContext);

      // Current FfunctionDefinition, FmoduleDefinition stack list
      m_nodeStack = new Stack<IXmlNode>();

    } catch (FactoryConfigurationError e) {
      e.printStackTrace();
    } catch (JAXBException e) {
      e.printStackTrace();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Read the file from the source file and perform preprocessing.
   *
   * @param file source file
   */
  @Override
  public void readFile(SourceFile file) {
    m_sourceFile = file;
  }

  /**
   * Create a line of code from the generated XcodeProgram class and register it in the database.
   *
   * @param ft Analysis result storage Fortran database
   * @throws InterruptedException Interrupt error
   */
  @Override
  public void parseFile(Fortran ft) throws InterruptedException {

    firePropertyChange("prograss_start", null, null);
    firePropertyChange("prograss_maxvalue", null, 100);
    firePropertyChange(
        "status_sub_message", null, "parse start " + m_sourceFile.getFile().getName());

    XMLStreamReader reader = null;
    InputStream stream = null;

    // Preparation for parsing XcodeML
    m_dbFortran = ft;
    DbUpdater db = new DbUpdater(m_dbFortran, m_xmodContext);
    m_xmodContext.setDbUpdater(db);
    m_xmodContext.setSourceXmlFile(m_sourceFile);

    try {
      String xmlfile = m_sourceFile.getPath();
      stream = new FileInputStream(xmlfile);

      // Generate parser
      reader = factory.createXMLStreamReader(stream);

      // Event loop
      while (reader.hasNext()) {
        // get the next event
        int eventType = reader.next();
        if (eventType == XMLStreamReader.START_ELEMENT) {
          String elem_name = reader.getLocalName();
          //                    System.out.println(elem_name);
          firePropertyChange("status_sub_message", null, "parsing..." + elem_name);

          // XcodeProgram
          if (parseXcodeProgram(reader)) {
          }
          // typeTable, globalSymbols
          else if (unmarshalType(reader)) {
          }
          // FfunctionDefinition, FmoduleDefinition, body, FcontainsStatement
          else if (startElement(reader)) {
          }
          // Other
          else if (unmarshalNode(reader)) {
          }
        } else if (eventType == XMLStreamReader.END_ELEMENT) {
          endElement(reader);
        }

        /** for debug by @hira */
        if (Thread.interrupted()) {
          throw new InterruptedException();
        }
        /** for debug by @hira */
      }
    } catch (IOException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);

    } catch (XcodeMLException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);
    } catch (XMLStreamException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);
    } finally {

      // Status bar: Progress message
      firePropertyChange("status_sub_message", null, "done");
      firePropertyChange("prograss_clear", null, null);

      if (reader != null) {
        try {
          reader.close();
        } catch (XMLStreamException ex) {
        }
      }
      if (stream != null) {
        try {
          stream.close();
        } catch (IOException ex) {
        }
      }
    }

    // Status bar: Progress message
    firePropertyChange("status_sub_message", null, "done");
    firePropertyChange("prograss_clear", null, null);
  }

  /**
   * Unmarshall type table elements with JAXB. Unmarshall only if the element name is typeTable,
   * globalSymbols.
   *
   * @param reader Stream reader
   * @return Success or failure
   * @throws XcodeMLException XML read error
   */
  private boolean unmarshalType(XMLStreamReader reader) throws XcodeMLException {

    String elem_name = reader.getLocalName();
    if (!"typeTable".equals(elem_name) && !"globalSymbols".equals(elem_name)) {
      return false;
    }

    try {
      // Unmarshalling
      Object obj = unmarshaller.unmarshal(reader);
      if (m_program == null) {
        m_program = new XcodeProgram();
      }

      // Set the type table in XcodeProgram.
      if ("typeTable".equals(elem_name)) {
        m_program.setTypeTable((TypeTable) obj);
        invokeEnter((TypeTable) obj);
      } else if ("globalSymbols".equals(elem_name)) {
        m_program.setGlobalSymbols((GlobalSymbols) obj);
        invokeEnter((GlobalSymbols) obj);
      }

    } catch (JAXBException ex) {
      // Exception handling: Unmarshalling failed. [Element name =% s]
      throw new XcodeMLException(
          Message.getString("xcodemlparserstax.error.unmarshaller", reader.getLocalName()));
    }

    return true;
  }

  /**
   * Unmarshall type table elements with JAXB.
   *
   * @param reader Stream reader
   * @return Success or failure
   * @throws XcodeMLException XML read error
   */
  private boolean unmarshalNode(XMLStreamReader reader) throws XcodeMLException {
    try {
      // element name
      String elem_name = reader.getLocalName();

      // For FfunctionDefinition / name
      if ("name".equals(elem_name)) {
        // Pass it to Visitor along with the name element under the FfunctionDefinition element.
        // DB registration is done from Visitor.
        IXmlNode node = m_nodeStack.peek();
        if (node != null && node instanceof FfunctionDefinition) {
          // Unmarshalling
          Name name = (Name) unmarshaller.unmarshal(reader);
          assert (name != null);

          // Assemble only the FfunctionDefinition / name element and pass it to the visitor.
          ((FfunctionDefinition) node).setName(name);
          node.enter(m_xmodVisitor);
          return true;
        }
      }

      // Unmarshalling
      IXmlNode node = (IXmlNode) unmarshaller.unmarshal(reader);

      invokeEnter(node);

    } catch (JAXBException ex) {
      // Exception handling: Unmarshalling failed. [Element name =% s]
      throw new XcodeMLException(
          Message.getString("xcodemlparserstax.error.unmarshaller", reader.getLocalName()));
    }

    return true;
  }

  /**
   * Get the XML element directly without unmarshalling with JAXB.
   *
   * <p>FfunctionDefinition, FmoduleDefinition, FcontainsStatement elements
   *
   * @param reader Stream reader
   * @return Success or failure
   * @throws XcodeMLException XML read error
   */
  private boolean startElement(XMLStreamReader reader) throws XcodeMLException {

    // element name
    String elem_name = reader.getLocalName();

    // Elements to ignore
    if ("globalDeclarations".equals(elem_name)) return true;
    if ("body".equals(elem_name)) return true;

    // Attribute
    String filename = reader.getAttributeValue(null, "file");
    String lineno = reader.getAttributeValue(null, "lineno");
    String endlineno = reader.getAttributeValue(null, "endlineno");

    IXmlNode node = null;
    // Except for FfunctionDefinition, FmoduleDefinition, and body elements
    if ("FfunctionDefinition".equals(elem_name)) {
      node = new FfunctionDefinition();
      ((FfunctionDefinition) node).setFile(filename);
      ((FfunctionDefinition) node).setLineno(lineno);
      ((FfunctionDefinition) node).setEndlineno(endlineno);
    } else if ("FmoduleDefinition".equals(elem_name)) {
      String name = reader.getAttributeValue(null, "name");
      node = new FmoduleDefinition();
      ((FmoduleDefinition) node).setFile(filename);
      ((FmoduleDefinition) node).setLineno(lineno);
      ((FmoduleDefinition) node).setEndlineno(endlineno);
      ((FmoduleDefinition) node).setName(name);
      node.enter(m_xmodVisitor);
    } else if ("FcontainsStatement".equals(elem_name)) {
      node = new FcontainsStatement();
      ((FcontainsStatement) node).setFile(filename);
      ((FcontainsStatement) node).setLineno(lineno);
      ((FcontainsStatement) node).setEndlineno(endlineno);
      node.enter(m_xmodVisitor);
    }

    if (node == null) return false;

    // Put the XML element on the stack
    m_nodeStack.push(node);

    // Clear the previous XML node.
    m_xmodContext.setPreviousNode(null);

    return true;
  }

  /**
   * Event by node termination tag
   *
   * <p>Call the leave method of Visitor and register the end of the block in the DB.
   *
   * @param reader XML Schema Reader
   * @return Success or failure
   * @throws XcodeMLException Parsing error
   */
  private boolean endElement(XMLStreamReader reader) throws XcodeMLException {

    // element name
    String elem_name = reader.getLocalName();

    if (m_nodeStack.size() <= 0) return true;

    // Stack node
    IXmlNode node = m_nodeStack.peek();

    if ("FfunctionDefinition".equals(elem_name) && node instanceof FfunctionDefinition) {
      node.leave(m_xmodVisitor);
      m_nodeStack.pop();
    } else if ("FmoduleDefinition".equals(elem_name) && node instanceof FmoduleDefinition) {
      node.leave(m_xmodVisitor);
      m_nodeStack.pop();
    } else if ("FcontainsStatement".equals(elem_name) && node instanceof FcontainsStatement) {
      node.leave(m_xmodVisitor);
      m_nodeStack.pop();
    }

    return true;
  }

  /**
   * Parse XML node
   *
   * @param node XML node
   * @throws XcodeMLException
   */
  private void invokeEnter(IXmlNode node) throws XcodeMLException {

    if (!m_xmodVisitor.invokeEnter(node)) {
      throw new XcodeMLException(m_xmodContext.getLastErrorMessage(), m_xmodContext.getLastCause());
    }

    // Set the XML node in advance.
    m_xmodContext.setPreviousNode(node);

    if (m_xmodContext.getCodeBuilder() != null) m_xmodContext.getCodeBuilder().flush();
  }

  /**
   * Parse the root element XcodeProgram.
   *
   * <p>Check if the source file described in the Xcode Program exists. If the source file does not
   * exist, XcodeMLException will be thrown.
   *
   * @param reader XML Schema Reader
   * @return Success or failure
   * @throws XcodeMLException Parsing error
   */
  private boolean parseXcodeProgram(XMLStreamReader reader) throws XcodeMLException {

    // element name
    String elem_name = reader.getLocalName();
    if (!"XcodeProgram".equals(elem_name)) return false;

    String source = reader.getAttributeValue(null, "source");
    if (source == null) {
      throw new XcodeMLException(
          Message.getString("xcodemlparserstax.error.sourcefile")); // The source file is not set.
    }

    if (new File(source).isAbsolute()) {
      // Set the original source file
      this.languageFile = new SourceFile(source);
    } else {
      // Get the original source file path
      File path = m_sourceFile.getFile().getParentFile();
      File srcPath = new File(path.getAbsoluteFile() + File.separator + source);

      if (this.m_xmodContext.getBaseFolder() != null) {
        // Get the relative path from the reference path
        String relPath = FileUtils.getRelativePath(srcPath, this.m_xmodContext.getBaseFolder());
        if (relPath == null) {
          this.languageFile = new SourceFile(srcPath);
        } else {
          this.languageFile = new SourceFile(relPath);
        }
      } else {
        this.languageFile = new SourceFile(srcPath);
      }
      // Since it may be a relative path setting, set the update date from the absolute path.
      this.languageFile.setModifyDate(srcPath);
    }

    // // Existence check of original source file
    //        if (!this.languageFile.exists()) {
    // throw new XcodeMLException ("source file [" + source + "] does not exist.");
    //        }
    // Update the modification date of the XML file.
    this.m_sourceFile.updateModifyDate();

    // Associate the source file with the XML file and the XML file with the source file
    this.m_sourceFile.setRelationFile(this.languageFile);
    this.languageFile.setRelationFile(this.m_sourceFile);

    return true;
  }

  @Override
  public CodeLine[] getCodeLineList() throws InterruptedException {

    // Create a dummy database
    Fortran ft = new Fortran();
    parseFile(ft);
    CodeBuilder builder = m_xmodContext.getCodeBuilder();
    if (builder == null) return null;

    return builder.getCodeLineList();
  }

  /**
   * Get the original source file (Fortran source file)
   *
   * @return Original source file (Fortran source file)
   */
  @Override
  public SourceFile getLanguageFile() {
    return languageFile;
  }

  /** Get the source file from the XML file. */
  @Override
  public void parseSourceFile() {

    firePropertyChange("prograss_start", null, null);
    firePropertyChange("prograss_maxvalue", null, 100);
    firePropertyChange(
        "status_sub_message", null, "parse start " + m_sourceFile.getFile().getName());

    XMLStreamReader reader = null;
    InputStream stream = null;

    try {
      String xmlfile = m_sourceFile.getPath();
      stream = new FileInputStream(xmlfile);

      // Generate parser
      reader = factory.createXMLStreamReader(stream);

      // Event loop
      while (reader.hasNext()) {
        // get the next event
        int eventType = reader.next();
        if (eventType == XMLStreamReader.START_ELEMENT) {
          String elem_name = reader.getLocalName();
          //                    System.out.println(elem_name);
          firePropertyChange("status_sub_message", null, "parsing..." + elem_name);

          // Parse only the source file list from the Xcode Program.
          if (parseXcodeProgram(reader)) {
            break;
          }
        } else if (eventType == XMLStreamReader.END_ELEMENT) {
          endElement(reader);
        }
      }
    } catch (IOException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);

    } catch (XcodeMLException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);
    } catch (XMLStreamException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);
    } finally {

      // Status bar: Progress message
      firePropertyChange("status_sub_message", null, "done");
      firePropertyChange("prograss_clear", null, null);

      if (reader != null) {
        try {
          reader.close();
        } catch (XMLStreamException ex) {
        }
      }
      if (stream != null) {
        try {
          stream.close();
        } catch (IOException ex) {
        }
      }
    }

    // Status bar: Progress message
    firePropertyChange("status_sub_message", null, "done");
    firePropertyChange("prograss_clear", null, null);
  }

  /**
   * Set the reference folder for source files
   *
   * @param folder Reference folder for source files
   */
  @Override
  public void setBaseFolder(File folder) {
    m_xmodContext.setBaseFolder(folder);
  }

  /**
   * Get error information.
   *
   * @return Error information list
   */
  @Override
  public ErrorInfo[] getErrorInfos() {
    if (this.m_xmodContext == null) return null;
    if (this.m_xmodContext.getDbUpdater() == null) return null;

    return this.m_xmodContext.getDbUpdater().getListErrorInfo();
  }
}
