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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import jp.riken.kscope.Application;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.parser.IAnalyseParser;
import jp.riken.kscope.xcodeml.xml.gen.ObjectFactory;
import jp.riken.kscope.xcodeml.xml.gen.XcodeProgram;

/**
 * XcodeML parsing abstract class
 *
 * <p>Generate XcodeProgram class by batch binding from XML file of XcodeML output. Create a line of
 * code from the generated XcodeProgram class and register it in the database.
 *
 * @author RIKEN
 */
public abstract class XcodeMLParserCls implements IAnalyseParser {

  int HIST_NUM = 10;
  Integer[] histgram;
  int line_counter = 0;
  /** Progress display property */
  PropertyChangeSupport m_statusProperty;
  /** Cancel flag */
  boolean m_cancel = false;
  /** What to do if there is no include file true: Confirm / false: Ignore */
  @SuppressWarnings("unused")
  private boolean m_confirm_include = true;

  /** XML file to be parsed */
  protected SourceFile m_sourceFile;

  /** XcodeML: XcodeProgram element (top element) */
  protected XcodeProgram m_program;

  /** Constructor */
  public XcodeMLParserCls() {
    histgram = new Integer[10];
    for (int i = 0; i < 10; i++) histgram[i] = 0;

    // Display to status bar For listeners
    m_statusProperty = new PropertyChangeSupport(this);
  }

  /**
   * Read the file from the source file and perform preprocessing.
   *
   * @param file source file
   */
  @Override
  public void readFile(SourceFile file) {

    firePropertyChange("prograss_start", null, null);
    firePropertyChange("prograss_maxvalue", null, 100);

    try {
      m_sourceFile = file;

      String filePath = file.getPath();

      long start = System.currentTimeMillis();

      // Create a JAXBContext object
      JAXBContext context = JAXBContext.newInstance(ObjectFactory.class);

      // Get the Unmarsaller object
      Unmarshaller unmarshaller = context.createUnmarshaller();

      // Unmarshalling
      // The return value is the XcodeProgram class
      m_program = (XcodeProgram) unmarshaller.unmarshal(new File(filePath));

      long stop = System.currentTimeMillis();
      long diff = stop - start;
      // System.out.println ("JaxbReader execution time:" + diff + "milliseconds");

    } catch (JAXBException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);
    } finally {
      firePropertyChange("prograss_clear", null, null);
    }
  }

  /**
   * Register the property change listener.
   *
   * @param listener Status listener
   */
  @Override
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    this.m_statusProperty.addPropertyChangeListener(listener);
  }

  /**
   * Delete the property change listener.
   *
   * @param listener Status listener
   */
  @Override
  public void removePropertyChangeListener(PropertyChangeListener listener) {
    this.m_statusProperty.removePropertyChangeListener(listener);
  }

  /**
   * Notify listener of property changes
   *
   * @param propertyName Property name
   * @param oldValue Old property value
   * @param newValue New property value
   */
  @Override
  public void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
    this.m_statusProperty.firePropertyChange(propertyName, oldValue, newValue);
    if ("status_message".equals(propertyName)) {
      Application.status.setMessageStatus((String) newValue);
    } else if ("status_sub_message".equals(propertyName)) {
      Application.status.setMessageStatus((String) newValue);
    } else if ("prograss_clear".equals(propertyName)) {
      Application.status.setProgressStart(false);
    }
    /*
    else if ("prograss_maxvalue".equals(propertyName)) {
        Application.status.setProgress(0, (Integer)newValue);
    }
    else if ("prograss_value".equals(propertyName)) {
        Application.status.setProgressValue((Integer)newValue);
    }
    */
    else if ("prograss_start".equals(propertyName)) {
      Application.status.setProgressStart(true);
    }
  }

  /**
   * Get the event to stop parsing processing due to property change.
   *
   * @param evt Property change event
   */
  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    if ("cancel".equals(evt.getPropertyName())) {
      setCancel((Boolean) evt.getNewValue());
    }
  }

  /**
   * Set to stop parsing processing.
   *
   * @param cancel true: Cancel
   */
  @Override
  public void setCancel(boolean cancel) {
    m_cancel = cancel;
  }

  /**
   * Judge to stop the parsing process.
   *
   * @return true: Cancel
   */
  public boolean isCancel() {
    return m_cancel;
  }

  /**
   * Check if the currentUnit of the Fortran class is ProgramUnit.
   *
   * @param ft Output Fortran class
   * @return true: currentUnit is a ProgramUnit.
   */
  @SuppressWarnings("unused")
  private boolean isCurrentProgramUnit(Fortran ft) {
    ProgramUnit unit = ft.get_current_unit();
    if (unit instanceof ProgramUnit) {
      return true;
    }

    return false;
  }

  /**
   * Confirmation flag for includes that did not exist
   *
   * @param confirm true: Check include file if it does not exist
   */
  @Override
  public void setConfirmInclude(boolean confirm) {
    m_confirm_include = true;
  }

  /**
   * Create a line of code from the generated XcodeProgram class and register it in the database.
   *
   * @param ft Analysis result storage Fortran database
   * @throws InterruptedException Interrupt error
   */
  @Override
  public void parseFile(Fortran ft) throws InterruptedException {
    try {
      //            String outputFilePath = "xcodeml_debug.txt";
      //            PrintWriter writer = new PrintWriter(new BufferedWriter(
      //                    new FileWriter(outputFilePath)));

      XcodeMLContext context = new XcodeMLContext();
      // Debug output
      // XcodeMLOption.setDebugOutput(true);

      CodeBuilder fwriter = new CodeBuilder(context);
      //            fwriter.setWriter(writer);
      context.setCodeBuilder(fwriter);
      DbUpdater db = new DbUpdater(ft, context);
      context.setDbUpdater(db);
      context.setSourceXmlFile(m_sourceFile);

      XcodeMLVisitor visitor = new XcodeMLVisitor(context);
      if (!visitor.invokeEnter(m_program)) {
        throw new XcodeMLException(context.getLastErrorMessage(), context.getLastCause());
      }
      fwriter.flush();

    } catch (XcodeMLException ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);

    } catch (Exception ex) {
      firePropertyChange("status_sub_message", null, "exception");
      firePropertyChange("prograss_clear", null, null);
      throw new LanguageException(ex, m_sourceFile);
    }

    // Status bar: Progress message
    firePropertyChange("status_sub_message", null, "done");
    firePropertyChange("prograss_clear", null, null);
  }

  @Override
  public CodeLine[] getCodeLineList() throws InterruptedException {
    return null;
  }
}
