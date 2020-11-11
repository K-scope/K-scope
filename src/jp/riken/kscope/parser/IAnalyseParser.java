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

package jp.riken.kscope.parser;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Fortran;

/**
 * Fortran parsing interface
 *
 * @author RIKEN
 */
public interface IAnalyseParser extends PropertyChangeListener {

  /**
   * Cancel parsing
   *
   * @param cancel true = cancel
   */
  public void setCancel(boolean cancel);

  /**
   * Set a flag to display a confirmation dialog when the include file does not exist
   *
   * @param confirm true = Show confirmation dialog
   */
  public void setConfirmInclude(boolean confirm);

  /**
   * Read from source file
   *
   * @param file Source file
   * @throws IOException File read error
   */
  public void readFile(SourceFile file) throws IOException;

  /**
   * Parse the source file and set it in the Fortran database
   *
   * @param ft Fortran database
   * @throws InterruptedException Interrupt error
   */
  public void parseFile(Fortran ft) throws InterruptedException;

  /**
   * Add property change listener
   *
   * @param listener Property change listener
   */
  public void addPropertyChangeListener(PropertyChangeListener listener);

  /**
   * Delete property change listener
   *
   * @param listener Property change listener
   */
  public void removePropertyChangeListener(PropertyChangeListener listener);

  /**
   * Raise a property change event.
   *
   * @param propertyName Property name
   * @param oldValue property old value
   * @param newValue Property new value
   */
  public void firePropertyChange(String propertyName, Object oldValue, Object newValue);

  /**
   * Property change event
   *
   * @param evt Property change event information
   */
  @Override
  public void propertyChange(PropertyChangeEvent evt);

  /**
   * Get the source code line list
   *
   * @return Source code line list
   * @throws InterruptedException Interrupt error
   */
  public CodeLine[] getCodeLineList() throws InterruptedException;

  /**
   * Get the source file
   *
   * @return source file
   */
  public SourceFile getLanguageFile();

  /** Parse only the source file from the XML file */
  public void parseSourceFile();

  /**
   * Set the reference folder for source files
   *
   * @param folder Reference folder for source files
   */
  public void setBaseFolder(File folder);

  /**
   * Get error information.
   *
   * @return Error information list
   */
  public ErrorInfo[] getErrorInfos();
}
