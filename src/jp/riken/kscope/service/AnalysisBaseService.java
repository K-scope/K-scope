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

import jp.riken.kscope.language.Fortran;

/**
 * Base class for analytics services
 *
 * @author RIKEN
 */
public class AnalysisBaseService extends BaseService {

  /** Fortran database */
  protected Fortran fortranDb = null;

  /** Constructor */
  public AnalysisBaseService() {}

  /**
   * Constructor
   *
   * @param fortran Fortran database
   */
  public AnalysisBaseService(Fortran fortran) {
    this.setFortranDb(fortran);
  }

  /**
   * Get the Fortran database.
   *
   * @return fortranDb Fortran database
   */
  public Fortran getFortranDb() {
    return fortranDb;
  }

  /**
   * Set up the Fortran database.
   *
   * @param fortranDb Fortran database
   */
  public void setFortranDb(Fortran fortranDb) {
    this.fortranDb = fortranDb;
  }
}
