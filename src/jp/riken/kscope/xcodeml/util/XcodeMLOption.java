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

package jp.riken.kscope.xcodeml.util;

/**
 * XcodeML parsing options
 *
 * @author RIKEN
 */
public class XcodeMLOption {

  /**
   * Language type
   *
   * @author RIKEN
   */
  public enum LanguageType {
    /** C language */
    C,
    /** Fortran */
    F,
    ;
  }

  /** Compiler identifier: GNU */
  public static final int COMP_VENDOR_GNU = 'G';
  /** Compiler identifier: INTEL */
  public static final int COMP_VENDOR_INTEL = 'I';

  /** if suppress to write line directives */
  private static boolean _suppressLineDirective = true;

  /** if compiling Xcalable MP is enabled */
  private static boolean _xcalableMP = false;

  private static boolean _xcalableMPthreads = false;

  /** if compiling Xcalable MP is enabled */
  private static boolean _openMP = false;

  /** if debug output is enabled */
  private static boolean _debugOutput = false;

  /** target language ID */
  private static LanguageType _language = LanguageType.F;

  /** if transforming Fortran IO statement as atomic operation */
  private static boolean _isAtomicIO = false;

  /** backend compiler vendor */
  private static int _compilerVendor = COMP_VENDOR_GNU;

  /** Constructor */
  private XcodeMLOption() {}

  /**
   * Sets compiler to or not to suppress to write line directives.
   *
   * @param enable true then compiler suppress to write line directives.
   */
  public static void setIsSuppressLineDirective(boolean enable) {
    _suppressLineDirective = enable;
  }

  /**
   * Checks does decompiler suppress line directives.
   *
   * @return true if compiler suppress to write line directives.
   */
  public static boolean isSuppressLineDirective() {
    return _suppressLineDirective;
  }

  /**
   * Sets compiler to or not to translate XcalableMP directive.
   *
   * @param enable true then translate XcalableMP directive.
   */
  public static void setIsXcalableMP(boolean enable) {
    _xcalableMP = enable;
  }

  /**
   * Checks does compiler translate XcalableMP directive.
   *
   * @return true if compiler translate XcalableMP directive.
   */
  public static boolean isXcalableMP() {
    return _xcalableMP;
  }

  /**
   * Sets compiler to or not to translate XcalableMP-threads directive.
   *
   * @param enable true then translate XcalableMP directive.
   */
  public static void setIsXcalableMPthreads(boolean enable) {
    _xcalableMPthreads = enable;
  }

  /**
   * Checks does compiler translate XcalableMP-threads directive.
   *
   * @return true if compiler translate XcalableMP directive.
   */
  public static boolean isXcalableMPthreads() {
    return _xcalableMPthreads;
  }

  /**
   * Sets compiler to or not to translate OpenMP directive.
   *
   * @param enable true then translate XcalableMP directive.
   */
  public static void setIsOpenMP(boolean enable) {
    _openMP = enable;
  }

  /**
   * Checks does compiler translate OpenMP directive.
   *
   * @return true if compiler translate OpenMP directive.
   */
  public static boolean isOpenMP() {
    return _openMP;
  }

  /**
   * Return true if debug output enabled.
   *
   * @return debug output
   */
  public static boolean isDebugOutput() {
    return _debugOutput;
  }

  /**
   * Set debug output.
   *
   * @param enable debug output
   */
  public static void setDebugOutput(boolean enable) {
    _debugOutput = enable;
  }

  /**
   * Set language
   *
   * @param lang language
   */
  public static void setLanguage(LanguageType lang) {
    _language = lang;
  }

  /**
   * Get language
   *
   * @return language
   */
  public static LanguageType getLanguage() {
    return _language;
  }

  /**
   * Return if the language is C
   *
   * @return language is C
   */
  public static boolean isLanguageC() {
    return _language.equals(LanguageType.C);
  }

  /**
   * Return if the language is Fortran
   *
   * @return language is Fortran
   */
  public static boolean isLanguageF() {
    return _language.equals(LanguageType.F);
  }

  /**
   * Return compiler vendor constant. (COMP_VENDOR_*)
   *
   * @return compiler vendor
   */
  public static int getCompilerVendor() {
    return _compilerVendor;
  }

  /**
   * Set compiler vendor constant. (COMP_VENDOR_*)
   *
   * @param vendor compiler vendor
   */
  public static void setCompilerVendor(int vendor) {
    _compilerVendor = vendor;
  }

  /**
   * Get if or not IO statements are transformed to atomic operation.
   *
   * @return atomic operation
   */
  public static boolean isAtomicIO() {
    return _isAtomicIO || _compilerVendor == COMP_VENDOR_INTEL;
  }

  /**
   * Set if or not IO statements are transformed to atomic operation.
   *
   * @param atomicIO atomic operation
   */
  public static void setIsAtomicIO(boolean atomicIO) {
    _isAtomicIO = atomicIO;
  }
}
