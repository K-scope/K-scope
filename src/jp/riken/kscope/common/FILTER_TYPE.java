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
package jp.riken.kscope.common;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import jp.riken.kscope.Message;

/**
 * Structure tree filter type
 *
 * @author RIKEN
 */
public enum FILTER_TYPE {
  // Structure tree filter type
  /** Subroutines / functions */
  PROCEDURE(
      Message.getString("mainmenu.view.filter.subroutine-function"), // Subroutines / functions
      new Class<?>[] {jp.riken.kscope.language.Procedure.class}),
  /** CALL statement */
  PROCEDUREUSAGE(
      Message.getString("filter_type.enum.call"), // CALL statement
      new Class<?>[] {jp.riken.kscope.language.ProcedureUsage.class}),
  /** DO statement */
  REPETITION(
      Message.getString("filter_type.enum.do"), // DO statement
      new Class<?>[] {
        jp.riken.kscope.language.Repetition.class, jp.riken.kscope.language.ArrayExpression.class
      }),
  /** SELECT statement */
  SELECTION_SELECT(
      Message.getString("filter_type.enum.selection"), // SELECT, CASE statement
      new Class<?>[] {
        jp.riken.kscope.language.Selection.class, jp.riken.kscope.language.Condition.class
      }),
  /** IF statement */
  SELECTION_IF(
      Message.getString("filter_type.enum.if"), // IF, WHERE, ELSE statement
      new Class<?>[] {
        jp.riken.kscope.language.Selection.class, jp.riken.kscope.language.Condition.class
      }),
  /** Assignment statement */
  SUBSTITUTION(
      Message.getString("filter_type.enum.assign"), // Assignment statement
      new Class<?>[] {jp.riken.kscope.language.Substitution.class}),
  /** Flow control statement */
  FLOW(
      Message.getString("filter_type.enum.flow"), // Flow control statement
      new Class<?>[] {
        jp.riken.kscope.language.Return.class,
        jp.riken.kscope.language.Break.class,
        jp.riken.kscope.language.Continue.class,
        jp.riken.kscope.language.GoTo.class,
        jp.riken.kscope.language.Termination.class
      }),
  /** Directive statement: OpenMP */
  DIRECTIVE_OPENML(
      Message.getString("mainmenu.view.filter.openmp"), // OpenMP
      new Class<?>[] {jp.riken.kscope.language.Directive.class}),
  /** Directive statement: OCL */
  DIRECTIVE_OCL(
      Message.getString("mainmenu.view.filter.ocl"), // OCL
      new Class<?>[] {jp.riken.kscope.language.Directive.class}),
  /** Default */
  DEFAULT(Message.getString("mainmenu.view.filter.default"), null), // Default
  /** Show all (no filter) */
  ALL(Message.getString("filter_type.enum.show-all"), null), // Show all
  /** Hide all */
  NONE(Message.getString("filter_type.enum.hide-all"), null), // Hide all
  /** Unknown */
  UNKNOWN(Message.getString("filter_type.enum.trace-unknown"), null); // Trace: Unknown

  /** Filter name */
  private String name;
  /** Filter class */
  private Class<?>[] filterClass;

  /**
   * Constructor
   *
   * @param name Filter name
   * @param filterClass Filter class
   */
  private FILTER_TYPE(String name, Class<?>[] filterClass) {
    this.name = name;
    this.filterClass = filterClass;
  }

  /**
   * Get the filter name
   *
   * @return filter name
   */
  public String getName() {
    return this.name;
  }

  /**
   * Get the filter class
   *
   * @return filter class
   */
  public Class<?>[] getFilterClass() {
    return this.filterClass;
  }

  /**
   * Check if the Fortran class and filter match
   *
   * @param node node object
   * @return true = match
   */
  public boolean isFilter(Object node) {

    if (this == FILTER_TYPE.ALL) {
      // No filter applied (display all)
      return true;
    }

    // Filter class
    Class<?>[] filterClasses = this.getFilterClass();
    if (filterClasses == null) return true;

    // Is it a filter class?
    boolean filter = false;
    for (Class<?> subclass : filterClasses) {
      if (subclass.isInstance(node)) {
        filter = true;
        break;
      }
    }
    if (filter == false) return false;

    // SELECT, IF statement
    if (this == FILTER_TYPE.SELECTION_SELECT || this == FILTER_TYPE.SELECTION_IF) {
      jp.riken.kscope.language.Selection selection = null;
      if (node instanceof jp.riken.kscope.language.Selection) {
        selection = (jp.riken.kscope.language.Selection) node;
      } else if (node instanceof jp.riken.kscope.language.Condition) {
        jp.riken.kscope.language.Condition condition = (jp.riken.kscope.language.Condition) node;
        selection = (jp.riken.kscope.language.Selection) condition.get_mother();
      }
      if (selection != null) {
        if (this == FILTER_TYPE.SELECTION_SELECT) {
          return (selection.isSelect());
        } else if (this == FILTER_TYPE.SELECTION_IF) {
          return (selection.isIF() || (selection.isWHERE()));
        }
      }
      return false;
    } else if (this == FILTER_TYPE.DIRECTIVE_OPENML) {
      jp.riken.kscope.language.Directive directive = (jp.riken.kscope.language.Directive) node;
      String message = directive.getArgument();
      // Search for OpenMP directives
      String regex = "^omp.*";
      int flags = Pattern.CASE_INSENSITIVE + Pattern.MULTILINE;
      // Regular expression search
      Matcher m = Pattern.compile(regex, flags).matcher(message);
      return m.matches();
    } else if (this == FILTER_TYPE.DIRECTIVE_OCL) {
      jp.riken.kscope.language.Directive directive = (jp.riken.kscope.language.Directive) node;
      String message = directive.getArgument();
      // Search for OCL directives
      String regex = "^ocl.*";
      int flags = Pattern.CASE_INSENSITIVE + Pattern.MULTILINE;
      // Regular expression search
      Matcher m = Pattern.compile(regex, flags).matcher(message);
      return m.matches();
    }
    return true;
  }
}
