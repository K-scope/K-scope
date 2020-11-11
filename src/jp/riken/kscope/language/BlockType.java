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

package jp.riken.kscope.language;

import java.io.Serializable;

/**
 * An enum that represents the type of Block.
 *
 * @author RIKEN
 */
public enum BlockType implements Serializable {
  /** Dynamically allocate memory area. */
  DYNAMIC_ALLOCATION,
  /** Free up dynamically allocated memory area. */
  DYNAMIC_DEALLOCATION,
  /** Dynamically discard pointer reference (set pointer variable to null). */
  DYNAMIC_NULLIFICATION,
  /** Exit iterative processing. */
  BREAK,
  /** Jump to the end of the iteration. */
  CONTINUE,
  /** Exit the program. */
  TERMINATION,
  /** do nothing. */
  DO_NOTHING,
  /** Pause. */
  PAUSE,
  /** Moving. */
  GOTO,
  /** Directive */
  DIRECTIVE,
  /** Give initial value */
  DATA,
  /** Give storage sharing within a program unit */
  EQUIVALENCE,
  /** Give storage sharing between program units */
  COMMON,
  /** List of generic types */
  PROCEDURES,
  /** Branch */
  SELECTION,
  /** Conditional block */
  CONDITION,
  /** Execution statement */
  BODY,
  /** module */
  MODULE,
  /** Procedure */
  PROCEDURE,
  /** Call procedure */
  PROCEDUREUSAGE,
  /** Iterate */
  REPETITION,
  /** Return */
  RETURN,
  /** Assignment statement */
  SUBSTITUTION,
  /** User-defined */
  USERDEFINITION,
  /** Variable declaration */
  VARIABLEDEFINITION,
  /** USE statement */
  USE,
  /** Structure */
  TYPE,
  /** Unknown */
  UNKNOWN;
}
