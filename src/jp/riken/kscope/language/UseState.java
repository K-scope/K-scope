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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A class that represents a reference to a module. Supports USE statements in Fortran.
 *
 * @author RIKEN
 */
public class UseState extends Block {
  /** Serial number */
  private static final long serialVersionUID = 4969852934134083633L;

  private String moduleName;
  private Set<String> onlyMember;
  private Map<String, String> translationName;
  private Map<String, String> translationNameReverse;

  @Override
  public String toString() {
    StringBuilder st = new StringBuilder(this.moduleName);
    if (this.onlyMember != null) {
      st.append(", only:");
      for (String on : this.getOnlyMember()) {
        st.append(" " + on);
      }
    }
    return st.toString();
  }
  /**
   * Returns a conversion map of variable names.
   *
   * @return Variable name conversion map <Variable name after conversion, variable name in module>.
   *     If not, null is returned.
   */
  public Map<String, String> getTranslationName() {
    return translationName;
  }
  /**
   * Returns the original variable name corresponding to the converted name.
   *
   * @param nm Converted name
   * @return Original variable name. Returns null if not found.
   */
  public String getTranslationName(String nm) {
    if (this.translationName == null) {
      return null;
    } else {
      return this.translationName.get(nm);
    }
  }
  /**
   * Get the conversion map of variable names (reverse).
   *
   * @return Variable name conversion map <Variable name in module, variable name after conversion>
   */
  public Map<String, String> getTranslationNameReverse() {
    return translationNameReverse;
  }

  /**
   * Set the conversion map of variable names.
   *
   * @param transName Variable name conversion map <Variable name after conversion, variable name in
   *     module>
   */
  @Deprecated
  public void setTranslationName(Map<String, String> transName) {
    this.translationName = transName;
  }

  /**
   * Add conversion support for variable names.
   *
   * @param transName Converted name
   * @param orgName Name in the module
   */
  public void addTranslationName(String transName, String orgName) {
    if (this.translationName == null) {
      this.translationName = new HashMap<String, String>();
      this.translationNameReverse = new HashMap<String, String>();
    }
    this.addOnlyMember(orgName);
    this.translationName.put(transName, orgName);
    this.translationNameReverse.put(orgName, transName);
  }

  /**
   * Set the name of the referenced module.
   *
   * @param name Module name
   */
  public void setModuleName(String name) {
    moduleName = name.toLowerCase();
  }

  /**
   * Get the name of the referenced module.
   *
   * @return module name
   */
  public String getModuleName() {
    return moduleName;
  }

  /**
   * If the variables / subroutines to be referenced are limited, add their names.
   *
   * @param member Variable / subroutine name
   */
  public void addOnlyMember(String member) {
    if (this.onlyMember == null) {
      this.onlyMember = new HashSet<String>();
    }
    onlyMember.add(member.toLowerCase());
  }

  /**
   * Get the variable / subroutine name to be referenced.
   *
   * @return List of variable / subroutine names. If not, it returns an empty set.
   */
  public Set<String> getOnlyMember() {
    if (this.onlyMember == null) {
      return new HashSet<String>();
    }
    return onlyMember;
  }

  /**
   * Determine if the variable / subroutine name to be referenced is held.
   *
   * @param name Variable / subroutine name
   * @return True if you keep the name
   */
  public boolean hasOnlyMember(String name) {
    if (onlyMember != null) {
      if (onlyMember.contains(name)) {
        return true;
      }
    }
    return false;
  }
  /** Determine if it has a only clause. With @return only clause: true */
  public boolean hasOnlyMember() {
    if (onlyMember == null) return false;
    if (onlyMember.size() > 0) {
      return true;
    }
    return false;
  }

  /**
   * If there is a rule to convert a given variable, return the converted name.
   *
   * @param var Variable before conversion (variable declared in the module)
   * @return Variable name. If there is no rule, the variable name before conversion is returned.
   */
  public String translation(VariableDefinition var) {
    if (var.getMother().get_name().equalsIgnoreCase(this.getModuleName())) {
      if (this.translationNameReverse != null) {
        String nm = this.translationNameReverse.get(var.get_name());
        if (nm != null) {
          return nm;
        }
      }
    }
    return var.get_name();
  }

  /**
   * If there is a rule to convert to the given variable name, the variable name before conversion
   * is returned.
   *
   * @param name Variable name after conversion
   * @return Variable name before conversion (name declared in the module). If there is no rule, the
   *     original name is returned.
   */
  public String translationReverse(String name) {
    if (this.translationName != null) {
      String nm = this.translationName.get(name);
      if (nm != null) {
        return nm;
      }
    }
    return name;
  }
  /** Get block type. @ return BlockType.USE */
  public BlockType getBlockType() {
    return BlockType.USE;
  }
}
