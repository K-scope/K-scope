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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import jp.riken.kscope.language.Expression;

/**
 * A class that indicates the data type of a variable. <br>
 *
 * @author RIKEN
 */
public class VariableType implements Serializable, jp.riken.kscope.language.IVariableType {

  /** Serial number */
  private static final long serialVersionUID = -7257691935972191261L;

  /**
   * An enum that indicates the basic data type.
   *
   * @author RIKEN
   */
  public enum PrimitiveDataType {
    /** Indefinite. */
    UNKOWN("Unkown"),
    /** Untyped (subroutine type). */
    VOID(""),
    /** byte type. */
    BYTE("byte"),
    /** integer type. */
    INTEGER("integer"),
    /** real type. */
    REAL("real"),
    /** double precision type. */
    DOUBLE_PRECISION("double precision"),
    /** logical type. */
    LOGICAL("logical"),
    /** complex type. */
    COMPLEX("complex"),
    /** double complex type. */
    DOUBLE_COMPLEX("double complex"),
    /** character type. */
    CHARACTER("character"),
    /** type type. */
    TYPE("type"),
    /** structure type. */
    STRUCTURE("structure"),
    /** union type. */
    UNION("union");

    /** Data type name */
    private String name = "";

    /**
     * Constructor.
     *
     * @param nm Model name
     */
    private PrimitiveDataType(String nm) {
      this.name = nm;
    }

    /**
     * Get the type name.
     *
     * @return type name
     */
    public String getName() {
      return name;
    }

    /**
     * Search for VariableType by type name.
     *
     * @param typeName Model name
     * @return VariableType corresponding to the type name
     */
    public static PrimitiveDataType findTypeBy(String typeName) {
      PrimitiveDataType result = PrimitiveDataType.UNKOWN;
      for (PrimitiveDataType type : PrimitiveDataType.values()) {
        if (type.getName().equalsIgnoreCase(typeName)) {
          result = type;
          break;
        }
      }
      return result;
    }
  }

  /** Basic data type */
  private PrimitiveDataType primitiveDataType = PrimitiveDataType.UNKOWN;
  /** KIND attribute value */
  private Expression kind = null;
  /** LEN attribute value */
  private Expression len = null;
  /** TYPE statement */
  private Type type = null;
  /** STRUCTURE statement */
  private Structure structure = null;
  /** UNION statement */
  private Union union = null;

  /**
   * Constructor.
   *
   * @param typ Basic data type
   */
  public VariableType(PrimitiveDataType typ) {
    this.primitiveDataType = typ;
  }

  /**
   * Constructor.
   *
   * @param value type type definition
   */
  public VariableType(Type value) {
    this.primitiveDataType = PrimitiveDataType.TYPE;
    type = value;
  }

  /**
   * Constructor.
   *
   * @param value Structure type definition
   */
  public VariableType(Structure value) {
    this.primitiveDataType = PrimitiveDataType.STRUCTURE;
    structure = value;
  }

  /**
   * Constructor.
   *
   * @param value Union type definition
   */
  public VariableType(Union value) {
    this.primitiveDataType = PrimitiveDataType.UNION;
    union = value;
  }

  /**
   * Get the type name.
   *
   * @return Type name. <br>
   *     However, if the basic data type is structure or type, <br>
   *     Returns the structure or type name.
   */
  @Override
  public String getName() {
    String result = "";
    if (this.primitiveDataType == PrimitiveDataType.TYPE) {
      if (type != null) {
        result = type.getName();
      }
    } else if (this.primitiveDataType == PrimitiveDataType.STRUCTURE) {
      if (structure != null) {
        result = structure.getName();
      }
    } else {
      result = primitiveDataType.getName();
    }
    return result;
  }

  /**
   * Get basic data type.
   *
   * @return Basic data type
   */
  public PrimitiveDataType getPrimitiveDataType() {
    return primitiveDataType;
  }

  /**
   * Get kind value.
   *
   * @return kind value
   */
  public Expression getKind() {
    return kind;
  }

  /**
   * Kind value setting.
   *
   * @param value kind value
   */
  public void setKind(Expression value) {
    kind = value;
  }

  /**
   * Get len value.
   *
   * @return len value
   */
  public Expression getLen() {
    return len;
  }

  /**
   * Setting the len value.
   *
   * @param value len value
   */
  public void setLen(Expression value) {
    len = value;
  }

  /**
   * type Get type.
   *
   * @return type value
   */
  public Type getType() {
    return type;
  }

  /**
   * type type setting.
   *
   * @param type Value of type
   */
  public void setType(Type type) {
    this.type = type;
  }

  /**
   * Get structure type.
   *
   * @return structure type value
   */
  public Structure getStructure() {
    return structure;
  }

  /**
   * Get union type.
   *
   * @return union type value
   */
  public Union getUnion() {
    return union;
  }

  /**
   * Search for VariableType by type name.
   *
   * @param typeName Model name
   * @return VariableType corresponding to the type name
   */
  @Override
  public VariableType findTypeBy(String typeName) {
    VariableType result = new VariableType(PrimitiveDataType.UNKOWN);
    result.primitiveDataType = PrimitiveDataType.findTypeBy(typeName);
    return result;
  }

  /**
   * Whether the type is compatible. <br>
   * When searching for the corresponding function from the overloaded function group, <br>
   * It is necessary to check the type of formal and actual arguments. <br>
   * "Matching" means that this type check determines that the type is the same.
   *
   * @param value Type
   * @return true: Conforms <br>
   *     false: Not compatible
   */
  @Override
  public boolean matches(jp.riken.kscope.language.IVariableType value) {
    if (!(value instanceof VariableType)) {
      return false;
    }

    boolean result = false;
    VariableType targetType = (VariableType) value;

    if (targetType.getPrimitiveDataType() == this.primitiveDataType) {
      switch (this.primitiveDataType) {
        case STRUCTURE:
          if (this.structure == null) {
            break;
          }
          result = this.structure.matches(targetType.getStructure());
          break;
        case TYPE:
          if (this.type == null) {
            break;
          }
          result = this.type.matches(targetType.getType());
          break;
        case UNION:
          break;
        default:
          if (this.kind == null) {
            if (targetType.getKind() == null) {
              result = true;
            } else {
              result = false;
            }
          } else {
            if (targetType.getKind() == null) {
              result = false;
            } else {
              // If TODO kind is represented by a variable, it may return false at runtime even
              // though it is the same kind.
              result = (targetType.getKind().toString().equals(this.kind.toString()));
            }
          }
          break;
      }
    }
    return result;
  }

  @Override
  public String toString() {
    StringBuffer buf = new StringBuffer();
    if (primitiveDataType != null) {
      buf.append(this.getName());
      if (kind != null) {
        buf.append("(" + kind + ")");
      }
      if (len != null) {
        buf.append("(len=" + len + ")");
      }
    }

    return buf.toString();
  }

  /**
   * Check if it is a real variable.
   *
   * @return true = real number
   */
  @Override
  public boolean isRealType() {
    if (this.primitiveDataType == null) return false;
    if (this.primitiveDataType == PrimitiveDataType.REAL) return true;
    if (this.primitiveDataType == PrimitiveDataType.DOUBLE_PRECISION) return true;
    if (this.primitiveDataType == PrimitiveDataType.COMPLEX) return true;
    if (this.primitiveDataType == PrimitiveDataType.DOUBLE_COMPLEX) return true;

    return false;
  }

  /**
   * Check if it is an integer variable.
   *
   * @return true = integer
   */
  @Override
  public boolean isIntegerType() {
    if (this.primitiveDataType == null) return false;
    if (this.primitiveDataType == PrimitiveDataType.INTEGER) return true;
    if (this.primitiveDataType == PrimitiveDataType.BYTE) return true;

    return false;
  }
}
