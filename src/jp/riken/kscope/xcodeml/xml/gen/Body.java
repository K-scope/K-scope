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

package jp.riken.kscope.xcodeml.xml.gen;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import jp.riken.kscope.xcodeml.xml.*;

/**
 * Java class for anonymous complex type.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice maxOccurs="unbounded" minOccurs="0">
 *           &lt;element ref="{}FifStatement"/>
 *           &lt;element ref="{}FdoStatement"/>
 *           &lt;element ref="{}FdoWhileStatement"/>
 *           &lt;element ref="{}FselectCaseStatement"/>
 *           &lt;element ref="{}FcaseLabel"/>
 *           &lt;element ref="{}FwhereStatement"/>
 *           &lt;element ref="{}FstopStatement"/>
 *           &lt;element ref="{}FpauseStatement"/>
 *           &lt;element ref="{}gotoStatement"/>
 *           &lt;element ref="{}FprintStatement"/>
 *           &lt;element ref="{}FallocateStatement"/>
 *           &lt;element ref="{}FdeallocateStatement"/>
 *           &lt;element ref="{}FreadStatement"/>
 *           &lt;element ref="{}FwriteStatement"/>
 *           &lt;element ref="{}FinquireStatement"/>
 *           &lt;element ref="{}FcycleStatement"/>
 *           &lt;element ref="{}FexitStatement"/>
 *           &lt;element ref="{}statementLabel"/>
 *           &lt;element ref="{}FformatDecl"/>
 *           &lt;element ref="{}FentryDecl"/>
 *           &lt;element ref="{}FcontainsStatement"/>
 *           &lt;element ref="{}exprStatement"/>
 *           &lt;element ref="{}FrewindStatement"/>
 *           &lt;element ref="{}FdataDecl"/>
 *           &lt;element ref="{}FnullifyStatement"/>
 *           &lt;element ref="{}continueStatement"/>
 *           &lt;element ref="{}FreturnStatement"/>
 *           &lt;element ref="{}FendFileStatement"/>
 *           &lt;element ref="{}FbackspaceStatement"/>
 *           &lt;element ref="{}FopenStatement"/>
 *           &lt;element ref="{}FcloseStatement"/>
 *           &lt;element ref="{}FpragmaStatement"/>
 *           &lt;element ref="{}text"/>
 *           &lt;element ref="{}FassignStatement"/>
 *           &lt;element ref="{}FpointerAssignStatement"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"fifStatementOrFdoStatementOrFdoWhileStatement"})
@XmlRootElement(name = "body")
public class Body implements IXmlNode {

  @XmlElements({
    @XmlElement(name = "FstopStatement", type = FstopStatement.class),
    @XmlElement(name = "FdoStatement", type = FdoStatement.class),
    @XmlElement(name = "FnullifyStatement", type = FnullifyStatement.class),
    @XmlElement(name = "FcaseLabel", type = FcaseLabel.class),
    @XmlElement(name = "FifStatement", type = FifStatement.class),
    @XmlElement(name = "FallocateStatement", type = FallocateStatement.class),
    @XmlElement(name = "FwriteStatement", type = FwriteStatement.class),
    @XmlElement(name = "FwhereStatement", type = FwhereStatement.class),
    @XmlElement(name = "FassignStatement", type = FassignStatement.class),
    @XmlElement(name = "FcycleStatement", type = FcycleStatement.class),
    @XmlElement(name = "FrewindStatement", type = FrewindStatement.class),
    @XmlElement(name = "FopenStatement", type = FopenStatement.class),
    @XmlElement(name = "FcloseStatement", type = FcloseStatement.class),
    @XmlElement(name = "continueStatement", type = ContinueStatement.class),
    @XmlElement(name = "FselectCaseStatement", type = FselectCaseStatement.class),
    @XmlElement(name = "FexitStatement", type = FexitStatement.class),
    @XmlElement(name = "statementLabel", type = StatementLabel.class),
    @XmlElement(name = "FreadStatement", type = FreadStatement.class),
    @XmlElement(name = "FprintStatement", type = FprintStatement.class),
    @XmlElement(name = "FendFileStatement", type = FendFileStatement.class),
    @XmlElement(name = "FentryDecl", type = FentryDecl.class),
    @XmlElement(name = "FdoWhileStatement", type = FdoWhileStatement.class),
    @XmlElement(name = "FpauseStatement", type = FpauseStatement.class),
    @XmlElement(name = "gotoStatement", type = GotoStatement.class),
    @XmlElement(name = "FformatDecl", type = FformatDecl.class),
    @XmlElement(name = "FdeallocateStatement", type = FdeallocateStatement.class),
    @XmlElement(name = "FcontainsStatement", type = FcontainsStatement.class),
    @XmlElement(name = "FbackspaceStatement", type = FbackspaceStatement.class),
    @XmlElement(name = "FreturnStatement", type = FreturnStatement.class),
    @XmlElement(name = "text", type = Text.class),
    @XmlElement(name = "FdataDecl", type = FdataDecl.class),
    @XmlElement(name = "exprStatement", type = ExprStatement.class),
    @XmlElement(name = "FinquireStatement", type = FinquireStatement.class),
    @XmlElement(name = "FpragmaStatement", type = FpragmaStatement.class),
    @XmlElement(name = "FpointerAssignStatement", type = FpointerAssignStatement.class)
  })
  protected List<IXmlNode> fifStatementOrFdoStatementOrFdoWhileStatement;

  /**
   * Gets the value of the fifStatementOrFdoStatementOrFdoWhileStatement property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the fifStatementOrFdoStatementOrFdoWhileStatement
   * property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getFifStatementOrFdoStatementOrFdoWhileStatement().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link FstopStatement } {@link
   * FdoStatement } {@link FnullifyStatement } {@link FcaseLabel } {@link FifStatement } {@link
   * FallocateStatement } {@link FwriteStatement } {@link FwhereStatement } {@link FassignStatement
   * } {@link FcycleStatement } {@link FrewindStatement } {@link FopenStatement } {@link
   * FcloseStatement } {@link ContinueStatement } {@link FselectCaseStatement } {@link
   * FexitStatement } {@link StatementLabel } {@link FreadStatement } {@link FprintStatement }
   * {@link FendFileStatement } {@link FentryDecl } {@link FdoWhileStatement } {@link
   * FpauseStatement } {@link GotoStatement } {@link FformatDecl } {@link FdeallocateStatement }
   * {@link FcontainsStatement } {@link FbackspaceStatement } {@link FreturnStatement } {@link Text
   * } {@link FdataDecl } {@link ExprStatement } {@link FinquireStatement } {@link FpragmaStatement
   * } {@link FpointerAssignStatement }
   *
   * @return IXmlNode List
   */
  public List<IXmlNode> getFifStatementOrFdoStatementOrFdoWhileStatement() {
    if (fifStatementOrFdoStatementOrFdoWhileStatement == null) {
      fifStatementOrFdoStatementOrFdoWhileStatement = new ArrayList<IXmlNode>();
    }
    return this.fifStatementOrFdoStatementOrFdoWhileStatement;
  }

  @Override
  public boolean enter(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  @Override
  public void leave(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
