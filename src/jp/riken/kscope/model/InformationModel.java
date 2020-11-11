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

package jp.riken.kscope.model;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Additional information model class
 *
 * @author RIKEN
 */
public class InformationModel extends Observable implements PropertyChangeListener {

  /** Additional information list */
  List<InformationNode> listInformation;

  /** External tool settings properties */
  private ProgramProperties propertiesProgram;

  /** Project folder */
  private File projectFolder;

  /** Title */
  private String title;

  /**
   * Additional information node class.
   *
   * @author RIKEN
   */
  private class InformationNode {
    /** Additional information setting start node */
    private IInformation startNode;
    /** Additional information setting end node */
    private IInformation endNode;
    /** Additional information */
    private TextInfo info;

    /**
     * Constructor
     *
     * @param node Additional information setting statement
     * @param info Additional information
     */
    @SuppressWarnings("unused")
    public InformationNode(IInformation node, TextInfo info) {
      super();
      this.startNode = node;
      this.endNode = node;
      this.info = info;
    }
    /**
     * Constructor
     *
     * @param snode Additional information setting start statement
     * @param enode Additional information setting end statement
     * @param info Additional information
     */
    public InformationNode(IInformation snode, IInformation enode, TextInfo info) {
      super();
      this.startNode = snode;
      this.endNode = enode;
      this.info = info;
    }

    /**
     * Get additional information setting node
     *
     * @return Additional information setting node
     */
    public IInformation getNode() {
      return this.startNode;
    }
    /**
     * Get the additional information setting start node
     *
     * @return Additional information setting start node
     */
    public IInformation getStartNode() {
      return this.startNode;
    }
    /**
     * Get the additional information setting end node
     *
     * @return Additional information setting end node
     */
    public IInformation getendNode() {
      return this.endNode;
    }

    /**
     * Get additional information
     *
     * @return Additional information
     */
    public TextInfo getInfo() {
      return info;
    }

    /**
     * Set additional information
     *
     * @param info Additional information
     */
    public void setInfo(TextInfo info) {
      this.info = info;
    }
  }

  /** Notify model changes */
  private void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get the title.
   *
   * @return Additional information title
   */
  public String getTitle() {
    return this.title;
  }

  /**
   * Set the title.
   *
   * @param title Additional information title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Get the number of additional information lists
   *
   * @return Number of additional information lists
   */
  public int getInformationListCount() {
    if (listInformation == null) return 0;
    return listInformation.size();
  }

  /**
   * Get the additional information setting node of the specified index from the additional
   * information list
   *
   * @param index index
   * @return Additional information setting node
   */
  public IInformation getInformationNode(int index) {
    if (this.listInformation == null) return null;
    if (this.listInformation.size() <= index) return null;
    return listInformation.get(index).getNode();
  }

  /**
   * Get additional information of the specified index from the additional information list
   *
   * @param index index
   * @return Additional information
   */
  public TextInfo getInformationInfo(int index) {
    if (this.listInformation == null) return null;
    if (this.listInformation.size() <= index) return null;
    return listInformation.get(index).getInfo();
  }

  /**
   * Get additional information content
   *
   * @param index Additional information list index
   * @return Additional Information: Content
   */
  public String getInformationContent(int index) {
    if (listInformation == null) return null;
    if (listInformation.size() <= index) return null;
    if (listInformation.get(index) == null) return null;
    if (listInformation.get(index).getInfo() == null) return null;
    return listInformation.get(index).getInfo().getContent();
  }

  /**
   * Get the HTML content of additional information
   *
   * @param index Additional information list index
   * @return Additional information: HTML content
   */
  public String getInformationHtmlContent(int index) {
    if (listInformation == null) return null;
    if (listInformation.size() <= index) return null;
    if (listInformation.get(index) == null) return null;
    if (listInformation.get(index).getInfo() == null) return null;

    if (propertiesProgram == null) {
      return listInformation.get(index).getInfo().getContent();
    }

    // Get the external tool setting list
    List<Program> list = this.propertiesProgram.getListProgram();
    if (list == null || list.size() <= 0) {
      return listInformation.get(index).getInfo().getContent();
    }

    // Additional information string
    String content = listInformation.get(index).getInfo().getContent();

    // Convert external tool files to anchor tags
    content = createHtmlContent(content);

    return content;
  }

  /**
   * Convert strings to HTML anchor strings
   *
   * @param content Additional information
   * @return HTML string
   */
  public String createHtmlContent(String content) {

    // Convert external tool files to anchor tags
    content = replaceAnchorTag(content);

    // Replace newline characters, whitespace, and tabs with HTML code
    content = StringUtils.textTohtml(content);

    content = "<html><body>\n" + content;
    content = content + "\n</body></html>";

    return content;
  }

  /**
   * Replace with anchor tag by external tool setting
   *
   * @param content Additional information
   * @return Anchor tag replacement additional information
   */
  private String replaceAnchorTag(String content) {

    StringTokenizer st = new StringTokenizer(content, " \t\n\r\f\"　", true);
    List<String> list = new ArrayList<String>();
    boolean openquot = false;
    StringBuffer quotbuf = new StringBuffer();
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      if ("\"".equals(token)) {
        if (openquot) {
          list.add(quotbuf.toString());
          quotbuf = new StringBuffer();
        }
        list.add(token);
        openquot = !openquot;
      } else {
        if (openquot) {
          quotbuf.append(token);
        } else {
          list.add(token);
        }
      }
    }

    // Replace with anchor tag by external tool setting
    StringBuffer buf = new StringBuffer();
    for (String word : list) {
      // Get the external tool setting list
      List<Program> programs = this.propertiesProgram.getListProgram();
      if (programs == null || programs.size() <= 0) break;

      String atag = null;
      for (Program prog : programs) {
        // External tool program
        String program = null;
        String option = null;
        if (!prog.isRelation()) {
          // Since it is not an association, set the external program name
          program = prog.getExename();
          if (prog.getOption() != null && !prog.getOption().isEmpty()) {
            option = prog.getOption();
          }
        }

        // Regular expressions
        if (prog.isRegex()) {
          atag = createAnchorTag(word, prog.getPattern(), program, option);
        } else {
          // extension
          int count = prog.getPatternExtsCount();
          for (int i = 0; i < count; i++) {
            String ext = prog.getPatternExt(i);
            // Search for extension files Regular expression
            String pattern = "^.+\\." + ext + "$";
            atag = createAnchorTag(word, pattern, program, option);
            if (atag != null) break;
          }
        }
        if (atag != null) break;
      }
      if (atag != null) buf.append(atag);
      else buf.append(word);
    }

    return buf.toString();
  }

  /**
   * Enclose the external tool configuration file in an anchor tag. <br>
   * Returns null if it is not an external tool configuration file.
   *
   * @param content Additional information
   * @param regex startup file regular expression
   * @param program External program
   * @param option Boot option
   * @return Anchor tag conversion additional information
   */
  private String createAnchorTag(String content, String regex, String program, String option) {

    if (content == null || content.isEmpty() || content.trim().isEmpty()) {
      return null;
    }

    // Regular expression matching
    java.util.regex.Pattern pattern =
        java.util.regex.Pattern.compile(
            regex, java.util.regex.Pattern.CASE_INSENSITIVE + java.util.regex.Pattern.MULTILINE);
    Matcher m = pattern.matcher(content);
    if (!m.find()) return null;

    // Insert anchor tag
    String file = null;
    int start = 0;
    int end = 0;
    if (m.groupCount() == 0) {
      file = m.group();
      start = m.start();
      end = m.end();
    } else if (m.groupCount() >= 1) {
      // If grouped, exclude the first (whole).
      file = m.group(m.groupCount());
      start = m.start(m.groupCount());
      end = m.end(m.groupCount());
    }
    file = file.trim();

    // Convert URL and file name to URL string
    String href = toURL(file);

    // href assembly
    String anchor = "<a href='" + href + "' ";

    // Set the startup program name in the class attribute
    if (program != null) {
      anchor += "class='" + program + "' ";
    }
    // Set the boot option to the comment attribute
    if (option != null) {
      anchor += "comment='" + option + "' ";
    }
    anchor += ">" + file + "</a>";

    // Replace anchor tag
    content = content.substring(0, start) + anchor + content.substring(end);

    return content;
  }

  /**
   * Convert URLs and file names to URL format strings
   *
   * @param name url or filename
   * @return URL format string
   */
  private String toURL(String name) {
    if (name == null || name.isEmpty()) return null;

    URL url = null;
    try {
      URI uri = new URI(name);

      url = uri.toURL();

    } catch (Exception e) {
      try {
        File file = new File(name);
        if (!file.isAbsolute() && projectFolder != null) {
          file = new File(projectFolder, name);
        }
        URI uri = file.toURI();
        url = uri.toURL();
      } catch (MalformedURLException e1) {
      }
    }

    if (url == null) return null;

    return url.toString();
  }

  /**
   * Export model information
   *
   * @param file Output file
   */
  public void writeFile(File file) {

    if (this.listInformation == null || this.listInformation.size() <= 0) return;

    try {
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));
      // Header output
      pw.println(Message.getString("informationmodel.file.header")); // Code, additional information

      // Output additional information
      for (InformationNode node : this.listInformation) {
        // name
        String name = node.getNode().toString();
        // Additional information
        String content = node.getInfo().getContent();
        content = content.trim();

        // Output
        pw.print(SwingUtils.escapeCsv(name));
        pw.print(",");
        pw.print(SwingUtils.escapeCsv(content));

        // Add line breaks
        pw.println();
      }

      pw.close();

    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * External tool setting property change event
   *
   * @param event Event information
   */
  @Override
  public void propertyChange(PropertyChangeEvent event) {

    // Change external tool setting properties
    if (event.getNewValue() instanceof ProgramProperties) {
      propertiesProgram = (ProgramProperties) event.getNewValue();

      // Notify the setting of the additional information list
      notifyModel();
    }
  }

  /**
   * Set external tool settings properties
   *
   * @param properties External tool settings properties
   */
  public void setPropertiesExtension(ProgramProperties properties) {
    this.propertiesProgram = properties;
  }

  /**
   * Set the project folder
   *
   * @param folder Project folder
   */
  public void setProjectFolder(File folder) {
    this.projectFolder = folder;
  }

  /**
   * Set additional information
   *
   * @param node Additional information setting node
   * @param info Additional information
   */
  public void setInformation(IInformation node, TextInfo info) {
    // Clear the model
    clearInformation();

    if (node == null) return;
    if (info == null) return;

    // Does additional information exist?
    if (info == null || info.getContent() == null || info.getContent().isEmpty()) return;

    // Add additional information
    addInformation(node, info);
  }

  /**
   * Set additional information
   *
   * @param node Additional information setting node
   */
  public void setInformation(IInformation node) {
    // Clear the model
    clearInformation();

    if (node == null) return;
    setInformation(node, node.getInformation());
  }

  /**
   * Add additional information
   *
   * @param node Additional information setting node
   */
  public void addInformation(IInformation node) {
    if (node == null) return;

    // Does additional information exist?
    TextInfo info = node.getInformation();
    if (info == null || info.getContent() == null || info.getContent().isEmpty()) return;

    // Add additional information
    addInformation(node, info);
  }

  /**
   * Add additional information
   *
   * @param node Additional information setting node
   * @param info Additional information
   */
  public void addInformation(IInformation node, TextInfo info) {
    this.addInformation(node, node, info);
  }
  /**
   * Add additional information
   *
   * @param snode Additional information setting start node
   * @param enode Additional information setting end node
   * @param info Additional information
   */
  public void addInformation(IInformation snode, IInformation enode, TextInfo info) {
    if (snode == null) return;

    // Search from the existing list of additional information
    InformationNode infonode = getInformationNode(snode, enode);
    if (infonode != null) {
      // Set additional information
      infonode.setInfo(info);
    } else {

      // It doesn't exist, so add it.
      if (this.listInformation == null) {
        this.listInformation = new ArrayList<InformationNode>();
      }
      infonode = new InformationNode(snode, enode, info);
      this.listInformation.add(infonode);
    }

    // Notify the setting of the additional information list
    notifyModel();
  }

  /**
   * Get additional information
   *
   * @param snode Setting start node
   * @param enode Setting end node
   * @return Additional information
   */
  private InformationNode getInformationNode(IInformation snode, IInformation enode) {
    if (this.listInformation == null) return null;

    for (InformationNode infonode : this.listInformation) {
      if (infonode.getStartNode() == snode) {
        if (infonode.getendNode() == null) {
          return infonode;
        } else if (infonode.getendNode() == enode) {
          return infonode;
        }
      }
    }

    return null;
  }

  /** Clear the table model. */
  public void clearInformation() {
    this.listInformation = new ArrayList<InformationNode>();
    this.title = null;

    // Notify the setting of the additional information list
    notifyModel();
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (ture: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.listInformation == null) return true;
    return (this.listInformation.size() < 1);
  }
}
