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

import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.information.InformationBase;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Break;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.Continue;
import jp.riken.kscope.language.DynamicAllocation;
import jp.riken.kscope.language.DynamicDeallocation;
import jp.riken.kscope.language.DynamicNullification;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.GoTo;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Pause;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Repetition;
import jp.riken.kscope.language.Return;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Termination;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.generic.IProcedureItem;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.ValidateLanguage;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.parser.IAnalyseParser;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.SwingUtils;

/**
 * A class that builds and searches a database.
 *
 * @author RIKEN
 */
public class LanguageService extends BaseService {

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = (debug && System.getenv("DEBUG").equalsIgnoreCase("high"));
  /** Read source file. */
  private SourceFile[] files = null;
  /** Fortran parsing result storage database. */
  private Fortran fortranDb = null;
  /** Fortran / XcodeML parsing parser. */
  private IAnalyseParser fortranParser = null;
  /** Structural tree model. */
  private LanguageTreeModel modelLanguage = null;
  /** Module tree model. */
  private ModuleTreeModel modelModule = null;
  /** Source file tree model. */
  private FileTreeModel modelFile = null;
  /** XML file tree model. */
  private FileTreeModel modelXml = null;
  /** Project folder */
  private File projectFolder = null;

  /** Working set to store program units already added in the tree */
  private HashSet<ProgramUnit> checkProgramUnitFlag = new HashSet<ProgramUnit>();

  /** Working set for determining circulation in tree generation */
  private HashSet<String> recursiveSub = new HashSet<String>();

  /** Working member for judging / adding blocks defined in the performance area. */
  private String regionStart;

  private int regionStartNum;
  private String regionEnd;
  private int regionEndNum;
  private InformationBase regionInfo;
  private Map<String, List<InformationBlock>> regionMap =
      new HashMap<String, List<InformationBlock>>();
  private InformationBlocks regionInfos;

  /** Thread execution flag true: Continue execution / false: Cancel. */
  private volatile boolean m_running = true;
  /** Structure tree expansion depth: Default = 2, */
  private int treeDepth = 2;
  /** Database serialization stream */
  private volatile ObjectInputStream languageStream;

  /** Constructor. */
  public LanguageService() {
    this.files = null;
    this.fortranDb = null;
    this.fortranParser = null;
  }

  /**
   * Constructor.
   *
   * @param fortran Fortran parsing result storage database
   */
  public LanguageService(Fortran fortran) {
    this.files = null;
    this.fortranDb = fortran;
    this.fortranParser = null;
  }

  /**
   * Constructor.
   *
   * @param files Read source file
   * @param fortran Fortran parsing result storage database
   * @param parser Fortran parser
   */
  public LanguageService(SourceFile[] files, Fortran fortran, IAnalyseParser parser) {
    this.files = files;
    this.fortranDb = fortran;
    this.fortranParser = parser;
  }

  /**
   * Constructor.
   *
   * @param files Read source file
   * @param fortran Fortran parsing result storage database
   */
  public LanguageService(SourceFile[] files, Fortran fortran) {
    this.files = files;
    this.fortranDb = fortran;
  }

  /**
   * Set the structure tree model.
   *
   * @param model Structural tree model
   */
  public void setLanguageTreeModel(LanguageTreeModel model) {
    this.modelLanguage = model;
  }

  /**
   * Set the module tree model.
   *
   * @param model Module tree model
   */
  public void setModuleTreeModel(ModuleTreeModel model) {
    this.modelModule = model;
  }

  /**
   * Set the source file tree model.
   *
   * @param model Source file tree model
   */
  public void setSourceTreeModel(FileTreeModel model) {
    this.modelFile = model;
  }

  /**
   * Set up an XML file tree model.
   *
   * @param model XML file tree model
   */
  public void setXmlTreeModel(FileTreeModel model) {
    this.modelXml = model;
  }

  /** Thread execution Read from the source file and perform parsing. */
  public void parseSourceFile() {
    if (this.files == null) return;
    if (this.fortranDb == null) return;
    if (this.fortranParser == null) return;
    this.fortranParser.setCancel(false);
    this.fortranParser.setConfirmInclude(true);

    // Cancel check
    if (this.isCancel()) {
      return;
    }
    // Set the project folder
    this.fortranParser.setBaseFolder(this.projectFolder);

    // Remove duplicate files from parsing files
    List<SourceFile> filelist = validFileList(this.files);
    if (filelist == null) return;

    try {
      ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
      for (SourceFile file : filelist) {
        try {
          String filename = file.toString();
          Pattern pattern = Pattern.compile("^[^\\.].*$");
          Matcher matcher = pattern.matcher(filename);
          boolean b = matcher.matches();
          if (b) {
            Application.status.setMessageStatus(filename);

            // Read the file from the source file
            fortranParser.readFile(file);

            // Parse the read code line.
            fortranParser.parseFile(fortranDb);

            // Get the original Fortran source file
            sourceFileList.add(fortranParser.getLanguageFile());

            // Get parse error
            if (fortranParser.getErrorInfos() != null) {
              this.addErrorInfos(fortranParser.getErrorInfos());
            }
          }
        } catch (LanguageException lang_ex) {
          Logger.error(lang_ex);
          Logger.error(lang_ex.getCodeInfo());

          // Set the error location information
          this.addErrorInfo(lang_ex);

          // Error message
          // String error_message = lang_ex.getMessage();
          // lang_ex.printStackTrace();
          // JOptionPane.showMessageDialog(null, error_message, "Analyse Error",
          // JOptionPane.ERROR_MESSAGE);
        }

        // Cancel check
        if (this.isCancel()) {
          return;
        }
      }

      Application.status.setProgressStart(true);
      Application.status.setMessageStatus("set source file...");

      // Set the original Fortran source file in the source tree
      // delete at 2013/03/01 by @hira
      //            ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
      //            if (modelFile != null && listLangFile.size() > 0) {
      //                for (int i=0; i<listLangFile.size(); i++) {
      //                    sourceFileList.add(new SourceFile(listLangFile.get(i)));
      //                }
      //            }

      Application.status.setMessageStatus("analysys database...");
      fortranDb.analyseDB();

      // Cancel check
      if (this.isCancel()) {
        return;
      }

      // Validate the database.
      {
        ValidateLanguage validate = new ValidateLanguage(fortranDb);
        LanguageVisitor visitor = new LanguageVisitor(validate);
        visitor.entry();
        int error = validate.analyseTypes();
        if (error > 0) {
          this.getErrorInfoModel().addErrorInfos(validate.getErrorList());
          String msg = Message.getString("validatelanguage.final.error", error);
          this.addErrorInfo(msg);
        }
      }

      // Source file list settings
      fortranDb.setSourceFileList(sourceFileList);

      // Set explorer view
      setExplorerView();

      Application.status.setProgressStart(false);
      Application.status.setMessageStatus(
          Message.getString(
              "languageservice.parsesourcefile.finalize.status")); // Structural analysis: Finished

    } catch (LanguageException lang_ex) {
      Logger.error(lang_ex);
      Logger.error(lang_ex.getCodeInfo());
      lang_ex.printStackTrace();

      String error_message = lang_ex.getMessage();

      // Set the error location information
      this.addErrorInfo(lang_ex);

      // Error message
      // JOptionPane.showMessageDialog(null, error_message, "Analyse Error",
      // JOptionPane.ERROR_MESSAGE);
    } catch (InterruptedException ex) {
      // End due to cancellation
      this.addErrorInfo(
          Message.getString("languageservice.parsesourcefile.cancel")); // End due to cancellation
      Application.status.setMessageStatus(
          Message.getString("languageservice.parsesourcefile.cancel")); // End due to cancellation

    } catch (Exception ex) {
      Logger.error(ex);
      ex.printStackTrace();

      String error_message = ex.getMessage();
      if (error_message == null) {
        error_message = ex.toString();
      }
      // Error message
      // JOptionPane.showMessageDialog(null, error_message, "Analyse Error",
      // JOptionPane.ERROR_MESSAGE);

      // Set the error location information
      this.addErrorInfo(error_message);
    }
  }

  /**
   * Check if thread execution is cancelled
   *
   * @return true = Cancel
   */
  public boolean isCancel() {
    return !this.m_running;
  }

  /** Cancel the thread execution. */
  public void cancelRunning() {

    m_running = false;
    if (fortranParser != null) {
      fortranParser.setCancel(true);
    }
    if (this.fortranDb != null) {
      this.fortranDb.setCancel(true);
    }
    try {
      if (this.languageStream != null) {
        this.languageStream.close();
        this.languageStream = null;
      }
    } catch (IOException ex) {
    }
  }

  /**
   * Register a status display listener in the parser.
   *
   * @param listener Listener for status display
   */
  public void addStatusPropertyChangeListener(PropertyChangeListener listener) {
    fortranParser.addPropertyChangeListener(listener);
  }

  /** Create a structure tree */
  public void writeTree() {
    // Load performance area
    try {
      FileReader in = new FileReader("properties/performance.ksx");
      BufferedReader br = new BufferedReader(in);
      String line;
      int cnt = 0;
      while ((line = br.readLine()) != null) {
        if (cnt == 0) {
          this.regionStart = line;
        } else if (cnt == 1) {
          this.regionStartNum = Integer.valueOf(line);
        } else if (cnt == 2) {
          this.regionEnd = line;
        } else if (cnt == 3) {
          this.regionEndNum = Integer.valueOf(line);
        } else if (cnt == 4) {
          this.regionInfo = new TextInfo(line);
        }
        cnt++;
      }
      br.close();
      in.close();
      this.regionInfos = new InformationBlocks();
    } catch (FileNotFoundException e) {
      // e.printStackTrace();
    } catch (IOException e) {
      // e.printStackTrace();
    }

    int depth = 0;
    // Set the database in the model
    this.modelLanguage.setLanguageDb(this.fortranDb);
    DefaultMutableTreeNode root = this.modelLanguage.getRootNode();
    writeTree("main", root, true, depth);
    if (this.regionInfos != null) {
      this.fortranDb.setInformationBlocks(this.regionInfos);
    }
    return;
  }

  /**
   * Create a structure tree
   *
   * @param block Root block
   * @return true = success
   */
  public boolean writeTree(IBlock block) {
    if (!(block instanceof Procedure)) {
      return false;
    }
    // Set the database in the model
    this.modelLanguage.setLanguageDb(this.fortranDb);
    DefaultMutableTreeNode root = this.modelLanguage.getRootNode();
    checkProgramUnitFlag.clear();
    if (block instanceof Procedure) {
      checkProgramUnitFlag.add((Procedure) block);

      int depth = 0;
      FilterTreeNode node = new FilterTreeNode(block, depth);
      root.add(node);
      writeBlocks(((Procedure) block).getBody(), node, true, depth + 1);
    }
    return true;
  }

  /**
   * Build the routine tree below the specified subroutine with JTree.
   *
   * @param procName Subroutine name
   * @param root Root node
   * @param flag True when searching for child nodes
   * @param depth Number of offspring nodes
   * @return Success or failure
   */
  private boolean writeTree(String procName, DefaultMutableTreeNode root, boolean flag, int depth) {
    checkProgramUnitFlag.clear();
    String name;
    if (procName.equalsIgnoreCase("main")) {
      name = fortranDb.getMainName();
    } else {
      name = procName;
    }
    Procedure proc = fortranDb.search_subroutine(name);
    checkProgramUnitFlag.add(proc);
    if (proc == null) {
      // Error message: languageservice.procedure.error = [% s] does not exist.
      String error_message = Message.getString("languageservice.procedure.error", procName);

      // Set the error location information
      this.addErrorInfo(error_message);
      return false;
    }
    FilterTreeNode node = new FilterTreeNode(proc, depth);
    root.add(node);
    writeBlocks(proc.getBody(), node, flag, depth + 1);
    return true;
  }

  /**
   * Recursively search for processing blocks and generate child nodes.
   *
   * @param block Processing block
   * @param parent parent node
   * @param flag A flag that expands the declaration of a procedure call. True to expand
   * @param depth Number of offspring nodes
   */
  public void writeBlocks(Block block, FilterTreeNode parent, boolean flag, int depth) {
    FilterTreeNode child;
    // The expansion hierarchy is 2. If the expansion hierarchy is 0 or less, all offspring nodes
    // are read.
    if (this.treeDepth > 0 && depth > this.treeDepth) return;

    List<Block> blocks = block.getBlocks();
    for (Block blk : blocks) {
      // When the target is a procedure call
      if (blk instanceof ProcedureUsage) {
        ProcedureUsage call = (ProcedureUsage) blk;
        String callName = call.getCallName();
        if (callName.equalsIgnoreCase("")) {
          continue;
        }
        child = new FilterTreeNode(call, depth);
        child = parent.add(child);
        // Check if the call statement corresponds to the performance area
        this.chechPerformance(call);
        if (flag) {
          if (call.getCallDefinition() != null) {
            Procedure proc = call.getCallDefinition();
            FilterTreeNode procNode = new FilterTreeNode(proc, depth + 1);
            procNode = child.add(procNode);
            boolean flagToWriteOnce =
                false; // true if only the first declaration of procedure is generated
            if (flagToWriteOnce) {
              if (this.checkProgramUnitFlag.contains(proc)) {
                continue;
              }
              this.checkProgramUnitFlag.add(proc);
            }
            // Check circulation
            if (SwingUtils.recursiveTreeNode(procNode)) {
              continue;
            } else if (this.recursiveSub.contains(callName)) {
              continue;
            } else {
              Application.status.setMessageStatus("writeBlock " + callName + "...");
              recursiveSub.add(callName);
              writeBlocks(proc.getBody(), procNode, flag, depth + 2);
              recursiveSub.remove(callName);
            }
          }
        }
      } else if (blk instanceof Selection) {
        Selection selec = (Selection) blk;
        child = new FilterTreeNode(selec, depth);
        child = parent.add(child);
        // For SELECT statement
        if (selec.isSelect()) {
          for (Condition cond : selec.getConditions()) {
            FilterTreeNode child2 = new FilterTreeNode(cond, depth + 1);
            child2 = child.add(child2);
            writeBlocks(cond, child2, flag, depth + 2);
          }
          // For IF, WHERE statements
        } else {
          Block cond0 = selec.getConditions().get(0);
          writeBlocks(cond0, child, flag, depth + 1);
          for (int j = 1; j < selec.getConditions().size(); j++) {
            Condition cond = selec.getConditions().get(j);
            child = new FilterTreeNode(cond, depth);
            child = parent.add(child);
            writeBlocks(cond, child, flag, depth + 1);
          }
        }
      } else if (blk instanceof Substitution) {
        child = new FilterTreeNode(blk, depth);
        child = parent.add(child);
        int childdepth = depth + 1;
        // If there is a function call of the assignment statement, be sure to insert it.
        if (childdepth > this.treeDepth) {
          List<Block> childblocks = block.getBlocks();
          if (childblocks != null && childblocks.size() > 0) {
            childdepth = this.treeDepth;
          }
        }
        writeBlocks(
            blk,
            parent,
            flag,
            childdepth); // Make the assignment statement function call a child element of parent
      } else {
        child = new FilterTreeNode(blk, depth);
        child = parent.add(child);
        writeBlocks(blk, child, flag, depth + 1);
      }
    }
  }

  /**
   * Recursively search for processing blocks and generate child nodes.
   *
   * @param proc processing block
   * @param parent parent node
   * @param flag A flag that expands the declaration of a procedure call. True to expand
   * @param depth Number of offspring nodes
   */
  public void writeProcedure(Procedure proc, FilterTreeNode parent, boolean flag, int depth) {
    if (!flag) return;

    String callName = proc.get_name();
    boolean flagToWriteOnce = false; // true if only the first declaration of procedure is generated
    if (flagToWriteOnce) {
      if (this.checkProgramUnitFlag.contains(proc)) {
        return;
      }
      this.checkProgramUnitFlag.add(proc);
    }
    // Check circulation
    if (SwingUtils.recursiveTreeNode(parent)) {
      return;
    }
    if (this.recursiveSub.contains(callName)) {
      return;
    } else {
      recursiveSub.add(callName);
      writeBlocks(proc.getBody(), parent, flag, depth + 1);
      recursiveSub.remove(callName);
    }
  }

  /**
   * Recursively search for processing blocks and generate child nodes.
   *
   * @param call processing block
   * @param parent parent node
   * @param flag A flag that expands the declaration of a procedure call. True to expand
   * @param depth Number of offspring nodes
   */
  public void writeProcedureUsage(
      ProcedureUsage call, FilterTreeNode parent, boolean flag, int depth) {
    String callName = call.getCallName();
    if (callName.equalsIgnoreCase("")) {
      return;
    }
    // Check if the call statement corresponds to the performance area
    this.chechPerformance(call);
    if (flag) {
      if (call.getCallDefinition() != null) {
        Procedure proc = call.getCallDefinition();
        FilterTreeNode procNode = new FilterTreeNode(proc, depth + 1);
        procNode = parent.add(procNode);
        writeProcedure(proc, procNode, flag, depth);
      }
    }
  }

  /**
   * Recursively search for processing blocks and generate child nodes.
   *
   * @param select processing block
   * @param parent parent node
   * @param flag A flag that expands the declaration of a procedure call. True to expand
   * @param depth Number of offspring nodes
   */
  public void writeSelection(Selection select, FilterTreeNode parent, boolean flag, int depth) {
    // For SELECT statement
    if (select.isSelect()) {
      for (Condition cond : select.getConditions()) {
        FilterTreeNode child2 = new FilterTreeNode(cond, depth + 1);
        child2 = parent.add(child2);
        writeBlocks(cond, child2, flag, depth + 2);
      }
      // For IF, WHERE statements
    } else {
      Block cond0 = select.getConditions().get(0);
      writeBlocks(cond0, parent, flag, depth + 1);
    }
  }

  /**
   * Check if the specified procedure call defines a performance area and Add if applicable.
   *
   * @param call Procedure call
   */
  private void chechPerformance(ProcedureUsage call) {
    if (call.getCallName().equalsIgnoreCase(this.regionStart)) {
      String key = call.getArguments().get(this.regionStartNum - 1).toString();
      if (key != null) {
        InformationBlock blk = new InformationBlock(this.regionInfo, call, null);
        List<InformationBlock> blks = this.regionMap.get(key);
        if (blks == null) {
          blks = new ArrayList<InformationBlock>();
          this.regionMap.put(key, blks);
        }
        blks.add(blk);
      }
    } else if (call.getCallName().equalsIgnoreCase(this.regionEnd)) {
      String key = call.getArguments().get(this.regionEndNum - 1).toString();
      if (key != null) {
        List<InformationBlock> blks = this.regionMap.get(key);
        if (blks != null) {
          InformationBlock current = blks.get(blks.size() - 1);
          current.setEndBlock(call);
          this.regionInfos.add(current);
          blks.remove(blks.size() - 1);
        }
      }
    }
  }

  /** Create a module tree. */
  public void setFortranModules() {

    // Set the database in the model
    this.modelModule.setLanguageDb(this.fortranDb);
    DefaultMutableTreeNode root = this.modelModule.getRootNode();

    String[] moduleName = this.fortranDb.get_module_name();
    Arrays.sort(
        moduleName,
        new Comparator<String>() {
          @Override
          public int compare(String obj0, String obj1) {
            return obj0.compareTo(obj1);
          }
        });
    Module currentModule = this.fortranDb.module("NO_MODULE");
    DefaultMutableTreeNode child = new DefaultMutableTreeNode(currentModule);
    root.add(child);
    Procedure[] subs = currentModule.get_procedures();
    // Add main to the node first.
    List<Procedure> subsList = new ArrayList<Procedure>(Arrays.asList(subs));
    for (Procedure proc : subsList) {
      if (proc.isProgram()) { // Is it a program statement?
        subsList.remove(proc);
        subsList.add(0, proc);
        break;
      }
    }
    subs = subsList.toArray(new Procedure[0]);

    setSubroutines(child, subs);

    for (int i = 0; i < moduleName.length; i++) {
      if (!(moduleName[i].equalsIgnoreCase("NO_MODULE"))) {
        currentModule = this.fortranDb.module(moduleName[i]);
        child = new DefaultMutableTreeNode(currentModule);
        root.add(child);
        // Display use statement
        List<UseState> uses = currentModule.getUseList();
        DefaultMutableTreeNode usesNode = new DefaultMutableTreeNode("use");
        if (uses.size() > 0) {
          child.add(usesNode);
          for (UseState use : uses) {
            DefaultMutableTreeNode useNode = new DefaultMutableTreeNode(use);
            usesNode.add(useNode);
          }
        }
        // Display interface statement
        List<Procedures> interfaces = currentModule.getInterfaceList();
        for (Procedures in : interfaces) {
          DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(in);
          child.add(varChild);
          for (IProcedureItem item : in.getProcedures()) {
            DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(item);
            varChild.add(itemNode);
          }
        }
        // Display type statement
        List<Type> types = currentModule.getTypeList();
        for (Type tp : types) {
          DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(tp);
          child.add(varChild);
          for (VariableDefinition item : tp.getDefinitions()) {
            DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(item);
            varChild.add(itemNode);
          }
        }
        // Display common statement
        DefaultMutableTreeNode comRoot = new DefaultMutableTreeNode("common");
        List<Common> comList = currentModule.getCommonList();
        if (comList.size() > 0) {
          child.add(comRoot);
          for (Common cm : comList) {
            DefaultMutableTreeNode comNode = new DefaultMutableTreeNode(cm);
            comRoot.add(comNode);
          }
        }
        // Display TODO equivalence statement
        // Display TODO data statement
        VariableDefinition[] vars = currentModule.get_variables();
        for (int j = 0; j < vars.length; j++) {
          DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(vars[j]);
          child.add(varChild);
        }
        subs = currentModule.get_procedures();
        setSubroutines(child, subs);
      }
    }
  }

  /**
   * Add a procedure node
   *
   * @param child Additional node
   * @param subs Procedure element
   */
  private void setSubroutines(DefaultMutableTreeNode child, Procedure[] subs) {
    if (subs == null) {
      return;
    }
    for (int i = 0; i < subs.length; i++) {
      DefaultMutableTreeNode child2 = new DefaultMutableTreeNode(subs[i]);
      child.add(child2);
      // Display use statement
      List<UseState> uses = subs[i].getUseList();
      DefaultMutableTreeNode usesNode = new DefaultMutableTreeNode("use");
      if (uses.size() > 0) {
        child2.add(usesNode);
        for (UseState use : uses) {
          DefaultMutableTreeNode useNode = new DefaultMutableTreeNode(use);
          usesNode.add(useNode);
        }
      }
      // Display interface statement
      List<Procedures> interfaces = subs[i].getInterfaceList();
      for (Procedures in : interfaces) {
        DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(in);
        child2.add(varChild);
        for (IProcedureItem item : in.getProcedures()) {
          DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(item);
          varChild.add(itemNode);
        }
      }
      // Display common statement
      List<Common> comList = subs[i].getCommonList();
      DefaultMutableTreeNode comRoot = new DefaultMutableTreeNode("common");
      if (comList.size() > 0) {
        child.add(comRoot);
        for (Common cm : comList) {
          DefaultMutableTreeNode comNode = new DefaultMutableTreeNode(cm);
          child2.add(comNode);
        }
      }
      VariableDefinition[] vars = subs[i].get_variables();
      for (int j = 0; j < vars.length; j++) {
        DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(vars[j]);
        child2.add(varChild);
      }
      Procedure[] subChildren = subs[i].get_children();
      setSubroutines(child2, subChildren);
    }
  }

  /**
   * Output the specified procedure as text from the Fortran database.
   *
   * @param file Output file
   * @param blocks Output procedure list
   */
  public void exportLanguage(File file, IBlock[] blocks) {
    // Output procedure check
    List<String> procs = new ArrayList<String>();
    if (blocks != null) {
      for (IBlock block : blocks) {
        if (block instanceof Procedure && ((Procedure) block).get_name() != null) {
          procs.add(((Procedure) block).get_name());
        }
      }
    }

    // Since the output procedure does not exist, search from the root
    if (procs.size() <= 0) {
      if (this.fortranDb.getMainName() != null) {
        procs.add(this.fortranDb.getMainName());
      }
    }
    if (procs.size() <= 0) {
      // Error message
      // languageservice.exportlanguage.procedure.error = Output procedure does not exist.
      // languageservice.error = error
      JOptionPane.showMessageDialog(
          null,
          Message.getString(
              "languageservice.exportlanguage.procedure.error"), // The output procedure does not
                                                                 // exist.
          Message.getString("languageservice.error"), // error
          JOptionPane.ERROR_MESSAGE);
    }

    try {
      FileOutputStream fos = new FileOutputStream(file);
      OutputStreamWriter osw = new OutputStreamWriter(fos);
      BufferedWriter bw = new BufferedWriter(osw);
      StringBuffer buf = new StringBuffer();
      for (String proc : procs) {
        String text = writeText(proc);
        buf.append(text);
        buf.append("\n");
      }
      bw.write(buf.toString());
      bw.close();
      osw.close();
      fos.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Output the routine tree under the specified subroutine as text
   *
   * @param procName Procedure name
   * @return Tree text representation
   */
  private String writeText(String procName) {

    checkProgramUnitFlag.clear();

    Procedure proc = this.fortranDb.search_subroutine(procName);
    boolean flag = true;
    StringBuilder out = new StringBuilder();
    checkProgramUnitFlag.add(proc);
    if (proc != null && proc.get_name().equals(this.fortranDb.getMainName())) {
      out.append("program " + procName + "\n");
      writeTexts(proc.getBody(), 0, flag, out);
      out.append("end program\n");
      return out.toString();
    } else if (proc == null) {
      // Error message: languageservice.procedure.error = [% s] does not exist.
      JOptionPane.showMessageDialog(
          null,
          Message.getString("languageservice.procedure.error", procName),
          Message.getString("languageservice.error"),
          JOptionPane.ERROR_MESSAGE);
      return null;
    }
    out.append(procName + "\n");
    writeTexts(proc.getBody(), 0, flag, out);
    out.append("end\n");
    return out.toString();
  }

  /**
   * Recursively get the processing block and output the structure as text.
   *
   * @param parent Array of target blocks
   * @param depth Tree nesting depth
   * @param flag A flag that determines whether the call destination is the output target. If true,
   *     search.
   * @param out Output String
   */
  private void writeTexts(Block parent, int depth, boolean flag, StringBuilder out) {
    StringBuilder indent = new StringBuilder("");
    for (int i = 0; i < depth; i++) {
      indent.append("|  ");
    }
    String indentString = indent.toString();
    for (Block block : parent.getBlocks()) {
      // Processing for function calls
      if (block instanceof ProcedureUsage) {
        ProcedureUsage call = (ProcedureUsage) block;
        String callName = call.getCallName();
        out.append(indentString + "+-" + call + "\n");
        // If the flag is true, execute the process for the calling procedure
        if (flag) {
          if (call.getCallDefinition() != null) {
            Procedure proc = call.getCallDefinition();
            out.append(indentString + "|  " + proc + "\n");
            boolean flagToWriteOnce =
                false; // true if only the first declaration of procedure is generated
            if (flagToWriteOnce) {
              if (this.checkProgramUnitFlag.contains(proc)) {
                continue;
              }
              this.checkProgramUnitFlag.add(proc);
            }
            // Check circulation
            if (this.recursiveSub.contains(callName)) {
              continue;
            } else {
              this.recursiveSub.add(callName);
              writeTexts(proc.getBody(), depth + 1, flag, out);
              recursiveSub.remove(callName);
            }
            out.append(indentString + "|  end\n");
          }
        }
        // Processing for branch
      } else if (block instanceof Selection) {
        // Corresponds to the Expression of the TODO conditional expression
        Selection selec = (Selection) block;
        if (selec.isSelect()) {
          out.append(indentString + "T-" + selec + "\n");
          for (Condition cond : selec.getConditions()) {
            out.append(indentString + "+-" + cond + "\n");
            writeTexts(cond, depth + 2, flag, out);
          }
          out.append(indentString + "V-endselect\n");
        } else {
          out.append(indentString + "T-" + selec + "\n");
          writeTexts(selec.getConditions().get(0), depth + 1, flag, out);
          for (int j = 1; j < selec.getConditions().size(); j++) {
            out.append(indentString + "+-" + selec.getConditions().get(j) + "\n");
            writeTexts(selec.getConditions().get(j), depth + 1, flag, out);
          }
          out.append(indentString + "V-endif\n");
        }
        // Processing for iteration
      } else if (block instanceof Repetition) {
        out.append(indentString + "T-" + block + "\n");
        writeTexts(block, depth + 1, flag, out);
        out.append(indentString + "V-enddo\n");
        // Processing for control statements that do not have child elements
      } else if (block instanceof Break
          || block instanceof GoTo
          || block instanceof Pause
          || block instanceof Return
          || block instanceof Termination
          || block instanceof Continue) {
        out.append(indentString + "+-" + block + "\n");
      } else if (block instanceof Substitution) {
        writeTexts(block, depth, flag, out);
      }
    }
    return;
  }

  /**
   * Set the properties of the node object
   *
   * @param node node object
   * @param model Property model
   */
  public void setProperties(Object node, PropertiesTableModel model) {

    List<String> items = new ArrayList<String>();
    List<String> values = new ArrayList<String>();

    if (node instanceof IBlock) {
      String[] blockItems =
          new String[] {
            Message.getString("languageservice.properties.classname"), // name of the class
            Message.getString("languageservice.properties.file"), // File
            Message.getString("languageservice.properties.linenumber"), // line number
            Message.getString("languageservice.properties.statement")
          }; // statement

      items.addAll(java.util.Arrays.asList(blockItems));

      // name of the class
      values.add(node.getClass().getName());
      // File Path
      values.add(getSourceFilePath((IBlock) node));
      // line number
      values.add(getCodeLine((IBlock) node));
      // statement
      values.add(((IBlock) node).toString());

      if (node instanceof VariableDefinition) {
        items.addAll(
            Arrays.asList(
                Message.getString("languageservice.properties.variablename"), // Variable name
                Message.getString("languageservice.properties.datatype"), // Data type
                Message.getString("languageservice.properties.attribute"))); // attribute
        // Variable name
        values.add(((VariableDefinition) node).get_name());
        // Data type
        values.add(((VariableDefinition) node).getType().toString());
        // Attribute
        String attribute = null;
        if (((VariableDefinition) node).getAttribute() != null) {
          attribute = ((VariableDefinition) node).getAttribute().toString();
        }
        values.add(attribute);
      } else if (node instanceof Continue) {
        items.add(Message.getString("languageservice.properties.label")); // label
        // Label
        String label = ((Continue) node).get_start().get_label();
        if (Statement.NO_LABEL.equalsIgnoreCase(label)) {
          label = null;
        }
        values.add(label);
      } else if (node instanceof Break) {
        items.add(Message.getString("languageservice.properties.doname")); // DO syntax name
        // DO syntax name
        String label = ((Break) node).get_start().get_label();
        if (Statement.NO_LABEL.equalsIgnoreCase(label)) {
          label = null;
        }
        values.add(label);
      } else if (node instanceof GoTo) {
        items.add(
            Message.getString("languageservice.properties.statementnumber")); // statement number
        // statement number
        String argument = ((GoTo) node).getArgument();
        values.add(argument);
      } else if (node instanceof Pause) {
        items.add(Message.getString("languageservice.properties.pausecode")); // PAUSE-CODE
        // pause-code
        String argument = ((Pause) node).getArgument();
        values.add(argument);
      } else if (node instanceof Termination) {
        items.add(Message.getString("languageservice.properties.statuscode")); // Exit code
        // Exit code
        String argument = ((Termination) node).getArgument();
        values.add(argument);
      } else if (node instanceof ProcedureUsage) {
        items.add(
            Message.getString("languageservice.properties.actualargument")); // actual argument
        // Actual argument
        List<Expression> args = ((ProcedureUsage) node).getArguments();
        StringBuffer argument = new StringBuffer();
        if (args != null) {
          int count = 0;
          for (Expression expr : args) {
            if (count > 0) argument.append(", ");
            argument.append(expr.getLine());
            count++;
          }
        }
        values.add(argument.toString());
      } else if (node instanceof Procedure) {
        items.addAll(
            Arrays.asList(
                Message.getString("languageservice.properties.dummyargument"), // Formal argument
                Message.getString("languageservice.properties.returnvalue"), // Return value
                Message.getString("languageservice.properties.attribute"), // attribute
                Message.getString("languageservice.properties.result"))); // result
        // Formal argument
        Variable[] args = ((Procedure) node).get_args();
        StringBuffer argument = new StringBuffer();
        if (args != null) {
          int count = 0;
          for (Variable var : args) {
            if (count > 0) argument.append(", ");
            argument.append(var.getName());
            count++;
          }
        }
        values.add(argument.toString());

        // Return value
        IVariableType varType = ((Procedure) node).getReturnValueType();
        StringBuffer buf = new StringBuffer();
        if (varType != null) {
          buf.append(varType.toString());
        }
        values.add(buf.toString());

        // Attribute
        buf = new StringBuffer();
        ScopeAttribute scope = ((Procedure) node).getScope();
        if (scope == ScopeAttribute.PUBLIC) {
          if (buf.length() > 0) buf.append(",");
          buf.append("public");
        }
        if (scope == ScopeAttribute.PRIVATE) {
          if (buf.length() > 0) buf.append(",");
          buf.append("private");
        }
        if (((Procedure) node).has_attribute("recursive")) {
          if (buf.length() > 0) buf.append(",");
          buf.append("recursive");
        }
        values.add(buf.toString());

        // result
        values.add(((Procedure) node).getResult());
      } else if (node instanceof DynamicNullification) {
        items.add(
            Message.getString("languageservice.properties.pointervariable")); // Pointer variable
        // Pointer variable
        List<Variable> args = ((DynamicNullification) node).getTarget();
        StringBuffer argument = new StringBuffer();
        if (args != null) {
          int count = 0;
          for (Variable expr : args) {
            if (count > 0) argument.append(", ");
            argument.append(expr.getName());
            count++;
          }
        }
        values.add(argument.toString());
      } else if (node instanceof DynamicAllocation) {
        items.add(
            Message.getString("languageservice.properties.allocationvariable")); // Assign variables
        // Allocation variable
        Map<Variable, VariableDimension> args = ((DynamicAllocation) node).getTarget();
        StringBuffer argument = new StringBuffer();
        if (args != null) {
          int count = 0;
          Set<Variable> keySet = args.keySet();
          Iterator<Variable> keyIte = keySet.iterator();
          while (keyIte.hasNext()) {
            if (count > 0) argument.append(", ");
            Variable key = keyIte.next();
            VariableDimension value = args.get(key);
            argument.append(key.toString());
            if (value != null) {
              argument.append(value.toString());
            }
            count++;
          }
        }
        values.add(argument.toString());
      } else if (node instanceof DynamicDeallocation) {
        items.add(
            Message.getString(
                "languageservice.properties.deallocationvariable")); // Discard variable
        // Discard variable
        List<Variable> args = ((DynamicDeallocation) node).getTarget();
        StringBuffer argument = new StringBuffer();
        if (args != null) {
          int count = 0;
          for (Variable var : args) {
            if (count > 0) argument.append(", ");
            argument.append(var.toString());
            count++;
          }
        }
        values.add(argument.toString());
      } else if (node instanceof Selection) {
        if (((Selection) node).isSelect()) {
          items.addAll(
              Arrays.asList(
                  Message.getString(
                      "languageservice.properties.conditional"), // Conditional expression
                  Message.getString("languageservice.properties.case"))); // CASE statement
          // Conditional expression
          Expression caseCondition = ((Selection) node).getCaseCondition();
          values.add(caseCondition.toString());
        } else if (((Selection) node).isIF()) {
          items.addAll(
              Arrays.asList(
                  Message.getString(
                      "languageservice.properties.conditional"))); // Conditional expression
        }
        StringBuffer buf = new StringBuffer();
        List<Condition> conditions = ((Selection) node).getConditions();
        if (conditions != null) {
          int count = 0;
          for (Condition cond : conditions) {
            if (count > 0) buf.append(", ");
            buf.append("(");
            if (cond.getExpression() != null) {
              buf.append(cond.conditionToString());
            } else {
              if (((Selection) node).isSelect()) {
                buf.append("default");
              } else if (((Selection) node).isIF()) {
                buf.append("else");
              }
            }
            buf.append(")");
            count++;
          }
        }
        values.add(buf.toString());
      }

      /***  for debug  at 2012/01/22 by @hira  */
      // Debug output
      // printProperties((IBlock)node, 0);
      /***  for debug  at 2012/01/22 by @hira  */
    } else {
      String[] blockItems =
          new String[] {
            Message.getString("languageservice.properties.classname"), // name of the class
            Message.getString("languageservice.properties.statement")
          }; // statement
      items.addAll(java.util.Arrays.asList(blockItems));

      // name of the class
      values.add(node.getClass().getName());
      // statement
      values.add(node.toString());
    }

    // Set in property model
    // Notification to the property panel is notified by Observer.
    model.setTitle(node.toString());
    model.setProperties(items.toArray(new String[0]), values.toArray(new String[0]));
  }

  /**
   * Get the source file path of the block
   *
   * @param node block
   * @return Source file path
   */
  private String getSourceFilePath(IBlock node) {

    if (node == null) return null;
    if (node.getStartCodeLine() == null) return null;

    // name of the class
    String classname = node.getClass().getName();

    // File
    String errMsg = null;
    if (node.getStartCodeLine() == null) {
      errMsg =
          Message.getString(
              "languageservice.sourcefilepath.block.error",
              classname); // Could not get the line block.
    }
    if (errMsg != null) {
      // Do not display an error message for variable declaration statements
      // At the time of implicit type declaration, there is no file or line number
      if (!(node instanceof VariableDefinition)) {
        this.addErrorInfo(errMsg);
      }
      return null;
    }

    //        return node.getStartCodeLine().getSourceFile().getFile().getPath();
    return node.getStartCodeLine().getStrSourceFile();
  }

  /**
   * Get the line number of the block
   *
   * @param node block
   * @return line number
   */
  private String getCodeLine(IBlock node) {

    if (node == null) return null;
    if (node.getStartCodeLine() == null) return null;

    String line = "";
    int start = node.getStartCodeLine().getStartLine();
    int end = node.getEndCodeLine().getEndLine();
    if (start > 0) {
      line = String.valueOf(start);
      if (start < end) {
        line += ":" + String.valueOf(end);
      }
    }

    return line;
  }

  /**
   * Deserialize the Language class
   *
   * @param folder Language class serialized folder
   */
  public void readLanguage(File folder) {
    // languageservice.deserialize.start.status = Language Deserialize: Start
    Application.status.setMessageStatus(
        Message.getString("languageservice.deserialize.start.status"));
    Application.status.setProgressStart(true);

    /*
     * Deserialize Language class from folder
     */
    try {
      if (!new File(folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE).exists()) {
        String error =
            Message.getString(
                "languageservice.readlanguage.notexists.database.error",
                KscopeProperties.DATABASE_FILE);
        throw new LanguageException(error);
      }
      // (2012/5/24) changed by Tomiyama
      languageStream =
          new ObjectInputStream(
              new FileInputStream(
                  folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE));
      if (debug)
        System.out.println(
            "Reading file " + folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE);
      try {
        this.fortranDb = (Fortran) languageStream.readObject();
      } catch (java.io.StreamCorruptedException ex) {
        System.err.println(
            "Error reading from "
                + folder.getPath()
                + File.separator
                + KscopeProperties.DATABASE_FILE);
        ex.printStackTrace();
      } finally {
        languageStream.close();
      }
      languageStream = null;

      this.fortranDb.analyseDB();

      // Set the source file list
      List<SourceFile> listSrc = this.fortranDb.getProcedureFileList();
      SourceFile[] sourceFiles = null;
      if (listSrc != null) {
        sourceFiles = listSrc.toArray(new SourceFile[0]);
      }

      if (sourceFiles != null && sourceFiles.length > 0) {
        this.modelFile.setSourceFile(sourceFiles);
      }

      /*
       * After deserializing the Language class, create the structure tree and module tree.
       */
      // Create in structure tree
      writeTree();
      this.modelLanguage.notifyModel();

      // Create a module tree
      setFortranModules();
      this.modelModule.notifyModel();

    } catch (LanguageException lang_ex) {
      Logger.error(lang_ex);
      Logger.error(lang_ex.getCodeInfo());
      // lang_ex.printStackTrace();
      // Set the error location information
      this.addErrorInfo(lang_ex);
      throw lang_ex;
    } catch (IOException io_ex) {
      if (!this.isCancel()) {
        Logger.error(io_ex);
        io_ex.printStackTrace();
        String error_message = io_ex.getMessage();
        if (error_message == null) {
          error_message = io_ex.toString();
        }
        // Set the error location information
        this.addErrorInfo(error_message);
      }
    } catch (Exception ex) {
      Logger.error(ex);
      ex.printStackTrace();
      String error_message = ex.getMessage();
      if (error_message == null) {
        error_message = ex.toString();
      }
      // Set the error location information
      this.addErrorInfo(error_message);
    }

    Application.status.setProgressStart(false);
    // languageservice.deserialize.done.status = Language deserialize: finished
    Application.status.setMessageStatus(
        Message.getString("languageservice.deserialize.done.status"));
  }

  /**
   * Serialize the Language class
   *
   * @param folder Language class serialized folder
   */
  public void writeLanguage(File folder) {
    // languageservice.serialize.start.status = Language Serialize: Start
    Application.status.setMessageStatus(
        Message.getString("languageservice.serialize.start.status"));
    Application.status.setProgressStart(true);

    /*
     * Serialize the Language class in the folder
     */
    try {
      // (2012/5/24) added by Tomiyama
      ObjectOutputStream oos =
          new ObjectOutputStream(
              new FileOutputStream(
                  folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE));
      oos.writeObject(this.fortranDb);
      oos.close();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }

    Application.status.setProgressStart(false);
    // languageservice.serialize.done.status = Language Serialize: Finished
    Application.status.setMessageStatus(Message.getString("languageservice.serialize.done.status"));
  }

  /**
   * Get the Fortran parsing result storage database
   *
   * @return Fortran parsing result storage database
   */
  public Fortran getFortranLanguage() {
    return fortranDb;
  }

  /**
   * Set up the Fortran parsing result storage database
   *
   * @param fDb Fortran parsing result storage database
   */
  public void setFortranLanguage(Fortran fDb) {
    this.fortranDb = fDb;
  }

  /**
   * Get the project folder
   *
   * @return project folder
   */
  public File getProjectFolder() {
    return projectFolder;
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
   * Set the source file
   *
   * @param files
   */
  public void setSourceFiles(SourceFile[] files) {
    this.files = files;
  }

  /**
   * Set up a parser
   *
   * @param parser parser
   */
  public void setPerser(IAnalyseParser parser) {
    this.fortranParser = parser;
  }

  /**
   * Check if parsing can be done
   *
   * @return true = executable
   */
  public boolean canParse() {
    if (this.files == null) return false;
    if (this.files.length <= 0) return false;
    if (this.fortranDb == null) return false;
    if (this.fortranParser == null) return false;
    if (this.projectFolder == null) return false;
    if (!this.projectFolder.exists()) return false;
    if (!this.projectFolder.isDirectory()) return false;
    if (this.modelFile == null) return false;
    if (this.modelLanguage == null) return false;

    return true;
  }

  /** Set the explorer view. */
  public void setExplorerView() {
    if (this.fortranDb == null) return;

    // Cancel check
    if (this.isCancel()) return;

    // Source file list
    List<SourceFile> listSrc = this.fortranDb.getProcedureFileList();
    SourceFile[] srcs = null;
    if (listSrc != null) {
      srcs = listSrc.toArray(new SourceFile[0]);
    }

    Application.status.setMessageStatus("creating XML List...");
    // XML file list
    SourceFile[] xmls = null;
    if (srcs != null && srcs.length > 0) {
      List<SourceFile> listXml = new ArrayList<SourceFile>();
      xmls = new SourceFile[srcs.length];
      for (int i = 0; i < srcs.length; i++) {
        if (srcs[i] != null
            && srcs[i].getRelationFile() != null
            && !listXml.contains(srcs[i].getRelationFile())) {
          listXml.add(srcs[i].getRelationFile());
        }
      }
      if (listXml.size() > 0) {
        xmls = listXml.toArray(new SourceFile[0]);
      }
    }

    // Cancel check
    if (this.isCancel()) return;
    Application.status.setMessageStatus("creating Source tree...");
    // Create a source file tree
    if (this.modelFile != null) {
      this.modelFile.clearTreeModel();
      this.modelFile.setSourceFile(srcs);
    }

    // Cancel check
    if (this.isCancel()) return;
    Application.status.setMessageStatus("creating XML tree...");
    // Create XML file tree
    if (this.modelXml != null) {
      this.modelXml.clearTreeModel();
      this.modelXml.setSourceFile(xmls);
    }

    // Cancel check
    if (this.isCancel()) return;
    Application.status.setMessageStatus("creating Language tree...");
    // Create in structure tree
    if (this.modelLanguage != null) {
      this.modelLanguage.clearTreeModel();
      writeTree();
      this.modelLanguage.notifyModel();
    }

    // Cancel check
    if (this.isCancel()) return;
    Application.status.setMessageStatus("creating Module tree...");
    // Create a module tree
    if (this.modelModule != null) {
      this.modelModule.clearTreeModel();
      setFortranModules();
      this.modelModule.notifyModel();
    }

    return;
  }

  /**
   * Remove duplicate files from the file list.
   *
   * @param list File list
   * @return Duplicate deletion file list
   */
  private List<SourceFile> validFileList(SourceFile[] list) {
    if (list == null) return null;
    List<SourceFile> results = new ArrayList<SourceFile>();
    for (SourceFile file : list) {
      if (!results.contains(file)) {
        results.add(file);
      }
    }
    if (results.size() <= 0) return null;
    return results;
  }

  /** Clear the structure tree expansion depth. */
  public void clearTreeDepth() {
    this.treeDepth = 0;
  }
}
