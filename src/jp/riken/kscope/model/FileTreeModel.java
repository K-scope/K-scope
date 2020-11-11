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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Observable;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * File tree model class
 *
 * @author RIKEN
 */
public class FileTreeModel extends Observable {

  /** File tree model */
  private DefaultTreeModel treeModel;

  /** Project folder */
  private File projectFolder = null;

  /** Initial tree title */
  private String initTreeTitle;

  /**
   * Set the source file.
   *
   * @param sourceFiles Source file list
   */
  public void setSourceFile(SourceFile[] sourceFiles) {
    if (sourceFiles == null) {
      clearTreeModel();
      return;
    }
    // Sort the source file list
    java.util.Arrays.sort(
        sourceFiles,
        new java.util.Comparator<SourceFile>() {
          public int compare(SourceFile o1, SourceFile o2) {
            if (o1 == null) return -1;
            if (o2 == null) return 1;
            SourceFile src1 = (SourceFile) o1;
            SourceFile src2 = (SourceFile) o2;
            return src1.getPath().compareTo(src2.getPath());
          }
        });

    DefaultMutableTreeNode root = null;
    File topFile = null;
    if (projectFolder != null) {
      root = new DefaultMutableTreeNode(projectFolder);
      topFile = projectFolder;
      // Check if the file is under the root folder.
      for (int i = 0; i < sourceFiles.length; i++) {
        File srcFile = null;
        if (sourceFiles[i] == null || sourceFiles[i].getFile() == null) {
          continue;
        }
        if (sourceFiles[i].getFile().isAbsolute()) {
          srcFile = sourceFiles[i].getFile();
        } else {
          srcFile =
              new File(
                  projectFolder.getAbsolutePath()
                      + File.separator
                      + sourceFiles[i].getFile().getPath());
        }
        if (!srcFile.exists()) continue;
        if (!FileUtils.isChildsFile(srcFile, projectFolder)) {
          // Since it is not a child path of the root folder, make the root folder the top level
          // folder
          root = null;
          break;
        }
      }
    }

    if (root == null) {
      // Make the root folder the top level folder
      if (sourceFiles[0] != null) {
        File srcFile = null;
        if (sourceFiles[0].getFile().isAbsolute()) {
          srcFile = sourceFiles[0].getFile();
        } else {
          srcFile =
              new File(
                  projectFolder.getAbsolutePath()
                      + File.separator
                      + sourceFiles[0].getFile().getPath());
        }
        File path_list[] = FileUtils.getPathList(srcFile);
        root = new DefaultMutableTreeNode(path_list[1].getParentFile());
        topFile = path_list[0];
      }
    }

    // create a tree node
    createNodes(root, topFile, sourceFiles);

    // Sort the file tree
    root = SwingUtils.sortTreeNode(root);

    // Create a tree model
    treeModel = new DefaultTreeModel(root);

    // Notify model changes
    notifyModel();
  }

  /**
   * Create a tree node
   *
   * @param tree Additional parent node
   * @param folder Additional folder element
   * @param sourceFiles Source file list
   */
  private void createNodes(DefaultMutableTreeNode tree, File folder, SourceFile[] sourceFiles) {
    if (tree == null || folder == null) {
      clearTreeModel();
      return;
    }
    if (!folder.isDirectory()) return;

    for (int i = 0; i < sourceFiles.length; i++) {
      File srcFile = null;
      if (sourceFiles[i] == null || sourceFiles[i].getFile() == null) {
        continue;
      }
      if (sourceFiles[i].getFile().isAbsolute()) {
        srcFile = sourceFiles[i].getFile();
      } else {
        srcFile =
            new File(
                projectFolder.getAbsolutePath()
                    + File.separator
                    + sourceFiles[i].getFile().getPath());
      }
      try {
        if (folder.getCanonicalFile().equals(srcFile.getParentFile().getCanonicalFile())) {
          DefaultMutableTreeNode subtree = new DefaultMutableTreeNode(new SourceFile(srcFile));
          if (!containsChildNode(tree, subtree)) {
            tree.add(subtree);
          }
        }
      } catch (IOException ex) {
        ex.printStackTrace();
      }
    }

    File[] files = folder.listFiles();
    if (files == null) return;
    for (int i = 0; i < files.length; i++) {
      try {
        if (files[i] == null) continue;
        if (files[i].isFile()) continue;
        if (FileUtils.isSymbolicLink(files[i])) continue;
        boolean ischild = false;
        for (SourceFile srcFile : sourceFiles) {
          if (srcFile == null || srcFile.getFile() == null) {
            continue;
          }
          File file = null;
          if (srcFile.getFile().isAbsolute()) {
            file = srcFile.getFile();
          } else {
            file =
                new File(
                    projectFolder.getAbsolutePath() + File.separator + srcFile.getFile().getPath());
          }
          if (!FileUtils.isChildsFile(file, files[i])) continue;
          ischild = true;
        }
        if (!ischild) {
          continue;
        }
        DefaultMutableTreeNode subtree = new DefaultMutableTreeNode(files[i]);
        if (!containsChildNode(tree, subtree)) {
          createNodes(subtree, files[i], sourceFiles);
          if (subtree.getChildCount() > 0) {
            tree.add(subtree);
          }
        }
      } catch (IOException ex) {
        ex.printStackTrace();
      }
    }
  }

  /**
   * Check if it is the parent path of the source file. <br>
   * Returns true if they are the same file
   *
   * @param parent parent file
   * @param sourceFiles Source file list
   * @return true = parent path
   */
  @SuppressWarnings("unused")
  private boolean isParentPath(File parent, SourceFile[] sourceFiles) {
    for (int i = 0; i < sourceFiles.length; i++) {
      File srcFile = null;
      if (sourceFiles[i] == null || sourceFiles[i].getFile() == null) {
        continue;
      }
      if (sourceFiles[i].getFile().isAbsolute()) {
        srcFile = sourceFiles[i].getFile();
      } else {
        srcFile =
            new File(
                projectFolder.getAbsolutePath()
                    + File.separator
                    + sourceFiles[i].getFile().getPath());
      }
      if (FileUtils.isEqualsFile(parent, srcFile)) {
        return true;
      }
      if (FileUtils.isEqualsFile(parent, srcFile.getParentFile())) {
        return true;
      }

      File[] path_list = FileUtils.getChildPathList(srcFile, parent);
      if (path_list != null) return true;
    }

    return false;
  }

  /**
   * Get all source files.
   *
   * @return Source file list
   */
  public SourceFile[] getAllSourceFiles() {

    ArrayList<SourceFile> list = new ArrayList<SourceFile>();

    // Root node
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) treeModel.getRoot();
    if (node == null) return null;

    SourceFile[] child_list = getSourceFiles(node);
    if (child_list != null) {
      // Add source file
      list.addAll(Arrays.asList(child_list));
    }

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Get the source file of the child element under the specified node. <br>
   * Get all child and grandchild elements.
   *
   * @param node Child element search node
   * @return Source file list
   */
  private SourceFile[] getSourceFiles(DefaultMutableTreeNode node) {

    if (node == null) return null;

    ArrayList<SourceFile> list = new ArrayList<SourceFile>();

    // Add yourself
    Object obj = node.getUserObject();
    // Is it a source file object?
    if (obj instanceof SourceFile) {
      // Add source file
      list.add((SourceFile) obj);
    }

    // Search for child elements
    int count = node.getChildCount();
    for (int i = 0; i < count; i++) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(i);
      Object child_obj = child.getUserObject();
      if (child_obj == null) continue;

      // Get the source file from the child element
      SourceFile[] child_list = getSourceFiles(child);
      if (child_list != null) {
        // Add source file
        list.addAll(Arrays.asList(child_list));
      }
    }

    if (list.size() <= 0) return null;

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Delete the selected source file.
   *
   * @param paths Delete tree paths
   */
  public void deleteSelectedFiles(TreePath[] paths) {

    if (paths == null) return;
    for (int i = 0; i < paths.length; i++) {
      int count = paths[i].getPath().length;
      // Other than root
      if (count >= 2) {
        // Get the source file list of child elements.
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];
        SourceFile[] del_files = getSourceFiles(node);

        // Delete on the tree.
        MutableTreeNode parent = (MutableTreeNode) paths[i].getPath()[count - 2];
        parent.remove((MutableTreeNode) paths[i].getPath()[count - 1]);
      } else if (count == 1) {

        // For root, delete all child elements
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) treeModel.getRoot();
        node.removeAllChildren();
        break;
      }
    }
  }

  /**
   * Get the file tree model
   *
   * @return treeModel File tree model
   */
  public DefaultTreeModel getTreeModel() {
    if (this.treeModel == null) {
      DefaultMutableTreeNode root = new DefaultMutableTreeNode(this.initTreeTitle);
      treeModel = new DefaultTreeModel(root);
    }
    return treeModel;
  }

  /**
   * Set up a file tree model
   *
   * @param treeModel File tree model
   */
  public void setTreeModel(DefaultTreeModel treeModel) {
    this.treeModel = treeModel;
  }

  /**
   * Set the project folder
   *
   * @param folder Project folder
   */
  public void setProjectFolder(File folder) {
    this.projectFolder = folder;
  }

  /** Notify model changes */
  public void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /** Clear the file tree model. */
  public void clearTreeModel() {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode(this.initTreeTitle);
    treeModel = new DefaultTreeModel(root);

    // Notify model changes
    notifyModel();
  }

  /**
   * Get the initial tree title
   *
   * @return initTreeTitle Initial tree title
   */
  public String getInitTreeTitle() {
    return initTreeTitle;
  }

  /**
   * Set the initial tree title
   *
   * @param initTreeTitle Initial tree title
   */
  public void setInitTreeTitle(String initTreeTitle) {
    this.initTreeTitle = initTreeTitle;
  }

  /**
   * Output tree information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {
    // Root node
    if (this.treeModel == null) return;
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.treeModel.getRoot();
    if (root == null) return;
    if (root.getChildCount() <= 0) return;

    try {
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      // Make the tree a CSV string.
      String buf = SwingUtils.toTreeText(root);
      // File output
      pw.print(buf);

      pw.close();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Get the root of the source <br>
   * A folder that contains all the source files, not the root of the tree
   *
   * @return root
   */
  public String getRootFolder() {
    if (this.treeModel == null) return "";
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.treeModel.getRoot();
    if (root == null) return "";
    // Go down the hierarchy from root, just before the first branch, or if there is no branch, the
    // parent folder at the end
    return scanSourceRoot(root);
  }

  private String scanSourceRoot(DefaultMutableTreeNode node) {
    if (node == null) return "";
    if (node.getChildCount() > 1) return node.toString();
    DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(0);
    if (child.isLeaf()) return node.toString();
    return scanSourceRoot((DefaultMutableTreeNode) node.getChildAt(0));
  }

  /**
   * Check if the same node exists in the child node.
   *
   * @param parent parent node
   * @param child child node
   * @return true = The same node exists.
   */
  private boolean containsChildNode(DefaultMutableTreeNode parent, DefaultMutableTreeNode child) {
    if (parent == null) return false;
    if (child == null) return false;
    if (parent.getChildCount() <= 0) return false;
    Object childObj = child.getUserObject();
    if (childObj == null) return false;

    int count = parent.getChildCount();
    if (childObj instanceof SourceFile) {
      SourceFile childFile = (SourceFile) childObj;
      for (int i = 0; i < count; i++) {
        Object obj = ((DefaultMutableTreeNode) (parent.getChildAt(i))).getUserObject();
        SourceFile file = null;
        if (obj != null && obj instanceof SourceFile) {
          file = (SourceFile) obj;
        }
        if (childFile.equals(file)) {
          return true;
        }
      }
      return false;
    } else if (childObj instanceof File) {
      File childFile = (File) childObj;
      for (int i = 0; i < count; i++) {
        Object obj = ((DefaultMutableTreeNode) (parent.getChildAt(i))).getUserObject();
        File file = null;
        if (obj != null && obj instanceof File) {
          file = (File) obj;
        }
        if (childFile.equals(file)) {
          return true;
        }
      }
      return false;
    }
    return false;
  }
}
