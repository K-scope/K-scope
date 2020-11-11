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

package jp.riken.kscope.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.ArrayList;

/**
 * File utility class
 *
 * @author RIKEN
 */
public class FileUtils {

  /**
   * Get a list of hierarchical files in two paths. <br>
   * Returns null if the child path is not a child hierarchy of the parent path. <br>
   * Returns null if the child path and parent path are in the same hierarchy.
   *
   * @param childFile Child path
   * @param parentFile Parent path
   * @return Hierarchical file list
   */
  public static File[] getChildPathList(File childFile, File parentFile) {
    try {
      ArrayList<File> filelist = new ArrayList<File>();

      // Go back from the child file to the parent path.
      File curPath = childFile.getCanonicalFile();
      if (!curPath.isDirectory()) {
        curPath = curPath.getParentFile();
      }
      File srcPath = parentFile.getCanonicalFile();
      if (!srcPath.isDirectory()) {
        srcPath = srcPath.getParentFile();
      }

      if (curPath.equals(srcPath)) return null;

      boolean ischild = false;
      while (curPath != null) {
        filelist.add(0, curPath);
        curPath = curPath.getParentFile();
        if (curPath == null) break;
        if (curPath.equals(srcPath)) {
          ischild = true;
          break;
        }
      }

      if (!ischild) return null;
      if (filelist.size() == 0) return null;

      return (File[]) filelist.toArray(new File[0]);

    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Check if it is a descendant file
   *
   * @param childFile Search file
   * @param parentFile Parent file
   * @return true = Offspring files
   */
  public static boolean isChildsFile(File childFile, File parentFile) {
    File childPath = childFile;
    File parentPath = parentFile;
    if (childFile.isFile()) {
      childPath = childFile.getParentFile();
    }
    if (parentFile.isFile()) {
      parentPath = parentFile.getParentFile();
    }
    if (childFile.isFile()) {
      if (isEqualsFile(childPath, parentPath)) {
        return true;
      }
    }
    // Get a list of hierarchical files in two paths.
    File[] files = getChildPathList(childPath, parentFile);
    return (files != null && files.length > 0);
  }

  /**
   * Check the parent-child relationship of the path
   *
   * @param path Parent path
   * @param childPath Child path
   * @return true = parent-child path
   */
  public static boolean isChildPath(String path, String childPath) {
    if (path.equals(childPath)) {
      return true;
    }
    if (childPath.startsWith(path + File.separator)) {
      return true;
    }
    // For Windows
    else {
      String[] pathArr = path.split("\\\\");
      String[] childPathArr = childPath.split("\\\\");

      if (pathArr.length < childPathArr.length) {
        for (int i = 0; i < pathArr.length; i++) {
          if (!pathArr[i].equals(childPathArr[i])) {
            return false;
          }
        }
        return true;
      }
    }

    return false;
  }

  /**
   * Get the relative path of two paths. <br>
   * If the child path is not a child hierarchy of the parent path, the absolute path of the child
   * path is returned.
   *
   * @param childFile Child path
   * @param parentFile Parent path
   * @return Hierarchical file list
   */
  public static String getRelativePath(File childFile, File parentFile) {
    if (childFile == null) return null;
    if (parentFile == null) return childFile.getAbsolutePath();
    try {
      if (childFile.equals(parentFile)) return "." + File.separator;
      if (childFile.getCanonicalFile().equals(parentFile.getCanonicalFile()))
        return "." + File.separator;
      if (childFile.isFile()) {
        if (isEqualsFile(childFile.getParentFile(), parentFile))
          return "." + File.separator + childFile.getName();
      }
      if (!childFile.exists()) {
        return null;
      }

      // Get two passlists
      File[] paths = getChildPathList(childFile, parentFile);
      if (paths == null) {
        // not a child path
        return childFile.getAbsolutePath();
      }

      // Assemble the path hierarchy.
      StringBuffer buf = new StringBuffer();
      for (int i = 0; i < paths.length; i++) {
        buf.append(paths[i].getName());
        if (i < paths.length - 1) buf.append(File.separator);
      }

      if (childFile.isFile()) {
        buf.append(File.separator);
        buf.append(childFile.getName());
      }

      return buf.toString();
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Get a relative path
   *
   * @param path Target path
   * @param root Reference path
   * @return Relative path to the reference path of the target path
   */
  public static String getSubPath(String path, String root) {
    if (path.length() <= root.length()) {
      return "";
    }
    return path.substring(root.length() + 1);
  }

  /**
   * Convert absolute path abs_path to relative to base_path. abs_path must be subdirectory of
   * base_path.
   *
   * @param base_path
   * @param abs_path
   * @return relative path from base_path to abs_path
   */
  public static String getRelativePath(String base_path, String abs_path) throws IOException {
    // Normalize the paths
    String normalized_abs_path = new File(abs_path).getCanonicalPath();
    String normalized_base_path = new File(base_path).getCanonicalPath();
    if (normalized_base_path.charAt(normalized_base_path.length() - 1) != File.separatorChar)
      normalized_base_path = normalized_base_path + File.separator;

    // System.out.println(normalized_abs_path + " vs " + normalized_base_path);

    String relative_path = "";

    if (normalized_abs_path.indexOf(normalized_base_path) == 0) {
      relative_path = normalized_abs_path.substring(normalized_base_path.length());
    } else if (normalized_abs_path.indexOf(normalized_base_path) > 0) {
      System.err.println(
          "Something wrong with these paths: \nbase: " + base_path + "\nabs:  " + abs_path);
      throw new IOException(
          "Couldn't process these paths:\nnorm_base: "
              + normalized_base_path
              + "\nnorm_abs:  "
              + normalized_abs_path);
    }

    return relative_path;
  }

  /**
   * Get the file that combines the files of base_file and name.
   *
   * @param base_file Reference path
   * @param name file name
   * @return Combined file
   */
  public static File joinFilePath(File base_file, String name) {
    if (base_file == null || name == null) return null;

    // Check if the file name is an absolute path.
    if ((new File(name)).isAbsolute()) {
      // Since it is an absolute path, return it as it is.
      return new File(name);
    }

    // Get the relative path from the reference path
    String parent = null;
    if (base_file.isDirectory()) {
      parent = base_file.getAbsolutePath();
    } else {
      parent = base_file.getParentFile().getAbsolutePath();
    }

    // Combine files to reference path
    return new File(parent + File.separator + name);
  }

  /**
   * Replace \\ in the path with /. Absorbs the difference between Windows and Linux file
   * separators.
   *
   * @param path File Path
   * @return "/" Separator path name
   */
  public static final String replaceFileSeparator(String path) {
    if (path == null || path.isEmpty()) return null;
    return path.replace('\\', '/');
  }

  /**
   * Check if it is an absolute path
   *
   * @param path Check pass
   * @return true: absolute path
   */
  public static final boolean isAbsolutePath(String path) {
    String slash_path = replaceFileSeparator(path);
    if (slash_path == null || slash_path.isEmpty()) return false;

    if (slash_path.startsWith("/")) return true;

    // if (slash_path.indexOf (":")! = 1) // 2013/01/22 Fixed yabe
    if (slash_path.indexOf(":") == 1) return true;
    return false;
  }

  /**
   * Get the pass list from the root path.
   *
   * @param src_path Pass
   * @return Hierarchical file list
   */
  public static File[] getPathList(File src_path) {
    ArrayList<File> filelist = new ArrayList<File>();

    // Go back from the child file to the parent path.
    File curPath = src_path;
    if (!curPath.isDirectory()) {
      curPath = curPath.getParentFile();
    }

    while (curPath != null) {
      filelist.add(0, curPath);
      curPath = curPath.getParentFile();
      if (curPath == null) break;
    }
    if (filelist.size() == 0) return null;

    return (File[]) filelist.toArray(new File[0]);
  }

  /**
   * Check if the two files are the same and have the same file path.
   *
   * @param src File name 1
   * @param dest File name 2
   * @return true = 2 files are the same
   */
  public static boolean isEqualsFile(String src, String dest) {
    if (src == null || src.isEmpty()) return false;
    if (dest == null || dest.isEmpty()) return false;
    try {
      File srcFile = new File(src);
      File destFile = new File(dest);
      srcFile = srcFile.getCanonicalFile();
      destFile = destFile.getCanonicalFile();
      return srcFile.equals(destFile);

    } catch (IOException e) {
      return false;
    }
  }

  /**
   * Check if the two files are the same and have the same file path.
   *
   * @param srcFile File 1
   * @param destFile File 2
   * @return true = 2 files are the same
   */
  public static boolean isEqualsFile(File srcFile, File destFile) {
    if (srcFile == null) return false;
    if (destFile == null) return false;
    try {
      srcFile = srcFile.getCanonicalFile();
      destFile = destFile.getCanonicalFile();
      return srcFile.equals(destFile);

    } catch (IOException e) {
      return false;
    }
  }

  /**
   * Get the files under it
   *
   * @param dir Search directory
   * @param exclude Excluded file names (wildcards allowed) Separated by commas
   * @param excludePath Application-specific exclusion paths such as system folders
   * @return Child file list
   */
  public static File[] getChildren(File dir, String exclude, String[] excludePath) {
    File[] res = null;
    String regexp = "";

    if (dir == null) return null;
    if (!dir.exists()) return null;
    if (!dir.isDirectory()) return null;

    if (!StringUtils.isNullOrEmpty(exclude)) {
      String[] sp = exclude.split(",");
      if (sp != null && sp.length > 0) {
        for (String s : sp) {
          if (StringUtils.isNullOrEmpty(s)) continue;
          if (!regexp.isEmpty()) regexp += "|";
          regexp += s.trim();
        }
      } else {
        regexp = exclude.trim();
      }
      regexp = regexp.replaceAll("\\.", "\\\\.");
      regexp = regexp.replaceAll("\\*", "\\.*");
      regexp = "(" + regexp + ")";
    }
    res = getChildrenSub(dir, regexp, excludePath);
    return res;
  }

  /**
   * Get the files under it
   *
   * @param dir Search directory
   * @param exclude Excluded file names (wildcards allowed) Separated by commas
   * @param excludePath Application-specific exclusion paths such as system folders
   * @return Child file list
   */
  private static File[] getChildrenSub(File dir, String exclude, String[] excludePath) {
    if (dir == null) return null;
    if (!dir.exists()) return null;
    if (!dir.isDirectory()) return null;

    ArrayList<File> list = new ArrayList<File>();
    if (dir.isDirectory()) {
      File[] files = dir.listFiles();
      for (File f : files) {
        if (f == null) continue;
        if (f.isFile()) {
          String name = f.getName();
          if (!StringUtils.isNullOrEmpty(exclude)) {
            if (StringUtils.patternMatch(name, exclude)) continue;
          }
          if (excludePath != null && excludePath.length > 0) {
            boolean flag = false;
            for (String s : excludePath) {
              if (f.getAbsolutePath().startsWith(s)) {
                flag = true;
                break;
              }
            }
            if (flag) continue;
          }
          list.add(f);
        } else if (f.isDirectory()) {
          File[] children = getChildrenSub(f, exclude, excludePath);
          if (children != null) {
            for (File c : children) {
              if (c != null) {
                list.add(c);
              }
            }
          }
        }
      }
    }
    File[] result = list.toArray(new File[0]);
    return result;
  }

  /**
   * File copy
   *
   * @param from
   * @param toDir
   * @param to
   * @throws IOException File copy error
   */
  public static void copyFile(File from, File toDir, File to) throws IOException {
    File[] children = getChildPathList(to, toDir);
    if (children != null && children.length > 0) {
      for (File c : children) {
        if (c == null) continue;
        if (!c.exists()) c.mkdir();
      }
    }

    FileInputStream in = new FileInputStream(from);
    FileOutputStream out = new FileOutputStream(to);
    FileChannel fromChannel = in.getChannel();
    FileChannel toChannel = out.getChannel();
    try {
      fromChannel.transferTo(0, fromChannel.size(), toChannel);
    } finally {
      fromChannel.close();
      toChannel.close();
      in.close();
      out.close();
    }
  }

  /**
   * Check if file is a symbolic link.
   *
   * @param file Inspection file
   * @return true = symbolic link
   * @throws IOException File IO exception
   */
  public static boolean isSymbolicLink(File file) throws IOException {
    if (file == null) return false;
    File canonicalFile;
    if (file.getParent() == null) {
      canonicalFile = file;
    } else {
      File folder = file.getParentFile().getCanonicalFile();
      canonicalFile = new File(folder, file.getName());
    }
    return !canonicalFile.getCanonicalFile().equals(canonicalFile.getAbsoluteFile());
  }
}
