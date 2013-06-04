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
 * ファイルユーティリティクラス
 * 
 * @author riken
 * 
 */
public class FileUtils {

	/**
	 * ２つのパスの階層ファイルのリストを取得する.<br/>
	 * 子パスが親パスの子階層ではない場合はnullを返す.<br/>
	 * 子パスと親パスが同一階層の場合はnullを返す.
	 * 
	 * @param childFile
	 *            子パス
	 * @param parentFile
	 *            親パス
	 * @return 階層ファイルリスト
	 */
	public static File[] getChildPathList(File childFile, File parentFile) {
		try {
			ArrayList<File> filelist = new ArrayList<File>();

			// 子ファイルから親パスまでさかのぼる。
			File curPath = childFile.getCanonicalFile();
			if (!curPath.isDirectory()) {
				curPath = curPath.getParentFile();
			}
			File srcPath = parentFile.getCanonicalFile();
			if (!srcPath.isDirectory()) {
				srcPath = srcPath.getParentFile();
			}

			if (curPath.equals(srcPath))
				return null;

			boolean ischild = false;
			while (curPath != null) {
				filelist.add(0, curPath);
				curPath = curPath.getParentFile();
				if (curPath == null)
					break;
				if (curPath.equals(srcPath)) {
					ischild = true;
					break;
				}
			}

			if (!ischild)
				return null;
			if (filelist.size() == 0)
				return null;

			return (File[]) filelist.toArray(new File[0]);

		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * 子孫ファイルであるかチェックする
	 * 
	 * @param childFile
	 *            検索ファイル
	 * @param parentFile
	 *            親ファイル
	 * @return true=子孫ファイル
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
		// ２つのパスの階層ファイルのリストを取得する。
		File[] files = getChildPathList(childPath, parentFile);
		return (files != null && files.length > 0);
	}

	/**
	 * パスの親子関係をチェックする
	 * 
	 * @param path
	 *            親パス
	 * @param childPath
	 *            子パス
	 * @return true=親子パス
	 */
	public static boolean isChildPath(String path, String childPath) {
		if (path.equals(childPath)) {
			return true;
		}
		if (childPath.startsWith(path + File.separator)) {
			return true;
		}
		// Windowsの場合
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
	 * ２つのパスの相対パスを取得する。<br/>
	 * 子パスが親パスの子階層ではない場合は子パスの絶対パスを返す。
	 * 
	 * @param childFile
	 *            子パス
	 * @param parentFile
	 *            親パス
	 * @return 階層ファイルリスト
	 */
	public static String getRelativePath(File childFile, File parentFile) {
		if (childFile == null)
			return null;
		if (parentFile == null)
			return childFile.getAbsolutePath();
		try {
			if (childFile.equals(parentFile))
				return "." + File.separator;
			if (childFile.getCanonicalFile().equals(
					parentFile.getCanonicalFile()))
				return "." + File.separator;
			if (childFile.isFile()) {
				if (isEqualsFile(childFile.getParentFile(), parentFile))
					return "." + File.separator + childFile.getName();
			}
			if (!childFile.exists()) {
				return null;
			}

			// ２つのパスリストの取得を行う
			File[] paths = getChildPathList(childFile, parentFile);
			if (paths == null) {
				// 子パスではない
				return childFile.getAbsolutePath();
			}

			// パス階層の組み立てを行う。
			StringBuffer buf = new StringBuffer();
			for (int i = 0; i < paths.length; i++) {
				buf.append(paths[i].getName());
				if (i < paths.length - 1)
					buf.append(File.separator);
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
	 * 相対パスを取得する
	 * 
	 * @param path
	 *            対象パス
	 * @param root
	 *            基準パス
	 * @return 対象パスの基準パスに対する相対パス
	 */
	public static String getRelativePath(String path, String root) {
		if (path.length() <= root.length()) {
			return "";
		}
		return path.substring(root.length() + 1);
	}

	/**
	 * base_fileとnameのファイルを結合したファイルを取得する。
	 * 
	 * @param base_file
	 *            基準パス
	 * @param name
	 *            ファイル名
	 * @return 結合ファイル
	 */
	public static File joinFilePath(File base_file, String name) {
		if (base_file == null || name == null)
			return null;

		// ファイル名が絶対パスであるかチェックする。
		if ((new File(name)).isAbsolute()) {
			// 絶対パスであるので、そのまま返す。
			return new File(name);
		}

		// 基準パスからの相対パスを取得する
		String parent = null;
		if (base_file.isDirectory()) {
			parent = base_file.getAbsolutePath();
		} else {
			parent = base_file.getParentFile().getAbsolutePath();
		}

		// 基準パスにファイルを結合する
		return new File(parent + File.separator + name);
	}

	/**
	 * パス中の\\を/に置換する。 WindowsとLinuxのファイルセパレータの違いを吸収する。
	 * 
	 * @param path
	 *            ファイルパス
	 * @return "/"セパレータパス名
	 */
	public static final String replaceFileSeparator(String path) {
		if (path == null || path.isEmpty())
			return null;
		return path.replace('\\', '/');
	}

	/**
	 * 絶対パスかチェックする
	 * 
	 * @param path
	 *            チェックパス
	 * @return true:絶対パス
	 */
	public static final boolean isAbsolutePath(String path) {
		String slash_path = replaceFileSeparator(path);
		if (slash_path == null || slash_path.isEmpty())
			return false;

		if (slash_path.startsWith("/"))
			return true;

		// if (slash_path.indexOf(":") != 1) // 2013/01/22 修正 yabe
		if (slash_path.indexOf(":") == 1)
			return true;
		return false;
	}

	/**
	 * ルートパスからのパスリストを取得する。
	 * 
	 * @param src_path
	 *            パス
	 * @return 階層ファイルリスト
	 */
	public static File[] getPathList(File src_path) {
		ArrayList<File> filelist = new ArrayList<File>();

		// 子ファイルから親パスまでさかのぼる。
		File curPath = src_path;
		if (!curPath.isDirectory()) {
			curPath = curPath.getParentFile();
		}

		while (curPath != null) {
			filelist.add(0, curPath);
			curPath = curPath.getParentFile();
			if (curPath == null)
				break;
		}
		if (filelist.size() == 0)
			return null;

		return (File[]) filelist.toArray(new File[0]);
	}

	/**
	 * ２つのファイルが同じでファイルパスであるかチェックする。
	 * 
	 * @param src
	 *            ファイル名1
	 * @param dest
	 *            ファイル名2
	 * @return true=2つファイルは同じ
	 */
	public static boolean isEqualsFile(String src, String dest) {
		if (src == null || src.isEmpty())
			return false;
		if (dest == null || dest.isEmpty())
			return false;
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
	 * ２つのファイルが同じでファイルパスであるかチェックする。
	 * 
	 * @param srcFile
	 *            ファイル1
	 * @param destFile
	 *            ファイル2
	 * @return true=2つファイルは同じ
	 */
	public static boolean isEqualsFile(File srcFile, File destFile) {
		if (srcFile == null)
			return false;
		if (destFile == null)
			return false;
		try {
			srcFile = srcFile.getCanonicalFile();
			destFile = destFile.getCanonicalFile();
			return srcFile.equals(destFile);

		} catch (IOException e) {
			return false;
		}
	}

	/**
	 * 配下のファイルを取得する
	 * 
	 * @param dir
	 *            検索ディレクトリ
	 * @param exclude
	 *            除外ファイル名（ワイルドカード可）カンマ区切りで指定
	 * @param excludePath
	 *            システムフォルダなどのアプリケーション固有の除外パス
	 * @return 子ファイルリスト
	 */
	public static File[] getChildren(File dir, String exclude,
			String[] excludePath) {
		File[] res = null;
		String regexp = "";

		if (dir == null)
			return null;
		if (!dir.exists())
			return null;
		if (!dir.isDirectory())
			return null;

		if (!StringUtils.isNullOrEmpty(exclude)) {
			String[] sp = exclude.split(",");
			if (sp != null && sp.length > 0) {
				for (String s : sp) {
					if (StringUtils.isNullOrEmpty(s))
						continue;
					if (!regexp.isEmpty())
						regexp += "|";
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
	 * 配下のファイルを取得する
	 * 
	 * @param dir
	 *            検索ディレクトリ
	 * @param exclude
	 *            除外ファイル名（ワイルドカード可）カンマ区切りで指定
	 * @param excludePath
	 *            システムフォルダなどのアプリケーション固有の除外パス
	 * @return 子ファイルリスト
	 */
	private static File[] getChildrenSub(File dir, String exclude,
			String[] excludePath) {
		if (dir == null)
			return null;
		if (!dir.exists())
			return null;
		if (!dir.isDirectory())
			return null;

		ArrayList<File> list = new ArrayList<File>();
		if (dir.isDirectory()) {
			File[] files = dir.listFiles();
			for (File f : files) {
				if (f == null)
					continue;
				if (f.isFile()) {
					String name = f.getName();
					if (!StringUtils.isNullOrEmpty(exclude)) {
						if (StringUtils.patternMatch(name, exclude))
							continue;
					}
					if (excludePath != null && excludePath.length > 0) {
						boolean flag = false;
						for (String s : excludePath) {
							if (f.getAbsolutePath().startsWith(s)) {
								flag = true;
								break;
							}
						}
						if (flag)
							continue;
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
	 * ファイルコピー
	 * 
	 * @param from
	 * @param toDir
	 * @param to
	 * @throws IOException
	 *             ファイルコピーエラー
	 */
	public static void copyFile(File from, File toDir, File to)
			throws IOException {
		File[] children = getChildPathList(to, toDir);
		if (children != null && children.length > 0) {
			for (File c : children) {
				if (c == null)
					continue;
				if (!c.exists())
					c.mkdir();
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
	 * fileがシンボリックリンクであるかチェックする.
	 * 
	 * @param file
	 *            検査ファイル
	 * @return true=シンボリックリンク
	 * @throws IOException
	 *             ファイルIO例外
	 */
	public static boolean isSymbolicLink(File file) throws IOException {
		if (file == null)
			return false;
		File canonicalFile;
		if (file.getParent() == null) {
			canonicalFile = file;
		} else {
			File folder = file.getParentFile().getCanonicalFile();
			canonicalFile = new File(folder, file.getName());
		}
		return !canonicalFile.getCanonicalFile().equals(
				canonicalFile.getAbsoluteFile());
	}
}
