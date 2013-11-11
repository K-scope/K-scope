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
 * 
 * Version to work with SSHconnect.
 */

package jp.riken.kscope.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.SizeRequirements;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.JTextComponent;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;
import javax.swing.text.Utilities;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.properties.KscopeProperties;

/**
 * Swingユーティリティクラス
 * 
 * @author riken
 */
public class SwingUtils {
	/** タブサイズチェック用文字 */
	private static final char TAB_CHECK_CHARACTOR = 'o';

	/**
	 * テキストコンポーネントのタブサイズを設定する
	 * 
	 * @param component
	 *            テキストコンポーネント
	 * @param tabsize
	 *            タブサイズ
	 */
	public static void setTabSize(JTextComponent component, int tabsize) {

		SimpleAttributeSet attrs = new SimpleAttributeSet();

		FontMetrics fm = component.getFontMetrics(component.getFont());
		int charWidth = fm.charWidth(TAB_CHECK_CHARACTOR);
		int tabLength = charWidth * tabsize;
		TabStop[] tabs = new TabStop[10];
		for (int j = 0; j < tabs.length; j++) {
			tabs[j] = new TabStop((j + 1) * tabLength);
		}
		TabSet tabSet = new TabSet(tabs);
		attrs = new SimpleAttributeSet();
		StyleConstants.setTabSet(attrs, tabSet);

		int length = component.getDocument().getLength();
		((StyledDocument) component.getDocument()).setParagraphAttributes(0,
				length, attrs, false);

		return;
	}

	/**
	 * 折り返し無しのテキストビューの作成
	 * 
	 * @return 折り返し無しテキストビュー
	 */
	public static NoWrapEditorKit factoryNoWrapEditorKit() {
		return new SwingUtils().new NoWrapEditorKit();
	}

	/**
	 * 行折り返し段落のビュークラス
	 * 
	 * @author riken
	 */
	private class NoWrapParagraphView extends ParagraphView {
		/**
		 * コンストラクタ
		 * 
		 * @param elem
		 *            このビューが扱う要素
		 */
		public NoWrapParagraphView(Element elem) {
			super(elem);
		}

		/**
		 * 行の幅のサイズ要件を計算します.<br/>
		 * １行の折り返しサイズを設定する。
		 * 
		 * @param axis
		 *            行位置
		 * @param r
		 *            コンポーネントのサイズと位置オブジェクト
		 * @return コンポーネントのサイズと位置オブジェクト
		 */
		@Override
		protected SizeRequirements calculateMinorAxisRequirements(int axis,
				SizeRequirements r) {
			SizeRequirements req = super
					.calculateMinorAxisRequirements(axis, r);
			req.minimum = req.preferred;
			return req;
		}

		/**
		 * 指定された子のインデックスに反してフローする制約スパンを取り出します。
		 * 
		 * @param index
		 *            照会されるビューのインデックス
		 * @return ビューの制約スパン
		 */
		@Override
		public int getFlowSpan(int index) {
			return Integer.MAX_VALUE;
		}
	}

	/**
	 * ビューの作成クラス
	 * 
	 * @author riken
	 */
	class NoWrapViewFactory implements ViewFactory {
		/**
		 * 要素に基づいてビューを作成します。
		 * 
		 * @param elem
		 *            作成対象要素
		 * @return ビュー
		 */
		@Override
		public View create(Element elem) {
			String kind = elem.getName();
			if (kind != null) {
				if (kind.equals(AbstractDocument.ContentElementName)) {
					return new LabelView(elem);
				} else if (kind.equals(AbstractDocument.ParagraphElementName)) {
					return new NoWrapParagraphView(elem);
				} else if (kind.equals(AbstractDocument.SectionElementName)) {
					return new BoxView(elem, View.Y_AXIS);
				} else if (kind.equals(StyleConstants.ComponentElementName)) {
					return new ComponentView(elem);
				} else if (kind.equals(StyleConstants.IconElementName)) {
					return new IconView(elem);
				}
			}
			return new LabelView(elem);
		}
	}

	/**
	 * 書式付きテキストスタイル
	 * 
	 * @author riken
	 */
	@SuppressWarnings("serial")
	class NoWrapEditorKit extends StyledEditorKit {

		/**
		 * ビュー作成クラスを取得する。
		 * 
		 * @return ビュー作成クラス
		 */
		@Override
		public ViewFactory getViewFactory() {
			return new NoWrapViewFactory();
		}
	}

	/**
	 * HTML形式テキスト要素から指定要素、属性の値の取得を行う
	 * 
	 * @param elem
	 *            HTML形式テキスト要素
	 * @param tagname
	 *            要素名
	 * @param attrname
	 *            属性名
	 * @return 属性値
	 */
	@SuppressWarnings("rawtypes")
	public static String getAttributeValue(javax.swing.text.Element elem,
			HTML.Tag tagname, HTML.Attribute attrname) {

		// タグの探索
		javax.swing.text.AttributeSet attrs = elem.getAttributes();
		String attrValue = null;
		for (Enumeration enumAttrs = attrs.getAttributeNames(); enumAttrs
				.hasMoreElements();) {
			Object attrTag = enumAttrs.nextElement();

			// 検索要素名と一致しているか
			if (attrTag instanceof HTML.Tag && (HTML.Tag) attrTag == tagname) {
				javax.swing.text.SimpleAttributeSet atag = (SimpleAttributeSet) attrs
						.getAttribute(attrTag);

				// 属性の探索
				for (Enumeration enumAtag = atag.getAttributeNames(); enumAtag
						.hasMoreElements();) {
					Object attrAtag = enumAtag.nextElement();

					// 検索属性と一致しているか
					if (attrAtag instanceof HTML.Attribute
							&& (HTML.Attribute) attrAtag == attrname) {
						attrValue = (String) atag.getAttribute(attrAtag);
					}
				}
			}
		}

		return attrValue;
	}

	/**
	 * 外部プログラムを起動する.<br/>
	 * program指定が無い場合は、OS関連付けによる起動(java.awt.Desktop)による起動を行う。
	 * 
	 * @param filename
	 *            起動ファイル名
	 * @param program
	 *            起動プログラム
	 * @param options
	 *            起動引数
	 * @return エラーメッセージ（null=正常起動)
	 */
	public static String processOpenProgram(String filename, String program,
			String[] options) {
		String errMsg = null;
		try {
			if (program == null || program.isEmpty()) {

				if (filename == null || filename.isEmpty()) {
					// swingutils.processopenprogram.error.empty=起動URL又はファイル名が指定されていません。
					errMsg = Message
							.getString("swingutils.processopenprogram.error.empty");
					return errMsg;
				}

				// add by @hira at 2013/05/13 : デスクトップがサポートされているか.
				if (!Desktop.isDesktopSupported()) {
					// デスクトップがサポートされていません。
					errMsg = Message
							.getString("swingutils.processopenprogram.error.desktopsupported");
					return errMsg;
				}

				// OS関連付けによる起動
				Desktop desktop = Desktop.getDesktop();
				if (filename.startsWith("http://")
						|| filename.startsWith("https://")) {
					// OS依存によるブラウザ起動
					URI uri = null;
					try {
						uri = new URI(filename);
					} catch (URISyntaxException ex) {
						errMsg = ex.getMessage();
						return errMsg;
					}
					try {
						desktop.browse(uri);
					} catch (IOException ex) {
						errMsg = ex.getMessage();
						return errMsg;
					}
				} else {
					File openFile = null;
					if (filename.startsWith("file:/")) {
						URL url = new URL(filename);
						URI uri = url.toURI();
						openFile = new File(uri);
					} else {
						openFile = new File(filename);
					}
					if (!openFile.exists()) {
						// swingutils.processopenprogram.error.notexists.file=起動ファイル[%s]が存在しません。
						errMsg = Message
								.getString(
										"swingutils.processopenprogram.error.notexists.file",
										openFile.getPath());
						return errMsg;
					}
					if (openFile != null) {
						try {
							// ファイル起動
							desktop.open(openFile);
						} catch (IOException e) {
							// e.printStackTrace();
							// swingutils.processopenprogram.error.notexists.program=ファイル[%s]に関連付けられたプログラムが存在しません。
							errMsg = Message
									.getString(
											"swingutils.processopenprogram.error.notexists.program",
											openFile.getName());
							return errMsg;
						}
					}
				}
			} else {
				String openFilename = null;
				if (filename != null && !filename.isEmpty()) {
					if (filename.startsWith("file:/")) {
						URL url = new URL(filename);
						URI uri = url.toURI();
						File openFile = new File(uri);
						openFilename = openFile.getAbsolutePath();
					} else {
						File openFile = new File(filename);
						openFilename = openFile.getAbsolutePath();
					}
				}
				// プログラム指定による起動
				List<String> args = new ArrayList<String>();
				args.add(program);
				if (options != null && options.length > 0) {
					args.addAll(Arrays.asList(options));
				}
				if (openFilename != null) {
					args.add(openFilename);
				}
				// swingutils.processopenprogram.error.invalid.process=起動プログラム[%s]によるプロセス起動に失敗しました。
				errMsg = Message.getString(
						"swingutils.processopenprogram.error.invalid.process",
						program);
				try {
					String[] validcommand = args.toArray(new String[0]);
					for (int i = 0; i < validcommand.length; i++) {
						validcommand[i] = StringUtils
								.trimQuote(validcommand[i]);
					}
					ProcessBuilder pb = new ProcessBuilder(validcommand);
					Process p = pb.start();
					if (p == null) {
						return errMsg;
					}
				} catch (IOException e) {
					return errMsg;
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			errMsg = ex.getMessage();
			return errMsg;
		}
		return null;
	}

	/**
	 * 外部プログラムを起動する.<br/>
	 * program指定が無い場合は、OS関連付けによる起動(java.awt.Desktop)による起動を行う。
	 * 
	 * @param commands
	 *            起動コマンドリスト
	 * @param workdirectory
	 *            実行フォルダ
	 * @param outStream
	 *            実行コンソール出力ストリーム
	 * @return エラーメッセージ（null=正常起動)
	 * @throws Exception
	 */
	public static int processRun(String commands[], File workdirectory,	OutputStream outStream) throws Exception {
		String errMsg = null;
		int result = -1;
		Process process = null;
		StringBuilder buf = new StringBuilder();
		// String bufStr = new String();
		if (commands == null || commands.length <= 0)
			return 1;
		String cmdString = "";
		for (int i = 0; i < commands.length; i++) {
			cmdString += commands[i] + " ";
		}
		cmdString = cmdString.trim();

		try {
			String[] validcommand = commands.clone();
			for (int i = 0; i < validcommand.length; i++) {
				validcommand[i] = StringUtils.trimQuote(validcommand[i]);
			}
			ProcessBuilder pb = null;
			if (commands[0].indexOf("java")==0) { // call SSHconnect
				pb = new ProcessBuilder(commands);
			}
			else {
				pb = new ProcessBuilder(validcommand);
			}
			pb.redirectErrorStream(true);
			
			/*System.out.println(System.getProperty("user.dir"));
			// SET location to look for SSHcommect to the same directory as kscope.jar
			pb.directory(new File(System.getProperty("user.dir")));*/
			if (workdirectory != null && commands[0].indexOf("java") != 0) {
				pb.directory(workdirectory);
			}
			process = pb.start();
			if (process == null) {
				errMsg = Message.getString(
						"swingutils.processrun.error.invalid.command",
						cmdString);
				throw new Exception(errMsg);
			}
			// プロセスのエラーストリーム取得
			InputStream is = process.getInputStream();
			while (true) {
				int c = is.read();
				if (c == -1) {
					is.close();
					break;
				}
				if (outStream != null) {
					outStream.write(c);
					if (c == 0x0A || c == 0x0D) {
						outStream.flush();
					}
				}
				buf.append((char) c);
			}
			is.close();
			if (outStream != null) {
				outStream.write(0x0A);
				outStream.flush();
			}
			// bufStr = new String(buf.toString().getBytes("iso-8859-1"));
			process.waitFor();
			result = process.exitValue();

		} catch (Exception ex) {
			ex.printStackTrace();
			errMsg = ex.getMessage();
			result = -1;
			if (process != null) {
				result = process.exitValue();
			}
			throw new Exception(errMsg);
		}

		return result;
	}

	/**
	 * 指定キャレット位置の行番号を取得する
	 * 
	 * @param editor
	 *            テキストコンポーネント
	 * @param pos
	 *            キャレット位置
	 * @return 行番号(1〜)
	 */
	public static int getRow(JTextComponent editor, int pos) {
		int rn = (pos == 0) ? 1 : 0;
		try {
			int offs = pos;
			while (offs > 0) {
				offs = Utilities.getRowStart(editor, offs) - 1;
				rn++;
			}
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		return rn;
	}

	/**
	 * 指定キャレット位置の列番号を取得する
	 * 
	 * @param editor
	 *            テキストコンポーネント
	 * @param pos
	 *            キャレット位置
	 * @return 列番号(1〜)
	 */
	public static int getColumn(JTextComponent editor, int pos) {
		try {
			return pos - Utilities.getRowStart(editor, pos) + 1;
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		return -1;
	}

	/**
	 * 子コンポーネントすべてにフォーカスリスナを設定する
	 * 
	 * @param container
	 *            親コンポーネント
	 * @param listener
	 *            フォーカスリスナ
	 */
	public static void addChildFocusListener(Container container,
			FocusListener listener) {
		Component[] comps = container.getComponents();

		for (Component child : comps) {
			child.addFocusListener(listener);
			if (child instanceof Container) {
				addChildFocusListener((Container) child, listener);
			}
		}
	}

	/**
	 * 子コンポーネントのJPanelすべてに背景色を設定する
	 * 
	 * @param container
	 *            親コンポーネント
	 * @param background
	 *            背景色
	 */
	public static void setBackgroundChildPanel(Container container,
			Color background) {

		if (container instanceof JPanel) {
			if (background != null) {
				((JPanel) container).setOpaque(true);
				container.setBackground(background);
			} else {
				((JPanel) container).setOpaque(false);
				container.setBackground(background);
			}
		}

		Component[] comps = container.getComponents();
		for (Component child : comps) {
			if (child instanceof Container) {
				setBackgroundChildPanel((Container) child, background);
			}
		}
	}

	/**
	 * フォルダ選択ダイアログを表示する. Macの場合、java.awt.FileDialogを使用してフォルダ選択ダイアログを表示する. Linux,
	 * Windowsの場合はjavax.swing.JFileChooserを使用する。
	 * 
	 * @param parent
	 *            親コンポーネント
	 * @param title
	 *            ダイアログタイトル
	 * @param currentDirectoryPath
	 *            表示フォルダ
	 * @param multiselection
	 *            複数選択(true=複数選択):(Macでは使用不可)
	 * @return 選択ファイル
	 */
	public static File[] showOpenFolderDialog(Component parent, String title,
			String currentDirectoryPath, boolean multiselection) {

		try {
			// for Mac
			// add at 2013/05/31 by @hira
			// Macのフォルダダイアログの表示フラグ
			// java1.7以上はAppleScriptにてフォルダダイアログを表示する
			// java1.6以下はjava.awt.FileDialogを使用する。
			//boolean applescript = KscopeProperties.isJava17Later();
                        boolean applescript = KscopeProperties.isApplescript();
			if (KscopeProperties.isMac() && applescript) {
				File selected = AppleScriptEngine.showFolderDialog(title,
						currentDirectoryPath);
				if (selected == null) {
					return null;
				}
				File[] files = { selected };
				return files;
			} else if (KscopeProperties.isMac()) {
				// フォルダ選択に変更
				System.setProperty("apple.awt.fileDialogForDirectories", "true");

				// java.awt.FileDialogを使用
				FileDialog dialog = null;
				if (parent instanceof Frame) {
					dialog = new FileDialog((Frame) parent, title,
							FileDialog.LOAD);
				} else if (parent instanceof Dialog) {
					dialog = new FileDialog((Dialog) parent, title,
							FileDialog.LOAD);
				} else {
					dialog = new FileDialog((Frame) null, title,
							FileDialog.LOAD);
				}

				// 親フォルダ
				dialog.setDirectory(currentDirectoryPath);
				// フォルダ選択ダイアログの表示
				dialog.setVisible(true);

				// 選択ファイル
				String file = dialog.getFile();
				if (file == null || file.isEmpty())
					return null;

				// 選択ファイルの取得
				String selected = dialog.getDirectory() + File.separator
						+ dialog.getFile();
				if (selected != null) {
					File[] files = { new File(selected) };
					return files;
				}
			}
			// for Windows, Linux
			else {

				JFileChooser filechooser = new JFileChooser(
						currentDirectoryPath);
				filechooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				filechooser.setDialogTitle(title);
				filechooser.setMultiSelectionEnabled(multiselection);
				int selected = filechooser.showOpenDialog(parent);
				if (selected != JFileChooser.APPROVE_OPTION)
					return null;
				if (multiselection) {
					File[] files = filechooser.getSelectedFiles();
					return files;
				} else {
					File file = filechooser.getSelectedFile();
					File[] files = { file };
					return files;
				}
			}
		} catch (Exception e) {

		} finally {
			// フォルダ選択を解除
			System.setProperty("apple.awt.fileDialogForDirectories", "false");
		}

		return null;

	}

	/**
	 * プロジェクト選択ダイアログを表示する. Macの場合、java.awt.FileDialogを使用してフォルダ選択ダイアログを表示する.
	 * Linux, Windowsの場合はjavax.swing.JFileChooserを使用する。
	 * 
	 * @param parent
	 *            親コンポーネント
	 * @param title
	 *            ダイアログタイトル
	 * @param currentDirectoryPath
	 *            表示フォルダ
	 * @return 選択ファイル
	 */
	public static File showOpenProjectDialog(Component parent, String title,
			String currentDirectoryPath) {
		try {
			ProjectFilter filter = new SwingUtils().new ProjectFilter(
					KscopeProperties.PROJECT_FILE + " | Project Folder");

			// for Mac
			// add at 2013/05/31 by @hira
			// Macのフォルダダイアログの表示フラグ
			// java1.7以上はAppleScriptにてフォルダダイアログを表示する
			// java1.6以下はjava.awt.FileDialogを使用する。
			//boolean applescript = KscopeProperties.isJava17Later();
                        boolean applescript = KscopeProperties.isApplescript();
			if (KscopeProperties.isMac() && applescript) {
				File projectfile = AppleScriptEngine.showFolderDialog(title,
						currentDirectoryPath);
				return projectfile;
			} else if (KscopeProperties.isMac()) {
				// フォルダ選択に変更
				System.setProperty("apple.awt.fileDialogForDirectories", "true");

				// java.awt.FileDialogを使用
				FileDialog dialog = null;
				if (parent instanceof Frame) {
					dialog = new FileDialog((Frame) parent, title,
							FileDialog.LOAD);
				} else if (parent instanceof Dialog) {
					dialog = new FileDialog((Dialog) parent, title,
							FileDialog.LOAD);
				} else {
					dialog = new FileDialog((Frame) null, title,
							FileDialog.LOAD);
				}

				// 親フォルダ
				dialog.setDirectory(currentDirectoryPath);
				// ファイルフィルタ
				// dialog.setFilenameFilter(filter);
				// フォルダ選択ダイアログの表示
				dialog.setVisible(true);

				// 選択ファイル
				String file = dialog.getFile();
				if (file == null || file.isEmpty())
					return null;

				// 選択ファイルの取得
				String selected = dialog.getDirectory() + File.separator
						+ dialog.getFile();
				if (selected != null) {
					File projectfile = new File(selected);
					return projectfile;
				}
			}
			// for Windows, Linux
			else {

				JFileChooser filechooser = new JFileChooser(
						currentDirectoryPath);
				// ファイルとフォルダを選択可能とする.
				filechooser
						.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
				filechooser.setDialogTitle(title);
				filechooser.addChoosableFileFilter(filter);
				// JDK7の場合はsetFileFilterを設定しないとフィルタが選択されない。 at 2013/05/14 by
				// @hira
				filechooser.setFileFilter(filter);
				int selected = filechooser.showOpenDialog(parent);
				if (selected != JFileChooser.APPROVE_OPTION)
					return null;
				File file = filechooser.getSelectedFile();
				return file;
			}
		} catch (Exception e) {

		} finally {
			// フォルダ選択を解除
			System.setProperty("apple.awt.fileDialogForDirectories", "false");
		}

		return null;
	}

	/**
	 * フォルダ保存ダイアログを表示する. Macの場合、java.awt.FileDialogを使用してフォルダ選択ダイアログを表示する. Linux,
	 * Windowsの場合はjavax.swing.JFileChooserを使用する。
	 * 
	 * @param parent
	 *            親コンポーネント
	 * @param title
	 *            ダイアログタイトル
	 * @param currentDirectoryPath
	 *            表示フォルダ
	 * @param multiselection
	 *            複数選択(true=複数選択):(Macでは使用不可)
	 * @return 選択ファイル
	 */
	public static File[] showSaveFolderDialog(Component parent, String title,
			String currentDirectoryPath, boolean multiselection) {
		try {
			// for Mac
			// add at 2013/05/31 by @hira
			// Macのフォルダダイアログの表示フラグ
			// java1.7以上はAppleScriptにてフォルダダイアログを表示する
			// java1.6以下はjava.awt.FileDialogを使用する。
			//boolean applescript = KscopeProperties.isJava17Later();
                        boolean applescript = KscopeProperties.isApplescript();
			if (KscopeProperties.isMac() && applescript) {
				File selected = AppleScriptEngine.showFolderDialog(title,
						currentDirectoryPath);
				if (selected == null) {
					return null;
				}
				File[] files = { selected };
				return files;
			} else if (KscopeProperties.isMac()) {
				// フォルダ選択に変更
				System.setProperty("apple.awt.fileDialogForDirectories", "true");

				// java.awt.FileDialogを使用
				// modify FileDialog Mode from FileDialog.SAVE to
				// FileDialog.LOAD at 2013/05/31 by @hira
				// フォルダ保存ダイアログから選択ダイアログに変更する。既存フォルダが選択できない（入力が必要）為
				FileDialog dialog = null;
				if (parent instanceof Frame) {
					dialog = new FileDialog((Frame) parent, title,
							FileDialog.LOAD);
				} else if (parent instanceof Dialog) {
					dialog = new FileDialog((Dialog) parent, title,
							FileDialog.LOAD);
				} else {
					dialog = new FileDialog((Frame) null, title,
							FileDialog.LOAD);
				}
				// 親フォルダ
				dialog.setDirectory(currentDirectoryPath);
				// フォルダ選択ダイアログの表示
				dialog.setVisible(true);

				// 選択ファイル
				String file = dialog.getFile();
				if (file == null || file.isEmpty())
					return null;

				// 選択ファイルの取得
				String selected = dialog.getDirectory() + File.separator
						+ dialog.getFile();
				if (selected != null) {
					File[] files = { new File(selected) };
					return files;
				}
			}
			// for Windows, Linux
			else {

				JFileChooser filechooser = new JFileChooser(
						currentDirectoryPath);
				filechooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				filechooser.setDialogTitle(title);
				filechooser.setMultiSelectionEnabled(multiselection);
				int selected = filechooser.showSaveDialog(parent);
				if (selected != JFileChooser.APPROVE_OPTION)
					return null;
				if (multiselection) {
					File[] files = filechooser.getSelectedFiles();
					return files;
				} else {
					File file = filechooser.getSelectedFile();
					File[] files = { file };
					return files;
				}
			}
		} catch (Exception e) {

		} finally {
			// フォルダ選択を解除
			System.setProperty("apple.awt.fileDialogForDirectories", "false");
		}

		return null;

	}

	/**
	 * ファイル選択ダイアログを表示する. Macの場合、java.awt.FileDialogを使用してフォルダ選択ダイアログを表示する. Linux,
	 * Windowsの場合はjavax.swing.JFileChooserを使用する。
	 * 
	 * @param parent
	 *            親コンポーネント
	 * @param title
	 *            ダイアログタイトル
	 * @param currentDirectoryPath
	 *            表示フォルダ
	 * @param filter
	 *            ファイルフィルタ
	 * @param multiselection
	 *            複数選択(true=複数選択):(Macでは使用不可)
	 * @return 選択ファイル
	 */
	public static File[] showOpenFileDialog(Component parent, String title,
			String currentDirectoryPath, ExtFileFilter filter,
			boolean multiselection) {
		try {
			if (KscopeProperties.isMac()) {
				FileDialog dialog = null;
				if (parent instanceof Frame) {
					dialog = new FileDialog((Frame) parent, title,
							FileDialog.LOAD);
				} else if (parent instanceof Dialog) {
					dialog = new FileDialog((Dialog) parent, title,
							FileDialog.LOAD);
				} else {
					dialog = new FileDialog((Frame) null, title,
							FileDialog.LOAD);
				}
				// 親フォルダ
				dialog.setDirectory(currentDirectoryPath);

				// ファイルフィルタ
				dialog.setFilenameFilter(filter);

				// Multiselection
				dialog.setMultipleMode(multiselection);
				
				// フォルダ選択ダイアログの表示
				dialog.setVisible(true);

				// 選択ファイル
				String file = dialog.getFile();
				if (file == null || file.isEmpty())
					return null;

				// 選択ファイルの取得
				String selected = dialog.getDirectory() + File.separator
						+ dialog.getFile();
				if (selected != null) {
					File[] files = { new File(selected) };
					return files;
				}
			} else {

				// ファイル選択ダイアログを表示する。
				JFileChooser filechooser = new JFileChooser(
						currentDirectoryPath);
				filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				filechooser.setMultiSelectionEnabled(multiselection);
				filechooser.setDialogTitle(title);
				if (filter != null) {
					filechooser.addChoosableFileFilter(filter);
					// JDK7の場合はsetFileFilterを設定しないとフィルタが選択されない。 at 2013/05/14 by
					// @hira
					filechooser.setFileFilter(filter);
				}
				// XMLファイル選択ダイアログの表示
				int selected = filechooser.showOpenDialog(parent);
				if (selected != JFileChooser.APPROVE_OPTION)
					return null;

				if (multiselection) {
					File[] files = filechooser.getSelectedFiles();
					return files;
				} else {
					File file = filechooser.getSelectedFile();
					File[] files = { file };
					return files;
				}
			}
		} catch (Exception e) {

		}

		return null;
	}

	/**
	 * 拡張子ファイルフィルタ.
	 * 
	 * @author riken
	 */
	public class ExtFileFilter extends FileFilter implements FilenameFilter {
		/** ファイルフィルタ説明文 */
		private String description;
		/** 拡張子 */
		private String[] exts;
		/** フィルタ */
		private FileFilter filter;

		/**
		 * コンストラクタ
		 * 
		 * @param description
		 *            ファイルフィルタ説明文
		 * @param exts
		 *            拡張子リスト
		 */
		public ExtFileFilter(String description, String[] exts) {
			this.description = description;
			this.exts = exts;
		}

		/**
		 * コンストラクタ
		 * 
		 * @param filter
		 */
		public ExtFileFilter(FileFilter filter) {
			this.description = filter.getDescription();
			this.filter = filter;
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param file
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		@Override
		public boolean accept(File file) {
			if (file.isDirectory())
				return true;
			return accept(file.getName());
		}

		/**
		 * ファイルフィルタ説明文を取得する
		 * 
		 * @return ファイルフィルタ説明文
		 */
		@Override
		public String getDescription() {
			return this.description;
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param dir
		 *            表示フォルダ
		 * @param name
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		@Override
		public boolean accept(File dir, String name) {
			return accept(name);
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param name
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		public boolean accept(String name) {
			String file_name = name.toLowerCase();
			String ext = file_name.substring(file_name.lastIndexOf(".") + 1);
			if (this.exts == null && this.filter == null)
				return true;
			if (this.exts != null) {
				for (String filterExt : exts) {
					if (ext.equalsIgnoreCase(filterExt)) {
						return true;
					}
				}
			}
			if (this.filter != null) {
				return this.filter.accept(new File(name));
			}
			return false;
		}
	}

	/**
	 * Makeファイル選択ダイアログを表示する. Macの場合、java.awt.FileDialogを使用してフォルダ選択ダイアログを表示する.
	 * Linux, Windowsの場合はjavax.swing.JFileChooserを使用する。
	 * 
	 * @param parent
	 *            親コンポーネント
	 * @param title
	 *            ダイアログタイトル
	 * @param currentDirectoryPath
	 *            表示フォルダ
	 * @param multiselection
	 *            複数選択(true=複数選択):(Macでは使用不可)
	 * @return 選択ファイル
	 */
	public static File[] showOpenMakefileDialog(Component parent, String title,
			String currentDirectoryPath, boolean multiselection) {
		try {
			MakefileFilter filter = new SwingUtils().new MakefileFilter(
					"Makefile(Makefile*, makefile*)");
			if (KscopeProperties.isMac()) {
				FileDialog dialog = null;
				if (parent instanceof Frame) {
					dialog = new FileDialog((Frame) parent, title,
							FileDialog.LOAD);
				} else if (parent instanceof Dialog) {
					dialog = new FileDialog((Dialog) parent, title,
							FileDialog.LOAD);
				} else {
					dialog = new FileDialog((Frame) null, title,
							FileDialog.LOAD);
				}
				// 親フォルダ
				dialog.setDirectory(currentDirectoryPath);

				// ファイルフィルタ
				dialog.setFilenameFilter(filter);

				// フォルダ選択ダイアログの表示
				dialog.setVisible(true);

				// 選択ファイル
				String file = dialog.getFile();
				if (file == null || file.isEmpty())
					return null;

				// 選択ファイルの取得
				String selected = dialog.getDirectory() + File.separator
						+ dialog.getFile();
				if (selected != null) {
					File[] files = { new File(selected) };
					return files;
				}
			} else {

				// ファイル選択ダイアログを表示する。
				JFileChooser filechooser = new JFileChooser(
						currentDirectoryPath);
				filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				filechooser.setMultiSelectionEnabled(multiselection);
				filechooser.setDialogTitle(title);
				filechooser.addChoosableFileFilter(filter);
				// JDK7の場合はsetFileFilterを設定しないとフィルタが選択されない。 at 2013/05/14 by
				// @hira
				filechooser.setFileFilter(filter);
				// ファイル選択ダイアログの表示
				int selected = filechooser.showOpenDialog(parent);
				if (selected != JFileChooser.APPROVE_OPTION)
					return null;

				if (multiselection) {
					File[] files = filechooser.getSelectedFiles();
					return files;
				} else {
					File file = filechooser.getSelectedFile();
					File[] files = { file };
					return files;
				}
			}
		} catch (Exception e) {

		}

		return null;
	}

	/**
	 * Makefileファイルフィルタ.
	 * 
	 * @author riken
	 */
	public class MakefileFilter extends FileFilter implements FilenameFilter {
		/** ファイルフィルタ説明文 */
		private String description;

		/**
		 * コンストラクタ
		 * 
		 * @param description
		 *            ファイルフィルタ説明文
		 */
		public MakefileFilter(String description) {
			this.description = description;
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param file
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		@Override
		public boolean accept(File file) {
			if (file.isDirectory())
				return true;
			return accept(file.getName());
		}

		/**
		 * ファイルフィルタ説明文を取得する
		 * 
		 * @return ファイルフィルタ説明文
		 */
		@Override
		public String getDescription() {
			return this.description;
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param dir
		 *            表示フォルダ
		 * @param name
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		@Override
		public boolean accept(File dir, String name) {
			return accept(name);
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param name
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		public boolean accept(String name) {
			if (name.length() >= 8) {
				String s = name.substring(0, 8);
				if (s.equalsIgnoreCase("makefile")) {
					return true;
				}
			}
			return false;
		}
	}

	/**
	 * Projectファイルフィルタ.
	 * 
	 * @author riken
	 */
	public class ProjectFilter extends FileFilter implements FilenameFilter {
		/** ファイルフィルタ説明文 */
		private String description;

		/**
		 * コンストラクタ
		 * 
		 * @param description
		 *            ファイルフィルタ説明文
		 */
		public ProjectFilter(String description) {
			this.description = description;
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param file
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		@Override
		public boolean accept(File file) {
			if (file.isDirectory())
				return true;
			return accept(file.getName());
		}

		/**
		 * ファイルフィルタ説明文を取得する
		 * 
		 * @return ファイルフィルタ説明文
		 */
		@Override
		public String getDescription() {
			return this.description;
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param dir
		 *            表示フォルダ
		 * @param name
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		@Override
		public boolean accept(File dir, String name) {
			return accept(name);
		}

		/**
		 * ファイルのフィルタチェックを行う
		 * 
		 * @param name
		 *            表示ファイル
		 * @return true=フィルタ対象ファイル
		 */
		public boolean accept(String name) {
			if (KscopeProperties.PROJECT_FILE.equalsIgnoreCase(name)) {
				return true;
			}
			return false;
		}
	}

	/**
	 * ファイルの保存ダイアログを表示する. Macの場合、java.awt.FileDialogを使用してフォルダ選択ダイアログを表示する.
	 * Linux, Windowsの場合はjavax.swing.JFileChooserを使用する。
	 * 
	 * @param parent
	 *            親コンポーネント
	 * @param title
	 *            ダイアログタイトル
	 * @param currentDirectoryPath
	 *            表示フォルダ
	 * @param defaultname
	 *            デフォルトファイル名
	 * @return 保存ファイル
	 */
	public static File showSaveFileDialog(Component parent, String title,
			String currentDirectoryPath, String defaultname) {
		try {
			// Mac
			if (KscopeProperties.isMac()) {
				FileDialog dialog = null;
				if (parent instanceof Frame) {
					dialog = new FileDialog((Frame) parent, title,
							FileDialog.SAVE);
				} else if (parent instanceof Dialog) {
					dialog = new FileDialog((Dialog) parent, title,
							FileDialog.SAVE);
				} else {
					dialog = new FileDialog((Frame) null, title,
							FileDialog.SAVE);
				}
				// 親フォルダ
				dialog.setDirectory(currentDirectoryPath);
				// デフォルトファイル名
				dialog.setFile(defaultname);
				// フォルダ選択ダイアログの表示
				dialog.setVisible(true);

				// 選択ファイル
				String file = dialog.getFile();
				if (file == null || file.isEmpty())
					return null;

				// 選択ファイルの取得
				String selected = dialog.getDirectory() + File.separator
						+ dialog.getFile();
				if (selected != null) {
					return new File(selected);
				}
			} else {

				// ファイル保存ダイアログを表示する。
				JFileChooser filechooser = new JFileChooser(
						currentDirectoryPath);
				filechooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				filechooser.setDialogTitle(title);

				// デフォルトファイル名
				filechooser.setSelectedFile(new File(defaultname));

				// ファイル保存ダイアログの表示
				File file = null;
				while (file == null) {
					int selected = filechooser.showSaveDialog(parent);
					if (selected != JFileChooser.APPROVE_OPTION)
						break;
					file = filechooser.getSelectedFile();
					if (isOverwriteConfirmed(file, parent)) {
						break;
					}
					file = null;
				}
				return file;
			}
		} catch (Exception e) {

		}

		return null;
	}

	/**
	 * ファイル上書き確認ダイアログを表示する。
	 * 
	 * @param file
	 *            上書き対象ファイル
	 * @param frame
	 *            親コンポーネント
	 * @return true=上書き
	 */
	private static boolean isOverwriteConfirmed(File file, Component frame) {

		if (!file.exists())
			return true;
		// swingutils.savefiledialog.overwrite.title=上書き確認
		String title = Message
				.getString("swingutils.savefiledialog.overwrite.title");
		// swingutils.savefiledialog.overwrite.message=ファイル[%s]は既に存在します。既存のファイルを置き換えますか？
		String message = Message.getString(
				"swingutils.savefiledialog.overwrite.message", file.getName());
		int confirm = JOptionPane.showConfirmDialog(frame, message, title,
				JOptionPane.WARNING_MESSAGE, JOptionPane.OK_CANCEL_OPTION);

		if (confirm == JOptionPane.OK_OPTION)
			return true;

		return false;
	}

	/**
	 * ツリーノードの親子関係をチェックする.<br/>
	 * 親ノードと子ノードが同じ場合は、falseを返す
	 * 
	 * @param parent
	 *            親ノード
	 * @param child
	 *            子ノード
	 * @return treu=親子ノード
	 */
	public static boolean isChildNode(DefaultMutableTreeNode parent,
			DefaultMutableTreeNode child) {
		if (parent == child)
			return false;
		DefaultMutableTreeNode node = child;
		while ((node = (DefaultMutableTreeNode) node.getParent()) != null) {
			if (node.getUserObject() == parent.getUserObject())
				return true;
		}
		return false;
	}

	/**
	 * ツリーパスを取得する
	 * 
	 * @param treeNode
	 *            ツリーノード
	 * @return ツリーパス
	 */
	public static TreePath getTreePath(TreeNode treeNode) {
		List<Object> nodes = new ArrayList<Object>();
		if (treeNode != null) {
			nodes.add(treeNode);
			treeNode = treeNode.getParent();
			while (treeNode != null) {
				nodes.add(0, treeNode);
				treeNode = treeNode.getParent();
			}
		}
		return nodes.isEmpty() ? null : new TreePath(nodes.toArray());
	}

	/**
	 * TreeNodeからCSV文字列に変換する
	 * 
	 * @param node
	 *            ツリーノード
	 * @return CSV文字列
	 */
	public static String toCsv(TreeNode node) {

		String buf = toCsv(node, 0);

		return buf;
	}

	/**
	 * TableModelからCSV文字列に変換する
	 * 
	 * @param model
	 *            テーブルモデル
	 * @return CSV文字列
	 */
	public static String toCsv(TableModel model) {
		return toCsv(model, null);
	}

	/**
	 * TableModelからCSV文字列に変換する
	 * 
	 * @param model
	 *            テーブルモデル
	 * @param visibled
	 *            出力列設定
	 * @return CSV文字列
	 */
	public static String toCsv(TableModel model, boolean[] visibled) {

		StringBuffer buf = new StringBuffer();

		// テーブルデータ
		int column = model.getColumnCount();
		int row = model.getRowCount();

		// ヘッダー
		StringBuffer header = new StringBuffer();
		for (int i = 0; i < column; i++) {
			if (visibled != null && !visibled[i])
				continue;
			String name = model.getColumnName(i);
			// 列名が空は出力しない。
			if (name == null || name.isEmpty())
				continue;
			if (header.length() > 0)
				header.append(",");
			header.append(escapeCsv(name));
		}
		buf.append(header);
		buf.append("\n");

		// データ
		for (int i = 0; i < row; i++) {
			StringBuffer line = new StringBuffer();
			for (int j = 0; j < column; j++) {
				if (visibled != null && !visibled[j])
					continue;
				String name = model.getColumnName(j);
				// 列名が空は出力しない。
				if (name == null || name.isEmpty())
					continue;

				// データ
				String value = model.getValueAt(i, j) != null ? model
						.getValueAt(i, j).toString() : "";
				if (line.length() > 0)
					line.append(",");
				line.append(escapeCsv(value));
			}
			buf.append(line);
			buf.append("\n");
		}

		return buf.toString();
	}

	/**
	 * JTableから選択行のみをCSV文字列に変換する
	 * 
	 * @param table
	 *            テーブル
	 * @return CSV文字列
	 */
	public static String toCsvOfSeletedRows(JTable table) {

		TableModel model = table.getModel();
		StringBuffer buf = new StringBuffer();

		// テーブルデータ
		int col = model.getColumnCount();

		// 選択行
		int[] selections = table.getSelectedRows();
		if (selections == null || selections.length <= 0) {
			return null;
		}
		for (int i = 0; i < selections.length; i++) {
			// テーブル・モデルの行数に変換
			selections[i] = table.convertRowIndexToModel(selections[i]);
		}

		DefaultTableColumnModel columnModel = (DefaultTableColumnModel) table
				.getColumnModel();
		// ヘッダー
		StringBuffer header = new StringBuffer();
		for (int i = 0; i < col; i++) {
			// 列幅が0は出力しない
			TableColumn column = columnModel.getColumn(i);
			int width = column.getPreferredWidth();
			if (width <= 0)
				continue;
			// 列名が空は出力しない。
			String name = model.getColumnName(i);
			if (name == null || name.isEmpty())
				continue;
			if (header.length() > 0)
				header.append(",");
			header.append(escapeCsv(name));
		}
		buf.append(header);
		buf.append("\n");

		// データ
		for (int row : selections) {
			StringBuffer line = new StringBuffer();
			for (int j = 0; j < col; j++) {
				// 列幅が0は出力しない
				TableColumn column = columnModel.getColumn(j);
				int width = column.getPreferredWidth();
				if (width <= 0)
					continue;
				String name = model.getColumnName(j);
				// 列名が空は出力しない。
				if (name == null || name.isEmpty())
					continue;

				// データ
				String value = model.getValueAt(row, j) != null ? model
						.getValueAt(row, j).toString() : "";
				if (line.length() > 0)
					line.append(",");
				line.append(escapeCsv(value));
			}
			buf.append(line);
			buf.append("\n");
		}

		return buf.toString();
	}

	/**
	 * TreeNodeからCSV文字列に変換する
	 * 
	 * @param node
	 *            ツリーノード
	 * @param depth
	 *            階層
	 * @return CSV文字列
	 */
	private static String toCsv(TreeNode node, int depth) {
		StringBuffer buf = new StringBuffer();
		String depthtext = "";
		for (int i = 0; i < depth; i++) {
			depthtext += ",";
		}
		buf.append(depthtext + escapeCsv(node.toString()));
		buf.append("\n");
		for (int i = 0; i < node.getChildCount(); i++) {
			String child = toCsv(node.getChildAt(i), depth + 1);
			buf.append(child);
		}

		return buf.toString();
	}

	/**
	 * CSV出力文字列をエスケープする.<br/>
	 * ダブルクォーテーションは'\'を付加する.<br/>
	 * 文字列中にカンマ、改行が存在する場合は、全体をダブルクォーテーションで囲む.<br/>
	 * 
	 * @param text
	 *            CSV出力文字列
	 * @return エスケープ文字列
	 */
	public static String escapeCsv(String text) {
		if (text == null)
			return null;
		text = text.replaceAll("\"", "\\\"");
		if (text.indexOf(",") >= 0 || text.indexOf("\n") >= 0) {
			text = "\"" + text + "\"";
		}
		return text;
	}

	/**
	 * 2つのビューを同期する
	 * 
	 * @param masterViewport
	 *            元ビュー
	 * @param slaveViewport
	 *            同期先ビュー
	 * @param orientation
	 *            同期方向(SwingConstants.HORIZONTAL(0x00)=水平方向,
	 *            SwingConstants.VERTICAL(0x01)=垂直方向)
	 */
	public static void synchronizeView(final JViewport masterViewport,
			final JViewport slaveViewport, final int orientation) {
		final ChangeListener c1 = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (masterViewport.getView() == null
						|| slaveViewport.getView() == null) {
					return;
				}
				if (orientation == SwingConstants.HORIZONTAL) {
					Point v1 = masterViewport.getViewPosition();
					Point v2 = slaveViewport.getViewPosition();
					if (v1.x != v2.x) {
						slaveViewport.setViewPosition(new Point(v1.x, v2.y));
					}
				} else if (orientation == SwingConstants.VERTICAL) {
					Point v1 = masterViewport.getViewPosition();
					Point v2 = slaveViewport.getViewPosition();
					if (v1.y != v2.y) {
						slaveViewport.setViewPosition(new Point(v2.x, v1.y));
					}
				}
			}
		};

		masterViewport.addChangeListener(c1);
	}

	/**
	 * 文字列をクリップボードにコピーする.
	 * 
	 * @param text
	 *            クリップボードコピー文字列
	 */
	public static void copyClipboard(String text) {
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		StringSelection selection = new StringSelection(text);
		clipboard.setContents(selection, selection);

		return;
	}

	/**
	 * グラデーション色をRGBで算出する.
	 * 
	 * @param ratio
	 *            グラデーション色の比率(0.0 = minColor ～ 1.0 = maxColor)
	 * @param minColor
	 *            比率=0.0の時の色
	 * @param maxColor
	 *            比率=1.0の時の色
	 * @return グラデーション色
	 */
	public static Color getGradientRgbColor(float ratio, Color minColor,
			Color maxColor) {
		if (ratio < 0)
			ratio = 0.0F;
		if (ratio > 1.0)
			ratio = 1.0F;
		int red = (int) (maxColor.getRed() * ratio + minColor.getRed()
				* (1.0 - ratio));
		int green = (int) (maxColor.getGreen() * ratio + minColor.getGreen()
				* (1 - ratio));
		int blue = (int) (maxColor.getBlue() * ratio + minColor.getBlue()
				* (1 - ratio));
		Color gradient = new Color(red, green, blue);
		return gradient;
	}

	/**
	 * グラデーション色をHSBで算出する.
	 * 
	 * @param ratio
	 *            グラデーション色の比率(0.0 = minColor ～ 1.0 = maxColor)
	 * @param minColor
	 *            比率=0.0の時の色
	 * @param maxColor
	 *            比率=1.0の時の色
	 * @return グラデーション色
	 */
	public static Color getGradientHsbColor(float ratio, Color minColor,
			Color maxColor) {
		float[] startHSB = Color.RGBtoHSB(minColor.getRed(),
				minColor.getGreen(), minColor.getBlue(), null);
		float[] endHSB = Color.RGBtoHSB(maxColor.getRed(), maxColor.getGreen(),
				maxColor.getBlue(), null);

		float brightness = (startHSB[2] + endHSB[2]) / 2;
		float saturation = (startHSB[1] + endHSB[1]) / 2;

		float hueMax = 0;
		float hueMin = 0;
		// if (startHSB[0] > endHSB[0]) {
		hueMax = startHSB[0];
		hueMin = endHSB[0];
		// } else {
		// hueMin = startHSB[0];
		// hueMax = endHSB[0];
		// }

		float hue = ((hueMax - hueMin) * (1.0F - ratio)) + hueMin;

		return Color.getHSBColor(hue, saturation, brightness);
	}

	/**
	 * ブロックツリーからブロックリストのみのツリーを作成する.
	 * 
	 * @param root
	 *            ブロックツリー
	 * @param list
	 *            ブロックリスト
	 * @return ブロックリストのみのツリー
	 */
	public static DefaultMutableTreeNode createTreeNode(
			DefaultMutableTreeNode root, List<Object> list) {
		if (root == null)
			return null;
		if (list == null || list.size() <= 0)
			return null;
		DefaultMutableTreeNode top = new DefaultMutableTreeNode();
		if (list.get(0) instanceof BlockList) {
			for (Object obj : list) {
				DefaultMutableTreeNode node = new DefaultMutableTreeNode(obj);
				if (top.getChildCount() == 0) {
					top.add(node);
				} else {
					((DefaultMutableTreeNode) top.getChildAt(0)).add(node);
				}
			}
			return top;
		}
		for (Object obj : list) {
			DefaultMutableTreeNode node = new DefaultMutableTreeNode(obj);
			if (top.getChildCount() == 0) {
				top.add(node);
			} else {
				Object[] objpath = searchTreePath(root, obj);
				DefaultMutableTreeNode parent = searchParentNode(top, objpath);
				if (parent != null) {
					parent.add(node);
				} else {
					top.add(node);
				}
			}
		}

		return top;
	}

	/**
	 * ツリーノードからユーザオブジェクトと一致するノードのユーザオブジェクトパスリストを取得する.<br/>
	 * 
	 * @param root
	 *            ツリーノード
	 * @param obj
	 *            ユーザオブジェクト
	 * @return ユーザオブジェクトパスリスト
	 */
	private static Object[] searchTreePath(DefaultMutableTreeNode root,
			Object obj) {

		// ツリーノードを順方向で列挙
		Enumeration<?> depth = root.preorderEnumeration();
		while (depth.hasMoreElements()) {
			DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth
					.nextElement();

			// ノード検索を行う
			if (treeNode == null || treeNode.getUserObject() == null) {
				continue;
			}
			if (treeNode.getUserObject() == obj) {
				return treeNode.getUserObjectPath();
			}
		}
		return null;
	}

	/**
	 * ツリーノードからユーザオブジェクトパスリストと一致するノードを検索する.<br/>
	 * ユーザオブジェクトパスリストに最も一致しているノードを取得する.<br/>
	 * ユーザオブジェクトパスリストと同一ノードではない.
	 * 
	 * @param root
	 *            ツリーノード
	 * @param objpath
	 *            ユーザオブジェクトパスリスト
	 * @return 一致ノード
	 */
	private static DefaultMutableTreeNode searchParentNode(
			DefaultMutableTreeNode root, Object[] objpath) {

		// ツリーノードを順方向で列挙
		int depthCount = -1;
		DefaultMutableTreeNode depthNode = root;
		Enumeration<?> depth = root.preorderEnumeration();
		while (depth.hasMoreElements()) {
			DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth
					.nextElement();
			// ノード検索を行う
			if (treeNode == null || treeNode.getUserObject() == null) {
				continue;
			}

			for (int i = 0; i < objpath.length; i++) {
				if (((DefaultMutableTreeNode) treeNode).getUserObject() == objpath[i]) {
					if (depthCount < i) {
						depthCount = i;
						depthNode = treeNode;
					}
				}
			}
		}
		if (depthCount == -1)
			return null;
		return depthNode;
	}

	/**
	 * 元ノードのコピーを取得する.
	 * 
	 * @param srcNode
	 *            元ノード
	 * @return ノードのコピー
	 */
	public static DefaultMutableTreeNode cloneTreeNode(
			DefaultMutableTreeNode srcNode) {
		if (srcNode == null)
			return null;
		DefaultMutableTreeNode node = new DefaultMutableTreeNode(
				srcNode.getUserObject());
		for (int i = 0; i < srcNode.getChildCount(); i++) {
			DefaultMutableTreeNode child = (DefaultMutableTreeNode) srcNode
					.getChildAt(i);
			addCloneNode(child, node);
		}
		return node;
	}

	/**
	 * 追加子ノードのコピーノードを親ノードに追加する.
	 * 
	 * @param srcNode
	 *            追加子ノード
	 * @param root
	 *            追加親ノード
	 * @return 追加子ノードのコピーノード
	 */
	private static DefaultMutableTreeNode addCloneNode(
			DefaultMutableTreeNode srcNode, DefaultMutableTreeNode root) {
		DefaultMutableTreeNode clone = new DefaultMutableTreeNode(
				srcNode.getUserObject());
		root.add(clone);
		for (int i = 0; i < srcNode.getChildCount(); i++) {
			DefaultMutableTreeNode child = (DefaultMutableTreeNode) srcNode
					.getChildAt(i);
			addCloneNode(child, clone);
		}
		return clone;
	}

	/**
	 * 循環参照ノードであるかチェックする.
	 * 
	 * @param node
	 *            チェックノード
	 * @return true=循環参照ノード
	 */
	public static boolean recursiveTreeNode(DefaultMutableTreeNode node) {
		if (node == null)
			return false;
		Object[] objs = node.getUserObjectPath();
		if (objs == null)
			return false;
		for (int i = 0; i < objs.length; i++) {
			for (int j = i; j < objs.length; j++) {
				if (i == j)
					continue;
				if (objs[i] == objs[j]) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * ツリーノードのすべての子孫ノード数を取得する.
	 * 
	 * @param node
	 *            ツリーノード
	 * @return 子孫ノード数
	 */
	public static int getAllChildCount(TreeNode node) {
		if (node == null)
			return 0;
		int count = 0;
		int childCount = node.getChildCount();
		for (int i = 0; i < childCount; i++) {
			count++;
			TreeNode child = node.getChildAt(i);
			count += getAllChildCount(child);
		}
		return count;
	}

	/**
	 * ツリーを昇順にソートを行う.
	 * 
	 * @param root
	 *            ソート対象ツリー
	 * @return ソート結果ツリー
	 */
	public static DefaultMutableTreeNode sortTreeNode(
			DefaultMutableTreeNode root) {
		if (root == null)
			return root;
		int count = root.getChildCount();
		if (count <= 0)
			return root;
		for (int i = 0; i < count; i++) {
			for (int j = count - 1; j > i; j--) {
				DefaultMutableTreeNode node = (DefaultMutableTreeNode) root
						.getChildAt(j);
				String nt = node.getUserObject().toString();
				DefaultMutableTreeNode prevNode = (DefaultMutableTreeNode) root
						.getChildAt(j - 1);
				String np = prevNode.getUserObject().toString();
				if (nt.compareToIgnoreCase(np) < 0) {
					root.insert(node, j - 1);
					root.insert(prevNode, j);
				}
			}
			sortTreeNode((DefaultMutableTreeNode) root.getChildAt(i));
		}

		for (int i = 0; i < count; i++) {
			for (int j = count - 1; j > i; j--) {
				DefaultMutableTreeNode node = (DefaultMutableTreeNode) root
						.getChildAt(j);
				DefaultMutableTreeNode prevNode = (DefaultMutableTreeNode) root
						.getChildAt(j - 1);
				if (prevNode.isLeaf() && !node.isLeaf()) {
					root.insert(node, j - 1);
					root.insert(prevNode, j);
				}
			}
		}

		return root;
	}

	/**
	 * "Meiryo UI", "Dialog"のフォントを取得する.<br/>
	 * "Meiryo UI", "Dialog"のフォントが存在しなければ、"Monospaced"フォントとする.
	 * 
	 * @return デフォルトフォント
	 */
	public static Font getDefaultFont() {
		final int DEFAULTFONT_SIZE = 11;
		final String[] defaultnames = { "Meiryo UI", "Dialog" };
		// フォント名リストの取得
		GraphicsEnvironment env = GraphicsEnvironment
				.getLocalGraphicsEnvironment();
		String[] fontNames = env.getAvailableFontFamilyNames();

		// デフォルト、論理フォントを設定
		Font defaultFont = new Font("Monospaced", Font.PLAIN, DEFAULTFONT_SIZE);

		// 物理フォントの探索
		if (fontNames != null && fontNames.length > 0) {
			for (int j = 0; j < defaultnames.length; j++) {
				String name = defaultnames[j];
				for (int i = 0; i < fontNames.length; i++) {
					if (fontNames[i].equals(name)) {
						defaultFont = new Font(name, Font.PLAIN,
								DEFAULTFONT_SIZE);
						return defaultFont;
					}
				}
			}
		}
		return defaultFont;
	}

	/**
	 * TreeNodeからツリー形式の文字列に変換する
	 * 
	 * @param node
	 *            ツリーノード
	 * @return CSV文字列
	 */
	public static String toTreeText(TreeNode node) {
		StringBuffer buf = new StringBuffer();
		Enumeration<?> elems = ((DefaultMutableTreeNode) node)
				.preorderEnumeration();
		try {
			while (elems.hasMoreElements()) {
				DefaultMutableTreeNode next = (DefaultMutableTreeNode) elems
						.nextElement();
				Object obj = next.getUserObject();
				String text = null;
				File file = null;
				if (obj instanceof SourceFile) {
					file = ((SourceFile) obj).getFile();
				} else if (obj instanceof File) {
					file = (File) obj;
				}
				if (file != null) {
					if (next.isRoot()) {
						text = file.getCanonicalPath();
					} else {
						text = file.getName();
					}
				} else {
					text = next.toString();
				}

				StringBuffer leaf = new StringBuffer();
				TreeNode[] paths = next.getPath();
				DefaultMutableTreeNode previous = (DefaultMutableTreeNode) paths[0];
				for (int i = 0; i < paths.length - 1; i++) {
					if (previous == paths[i])
						continue;
					if (previous.getLastChild() == paths[i]) {
						leaf.append("    ");
					} else {
						leaf.append("|   ");
					}
				}
				DefaultMutableTreeNode parent = (DefaultMutableTreeNode) next
						.getParent();
				if (parent == null) {
					leaf.append("");
				} else if (parent.getLastChild() == next) {
					leaf.append("`-- ");
				} else {
					leaf.append("|-- ");
				}
				buf.append(leaf);
				buf.append(text);
				buf.append("\n");
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		return buf.toString();
	}

}
