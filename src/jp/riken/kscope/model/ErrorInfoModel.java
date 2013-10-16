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
import java.util.List;
import java.util.Observable;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * エラー情報モデル
 * @author riken
 *
 */
public class ErrorInfoModel extends Observable {

    /**
     * テーブルヘッダーリスト
     * 1列目はCodeLine情報とする。
     */
    private String[] HEADER_COLUMNS = {"",
        Message.getString("settingprojectdialog.column_header.message"), //メッセージ
        Message.getString("mainmenu.file"), //ファイル
        Message.getString("languageservice.properties.linenumber")}; //行番号

    /** タイトル */
    private String title;

    /** プロジェクトフォルダ */
    private File projectFolder = null;

    /** エラー情報リスト */
    private List<ErrorInfo> listError = null;

    /**
     * コンストラクタ
     */
    public ErrorInfoModel() {
        super();

        // テーブルモデルの作成
        createTableModel();
    }

    /**
     * モデルの変更を通知する
     */
    private void notifyModel() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
		        setChanged();
		        notifyObservers();
		        clearChanged();
            }
        });
    }

    /**
     * テーブルモデルを取得する
     * @return		テーブルモデル
     */
    public DefaultTableModel getTableModel() {
        return createTableModel();
    }


    /**
     * テーブルモデルを作成する
     */
    private DefaultTableModel createTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel();
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);

        // エラーリストからテーブルモデルの作成を行う。
        if (listError == null) return tableModel;
        CopyOnWriteArrayList<ErrorInfo> copyList = new CopyOnWriteArrayList<ErrorInfo>(listError);
        for (ErrorInfo error : copyList) {
            CodeLine line = error.getCodeLine();
            String message = error.getMessage();
            String filename = null;
            if (line != null) {
            	if (line.getSourceFile() != null && line.getSourceFile().getFile() != null){
            		File file = line.getSourceFile().getFile();
                    filename = getRelativePath(file);
                    if (filename == null)
                    	filename = line.getStrSourceFile();
            	}
            	else {
            		filename = line.getStrSourceFile();
            	}
            }

            String no = null;
            if (line != null) {
                no = line.getLineno();
            }

            // テーブル行配列の作成
            Object[] row = {line, message, filename, no};

            // テーブル行追加
            tableModel.addRow(row);
        }

        return tableModel;
    }

    /**
     * ヘッダー列リストを取得する。
     * @return		ヘッダー列リスト
     */
    public String[] getHeaderColumns() {
        return HEADER_COLUMNS;
    }

    /**
     * エラーメッセージを追加する
     * @param lineInfo          エラー箇所情報
     * @param message			エラーメッセージ
     */
    public void addErrorInfo(CodeLine lineInfo, String message) {

        // エラーメッセージをテーブルモデルに行追加する
        addTableRow(lineInfo, message);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * エラーメッセージを追加する
     * @param infos          エラー情報リスト
     */
    public void addErrorInfos(ErrorInfo[] infos) {
    	if (infos == null) return;

        // エラーメッセージをテーブルモデルに行追加する
        addTableRows(infos);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * エラーメッセージをエラー情報リストに追加する
     * @param lineInfo          エラー箇所情報
     * @param message			エラーメッセージ
     */
    private void addTableRow(CodeLine lineInfo, String message) {
        if (this.listError == null) {
            this.listError = new ArrayList<ErrorInfo>();
        }
        listError.add(new ErrorInfo(lineInfo, message));
    }


    /**
     * エラー情報リストをエラー情報リストに追加する
     * @param infos          エラー情報リスト
     */
    private void addTableRows(ErrorInfo[] infos) {
    	if (infos == null) return;
        if (this.listError == null) {
            this.listError = new ArrayList<ErrorInfo>();
        }
        listError.addAll(Arrays.asList(infos));
    }

    /**
     * エラー情報をエラー情報リストに追加する
     * @param info          エラー情報
     */
    private void addTableRow(ErrorInfo info) {
    	if (info == null) return;
        if (this.listError == null) {
            this.listError = new ArrayList<ErrorInfo>();
        }
        listError.add(info);
    }
    
    /**
     * エラーメッセージを追加する
     * @param message			エラーメッセージ
     */
    public void addErrorInfo( String message) {

        // エラーメッセージをエラー情報リストに追加する
        addTableRow(null, message);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * エラーメッセージを追加する
     * @param ex			エラー発生例外
     */
    public void addErrorInfo( Exception ex) {
        String message = ex.getMessage();
        if (message == null) {
            message = ex.toString();
        }
        // エラーメッセージをエラー情報リストに追加する
        addTableRow(null, message);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * エラーメッセージを追加する
     * @param file				エラーファイル
     * @param message			エラーメッセージ
     */
    public void addErrorInfo(SourceFile file, String message) {
        addErrorInfo(file, message, 0);
    }

    /**
     * エラーメッセージを追加する
     * @param file				エラーファイル
     * @param message			エラーメッセージ
     * @param lineno			エラー行番号
     */
    public void addErrorInfo(SourceFile file, String message, int lineno) {

    	String fn = null;
    	if (file != null) {
    		fn = file.getPath();
    	}
        CodeLine line = new CodeLine(file, lineno, lineno, fn);

        // エラーメッセージをエラー情報リストに追加する
        addTableRow(line, message);

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * エラーメッセージを追加する
     * @param lineInfos          エラー箇所情報
     * @param messages			エラーメッセージ
     */
    public void setErrorInfoList(CodeLine lineInfos[], String messages[]) {
        // テーブルモデルの作成
        createTableModel();

        for (int i=0; i<lineInfos.length; i++) {
            // エラーメッセージをテーブルモデルに行追加する
            addTableRow(lineInfos[i], messages[i]);
        }

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * テーブルモデルをクリアする。
     */
    public void clearErrorList() {
        if (this.listError == null) {
            this.listError = new ArrayList<ErrorInfo>();
        }
        this.listError.clear();

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * エラー情報リストを取得する
     * @return		エラー情報リスト
     */
    public List<ErrorInfo> getErrorList() {
        return this.listError;
    }

    /**
     * エラー情報リスト数を取得する
     * @return		エラー情報リスト数
     */
    public int getErrorListCount() {
        if (this.listError == null) return 0;
        return this.listError.size();
    }


    /**
     * タイトルを取得する
     * @return	タイトル
     */
    public String getTitle() {
        return title;
    }

    /**
     * タイトルを設定する
     * @param title		タイトル
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * プロジェクトフォルダからの相対パスを取得する
     * @param file		対象ファイル
     * @return			相対パス
     */
    private String getRelativePath(File file) {
        if (file == null) return null;
        if (this.projectFolder == null) {
            return file.getAbsolutePath();
        }

        // 相対パスの取得を行う
        return FileUtils.getRelativePath(file, this.projectFolder);
    }

    /**
     * プロジェクトフォルダを設定する
     * @param folder		プロジェクトフォルダ
     */
    public void setProjectFolder(File folder) {
        this.projectFolder = folder;
    }

    /**
     * テーブル情報をファイル出力する。
     * @param  file   出力ファイル
     */
    public void writeFile(File file) {

        // エラーテーブルモデル
        DefaultTableModel tableModel = createTableModel();

        if (tableModel == null) return;

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

            // テーブルデータ
            String buf = SwingUtils.toCsv(tableModel);
            // ファイル出力
            pw.print(buf);

            pw.close();

        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }

    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
    	if (this.listError == null) return true;
    	return (this.listError.size() < 1);
    }

    /**
     * エラーメッセージを追加する
     * @param error          エラー情報
     */
	public void addErrorInfo(ErrorInfo error) {
    	if (error == null) return;

        // エラーメッセージをテーブルモデルに行追加する
        addTableRow(error);

        // モデルの変更を通知
        notifyModel();
	}
}


