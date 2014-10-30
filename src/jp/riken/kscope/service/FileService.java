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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.utils.TextFileReader;


/**
 * ファイル管理を行うサービスクラス
 * @author RIKEN
 */
public class FileService extends BaseService {

    /** 文字コード */
    private Charset charset;

    /**
     * コンストラクタ
     */
    public FileService() {
    }

    /**
     * コンストラクタ
     * @param   errorModel  エラーモデル
     */
    public FileService(ErrorInfoModel errorModel) {
        super(errorModel);
    }

    /**
     * ファイルプロパティを設定する
     * @param file		プロパティ取得ファイル
     * @param model		プロパティ設定モデル
     */
    public void setFileProperties(File file, PropertiesTableModel model) {
        String[] items = {
            Message.getString("fileservice.fileproperties.name"), //名前
            Message.getString("fileservice.fileproperties.location"), //ロケーション
            Message.getString("fileservice.fileproperties.size"), //サイズ
            Message.getString("fileservice.fileproperties.kind"), //種類
            Message.getString("fileservice.fileproperties.update")}; //更新日時
        String[] values = new String[5];

        // 名前
        values[0] = file.getName();
        // ロケーション
        try {
            values[1] = file.getCanonicalPath();
        } catch (IOException e) {
        }

        if (file.isDirectory()) {
            values[2] = "";
            values[3] = Message.getString("fileservice.fileproperties.folder"); //フォルダ
        }
        else if (file.isFile()) {
            // サイズ
            float kbyte = (float) (file.length() / 1000.0);
            values[2] = String.format("%#.1f KB", kbyte);
            // 種類
            values[3] = Message.getString("fileservice.fileproperties.file"); //ファイル
        }
        // 更新日時
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis( file.lastModified() );
        values[4] = new SimpleDateFormat(Message.getString("fileservice.fileproperties.dateformat")).format(calendar.getTime()); //YYYY年MM月DD日 HH:MM:SS

        // モデルに設定する
        // プロパティパネルへの通知はObserverにて通知される。
        model.setTitle(file.getName());
        model.setProperties(items, values);

    }

    /**
     * コード行をファイル出力する
     * @param srcfile		ソースファイル
     * @param lines		コード行リスト
     * @throws Exception		読込例外
     */
    public void writeFile(File srcfile, CodeLine[] lines) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (CodeLine line : lines) {
            buf.append(line.getStatement());
            buf.append("\n");
        }
        if (this.charset == null) {
            // UTF-8でファイル出力する
            this.charset = Charset.forName("UTF-8");
        }
        writeFile(srcfile, buf.toString(), this.charset);
    }


    /**
     * コード行をファイル出力する
     * @param srcfile		ソースファイル
     * @param lines		コード行リスト
     * @throws Exception		読込例外
     */
    public void writeFile(SourceFile srcfile, CodeLine[] lines) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (CodeLine line : lines) {
            buf.append(line.getStatement());
            buf.append("\n");
        }
        if (this.charset == null) {
            // UTF-8でファイル出力する
            this.charset = Charset.forName("UTF-8");
        }
        writeFile(srcfile.getFile(), buf.toString(), this.charset);

    }


    /**
     * コード行をファイル出力する
     * @param srcfile		ソースファイル
     * @param text			出力文字列
     * @throws Exception		書込例外
     */
    public void writeFile(SourceFile srcfile, String text) throws Exception {
        if (this.charset == null) {
            // UTF-8でファイル出力する
            this.charset = Charset.forName("UTF-8");
        }
        writeFile(srcfile.getFile(), text, this.charset);
    }


    /**
     * コード行を出力文字コードでファイル出力する
     * @param outfile		ソースファイル
     * @param text			出力文字列
     * @param charset		出力文字コード(nullの場合は、UTF-8とする)
     * @throws Exception	書込例外
     */
    private void writeFile(File outfile, String text, Charset charset) throws Exception {

        try {
            // フォルダの生成
            File folder = outfile.getParentFile();
            if (!folder.exists()) {
                folder.mkdirs();
            }
            // 出力ストリームの生成
            FileOutputStream fs = new FileOutputStream(outfile);
            if (charset == null) {
                charset = Charset.defaultCharset();
            }
            PrintWriter pw = new PrintWriter(new OutputStreamWriter(fs, charset));

            // ファイルへの書き込み
            pw.print(text);

            // 後始末
            pw.close();
            fs.close();

        } catch(Exception ex) {
            this.addErrorInfo(ex);
            throw ex;
        }
    }


    /**
     * ファイルからコード行を取得する
     * @param file			ソースファイル
     * @param parentPath	親フォルダ
     * @return				コード行リスト
     * @throws Exception    読込エラー
     */
    public CodeLine[] readSourceFile(SourceFile file, File parentPath) throws Exception {
        if (file == null) return null;
        File readfile = file.getFile();
        if (readfile == null) return null;
        if (!readfile.isAbsolute() && parentPath != null) {
            readfile = new File(parentPath, readfile.getPath());
        }
        ArrayList<CodeLine> list = new ArrayList<CodeLine>();
        try {
            TextFileReader reader = new TextFileReader(readfile);
            String line;
            int line_no = 0;
            while ((line = reader.readLine()) != null) {
                line_no++;
                // コードラインの生成、追加を行う
                list.add(new CodeLine(file, line, line_no, file.getPath()));
            }
            // 文字コードをセットする
            this.charset = reader.getCharset();

        } catch (Exception ex) {
            this.addErrorInfo(ex);
            throw ex;
        }

        if (list.size() <= 0) return null;

        // 読込コード行リスト
        return list.toArray(new CodeLine[0]);
    }

    /**
     * 文字コードを取得する
     * @return 		文字コード
     */
    public Charset getCharset() {
        return charset;
    }

    /**
     * 文字コードを設定する
     * @param charset 	文字コード
     */
    public void setCharset(Charset charset) {
        this.charset = charset;
    }

}
