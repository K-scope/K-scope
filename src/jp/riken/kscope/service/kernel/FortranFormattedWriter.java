/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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

package jp.riken.kscope.service.kernel;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jp.riken.kscope.utils.LanguageTokenizer;
import jp.riken.kscope.utils.StringUtils;

/**
 * ラインバッファーライタクラス
 * 指定桁数以上の行は改行を挿入する。
 * @author RIKEN
 */
public class FortranFormattedWriter extends StringWriter {

    /** 改行コード */
    protected static final byte CODE_LF = 10;
    protected static final int DEFAULT_LIMIT_COLUMN = 82;
    protected static final String DEFAULT_SURFIX = "&";

    /** 出力ファイル名 */
    private String  output_filename;
    /** ラインバッファー */
    private StringBuffer line_buf;
    /** ライン制限桁数 */
    private int limit_column = DEFAULT_LIMIT_COLUMN;
    /** 改行接尾語文字 */
    private String suffix = DEFAULT_SURFIX;
    /** 改行コード */
    private String endofline = "\n";
    /** 次行インデント幅 */
    private int next_indent = 4;
    /** 最大インデント幅 */
    private int limit_indent = 40;
    /** コメント接頭文字 */
    private List<String> comments = new ArrayList<String>();
    /** 現在行のインデント */
    private int current_indent;
    private List<String> remove_paths;
    private String[] none_indents = {"!ocl", "!$omp"};

    /**
     * コンストラクタ
     */
    public FortranFormattedWriter() {
        this.output_filename = null;
        this.clearLineBuffer();

        String path = null;
        this.remove_paths = new ArrayList<String>();
        this.remove_paths.add("templates/kernel/default");
        this.remove_paths.add("kernel/default");
        if (!"/".equals(File.separator)) {
            path = "templates" + File.separator + "kernel" + File.separator + "default";
            this.remove_paths.add(path);
            path = "kernel" + File.separator + "default";
            this.remove_paths.add(path);
        }
    }

    /**
     * 単一の文字を書き込みます。
     * @param  c   文字
     */
    @Override
    public void write(int c) {
        if (c == 0) return;

        this.line_buf.append(c);
        if (c == CODE_LF) {
            this.writeLineBuffer();
        }
    }

    /**
     * ストリームに書込みを行う。
     */
    @Override
    public void write(char[] cbuf, int off, int len) {
        if (cbuf == null) return;

        for (int i=off; i<off+len; i++) {
            if (i >= cbuf.length) break;
            char ch = cbuf[i];
            this.write((int)ch);
        }

        return;
    }

    /**
     * ストリームに書込みを行う。
     */
    @Override
    public void write(String str) {
        if (str == null) return;

        String buf = "";
        byte[] bytes = {CODE_LF};
        String lf = new String(bytes);
        for (int i=0; i<str.length(); i++) {
            String ch = str.substring(i, i+1);
            buf += ch;
            if (ch.equals(lf)) {
                this.line_buf.append(buf);
                this.writeLineBuffer();
                buf = "";
            }
        }
        if (buf.length() > 0) {
            this.line_buf.append(buf);
        }

        // インデントを取得する。
        setCurrentIndent(str);

        return;
    }

    /**
     * ストリームに書込みを行う。
     */
    @Override
    public void write(String str, int off, int len) {
        String buf = str.substring(off, off + len);
        this.write(buf);
    }

    /**
     * ストリームに書込みを行う。
     */
    @Override
    public void write(char[] cbuf) throws IOException {
        String buf = String.valueOf(cbuf);
        this.write(buf);
    }

    /**
     * ストリームをフラッシュします。
     * ラインバッファーに存在する文字列をストリームに追加する.
     */
    @Override
    public void flush() {
        this.write(this.line_buf.toString());
        this.clearLineBuffer();
        super.flush();
    }

    /**
     * ストリームをクローズします。
     */
    @Override
    public void close() throws IOException {
        this.flush();
        super.close();
    }

    /**
     * ラインバッファーをクリアする。
     */
    public void clearLineBuffer() {
        this.line_buf = new StringBuffer();
        this.current_indent = 0;
    }

    /**
     * ストリームサイズを取得する.
     */
    public int size() {
        if (super.getBuffer() == null) return 0;
        return super.getBuffer().length();
    }

    /**
     * ラインバッファーをストリームに追加する.
     * 指定桁数以上の場合は、改行を行う。
     */
    public void writeLineBuffer() {
        // 行番号の整形
        String line = this.formatLineNumber(this.line_buf.toString());
        if (line == null) {
            this.clearLineBuffer();
            return;
        }

        // ディレクティブ文のインデント削除
        if (this.isDelectiveLine(line)) {
            line = StringUtils.trimLeft(line);
        }

        if (line.length() <= this.limit_column) {
            super.write(line);
        }
        else {
            // ラインバッファーの改行
            String[] list = this.splitLine(line);
            if (list != null) {
                for (String str : list) {
                    if (this.validateLine(str)) {
                        super.write(str);
                    }
                }
            }
        }

        this.clearLineBuffer();
        return;
    }

    /**
     * ライン文字列のチェックを行う.
     *
     * @param str
     * @return
     */
    private boolean validateLine(String line) {
        String buf;
        buf = line.trim();
        if (buf.isEmpty()) return false;
        if ("&".equals(buf)) return false;


        return true;
    }

    /**
     * ライン先頭の行番号を整形する
     * ラインに行番号が存在する場合、行番号を先頭に移動する。
     * @param line        ラインバッファー
     * @return        整形文字列
     */
    private String formatLineNumber(String line) {
        if (line == null) return null;

        int line_len = line.length();
        // 途中から番号が始まる場合
        String regex = "^([ \t]+)([0-9]+)[ ]+(.*)";
        Pattern p = Pattern.compile(regex, Pattern.DOTALL);
        Matcher m = p.matcher(line);
        int top_indent = 0;
        if (m.find() && m.groupCount() >= 3){
            String indent_str = m.group(1);
            indent_str = indent_str.replaceAll("\t", "    ");
            top_indent = indent_str.length();
            String line_str = m.group(2);
            int n_line = Integer.parseInt(line_str);
            line_str = String.valueOf(n_line);
            String code = m.group(3);
            int num_sp = top_indent - line_str.length();
            if (num_sp <= 0) num_sp = 1;
            else if (num_sp+line_str.length() > this.limit_indent) {
                num_sp = this.limit_indent - line_str.length();
            }
            String formatted = line_str + StringUtils.repeat(" ", num_sp);
            formatted += code;

            return formatted;
        }
        else if (line_len > this.limit_indent) {
            regex = "^([ \t]+)(.*)";
            p = Pattern.compile(regex, Pattern.DOTALL);
            m = p.matcher(line);
            top_indent = 0;
            if (m.find() && m.groupCount() >= 2){
                String indent_str = m.group(1);
                indent_str = indent_str.replaceAll("\t", "    ");
                top_indent = indent_str.length();
                String code = m.group(2);
                if (top_indent > this.limit_indent) {
                    top_indent = this.limit_indent;
                    String formatted = StringUtils.repeat(" ", top_indent);
                    formatted += code;

                    return formatted;
                }
            }
        }

        return line;
    }

    /**
     * ラインバッファーを制限桁数で分割する
     * @param line        ラインバッファー
     * @return        分割文字列リスト
     */
    private String[] splitLine(String line) {
        if (line == null || line.isEmpty()) return null;

        // 先頭のインデントを取得する
        int top_indent = 0;
        String regex = "^[0-9 ]+";
        Pattern p = Pattern.compile(regex);
        Matcher m = p.matcher(line);
        if (m.find()){
            top_indent = m.end();
        }

        // ディレクティブ文のインデント
        String delective_prefix = this.getDelectivePrefix(line);

        List<String> list = new ArrayList<String>();
        try {
            LanguageTokenizer spliter = new LanguageTokenizer(line);

            // 区切り文字:'[' ']' ',' 'SPACE'
            spliter.useDelimiter("[");
            spliter.useDelimiter("]");
            spliter.useDelimiter(" ");
            spliter.useDelimiter("::");
            spliter.useDelimiter("//");
            spliter.useDelimiter("(");
            spliter.useDelimiter(")");
            spliter.useDelimiter(",");
            spliter.useDelimiter("**");
            spliter.useDelimiter("*");
            spliter.useDelimiter("/");
            // spliter.useDelimiter("+");
            // spliter.useDelimiter("-");
            // ディレクティブ文の場合、コメント設定は行わない
            if (delective_prefix == null) {
                for (String comment : this.comments) {
                    spliter.useDelimiter(comment);
                }
            }
            spliter.wordChar('\t');
            spliter.eolIsSignificant(true);

            String buf = new String();
            int ttype;
            boolean is_comment = false;
            int limit = this.limit_column - this.suffix.length();
            while ((ttype = spliter.nextToken()) != LanguageTokenizer.LT_EOF) {
                if (ttype == LanguageTokenizer.LT_EOL) {
                    buf += this.endofline;
                    continue;
                }
                if (spliter.sval == null || spliter.sval.length() <= 0) continue;
                if (this.comments.contains(spliter.sval)) {
                    is_comment = true;
                }
                if (buf.length() + spliter.sval.length() <= limit) {
                    buf += spliter.sval;
                }
                else if (is_comment) {
                    buf += spliter.sval;
                }
                else {
                    if (buf.trim().length() > 0) {
                        String trim = buf.trim();
                        if (!this.suffix.equals(trim)) {
                            buf += this.suffix + this.endofline;
                            list.add(buf);
                        }
                    }
                    // 新規行
                    if (top_indent + this.next_indent <= this.limit_indent) {
                        buf = StringUtils.repeat(" ", top_indent + this.next_indent);
                    }
                    else {
                        buf = StringUtils.repeat(" ", this.limit_indent);
                    }
                    // ディレクティブ文を付ける。
                    if (delective_prefix != null) {
                        buf = delective_prefix + buf;
                    }

                    if (list.size() > 0) buf += this.suffix;
                    buf += spliter.sval;
                }
            }
            if (buf.length() > 0) {
                list.add(buf);
            }

        } catch (IOException ex) {
            return null;
        }
        if (list.size() <= 0) {
            return null;
        }

        return list.toArray(new String[0]);
    }

    /**
     * ライン制限桁数を取得する.
     * @return limit_column        ライン制限桁数
     */
    public int getLimitColumn() {
        return this.limit_column;
    }

    /**
     * ライン制限桁数を設定する.
     * @param limit_column   ライン制限桁数
     */
    public void setLimitColumn(int limit_column) {
        this.limit_column = limit_column;
    }

    /**
     * 改行接尾語文字を取得する.
     * @return suffix        改行接尾語文字
     */
    public String getSuffix() {
        return this.suffix;
    }

    /**
     * 改行接尾語文字を設定する.
     * @param suffix     改行接尾語文字
     */
    public void setSuffix(String suffix) {
        this.suffix = suffix;
    }

    /**
     * 改行コードを取得する
     * @return endofline        改行コード
     */
    public String getEndofline() {
        return this.endofline;
    }

    /**
     * 改行コードを設定する.
     * @param endofline     改行コード
     */
    public void setEndofline(String endofline) {
        this.endofline = endofline;
    }

    /**
     * 次行インデント幅を取得する.
     * @return next_indent        次行インデント幅
     */
    public int getNextIndent() {
        return this.next_indent;
    }

    /**
     * 次行インデント幅を設定する.
     * @param next_indent  次行インデント幅
     */
    public void setNextIndent(int next_indent) {
        this.next_indent = next_indent;
    }

    /**
     * コメント接頭文字を取得する.
     * @return comments        コメント接頭文字
     */
    public List<String> getComments() {
        return this.comments;
    }

    /**
     * コメント接頭文字追加する。
     * @param comments コメント接頭文字
     */
    public void addComments(String comment) {
        if (comment == null || comment.isEmpty()) return;
        this.comments.add(comment);
    }

    /**
     * 出力ファイル名を取得する.
     * @return output_filename        出力ファイル名
     */
    public String getOutputFilename() {
        return this.output_filename;
    }

    /**
     * 出力ファイル名を設定する
     * @param output_filename   出力ファイル名
     */
    public void setOutputFilename(String filename) {
        this.output_filename = filename;
    }

    /**
     * ファイル出力する.
     * @param filename    出力ファイル名
     * @throws IOException
     */
    public void writeFile(File file) throws IOException {
        if (file == null) return;
        this.flush();
        StringBuffer buf = this.getBuffer();
        if (buf == null || buf.length() <= 0) return;

        // 出力パスの作成
        String dir = file.getParent();

        // mkdirs除外パス
        if (dir != null) {
            for (String path : this.remove_paths) {
                if (dir.endsWith(path)) {
                    dir = dir.substring(0, dir.length() - path.length());
                    file = new File(dir + file.getName());
                    break;
                }
            }
        }

        if (dir != null) {
            File outdir = new File(dir);
            if (!outdir.exists()) {
                // 出力パスの一括作成
                outdir.mkdirs();
            }
        }
        // ファイル出力
        BufferedWriter bwr = new BufferedWriter(new FileWriter(file));

        //write contents of StringBuffer to a file
        bwr.write(buf.toString());

        //flush the stream
        bwr.flush();

        //close the stream
        bwr.close();

        return;

    }

    /**
     * ファイル出力する.
     * @param filename    出力ファイル名
     * @throws IOException
     */
    public void writeFile(String filename) throws IOException {
        if (filename == null || filename.isEmpty()) return;

        // ファイル出力
        this.writeFile(new File(filename));

        return;
    }

    /**
     * ファイル出力する.
     * @throws IOException
     */
    public void writeFile() throws IOException {
        // ファイル出力
        this.writeFile(this.getOutputFilename());
        return;
    }


    /**
     * バッファーをクリアする.
     */
    public void clear() {
        this.clearLineBuffer();
        StringBuffer buf = this.getBuffer();
        buf.delete(0, buf.length());
    }

    /**
     * 現在行のインデント（先頭空白数）を設定する
     * @param line        書込文字列
     */
    private void setCurrentIndent(String line) {
        if (line == null || line.isEmpty()) return;

        String regexp = "^ +$";
        boolean match = Pattern.matches(regexp, line);
        if (match) {
            this.current_indent += line.length();
        }
        else {
            this.current_indent = 0;
        }
    }

    /**
     * 現在行のインデント（先頭空白数）を取得する.
     * @return        現在行のインデント（先頭空白数）
     */
    public int getCurrentIndent() {
        return this.current_indent;
    }

    /**
     * ディレクティブ文であるかチェックする.
     * @param line        ライン
     * @return        true = ディレクティブ文
     */
    private boolean isDelectiveLine(String line) {
        // ディレクティブ文
        for (String prefix : this.none_indents) {
            if (StringUtils.startsWithIgnoreCase(line.trim(), prefix)) {
                return true;
            }
        }

        return false;
    }


    /**
     * ディレクティブ文指示文接頭語を取得する.
     * @param line        ライン
     * @return        指示文接頭語
     */
    private String getDelectivePrefix(String line) {
        // ディレクティブ文
        for (String prefix : this.none_indents) {
            if (StringUtils.startsWithIgnoreCase(line.trim(), prefix)) {
                return line.trim().substring(0, prefix.length());
            }
        }

        return null;
    }
}


