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

package jp.riken.kscope.data;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;


/**
 * 拡張子設定データクラス
 * @author riken
 */
public class Program {

    /** 設定名 */
    private String name;
    /**
     * 拡張子又は正規表現<br/>
     * 拡張子の場合は、カンマ区切りで拡張子のみを記述する
     */
    private String pattern;
    /** 正規表現 */
    private boolean regex = false;
    /** 拡張子 */
    private boolean exts = false;
    /** 外部プログラム */
    private String exename;
    /** 関連付けプログラム */
    private boolean relation;
    /** オプション */
    private String option;

    /**
     * 設定名を取得する
     * @return		設定名
     */
    public String getName() {
        return name;
    }

    /**
     * 設定名を設定する
     * @param name		設定名
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * パターンを取得する
     * @return		パターン
     */
    public String getPattern() {
        return pattern;
    }

    /**
     * パターンを設定する
     * @param pattern		パターン
     */
    public void setPattern(String pattern) {
        this.pattern = pattern;
    }

    /**
     * パターンが正規表現であるか取得する
     * @return		true=正規表現
     */
    public boolean isRegex() {
        return regex;
    }

    /**
     * パターンを正規表現として設定する。
     * @param regex		true=正規表現
     */
    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    /**
     * パターンが拡張子であるか取得する
     * @return		true=拡張子
     */
    public boolean isExts() {
        return exts;
    }

    /**
     * パターンを拡張子として設定する
     * @param exts		true=拡張子
     */
    public void setExts(boolean exts) {
        this.exts = exts;
    }

    /**
     * プログラムパスを設定する
     * @return		プログラムパス
     */
    public String getExename() {
        return exename;
    }

    /**
     * プログラムパスを設定する
     * @param path		プログラムパス
     */
    public void setExename(String path) {
        this.exename = path;
    }

    /**
     * 関連付けプログラムとするか取得する
     * @return		true=関連付けプログラム
     */
    public boolean isRelation() {
        return relation;
    }

    /**
     * 関連付けプログラムをして設定する
     * @param relation		関連付けプログラム
     */
    public void setRelation(boolean relation) {
        this.relation = relation;
    }

    /**
     * パターンに設定の拡張子数を取得する
     * @return		拡張子数
     */
    public int getPatternExtsCount() {
        String[] exts = getPatternExts();
        if (exts == null || exts.length <= 0) return 0;
        return exts.length;
    }


    /**
     * パターンに設定の拡張子を取得する
     * @param index			インデックス
     * @return			拡張子
     */
    public String getPatternExt(int index) {
        String[] exts = getPatternExts();
        if (exts == null || exts.length <= 0) return null;
        if ( exts.length <= index) return null;

        return exts[index];
    }

    /**
     * パターンに設定の拡張子リストを取得する
     * @return			拡張子リスト
     */
    public String[] getPatternExts() {
        if (this.pattern == null || this.pattern.isEmpty()) return null;

        // カンマ区切り分解する
        String[] exts = this.pattern.split(",", 0);
        if (exts == null || exts.length <= 0) return null;

        List<String> list = new ArrayList<String>();
        for (int i=0; i<exts.length; i++) {
            if (exts[i] == null) continue;
            exts[i] = exts[i].trim();
            if (exts[i].isEmpty()) continue;
            list.add(exts[i]);
        }

        return list.toArray(new String[0]);
    }

    /**
     * オプションを取得する
     * @return option		オプション
     */
    public String getOption() {
        return this.option;
    }

    /**
     * オプションを設定する
     * @param value オプション
     */
    public void setOption(String value) {
        this.option = value;
    }

    /**
     * 検索文字と一致するプログラムであるかチェックする
     * @param text		検索文字
     * @return			true=一致プログラム
     */
    public boolean isMatchProgram(String text) {
        if (text == null || text.isEmpty()) return false;

        // 正規表現
        if (this.isRegex()) {
            String pattern = this.getPattern();
            return isMatchRegex(text, pattern);
        }
        else {
            // 拡張子
            int count = this.getPatternExtsCount();
            for (int i=0; i<count; i++) {
                String ext = this.getPatternExt(i);
                // 拡張子ファイルの検索正規表現
                String pattern = "^.+\\." + ext + "$";
                if (isMatchRegex(text, pattern)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * 正規表現で検索文字列が一致しているかチェックする
     * @param text			検索文字列
     * @param regexPattern		正規表現パターン
     * @return		true=一致
     */
    private boolean isMatchRegex(String text, String regexPattern) {
        if (text == null || text.isEmpty()) return false;
        if (regexPattern == null || regexPattern.isEmpty()) return false;

        // 正規表現によるマッチング
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
                                                regexPattern,
                                                java.util.regex.Pattern.CASE_INSENSITIVE+java.util.regex.Pattern.MULTILINE);
        Matcher m = pattern.matcher(text);
        return m.find();
    }
}


