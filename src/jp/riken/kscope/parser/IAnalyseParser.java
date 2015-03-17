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

package jp.riken.kscope.parser;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Fortran;

/**
 * フォートラン構文解析インターフェイス
 * @author RIKEN
 */
public interface IAnalyseParser extends PropertyChangeListener {

    /**
     * 構文解析をキャンセルする
     * @param cancel		true=キャンセル
     */
    public void setCancel(boolean cancel);

    /**
     * インクルードファイルが存在しないときに確認ダイアログを表示するフラグを設定する
     * @param confirm		true=確認ダイアログを表示する
     */
    public void setConfirmInclude(boolean confirm);

    /**
     * ソースファイルから読込を行う
     * @param file		ソースファイル
     * @throws IOException		ファイル読込エラー
     */
    public void readFile(SourceFile file) throws IOException;

    /**
     * ソースファイルを構文解析してフォートランデータベースに設定する
     * @param ft		フォートランデータベース
     * @throws InterruptedException		割り込みエラー
     */
    public void parseFile(Fortran ft) throws InterruptedException;

    /**
     * プロパティ変更リスナを追加する
     * @param listener		プロパティ変更リスナ
     */
    public void addPropertyChangeListener(PropertyChangeListener listener);

    /**
     * プロパティ変更リスナを削除する
     * @param listener		プロパティ変更リスナ
     */
    public void removePropertyChangeListener(PropertyChangeListener listener);

    /**
     * プロパティ変更イベントを発生させる。
     * @param propertyName		プロパティ名
     * @param oldValue			プロパティ旧値
     * @param newValue			プロパティ新値
     */
    public void firePropertyChange(String propertyName, Object oldValue, Object newValue);

    /**
     * プロパティ変更イベント
     * @param evt		プロパティ変更イベント情報
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt);

    /**
     * ソースコード行リストを取得する
     * @return		ソースコード行リスト
     * @throws InterruptedException		割り込みエラー
     */
    public CodeLine[] getCodeLineList() throws InterruptedException;

    /**
     * ソースファイルを取得する
     * @return		ソースファイル
     */
    public SourceFile getLanguageFile();

    /**
     * XMLファイルからソースファイルのみパースする
     */
    public void parseSourceFile();

    /**
     * ソースファイルの基準フォルダを設定する
     * @param	folder	ソースファイルの基準フォルダ
     */
    public void setBaseFolder(File folder);

    /**
     * エラー情報を取得する.
     * @return		エラー情報リスト
     */
    public ErrorInfo[] getErrorInfos();


    /**
     * ソースファイルの基準フォルダを取得する
     * @return   ソースファイルの基準フォルダ
     */
    public File getBaseFolder();
}
