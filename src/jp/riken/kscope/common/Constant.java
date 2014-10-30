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
package jp.riken.kscope.common;


/**
 * 定数クラス
 * @author RIKEN
 *
 */
public class Constant {

    /** ダイアログOK, YES戻り値 */
    public static final int OK_DIALOG = 0;
    /** ダイアログNO戻り値 */
    public static final int NO_DIALOG = 1;
    /** ダイアログCANCEL戻り値 */
    public static final int CANCEL_DIALOG = 2;
    /** ダイアログCANCEL戻り値 */
    public static final int CLOSE_DIALOG = -1;
    /** ダイアログDELETE戻り値 */
    public static final int DELETE_DIALOG = 3;
    /** ダイアログ次への戻り値:次のダイアログを表示する */
    public static final int NEXT_DIALOG = 4;

    /** スレッドの終了コード:正常終了 */
    public static final int SUCCESS_RESULT = 0;
    /** スレッドの終了コード:異常終了 */
    public static final int ERROR_RESULT = 1;
    /** スレッドの終了コード:キャンセル終了 */
    public static final int CANCEL_RESULT = 2;

    /** スレッド終了通知プロパティ名 */
    public static final String PROPERTYNAME_THREADDONE = "thread_done";
    /** マウススクロール量 */
    public static final int VERTICALSCROLL_INCREMENT = 25;

}
