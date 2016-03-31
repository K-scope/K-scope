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

package jp.riken.kscope;

import java.awt.Font;
import java.awt.GraphicsEnvironment;

import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.OceanTheme;

/**
 * Windows用ルックアンドフィール
 * @author RIKEN
 */
public class ThemeWindows extends OceanTheme {

    /** デフォルト、論理フォント */
    private Font font_normal;
    /** デフォルト、論理フォント:スモール */
    private Font font_small;
    /** コントロールテキストフォント */
    private FontUIResource controlFont = null;
    /** システムテキストフォント */
    private FontUIResource systemFont = null;
    /** ユーザーテキストフォント */
    private FontUIResource userFont = null;
    /** サブテキストフォント */
    private FontUIResource smallFont = null;

    /**
     * ルックアンドフィール名
     */
    @Override
    public String getName() {
        return "ThemeWindows";
    }

    /**
     * コンストラクタ
     */
    public ThemeWindows() {
        //物理フォントを優先、デフォルトは論理フォント
        // フォント名リストの取得
        GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[] fontNames = env.getAvailableFontFamilyNames();

        //デフォルト、論理フォントを設定
        font_normal = new Font("Monospaced", Font.PLAIN, 11);
        font_small  = new Font("Monospaced", Font.PLAIN, 9);

        //物理フォントの探索
        if (fontNames != null && fontNames.length > 0) {
            for (int i = 0; i < fontNames.length; i++) {
                if (fontNames[i].equals("Meiryo UI")) {
                    font_normal = new Font("Meiryo UI", Font.PLAIN, 11);
                    font_small  = new Font("Meiryo UI", Font.PLAIN, 9);
                    break;
                }
            }
        }

        //フォントの設定
        controlFont = new FontUIResource(font_normal);
        systemFont  = new FontUIResource(font_normal);
        userFont    = new FontUIResource(font_normal);
        smallFont   = new FontUIResource(font_small);
    }

    @Override
    public FontUIResource getControlTextFont() {
        return controlFont;
    }

    @Override
    public FontUIResource getSystemTextFont() {
        return systemFont;
    }

    @Override
    public FontUIResource getUserTextFont() {
        return userFont;
    }

    @Override
    public FontUIResource getMenuTextFont() {
        return controlFont;
    }

    @Override
    public FontUIResource getWindowTitleFont() {
        return controlFont;
    }

    @Override
    public FontUIResource getSubTextFont() {
        return smallFont;
    }
}
