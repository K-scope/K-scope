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
 * Look and Feel for Windows
 *
 * @author RIKEN
 */
public class ThemeWindows extends OceanTheme {

  /** Default, logical font */
  private Font font_normal;
  /** Default, logical font: Small */
  private Font font_small;
  /** Control text font */
  private FontUIResource controlFont = null;
  /** System text font */
  private FontUIResource systemFont = null;
  /** User text font */
  private FontUIResource userFont = null;
  /** Subtext font */
  private FontUIResource smallFont = null;

  /** Look and feel name */
  @Override
  public String getName() {
    return "ThemeWindows";
  }

  /** Constructor */
  public ThemeWindows() {
    // Prefer physical fonts, default logical fonts
    // Get the font name list
    GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
    String[] fontNames = env.getAvailableFontFamilyNames();

    // Set default, logical font
    font_normal = new Font("Monospaced", Font.PLAIN, 11);
    font_small = new Font("Monospaced", Font.PLAIN, 9);

    // Search for physical fonts
    if (fontNames != null && fontNames.length > 0) {
      for (int i = 0; i < fontNames.length; i++) {
        if (fontNames[i].equals("Meiryo UI")) {
          font_normal = new Font("Meiryo UI", Font.PLAIN, 11);
          font_small = new Font("Meiryo UI", Font.PLAIN, 9);
          break;
        }
      }
    }

    // Font settings
    controlFont = new FontUIResource(font_normal);
    systemFont = new FontUIResource(font_normal);
    userFont = new FontUIResource(font_normal);
    smallFont = new FontUIResource(font_small);
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
