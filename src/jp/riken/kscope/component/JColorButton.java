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
package jp.riken.kscope.component;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.Icon;
import javax.swing.JButton;

/**
 * Color button
 *
 * @author RIKEN
 */
public class JColorButton extends JButton {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Display color */
  private Color color;

  /** Constructor */
  public JColorButton() {
    super();
    this.setColor(null);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param color Color settings
   */
  public JColorButton(Color color) {
    super();
    this.setColor(color);
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    java.awt.Dimension size = new java.awt.Dimension(48, 22);
    this.setSize(size);
    this.setPreferredSize(size);
    this.setMinimumSize(size);
    this.setMaximumSize(size);
    this.setMargin(new Insets(5, 5, 5, 5));
  }

  /**
   * Get button color
   *
   * @return color Button color
   */
  public Color getColor() {
    return color;
  }

  /**
   * Set the button color
   *
   * @param color Button color
   */
  public void setColor(Color color) {
    this.color = color;
    this.setIcon(new ColorIcon(color));
  }

  /**
   * Set the button display string to null. <br>
   * The color button does not display a character string.
   *
   * @return null fixed
   */
  @Override
  public String getText() {
    return null;
  }

  /**
   * Color icon creation class for color buttons
   *
   * @author RIKEN
   */
  private static class ColorIcon implements Icon {
    /** Icon width */
    private int width;
    /** Icon height */
    private int height;
    /** Color */
    private Color color;

    /** Constructor */
    public ColorIcon() {
      // Icon size set to 32x12
      width = 32;
      height = 12;
    }
    /**
     * Constructor
     *
     * @param color Icon color
     */
    public ColorIcon(Color color) {
      this();
      this.color = color;
    }

    /** Draw a color icon. */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
      if (this.color == null) return;

      g.translate(x, y);
      g.setColor(this.color);
      g.fillRect(0, 0, width, height);
      g.translate(-x, -y);
    }

    /** Get the color icon width. */
    @Override
    public int getIconWidth() {
      return width;
    }

    /** Get the color icon height. */
    @Override
    public int getIconHeight() {
      return height;
    }
  }
}
