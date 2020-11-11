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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

/**
 * Border class with components
 *
 * @author RIKEN
 */
public class JComponentTitledBorder
    implements Border, MouseListener, MouseMotionListener, SwingConstants {
  /** X-direction offset of components on the border */
  private static final int offset = 5;
  /** Components on the border */
  private final Component comp;
  /** Container */
  private final JComponent container;
  /** Border */
  private final Border border;

  /**
   * Constructor
   *
   * @param comp Components on the border
   * @param container container
   * @param border Border
   */
  public JComponentTitledBorder(Component comp, JComponent container, Border border) {
    this.comp = comp;
    this.container = container;
    this.border = border;
    if (comp instanceof JComponent) {
      ((JComponent) comp).setOpaque(true);
    }
    if (this.container != null) {
      this.container.addMouseListener(this);
      this.container.addMouseMotionListener(this);
    }
  }

  /**
   * Make the border transparent or get it.
   *
   * @return transparent drawing
   */
  @Override
  public boolean isBorderOpaque() {
    return true;
  }

  /**
   * Draw a border
   *
   * @param c component
   * @param g graphic
   * @param x X position
   * @param y Y position
   * @param width width
   * @param height Height
   */
  @Override
  public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
    Insets borderInsets = border.getBorderInsets(c);
    Insets insets = getBorderInsets(c);
    int temp = (insets.top - borderInsets.top) / 2;
    border.paintBorder(c, g, x, y + temp, width, height - temp);
    Dimension size = comp.getPreferredSize();
    Rectangle rect = new Rectangle(offset, 0, size.width, size.height);
    SwingUtilities.paintComponent(g, comp, (Container) c, rect);
    comp.setBounds(rect);
  }

  /**
   * Get border margins
   *
   * @param c component
   * @return Margin
   */
  @Override
  public Insets getBorderInsets(Component c) {
    Dimension size = comp.getPreferredSize();
    Insets insets = border.getBorderInsets(c);
    insets.top = Math.max(insets.top, size.height);
    return insets;
  }

  /**
   * Dispatch events
   *
   * @param me Mouse event information
   */
  private void dispatchEvent(MouseEvent me) {
    Component src = me.getComponent();
    comp.dispatchEvent(SwingUtilities.convertMouseEvent(src, me, comp));
    src.repaint();
  }
  /**
   * Mouse click event
   *
   * @param me Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent me) {
    dispatchEvent(me);
  }
  /**
   * Mouseover event
   *
   * @param me Mouse event information
   */
  @Override
  public void mouseEntered(MouseEvent me) {
    dispatchEvent(me);
  }

  /**
   * Mouse out event
   *
   * @param me Mouse event information
   */
  @Override
  public void mouseExited(MouseEvent me) {
    dispatchEvent(me);
  }

  /**
   * Mouse button down event
   *
   * @param me Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent me) {
    dispatchEvent(me);
  }

  /**
   * Mouse button up event
   *
   * @param me Mouse event information
   */
  @Override
  public void mouseReleased(MouseEvent me) {
    dispatchEvent(me);
  }

  /**
   * Mouse movement event
   *
   * @param me Mouse event information
   */
  @Override
  public void mouseMoved(MouseEvent me) {
    dispatchEvent(me);
  }

  /**
   * Mouse drag event
   *
   * @param me Mouse event information
   */
  @Override
  public void mouseDragged(MouseEvent me) {
    dispatchEvent(me);
  }
}
