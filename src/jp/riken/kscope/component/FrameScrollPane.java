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

import java.awt.*;
import javax.swing.*;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Frame scroll pine class. Scroll pine with views on the right and bottom
 *
 * @author RIKEN
 */
public class FrameScrollPane extends JScrollPane {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Row footer viewport */
  protected JViewport rowFooter;
  /** Column footer viewport */
  protected JViewport columnFooter;
  /** Left corner component */
  protected Component horizontalCornerLeft;
  /** Right corner component */
  protected Component horizontalCornerRight;
  /** Upper corner component */
  protected Component verticalCornerTop;
  /** Bottom corner component */
  protected Component verticalCornerBottom;
  /** Horizontal scroll bar full display flag */
  private boolean horizontalScrollBarCoversWhole;
  /** Vertical scroll bar full display flag */
  private boolean verticalScrollBarCoversWhole;
  /** Property change notification key: Change the entire display of the horizontal scroll bar */
  public static final String PROPERTY_HORIZONTAL_SCROLL_BAR_COVERS_WHOLE =
      "horizontalScrollBarCoversWhole";
  /** Property change notification key: Change the entire display of the vertical scroll bar */
  public static final String PROPERTY_VERTICAL_SCROLL_BAR_COVERS_WHOLE =
      "verticalScrollBarCoversWhole";
  /** Flag that makes the upper left corner and the upper right corner the same height */
  private boolean columnHeadersUnified;
  /** Flag that makes the lower left corner and the lower right corner the same height */
  private boolean columnFootersUnified;
  /**
   * Property change notification key: Flag change to make the upper left corner and the upper right
   * corner the same height
   */
  public static final String PROPERTY_COLUMN_HEADERS_UNIFIED = "columnHeadersUnified";
  /**
   * Property change notification key: Flag change to make the lower left corner and the lower right
   * corner the same height
   */
  public static final String PROPERTY_COLUMN_FOOTERS_UNIFIED = "columnFootersUnified";

  /**
   * Constructor
   *
   * @param view Component to place in the viewport in the scroll pane
   * @param verticalPolicy Vertical scroll bar display condition
   * @param horizontalPolicy Horizontal scroll bar display condition
   */
  public FrameScrollPane(Component view, int verticalPolicy, int horizontalPolicy) {
    setLayout(new FrameScrollPaneLayout.UIResource());
    setVerticalScrollBarPolicy(verticalPolicy);
    setHorizontalScrollBarPolicy(horizontalPolicy);
    setViewport(createViewport());
    setVerticalScrollBar(createVerticalScrollBar());
    setHorizontalScrollBar(createHorizontalScrollBar());
    if (null != view) {
      setViewportView(view);
    }
    setOpaque(true);
    updateUI();

    if (!getComponentOrientation().isLeftToRight()) {
      viewport.setViewPosition(new Point(Integer.MAX_VALUE, 0));
    }
  }

  /**
   * Constructor
   *
   * @param view Component to place in the viewport in the scroll pane
   */
  public FrameScrollPane(Component view) {
    this(view, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
  }

  /**
   * Constructor
   *
   * @param verticalPolicy Vertical scroll bar display condition
   * @param horizontalPolicy Horizontal scroll bar display condition
   */
  public FrameScrollPane(int verticalPolicy, int horizontalPolicy) {
    this(null, verticalPolicy, horizontalPolicy);
  }

  /** Constructor */
  public FrameScrollPane() {
    this(null, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
  }

  /**
   * Get the row footer viewport.
   *
   * @return line footer viewport
   */
  public JViewport getRowFooter() {
    return this.rowFooter;
  }

  /**
   * Set the row footer viewport
   *
   * @param footer row footer viewport
   */
  public void setRowFooter(JViewport footer) {
    JViewport old = getRowFooter();
    this.rowFooter = footer;
    if (null != footer) {
      add(footer, FrameScrollPaneLayout.ROW_FOOTER);
    } else if (null != old) {
      remove(old);
    }
    firePropertyChange("rowFooter", old, footer);
    revalidate();
    repaint();

    // Cost bar display: Measures to move the Viewport of SourceCodePanel to the top when ON at
    // 2013/05/15 by @hira
    // SwingUtils.synchronizeView(footer, getViewport(), SwingConstants.VERTICAL);
    SwingUtils.synchronizeView(getViewport(), footer, SwingConstants.VERTICAL);
  }

  /**
   * Set the row header viewport
   *
   * @param rowHeader Row header viewport
   */
  @Override
  public void setRowHeader(JViewport rowHeader) {
    super.setRowHeader(rowHeader);
    SwingUtils.synchronizeView(rowHeader, getViewport(), SwingConstants.VERTICAL);
  }

  /**
   * Set the line footer component
   *
   * @param view Row footer component
   */
  public void setRowFooterView(Component view) {
    if (null == getRowFooter()) {
      setRowFooter(createViewport());
    }
    getRowFooter().setView(view);
  }

  /**
   * Get column footer viewport
   *
   * @return Column footer viewport
   */
  public JViewport getColumnFooter() {
    return this.columnFooter;
  }

  /**
   * Set column footer viewport
   *
   * @param footer Column footer viewport
   */
  public void setColumnFooter(JViewport footer) {
    JViewport old = getColumnFooter();
    this.columnFooter = footer;
    if (null != footer) {
      add(footer, FrameScrollPaneLayout.COLUMN_FOOTER);
    } else if (null != old) {
      remove(old);
    }
    firePropertyChange("columnFooter", old, footer);

    revalidate();
    repaint();

    // Cost bar display: Measures to move the Viewport of SourceCodePanel to the top when ON at
    // 2013/05/15 by @hira
    // SwingUtils.synchronizeView(footer, getViewport(), SwingConstants.HORIZONTAL);
    SwingUtils.synchronizeView(getViewport(), footer, SwingConstants.HORIZONTAL);
  }

  /**
   * Set column header viewport
   *
   * @param columnHeader Column header viewport
   */
  @Override
  public void setColumnHeader(JViewport columnHeader) {
    super.setColumnHeader(columnHeader);
    SwingUtils.synchronizeView(this.columnHeader, getViewport(), SwingConstants.HORIZONTAL);
  }

  /**
   * Set the column footer component *
   *
   * @param view Column footer component
   */
  public void setColumnFooterView(Component view) {
    if (null == getColumnFooter()) {
      setColumnFooter(createViewport());
    }
    getColumnFooter().setView(view);
  }

  /**
   * Get the corner component
   *
   * @param key Corner identification string
   * @return corner component
   */
  public Component getScrollBarCorner(String key) {
    boolean isLeftToRight = getComponentOrientation().isLeftToRight();
    if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEADING)) {
      key =
          isLeftToRight
              ? FrameScrollPaneLayout.HORIZONTAL_LEFT
              : FrameScrollPaneLayout.HORIZONTAL_RIGHT;
    } else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_TRAILING)) {
      key =
          isLeftToRight
              ? FrameScrollPaneLayout.HORIZONTAL_RIGHT
              : FrameScrollPaneLayout.HORIZONTAL_LEFT;
    }

    if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEFT)) {
      return horizontalCornerLeft;
    } else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_RIGHT)) {
      return horizontalCornerRight;
    } else if (key.equals(FrameScrollPaneLayout.VERTICAL_BOTTOM)) {
      return verticalCornerBottom;
    } else if (key.equals(FrameScrollPaneLayout.VERTICAL_TOP)) {
      return verticalCornerTop;
    } else {
      return null;
    }
  }

  /**
   * Set the corner component
   *
   * @param key Corner identification string
   * @param corner Corner component
   */
  public void setScrollBarCorner(String key, Component corner) {
    Component old;
    boolean isLeftToRight = getComponentOrientation().isLeftToRight();
    if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEADING)) {
      key =
          isLeftToRight
              ? FrameScrollPaneLayout.HORIZONTAL_LEFT
              : FrameScrollPaneLayout.HORIZONTAL_RIGHT;
    } else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_TRAILING)) {
      key =
          isLeftToRight
              ? FrameScrollPaneLayout.HORIZONTAL_RIGHT
              : FrameScrollPaneLayout.HORIZONTAL_LEFT;
    }

    if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEFT)) {
      old = horizontalCornerLeft;
      horizontalCornerLeft = corner;
    } else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_RIGHT)) {
      old = horizontalCornerRight;
      horizontalCornerRight = corner;
    } else if (key.equals(FrameScrollPaneLayout.VERTICAL_TOP)) {
      old = verticalCornerTop;
      verticalCornerTop = corner;
    } else if (key.equals(FrameScrollPaneLayout.VERTICAL_BOTTOM)) {
      old = verticalCornerBottom;
      verticalCornerBottom = corner;
    } else {
      throw new IllegalArgumentException("invalid scroll bar corner key");
    }

    if (null != old) {
      remove(old);
    }
    if (null != corner) {
      add(corner, key);
    }
    if (corner != null) corner.setComponentOrientation(getComponentOrientation());
    firePropertyChange(key, old, corner);
    revalidate();
    repaint();
  }

  /** Redraw. */
  @Override
  public void updateUI() {
    super.updateUI();
    setLayout(new FrameScrollPaneLayout.UIResource());
    LookAndFeel.installBorder(this, "FrameScrollPane.border");
  }

  /**
   * Get the full display flag of the vertical scroll bar.
   *
   * @return Full display flag of vertical scroll bar
   */
  public boolean isVerticalScrollBarCoversWhole() {
    return this.verticalScrollBarCoversWhole;
  }

  /**
   * Set the full display flag of the horizontal scroll bar.
   *
   * @param whole Horizontal scroll bar full display flag
   */
  public void setHorizontalScrollBarCoversWhole(boolean whole) {
    boolean old = this.horizontalScrollBarCoversWhole;
    if (old != whole) {
      this.horizontalScrollBarCoversWhole = whole;
      firePropertyChange(
          PROPERTY_HORIZONTAL_SCROLL_BAR_COVERS_WHOLE, old, this.horizontalScrollBarCoversWhole);
      invalidate();
      doLayout();
      if (getHorizontalScrollBar() != null) {
        getHorizontalScrollBar().doLayout();
      }
    }
  }

  /**
   * Get the full display flag of the horizontal scroll bar.
   *
   * @return Horizontal scroll bar full display flag
   */
  public boolean isHorizontalScrollBarCoversWhole() {
    return this.horizontalScrollBarCoversWhole;
  }

  /**
   * Set the full display flag of the vertical scroll bar.
   *
   * @param whole Vertical scroll bar full display flag
   */
  public void setVerticalScrollBarCoversWhole(boolean whole) {
    boolean old = this.verticalScrollBarCoversWhole;
    if (old != whole) {
      this.verticalScrollBarCoversWhole = whole;
      firePropertyChange(
          PROPERTY_VERTICAL_SCROLL_BAR_COVERS_WHOLE, old, this.verticalScrollBarCoversWhole);
      invalidate();
      doLayout();
      if (getVerticalScrollBar() != null) {
        getVerticalScrollBar().doLayout();
      }
    }
  }

  /**
   * Get the flag to make the upper left corner and the upper right corner the same height
   *
   * @return true = Make the upper left corner and the upper right corner the same height
   */
  public boolean isColumnHeadersUnified() {
    return this.columnHeadersUnified;
  }

  /**
   * Set a flag to make the upper left corner and the upper right corner the same height
   *
   * @param unified true = Make the upper left corner and the upper right corner the same height
   */
  public void setColumnHeadersUnified(boolean unified) {
    boolean old = this.columnHeadersUnified;
    if (old != unified) {
      this.columnHeadersUnified = unified;
      firePropertyChange(PROPERTY_COLUMN_HEADERS_UNIFIED, old, this.horizontalScrollBarCoversWhole);
      invalidate();
      doLayout();
    }
  }

  /**
   * Get the flag to make the lower left corner and the lower right corner the same height
   *
   * @return true = Make the lower left and lower right corners the same height
   */
  public boolean isColumnFootersUnified() {
    return this.columnFootersUnified;
  }

  /**
   * Set a flag to make the lower left corner and the lower right corner the same height
   *
   * @param unified true = Make the lower left and lower right corners the same height
   */
  public void setColumnFootersUnified(boolean unified) {
    boolean old = this.columnFootersUnified;
    if (old != unified) {
      this.columnFootersUnified = unified;
      firePropertyChange(PROPERTY_COLUMN_FOOTERS_UNIFIED, old, this.horizontalScrollBarCoversWhole);
      invalidate();
      doLayout();
    }
  }
}
