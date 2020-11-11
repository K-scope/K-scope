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
import javax.swing.border.Border;

/**
 * FrameScrollPane layout manager
 *
 * @author RIKEN
 */
public class FrameScrollPaneLayout extends ScrollPaneLayout {
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
  /** View identification string: line footer */
  public static final String ROW_FOOTER = "ROW_FOOTER";
  /** View identification string: Column footer */
  public static final String COLUMN_FOOTER = "COLUMN_FOOTER";
  /** Position identification string: Horizontal left */
  public static final String HORIZONTAL_LEFT = "HORIZONTAL_LEFT";
  /** Position identification string: Horizontal right */
  public static final String HORIZONTAL_RIGHT = "HORIZONTAL_RIGHT";
  /** Position identification string: Horizontal forward */
  public static final String HORIZONTAL_LEADING = "HORIZONTAL_LEADING";
  /** Positioning string: Horizontally backward */
  public static final String HORIZONTAL_TRAILING = "HORIZONTAL_TRAILING";
  /** Position identification string: Vertically */
  public static final String VERTICAL_TOP = "VERTICAL_TOP";
  /** Positioning string: Vertical down */
  public static final String VERTICAL_BOTTOM = "VERTICAL_BOTTOM";

  /**
   * Initializes all internal fields set by addLayoutComponent ().
   *
   * @param sp scroll pine
   */
  @Override
  public void syncWithScrollPane(JScrollPane sp) {
    super.syncWithScrollPane(sp);
    if (sp instanceof FrameScrollPane) {
      this.rowFooter = ((FrameScrollPane) sp).getRowFooter();
      this.columnFooter = ((FrameScrollPane) sp).getColumnFooter();
      this.horizontalCornerLeft = ((FrameScrollPane) sp).getScrollBarCorner(HORIZONTAL_LEFT);
      this.horizontalCornerRight = ((FrameScrollPane) sp).getScrollBarCorner(HORIZONTAL_RIGHT);
      this.verticalCornerTop = ((FrameScrollPane) sp).getScrollBarCorner(VERTICAL_TOP);
      this.verticalCornerBottom = ((FrameScrollPane) sp).getScrollBarCorner(VERTICAL_BOTTOM);
    }
  }

  /**
   * Get the full display flag of the horizontal scroll bar.
   *
   * @param sp scroll pine
   * @return Horizontal scroll bar full display flag
   */
  protected boolean isHorizontalScrollBarCoversWhole(JScrollPane sp) {
    if (sp instanceof FrameScrollPane) {
      return ((FrameScrollPane) sp).isHorizontalScrollBarCoversWhole();
    } else {
      return false;
    }
  }

  /**
   * Get the full display flag of the vertical scroll bar.
   *
   * @param sp scroll pine
   * @return Full display flag of vertical scroll bar
   */
  protected boolean isVerticalScrollBarCoversWhole(JScrollPane sp) {
    if (sp instanceof FrameScrollPane) {
      return ((FrameScrollPane) sp).isVerticalScrollBarCoversWhole();
    } else {
      return false;
    }
  }

  /**
   * Get the flag to make the upper left corner and the upper right corner the same height
   *
   * @param sp scroll pine
   * @return true = Make the upper left corner and the upper right corner the same height
   */
  protected boolean isColumnHeadersUnified(JScrollPane sp) {
    if (sp instanceof FrameScrollPane) {
      return ((FrameScrollPane) sp).isColumnHeadersUnified();
    } else {
      return false;
    }
  }

  /**
   * Get the flag to make the lower left corner and the lower right corner the same height
   *
   * @param sp scroll pine
   * @return true = Make the lower left and lower right corners the same height
   */
  protected boolean isColumnFootersUnified(JScrollPane sp) {
    if (sp instanceof FrameScrollPane) {
      return ((FrameScrollPane) sp).isColumnFootersUnified();
    } else {
      return false;
    }
  }

  @Override
  public void addLayoutComponent(String s, Component c) {
    if (s.equals(ROW_FOOTER)) {
      this.rowFooter = (JViewport) addSingletonComponent(this.rowFooter, c);
    } else if (s.equals(COLUMN_FOOTER)) {
      this.columnFooter = (JViewport) addSingletonComponent(this.columnFooter, c);
    } else if (s.equals(HORIZONTAL_LEFT)) {
      this.horizontalCornerLeft = addSingletonComponent(this.horizontalCornerLeft, c);
    } else if (s.equals(HORIZONTAL_RIGHT)) {
      this.horizontalCornerRight = addSingletonComponent(this.horizontalCornerRight, c);
    } else if (s.equals(VERTICAL_TOP)) {
      this.verticalCornerTop = addSingletonComponent(this.verticalCornerTop, c);
    } else if (s.equals(VERTICAL_BOTTOM)) {
      this.verticalCornerBottom = addSingletonComponent(this.verticalCornerBottom, c);
    } else {
      super.addLayoutComponent(s, c);
    }
  }

  @Override
  public void removeLayoutComponent(Component c) {
    if (c == this.rowFooter) {
      this.rowFooter = null;
    } else if (c == this.columnFooter) {
      this.columnFooter = null;
    } else if (c == this.horizontalCornerLeft) {
      this.horizontalCornerLeft = null;
    } else if (c == this.horizontalCornerRight) {
      this.horizontalCornerRight = null;
    } else if (c == this.verticalCornerTop) {
      this.verticalCornerTop = null;
    } else if (c == this.verticalCornerBottom) {
      this.verticalCornerBottom = null;
    } else {
      super.removeLayoutComponent(c);
    }
  }

  /**
   * Get row header viewport
   *
   * @return row header viewport
   */
  public JViewport getRowFooter() {
    return this.rowFooter;
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
   * Get the corner component
   *
   * @param key Position identification string
   * @return corner component
   */
  public Component getScrollBarCorner(String key) {
    if (key.equals(HORIZONTAL_LEFT)) {
      return this.horizontalCornerLeft;
    } else if (key.equals(HORIZONTAL_RIGHT)) {
      return this.horizontalCornerRight;
    } else if (key.equals(VERTICAL_BOTTOM)) {
      return this.verticalCornerBottom;
    } else if (key.equals(VERTICAL_TOP)) {
      return this.verticalCornerTop;
    } else {
      return super.getCorner(key);
    }
  }

  @Override
  public Dimension preferredLayoutSize(Container parent) {
    JScrollPane scrollPane = (JScrollPane) parent;
    vsbPolicy = scrollPane.getVerticalScrollBarPolicy();
    hsbPolicy = scrollPane.getHorizontalScrollBarPolicy();

    Insets insets = parent.getInsets();
    int prefWidth = insets.left + insets.right;
    int prefHeight = insets.top + insets.bottom;

    Dimension extentSize = null;
    Dimension viewSize = null;
    Component view = null;

    if (viewport != null) {
      extentSize = viewport.getPreferredSize();
      viewSize = viewport.getViewSize();
      view = viewport.getView();
    }

    if (extentSize != null) {
      prefWidth += extentSize.width;
      prefHeight += extentSize.height;
    }

    Border viewportBorder = scrollPane.getViewportBorder();
    if (viewportBorder != null) {
      Insets vpbInsets = viewportBorder.getBorderInsets(parent);
      prefWidth += vpbInsets.left + vpbInsets.right;
      prefHeight += vpbInsets.top + vpbInsets.bottom;
    }

    int rowHeaderWidth = 0;
    if (rowHead != null && rowHead.isVisible()) {
      rowHeaderWidth = rowHead.getPreferredSize().width;
    }
    if (upperLeft != null && upperLeft.isVisible()) {
      rowHeaderWidth = Math.max(rowHeaderWidth, upperLeft.getPreferredSize().width);
    }
    if (lowerLeft != null && lowerLeft.isVisible()) {
      rowHeaderWidth = Math.max(rowHeaderWidth, lowerLeft.getPreferredSize().width);
    }
    prefWidth += rowHeaderWidth;

    int upperHeight = getUpperHeight();

    prefHeight += upperHeight;

    if ((this.rowFooter != null) && this.rowFooter.isVisible()) {
      prefWidth += this.rowFooter.getPreferredSize().width;
    }

    int lowerHeight = getLowerHeight();
    prefHeight += lowerHeight;
    if ((vsb != null) && (vsbPolicy != VERTICAL_SCROLLBAR_NEVER)) {
      if (vsbPolicy == VERTICAL_SCROLLBAR_ALWAYS) {
        prefWidth += vsb.getPreferredSize().width;
      } else if ((viewSize != null) && (extentSize != null)) {
        boolean canScroll = true;
        if (view instanceof Scrollable) {
          canScroll = !((Scrollable) view).getScrollableTracksViewportHeight();
        }
        if (canScroll && (viewSize.height > extentSize.height)) {
          prefWidth += vsb.getPreferredSize().width;
        }
      }
    }

    if ((hsb != null) && (hsbPolicy != HORIZONTAL_SCROLLBAR_NEVER)) {
      if (hsbPolicy == HORIZONTAL_SCROLLBAR_ALWAYS) {
        prefHeight += hsb.getPreferredSize().height;
      } else if ((viewSize != null) && (extentSize != null)) {
        boolean canScroll = true;
        if (view instanceof Scrollable) {
          canScroll = !((Scrollable) view).getScrollableTracksViewportWidth();
        }
        if (canScroll && (viewSize.width > extentSize.width)) {
          prefHeight += hsb.getPreferredSize().height;
        }
      }
    }

    return new Dimension(prefWidth, prefHeight);
  }

  /**
   * Get the height of the row header
   *
   * @return Row header height
   */
  private int getUpperHeight() {
    int upperHeight = 0;

    if ((upperLeft != null) && upperLeft.isVisible()) {
      upperHeight = upperLeft.getPreferredSize().height;
    }
    if ((upperRight != null) && upperRight.isVisible()) {
      upperHeight = Math.max(upperRight.getPreferredSize().height, upperHeight);
    }

    if ((colHead != null) && colHead.isVisible()) {
      upperHeight = Math.max(colHead.getPreferredSize().height, upperHeight);
    }
    return upperHeight;
  }

  /**
   * Get the height of the line footer
   *
   * @return Line footer height
   */
  private int getLowerHeight() {
    int lowerHeight = 0;

    if ((lowerLeft != null) && lowerLeft.isVisible()) {
      lowerHeight = lowerLeft.getPreferredSize().height;
    }
    if ((lowerRight != null) && lowerRight.isVisible()) {
      lowerHeight = Math.max(lowerRight.getPreferredSize().height, lowerHeight);
    }
    if ((this.columnFooter != null) && this.columnFooter.isVisible()) {
      lowerHeight = Math.max(this.columnFooter.getPreferredSize().height, lowerHeight);
    }
    return lowerHeight;
  }

  @Override
  public Dimension minimumLayoutSize(Container parent) {
    JScrollPane scrollPane = (JScrollPane) parent;
    vsbPolicy = scrollPane.getVerticalScrollBarPolicy();
    hsbPolicy = scrollPane.getHorizontalScrollBarPolicy();

    Insets insets = parent.getInsets();
    int minWidth = insets.left + insets.right;
    int minHeight = insets.top + insets.bottom;

    if (viewport != null) {
      Dimension size = viewport.getMinimumSize();
      minWidth += size.width;
      minHeight += size.height;
    }

    Border viewportBorder = scrollPane.getViewportBorder();
    if (viewportBorder != null) {
      Insets vpbInsets = viewportBorder.getBorderInsets(parent);
      minWidth += vpbInsets.left + vpbInsets.right;
      minHeight += vpbInsets.top + vpbInsets.bottom;
    }

    int rowHeaderWidth = 0;
    if (rowHead != null && rowHead.isVisible()) {
      Dimension size = rowHead.getMinimumSize();
      rowHeaderWidth = size.width;
      minHeight = Math.max(minHeight, size.height);
    }
    if (upperLeft != null && upperLeft.isVisible()) {
      rowHeaderWidth = Math.max(rowHeaderWidth, upperLeft.getMinimumSize().width);
    }
    if (lowerLeft != null && lowerLeft.isVisible()) {
      rowHeaderWidth = Math.max(rowHeaderWidth, lowerLeft.getMinimumSize().width);
    }
    minWidth += rowHeaderWidth;

    int upperHeight = 0;

    if ((upperLeft != null) && upperLeft.isVisible()) {
      upperHeight = upperLeft.getMinimumSize().height;
    }
    if ((upperRight != null) && upperRight.isVisible()) {
      upperHeight = Math.max(upperRight.getMinimumSize().height, upperHeight);
    }

    if ((colHead != null) && colHead.isVisible()) {
      Dimension size = colHead.getMinimumSize();
      minWidth = Math.max(minWidth, size.width);
      upperHeight = Math.max(size.height, upperHeight);
    }

    minHeight += upperHeight;

    int lowerHeight = 0;

    if ((lowerLeft != null) && lowerLeft.isVisible()) {
      lowerHeight = lowerLeft.getMinimumSize().height;
    }
    if ((lowerRight != null) && lowerRight.isVisible()) {
      lowerHeight = Math.max(lowerRight.getMinimumSize().height, lowerHeight);
    }

    if ((this.columnFooter != null) && this.columnFooter.isVisible()) {
      Dimension size = this.columnFooter.getMinimumSize();
      minWidth = Math.max(minWidth, size.width);
      lowerHeight = Math.max(size.height, lowerHeight);
    }

    minHeight += lowerHeight;

    if ((this.rowFooter != null) && this.rowFooter.isVisible()) {
      Dimension size = this.rowFooter.getMinimumSize();
      minWidth = Math.max(minWidth, size.width);
      minHeight += size.height;
    }
    if ((vsb != null) && (vsbPolicy != VERTICAL_SCROLLBAR_NEVER)) {
      Dimension size = vsb.getMinimumSize();
      minWidth += size.width;
      minHeight = Math.max(minHeight, size.height);
    }

    if ((hsb != null) && (hsbPolicy != HORIZONTAL_SCROLLBAR_NEVER)) {
      Dimension size = hsb.getMinimumSize();
      minWidth = Math.max(minWidth, size.width);
      minHeight += size.height;
    }

    return new Dimension(minWidth, minHeight);
  }

  @Override
  public void layoutContainer(Container parent) {
    JScrollPane scrollPane = (JScrollPane) parent;
    vsbPolicy = scrollPane.getVerticalScrollBarPolicy();
    hsbPolicy = scrollPane.getHorizontalScrollBarPolicy();

    Rectangle availR = scrollPane.getBounds();
    availR.x = availR.y = 0;

    Insets insets = parent.getInsets();
    availR.x = insets.left;
    availR.y = insets.top;
    availR.width -= insets.left + insets.right;
    availR.height -= insets.top + insets.bottom;

    Rectangle colHeadR = new Rectangle(0, availR.y, 0, 0);

    int upperHeight = getUpperHeight();

    if ((colHead != null) && (colHead.isVisible())) {
      int colHeadHeight = Math.min(availR.height, upperHeight);
      colHeadR.height = colHeadHeight;
      availR.y += colHeadHeight;
      availR.height -= colHeadHeight;
    }

    Rectangle rowHeadR = new Rectangle(0, 0, 0, 0);

    if ((rowHead != null) && (rowHead.isVisible())) {
      int rowHeadWidth = rowHead.getPreferredSize().width;
      if (upperLeft != null && upperLeft.isVisible()) {
        rowHeadWidth = Math.max(rowHeadWidth, upperLeft.getPreferredSize().width);
      }
      if (lowerLeft != null && lowerLeft.isVisible()) {
        rowHeadWidth = Math.max(rowHeadWidth, lowerLeft.getPreferredSize().width);
      }
      rowHeadWidth = Math.min(availR.width, rowHeadWidth);

      rowHeadR.width = rowHeadWidth;
      availR.width -= rowHeadWidth;
      rowHeadR.x = availR.x;
      availR.x += rowHeadWidth;
    }

    Border viewportBorder = scrollPane.getViewportBorder();
    Insets vpbInsets;
    if (viewportBorder != null) {
      vpbInsets = viewportBorder.getBorderInsets(parent);
      availR.x += vpbInsets.left;
      availR.y += vpbInsets.top;
      availR.width -= vpbInsets.left + vpbInsets.right;
      availR.height -= vpbInsets.top + vpbInsets.bottom;
    } else {
      vpbInsets = new Insets(0, 0, 0, 0);
    }

    Rectangle rowFootR = new Rectangle(0, 0, 0, 0);

    if ((this.rowFooter != null) && (this.rowFooter.isVisible())) {
      int rowFootWidth = Math.min(availR.width, this.rowFooter.getPreferredSize().width);
      rowFootR.width = rowFootWidth;
      availR.width -= rowFootWidth;
      rowFootR.x = availR.x + availR.width;
    }

    Rectangle colFootR = new Rectangle(0, availR.y, 0, 0);

    int lowerHeight = getLowerHeight();

    if ((this.columnFooter != null) && (this.columnFooter.isVisible())) {
      int colFootHeight = Math.min(availR.height, lowerHeight);
      colFootR.height = colFootHeight;
      availR.height -= colFootHeight;
      colFootR.y = availR.y + availR.height;
    }

    Component view = (viewport != null) ? viewport.getView() : null;
    Dimension viewPrefSize = (view != null) ? view.getPreferredSize() : new Dimension(0, 0);

    Dimension extentSize =
        (viewport != null) ? viewport.toViewCoordinates(availR.getSize()) : new Dimension(0, 0);

    boolean viewTracksViewportWidth = false;
    boolean viewTracksViewportHeight = false;
    boolean isEmpty = (availR.width < 0 || availR.height < 0);
    Scrollable sv;
    if (!isEmpty && view instanceof Scrollable) {
      sv = (Scrollable) view;
      viewTracksViewportWidth = sv.getScrollableTracksViewportWidth();
      viewTracksViewportHeight = sv.getScrollableTracksViewportHeight();
    } else {
      sv = null;
    }

    Rectangle vsbR =
        new Rectangle(
            0,
            isVerticalScrollBarCoversWhole(scrollPane) ? -vpbInsets.top : availR.y - vpbInsets.top,
            0,
            0);

    boolean vsbNeeded;
    if (vsbPolicy == VERTICAL_SCROLLBAR_ALWAYS) {
      vsbNeeded = true;
    } else if (vsbPolicy == VERTICAL_SCROLLBAR_NEVER) {
      vsbNeeded = false;
    } else if (isEmpty) {
      vsbNeeded = false;
    } else {
      vsbNeeded =
          !viewTracksViewportHeight
              && (viewPrefSize.height > extentSize.height
                  || (rowHead != null
                      && rowHead.getView() != null
                      && rowHead.getView().getPreferredSize().height > extentSize.height));
    }

    if ((vsb != null) && vsbNeeded) {
      adjustForVSB(true, availR, vsbR, vpbInsets, true);
      extentSize = viewport.toViewCoordinates(availR.getSize());
    }

    Rectangle hsbR =
        new Rectangle(
            isHorizontalScrollBarCoversWhole(scrollPane)
                ? -vpbInsets.left
                : availR.x - vpbInsets.left,
            0,
            0,
            0);
    boolean hsbNeeded;
    if (hsbPolicy == HORIZONTAL_SCROLLBAR_ALWAYS) {
      hsbNeeded = true;
    } else if (hsbPolicy == HORIZONTAL_SCROLLBAR_NEVER) {
      hsbNeeded = false;
    } else if (isEmpty) {
      hsbNeeded = false;
    } else {
      hsbNeeded =
          !viewTracksViewportWidth
              && (viewPrefSize.width > extentSize.width
                  || (colHead != null
                      && colHead.getView() != null
                      && colHead.getView().getPreferredSize().width > extentSize.width));
    }

    if ((hsb != null) && hsbNeeded) {
      adjustForHSB(true, availR, hsbR, vpbInsets);

      if ((vsb != null) && !vsbNeeded && (vsbPolicy != VERTICAL_SCROLLBAR_NEVER)) {

        extentSize = viewport.toViewCoordinates(availR.getSize());
        vsbNeeded = viewPrefSize.height > extentSize.height;

        if (vsbNeeded) {
          adjustForVSB(true, availR, vsbR, vpbInsets, true);
        }
      }
    }

    boolean ltr = scrollPane.getComponentOrientation().isLeftToRight();

    if (viewport != null) {
      viewport.setBounds(adjustBounds(parent, availR, ltr));

      if (sv != null) {
        extentSize = viewport.toViewCoordinates(availR.getSize());

        boolean oldHSBNeeded = hsbNeeded;
        boolean oldVSBNeeded = vsbNeeded;
        viewTracksViewportWidth = sv.getScrollableTracksViewportWidth();
        viewTracksViewportHeight = sv.getScrollableTracksViewportHeight();
        if (vsb != null && vsbPolicy == VERTICAL_SCROLLBAR_AS_NEEDED) {
          boolean newVSBNeeded =
              !viewTracksViewportHeight
                  && (viewPrefSize.height > extentSize.height
                      || (rowHead != null
                          && rowHead.getView() != null
                          && rowHead.getView().getPreferredSize().height > extentSize.height));
          if (newVSBNeeded != vsbNeeded) {
            vsbNeeded = newVSBNeeded;
            adjustForVSB(vsbNeeded, availR, vsbR, vpbInsets, true);
            extentSize = viewport.toViewCoordinates(availR.getSize());
          }
        }
        if (hsb != null && hsbPolicy == HORIZONTAL_SCROLLBAR_AS_NEEDED) {
          boolean newHSBbNeeded =
              !viewTracksViewportWidth
                  && (viewPrefSize.width > extentSize.width
                      || (colHead != null
                          && colHead.getView() != null
                          && colHead.getView().getPreferredSize().width > extentSize.width));
          if (newHSBbNeeded != hsbNeeded) {
            hsbNeeded = newHSBbNeeded;
            adjustForHSB(hsbNeeded, availR, hsbR, vpbInsets);
            if ((vsb != null) && !vsbNeeded && (vsbPolicy != VERTICAL_SCROLLBAR_NEVER)) {

              extentSize = viewport.toViewCoordinates(availR.getSize());
              vsbNeeded = viewPrefSize.height > extentSize.height;

              if (vsbNeeded) {
                adjustForVSB(true, availR, vsbR, vpbInsets, true);
              }
            }
            if (this.rowFooter != null && this.rowFooter.isVisible()) {
              vsbR.x += rowFootR.width;
            }
          }
        }
        if (oldHSBNeeded != hsbNeeded || oldVSBNeeded != vsbNeeded) {
          viewport.setBounds(adjustBounds(parent, availR, ltr));
        }
      }
    }

    vsbR.height =
        isVerticalScrollBarCoversWhole(scrollPane)
            ? scrollPane.getHeight() - 1
            : availR.height + vpbInsets.top + vpbInsets.bottom;
    hsbR.width =
        isHorizontalScrollBarCoversWhole(scrollPane)
            ? scrollPane.getWidth() - vsbR.width
            : availR.width + vpbInsets.left + vpbInsets.right;
    rowHeadR.height = availR.height + vpbInsets.top + vpbInsets.bottom;
    rowHeadR.y = availR.y - vpbInsets.top;
    colHeadR.width = availR.width + vpbInsets.left + vpbInsets.right;
    colHeadR.x = availR.x - vpbInsets.left;

    colFootR.x = availR.x;
    colFootR.y = rowHeadR.y + rowHeadR.height;
    colFootR.width = availR.width;
    rowFootR.x = availR.x + availR.width;
    rowFootR.y = availR.y;
    rowFootR.height = availR.height;

    vsbR.x += rowFootR.width;
    hsbR.y += colFootR.height;

    if (rowHead != null) {
      rowHead.setBounds(adjustBounds(parent, rowHeadR, ltr));
    }

    if (this.rowFooter != null) {
      this.rowFooter.setBounds(adjustBounds(parent, rowFootR, ltr));
    }

    int columnHeaderHeight =
        isColumnHeadersUnified(scrollPane)
            ? Math.max(
                colHeadR.height,
                Math.max(
                    upperLeft == null ? 0 : upperLeft.getPreferredSize().height,
                    upperRight == null ? 0 : upperRight.getPreferredSize().height))
            : 0;
    int columnFooterHeight =
        isColumnFootersUnified(scrollPane)
            ? Math.max(
                colFootR.height,
                Math.max(
                    lowerLeft == null ? 0 : lowerLeft.getPreferredSize().height,
                    lowerRight == null ? 0 : lowerRight.getPreferredSize().height))
            : 0;

    if (colHead != null) {
      int height =
          isColumnHeadersUnified(scrollPane)
              ? columnHeaderHeight
              : Math.min(colHeadR.height, colHead.getPreferredSize().height);
      colHead.setBounds(
          adjustBounds(
              parent,
              new Rectangle(
                  colHeadR.x, colHeadR.y + colHeadR.height - height, colHeadR.width, height),
              ltr));
    }

    if (this.columnFooter != null) {
      int height =
          isColumnFootersUnified(scrollPane)
              ? columnFooterHeight
              : Math.min(colFootR.height, this.columnFooter.getPreferredSize().height);
      this.columnFooter.setBounds(
          adjustBounds(parent, new Rectangle(colFootR.x, colFootR.y, colFootR.width, height), ltr));
    }

    if (vsb != null) {
      if (vsbNeeded) {
        vsb.setVisible(true);
        if (this.verticalCornerTop == null && this.verticalCornerBottom == null)
          vsb.setBounds(adjustBounds(parent, vsbR, ltr));
        else {
          Rectangle rect = new Rectangle(vsbR);
          if (this.verticalCornerTop != null) {
            Dimension dim = this.verticalCornerTop.getPreferredSize();
            rect.y += dim.height;
            rect.height -= dim.height;
            this.verticalCornerTop.setVisible(true);
            this.verticalCornerTop.setBounds(
                adjustBounds(parent, new Rectangle(vsbR.x, vsbR.y, vsbR.width, dim.height), ltr));
          }
          if (this.verticalCornerBottom != null) {
            Dimension dim = this.verticalCornerBottom.getPreferredSize();
            rect.height -= dim.height;
            this.verticalCornerBottom.setVisible(true);
            this.verticalCornerBottom.setBounds(
                adjustBounds(
                    parent,
                    new Rectangle(
                        vsbR.x, vsbR.y + vsbR.height - dim.height, vsbR.width, dim.height),
                    ltr));
          }
          vsb.setBounds(adjustBounds(parent, rect, ltr));
        }
      } else {
        if (viewPrefSize.height > extentSize.height) {
          vsb.setVisible(true);
          vsb.setBounds(adjustBounds(parent, new Rectangle(vsbR.x, vsbR.y, 0, vsbR.height), ltr));
        } else {
          vsb.setVisible(false);
        }
        if (this.verticalCornerTop != null) this.verticalCornerTop.setVisible(false);
        if (this.verticalCornerBottom != null) this.verticalCornerBottom.setVisible(false);
      }
    }

    if (hsb != null) {
      if (hsbNeeded) {
        hsb.setVisible(true);
        if (this.horizontalCornerLeft == null && this.horizontalCornerRight == null)
          hsb.setBounds(adjustBounds(parent, hsbR, ltr));
        else {
          Rectangle rect = new Rectangle(hsbR);
          if (this.horizontalCornerLeft != null) {
            Dimension dim = this.horizontalCornerLeft.getPreferredSize();
            rect.x += dim.width;
            rect.width -= dim.width;
            this.horizontalCornerLeft.setVisible(true);
            this.horizontalCornerLeft.setBounds(
                adjustBounds(parent, new Rectangle(hsbR.x, hsbR.y, dim.width, hsbR.height), ltr));
            this.horizontalCornerLeft.doLayout();
          }
          if (this.horizontalCornerRight != null) {
            Dimension dim = this.horizontalCornerRight.getPreferredSize();
            rect.width -= dim.width;
            this.horizontalCornerRight.setVisible(true);
            this.horizontalCornerRight.setBounds(
                adjustBounds(
                    parent,
                    new Rectangle(hsbR.x + hsbR.width - dim.width, hsbR.y, dim.width, hsbR.height),
                    ltr));
          }
          hsb.setBounds(adjustBounds(parent, rect, ltr));
        }
      } else {
        if (viewPrefSize.width > extentSize.width) {
          hsb.setVisible(true);
          hsb.setBounds(adjustBounds(parent, new Rectangle(hsbR.x, hsbR.y, hsbR.width, 0), ltr));
        } else {
          hsb.setVisible(false);
        }
        if (this.horizontalCornerLeft != null) this.horizontalCornerLeft.setVisible(false);
        if (this.horizontalCornerRight != null) this.horizontalCornerRight.setVisible(false);
      }
    }

    if (lowerLeft != null && lowerLeft.isVisible()) {
      int height =
          isColumnFootersUnified(scrollPane)
              ? columnFooterHeight
              : Math.min(lowerLeft.getPreferredSize().height, colFootR.height);
      lowerLeft.setBounds(
          adjustBounds(
              parent,
              new Rectangle(
                  rowHeadR.x, colFootR.y != 0 ? colFootR.y : hsbR.y, rowHeadR.width, height),
              ltr));
    }

    if (lowerRight != null && lowerRight.isVisible()) {
      int height =
          isColumnFootersUnified(scrollPane)
              ? columnFooterHeight
              : Math.min(lowerRight.getPreferredSize().height, colFootR.height);
      lowerRight.setBounds(
          adjustBounds(
              parent,
              new Rectangle(
                  rowFootR.x,
                  colFootR.y != 0 ? colFootR.y : hsbR.y,
                  rowFootR.width + (isVerticalScrollBarCoversWhole(scrollPane) ? 0 : vsbR.width),
                  height),
              ltr));
    }

    if (upperLeft != null && upperLeft.isVisible()) {
      int height =
          isColumnHeadersUnified(scrollPane)
              ? columnHeaderHeight
              : Math.min(upperLeft.getPreferredSize().height, colHeadR.height);
      upperLeft.setBounds(
          adjustBounds(
              parent,
              new Rectangle(
                  rowHeadR.x, colHeadR.y + colHeadR.height - height, rowHeadR.width, height),
              ltr));
    }

    if (upperRight != null && upperRight.isVisible()) {
      int height =
          isColumnHeadersUnified(scrollPane)
              ? columnHeaderHeight
              : Math.min(upperRight.getPreferredSize().height, colHeadR.height);
      upperRight.setBounds(
          adjustBounds(
              parent,
              new Rectangle(
                  rowFootR.x,
                  colHeadR.y + colHeadR.height - height,
                  rowFootR.width + (isVerticalScrollBarCoversWhole(scrollPane) ? 0 : vsbR.width),
                  height),
              ltr));
    }
  }

  /**
   * Get border size
   *
   * @param container container
   * @param rect size
   * @param ltr direction
   * @return Border size
   */
  private Rectangle adjustBounds(Container container, Rectangle rect, boolean ltr) {
    if (ltr) {
      return rect;
    } else {
      Rectangle r = new Rectangle(rect);
      int w = container.getWidth();
      r.x = w - (rect.x + rect.width);
      return r;
    }
  }

  /**
   * Set the size of the vertical scrollbar
   *
   * @param wantsVSB Vertical scrollbar flag
   * @param available size
   * @param vsbR Scrollbar size
   * @param vpbInsets Margins
   * @param leftToRight Left and right flags
   */
  private void adjustForVSB(
      boolean wantsVSB,
      Rectangle available,
      Rectangle vsbR,
      Insets vpbInsets,
      boolean leftToRight) {
    int oldWidth = vsbR.width;
    if (wantsVSB) {
      int vsbWidth = Math.max(0, vsb.getPreferredSize().width);

      available.width -= vsbWidth;
      vsbR.width = vsbWidth;

      if (leftToRight) {
        vsbR.x = available.x + available.width + vpbInsets.right;
      } else {
        vsbR.x = available.x - vpbInsets.left;
        available.x += vsbWidth;
      }
    } else {
      available.width += oldWidth;
    }
  }

  /**
   * Set the size of the horizontal scrollbar
   *
   * @param wantsHSB Horizontal scrollbar flag
   * @param available size
   * @param hsbR Scrollbar size
   * @param vpbInsets Margins
   */
  private void adjustForHSB(
      boolean wantsHSB, Rectangle available, Rectangle hsbR, Insets vpbInsets) {
    int oldHeight = hsbR.height;
    if (wantsHSB) {
      int hsbHeight = Math.max(0, hsb.getPreferredSize().height);

      available.height -= hsbHeight;
      hsbR.y = available.y + available.height + vpbInsets.bottom;
      hsbR.height = hsbHeight;
    } else {
      available.height += oldHeight;
    }
  }

  /**
   * UI resource version of ScrollPaneLayout
   *
   * @author RIKEN
   */
  static class UIResource extends FrameScrollPaneLayout implements javax.swing.plaf.UIResource {
    /** Serial number */
    private static final long serialVersionUID = 1L;
  }
}
