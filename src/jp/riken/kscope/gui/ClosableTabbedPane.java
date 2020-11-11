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
package jp.riken.kscope.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JViewport;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import jp.riken.kscope.common.FRAME_VIEW;

/**
 * Tab pane with close button
 *
 * @author RIKEN
 */
public abstract class ClosableTabbedPane extends JTabbedPane
    implements ITabComponent, ChangeListener, ActionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Close button icon */
  private static final Icon icon = new CloseTabIcon();

  /** Parent component */
  private ITabComponent parentCompornent = null;

  /** View identifier */
  private FRAME_VIEW viewType;

  /**
   * Focus listener. <br>
   * Save for tabs added later.
   */
  TabFocusListener focusListener = null;

  /**
   * Constructor
   *
   * @param type View identifier
   */
  public ClosableTabbedPane(FRAME_VIEW type) {
    super();

    // Scroll the tab layout.
    this.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

    // Tab change event registration
    this.addChangeListener(this);

    // View identifier
    this.viewType = type;
  }

  /**
   * Close tab
   *
   * @param index Tab index
   */
  protected abstract void closeTab(int index);

  /**
   * Switch active tabs.
   *
   * @param index Active tab index
   */
  @Override
  public void setSelectedIndex(int index) {
    super.setSelectedIndex(index);

    try {
      // Display the close button only for the active tab.
      changeSelectedTab(index);

      JViewport vp = null;
      for (Component c : this.getComponents()) {
        if ("TabbedPane.scrollableViewport".equals(c.getName())) {
          vp = (JViewport) c;
          break;
        }
      }
      if (vp == null) return;

      final JViewport viewport = vp;
      Dimension dimen = this.getSize();
      Rectangle rect = this.getBoundsAt(index);
      int gw = (dimen.width - rect.width) / 2;
      if (gw > 0) {
        rect.grow(gw, 0);
        viewport.scrollRectToVisible(rect);
      }
    } catch (Exception ex) {
    }
  }

  /**
   * Add a component to the tab.
   *
   * @param title Tab display string
   * @param content Additional components
   */
  @Override
  public void addTab(String title, final Component content) {
    // Create tab label part (tab display string + close button)
    JPanel tab = createTabComponent(title);

    // Add a component to the tab.
    super.addTab(title, content);

    // Tab label settings
    setTabComponentAt(getTabCount() - 1, tab);

    // Focus listener settings
    if (content instanceof ITabComponent) {
      // Set the parent component.
      ((ITabComponent) content).setParentComponent(this);

      if (this.focusListener != null) {
        ((ITabComponent) content).addTabFocusListener(this.focusListener);
      }
    }

    return;
  }

  /**
   * Add a component to the tab.
   *
   * @param title Tab display string
   * @param content Additional components
   * @param index Additional index
   */
  public void insertTab(String title, TraceResultPanel content, int index) {
    // Create tab label part (tab display string + close button)
    JPanel tab = createTabComponent(title);

    super.insertTab(title, null, content, null, index);

    // Tab label settings
    setTabComponentAt(index, tab);

    // Focus listener settings
    if (content instanceof ITabComponent) {
      // Set the parent component.
      ((ITabComponent) content).setParentComponent(this);

      if (this.focusListener != null) {
        ((ITabComponent) content).addTabFocusListener(this.focusListener);
      }
    }
  }

  /**
   * Create tab label part (tab display string + close button)
   *
   * @param title Title
   * @return tab label part
   */
  private JPanel createTabComponent(String title) {
    // Create tab label part (tab display string + close button)
    JPanel tab = new JPanel(new BorderLayout());
    tab.setOpaque(false);

    // Tab display string label
    JLabel label = new JLabel(title);
    label.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 4));

    // Close button
    JButton button = new JButton(icon);
    button.setBorder(BorderFactory.createEmptyBorder());
    button.setContentAreaFilled(false);
    // Action when the close button is clicked
    button.addActionListener(this);
    tab.add(label, BorderLayout.WEST);
    tab.add(button, BorderLayout.EAST);
    tab.setBorder(BorderFactory.createEmptyBorder(2, 1, 1, 1));

    return tab;
  }

  /**
   * Set the tab label.
   *
   * @param index Tab index
   * @param title Tab label
   */
  public void setTabTitle(int index, String title) {
    Component comp = this.getTabComponentAt(index);
    if (!(comp instanceof JPanel)) return;
    JPanel tab = (JPanel) comp;
    int count = tab.getComponentCount();
    for (int i = 0; i < count; i++) {
      Component tabComp = tab.getComponent(i);
      if (tabComp instanceof JLabel) {
        ((JLabel) tabComp).setText(title);
      }
    }
    return;
  }

  /**
   * Active tab change event
   *
   * @param event Event occurrence source
   */
  @Override
  public void stateChanged(ChangeEvent event) {
    JTabbedPane tabbedpane = (JTabbedPane) event.getSource();
    int index = tabbedpane.getSelectedIndex();
    //        System.out.println("selected tab => "+tabbedpane.getSelectedIndex());

    // Display the close button only for the active tab.
    changeSelectedTab(index);
  }

  /**
   * Event to close tab
   *
   * @param event Event occurrence source
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    Object obj = event.getSource();
    if (!(obj instanceof JButton)) return;
    JButton button = (JButton) obj;
    Component tabComp = button.getParent();
    int index = this.indexOfTabComponent(tabComp);
    if (index >= 0) {
      closeTab(index);
    }
  }

  /**
   * Show close button only for active tab. <br>
   * Hide the close button on the inactive tab.
   *
   * @param activeIndex Active tab index
   */
  private void changeSelectedTab(int activeIndex) {
    if (activeIndex < 0) return;

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      JPanel tab = (JPanel) this.getTabComponentAt(i);
      if (tab == null) return;
      Component[] components = tab.getComponents();
      if (components == null) return;
      for (Component comp : components) {
        if (comp instanceof JButton) {
          JButton button = (JButton) comp;
          if (i == activeIndex) {
            button.setVisible(true);
          } else {
            button.setVisible(false);
          }
        }
      }
    }
  }

  /**
   * Close button icon
   *
   * @author RIKEN
   */
  private static class CloseTabIcon implements Icon {
    private int width; // / <icon width
    private int height; // / <icon height

    /** Constructor */
    public CloseTabIcon() {
      // Icon size set to 16x16
      width = 16;
      height = 16;
    }

    /**
     * Draw a close icon. <br>
     * Draw a cross mark.
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
      g.translate(x, y);

      // TYPE1
      /*
      g.setColor(Color.BLACK);
      g.drawLine(4,  4, 11, 11);
      g.drawLine(4,  5, 10, 11);
      g.drawLine(5,  4, 11, 10);
      g.drawLine(11, 4,  4, 11);
      g.drawLine(11, 5,  5, 11);
      g.drawLine(10, 4,  4, 10);
      */

      // TYPE2
      g.setColor(new Color(0x99, 0x99, 0x99));
      for (int i = 2; i < 3; i++) {
        g.drawLine(3 + i, 3, 12, 12 - i);
        g.drawLine(3, 3 + i, 12 - i, 12);
        g.drawLine(3 + i, 12, 12, 3 + i);
        g.drawLine(3, 12 - i, 12 - i, 3);
      }
      g.setColor(new Color(0x44, 0x44, 0x44));
      for (int i = 0; i < 2; i++) {
        g.drawLine(4 + i, 4, 11, 11 - i);
        g.drawLine(4, 4 + i, 11 - i, 11);
        g.drawLine(4 + i, 11, 11, 4 + i);
        g.drawLine(4, 11 - i, 11 - i, 4);
      }

      g.translate(-x, -y);
    }

    /** Get the close icon width. */
    @Override
    public int getIconWidth() {
      return width;
    }

    /** Get close icon height. */
    @Override
    public int getIconHeight() {
      return height;
    }
  }

  /**
   * Get the parent component.
   *
   * @return Parent component
   */
  @Override
  public ITabComponent getParentComponent() {
    return this.parentCompornent;
  }

  /**
   * Set the parent component.
   *
   * @param component Parent component
   */
  @Override
  public void setParentComponent(ITabComponent component) {
    this.parentCompornent = component;
  }

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {

    focusListener = listener;
    this.addFocusListener(listener);

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component tab = this.getComponentAt(i);
      if (tab instanceof ITabComponent) {
        ((ITabComponent) tab).addTabFocusListener(listener);
      }
    }
  }

  /**
   * Get the view identifier
   *
   * @return View identifier
   */
  public FRAME_VIEW getViewType() {
    return this.viewType;
  }

  /**
   * Set the view identifier
   *
   * @param type View identifier
   */
  public void setViewType(FRAME_VIEW type) {
    this.viewType = type;
  }
}
