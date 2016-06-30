---
layout: post
title: Delphi常用属性列表
categories: delphi之控件
tags: delphi 控件
---

转载自：[delphi常用属性列表（不断更新中……）](http://www.cnblogs.com/xcre/archive/2012/04/07/2435753.html)

##1.公用属性：

* Action:指定ActionList中的事件
* Alignment:控件内容在控件中的位置，或控件在窗体的位置
* Align：控件在窗体的位置 
* Anchors：控制控件与窗体的位置。如，left为true时，无论窗体怎么调整，控件与窗体的左边距不变
* BiDiMode：用于指定选项名是由左至右读或由右至左读
* Constraints：控件的高度和宽度的大小限制
* Cursor：鼠标的类型
* DragCursor、DragKind、DragMode
* HelpContext、HelpKeyword、HelpType
* Hit：当鼠标在当前控件上时，会显示此提示。
* ShowHit：设为true时，hit才能效
* PopupMenu：选择弹出式菜单
* TabOrder：获取焦点的顺序
* TabStop：设为false，无法用tab获得焦点
* Tag：用于记录一个整形的标签，并无实际意义。
* BelvelEdges：是否显示上下左右边界
* BelvelInner：控件内部线条的斜面
* BelvelOuter：控件外部线条的斜面
* BelvelKind：斜线的类型
* BorderStype：当设置为None时，BelvelEdges才有效果
* Ctl3D：控件是否为3D界面，设置为FALSE时，是平面的。
* imeMode：输入法模式
* imeName：输入法名称
* ReadOnly：是否只读
* Color：控件界面的颜色
* ScrollBars：显示滚动条的类型

##2.Form属性

* BorderIcons：窗体边框的按钮
* BorderStyle：窗体边框的类型
    * bsNone：没有边框，不可调大小
    * bsSingle：单线边框，不可调整大小
    * bsSizeable：标准应用程序边框，可调整大小 
    * bsDialog：标准对话框边框，不可调整大小
    * bsToolWindow：与Dialog相似，但标题较小
    * bsSizeToolWin，与bsSizeable相似，但标题较小
* ClientHigth/ClientWidth：窗体内部的大小，包括标题。
* DefaultMonitor
* FormStyle：窗体的样式
    * fsNormal：普通窗体
    * fsMDIChild:MDI子窗体
    * fsMDIForm：MDI主窗体
    * fsStayOnTop：总是位于屏幕最前的窗体
* Position：窗体在屏幕中的位置
* WindowState：打开窗体时窗体的大小 
* icon：标题栏图标
* PrintScale
* Scaled
* HorzScrollBar、VertScrollBar：滚动条

##3.面板PANEL

* 此面板的功能作相似与GROUPBOX。它有多种三维效果.
* BEVELINNER:图形面板内层边框斜面的类型.
* BEVELOUTER: 图形面板外层边框斜面的类型
* Bevelwidth:斜面的宽度.
* BORDERWIDTH:内层与外层边框之间的宽度
 
##4.按钮类组件

* (1)button 组件：
    * CAPTION 属性 ：用于在按钮上显示文本内容
    * Cancel 属性：是否设置成默认的取消按钮，当设置为真的时候（true） 按ESC触发改按钮的事件！
    * Default 属性：是否设置成默认的确认按钮，当设置为真的时候 按ENTER键的时候触发事件
    * Hint 属性：设置当鼠标在按钮上短暂停留是显示的提示内容。
    * Showhint 属性： 是否显示提示内容默认为假！
    * ModalResult: 指一个模式窗体的返回值 if Form2.ShowModal = mryes then ShowMessage('OK');
* (2)Bitbtn 组件
    * Kind 属性： 他的值就是位图按钮组件上所显示的图标！
    * GLYPH 属性： 用于在位图按钮上显示加载后的位图图形！
    * Numclyphs 属性：用于指定位图按钮上所能使用的位图个数！
    * Layout 属性： 用于指定位图在位图组件上的位置！
* (3)Speedbutton 组件
    * Flat 属性：是否具有OFFICE 2000的风格
    * Groupindex 属性：设置分组。
    * DOWN 属性： 设置改按钮是否处于按下状态
    * Allowallup 属性：设置同一组的快速按钮是否具有同时弹起的状态。
* (4)Radiobutton 组件
    * Checked 是否处于选中状态
    * Alignment 用于设置选择框和文字的排列方式！
* (5)Checkbox 组件
    * Allowgrayed 用来设置当前组件被选种时，是否为变灰状态！
    * State 设置当前复选框的状态。

##9.文本类组件

* (1)label 组件
    * Alignment 用来设置标签中的文本在水平方向的排列方式！
    * AUTOSIZE 用来设置标签的尺寸大小，是否随CAPTION属性中文字的字体大小而进行自动调节！
    * Font 用来设置显示文字的字体，颜色！
    * Layout 用于设置标签中的文本在垂直方向的排列方式！
    * WORDWRAP 用于设置是否自动换行！
* (2)EDIT组件
    * Charcase 用于设置组件中文字的大小写显示方式！
    * TEXT 用于显示编辑框中的文字内容
    * Modified 用来判断edit组件里的文字内容是否被修改过，若为假，就是没有被修改过！
    * Passwordchar 设置显示密码字符！
    * AutoSelect：当为True时，Edit框获取焦点时，会自动选择内容。
    * HideSelection：当edit框失去焦点时，是否还显示选择的文本。
    * MaxLength：最大长度
    * OEMConvert：是否将编辑框内文字ASCII码转换成OEM，再由OEM转换为ASCII...
* (3)memo组件
    * lines 用于访问框中每一行的内容
    * Maxlength 设置文本框中最大的文字输入量
    * Scrollbar   设置是否需要滚动条
    * Wantretruns 用于设置在框中按’enter’键是否起回车作用
    * Wanttabs 用于设置在框中按’tab’键是否起移位作用。
* (4)Maskedit 组件
    * Editmask 属性 用于设置所要显示文字的格式。
* (5)Labeledit 组件
    * Editlabel 用于对labeledit组件的标签部分进行设置！
    * Labelposition 用于设置标签放置的位置
    * Labelspaceing 用于调整标签和编辑区之间的间距，单位是象素

##10.组合框组件

* (1)combobox 组件
    * CharCase:显示大小写
    * dropdowncount 用于设置下拉后的列表框所有显示的列表项的数目！
    * sorted：是否对内容进行排序
    * Style 用于设置组合框显示时的样式，有以下五种样式
        * csdropdown: 标准格式，用户可以在编辑框中输入，每个列表项的高度相同
        * csdropdownlist: 只有列表框，没有编辑框，每个列表项的高度相等
        * csownerdrawfixed: 有编辑框和列表框，但用户不能在编辑框中输入，每个列表项的高度由itemheight 属性来指定
        * csownerdrawvariable: 有编辑框和列表框，但用户不能在编辑框中输入，每个列表项的高度可以不同
        * cssimple:只有编辑框，没有列表框
        * droppeddown 表示列表框是否已被下拉！
* (2)List组件
    * AutoComplete：失去焦点的时候，是否自动选择
    * Columns：有几列
    * ExtendedSelect：True时ctrl+左键可以多选，false时直接单击可以多选。
    * MultiSelect：是否可以多选
