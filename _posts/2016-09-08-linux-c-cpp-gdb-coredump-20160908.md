---
layout: post
title: 初步如何了解用GDB分析Core 文件
categories: 深入学习之操作系统 好资源之开发神器 深入学习之编译原理
tags: 操作系统 进程 dump WinDbg core GDB map 编译原理 linux windows c c++ 非法地址 堆 栈 内存
---

之前初步了解过Windows 下强大的调试工具，也简单的整理了一个初级的文章[《使用WinDbg、Map文件、Dump文件定位Access Violation的代码行》](http://www.xumenger.com/windbg-map-access-violation-20160715/)，在Linux 下面也有对应的功能强大的调试工具：GDB，它可以用来断点调试C/C++ 的程序，也可以用于分析Linux 下的C/C++ 程序运行崩溃产生的Core 文件

另外对于GDB 这个工具，在[《Linux gdb调试器用法全面解析》](http://blog.csdn.net/21cnbao/article/details/7385161)这篇文章中详细的介绍了怎么使用GDB 去调试C/C++ 代码

本文通过一个简单的例子展示怎么使用GDB 分析Core 文件，但就像我一直强调的，完全停留在一个很肤浅的入门级的水平，只是先让自己能有一个对GDB 的感性的认知，其实GDB 很强大，它能做的事情远不止于本文所提到的这些皮毛

本文的内容也是参考了网络上很多的文章，然后结合自己的验证整理出来的

* [《Linux gdb调试器用法全面解析》](http://blog.csdn.net/21cnbao/article/details/7385161)
* [《详解coredump》](http://blog.csdn.net/tenfyguo/article/details/8159176)
* [《Unix 用gdb分析core dump文件》](http://www.cnblogs.com/playerken/p/4157481.html)
* [《gdb core 调试》](http://blog.csdn.net/hanchaoman/article/details/5583457)

##什么是Core Dump文件

通常情况下，Core 文件会包含程序运行时的内存、寄存器状态、堆栈指针、内存管理信息还有各种函数调用堆栈信息等。我们可以理解为是程序工作当前状态存储生成第一个文件，许多程序出错时都会产生一个Core 文件，通过工具分析这个文件，我们可以定位到程序异常退出时对应的堆栈调用等信息，找出问题所在并进行及时解决
