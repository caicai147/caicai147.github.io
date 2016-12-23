---
layout: post
title: Delphi网络编程：再进一步研究ServerSocket/ClientSocket
categories: delphi之网络编程 深入学习之网络原理 深入学习之内存管理 delphi之指针与内存
tags: delphi 二进制 网络 Delphi网络编程
---

本文介绍一下Delphi的网络编程组件ServerSocket/ClientSocket，这两个类是对底层的WinSockAPI做了封装后给开发者的新形式的网络编程的接口

##ServerSocket和ClientSocket封装底层SockAPI

>研究一下Delphi是如何封装底层SockAPI的，不失为一种学习网络编程、学习代码封装的好资源

