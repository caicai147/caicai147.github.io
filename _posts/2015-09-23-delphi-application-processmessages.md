---
layout: post
title: Delphi的Application.ProcessMessages的作用？
categories: delphi之应用程序框架
tags: delphi application
---

这是我在CSDN上问的问题：[http://ask.csdn.net/questions/206626](http://ask.csdn.net/questions/206626)

通常情况下，程序在循环的时候不响应外界事件。直到循环结束为止才能接受和响应外界事件。

**为了防止界面假死，就可以在循环的某时，执行Application.ProcessMessages，使程序在循环时能够响应外界事件。把控制权暂时交给系统，这样界面就不会假死。**

Application.ProcessMessages可以有效的解决循环循环的独占问题，能够暂时中断应用程序的执行。

比如，你的一个循环10000次。你可以设置每循环100次的时候，执行一次Application.ProcessMessages，让程序响应其它操作，此时你就可以预先设置的cancel按钮取消循环。

注意，在一个很大的循环中，必须使用Application.ProcewwMessages保证能每隔一个循环或者几个循环就执行一次。假如这个大循环中没有使用Application.ProcessMessages的话，如果这个大循环运行起来需要花费很久的时间，那么就一直没有办法响应其他事件，就会造成程序的“假死”现象！所以这一点必须注意！