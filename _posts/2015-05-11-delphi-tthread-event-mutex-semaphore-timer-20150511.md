---
layout: post
title: Delphi多线程编程--线程同步的方法（事件、互斥、信号、计时器）简介
categories: delphi之多线程 深入学习之操作系统
tags: delphi 多线程 事件 互斥 信号 计时器
---


更详细的可以参考：[http://www.cnblogs.com/xumenger/p/4450659.html](http://www.cnblogs.com/xumenger/p/4450659.html)

或者参考之后的博客


>四个系统内核对象（事件、互斥、信号、计时器）都是线程同步的手段，从这也能看出处理线程同步的复杂性；不过这还不是全部，Windows Vista开始增加了 Condition variables(条件变量)、Slim Reader-Writer Locks(读写锁)等同步手段.

>不过最简单、最轻便（速度最快）的同步手段还是 CriticalSection（临界区），但是它不属于系统内核对象，当然也没有句柄、没有 TSecurityAttributes 这个安全属性，这也导致它不能跨进程使用，不过写多线程时一般不用跨进程，所以 CriticalSection 应该是最常用的同步手段

我觉得有必要在此刻了解的是：建立系统内核对象时一般都有这个属性（TSecureAttributes）

在接下来多线程的课题中要使用一些内核对象，不如先盘点一下，到时候碰到这个属性时给个 nil 即可, 不必再费神.

###建立事件###

    function CreateEvent(
      lpEventAttributes: PSecurityAttributes; {!}
      bManualReset: BOOL;
      bInitialState: BOOL;
      lpName: PWideChar
    ): THandle; stdcall;

###建立互斥###

    function CreateMutex(
      lpMutexAttributes: PSecurityAttributes; {!}
      bInitialOwner: BOOL;
      lpName: PWideChar
    ): THandle; stdcall;

###建立信号###

    function CreateSemaphore(
      lpSemaphoreAttributes: PSecurityAttributes; {!}
      lInitialCount: Longint;
      lMaximumCount: Longint;
      lpName: PWideChar
    ): THandle; stdcall;

###建立等待计时器###

    function CreateWaitableTimer(
      lpTimerAttributes: PSecurityAttributes; {!}
      bManualReset: BOOL;
      lpTimerName: PWideChar
    ): THandle; stdcall;


上面的四个系统内核对象（事件、互斥、信号、计时器）都是线程同步的手段，从这也能看出处理线程同步的复杂性；不过这还不是全部，Windows Vista开始增加了 Condition variables(条件变量)、Slim Reader-Writer Locks(读写锁)等同步手段.

不过最简单、最轻便（速度最快）的同步手段还是 CriticalSection（临界区），但是它不属于系统内核对象，当然也没有句柄、没有 TSecurityAttributes 这个安全属性，这也导致它不能跨进程使用，不过写多线程时一般不用跨进程，所以 CriticalSection 应该是最常用的同步手段