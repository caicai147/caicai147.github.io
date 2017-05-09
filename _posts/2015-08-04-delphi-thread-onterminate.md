---
layout: post
title: Delphi多线程的OnTerminate属性
categories: delphi之多线程
tags: delphi 线程 多线程
---


简介
---

首先看TThread源码中关于OnTerminate的代码：

    public
        ....
        property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
        ...
    end;

 

再看Delphi自带的帮助手册中对于OnTerminate的解释：

Occurs after the thread's Execute method has returned and before the thread is destroyed.

property OnTerminate: TNotifyEvent;

Description

Write an OnTerminate event handler to execute code after the thread finishes executing. The OnTerminate event handler is called in the context of the main thread, which means CLX methods and properties can be called freely.

翻译成中文的大致的解释就是：线程的该属性如果被赋值为一个方法，那么此方法将会在该线程的Execute方法执行完成并退出，但是线程还没有被释放之前执行。

但是这里只是讲到它的使用，而且还可能只是多种可以使用的地方之一，但是OnTerminate是什么，它的原理是什么并没有讲到，所以还需要再去钻研。

 

一个实际的例子：
---

    unit Unit1;
    
    interface
    
    uses
      Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
      Dialogs, StdCtrls;
    
    type
      TForm1 = class(TForm)
        btn1: TButton;
        procedure btn1Click(Sender: TObject);
      private
        procedure ThreadDone(Sender: TObject);
      public
        { Public declarations }
      end;
    
      TTestThread = class(TThread)
      protected
        procedure Execute; override;
      end;
    
    var
      Form1: TForm1;
    
    implementation
    
    {$R *.dfm}
    
    const
      MaxSize = 128;
    var
      NextNumber: Integer = 0;
      DoneFlags: Integer = 0;
      GlobalArray: array[1..MaxSize] of Integer;
      CS: TRTLCriticalSection;
    
      function GetNextNumber: Integer;
      begin
        Result:= NextNumber;
        Inc(NextNumber);
      end;
    
      procedure TTestThread.Execute;
      var
        i: Integer;
      begin
        OnTerminate:= Form1.ThreadDone;    //在这里设置OnTerminate属性的值为Form1的ThreadDone方法，
                                          //表示在线程执行完Execute之后，还没有被释放之前，要紧接着执行Form1的ThreadDone方法。
        EnterCriticalSection(CS);
        for i:= 1 to MaxSize do
        begin
          GlobalArray[i]:= GetNextNumber;
          Sleep(5);
        end;
        LeaveCriticalSection(CS);
      end;
    
      procedure TForm1.ThreadDone(Sender: TObject);
      var
        i: Integer;
      begin
        inc(DoneFlags);
        if DoneFlags = 2 then
        begin
          for i:= 1 to MaxSize do
            lst1.Items.Add(IntToStr(GlobalArray[i]));
          DeleteCriticalSection(CS);
        end;
      end;
    
    procedure TForm1.btn1Click(Sender: TObject);
    begin
      initializeCriticalSection(CS);
      TTestThread.Create(False);
      TTestThread.Create(False);
    
    end;
    
    end.    

 

### 先解释OnTerminate

在这里设置OnTerminate属性为Form1的ThreadDone方法，表示在线程执行完Execute之后，还没有被释放之前，要紧接着执行Form1的ThreadDone方法。

### 再对线程的同步进行说明

这里使用临界区进行线程的同步，该例程会在点击按钮之后开启两个线程。

* 每个线程都是对一个全局数组GlobalArray进行操作，使用了临界区的话，（看TForm1的btn1Click方法）虽然是连续开启两个线程，但是因为使用了临界区，而且是在线程的Execute中进行操作之前先进入临界区，所以当一个线程运行（也就是执行Execute方法）时，第一个线程进入临界区对全局数组GlobalArray进行性操作，那么另外一个线程就只能等待，等到第一个线程完成操作并且离开临界区之后（此时GlobalArray数组的值是从0到127）。第二个线程开始进入临界区，并且开始对GlobalArray进行操作，最后GlobalArray数组中的值是从128到255。
* 通过使用临界区就可以保证多个线程不能同时对一个资源进行操作，而如果没有使用临界区对线程进行同步的话，那么最后可能是线程1、线程2随机连续对数组进行操作（因为操作系统对线程的时间片的分配是随机的）（还有NextNumber这个全局变量也会被两个线程随机连续操作，可能造成冲突），最后的结果就是当两个线程都执行完成之后，GlobalArray数组中的值没有任何规律，所以这时候就不可能根据规则来控制线程，这时候开发出来的程序就是开发者所不能控制的，显然不利于程序的稳定性等。
* 所以在多个线程会去操作同一个资源等情况的时候，必须对线程进行同步，保证线程的可控性。