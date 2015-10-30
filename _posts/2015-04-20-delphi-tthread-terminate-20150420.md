---
layout: post
title: Delphi线程的终止
categories: delphi之多线程
tags: delphi 多线程 
---


当线程对象的Execute()执行完毕，我们就认为此线程终止了。这时候，它会调用Delphi的一个标准例程EndThread()，这个例程再调用API函数ExitThread()。由ExitThread()来清除线程所占用的栈。

当结束使用TThread对象时，应该确保已经把这个Delphi对象从内存中清除了。这才能确保所有内存占有都释放掉。尽管在进程终止时会自动清除所有的线程对象，但是及时清除已经不再使用的对象，可以使内存的使用效率提高。利用将FreeOnTerminate的属性设置为True的方法来及时清除线程对象时最方便的方法，这只需要在Execute()退出之前设置就行了。设置的方法如下

    procedure TTestThread.Execute;
    var
        i: Integer;
    begin
        FreeOnTerminate:= True;
        for i:=1 to 2000000 do
            inc(Answer, Round(Abs(Sin(Sqrt(i)))));
    end;

这样，当一个线程终止的时候，就会触发OnTerminate事件，就会有机会在事件处理过程中清除线程对象了。

提示：OnTerminate事件是在主线程的环境中发生的。这就意味着，在处理这个事件的过程中，你可以不需要借助于Synchronize()而自由地访问VCL

--- 

要记住Execute()需要经常地检查Terminated属性的值，来确认是否要提前退出。尽管这将意味着当使用线程工作的时候，你必须关心更多的事情，但它能确保在线程结束时，能够完成必要的清除。下面是一段在Execute()增加处理操作的简单代码：

    procedure TTestThread.Execute;
    var
        i: Integer;
    begin
        FreeOnTerminate:= True;
        for i:= 1 to 2000000 do 
        begin
            if Terminated then
                break;
            inc(Answer, Round(Abs(Sin(Sqrt(i)))));
        end;
    end;

注意，在某些紧急情况下，你可以使用Win32 API函数 TerminateThread()来终止一个线程。但是，除非没有别的办法了，否则不要使用它。例如，当线程代码陷入死循环中。TerminateThread()的声明如下

    function TerminateThread(hThread: THandle; dwExitCode: DWORD);

TThread的Handle属性可以作为第一个参数，因此，TerminateThread()常这样调用

    TerminateThread(MyHosedThread.Handle, 0);

如果选择这个函数，应该考虑到它的负面影响。首先，此函数在Windows NT与在Windows95/98下并不相同。在Windows95/98下，这个函数能够自动清除线程所占用的栈；而在Windows NT下，在进程被终止前栈仍被保留。其次，无论线程代码中是否有try...finally块，这个函数都会使线程立即终止执行。这意味着，被线程打开的文件没有被关闭、由线程申请的内存也没有被释放等情况。而且，这个函数在终止线程的时候也不通知DLL，当DLL关闭的时候，这也容易出现enti问题