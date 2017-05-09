---
layout: post
title: Delphi中怎么结束线程（这个线程是定时执行的）（方案一）
categories: delphi之多线程
tags: delphi 多线程
---


在线程对象被释放之前，首先要检查线程是否还在执行中，如果线程还在执行中（线程ID不为0，并且线程结束标志未设置），则调用Terminate 过程结束线程。Terminate 过程只是简单地设置线程类的 Terminated标志，如下面的代码：

    procedure TThread.Terminate;
    begin
        FTerminated:= True;
    end;

所以线程仍然必须继续执行到正常结束之后才行，而不是立即终止线程，这一点要注意

在这里说一些题外话：如何才能“立即”终止线程（当前是指用TThread 创建的线程）。结果当然是不行！终止线程的唯一的方法就是让Execute 方法执行完毕，所以一般来说，要让你的线程能尽快终止，必须在Execute 方法中在较短的时间内不断检查Terminated标志，以便能及时地退出。这是设计线程代码的一个很重要的原则！

当然如果你一定要能“立即”退出线程，那么TThread 类不是一个好的选择，因为如果用API强制终止线程的话，最终会导致TThread 线程对象不能被正确释放，在对象析构时出现 Access Violation。这种情况你只能使用API或者RTL函数来创建线程

 
## 比如这样一个例子：如何终结一个定时执行的线程

有一个线程（ Execute 方法）是定时执行的，就像上面的红字中提到的原则：必须在Execute 方法中在较短的时间内不断检查Terminated标志，以便能及时地退出

    procedure TDoSomeThingThread.Execute;
    var
      i: Integer;
    begin
      inherited;
      CanFree := False;
      try
        while not Self.Terminated do
        begin
          ........
          {
            在这里编写该线程具体所要做的事情的代码逻辑，
            如果这里面还是要循环执行一些任务，
            最好能在每次循环的时候也都再去检查一下Terminated的值
            如果是True就结束循环，
            就是保证在较短的事件内不断检查 Terminated标志，保证能够及时退出
          }
          ........
          Sleep(5);
          Application.ProcessMessages;
        end;
      finally
        CanFree := True;
        {在这里将线程申请的资源进行释放……后续操作}
      end;
    end;

注意这里面的 CanFree 是TDoSomeThingThread的一个属性（对应：private FCanFree: Boolean;）　

CanFree 是为了标记TDoSomeThingThread线程什么时候可以被释放，在线程的Execute函数里面，会在线程跳出循环后将CanFree赋值为True（将CanFree设置为True其实也就是线程结束执行前需要进行的最后一步操作）

* 情况一：就是线程检查Terminated为True，就跳出定时执行的循环，在finally里面将 CanFree设置为True
* 情况二：在try里面出现异常，也会跳出定时执行的循环，在finally里面将 CanFree设置为True

下面再介绍一个函数，用来结束并释放 TDoSomeThingThread的实例

    procedure DestroyAThread(testThread: TDoSomeThingThread); stdcall;
    var
      itime: Cardinal;
    begin
      if Assigned(testThread) then  //首先需要判断线程是否存在
      begin
        testThread.Terminate;      //在这里将testThread的 Terminated属性设置为True
        itime := GetTickCount;     //getTickCount。返回的值是自系统启动以来所经历的时间，单位是：毫秒
        while GetTickCount - itime < 10000 do
        begin
          Sleep(1);　　　　//Sleep()函数的参数的单位也是毫秒
          if testThread.CanFree then
          begin
            Break;
          end;
        end;
        testThread.Free();    //这里显示使用Free方法释放线程的资源
      end;
    end;


## 在这里上面的代码（线程的 Execute和 函数DestoryAThread）

### 1) testThread正在执行

就是正在执行Execute 里面的代码，这时候在Execute 里面检查 Terminated的值，因为是Fasle，所以就可以先进入 第一个 while循环，在进入 for循环，检查Terminated还是为 False，所以继续执行

### 2)调用DestroyAThread(testThread);

首先检查testThread是不是 Assigned，如果是，就用 testThread.Terminate; 将它的Terminated 属性设置为True，然后开始进入循环等待 testThread的Execute里面是不是检查到了 Terminated被设置为True了

其中的循环代码段

    while GetTickCount - itime < 10000 do
    begin
      Sleep(1);
      if testThread.CanFree then
      begin
        Break;
      end;
    end;

在10毫秒钟之内每过一秒去检查一下 testThread的CanFree的值，如果是True就break跳出循环。当线程的Execute执行函数中检测到DestroyAThread(testThread); 函数使用线程的Terminate 方法将线程的Terminated 属性设置为True，这时候线程就会开始跳出原来的循环，跳出循环后会将CanFree 属性设置为True，因为DestroyAThread(testThread) 函数会循环检查线程的CanFree属性，如果检查到其为True，那么就说明线程已经结束了循环，结束了执行，就可以调线程的Free方法来释放线程资源了。

**所以这就是一个结束线程的操作与线程本身的一个反复通信确认能不能结束线程、确认能不能释放线程资源的过程！**

另外上面 Execute的代码中我注释说：

    如果while循环（定时循环）里面还是要循环执行一些任务，
    最好能在每次循环的时候也都再去检查一下Terminated的值
    如果是True就结束循环，
    就是保证在较短的事件内不断检查 Terminated标志，保证能够及时退出

比如

    procedure TDoSomeThingThread.Execute;
    var
      i: Integer;
    begin
      inherited;
      CanFree := False;
      try
        while not Self.Terminated do
        begin
          for i:=0 to 1000 do
          begin
             if Self.Terminated then
                break;
             {
                执行内层循环的代码
             }
          end;
          Sleep(5);
          Application.ProcessMessages;
        end;
      finally
        CanFree := True;
        {在这里将线程申请的资源进行释放……后续操作}
      end;
    end;

之所以在内部的循环中还要每次循环检查 Terminated的值，就是为了保证能够及时退出，因为如果不每次循环都检查，有可能出现这样的情况：在DestroyAThread里面，将Terminated设置为True，但是这时候线程的Execute 里面刚好执行过检查（所以检查到的Terminated为False，所以就会去继续执行），这时候进入它的工作代码中，如果是一个很大的循环，花费的时间假如说是13毫秒，超过了10毫秒，这时候就出现了问题，当线程检查到 Terminated为True，并将CanFree设置为 True时，但是这时候已经过去了 10 毫秒了，所以在 DestroyAThread里面就没有检查到 CanFree，所以这时候线程控制、线程终止就出现问题了

所以通过这个也可以理解为什么要必须在Execute 方法中在较短的时间内不断检查Terminated标志，以便能及时地退出。

这个较短的时间怎么去设置、怎么去控制是多线程编程的一个很重要的技巧！！一方面要考虑到Execute里面怎么去保证能在较短的时间内检查Terminated的值，还要考虑在类似DestroyAThread这样的一个结束线程的函数中设置多少时间来等待线程的反馈

因为还说上面这个例子，还可能存在这样的情况：加入在定时循环里面还有一层（或多层循环），而所有的循环每个循环之前也都去检查了Terminated的值，加入就在其刚检查不为 True的时候（当然就会去继续下面的代码执行），而恰好这时候DestroyAThread函数将 Terminated设置为 True，然后循环10毫秒等待 线程检查到 Terminated的值去结束自己的任务、释放资源，并将 CanFree标识设置为 True通知 DestroyAThread，但是出现了问题：线程的这次循环因为出现死锁或者等待资源或者其他原因而执行了超过10毫秒（假如说20毫秒），20毫秒之后才检查到Terminated的值，再去结束任务、释放资源的时候，可是DestroyAThread因为已经等待超过 10毫秒了。所以这里也就出现了很令人纠结的问题。

？？这个问题要怎么解决呢

### 3)再到testThread的 Execute方法里面

检查到 Terminated属性设置为True（就是通知它要去结束执行了），就跳出定时执行的循环，结束它的工作，如果线程申请了很多资源，这时候也要去释放申请的资源，并将 CanFree设置为True，等待别人检查到它可以退出，并将它Free掉

### 4)再看DestroyAThread(testThread); 里

这里循环检查到 CanFree被 testThread 的Execute方法设置为 True（就是告诉函数，testThread已经在自己的Execute方法里面结束执行的工作了，可以被释放了），然后就 testThread.Free;来释放线程

注意：不要直接调用Destroy()，而调用更安全的Free()方法，因为Free()首选进行检查保证这个对象实例不为NIL，然后调用对象的析构方法Destroy()。

 