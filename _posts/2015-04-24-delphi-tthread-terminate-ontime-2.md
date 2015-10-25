---
layout: post
title: Delphi中怎么结束线程（这个线程是定时执行的）（方案二）
categories: delphi之多线程
tags: delphi 多线程
---

[上篇博客](http://xumenger.github.io/delphi-tthread-terminate-ontime-1/)中提出了一个问题：怎么结束一个定时循环执行的线程，并给出了一个解决方案，但是又出现了一个问题，详细去参考上一篇博客。

然后出去撒了个尿，突然脑子里出现了一个想法（看来工作和思考久了，出去走走，哪怕是去撒个尿，都可能尿出火花，所以工作和学习的时常根本就不等同于效率灵感不是在那里拼命工作、想就能出来的），需要结合这篇博客：[关于FreeOnTerminate的知识](http://xumenger.github.io/delphi-thread-freeonterminate/)

##上面所给出的第一种解决方案：##

线程（Execute）在执行定时循环；然后先让 DestroyAThread设置 Terminated属性，通知线程去结束执行、释放资源；然后在线程结束执行、释放资源之后再去通知DestroyAThread，再由DestroyAThread来显式调用线程的 Free方法来释放线程

 
##所以结合 FreeOnTerminate的作用，复习一下：##

类 Create 了就要 Free; 

但 TThread(的子类) 有特殊性, 很多时候我们不能确定新建的线程什么时候执行完(也就是什么时候该释放); 　　

如果线程执行完毕自己知道释放就好了, 所以 TThread 给了一个布尔属性 FreeOnTerminate, 如果为 True, 线程执行完毕后就会自释放.

 
##进行改造：##

首先在线程的Execute方法里面将 FreeOnTerminate设置为True，然后进行自己的定时循环执行；然后DestroyAThread设置Terminated属性，通知线程去结束执行、释放资源；然后在线程结束执行、释放资源之后，因为FreeOnTerminate设置为True了，所以就不要再通知 DestroyAThread，自己在释放完资源、结束执行之后，就会自释放。

同样DestroyAThread设置Terminated属性之后，就可以直接退出，因为不需要在DestroyAThread里面显式释放线程。

同样也就不需要线程再有 CanFree这种通知别人来释放它的属性了，所以也能简化线程类的设计（在面向对象的程序设计中一个原则就是：类尽可能小，所以那些能不需要的属性、方法就不要去定义和使用，把冗余的属性和方法一定要去掉……）

 
##所以新的代码可以是这样的##

###Execute方法：###

    procedure TDoSomeThingThread.Execute;
    var
      i: Integer;
    begin
      inherited;
      FreeOnTerminate:= True;
      //CanFree := False;    //不再需要 CanFree这样的属性了，也就简化了线程类的设计
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
        //CanFree := True;    //不再需要 CanFree这样的属性了，也就简化了线程类的设计
        {在这里将线程申请的资源进行释放……后续操作}
      end;
    end;

###DestroyAThread方法：###

    procedure DestroyAThread(testThread: TDoSomeThingThread); stdcall;
    var
      itime: Cardinal;
    begin
      if Assigned(testThread) then
      begin
        testThread.Terminate;      //在这里将testThread的 Terminated属性设置为True
        
        {
          这里就只需要通过Terminate方法将线程的Terminated设置为True，
          去通知线程结束就行了，
          不需要关心线程的释放问题，因为线程会自己释放
        }
      end;    //这个方案中就不需要使用Free方法去显式释放线程资源了，因为线程自己会在执行结束后自动释放
    end;

　　