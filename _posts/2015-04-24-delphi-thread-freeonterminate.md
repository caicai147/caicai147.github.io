---
layout: post
title: Delphi的TThread中的FreeOnTerminate成员
categories: delphi之多线程
tags: delphi tthread freeonterminate 多线程
---


类 Create 了就要 Free; 

但 TThread(的子类) 有特殊性, 很多时候我们不能确定新建的线程什么时候执行完(也就是什么时候该释放); 　　

如果线程执行完毕自己知道释放就好了, 所以 TThread 给了一个布尔属性 FreeOnTerminate, 如果为 True, 线程执行完毕后就会自释放.

首先看下面的例程

    type
      TMyThread = class(TThread)
      protected
        procedure Execute; override;
      end;
    
    procedure TMyThread.Execute;
    var
      i: Integer;
    begin
      FreeOnTerminate := True; {这可以让线程执行完毕后随即释放}
      for i := 0 to 500000 do
      begin
        Form1.Canvas.Lock;
        Form1.Canvas.TextOut(10, 10, IntToStr(i));
        Form1.Canvas.Unlock;
      end;
    end;
    
    procedure TForm1.Button1Click(Sender: TObject);
    begin
      TMyThread.Create(False);
    end;

TThread 类有一个抽象方法(Execute), 因而是个抽象类, 抽象类只能继承使用, 上面是继承为 TMyThread.

继承 TThread 主要就是实现抽象方法 Execute(把我们的代码写在里面), 等我们的 TMyThread 实例化后, 首先就会执行 Execute 方法中的代码.

按常规我们一般这样去实例化:

    procedure TForm1.Button1Click(Sender: TObject);
    var
      MyThread: TMyThread;
    begin
      MyThread := TMyThread.Create(False);
    end;
    
因为 MyThread 变量在这里毫无用处(并且编译器还有提示), 所以不如直接写做 TMyThread.Create(False); 

 
###关于 FreeOnTerminate###

在 TThread 类的例子中, 应该有这句: `FreeOnTerminate := True; `

先说它什么意思:
* 类 Create 了就要 Free; 
* 但 TThread(的子类) 有特殊性, 很多时候我们不能确定新建的线程什么时候执行完(也就是什么时候该释放); 
* 如果线程执行完毕自己知道释放就好了, 所以 TThread 给了一个布尔属性 FreeOnTerminate, 如果为 True, 线程执行完毕后就会自释放.

有些书介绍说: FreeOnTerminate 的默认值是 True(错误!), 经落实, 应该是 False, 起码在 Delphi 2007 和 2009 中是这样; 或许以前的某个版本和现在不一样。现在不考虑是哪个版本，也不考虑FreeOnTerminate的默认值是True 还是False，去显式地将它设置为 True就可以避免这些问题