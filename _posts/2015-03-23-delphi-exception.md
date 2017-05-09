---
layout: post
title: Delphi中的异常处理
categories: delphi之异常处理 软件质量之稳定性
tags: delphi 指针 内存 异常 异常处理
---


参考[http://www.cnblogs.com/dashan9zj/archive/2008/11/22/1338841.html](http://www.cnblogs.com/dashan9zj/archive/2008/11/22/1338841.html)

 

一.异常的来源
===

在Delphi应用程序中，下列的情况都比较有可能产生异常。

1. 文件处理
2. 内存分配
3. windows资源
4. 运行时创建对象和窗体
5. 硬件和操作系统冲突
6. 网络问题
7. 数据库
8. 控件中的异常
9. DLL文件的异常
10. 强制类型转换
11. 等等


二.异常的处理
===

## 1.try...except...end;

在try 体内的代码发生异常时，系统将转向except 部分进行异常的处理。这是Delphi处理异常的最基本的方式之一。

只有当try 体内的代码发生异常时，才会跳转到except 里面的代码进行执行

## 2.try...finally...end;

这种异常处理结构一般用于保护windows的资源分配等方面，它确保了无论try 体内的代码是否发生异常，都需要由系统进行最后的统一处理的一些Windows对象的正确处理

和try...except...end不同，该结构的finally部分总被执行

## 3.不存在try...except...finally...end 结构

不存在try...except...finally...end 结构来既处理异常，又保护资源分配的结构，但是，try...except...end结构允许嵌套到try...finally...end结构中，从而既处理异常，又保护资源的分配


三.异常的精确处理
===

## 1.定义一个异常

在Delphi中，每个异常都是Exception类的一个派生类。因此，定义一个异常就是定义一个Exception类的派生类

    type
        EMyException = class(Exception);

当然，基类可以是Exception或者是Exception的任何一个任何层次的派生类

## 2.在程序中抛出一个异常

根据不同的情况抛出异常是使用异常的最基本的模式。在Delphi中，由raise语句来实现

### raise 异常类.Create('异常信息说明');

注意因为使用了 异常类.Create('异常信息说明'); 所以也就是在内存中创建了一个异常实体。

    begin
      raise Exception.Create('抛出异常');
    end;

例2:

    begin
      raise Exception.CreateFmt('%s %d', ['错误代码:', 999]);
    end;

例3:

    var
      exc: Exception;
    begin
      exc := Exception.Create('发现异常');
      raise exc;
    end;


### 或者直接使用：raise;

看一个代码示例

    var
        iTest: Integer;
        iStr: string;
    ...
    try
        iTest:= StrIntTo(iStr);
    except
        raise Exception.Create('抛出异常');
    end;

上面的代码可能会抛出异常，假如这是一个函数，那么在编程时候如果调用了这个函数的话不要忘记了要捕捉这个异常！

### 建议用法

建议使用`raise 异常类.Create('异常信息说明');`这样的方式，以及另外两个相似的语法方式，因为这样可以在抛出异常的时候尽可能详细的说明异常的情况，那么以后在捕捉异常的时候就可以又比较多的信息来让我们了解到异常出现的地方，如果直接使用raise;的话

在except中使用raise 的时候，执行完raise的代码之后，在except中的且在raise后的代码将不会执行，它会在执行完raise 的代码后直接跳出except，例程

    var
        iTest: Integer;
        iStr: string;
    ...
    try
        iTest:= StrIntTo(iStr);
    except
        raise;
        ShowMessage('不会显示');
    end;

这个代码例子，如果出现异常之后，会进入except，执行完 raise之后将不会执行ShowMessage的语句，而是直接跳出，所以在编程的时候一定要注意！！

如果想要在except中执行的代码，一定要放在raise的前面！！

## 3.在try...except...end中更加精确的捕捉异常

可以使用 `on E:异常类 do...` 结构可以在 do 体内处理特定异常类所抛出的异常。

如果在except中使用 on E: 异常类 do... 的话，在except中的 on E: 异常类 do...之外不能有任何语句，例如下面的语法是正确的

    try
        ...
    except
        on E: Exception do
        begin
            ShowMessage('OK');
            ShowMessage('OK Again');
        end;
    end;

但是下面的方式就错了

    try
        ...
    except
        on E: Exception do
        begin
            ShowMessage('OK');
        end;
            ShowMessage('Not OK');    //如果在except中使用 on E: 异常类 do 的话，在except中的 on E: 异常类 do之外不能有任何语句
    end;

　　 
四.常见应用场景下的异常处理
===

异常信息的处理方法（写日志、抛出异常），函数或过程出现异常时，只能抛出异常，禁止弹出对话框架。

函数及过程、控件中发生异常，通常是直接抛出异常，不要显示信息提示框；

界面操作（按钮）中的异常，可以显示提示信息（视具体应用而定），也可以不显示提示而将异常信息保存到日志文件或两种方式同时使用；

DLL文件中的异常：如果是函数或过程，发生异常时就直接抛出异常；如果是界面操作，则按界面操作（按钮）中的异常处理方法处理。


五.异常的调试
===

## 1.如何设置才能进行异常调试

在Delphi IDE中，解除“Debugger Options”（可以使用菜单Tools->Debugger Options->OS Exception...进行访问）中的Integrated Debugging复选框的勾选状态可以进行异常的调试。

## 2.怎么能按照自己的异常处理来处理而不是报系统信息

另外还有一个可能会遇到的困惑：为什么明明自己在代码中捕捉了异常并且进行了异常处理，但是最后显示的还是系统的报错框而不是我自己的处理效果。

解决方法：需要在Tools-> Debugger Options-> Language Exceptions 配置界面上的 Stop on Delphi Exceptions选项解除勾选，就会是按照自己的异常处理逻辑处理，而不是系统的报错。


六.异常的补充说明
===

1.每一段程序都有可能出错！这是软件业的一个不容置疑的现象和规律。事实上，传统的 if...else...结构完全可以解决所有的错误，使用 Exception 机制也没能够回避在最原始的层次，通过遍历可能的情况来产生异常的做法，那么，为什么还需要异常机制

答案很简单：异常提供了一种更加灵活和开放的方式，使得后来的编程者可以来根据实际的情况处理这种错误，而不是使用预先设定好的处理结果。实际上，这就是异常处理机制的核心