---
layout: post
title: 理解一个窗体从创建到显示到释放的执行过程
categories: delphi之应用程序框架 delphi之指针与内存 delphi之控件
tags: delphi 指针 内存 控件
---


之前对于这样的代码

    Form1:= TForm1.Create(Application);
    try
        Form1.ShowModal;
    finally
        Form1.Free;
    end;

当时就迷惑了：明明try...finally...end是顺序执行的，那这样的代码岂不是刚刚创建了窗体，刚刚显示了窗体，然后就立刻Free了窗体吗？

那这样的代码到底有什么意义呢？

甚至怀疑其try...finally...end这个语法是不是顺序执行的。

现在想想当时真是傻帽，自己去写个代码，在 `Form1:= TForm1.Create(Application);` 加个断点，然后去一步一步跟着走不就可以发现了吗？

今天就去写了一极其简单的代码调试了一下，发现程序在执行到Form1.ShowModal; 之后会显示窗体，然后就在这里停住了。接着我点击了上面的一个按钮（这个按钮有处理OnClick事件的方法），这个时候代码运行就跳到这个响应点击按钮的方法中了。

所以，经过调试之后，发现问题很简单：当执行到`Form1.ShowModal`之后，就会显示窗体，这个时候对于窗体是不会向下执行的（比如这个例子中是不会执行Form1.ShowModal之后的代码的），而是等待响应你点击窗体、按钮……操作窗体或窗体上控件的事件（如果这些事件有对应的响应方法，就会跳到这个方法中执行，如果没有对应的相应方法，就会“沉默”），直到你点击上面的“X”来关闭窗体，才会接着执行到Finally里面的Free。

所以这个情况的出现不是因为try...finally...end; 结构的原理，而是因为窗体的特性，它需要在显示之后响应事件，而不会直接就顺序执行了。

另一个重要的反思就是

>如果在编程过程中对代码逻辑出现了疑惑，请一步一步进行单步调试，直到精确锁定到确切的位置！！

最后再说一点，建议将上面的代码改为下面的形式，保证能够处理所有可能的异常，提升程序的稳健性

    try
        Form1:= TForm1.Create(Application);
        try
            Form1.ShowModal;
        finally
            Form1.Free;
        end;
    except
        on E: Exception do
            ShowMessage(E.Message);
    end;

这段代码，如果Form1:= TForm1.Create(Application); 创建成功，就会接着执行下面的显示窗体的代码。

如果创建失败，就会直接跳到except进行异常处理，否则将会顺序执行窗体创建之后的代码，并且不会执行except 里面的代码。