---
layout: post
title: Delphi中DLL的创建和使用
categories: delphi之dll
tags: delphi dll
---


* 参考：[http://blog.csdn.net/ninetowns2008/article/details/6311663](http://blog.csdn.net/ninetowns2008/article/details/6311663)
* 结合这篇博客：[http://www.cnblogs.com/xumenger/p/4372289.html](http://www.cnblogs.com/xumenger/p/4372289.html)
* 再结合：[http://blog.csdn.net/lailai186/article/details/8770487](http://blog.csdn.net/lailai186/article/details/8770487)


## 1.DLL简介

DLL是Dynamic-Link Libraries（动态链接库）的缩写，库里面是一些可执行的模块以及资源（如：位图、图标……）。可以认为DLL和EXE基本上是一回事，只是DLL不能直接执行，而必须通过应用程序或者其他DLL调用。

DLL为应用程序间的资源共享提供了方便，同时也是多种语言混合编程的重要手段。由此可见学习使用DLL是Windows程序员必须掌握的一项重要技术


## 2.如何调用DLL

在Delphi中有两种方法调用DLL中的函数和过程，即外部声明或动态加载

### <1>外部声明

在Delphi中外部声明时访问外部例程最容易和最常用的方式，有两种声明方式：通过名字、通过索引号。举例如下：在MYDLL.DLL中有两个函数和一个过程，则其外部声明可以写成

    function test1: Integer;  external 'mydll';  //或者external 'mydll.dll';
    //直接通过名称调用test1（注意名称大小写敏感）
    
    function test11: Integer; external 'mydll' name 'test1';
    //通过名称调用test1，在程序中使用新名称（原名称仍然大小写敏感）
    
    procedure test2; external 'mydll' index 1;
    //通过索引号调用TEST2。程序中可以使用与DLL中不一样的名称

使用外部声明的缺点是程序启动时如果找不到mydll.dll将无法运行，即使没有调用其中的模块。动态加载的方法可以避免这种情况

### <2>动态加载

通过调用Windows API中的相关函数，将DLL调入内存并获得指向函数或过程的指针，执行完模块后释放内存。除了节约内存之外，这种方法的很大的优点是能处理找不到dll或者装入过程中出错的情况。这样即使某个dll有问题，应用程序的其他部分仍然能够正常运行。动态加载的例子如下

    var hDll : THandle;
        Test1 : function : integer;
    begin
        hDll := LoadLibrary('mydll.dll');　　//注意LoadLibrary函数的参数是一个PChar类型的，不是string，该参数是指要加载的DLL的DLL名
        if hDll < 32 then
            exit;    //如果Dll无法记载则跳出
        @Test1 := GetProcAddress(hDll, MakeIntResource(1));　　//取得mydll.dll中的第一个函数的地址。
        //取得mydll 中的第一个函数的地址
       
        ....
        FreeLibrary(hDll);
    end;


## 3.用Delphi创建DLL

用Delphi创建一个DLL是十分简单的，首先需要新建一个DLL的Project（如果使用Delphi3.0则可以在File->New对话框中选择DLL），当然也可以自己写，现在这个Project是这样的

    library Project1;
    uses SysUtils, Classes;
    begin
    end.

当然这是一个空的DLL，现在让我们来加入一个函数，让它成为我们的第一个可以使用的DLL。完成后的文件是这样的

    library dll1;
    uses SysUtils, Classes;
    
    function Test1(a, b: Integer): Integer;
    begin
        Result := a+b;
    end;
    
    exports
        Test1 index 1;
    
    begin
    end.

在这个DLL里我们声明了一个加法函数，然后用exports语句输出它，只有被输出的函数或过程才能被其他程序调用。exports语句的语法是

    exports
        函数名 [index <n>];

`index<n>`是为函数手工指定索引号，以便其他程序确定函数地址；也可以不指定，如果没有使用index 关键字，Delphi将按照exports 后的顺序从1开始自动分配索引号。现在我们可以调用这个DLL了，下面给出一个实例，运行后form1 的标题将变成 1+2=3

    //声明部分
    function Test1(a, b: Integer): Integer; external 'dll1';    //注意此处是大小写敏感的
    
    //运行部分
    form1.caption := '1+2=' + IntToStr(test1(1,2));


## 4.使用DLL的两个技巧

### <1>把现有的项目改成DLL

学会制作DLL之前，大多数程序员手中都积攒下来不少已经完成的项目，如果现在需要把这些项目做成DLL而不是可执行文件，重新写一遍显然是没有必要的，只要按照下面的步骤对已有的项目文件进行修改就可以了

(1)打开项目文件（.DPR），删除单元底部 begin 和 end. 之间的所有语句（一般情况下这些语句是有Delphi 自动生成的）。如果项目中没有用到Form，则从uses 子句中删除表单单元（Form），然后转向(2)

(2)对项目进行修改，令除了 Main Form之外的所有Form 都是动态生成的，这样我们只要在DLL 输出的一个函数或过程中生成Main Form，即可调用执行整个项目。我们假设Main Form的名字是MyMainForm，项目的名字是MyDll，现在在单元底部的begin语句加入之前加入一个过程，过程的名字是RunMyDll，这个过程动态生成Main Form，从而运行整个项目。RunMyDll的写法如下

    procedure InitDll2;
    begin
        Application.CreateForm(TMyMainForm, MyMainForm);
        MyMainForm.Show;    //如果MyMainForm不可视就不用加这一句
    end;

(3)如果想要输出其他函数或者过程，而原来的项目中没有，则可以在单元底部的begin语句之前加上这些代码

(4)在单元底部的begin 语句之前加上一个 exports小节，然后写出所有想输出的函数或过程的名字（最好指定索引号）。注意如果执行了第(2)补，一定要输出 RunMyDll的过程

(5)将项目文件顶部的保留字program改为library

(6)编译

现在就可以在其他程序中调用本项目中的函数和过程了，只要执行 RunMyDll 就可以执行这个项目，和执行原来的可执行文件一模一样

### <2>创建一个引入文件

如果DLL比较复杂，则为它的声明专门创建一个引入程序单元将是十分有意义的，并且会使这个DLL变得更加容易维护。引入单元的格式如下

    unit MyInport;    {Import unit for MyDll.Dll}
    interface
        procedure RunMyDll;
    implementation
        procedure RunMyDll; external 'MyDll' index 1;
    end.

这样以后想要使用MyDll的例程时，只要简单的在程序模块中的uses子句中加上 MyImport就可以了


## 5.DLL的初始化和善后工作

一般的DLL不需要做初始化和善后工作。但是如果你想让你的DLL在被载入时先设定一些初始设定，或者退出时释放资源，则可以有三种方法达到目的

### <1>利用在 Unit 的Initialization 与 Finalization这两个小节

可以在Unit的这两个小节里安排Unit 的进入和退出，但是 Program 与 Library并没有这两个部分，所以只能写在Unit里面

### <2>利用 ExitProc 变量

在Library 的begin .. end.中间是可以写代码的，这里可以放置DLL初始化代码。如果想要做善后工作，则可以利用ExitProc 变量。我们首先在初始化代码中把 ExitProc中包含的默认的善后过程地址保存下来，然后把自定义的过程的地址赋给他，这样DLL退出时就会执行我们制定的程序；在自定义的过程的最后，把ExitProc恢复原来的默认值，以便DLL 能股继续完成原来默认的善后工作。下面是示例

    library MyDLL;
    ...
    OldExitProc : pointer;
    ...
    procedure MyExitProc;
    begin
    ...//善后程序
    ExitProc := OldExitProc;
    end;
    ...
    begin
    ...//初始化程序
    OldExitProc := ExitProc;
    ExitProc := @MyExitProc;
    end.

以上的代码同样是关于Delphi的指针和函数指针的一个很好的应用的例子，值得研究一下

### <3>利用DllProc变量

和ExitProc 一样，DllProc也是一个在Systemd单元里预定义的变量。在使用DLLProc时，必须先写好一个具有以下原型的程序：　　骗人哦测读热 DLLHandler(Reason : Integer);

并在Library 的 begin ... end.之间，将这个DLLHandle程序的执行地址赋值给DLLProc，这时候就可以根据参数Reason的值分别作出相应的处理。另外注意将 windows单元加入uses子句中。示例

    library TestDLL;
    ...
    procedure MyDLLHandler(Reason : Integer);
    begn
        case Reason of
            DLL_Process_Attach;    //整个DLL的初始化代码
            DLL_Process_Detach;    //整个DLL的善后程序
            DLL_Thread_Attach;    //当主叫端开始一个Thread时
            DLL_Thread_Detach;    //当主叫端终止一个Thread时
        end;
    end;
    ...
    begin
        ..    //初始化代码
        DLLProc := @MyDLLHandler;
        MyDLLHandler(DLL_Process_Attach);
    end;

由上例可知，当DLL支持多进程的处理是，DLLProc非常适合使用

 
## 6.DLL中的例外处理

在用Delphi制作DLL的时候，在例外处理方面请注意以下三点

1. 如果uses子句中没有SysUtils的话，无法使用例外处理
2. 如果DLL中没有对例外进行处理的话，这个例外会向外传导到主叫端的应用程序。如果该应用程序也是Delphi写的话，这个例外可以由主叫端进行处理
3. 承上，如果主叫端的程序不是Delphi 或Borland C++ Builder，则例外以作业系统错误的形式来处理，例外编号是$0EEDFACE，ExceptionInformation中地第一个进入点是例外发生的地址，第二个进入点是指向Delphi 例外物件的引用