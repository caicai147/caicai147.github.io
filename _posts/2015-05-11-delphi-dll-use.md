---
layout: post
title: Delphi DLL的创建、静态及动态调用
categories: delphi之dll
tags: delphi dll
---


* 转载：[http://blog.csdn.net/welcome000yy/article/details/7905463](http://blog.csdn.net/welcome000yy/article/details/7905463)
* 结合这篇博客：[http://www.xumenger.com/delphi-dll-20150423-05/](http://www.xumenger.com/delphi-dll-20150423-05/)
* 再结合：[http://blog.csdn.net/lailai186/article/details/8770487](http://blog.csdn.net/lailai186/article/details/8770487)

不过这篇博客有很多讲解的不清楚，甚至有一些误解的地方，但是将这几篇篇文章结合起来，就可以互相弥补、互相补充，比如：

1. 这篇博客在介绍静态调用的时候，external 'DLL文件'，时候没有将文件的.dll后缀加上，而是直接使用DLL的文件名，这是不对的
2. 也没有讲解到 stdcall的问题……
3. 也没有讲解到string和ShareMem单元的问题

但是那篇博客里面也讲解了一些本文中所没有涉及到的内容

总之，对于任何人的任何说法（比如博客），甚至是所谓的官方的说法，一定要保持怀疑，要一直去求证、去实践、去了解更多的相关的知识和说法

还有必要参考：[http://www.xumenger.com/delphi-dll-20150423-05/](http://www.xumenger.com/delphi-dll-20150423-05/)

 
##一、DLL简单介绍##

首先简单介绍一下DLL：

###1.减少可执行文件的大小###

DLL技术的产生很大一部分原因是为了减少可执行文件的大小。当操作系统进入 Windows时代之后，其大小已经达到几十兆乃至几百兆。试想如果还是使用 DOS 时代的单执行文件体系的话一个可执行文件的大小可能达到几十兆，这是大家都不能介绍的。解决的方法就是采用动态链接技术奖一个大的可执行文件分割成许多小的可执行文件

###2.实现资源共享###

这里指的资源共享包括很多方面，最多的是内存共享、代码共享等等。DLL还有一个突出的特点就是在内存中只装载一次，这一点可以节省有限的内存，而且可以同时为多个进程服务

###3.便于维护和升级###

在软件运行出现问题的时候，我们并不需要重新安装程序，只需要替换相应的DLL文件即可。

###4.比较安全###

这里说的安全也包括很多方面。比如，DLL文件遭受病毒的侵害机率要比普通的EXE文件低很多。另外，由于是动态链接的，这给一些从事破坏工作的“高手”们多少带来了一些反汇编的困难。

 
##二、在Delphi中编写DLL文件##

编写DLL文件其实不是什么困难的事情，和我们平时在Delphi中编写程序基本相似，下面先以一个简单的例子来说明

    //这个例子是直接在项目文件中声明函数并且在项目文件中export函数
    //常用的是在其他的单元文件中声明函数，然后在项目文件中引入该单元，并在项目文件中export这些单元中的函数
    //只export需要export的函数，其他比如不需要发布的函数就不需要export
    
    library DLL;
    
    { Important note about DLL memory management: ShareMem must be the 
      first unit in your library's USES clause AND your project's (select 
      Project-View Source) USES clause if your DLL exports any procedures or 
      functions that pass strings as parameters or function results. This 
      applies to all strings passed to and from your DLL--even those that 
      are nested in records and classes. ShareMem is the interface unit to 
      the BORLNDMM.DLL shared memory manager, which must be deployed along 
      with your DLL. To avoid using BORLNDMM.DLL, pass string information 
      using PChar or ShortString parameters. } 
    
    uses
        ShareMem, SysUtils, Classes;
    
    function testStr(i: Integer): TStringList; stdcall;   
    //这里定义的参数i没有实际意义，就是为了展示DLL中有参数的函数的创建，
    //以及在下面展示有参数的函数的调用（其实和无参数的函数一样，不过想
    //通过参数更清楚的展示这就是函数，而不是变量）
    var
        str: String;
        strlist: TStringList;
    begin
        strlist:= TStringList.Create;
        strlist.Add('hello');
        srelist.Add('world');
        str:= 'hello world';
        result:= strlist;
    end;
    
    function testInt(i: Integer): Integer; stdcall;
    begin
        Inc(i);
        result:= i;
    end;
        
    {$R *.dfm}
    
    exports
        testStr,
        testInt;
    
    begin
    end.

可以看出在上面的DLL文件中我们封装了两个函数。跟一般Delphi编写的程序基本相同，只是在DLL文件中的函数后面多了一个stdcall 关键字，并且用 exports声明需要引出的函数。只要编译上面的代码就可以得到一个动态链接库的DLL文件

1. 在DLL中编写的函数必须在后面加上 stdcall调用参数，如果忘记添加 stdcall参数的话虽然可以编译通过，但是在调用这个DLL时可能会出现很严重的错误，导致操作系统的死锁
2. 所写的函数和过程必须用 exports 声明为外部函数。若没有声明，那么DLL内部的函数是不可调用的，这一点显然是我们不想看到的
3. 当使用了长字符串类型的参数、变量时，如string，要引用 ShareMem。虽然 Delphi中的String功能很强，但是如果您编写的DLL文件要供其他编程语言调用时（比如C/C++），最好使用 PChar类型。

注意：如果您要坚持使用 string 类型的参数、变量甚至是记录信息时，就要引用 ShareMem单元，而且这个单元必须是第一个引用的，即在uses语句后的第一个单元，比如：

    uses
        ShareMem,SysUtils,Classes;`

另外，还有一个地方我们需要添加ShareMem 单元，即在您的工程文件(\*.dpr)中的uses第一个引用 ShareMem单元。这里需要注意的是在 \*.dpr文件中而不是在单元文件(\*.pas)中添加相同的单元，这一点Delphi自带的帮助文档没有说清楚，造成了很多误会。若不这样的话，您可能付出死机的代价。避免使用 string 类型的方法是将其转换为 PChar或ShortString类型

 
##三、DLL文件的调用##

###1.静态调用###

调用DLL 文件要比编写DLL容易多了，这里先总结一下DLL的静态调用，首先看下面的小例子

    //静态调用  
    function testInt(i: Integer): Integer; stdcall; external 'DLL.dll'; 
    //静态调用使用别名方式  
    function test(i:integer):integer; stdcall; external 'MyDLL.dll' name 'testInt';
    
    
    procedure TForm1.btn2Click(Sender: TObject);
    begin
        showMessage(IntToStr(testInt(1)));
        //或者showMessage(IntToStr(test(1)));  这是使用别名的情况
    end;

大家可以看到我们做的唯一的工作是将DLL的函数说明放在implementation下（或是放在文件单元的var下），并且用external 语句指定了 DLL的位置。这样我们就可以直接使用 DLL中的函数了

1. 调用参数 stdcall，在引用DLL中的函数时也要使用stdcall参数
2. 用external 声明DLL文件的位置。注意文件的后缀名一定要加上
3. 不能从DLL 中调用全局变量。如果我们在 DLL中声明了某种全局变量，如var s: byte。这样在DLL中 s 这个全局变量是可以正常使用的，但是 s 不能被调用程序使用，即 s 不能作为全局变量传递给调用程序。不过在调用程序中声明的变量可以作为参数传递给 DLL

###2.动态调用###

动态调用 DLL文件要复杂的多，但是非常灵活。在运行大的程序时可以节省内存空间，看下面的例子

    procedure TForm1.btn1Click(Sender: TObject);
    type
        TAddc = function(i: Integer): TStringList; stdcall;
    var
        hh: THandle;
        addc: TAddc;
        temp: TStringList;
        i: Integer;
    begin
        hh:= LoadLibrary('DLL.dll');
        try
            if hh <>0 then
                @addc:= GetProcAddress(hh, PChar('testStr'));　　
                //GetProcAddress函数的第二个参数是想要获取DLL中函数地址的那个函数名，注意第二个参数类型是PChar
            if @addc<>nil then
            begin
                temp:= addc(i);
                for i:= 0 to temp.Count-1 do
                    ShowMessage(temp[i]);
            end
            else
            begin
                RaiseLastWin32Error;
            end;
        finally
            FreeLibrary(hh);
        end;
    
    end;

由上面的代码可以看出，这种动态调用技术比较复杂。但是只要更改参数就可以动态更改所有调用DLL文件的名字

1. 在 type 中定义的所要调用的函数或过程的类型，在这里同样也要加上 stdcall 参数
2. 释放 DLL。在上面的程序中我们使用 LoadLibrary函数加载了dll文件，在使用完 DLL文件时切记调用 FreeLibrary 函数来释放Dll，否则Dll将会一直占用你的内存直至您退出Windows或是关机为止。这里需要注意的是确保在释放DLL的时候没有任何指针指向该DLL所定义的变量、过程……否则让无法正常释放，招致访问冲突异常，或是导致程序出现空指针，导致程序意外中止
3. 注意要进行异常捕获
4. 在动态调用的DLL函数中我们使用了 String 类型，由于在两处文件内都引用了 ShareMem，所以String类型的数据可以正常使用，以及作为参数来传递

 
##四、静态和动态调用的对比##

1. 静态方法实现简单，易于掌握。并且一般来说速度更快，也更安全可靠一些。但是静态方法不能灵活地在运行时卸载不再需要的DLL，而主程序在开始运行时就装载了DLL，直到程序结束时才释放该DLL
2. 动态方法很好地解决了静态方法中的不足，可以方便的访问DLL中的函数和过程。但是动态方法难以掌握，使用时因为不同的函数或过程需要定义很多复杂的类型和调用方法