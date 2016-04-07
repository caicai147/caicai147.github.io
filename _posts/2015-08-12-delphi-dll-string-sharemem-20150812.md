---
layout: post
title: Delphi在创建和使用DLL的时候如果使用到string，请引入ShareMem单元
categories: delphi之dll
tags: delphi dll sharemem
---


当使用了长字符串类型的参数、变量时，如string，要引用ShareMem。

**虽然Delphi中的string功能很强大，但若是您编写的Dll文件要供其它编程语言调用时，最好使用PChar类型。如果您要坚持使用string类型的参数时、变量甚至是记录信息时，就要引用ShareMem单元，而且这个单元必须是第一个引用的，即在uses语句后的第一个单元。**

下面通过一个项目示例来讲解怎么使用ShareMem。

##先新建一个DLL项目##

先新建一个DLL项目，然后再新建一个Unit1单元。

###工程文件是这样的###

    library Project2;
    
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
      ShareMem,
      SysUtils,
      Classes,
      Unit1 in 'Unit1.pas';
    
    {$R *.res}
      exports
        test;
    
    
    begin
    
    end.

###单元文件里面实现了test函数####

    unit Unit1;
    
    interface
      function test(const input: string): string; stdcall;
    
    implementation
      function test(input: string): string; stdcall;
      begin
        Result:= '我是DLL中的test函数，输入是：' + input;
      end;
    end.

###引入ShareMem的注意事项###

1. 因为DLL中使用到string类型，所以一定要引入ShareMem单元。如果没有用到string就不需要ShareMem，但是为了保险起见，还是建议引入ShareMem单元
2. 以这个项目为例，注意要在DLL的项目文件中引入ShareMem单元，而不是在函数的声明和实现单元Unit1里面引入ShareMem单元
3. 在项目文件中可能会引入很多单元，但是ShareMem单元一定要第一个引入，为了第一个加载


##新建加载这个DLL的项目（生成可执行文件）##

先新建一个项目，然后再新建一个Unit1单元。

上面的那个DLL的项目编译生成了Project2.dll动态链接库，将该动态链接库文件放到这个可执行项目的项目目录下方便加载

###项目文件的源码是这样的###

    program Project1;
    
    uses
      ShareMem,
      Forms,
      Unit1 in 'Unit1.pas' {Form1};
    
    {$R *.res}
    
    begin
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end.

###单元文件的代码是这样的###

单元文件的代码是这样的（这个单元文件中主要是窗体相关代码）

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
        { Private declarations }
      public
        { Public declarations }
      end;
    
    var
      Form1: TForm1;
    
    implementation
    
    {$R *.dfm}
    
    procedure TForm1.btn1Click(Sender: TObject);
    type
      TAddc= function(const input: string): string; stdcall;
    var
      hh: THandle;
      addc: TAddc;
    begin
      hh:= LoadLibrary('Project2.dll');
      try
        if hh<>0 then
          @addc:= GetProcAddress(hh, PChar('test'));
        if not (@addc = nil) then
        begin
          ShowMessage(addc('lsls'));
        end;
      finally
        FreeLibrary(hh);
      end;
    
    end;
    
    end.

这个可执行项目加载了上面的DLL，而上面的那个DLL里面使用到了string，所以这个项目文件中也需要引入ShareMem单元。

###但是也是有注意事项的：###

1. 加载了使用string的DLL的项目也需要引入ShareMem。如果没有用到string就不需要ShareMem，但是为了保险起见，还是建议引入ShareMem单元
2. 以这个项目为例，注意要在该可执行项目文件中引入ShareMem单元，而不是在函数的声明和实现单元Unit1里面引入ShareMem单元
3. 在项目文件中可能会引入很多单元，但是ShareMem单元一定要第一个引入，为了第一个加载

###我在实验时候出现的问题###

因为我在可执行项目中引入ShareMem单元时候没有注意到应该在项目文件而不是在单元文件中引入，所以在编译执行之后，可以正确运行，但是当关闭程序的时候却报错，如下图

![image](../media/image/2015-08-12/sharemem.png)

报错：不合法的指针。

将引入ShareMem单元的引入改到项目文件而不是在单元文件中，然后问题就解决了。