---
layout: post
title: Delphi对象释放后，为什么对象的数据成员依然可以用？
categories: delphi之面向对象 delphi之指针与内存 
tags: delphi 面向对象 指针 内存
---

##说一下软件开发

这两天工作上确实比较忙，导致自己连晚上的时间都要拿出来放到工作上，自己的自学计划都被耽误了。没办法，客户现场的问题太多，又催的很急。

就拿今天我排查的一个问题来说，找到原因之后，简直惊呼：“操蛋，原来原因是这个？！”

这个问题就是因为使用Delphi开发时，错误的使用面向对象的语法而导致报错，另外因为自己对异常处理的逻辑还是有一些理解的不够全面（或者说自己理解的太死板）而导致没有很快的根据错误信息来定位出错的代码行，导致自己走了不少弯路。

其实看我在这两篇博客中所整理的两个点，单拿出来确实很简单、很直观，简直没有任何的技术含量，但是当这两个点放到成千上万行的代码中，加上自己对这两个点没有很熟悉，那么在出现问题的时候，怎么可能快速而精确地定位，也许这就是软件开发之所以比较有难度的原因之一，也是为什么要做出来一个高质量的软件项目实在是很难的原因之一。

本篇先谈论一下关于Delphi面向对象开发中的一些内存相关的问题

##先看这样一段代码

```
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

  TT = class(TObject)
    xxx: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  t: TT;
begin
  t:= TT.Create;
  t.xxx:= 1000;

  t.Free;
  
  ShowMessage(IntToStr(t.xxx));
end;

end.
```

看到这段代码，你可能立马会说：“靠，这明显有问题，在对象被Free之后，在去访问对象的数据成员，显然会报错”，但是当你编译运行之后，发现点击按钮之后确实弹出框了，没有任何报错，不出意外，弹出框中的数据就是1000。

注意我在上面所说的那个词：“不出意外”，也就是说虽然这段代码绝大多数情况下是不会出错，而且绝大多数情况可以显示“正确的值”，但是这是有隐患的，今天我排查的一个问题就是因为项目代码中有类似的这么一行不小心写出来的代码导致的。

可能的运行现象：

* 上面的代码绝大多数情况下是不会报错，并且能获取“正确的值”的
* 但是总可能出现因为那块内存已经被再次使用，并且做了修改，而导致获取错误的值
* 还有可能出现内存访问报错（虽然只是读操作），比如我今天排查的问题就是报错：Access violation at address 0B897D30 in module 'xxxxxx.dll'. Read of address 10950B48

因为对象刚刚被释放，所以其实它之前所占用的那块内存可能还没有被用作它用，所以原来的数据还在，在对象指针调用了Free方法之后，其实对象指针还是指向原来的那块内存的，再通过这个对象指针去访问其实还是可以访问到的，所以也是为什么建议在Delphi开发中将对象释放了之后，再去将对象指针（对象名）置为nil的原因，这就是使用指针的一个隐患点。

下面通过两个代码实例去证明一下它确实有问题。

##验证其有问题的实验代码一

上面的程序只是读取内存操作，在上面所说的各种巧合都满足的情况下，确实可以顺利访问到内存，获取“正确的值”，但是如果想要去进行写操作，那么就会报内存错误，这就是说明虽然可以访问这块内存，但是实际上它就是非法的。详细说明在代码中也有注释

```
procedure TForm1.btn1Click(Sender: TObject);
var
  t: TT;
begin
  try
    t:= TT.Create;
    t.xxx:= 1000;
    t.Free;
    t.xxx:= t.xxx - 1;    //这里对释放后的内存进行了写操作，但是我进行断点跟踪发现运行到这里并没有报错
  except
    on E: Exception do
    begin
      ShowMessage(e.Message);   //但是代码逻辑并没有运行到这里，不知道为什么？明明就应该直接在写“非法内存”的时候报异常的呀？！
    end;
  end;
  ShowMessage(IntToStr(t.xxx)); //这里的弹出框也顺利弹出
  //但是在ShowMessage之后才报内存错误：“Access violation at address 0B897D30 in module 'Test.exe'. Read of address 10950B48”
  //虽然报错的代码处不符合自己的预测，但是确实报错了，且必然报错，这就说明，访问了非法内存
  //具体的原因，目前我也没办法解释，相信等自己研究操作系统原理、编译原理足够深入之后，就能很容易解释这个现象了
end;

end.
```

##验证其有问题的实验代码二

当将对对象释放了之后，没有去直接访问那块内存，而是进行大量的内存读写、申请、释放操作，那么可能就把原来的内存占用了，如果再去访问非法内存，就没有办法获取“正确的值”了

```
procedure TForm1.btn1Click(Sender: TObject);
var
  t: TT;
  p: Pointer;
  i: Integer;
begin
  t:= TT.Create;
  t.xxx:= 1000;

  t.Free;
  //对象释放后，没有去立即访问原来的内存，而是进行一系列大量的内存操作
  
  for i:= 0 to 10000 do
  begin
    GetMem(p, 100);   //反复去申请100字节的内存
  end;
  
  //然后再去尝试读非法内存，不出意外，这时候的值就不是“正确的值了”
  ShowMessage(IntToStr(t.xxx));
end;
```

##总结

还是那句话，这么直接拿出来，很明显，但是假如在成千上万上代码中有掺杂这么不规范的代码，确实很头疼。

现在自己更多的只是总结现象 ，并且做简单的解释，但是更为底层的原理、原因，自己目前的水平还是理解不了，所以对于包括操作系统、编译原理……真正有技术含量的理论知识，还是必须在接下来的时间里结合实践去主动的自学、深入钻研
