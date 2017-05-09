---
layout: post
title: 理解Delphi编程中方法参数定义的关键字const
categories: delphi之函数 c/c++之指针与内存 
tags: delphi 函数 指针 内存
---

## 说明

* 本文的主要目的是补充[《Delphi和C/C++配合编程时的字符串传值规范》](http://www.xumenger.com/delphi-vcpp-string-chararray-20160511/)
* 不光在跨语言编程的时候需要注意字符数组、字符串、字符指针的规范使用
* 在Delphi一种语言的编程中也要注意开发规范
* 下面一个实例进行展示，详细的编译、运行情况在程序的注释中有详细的说明
* 可以自己进行尝试
* Delphi的版本是6

## 源码展示

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

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure ReadString(test: PChar);
begin
  ShowMessage(test);
end;

procedure WriteString(test: PChar);
begin
  test := '测试测试';       //这种写操作并不会抛出异常

  ShowMessage(test);        //弹出框的信息是'测试测试'
  
  //为什么test的值在该方法内部可以变化
  //同时看到在执行完该方法后，入参的值并没有变
  //这就是Delphi的传值调用
  //参见[正确理解Delphi中的传值调用/传址调用](http://www.xumenger.com/delphi-value-address-func-proc-20160506/)
end;

procedure WriteconstString(const test: PChar);
begin
  test := '测试测试';       //定义test是const的，这里对其进行写操作会编译通不过

  ShowMessage(test);
end;

procedure CopyString(test: PChar);
begin
  StrPLCopy(test, '测试测试', 100);   //这种写操作会抛出异常
  ShowMessage(test);
end;

procedure CopyconstString(const test: PChar);
begin
  StrPLCopy(test, '测试测试', 100);   //虽然定义test是const，但是这种写操作只是改变test指向的内存，并没有改变test本身
                                      //还是会在这里抛出异常
  ShowMessage(test);
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  s: string;
begin
  s:= '测试';
  ReadString(PChar(s));
  ShowMessage(s);               //弹出'测试'

  WriteConstString(PChar(s));   //如这个方法中说明的：编译通不过
  ShowMessage(s);

  WriteString(PChar(s));        //在WriteString中弹出'测试测试'
  ShowMessage(s);               //到这里弹出得到依然是'测试'

  CopyString(PChar(s));         //抛出异常：非法地址访问
  ShowMessage(s);

  CopyconstString(PChar(s));    //抛出异常：非法地址访问
  ShowMessage(s);

end;

end.

```
