---
layout: post
title: 正确理解Delphi中的传值调用/传址调用
categories: delphi之函数 delphi之指针与内存 
tags: delphi 函数 指针 内存
---

##说明

* 请结合这篇文章一起了解指针、内存、地址：[《正确理解C/C++中的传值调用/传址调用/引用调用》](http://www.xumenger.com/c-cpp-function-value/)
* 具体的现象在下面的程序的代码中有说明，具体的原理解释和上面的文章中一样，就不再这里再做说明了

##通过实例展示

```
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

function TestValue(testi: Integer): Integer;      //测试传值调用
function TestAddress(testi: PInteger): Integer;   //测试传址调用

implementation

{$R *.dfm}

//测试传值调用
procedure TForm1.btn1Click(Sender: TObject);
var
  i: Integer;
begin
  i := 0;
  TestValue(i);     //传值调用中的弹出框弹出 1
  ShowMessage(IntToStr(i)); //这里弹出框弹出 0，说明i 传入TestValue后并没有变化
  
end;

//测试传址调用
procedure TForm1.btn2Click(Sender: TObject);
var
  i: Integer;
begin
  i := 0;
  TestAddress(@i);      //传址调用中的弹出框弹出 1
  ShowMessage(IntToStr(i));  //这里弹出框弹出也是 1，说明i 传入TestAddress后真的变化了
  
end;

function TestValue(testi: Integer): Integer;
begin
  Inc(testi);
  ShowMessage(IntToStr(testi));
  
end;

function TestAddress(testi: PInteger): Integer;
begin
  Inc(testi^);
  ShowMessage(IntToStr(testi^));
end;

end.
```
