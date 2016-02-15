---
layout: post
title: Delphi异常处理的代码逻辑注意点：finally和Continue、finally和Break、finally和Exit
categories: delphi之异常处理 软件质量之稳定性
tags: delphi 异常处理
---

在之前的博客中详细的介绍了Delphi的异常处理得当代码逻辑、异常处理的各方面的知识，详见下面的链接

* [再看Delphi的异常处理](http://www.xumenger.com/delphi-except-20160116/)
* [Delphi异常处理时代码的执行逻辑](http://www.xumenger.com/delphi-exception-20151201/)
* [总结一下最近开发遇到的问题，以及最近需要学习的知识点](http://www.xumenger.com/learn-plan-20151123/)
* [Delphi中的异常处理](http://www.xumenger.com/delphi-exception-20150428/)
* [Delphi中的异常处理](http://www.xumenger.com/delphi-exception/)
* [好的异常处理策略](http://www.xumenger.com/delphi-except-20160128/)

不多说，直接看代码，在代码里面有详细的注释说明

>在循环中如果有出现Continue/Break和finally共用的情况，需要参考下面的代码，需要特别小心此时的代码运行逻辑

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
    procedure btn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//测试Continue和finally在一起时候的代码逻辑
procedure TForm1.btn1Click(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to 10 do
  begin
    try
      if i mod 2 = 1 then
      begin
          //如果当前的数是奇数，那么会执行Continue
          //那么下面的第一个ShowMessage 将会跳过不执行
          //但是在奇数的情况下，同样在Continue下面，在循环里面的finally里面的ShowMessage却一定会执行，不会跳过
        Continue;
      end;
      ShowMessage('Continue下面的，finally上面的ShowMessage： ' + IntToStr(i));
    finally
        //finally里面是一定会执行的
        //虽然是对于Continue的理解是，跳出当前循环（Continue下面的代码将不执行），跳到下一循环
        //但是finally是一定会执行的，尽管它是在循环里，在Continue的下面
        //可是可以理解finally的优先级大于Continue，所以finally里面的代码一定会执行
      showMessage('finally里面的ShowMessage： ' + IntToStr(i));
    end;
  end;
end;

//测试Break和finally在一起时候的代码逻辑
procedure TForm1.btn2Click(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to 10 do
  begin
    try
      if i = 5 then
      begin
        Break;
      end;
        //这个ShowMessage，在i为1、2、3、4时会一直弹出
        //但是在i为5时，因为满足Break的条件，所以跳出循环
        //所以，这里对于i为5，以及之后的数字将不会弹出
      ShowMessage('Break下面，finally上面的ShowMessage： ' + IntToStr(i));
    finally
        //但是异常处理的finally是一个比较特殊的结果
        //下面的在finally里面的ShowMessage，在i为1、2、3、4时和上面的一样会弹出
        //可是在i为5时，虽然执行Break跳出循环，上面的finally不会执行，但是因为finally特殊，依然会在i为5时弹出
        //但是在i为5时彻底会跳出循环，那么后面的6、7、8、9、10便不会弹出，因为在i为5时即结束了循环（Break的特性）
      showMessage('finally里面的ShowMessage： ' + IntToStr(i));
    end;
  end;
end;

//测试Exit和finally在一起时候的代码逻辑
procedure TForm1.btn3Click(Sender: TObject);
begin
  try
    //Exit之前的代码逻辑是执行的，所以这里面的弹出框是会出现的
    ShowMessage('Before Exit');
    Exit;
    
    //Exit之后的逻辑不会执行，所以这里的弹出框是不会出现的
    ShowMessage('After Exit');
  finally
  
    //finally里面的代码是一定会执行的，虽然它在Exit之后，这个和上面测试Break和Continue一样
    //所以这个弹出框是一定会出现的
    ShowMessage('finally');
  end;
end;

end.
```
