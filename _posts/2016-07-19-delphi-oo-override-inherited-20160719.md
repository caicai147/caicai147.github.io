---
layout: post
title: override和inherited关键字在Delphi面向对象多态机制中的作用
categories: delphi之面向对象
tags: delphi 面向对象 override inherited
---

##正确的语法

先将Delphi面向对象编程时候正确使用继承、多态的编程实例代码展示出来，或者点击[【这里】](../download/20160719/1.zip)下载demo程序

```
unit MainFrm;

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

  TTestClass1 = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTestClass2 = class(TTestClass1)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTestClass3 = class(TTestClass2)
  public
    constructor Create;
    destructor Destroy; override;
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  testClass3: TTestClass3;
begin
  testClass3 := TTestClass3.Create;

  ShowMessage('创建完成！开始释放');

  testClass3.Free;
  testClass3 := nil;
end;

constructor TTestClass1.Create;
begin
  inherited;
  ShowMessage('TTestClass1 Create');
end;

destructor TTestClass1.Destroy;
begin
  ShowMessage('TTestClass1 Destroy');
  inherited;
end;

constructor TTestClass2.Create;
begin
  inherited;
  ShowMessage('TTestClass2 Create');
end;

destructor TTestClass2.Destroy;
begin
  ShowMessage('TTestClass2 Destroy');
  inherited;
end;

constructor TTestClass3.Create;
begin
  inherited;
  ShowMessage('TTestClass3 Create');
end;

destructor TTestClass3.Destroy;
begin
  ShowMessage('TTestClass3 Destroy');
  inherited;
end;

end.
```

该程序运行之后弹出框的顺序如下

```
TTestClass1 Create
TTestClass2 Create
TTestClass3 Create
创建完成！开始释放
TTestClass3 Destroy
TTestClass2 Destroy
TTestClass1 Destroy
```

##错误的用法情况一

其他地方的编码规范和正确的示例一致，只有下面展示的这个地方有变化：TTestClass继承TTestClass2的时候Destroy方法没有使用override关键字。可以点击[【这里】](../download/20160719/2.zip)下载demo程序

```
  TTestClass3 = class(TTestClass2)
  public
    constructor Create;
    destructor Destroy;  
  end;
```

执行的时候弹出框的顺序如下，可以看到TTestClass3自己的Destroy没有执行，但其所继承的父类TTestClass2、TTestClass1，因为Destroy正确的使用了override关键字，所以对应的Destroy方法都被调用到了

```
TTestClass1 Create
TTestClass2 Create
TTestClass3 Create
创建完成！开始释放
TTestClass2 Destroy
TTestClass1 Destroy
```

经过测试，哪个类的Destroy方法没有使用override，哪个类的Destroy就不会被调用

所以可能导致内存泄露

之前开发中有犯过这样的错误，封装的一个打包类，在Destroy中没有使用override，因为创建和释放的次数多所以导致大量的内存泄露

##错误的用法情况二

其他地方的编码规范和正确的示例一致，只有下面展示的这个地方有变化：三个类的Destroy方法都没有使用inherited关键字。可以点击[【这里】](../download/20160719/3.zip)下载demo程序

```
destructor TTestClass1.Destroy;
begin
  ShowMessage('TTestClass1 Destroy'); 
end;

destructor TTestClass2.Destroy;
begin
  ShowMessage('TTestClass2 Destroy'); 
end;

destructor TTestClass3.Destroy;
begin
  ShowMessage('TTestClass3 Destroy'); 
end;
```

执行之后弹出框的情况如下，调用了所有父类的Create，但是只调用了自己的Destroy

```
TTestClass1 Create
TTestClass2 Create
TTestClass3 Create
创建完成！开始释放
TTestClass3 Destroy
```

这样也可能会造成内存泄露

##错误的用法情况三

其他地方的编码规范和正确的示例一致，只有下面展示的这个地方有变化：三个类的Create方法都没有使用inherited关键字。可以点击[【这里】](../download/20160719/4.zip)下载demo程序

```
constructor TTestClass1.Create;
begin
  ShowMessage('TTestClass1 Create');
end;

constructor TTestClass2.Create;
begin
  ShowMessage('TTestClass2 Create');
end;

constructor TTestClass3.Create;
begin
  ShowMessage('TTestClass3 Create');
end;
```

执行之后弹出框的情况如下，只调用了自己的Create，调用了所有父类的Destroy

```
TTestClass3 Create
创建完成！开始释放
TTestClass3 Destroy
TTestClass2 Destroy
TTestClass1 Destroy
```

本例中因为没有在Create中申请内存，也没有在对应的Destroy中释放内存，所以没有报错

但是如果在TTestClass1、TTestClass2的Create中分别申请了一些内存，在TTestClass1、TTestClass2的Destroy中分别释放了对应的内存

那么明显可以看到，因为这种不规范的写法导致只会调用释放内存的Destroy，而不会在开始的时候调用申请内存的Create，再加上如果在程序运行的过程中还需要使用这个本以为会创建但是实际上没有创建的内存空间，那么必然会导致非法地址访问报错！

针对上面的情况再实现一个测试程序，可以点击[【这里】](../download/20160719/5.zip)下载demo程序

Create没有inherited，内存没有创建，但是又在后续误使用本应该创建可因为没有使用inherited而没有创建的内存而导致非法地址访问错误

```
unit MainFrm;

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

  PTestRecord = ^TTestRecord;
  TTestRecord = record
    name: string;
    age: Integer;
  end;

  TTestClass1 = class
  public
    pt: PTestRecord;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTestClass2 = class(TTestClass1)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTestClass3 = class(TTestClass2)
  public
    constructor Create;
    destructor Destroy; override;
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  testClass3: TTestClass3;
begin
  testClass3 := TTestClass3.Create;

  testClass3.pt.name := 'xumenger';

  ShowMessage('创建完成！开始释放');

  testClass3.Free;
  testClass3 := nil;
end;

constructor TTestClass1.Create;
begin
  New(pt);
  ShowMessage('TTestClass1 Create');
end;

destructor TTestClass1.Destroy;
begin
  Dispose(pt);
  ShowMessage('TTestClass1 Destroy');
  inherited;
end;

constructor TTestClass2.Create;
begin
  ShowMessage('TTestClass2 Create');
end;

destructor TTestClass2.Destroy;
begin
  ShowMessage('TTestClass2 Destroy');
  inherited;
end;

constructor TTestClass3.Create;
begin 
  ShowMessage('TTestClass3 Create');
end;

destructor TTestClass3.Destroy;
begin
  ShowMessage('TTestClass3 Destroy');
  inherited;
end;

end.
```

执行后会报非法地址访问错误

```
TTestClass3 Create

Access violation at address 004042B0 in module 'override.exe'
Write of address 00000000
```
