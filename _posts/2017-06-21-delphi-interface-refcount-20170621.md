---
layout: post
title: Delphi引用模型：接口的引用计数特性
categories: delphi之面向对象 python之面向对象 c/c++之面向对象
tags: delphi interface 接口 引用计数 TInterfacedObject COM 对象内存模型 引用模型 Python C++
---

在[《Delphi通过接口和C++进行对接》](http://www.xumenger.com/cpp-delphi-interface-20170620/)提到了Delphi、C++的接口和引用计数等知识点，因为之前的系列文章[C++对象内存模型 ](http://www.xumenger.com/tags/#C++对象内存模型)已经对C++、Delphi类的内存模型进行探究了，对本文续接上一篇文章重点针对引用计数进行深入的研究和讲解！

Delphi的string、C++的是std::string都是引用计数应用的典型，本文会讲解引用计数

>本文的测试程序[代码](../download/20170621/testInterface.zip)

## 定义测试用的接口和类

```
unit TInfo;

interface
uses
  Windows, Dialogs, SysUtils, StrUtils;

type
  PPerson = ^RPerson;
  RPerson = record
    sName: string;
    iAge: Integer;
  end;

  IInfo = interface(IInterface)
  //Ctrl+Shift+G为定义的接口生成GUID
  ['{11BA9015-8463-4D7A-90BD-D66FF83E853C}']
    //接口定义以下接口，对应的类必须实现
    function GetName: string;
    function GetAge: Integer;
    function GetPerson(): PPerson;
    function GetRefCount(): Integer;
  end;

  TInfoDefault = class(TInterfacedObject, IInfo)
  public
    FPerson: PPerson;
  public
    constructor Create(sName: string = ''; iAge: Integer = 0);
    destructor Destroy; override;

    function GetName: string;
    function GetAge: Integer;
    function GetPerson(): PPerson;
    function GetRefCount(): Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

function GetInfoDefault(): IInfo;
function GetDefaultPerson(): PPerson;
function GetInfoDefaultV2(): IInfo;

implementation
var
  GInfoDefault: IInfo = nil;

function GetInfoDefault(): IInfo;
begin
  if (nil = GInfoDefault) then
  begin
    GInfoDefault := TInfoDefault.Create('Default单例', 100);
  end;
  Result := GInfoDefault;
end;

function GetDefaultPerson(): PPerson;
begin
  Result := GetInfoDefault().GetPerson();
end;

function GetInfoDefaultV2(): IInfo;
var
  aInfo: IInfo;
begin
  aInfo := TInfoDefault.Create('Default单例V2', 100);
  Result := aInfo;
end;

constructor TInfoDefault.Create(sName: string = ''; iAge: Integer = 0);
begin
  inherited Create;
  New(FPerson);
  if ('' <> sName) and (0 <> iAge) then
  begin
    FPerson.sName := sName;
    FPerson.iAge := iAge;
  end
  else
  begin
    FPerson.sName := 'Error';
    FPerson.iAge := 100;
  end;
end;

destructor TInfoDefault.Destroy();
begin
  Dispose(FPerson);
end;

function TInfoDefault.GetName: string;
begin
  Result := FPerson.sName;
end;

function TInfoDefault.GetAge: Integer;
begin
  Result := FPerson.iAge;
end;

function TInfoDefault.GetPerson(): PPerson;
begin
  Result := FPerson;
end;

function TInfoDefault.GetRefCount(): Integer;
begin
  Result := FRefCount;
end;

function TInfoDefault._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TInfoDefault._Release: Integer;
begin
  Result := inherited _Release;
end;

end.
```

## 什么情况下会改变引用计数？

```
procedure TForm1.btnChangeRefCountClick(Sender: TObject);
var
  Info1, Info2: IInfo;
  CInfo1, CInfo2: TInfoDefault;
begin
  //接口指针，通过实现类创建对象
  Info1 := TInfoDefault.Create('接口的指针', 10);
  mmoMessage.Lines.Add(Info1.GetName + '的引用计数值 = ' + IntToStr(Info1.GetRefCount));
  Info2 := Info1;
  mmoMessage.Lines.Add(Info1.GetName + '的引用计数值 = ' + IntToStr(Info1.GetRefCount));
  Info2 := nil;
  mmoMessage.Lines.Add(Info1.GetName + '的引用计数值 = ' + IntToStr(Info1.GetRefCount));

  mmoMessage.Lines.Add('');
  //实现类指针，通过实现类创建对象
  CInfo1 := TInfoDefault.Create('实现类的指针', 10);
  mmoMessage.Lines.Add(CInfo1.GetName + '的引用计数值 = ' + IntToStr(CInfo1.GetRefCount));
  CInfo2 := CInfo1;
  mmoMessage.Lines.Add(Info1.GetName + '的引用计数值 = ' + IntToStr(CInfo1.GetRefCount));
  CInfo2 := nil;
  mmoMessage.Lines.Add(Info1.GetName + '的引用计数值 = ' + IntToStr(CInfo1.GetRefCount));
  mmoMessage.Lines.Add('');
end;
```

点击按钮，输出信息如下

![image](../media/image/2017-06-21/01.png)

* Create的时候，引用计数+1
* 给一个指针赋值的时候，引用计数+1
* 将一个指针置为nil的时候，引用计数-1

>要想有引用计数的特性，必须通过接口指针来创建对象，而不能直接使用对应实现类的指针！就像上面的例程所展示的那样！

COM就是一个规范，而在实现和使用AddRef和Release时也需要遵守以下三条规则：

* 在返回之前调用AddRef。对于那些返回接口指针的函数，在返回之前应该用对应的指针调用AddRef。这些函数包括QueryInterface及各式各样的CreateInstance。这样当客户从这种函数获得一个接口后，就不需要再调用AddRef
* 使用完接口之后一定要记得调用Release。在使用完某个接口之后应调用此接口的Release函数。如果忘记了，就可能出现内存泄露
* 在赋值之后调用AddRef。在将一个接口指针赋给另外一个接口指针时，应调用AddRef。也就是说，在建立接口的另外一个引用之后应增加相应组件的引用计数

## TInterfacedObject缺省的引用计数方法

上面展示了Delphi引用计数的规律和特点，对应看一下TInterfacedObject引用计数的缺省实现

```
interface

type
  TInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

implementation

function TInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

...
```

\_AddRef就是最简单的增加引用计数；但\_Release在减少引用计数的基础上，还会去判断，如果引用计数为0，则直接调用Destroy进行析构！其中InterlockedIncrement、InterlockedDecrement这两个函数可以保证在同一时刻只会有同一个线程来访问成员变量

为什么要有引用计数机制呢？生命周期就是一个变量或对象存活的时间，而现在我们需要对这个时间进行控制。在C++编程中，当我们使用new开辟一块内存，并定义了一个对象之后，我们就可以使用该对象了，当使用完成时，我们就需要使用delete释放对应的内存。new和delete就被用来控制对象的生命周期

这听起来很简单，没有什么难点，但在COM中，我们面对的是一个个接口，当我们使用完一个接口之后，就释放这个接口对应的组件，那么就会出现这个组件对应的其他接口也无法被使用，这不是我们希望看到的。由于我们不知道哪些接口是由同一个组件实现的，所以，我们不能在使用完一个接口之后就释放对应的组件。正是这种很难知道两个接口指针是否指向同一组件，因此决定何时可以安全地释放一个组件将是极为复杂的。得知两个接口指针是否指向同一对象的唯一方法是查询这两个接口的IUnKnown，然后对结果进行比较。当程序越来越复杂时，决定何时可以释放一个组件将是非常复杂的，甚至有时候是不可行的

对于上面的问题，就有了如下的方法：我们可以通知组件何时需要使用它的某个接口以及何时用完此接口，而不是直接将接口删除。一般我总能精确地知道何时开始使用一个接口，并且可以精确地知道何时将用完此接口；就像前面说的，决定何时将用完整个组件并不是一件容易的事情。所以，当用完某个接口后给组件发个消息，告诉组件，这个接口已经使用完成了。当所有的接口使用都完成以后，对组件的释放动作就可以由组件自己来完成

在COM中，就是使用AddRef和Release实现组件自己来管理其生命周期；而使用AddRef和Release来完成的是一套叫做引用计数的机制

当然还可以修改成新的引用计数的管理方法：

```
...

//增加引用计数还是使用缺省的方法
function TInfoChange._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

//减少引用计数修改为新的方法：减少引用计数但不做释放的处理
function TInfoChange._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
end;

...
```

## 顺便看看Python的引用计数

引用计数的应用还是很广泛的，其实在Python里面也是使用了引用计数的。比如下面这个例子可以看到a、b是指向同一个对象的两个引用

```
>>> a = 1
>>> print(id(a))
31294696
>>> b = a
>>> print(id(a))
31294696
>>> 
```

再看Python类对象的引用计数特点！`b = a`之后，引用计数就变大了，`del a`只是减少这个对象的引用计数，并没有释放其在内存中的资源，等到`del b`的时候，引用计数

```
>>> from sys import getrefcount
>>> class testClass(object):
  def __init__(self):
    print '构造方法'
  def __del__(self):
    print '析构方法'

    
>>> a = testClass()
构造方法
>>> print getrefcount(a)
2
>>> print getrefcount(a)
2
>>> b = a
>>> print getrefcount(a)
3
>>> del a
>>> del b
析构方法
>>> print getrefcount(a)

Traceback (most recent call last):
  File "<pyshell#23>", line 1, in <module>
    print getrefcount(a)
NameError: name 'a' is not defined
>>> 
```

>这里可能有点不符合我们直观上认知：第一次创建了对象后，使用getrefcount获取其引用计数是2而不是1！

## 参考资料

* [《Delphi使用FillChar时候如果有string可能导致内存泄漏》](http://www.xumenger.com/delphi-string-memory-20151118/)
* [《标准C++类std::string的内存共享和Copy-On-Write技术》](http://www.xumenger.com/cpp-string-copy-on-time-20151119/)
* [《C++浅拷贝、深拷贝及引用计数浅析》](http://blog.csdn.net/ieearth/article/details/49332709)
* [《浅谈引用计数》](http://www.xuebuyuan.com/1150521.html)
* [《COM编程——引用计数（1）》](http://www.jellythink.com/archives/232)
* [《Delphi通过接口和C++进行对接》](http://www.xumenger.com/cpp-delphi-interface-20170620/)
* [《Python深入06 Python的内存管理》](http://www.cnblogs.com/vamei/p/3232088.html)

