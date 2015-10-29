---
layout: post
title: C++和Delphi在构造和析构上的语法比较
categories: c/c++之面向对象 delphi之面向对象
tags: c++ delphi 面向对象 构造 析构
---


前言
----

在学习Delphi的时候，和C/C++以及Java进行类比，是很好的学习方法，一方面可以同时加深对两种语言的理解，另一方面可以深入理解超越语言本身的知识：编译原理、内存管理等底层知识、设计模式、面向对象……知识，所以学习一定要有策略。 

Delphi永远没办法在栈上创建一个对象
--------------------

下面是一段常见的Delphi代码，在过程的开头声明本过程所需要的全部局部变量

    procedure Foo
    var
        obj : TObject;
    begin
        ...
    end;

C++程序员会以为这个变量就是TObject对象实例本身，会以为这一句是在栈上声明并构造了TObject类的一个对象实例，它们会与下面的C++代码混淆

    void Foo(){
        CObject obj;    //这一句的确是在栈上构造了TObject类的一个对象实例
                        //并且将在离开Foo函数自动析构它
        ...
    }

牢记着一点，**在Delphi中，永远不可能在栈上构造一个对象，也永远不可能对一个对象进行值传递，程序员能看到的是“指向对象实例的指针”，简单地说“一切皆指针”**，上例中的obj其实就是一个“TObject类型的指针”，当它在栈上被声明的时候，它的值不可知（与C++一样），也没有任何对象被构造出来，特别注意Delphi的这个特性，与C++作比较。上面的代码翻译成C++，基本上是

    void Foo(){
        CObject * obj;    //声明一个CObject类型的指针，但没有任何对象被构造或与之关联
        ...
    }

作为一个佐证，在Delphi里，sizeof(TObject)、sizeof(Self)、sizeof(obj)，结果都是4，即一个32位指针的大小。

Delphi的构造函数更像是一个类方法
---------------------------

类方法，在C++中又叫做静态成员函数。

由于Delphi不允许在栈上构造对象，那么对象实例就只能创建在堆上，Delphi没有关键字new（导师有一个名为New 的procedure），而是用一种有别于C++的语法来在堆上构造对象：

    procedure Foo;
    var 
        obj :TObject;    //obj本质上是一个TObject类型的指针
    begin
        obj := TObject.Create;    //在堆上构造一个TObject对象并将其地址赋值给obj
        obj.Free;    //令obj指向的对象析构
    end;

与C++一样，在堆上构造的函数不会在离开作用域的时候被自动解析，所以在离开Foo这个过程之前，要调用TObject的Free方法来析构它。Free方法会调用Destorey析构函数，只不过在调用之前会判断Self是否为空，如果为空就直接返回。Delphi里的Self，就是C++中的this。

Delphi是单根继承，所有类都从TObject派生而来，而所有类的构造函数一定名为Create，而析构函数一定名为Destory，当然，析构函数肯定是虚函数

从声明的形式上看，Create方法是一个成员函数，但是从使用上来看，它更像是一个类方法（C++中叫做静态成员函数），因为调用它的时候，限定名不是对象，而是类名（Txxx.Create）

Delphi的析构函数中可以调用纯虚方法
--------------------

由于在Delphi的析构函数Destory里，可以调用任何纯虚函数（在C++里这一点是不可想象的），所以可以认为这个时候，虚方法表有一定未被破坏，那么，如果基类就可以决定析构时一定要调用的函数，哪怕这个函数是虚函数，甚至是纯虚函数。

Delphi在构造的时候自动将成员变量清零
---------------------

任何一个Delphi中的类，当它被构造之后，它的所有成员变量被清零，布尔类型初始化为False，字符串初始化为空，整型和浮点型初始化为0，等等。而在C++里面就没有这样的保证

Delphi构造函数中抛出异常会自动先调用析构函数
-------------------------

Delphi中，如果构造函数中抛出异常，则会自动先执行析构函数，然后再把异常向外抛出；而在C++里，构造函数中若有异常抛出，则析构函数不会被调用

Delphi简化了COM接口中的AddRef、Release和QueryInterface
---------------------------------------------

C++里一般用模板对COM接口进行封装，而在Delphi里面，AddRef、Release以及QueryInterface都被编译器隐藏掉了，当把一个IUnknown类型的变量（本质上也是一个指针）赋值给另一个变量师，编译器在背后自动AddRef，当一个IUnknown变量离开作用域的时候（再也没有人使用它），Release被自动调用，而QueryInterface被抽象为AS运算符

    procedure Foo(const AParam : IUnknown);
    var
        bar : IUnknown;
        other : IStream;
    begin
        bar := AParam;    //Aparam指向的实例由于赋值操作被AddRef一次
        other := bar as IStream;    //调用了一次QueryInterface，引用计数再次加一
    end;    //返回时，other和bar都离开作用域，分别被调用Release各一次

C++中使用模板（比如_com_ptr）也都可以使引用计数自动化，不过QueryInterface就没有这么方便了。

其他关于构造函数
--------

**1)** 在C++中声明一个对象后，编辑器会主动调用构造函数，Delphi中则需要程序员主动调用，这是Delphi的一个麻烦处，析构也有些类似。通过Free进入Destory只是因为Free函数本身的实现调用了Destory函数

**2)** 以下面代码为例，在Delphi中，这里的test 实际上是指针（一定要记住）；所以必须主动调用构造和析构函数。

        var
            test: TMyClass;
        begin
        ...
        end;

**3)** 一般自己创建的对象要自己去释放，但对于有owner 的控件（调用 Create(Owner) ）可以不用释放，由拥有它的组件释放

**4)** 对象还没有创建时对象指针是nil，例子

        if test<> nil then
            ...

**5)** Delphi里所有的类的对象分配全部使用堆分配的方式，而C++里允许栈分配的方式（MyObject obj;），使用栈分配的方式时（比如函数中的局部变量）在生存期结束时，会自动调用析构函数进行释放相关资源！但是如果是堆分配方式的话（如C++中使用 new 关键字，或者Delphi中使用Create 函数），那么如果需要释放实体就必须显示声明（如free或Destory）

**6)** 为了节约内存，优化运行，我们总是动态创建组件。可当用完之后，如果不及时彻底的将其从内存中清除出去，那么久有违我们的初衷。可怎么“杀死”所创建的组件？比如创艰了一个Edit控件，现在想让他消失，但是用Edit.Free 之后，调用 Edit.Text 却仍然存在……我们知道，光Free是不行的，只是将Edit所指向的内存空间释放了，但是指针并没有设为 nil ，当调用Edit.Text 的时候，Delphi仍然会根据Edit 提供的指针访问已经释放的内存区域，所以会产生 Access Violation（违法访问内存）……错误。所以我们需要Free之后，在将 Edit := nil，或zheFreeAndNil（只在Delphi5下有效）将指针nil 掉，才能保证以后的正常运行。
