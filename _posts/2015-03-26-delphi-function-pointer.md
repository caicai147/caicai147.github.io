---
layout: post
title: Delphi函数指针
categories: delphi之指针与内存 delphi之函数
tags: delphi 指针 内存 函数
---


参考：

* [http://blog.chinaunix.net/uid-91034-id-2009700.html](http://blog.chinaunix.net/uid-91034-id-2009700.html)
* [http://blog.csdn.net/procedure1984/article/details/3897155](http://blog.csdn.net/procedure1984/article/details/3897155)

 
Delphi中的函数指针实际上就是指针，只是在使用的时候有些不同

函数执行要先定义一个函数类型

    type
        TTestProc = procedure of object;

这是一个最简单的函数指针，没有参数和返回值，并且要求是类的成员函数，这就是后面的 of object的作用，如果没有 of object 那就是一个普通的函数指针。

类的成员函数其实就代表了调用的时候参数的不同，因为类的成员件数隐含着传入一个对象参数，类方法存在一个隐藏参数self,也就是说两者形参不一样，而并不显式说明，函数都是静态的。当然了，如果有重载就变成了虚函数指针表，其中的调用就复杂一点。

所以普通的函数指针是不能和类的函数指针相互转换的。这也就是为什么delphi中可以这样赋值 `button1.onClick:=button2.onClick;`却不能这样赋值 `button1.onclick=buttonclick;` （buttonclick为本地函数,button2.onclick为类方法)的原因!

函数类型可以定义一个函数指针变量

    var
        p : TTestProc;

这个指针变量是4字节的Pointer，可以与Pointer直接做转换，但是要加一个@，比如

    var
        p1 : Pointer;
    begin
        p1 := @p;
        @p := p1;

当p被复制成一个真正的函数之后就可以使用了

    p();

如果有参数则可以直接加上参数，与普通的函数调用方法没有什么区别，如果需要取得函数指针本身的地址就需要：

    @@p;

加一个@其实就是为了防止歧义，因为p本身也可以当成函数来使，所以用@p 来代表指针，不过特殊情况下p 可以代表一个指针，比如

    assigned(p);

这时候没有岐义，所以不需要加上@。

基本上p和@p都代表了函数指针，只有@@p才代表函数指针本身的地址，为了不产生歧义，所以有时候使用@p，有时候使用p。一般在赋值的时候既可以使用p 又可以使用 @p，而在比较的时候必须使用@p，这在编译的时候就已经严格控制了，所以出现编译错误的时候就可以考虑这方面的问题