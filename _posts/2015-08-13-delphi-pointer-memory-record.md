---
layout: post
title: 从指针和内存角度区别Delphi的record、record类型的指针、类
categories: delphi之指针与内存
tags: delphi 指针 内存 record
---


Delphi的record，有的人可能理解它是没有方法的类，但是这样的理解只是从表面上去理解的，其实两者存在巨大的区别。

下面只是讲解Delphi的class与record，暂时还没有去钻研C++的类和结构体的差异。

给一个代码解释一下

    type
        PPerson = ^Person;   //定义一个record的指针类型
        Person = record     //定义一个record
            name: string;
            age: Integer;
        end;
    
        TPerson = class   //定义一个类
        public
            name: string;
            age: Integer;
        end;

上面定义了一个record 和一个class ，看起来有点像，但是其本质上有很多的区别。

区别一：对象不是没有方法　　　　　　　　　　　　　　　　　
------

就像上面的那个类应该是最简单的类了，看上去没有这个类没有方法，但是这个类默认是继承自TObject的。

虽然没有显式的声明方法，但是是它是有Create、Destroy、Free等大量继承自TObject的方法的。

区别二：类声明的变量其实是一个指针，但是record声明的不是
-----

见下面的声明的变量

    var
        PersonRecord: Person;    //一个record声明的变量
        PPersonRecord: PPerson;  //一个record指针类型声明的变量
        TPersonClass: TPerson;    //一个类变量

第一个是通过一个record类型声明的一个变量，这个变量是一个record类型，但是不是指针。

第二个是通过一个record指针类型声明的变量，这个变量是一个record类型的指针，如果使用New或者赋值一个存在的record的地址，那么将会指向一个record实体。

第三个是通过一个类声明的一个变量，但是这个类变量就相当于一个指针，具体的使用、原理请参见本文前面的部分讲解。

所以下面的关于record的代码

    begin
        PersonRecord.name:= 'xumenger';
        PersonRecord.age:= 22;
        //这样是对的，因为在使用record类型声明一个变量的时候就已经在内存的栈为其分配了内存空间了
    end;

下面是关于record指针类型变量的代码

    begin
        //PPersonRecord.name:= 'xumenger'
        //PPersonRecord.age:= 22;
        //上面的两行是错误的，因为PPerssonRecord是一个record类型的指针，
        //在声明这个变量的时候，只是在内存中分配了存储指针的空间，并没有分配存储record的空间
    
       //所以要下面这样，先为record申请空间
        New(PPersonRecord);
        PPersonRecord.name:= 'xumenger';    //或者PPersonRecord^.name:= 'xumenger';
        PPersonRecord.age:= 22;    //或者PPersonRecord^.age:= 22;
    end;

注意使用New 为Record指针分配了一个记录类型的空间，并使指针指向该内存中的record实体，在使用完成之后，不要忘记了使用Dispose去释放申请的内存空间。

再看类的代码

    begin
        //TPersonClass.name:= 'xumenger';
        //TPersonClass.age:= 22;
        //上面的两行是错误的，因为使用类声明的变量相当于一个指针，所以也只是在内存中分配了存储其指针的空间
    
        //所以要下面这样，先构造一个对象，也就是在内存中分配空间
        //注意要和record的指针类型区别
        TPersonClass:= TPerson.Create;   //虽然说它也是指针但是不能使用New(TPersonClass);
        TPersonClass.name:= 'xumenger';    //不能使用TPersonClass^.name:= 'xumenger';
        TPersonClass.age:= 22;    //不能使用TPersonClass^.age:= 22;

　　注意对于类，构造了类对象的实体之后，一定不要忘记在使用完对象之后去释放对象。