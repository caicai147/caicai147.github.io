---
layout: post
title: Delphi和C的类比：指针、字符串、函数指针、内存分配等 
categories: delphi之指针与内存 c/c++之指针与内存 delphi之字符串 c/c++之字符串 深入钻研之内存管理
tags: c c++ delphi 内存 指针 函数 字符串
---


在学习Delphi的时候，一个很好的建议是和C/C++去类比着学习，从指针，到内存管理，到数组，到面向对象……各个方面，都是有很多可以相似和或者也有不同的方，类比着学习，一方面加深对Delphi的理解，一方面加深对C/C++的理解，一方面加深对计算机系统的理解，一方面加深对面向对象的理解……由1向多可以很方便的扩展，而且完全不冲突，完全是互相促进的过程。所以学习要有技巧！

大家都认为，C语言之所以强大，以及其自由性，很大部分体现在其灵活的指针运算上。因此，说指针是C语言的灵魂，一点都不为过。同时，这种说法也让很多人产生误解，似乎只有C语言的指针才能算指针。Basic不支持指针，在此不论。其实Pascal语言本身也是支持指针的。从最初的Pascal发展至现在的Object Pascal，可以说在指针运算上，丝毫不会逊色于C语言的指针

类型指针的定义
--------

对于指向特定类型的指针，在C中是这样定义的

    int * iPtr;
    char *cPtr;

与之等价的Object Pascal是如何定义的呢？

    var
        iPtr : ^Integer;
        cPtr : ^char;　　//注意 ^ 符号是放在类型的前面

其实也就是符号的差别而已

无类型指针的定义
--------

C中有 void * 类型，也就是可以指向任何类型数据的指针。Object Pascal为其定义了一个专门的类型：Pointer。于是Delphi的

    ptr : Pointer;

等价于C的

    void *ptr;

指针的解除引用
--------

要解除指针引用（即取出指针所指向区域的值），C的语法是

    *ptr

而Object Pascal的语法是

    ptr^　　//注意 ^ 符号是放在指针变量的后面

取地址（指针赋值）
---------

取某对象的地址并且将其赋值给指针变量，C的语法是

    int i = 4;
    int *iPtr = &i;

而Object Pascal的语法是

    i : Integer;
    iPtr : ^Integer;
    iPtr := @i;

也只是符号的差别而已

指针运算
----

在C中，可以对指针进行移动的运算，如

    char a[20];
    char *ptr = a;
    ptr++;
    ptr +=2;

当执行ptr++时候，编译器会产生让ptr前进sizeof(char)步长的代码，之后，ptr将指向a[1]。ptr+=2;这句使得ptr前进两个sizeof(char)大小的步长。同样，我们来看一下Object Pascal中如何实现

    var
        a : array[1..20] of Char;　　//注意Delphi里面定义数组变量的这一种方法
        ptr : ^char;  
    begin
        ptr := @a;　　//此处不同于C，C中数组名就是指针，
                     //而在Delphi中还需要使用取数组名地址的语法：@a
        Inc(ptr);    //这句等价于C的ptr++
        Inc(ptr, 2);    //这句等价于C的ptr+=2;
    end;

动态内存分配
------

在C中，使用malloc()库函数分配内存，free()函数释放内存。如这样的代码：

    int *ptr, *ptr2;
    int i;
    ptr = (int *)malloc(sizeof(int) * 20);
    ptr2 = ptr;　　//因为后来可能要对ptr指针进行操作，为了保存分配的内存空间的地址，
                  //所以要用新的指针 ptr2来记录分配的内存地址，保证后面可以在使用完
                  //内存后进行free，而不会造成内存泄露
    for(i=0; i<20; i++){
        *ptr = 1;
        ptr++;
    }
    free(ptr2);
    ptr2 = NULL;

在Object pascal中，动态内存分配的函数是GetMem()，与之对应的释放函数是FreeMem()，（传统的Pascal中获取内存的函数是New()和Dispose()，但是New()只能获取对象的单个实体的内存大小，无法取得连续的存放多个对象的内存块）。因此，与上面那段的C代码等价的Object Pascal的代码是：

    var
        ptr, ptr2 : ^Integer;
        i : Integer;
    begin
        GetMem(ptr, sizeof(Integer)*20);
        //这句话等价于C的ptr = (int *)malloc(sizeof(int) * 20);
        ptr2 := ptr;    //保留原始指针的位置
        for i:=0 to 19 do
        begin
            ptr^ :=i;    //注意指针的解除引用的语法格式
            Inc(ptr);
        end;
        FreeMem(ptr2);
        ptr2:= nil;
    end.

对于上面的例子（无论是C的还是Object Pascal的），都需要注意一个问题，就是分配内存的单位是字节（BYTE），因此在使用GetMem时候，其第二个参数如果想写成20，那么就会出问题（内存访问越界）。因为GetMem(ptr, 20);实际上只分配了20个字节的内存空间，而一个整型的大小是4字节，那么访问第5个之后的所有元素都是非法的了（对于malloc()参数也是一样）

字符数组的运算
-------

C语言中，是没有字符串类型的，因此，字符串都是用字符数组实现的，于是也就有了一套str 开头的库函数用于进行字符数组的运算，如以下的代码

    char str[15];
    char *ptr;
    strcpy(str, "teststr");
    strcat(str, "_testok");
    ptr = (char *)malloc(sizeof(char) * 15);
    strcpy(ptr, str);
    printf(ptr);
    free(ptr);
    ptr = NULL;

而在Object Pascal中，有了String类型，因此可以很方便的对字符串进行各种运算。但是，有时候我们的Pascal代码需要与C进行交互（比如：用Object Pascal的代码调用C写的DLL或者用Object Pascal写的DLL准备允许用C写客户端的代码）的话，就不能使用string类型了，而必须使用两种语言通用的字符数组。其实，Object Pascal提供了完全相似C的一整套字符数组的运算函数，以上的那段代码的Object Pascal的版本是

    var
        str : array[1..15] of char;
        ptr : ^char;
    begin
        StrCopy(@str, 'teststr');    //在C中，数组的名称可以直接作为数组的首地址指针用，
                                    //但是在Delphi里面要注意，要在str前面加上取地址运算符
        StrCat(@str, '_testok';
        GetMem(ptr, sizeof(char) * 15);
        StrCopy(ptr, @str);
        Write(ptr);    
        FreeMem(ptr);
        ptr:= nil;
    end;

函数指针
----

在动态调用DLL的函数时候，就会用到函数指针。假设用C写一段代码如下：

    typedef int (*PVFN)(int);    //定义函数指针类型
    int main(){
        HMODULE hModule = LoadLibrary("test.dll");
        PVFN pvfn = NULL;
        pvfn = (PVFN)GetProcAddress(hModule, "Function1");
        pvfn(2);    
        FreeLibrary(hModule);
    }

C语言中定义函数指针的类型typedef代码的语法有些晦涩，而同样的代码在Object Pascal中却非常易懂

    type
        PVFN = Function(para : Integer) : Integer;    //这里定义的PVFN是一个类型，而不是一个实例
    var
        fn : PVFN;    //要使用PVFN类型的函数指针，就需要生成一个实例
        //或者可以直接定义 fn : function(para : Integer) : Integer;
        hm : HMOUDLE;
    begin
        hm := LoadLibrary("test.dll");
        fn := GetProcAddress(hm, 'Function1');
        fn(2);
        FreeLibaray(hm);
    end;

附加说明：
--

Delphi的指针功能非常强大，所有C中能够实现的指针Delphi都能实现，上面认为Delphi指针不是强项的只是一种误解（或者对指针的机制一知半解）

由于Pascal语言的限制，用Delphi的指针很多情况下需要强制类型转换，Delphi中提供了很多指针类型，而且非常方便的是你可以自定义自己的指针类型

**一个经验是**：要掌握一种数据类型并且熟练灵活应用，一个比较好的办法是别考虑什么类型是什么名字，而只需要考虑这种类型的变量将占用多少字节。凡是字节数亩相同的数据类型都可以认为是同一种类型，提供不同的类型只是为了编译器能够更方便的查找错误而已，比如：Integer、Pointer、PChar、TSmallPoint甚至是attay[0..3] of char你都可以把他们当成是同一类型加以使用（有了这种思路，可以实现很大的程序灵活性的代码高效性）
