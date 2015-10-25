---
layout: post
title: Delphi中record和packed record的区别
categories: delphi之指针与内存
tags: delphi 指针 内存
---


转载：[http://blog.csdn.net/rznice/article/details/6566978](http://blog.csdn.net/rznice/article/details/6566978)

第一种不带packed关键字的结构体表明编译器编译时要求进行字对齐。

而第二种带packed关键字的结构体表明编译器编译结构体的时候不需要进行字对齐，这种方式对结构中的字段访问回避第一种方式慢！但是更节约空间。

所以归纳来说，有packed的占用内存小，但是速度慢一点。没有packed的占用内存大，但是速度快一点

比如

    TA = record
        a : char;
        b : integer;
    end;

和

    TB = packed record
        a : char;
        b : integer;
    end;

中，TA.b位于TA结构开始出的第五个字节，TB.b位于TB结构开始处的第二个字节，即TA结构中在TA.a和TA.b之间插入了3个无用字节，因为TA.b在内存中的地址是按字对齐的---即这个地址能被4整除，而TB中则没有这些无用字节，但是TB.b不是字对齐的，对他的访问比对TA.b慢。

例子1
---

    type
        t = packed record
            a : char;    //1个字节
            d : double;    //8个字节
            b : smallint; //4个字节，虽然本来smallint是2个字节，但是因为如果一个变量没有4个字节宽的话也要占4个字节，所以现在要补宽到4个字节
    end;
    
    //所以sizeof(t) = 13

和

    type
        t = record
            a : char;    //8字节，因为a的长度不够8个字节，而紧邻着的d 为一个字（8个字节），所以要补齐8
            d : double;    //8字节
            b : smallint;    //8字节
    end;
    
    //sizeof(t)=24

例子2
---

    type
        t = record
            a : char;    //1个字节
            b : smallint; //2个字节
            d : double;    //8个字节
    end;
    
    //sizeof(t)=13

和

    type
        t = record
            a : char;    //因为a的长度为1个字节，后面的b为两个字节，累加起来是3个字节
                            //再后面的d为8个字节，所以a+b要补齐到8个字节
            b : smallint;    //a+b共占8个自己
            d : double;    //8个字节
    end;
    
    //sizeof(t) = 16;

在windows中内存的分配一次是4个字节。而packed按字节进行内存的申请和分配，这样速度要慢一点，因为需要额外的事件来进行指针的定位。

因此如果不用packed的话，Delphi将按一次4个字节的方式申请内存，因此如果一个变量没有4个字节宽的话也要占4个字节