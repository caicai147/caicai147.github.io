---
layout: post
title: Delphi数组
categories: delphi之基础语法 delphi之指针与内存 delphi之容器类
tags: delphi 数组　指针
---

参考：[http://www.cnblogs.com/huangjacky/archive/2009/12/21/1628833.html](http://www.cnblogs.com/huangjacky/archive/2009/12/21/1628833.html)

数组就是一堆相同特性数据的一个组合，也就是每个元素的类型必须是一样的，当然在其他一些弱语法的语言里面，数组的元素可以是千奇百怪，例子

    var
        A : Array[0..2] of Integer;
    begin
        A[0] := 1;
        a[1] := 1.0;    //这里是错的，因为每个元素都必须是Integer类型
    end;
　　
##Delphi中数组的分类##

###1.定长和不定长###

####定长数组####

定长数组也就是长度在声明的时候就确定的，后面是不能改变的，而在定长数组里，起始序号不必从0开始，可以自己定，例如

    var
        A : Array[2..3] of Integer;
    begin
        A[2] := 1;
        SetLength(A,3);    //这里会出错，因为定长数组不能再分配
    end;

从上面我们可以看到起始序号是2，但是步长是1，是不能改变的。为什么我们看到很多数组的起始序号是0呢？习惯而已。

来看一个特殊用法

    type
        THuangJacky = (hjA, hjB, hjC);
    const
        //用法1
        B : Array[0..2] of string = ('A', 'B', 'C');
        //用法2
        C : Array[THuangJacky] of string('A', 'B', 'C');
    var
        H : THuangJacky;
        S :String;
    begin
        S := B[Ord(H)]);
        S := C[H];
        //B[H]和C[1]都会出错
    end;

从上面的例子可以看出只要是有序数类型都可以当做数组的序号，但是我们用的时候序号就必须是声明的哪种序数类型，所以上面的代码注释中才会写出两种错误的情况

####不定长数组####

不定长数组：动态数组，也就是声明的时候没有说长度是多少，在使用之前必须声明，长度是可以再分配的，序号必须从0开始，例子

    var
        A : Array of Integer;    //定长数组可能的定义是 A :Array[0..10] of Integer
    begin
        SetLength(A, 3);    //数组一共有3个元素
        A[0] := 1;
        A[1] := 2;
        A[2] := 3;
        //A[3]没有，因为只有3个元素
        
        SetLength(A, 4);    //如果变长长度，直接增加后面的元素
        A[3] := 4;    //现在增加了第四个元素，而前三个元素还是那三个
        
        SetLength(A, 3);    //如果长度变短了，超出部分就会被去掉
        //现在A[3]没有了
    end;

有时候，大家这样要先设定长度，在赋值，很麻烦，现在介绍一个一气呵成的招数

    type
        TA = Array of Integer;
    var
        A : TA;
    begin
        A := TA.Create(1, 2, 3);
        //此招请勿在Delphi 7上面使用
    
        //使用上面的方法之后，A[0] :=1, A[1] := 2， A[2]:=3
    end;

　　
###2.一维和多维###

前面的所有例子，所讨论的都是一维数组，要想弄一个矩阵（多维数组）怎么办？

    var
        A : Array[0..2, 0..2] of Integer;
        B : Array[0..2] of Array[0..2] of Integer;
    begin
        A[0, 0] := 1;
        B[0, 0] := 1;
    end;

两种方法都是可以的，下面介绍二维数组中的不定长数组

    var
        B : Array of Array of Integer
    begin
        //设置一个3*3的矩阵
        SetLength(B, 3, 3);
    
        //如果需要实现齿状数组，必须像下面这么做
        SetLength(B, 3);
        SetLength(B[0], 1);
        SetLength(B[1], 2);
        SetLength(B[2], 3);
    end;

 
接下来介绍几个关于数组的常用函数

####1.复制函数####

    var
        A, B : Array[0..1] of Integer;
    begin
        A[0] := 1;
        A[1] := 2;
        B := A;
        B[0] :=2;
        ShowMessageFmt('A0:%D, B0:%D', [A[0], B[0]]);    //A0:1,B0:2
    end;

这个效果就是我们想要的，貌似没有什么好说的，但是如果是动态数组呢？

    var
        A, B : Array of Integer;
    begin
        SetLength(A, 2);    
        SetLength(B, 2);
        A[0] := 1;
        A[1] := 2;
        B := A;
        B[0] := 2;
        ShowMessageFmt('A0:%D, B0:%D', [A[0], B[0]]);    //A0:2, B0:2
        //很显然改了B[0]的值之后，对A[0]的也造成了影响，所以B和A之间存在着某种联系
    end;

现在怎么办？A和B被关联到一个地址了，其实现在我们可以使用Copy函数，就可以解决这个问题了

    var
        A, B : Array of Integer;
    begin
        SetLength(A, 2);
        SetLength(B, 2);
        A[0] :=1;
        A[1] :=2;
        B := Copy(A);    //整个数组都赋值过去
        B := Copy(A, 0, 2);    //选择性复制
        B[0] := 2;
        ShowMessageFmt('A0:%D,B0:%D', [A[0], B[0]]); // A0:1,B0:2
    end;

####2.序号相关####

函数Low()和High()值得信赖，不过我们需要注意的是，他们返回的类型是我们数组的序号的那个类型，并不都是Integer，如前面例子中的THuangJacky

    var
        A : Array of array of string;
        I, J : Integer;
    begin
        SetLength(A, 10);
        for I := Low(A) to High(A) do
        begin
            SetLength(A[I], I);
            for J := Low(A[I]) to High(A[I]) do
                A[I, J] := IntToStr(I) + ',' + IntToStr(J) + ' ';
            end
        end;
    end.

####3.数组长度####

Length()函数返回的是Integer类型

    var
        A : Array of Integer;
    begin
        SetLength(A, 2);
        Length(A);
    end.

>从上面的那个复制的例子我们可以看出来：定长数组变量就是一个变量，所以可以直接用 := 来赋值，而动态数组变量就是一个指针，如果用了 :=来赋值，两个变量就关联在一起了

    var
        A :Array[0..2] of Integer;
        B :Array of Integer;
    begin
        ShowMessageFmt('A:%8x, A[0]:%8p', [Integer(@A), @A[0]]);    //一样，从地址来看这个数组控件在栈上面
        SetLength(B, 3);
        ShowMessageFmt('B:%8p, B[0]:%8p', [B, @B[0]]);    //一样，这个数据空间在堆上面
    end

我们看到A要取地址才和A[0]取地址一样，那么也就是说A就是A[0];

而B直接就和B[0]取地址一样了，也就是说B就是B[0]的地址

数组在内存中的分布：连续分布的，间隔就是每个元素的大小

    var
        A: Array[0..2] of Integer;
        B: Array of Integer;
    begin
        A[1] := 123;
        //从A也就是A[0]的地址上面往下走4个直接就是A[1]
        ShowMessageFmt('A[1]:%D,直接取值:%D',[A[1], PInteger(Integer(@A)+4)^]);
        //一样，都是123
        SetLength(B, 3);
        B[2] := 88;
        //从B往下走8个字节就是B[2]
        ShowMessageFmt('B[2]:%D,直接取值:%D',[B[2],PInteger(Integer(B)+8)^]);
    end;

但是动态数组的结构和字符的结构就很像了

| 偏移 |     -8      |   -4   |  0~Length*元素  |
| --- | ----------- | ------ |  ----------|
| 内容 | 32位引用次数 | 元素个数 |  实际内容   |