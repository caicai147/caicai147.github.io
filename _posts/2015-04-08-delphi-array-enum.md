---
layout: post
title: Delphi基本数据类型---枚举、子界、集合、数组
categories: delphi之基础语法 delphi之容器类 delphi之指针与内存
tags: delphi
---


参考：[http://blog.csdn.net/qustdong/article/details/9230743](http://blog.csdn.net/qustdong/article/details/9230743)

参考：[http://www.cnblogs.com/xumenger/p/4402227.html](http://www.cnblogs.com/xumenger/p/4402227.html)
 

## 1.根据枚举定义集合

    type
        TMyColor = (mcBlue, mcRed);        //定义了一个枚举类型TMyColor
        TMyColorSet = set of TMyColor;        //定义了一个基于枚举类型的集合TMyColorSet

## 2.根据枚举定义数组

    type  
        TMyEnum = (red, green, blue);    //定义枚举
    procedure TForm1.ButtonClick(Sender : TObject);
    const
        ColorArr : array[TMyEnum] of string = ('红', '绿', '蓝');    //定义数据
    var
        myEnum : TMyEnum;    //定义枚举类型
    begin
        for myEnum := Low(ColorArr) to High(ColorArr) do
        begin
            ShowMessage(ColorArr[myEnum]);
        end;
    end.

注意其中根据枚举定义相关的数组的语法

注意其中使用枚举进行循环控制的方法

## 3.枚举类型的相关知识

参考[http://www.cnblogs.com/xumenger/p/4402227.html](http://www.cnblogs.com/xumenger/p/4402227.html)

## 4.子界类型

如果我们定义一个变量为Integer型，那么它的取值范围一般为-32768~32767.而事实上，每个程序所用到的变量的值都有一个确定的范围。例如，人的年龄一般为1~120岁，一年中的月数为1~12，一个月中的天数为1~31天等等

如果能在程序中对所用的变量的值域做具体规定，就便于检查出那些不合法的数据，这样就能更好的保证程序运行的正确性且在一定程度上节省了内存空间

子界类型就能很好的解决上面的问题。此外，在数组的定义中，常用到子界类型，以规定数组下标的范围

### 1)定义格式：

type 子界类型标识符 = 常量1..常量2　　//其中常量1称为子界的下界，常量2称为子界的上界

上界和下界必须是同一顺序类型（该类型称为子界类型的基类型），且上界的序号必须大于下界序号，例如

    type
        age = 1..100;
        letter = 'a'..'z';

可以直接在变量说明中定义子界类型，如

    type 
        letter = 'a'..'z';
    var
        ch1, ch2 : letter;

可以合并成

    var
        ch1, ch2 : 'a'...'z';

### 2)子界类型数据的运算规则

凡是可以使用基类型的运算规则同样适用于该类型的子界类型

例如，可以使用整型变量的地方，也可以使用以整型为基类型的子界类型数据

对基类型的运算规则同样适用于该类型的子界类型

例如，div，mod要求参数运算的数据为整型，因而也可以为整型的任何子界类型数据

基类型相同的不同子界类型可以进行混个运算，比如有以下说明

    var
        x : 1..100;
        y : 1..500;
        z : 1..1000;
        a : integer;

则下面的语句是合法的

    a:=Sqr(x) + y + z;
    z := x + y;

下面的语句：

    y := x + z + a;

当x+z+a的值在1~500范围内也就是（y所属的子界类型的范围）就是合法的，否则就会出错

### 3)子界类型的应用举例

例1，使用子界类型情况语句，当输入月、日、年（10 30 1986），输出30 oct 1986

    var
        month : 1..12;
        day : 1..31;
        year : 1900..2003;
    begin
        write('Enter date(mm dd yyyy):');
        readln(month, day, year);
        write(day);
        case month of
            1:write('Jan' :5);
            2:write('Feb' :5);
            3:write('Mar':5);  
            4:write('Apr':5);  
            5:write('May':5);  
            6:write('Jun':5);  
            7:write('Jul':5);  
            8:write('Aug':5);  
            9:write('Sep':5);  
            10:write('Oct':5);  
            11:write('Nov':5);  
            12:write('Dec':5);  
        end; 
        writeln(year:7);
    end.

例2，将一个四位的十六进制数转换成十进制

    var
        ch : char;
        n : 1..4;
        d1, d2, d3, d4, t : 0..15;
        s : real;
    begin
        write('The hex number is');
        for n:=1 to 4 do
        begin    {将四位的十六进制数分四次作为字符读入}
            read(ch);
            write(ch);
            {分别转换为十进制的数d1, d2, d3, d4}
            if (ch >= '0') and (ch <= '9') then
                t := ord(ch) - 48;
            if (ch >= 'a') and (ch <= 'z') then
                t:=ord(ch)-87;
            if (ch >= 'A') and (ch <= 'Z') then
                t:=ord(ch)-55;
            case n of
                1 : d1 := t;
                2 : d2 := t;
                3 : d3 := t;
                4 : d4 := t;
            end;
        end;
        s := d1*16*16*16 + d2*16*16 + d3*16 + d4;
        writeln('dec:', s);
    end.