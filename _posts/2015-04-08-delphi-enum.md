---
layout: post
title: Delphi的枚举类型
categories: delphi之基础语法 delphi之容器类
tags: delphi 枚举
---


参考：[http://blog.csdn.net/kissdeath/article/details/2060573](http://blog.csdn.net/kissdeath/article/details/2060573)

 
Delphi程序不仅可以用于数值处理，还更广泛的用于处理非数值的数据。例如：性别、月份、星期几、颜色、单位名、学历、职业等。

## 1.枚举类型的定义

格式：type 枚举类型标识符 = (标识符1, 标识符2, ..., 标识符n)

## 2.枚举类型数据特点

### 1)枚举元素只能是标识符

例如，下面的定义是合法的

    type 
        days = (sun, mon, tue, wed, thu, fri, sat);
        colors= (red, yellow, blue, white, black, green);

而下面的类型定义是错误的

    type
        colortype= ('red', 'yellow', 'blue', 'white');
        numbers= (1,3,4,5,6);

定义枚举类型的时候列出所有枚举元素构成了这种枚举类型的值域（取值范围）

### 2)枚举类型属于顺序类型

根据定义类型时各枚举类型的排列顺序确定他们的序号，且序号从0开始

例如，定义

    type 
        days=(sun,mon,tue,wed,thu,fri,sat);

则，ord(sun)=0, ord(mon)=1,...，以此类推

枚举类型中的第一个元素无前驱，最有一个元素无后继

### 3)同一个枚举元素不能出现在两个或两个以上的枚举类型定义中。如下的定义是错误的

    type
        color1 = (red, yellow, white);
        color2 = (blue, red, black);

因为red属于枚举类型color1和枚举类型color2

### 4)枚举类型变量只能进行赋值运算和关系运算，不能进行算术运算和逻辑运算

在枚举元素比较的时候，实际上是对其序号的比较

例如定义如下

    type
        days = (sun, mon, tue, wed, thu, fri, sat);
        colors = (red, yellow, blue, white, black, green);
    var
        color : colors;
        weekday : days;

则下面的语句是合法的

    weekday := mon;
    if weekday=sun then
        write('reset');

而下面的语句是不合法的

    mon := 1;    //错把枚举值当成变量名
    weekday := blue;     //枚举值blue不属于枚举变量weekday的值域
    read(color);    //枚举类型变量不能用读语句进行赋值
    
    write(weekday);
    writeln(blue);        //不能通过写语句输出枚举类型的变量值和枚举值

### 5)可以把变量的说明与类型的定义合并在一起，比如

    var
        hoilday, workday : (sun, mon, tue, wed, thu, fri, sat);
        color : (red, yellow, blue, white, black, green);

对枚举数据的输入与输出可通过间接方式进行。输入时，一般可输入一个代码，通过程序进行转换，输出时，也只是打印出与枚举元素相对应的字符串。这在后面会有实例展示


## 3.枚举类型的应用

例，输入今天是星期几的序号，输出明天是星期几的英文单词（星期天序号为0）

    type
        weekday = (sun, mon, tue, wed, thu, fri, sat);
    var
        i : integer;
        today, tomorrow : weekday;
    begin
        writeln('What date is it');
        readln(i);
        case i of        {根据输入转换成枚举类型}
            0 : today:=sun;
            1 : today:=mon;
            2 : today:=tue;
            3 : today:=wed;
            4 : today:=thu;
            5 : today:=fri;
            6 : today:=sat;
        endl
        
        if today=sat then
            tomorrow := sun
        else
            tomorrow := succ(today);    //succ()是求这个枚举元素的后继
    
        write('The tomorrow is ');
        case tomorrow of
            sun : writeln('sunday');
            mon : writeln('monday');
            tue : writeln('tuesday');
            wed : writeln('wednesday');
            thu : writeln('thursday');
            fri : writeln('friday');
            sat : writeln('saturday');
        end;
    end.

枚举类型是一种有序类型，所以枚举类型的变量可以作为循环变量。

学习枚举类型的时候，注意枚举元素和变量的区别，以及枚举与变量的输入输出方法的处理
