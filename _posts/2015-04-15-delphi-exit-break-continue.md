---
layout: post
title: Delphi中exit、break、continue等跳出操作的区别
categories: delphi之函数
tags: delphi 函数
---


Delphi中表示跳出的有break，continue，abort，exit，halt，runerror等

##1.break##

强制退出最近的一层循环（注意：只能放在循环里；而且是只能跳出最近的一层循环），用于从for、while、repeat语句中强制退出

类似于C/C++等语言中的break的功能

 
##2.continue##

用于从for、while、repeat语句中结束循环内的本次处理,继续从循环体的开始位置继续执行

类似于C/C++等语言中的continue的功能


##3.exit##

用于从当前代码块中退出。

若该代码是主程序，则终止该程序。

如果是函数或过程，则立即终止该函数或过程
 

##4.abort##

终止程序需的运行，产生不报错的异常信息。跳出祖先模块。和exit的区别是

    procedure p1;
    begin
        p2;
        p3;
    end;
    
    procedure p2;
    begin
        abort;    //或者exit;
    end;
    
    procedure p3;
    begin
        //进行一些操作
    end;

在执行p1的时候，如果p2里面用abort，则执行不到p3

如果使用exit，就能够执行到p3，因为exit只能控制终止它所在的函数或过程，不能终止调用它所在函数的那个代码块。如果用exit终止它所在的那个函数之后，会跳回调用这个函数的代码块，继续执行之后的代码


##5.halt##

用于强行终止应用程序的执行，返回操作系统（非正常退出方式）
 

##6.runerror##

终止程序的执行，并产生运行错误（返回错误代码）

