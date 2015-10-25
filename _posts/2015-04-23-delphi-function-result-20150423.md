---
layout: post
title: Delphi函数的返回值（注意这里与C/C++等语言有差异）
categories: delphi之函数
tags: delphi 函数
---


在C/C++等语言中，函数执行到 return 部分之后，将立即停止函数的执行，并返回值

但是在Delphi中不同

函数中，执行到result时，并不同于比如 C/C++ 中的 return，跳出函数，而是会继续执行下去，直到函数结束

下列代码：

    function(var A:string):string;
    var S,S1,S2:string;
    begin
      Result := '';    //此处将继续往下执行
    ......
      A:=S1;
    ......
      A:=S2;
    ......
      Result := S;    //跳出判断后，已经到程序结尾，过程结束。
    end;

而且在上面的函数中，最终的返回值是最后一个Result的值