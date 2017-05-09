---
layout: post
title: Delphi的字符串、PChar和字符数组之间的转换
categories: delphi之字符串 delphi之指针与内存
tags: delphi string 指针 内存
---


参考：[http://my.oschina.net/kavensu/blog/193719](http://my.oschina.net/kavensu/blog/193719)

以下的各种方法都是我在Delphi 6的环境下测试成功的，可能根据你的开发环境、不同的上下文语境……有一些可能会出现问题，以自己在使用的时候进行具体的测试为准

Char数组和String的相互转换的另一种方法和注意事项请看[下一篇博客](http://xumenger.github.io/delphi-string-pchar-chararray-20150422-02/)

同时建议结合以下的博客一起理解：

* 《[Delphi字符串的基本操作与常用函数](http://xumenger.github.io/delphi-string-func-20150421/)》
* 《[Delphi中的各种字符串、String、PChar、Char数组](http://xumenger.github.io/delphi-string-pchar-chararray-20150415/)》

 
### 0.前提条件

    var
        s: String;
        p: PChar;
        a: Array[0..20] of char;
　　

### 1.字符串 ---> PChar

    P:= PChar(S);
　　
### 2.PChar ---> 字符串

    s:= p;

或者

    s:= string(p);

上面的两种方式的效果一样，都是将p 指向的内存中的字符串拷贝到s 里面，s 和 p 各自有自己的内存空间，并不是将s 和 p指向同一个内存，所以对s 的修改不会影响p ，对p 的修改也不会影响s 

 
### 3.PChar --->字符数组

    StrCopy(@a, p);

StrCopy的原型如下

    function StrCopy(Dest: PChar; const Source: PChar): PChar;

StrCopy的说明如下（通过查看Delphi的API参考文档获得这些信息）

>Use StrCopy to copy a null-terminated string（使用StrCopy去拷贝一个null结尾的字符串）。

>StrCopy does not perform any length checking. The destination buffer must have room for at least StrLen(Source)+1 characters.（StrCopy不进行任何长度的检查，目标的buffer的长度至少是 StrLen(Source)+1）

>For length checking, use the StrLCopy function.（要想进行长度检查，请使用StrLCopy函数）

 
### 4.字符数组 ---> PChar

    PChar(@a);

### 5.字符串 ---> 字符数组

    StrCopy(@a, PChar(s));

这种方法是通过 PChar作为中转的


### 6.字符数组 ---> 字符串

    s:= PChar(@a);

这种方法是通过 PChar作为中转

或者

    s:= string(a);

这种方式类似于 将PChar转换为string

第二种方法是将数组a 指向的内存中的字符拷贝到s 里面，s 和 a 各自有自己的内存空间，并不是将s 和 a指向同一个内存，所以对s 的修改不会影响a ，对a 的修改也不会影响s 

或者还可以使用下面的方法

    s:= a;

但是第二、三种方法中涉及到的字符数组中是不是有 #0、初始化等问题，还需要继续探究，参考[下一篇博客](http://xumenger.github.io/delphi-string-pchar-chararray-20150422-02/)