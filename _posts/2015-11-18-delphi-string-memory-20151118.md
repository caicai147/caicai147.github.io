---
layout: post
title: Delphi使用FillChar时候如果有string可能导致内存泄漏【转载】
categories: delphi之字符串 delphi之指针与内存 深入学习之内存管理
tags: delphi 字符串 内存 out FillChar
---

* 转载自[Delphi　String可能的内存泄漏](http://www.360doc.com/content/11/0801/09/68419_137087408.shtml )
* 使用FillChar需要注意下面的问题，同样使用ZeroMemory和FillMemory也要注意，因为它们的实现是通过FilChar，关于这三个函数可以参见[这里](http://www.xumenger.com/delphi-zeromemory-20151118/)
* 可以利用`out`关键字的作用来做一个封装，然后可以将这个问题避免（具体见下文）

---

##Delphi是如何管理string的？##

为了提高string的读写性能，Delphi采用了`Copy-on-Write`机制进行内存管理。简单来说，在复制一个string时并不是真的在内存中把原来的string的内容复制一份到另外一个地址，而是把新的string在内存映射表中指向同原string相同的位置，并且把那块内存的`引用计数`加一。这样就省去了复制字符串的时间。只有当string的内容发生变化的时候，才真正将改动的内容完整复制一份到新的地址，然后对原地址的引用计数减一，将新地址的引用计数设为１，最后将新string在内存映射表中指向这个新的地址。当某个字符串内存块的引用计数为０时，这块内存就可以被其他程序使用了。

注意：所有常量string会在编译时率先分配内存，其引用计数不会在程序中变化，始终为-1。

更详细的介绍，可以参考[标准C++类std::string的内存共享和Copy-On-Write技术](http://blog.csdn.net/haoel/article/details/24058)

##内存泄漏的发现##

在检查内存泄露时，无意发现了使用记录过程中产生的内存泄漏，请看下面的代码：

```
type
    TMyRec = record
        S: string;
        I: Integer;
    end;
    
procedure Test;
var
    ARec: TMyRec;
begin
    FillChar(ARec, SizeOf(ARec), #0);
    ARec.S:= 'abcd';
    ARec.I:= 1234;
    //....
    FillChar(ARec, SizeOf(ARec), #0);   //<---a leak!
    //...
```

FillChar的作用是对一个内存块进行连续赋值，内存泄漏出现在第二次调用FillChar的时候，经过调试后发现：如果把记录中的string字段改为PChar或者删除，就不会有内存泄漏了

##原因分析##

我们现在先了解一下记录（record）在内存中的内存是如何分配的。记录是一个不同数据类型的集合体。记录长度就是每个字段的内存长度之和（不过，因为结构体有内存对齐机制，所以可能会占用更多的内存）。注意，`该长度在编译之前就已经是确定的。因此那些长度不定的类型如string、对象都是以指针的形式出现在记录中的`。

我的分析是：`由于FillChar是低级内存读写操作，它仅仅把记录所占的内存块清掉，但是没有通知编译器更新字符串的引用计数，因而造成了泄漏`。请看下面的代码：

```
function StringStatus(const S: string): string;
begin
    Result:= Format('Addr: %p, RefCount: %d, Value: %s',
        [Pointer(S), PInteger(Integer(S)-8)^, S]);
end;

procedure BadExample1;
var
    S1: string;
    Arec: TMyRec;
begin
    S1:= Copy('string', 1, 6);  //force allocate memory for the string
    WriteLn(StringStatus(S1));
    ARec.S:= S1;
    WriteLn(StringStatus(ARec.S));
    FillChar(ARec, SizeOf(ARec), #0);
    WriteLn(StringStatus(S1));
end;
```

运行后输出的结果是

```
Addr: 00E249E8, RefCount: 1, Value: string     //OK, Allocated as a new string
Addr: 00E249E8, RefCount: 2, Value: string     //OK, RefCount increated
Addr: 00E249E8, RefCount: 2, Value: string     //WRONG! RefCount should be 1
```

关于为什么能输出字符串的引用计数，可以参见[这篇博客](http://www.xumenger.com/delphi-string-pchar-chararray-20150415/)，可以看出存储字符串的内存结构是这样的

* 01~02 字节是代码页，如0x03A8为十进制的936，表示简体中文GBK
* 03~04 字节表示每个字符所占的字节数（ANSI为1，Unicode为2）
* 05~08 字节是该字符串的引用计数
* 09~12 字节是该字符串的字符个数
* 13~?? 字节就是字符串实际的内容了
* ??    最后一个字节是00，字符串的结束符

在执行FillChar之前，字符串S1的引用计数是２，但是执行完FillChar之后并没有减１。这段代码验证了我的推测：`FillChar操作可能会破坏字符串的Copy-On-Write机制，使用的时候需要小心！`

##进一步分析##

文章的开头我提到`所有的常量string会在编译时率先分配内存，其引用计数不会在程序中变化，始终是-1`。

那么，如果我们让S1和ARec.S都赋值为一个常量字符串，那么照理就不用管引用计数，也就没有泄漏问题了。请看下面的例子：

```
procedure BadExample2;
var
    S1: string;
    ARec: TMyRec;
begin
    S1:= 'string';  //Assign S1 to a const (compiler time allocated) string
    WriteLn(StringStatus(S1));
    ARec.S:= S1;
    WriteLn(StringStatus(ARec.S));
    FillChar(ARec, SizeOf(ARec), #0);
    WriteLn(StringStatus(S1));
end;
```

运行后的输出结果是

```
Addr: 00E249E8, RefCount: -1, Value: string    //OK, RefCount UN-Changed
Addr: 00E249E8, RefCount: 1, Value: string     //!!! Allocated as a new string
Addr: 00E249E8, RefCount: -1, Value: string     //OK, RefCount UN-Changed
```

是不是很吃惊？对赋值ARec.S的时候，结果并不是预期的那样直接将其指向常量字符串，而是重新分配了一个新的字符串。我个人认为：`记录在对字符串赋值上是有问题的`

##解决方法##

既然知道使用FillChar来初始化记录是不安全的，那么我们是不是要回到解放前，手动对记录进行初始化呢？

也不用！Delphi有一个保留字`out`。它和var、const一样，是用来修饰函数参数的。它和var的功能相似，不同的是，它会对那些以指针形式传进的变量进行引用计数清理。Delphi的帮助中的解释是：

>An out parameter, like a variable parameter, is passed by reference. With an out parameter, however, the  initial value of the referenced variable is discarded by the routine it is passed to. The out parameter is for output only; that is ,it tell the function or procedure where to store output, but doesn't provide any input

哈哈，这个不正是FillChar想做但又没有做到的吗？于是我改造了一下InitializeRecord来初始化记录

```
procedure InitializeRecord(out ARecord: TMyRec; count: Integer);
begin
    FillChar(ARecord, count, 0);
end;
```

仅仅是多了一层函数嵌套，内存泄漏的问题就解决了，多亏了这个神奇的out！

我们来仔细看看加了这个out之后，编译器到底做了什么？

```
mov edx,[$0040c904]
mov eac,ebx
call @FinalizedRecord   //<-----cleanup
mov edx,$0000000c
call InitializeRecord
```

关键是第三行调用了FinalizeRecord。这是System.pas中的一个汇编函数，作用就是对记录做一下清除工作。如果你想一探究竟，可以自己查一下这个函数是如何实现的。这里就不做详细解释了。

##想法总结##

没想到一个偶然的发现，竟可以带出这么多的问题，真是因祸得福。我总结了一下几点：

* FillChar是低级的内存读写，所以在使用之前你要非常清楚要打算做什么。
* 在记录类型中慎用string和Widestring。如果记录的结构复杂，不妨尝试封装成类，类可以提供更丰富的特性，扩展性更好。如果一定要定义带string的记录，最好注释一下，以免日后出错。（有时候的确是record更方便更高效）
* 活用out保留字可以解决接口类型和带string的记录类型的引用计数问题
