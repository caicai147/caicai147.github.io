---
layout: post
title: Delphi的数组名和C/C++的数组名
categories: delphi之指针与内存 c/c++之指针与内存
tags: c c++ delphi 数组
---


C/C++中的数组名相当于一个指针常量
-------

我们都知道，在C/C++中如果定义一个数组，那么这个数组的数组名就是这个数组的起始位置的指针，比如

    int i[10];

那么数组名 i 就是一个指针（准确的说是指针常量，因为它只能指向这个数组的首地址，不能被更改再去指向其他的地址），其就等于 &i[0] (取 i 数组的第一个元素的地址)

 

那么Delphi中的数组和数组名呢？
-------

那就自己编写一个程序尝试一下

    procedure TForm1.btn1Click(Sender: TObject);
    var
      a: array[0..7] of Integer;
      i: Integer;
    begin
      for i:= 0 to 7 do
        a[i]:= i;
    
      ShowMessage(IntToStr(Integer(a)) + '  ' + IntToStr(Integer(@a[0])));
    end;

但是编译的时候就报错，经过尝试之后进行修改

    procedure TForm1.btn1Click(Sender: TObject);
    var
      a: array[0..7] of Integer;
      i: Integer;
    begin
      for i:= 0 to 7 do
        a[i]:= i;
    
      ShowMessage(IntToStr(Integer(@a)) + '  ' + IntToStr(Integer(@a[0])));
    end;

在Delphi中，是不能够直接使用数组名的。否则会编译报错，要想取数组的首地址，需要 `@数组名`， 如上面的程序，或者 `@数组名[0]`，但是这也并不表示 数组名 就是数组名[0]，因为如果你以为 数组名就表示数组的第一个元素，那么你可以编程测试一下，一定会报错

而在C/C++中，要想获取数组的首地址，可以直接使用 数组名 或者 &数组名[0]

运行后结果如图
![img](../image/2015-08-07/c-array.png)

所以这两者的结果还是一样的，所以Delphi中的数组名还是表示数组的首地址，不过取数组名表示的地址的方法不一样

在Delphi中取数组的地址的时候，不能直接使用数组名，应该通过@，但是还有这样的直接使用数组名的方式

    procedure TForm1.btn1Click(Sender: TObject);
    var
      s: string;
      a: array[0..10] of Char;
      i: Integer;
    begin
      for i:=0 to 9 do
        a[i]:= 'j';
      a[10]= #0;
      s:= string(a);        //直接使用数组名来进行char数组的数组名来进行类型转换
      ShowMessage(s);
    end;

这样的程序编译成功，可以运行。
