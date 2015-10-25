---
layout: post
title: Delphi函数四种参数传递方式
categories: delphi之函数 delphi之基础语法
tags: delphi 函数
---


Delphi的参数可以分为：默认参数（传值）、var（传址）、out（输出）、const（常数）四类

可以对比C/C++的相关知识，类比学习。

##1.默认参数是传值，不会被改变##

例子

    function MyFun(x : Integer) : Integer;
    begin
        Inc(x);
        Result := x;
    end;

##2.var参数是传址，会被改变##

例子

    function MyFunVar(var x : Integer) : Integer;
    begin
        Inc(x);
        Result := x;
    end;

##3.out参数##

out参数是为支持Com的，和var的结果是一样的，一般我们用不着

    function MyFunOut(out x : Integer) : Integer;
    begin
        Inc(x);
        Result := x;
    end;
    
##4.const参数绝对不可以赋值的##

const参数绝对不可以赋值的，这是被编译器优化的方式，尽量多用
    
    function MyFunConst(const x : Integer) : Integer;
    begin
        Inc(x);    //这句会报错，因为带const前缀的参数是不可以被修改的
        Result := x;
    end;

下面做调用这些函数的测试

    procedure TForm1.Button1Click(Sender : TObject);
    var
       a : Integer;
    begin
        a := 5;
        MyFun(a);
        ShowMessage(IntToStr(a));    //6
        
        a := 6;
        MyFunVar(a);
        ShowMessage(IntToStr(a));    //7
    
        a := 6;
        MyFunOut(a);
        ShowMessage(IntToStr(a));    //7
    
        a := 6;
        MyFunConst(a);
        ShowMessage(IntToStr(a));    //6，因为在MyFunConst里面是不能对const参数进行修改的
    
    end;

　　