---
layout: post
title: 函数指针---C/C++中int *f(4)和int (*f)(4)的区别
categories: c/c++之函数 c/c++之指针与内存
tags: c c++ 函数 函数指针 指针 内存 字符串
---


简单说明
-----

`int *f()`，表示这个函数f，函数的返回值的类型是 int *。

`int (*f)()`，表示这是一个函数指针，它要指向一个函数才能有用，指向一个函数之后就可以用它来代替该函数，之后使用这个指针就相当于使用该函数。

通过程序去说明
-----

    #include<stdio.h>
    int fun(int);
    int main(void)
    {
        int (*f1)(int);
        int (*f2)(int);
    
        f1 = fun;
        f2 = &fun;//注意上面的两种初始化函数指针的方法都是正确的，
                  //所以在初始化函数指针的时候可以选择用&或者不用
        f1(2);
        f2(2);
        return 0;
    }
    
    int fun(int i)
    {
        printf(“%d”, i);
        return 1;
    }
    
     