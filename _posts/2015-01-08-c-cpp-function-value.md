---
layout: post
title: 正确理解C/C++中的传值调用/传址调用/引用调用
categories: C/C++之函数 C/C++之指针与内存 
tags: c c++ 函数 指针 内存
---


首先说明，函数的传值调用和传址调用在C和C++中都是合法的语法，但是引用调用就只是在C++中合法的。

传值调用
----

首先给一个广为人知的例子

    #include<stdio.h>
    void swap(int x, int y)
    {
        int tmp = x;
        x = y;
        y = tmp;
    }
    int main(void)
    {
        int a = 1, b = 2;
        printf("a=%d, b=%d\n", a, b);
        swap(a, b);
        printf("a=%d, b=%d\n", a, b);
        return 0;
    }

输出结果是  
a=1, b=2  
a=1, b=2  
没有交换

并没有完成交换，因为在传参调用中，main调用swap时候，将实参a, b的值分别拷贝给形参x, y，然后a, b就不再和swap有关了，swap交换的是x, y的值，但是x, y的作用域只在swap中，他们确实完成了交换，但是swap结束之后，x, y的值也就随之销毁了，所以根本不会对行参有任何影响，当然就不会实现实参的交换

传址调用
----

再给出一个类似的例子

    #include<stdio.h>
    void swap(int *x, int *y)
    {
        int tmp = *x;
        *x = *y;
        *y = tmp;
    }
    int main(void)
    {
        int a = 1, b = 2;
        printf("a=%d, b=%d\n", a, b);
        swap(&a, &b);
        printf("a=%d, b=%d\n", a, b);
        return 0;
    }

输出结果是  
a=1, b=2  
a=2, b=1  
完成了交换。

原因是这样的，传址调用实际上还是实参到形参的拷贝，不过这次实参是要交换的两个数字的指针（即地地址），而不是要交换的两个数本身，虽然形参在swap结束后被销毁，但是形参是根据要交换的两个数的地址完成交换的，所以对这两个数字产生影响，也就完成交换

引用调用
----

注意这个语法在C++里合法，但是在C里面是没有的

    #include<stdio.h>
    void swap(int &x, int &y)
    {
        int tmp = x;
        x = y;
        y = tmp;
    }
    int main(void)
    {
        int a = 1, b = 2;
        printf("a=%d, b=%d\n", a, b);
        swap(a, b);
        printf("a=%d, b=%d\n", a, b);
        return 0;
    }

注意：这段代码如果用gcc编译会报错，因为引用调用在C里面不合法，要用g++进行编译

输出结果是  
a=1, b=2  
a=2, b=1  
完成了交换

可以看到实现了两个数字的交换，但是这里的语法和上面的两种都不一样，这里并没有实参到形参的拷贝，而是直接将main里面的a, b传到swap里面，所以当然交换的就是a, b的值。

上面对int类型参数进行const传递只是为了演示其功能。

对于内部数据类型的输入参数，不要将“值传递”的方式改为“const 引用传递”。否则既达不到提高效率的目的，又降低了函数的可理解性。例如 `void Func(int x)` 不应该改为 `void Func(const int &x)`。

只是建议在传结构体或类对象的时候使用const 参数。