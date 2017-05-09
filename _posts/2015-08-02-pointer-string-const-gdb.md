---
layout: post
title: 指针和字符串和字符串常量、用gdb来获取非法内存中的内容
categories: c/c++之指针与内存
tags: c c++ 内存 指针 gdb 调试
---

## 例程1

```
#include<stdio.h>
int main(void)
{
    char *s="hello";
    printf("%s\n", s);
    s[0]="H"
        //因为s指针指向的字符串"hello"是字符串常量，所以不能通过指针进行更改，
        //所以这里会产生段错误
    printf("%s\n", s);
    return 0;
}
```

## 例程2

```
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
int main(void)
{
    char *s = (char *)malloc(6*sizeof(char));
    strcpy(s,"hello");
    printf("%s\n", s);
    s[0] = 'H';
    //因为为s分配了动态内存，所以更改可以通过s指针进行更改
    printf("%s\n", s);
    return 0;
}
```

## 例程3

字符串开始位置没有规定，但是规定必须以'\0'作为结尾标识符

```
int main(void)
{
    char *s="hello";
    printf("%s\n", s);
    //打印出hello
    printf("%s\n", &s[1]);
    //打印出ello
    printf("%s\n", &s[1]);
    //打印出llo

    //...

    printf("%s\n", &s[4]);
    //打印出o

    return 0;
}
```
    
## 编译调试例程3看看效果

假设例程3命名为string.c

用命令`gcc -g -o string string.c`来编译程序

再用gdb ./string来调试程序
>break main  
>run  
>next  
>next

现在步进到想要调试的位置

>print s

显示结果$1 = 0x40061c "hello"

>print s+1

显示结果$2 = 0x40061d "ello"

>print s[1]

显示结果$3= 0x40061d 'e'

注意区别 print s+1 与print s[1]的差异，其中print s+1等价于 print &s[1]

上面3条print命令都是访问string程序中合法的内存区域

>print s+10

显示的结果可能是$4 = 0x400626 "\003;("

这条print语句其实是去访问了在string中非法的内存区域（越界访问），但是通过gdb来访问也是可以获取相关信息的，有时候可以这样通过gdb来来获取某块以前使用过现在不合法但是可能有一些有用的信息的内存来获取一些有用信息