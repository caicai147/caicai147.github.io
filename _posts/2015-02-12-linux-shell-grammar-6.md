---
layout: post
title: Shell编程基础教程6--shell函数 
categories: linux之shell
tags: linux shell 命令行 函数
---


6.1.定义函数
---

简介：shell允许将一组命令集或语句形成一个可用块，这些块成为shell函数

### 定义函数的格式

方法一

    函数名()
    {
        命令1
        ......
    }

方法二

    function 函数名()
    {
        命令1
        ......
    }

函数定义的两种方式：函数可以放在同一个文件中作为一段代码，也可以放在只包含函数的单独文件中

例子，在一个单独文件中只定义以下函数：

    #!/bin/bash
    #hellofun
    function hello()
    {
        echo "Hello, today is `date`"
        #`date`  date用反引号括起来，表示在这条语句中先执行date命令，并将date命令的结果作为替换date位置的字符串
        return 1
    }

6.2.函数调用
---

例子

    #!/bin/bash
    #func
    function hello()
    {
       echo "Hello,today is `date`"
    }
    echo "now going to the fuction hello"
    #接下来就是调用函数hello
    hello
    echo "back from the function hello"

6.3.参数传递
---

简介：向函数传递参数就像在脚本中使用位置变量 $1、$2、...$9

例子

    #!/bin/bash
    #func
    function hello()
    {
        echo "Hello, $1 today is `date`"
    }
    echo "now going to the fuction hello"
    #接下来就是调用函数hello，传入参数 china
    hello china
    echo "back from the function hello"

6.4.函数文件
---

简介：可以将函数定义和函数调用放在同一个文件中，也可以在一个文件中专门定义函数，在其他文件来调用该函数

例子：在文件 hellofun 中定义函数

    #!/bin/bash
    function hello()
    {
        echo "Hello, today is `date`"
        return 1
    }
    
在另一个文件 func 中调用该函数

    #!/bin/bash
    #载入文件，标明定义函数的文件，格式为：.空格文件名（特别注意一定要在.和文件名之间有空格）
    . hellofun
    echo "now going to the fuction hello"
    hello
    echo "back from the function hello"

建议：学习shell脚本的时候，可以多看看linux系统的启动文件来学习，但是注意在看的过程中，如果想要对它进行修改，一定要首先备份一下这个文件，在对其进行修改，以便能在出错时使用备份文件挽回

6.5.载入和删除函数
---

检查载入函数和删除函数

### 查看载入函数：set命令查看这个函数如何载入

例子：

    #!/bin/bash
    . hellofun
    set
    echo "now going to the fuction hello"
    hello
    echo "back from the function hello"

### 删除函数：unset命令

例子

    #!/bin/bash
    . hellofun
    set
    #使用unset使得接下来执行hello时候不能执行成功，因为unset删除了函数
    unset hello
    echo "now going to the fuction hello"
    hello
    #这里会输出信息：./func:hello:command not found
    echo "back from the function hello"


6.6.函数返回状态值
---

例子1：

    #!/bin/bash
    function hello()
    {
         echo "Hello, today is `date`"
         return 0
    }
    echo "now going to the function hello"
    hello
    #接下来来输出返回状态值，用 $? 表示
    echo $?
    echo "back from the function hello"

例子2：

    #!/bin/bash
    function hello()
    {
       echo "Hello, today is `date`"
       return 0
    }
    echo "now going to the function hello"
    returnvalue=hello
    #接下来来输出返回状态值，用 $? 表示
    echo $?
    echo $returnvalue
    echo "back from the function hello"
    #注意：不同于C语言等语言，shell脚本中不能将函数的返回值赋给变量
    #returnvalue=hello 不是将hello函数的返回值赋给returnvalue，而是将hello字符串赋给returnvalue

 