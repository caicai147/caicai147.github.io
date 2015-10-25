---
layout: post
title: Shell编程基础教程2--变量和运算符
categories: linux之shell
tags: linux shell 命令行
---


2.1.变量的类型
-----

本地变量；环境变量；变量替换（显示变量）；位置变量；标准变量；特殊变量；

2.2.本地变量
-----

本地变量在用户现在的shell生命周期的脚本中使用

在命令行， LOCALTEST="test" 设置本地变量，$ echo $LOCALTEST 显示本地变量

set 命令可以查看这个shell的生命周期有哪些本地变量

readonly LOCALTEST ，就可以将LOCALTEST设置成只读的本地变量，不能再对其进行赋值操作，此时若执行如 LOCALTEST="local" 就会报错。注意设置成readonly的变量就不可能再重新变回可写的了，所以要谨慎使用

readonly ，就可以查看系统中有多少只读的变量

2.3.环境变量
-----

环境变量用于所有用户进程（经常成为子进程）。登录进程称为父进程。shell中执行的用户进程均称为子进程。不像本地变量（只用于现在的shell），环境变量可用于所有子进程，这包括编辑器、脚本和应用。

环境变量既可以用于父进程又可以用于子进程，环境变量都包含在本地变量中；但是不是环境变量的本地变量不能运行于所有的子进程中

在$HOME/.bash_profile这个配置文件里面设置对应用户的环境变量；在/etc/profile这个配置文件里面设置所有用户的环境变量

用 export CHINAPATH="china" 来设置环境变量

用 env 或者$ export 来输出所有环境变量的信息

同样可以用类似 readonly CHINAPATH 的命令将环境变量设置为只读

2.4.变量替换
-----

变量替换就是用变量的值替换它的名字

    ${Variable_name} 或者 $Variable_name ，显示实际值到Variable_name
    
    ${Variable_name:+value} ，如果设置了Variable_name，则显示value，否则为空
    
    ${Variable_name:?value} ，如果未设置Variable_name，显示用户自定义的错误信息value
    
    ${Variable_name:-value} ，如果未设置Variable_name，则显示value，如果设置过了就显示Variable_name的值
    
    ${Variable_name:=value} ，如果未设置Variable_name，设置其值为value，并显示，如果设置过了就显示Variable_name的值

2.5.变量清除
-----

例如， unset CHINAPATH ，就是清除变量CHINAPATH的值，若再执行 echo CHINAPATH就显示空值

readonly的变量也是不能unset的

2.6.位置变量
-----

位置变量表示$0，$1，$1，$2，……，$9


| $0     | $1 | $2 | $3 | $4 | $5 | $6 | $7 | $8 | $9 |  
| ------ |--- |--- |--- |--- |--- |--- |--- |--- | ---|  
| 脚本名字 |    |    |    |    |    |    |    |    |    | 

作用1：在脚本中使用位置参数，例如脚本parm.sh

    #!/bin/bash
    #测试位置变量
    echo "这是脚本的名称：$0"
    echo "这是脚本的第一个位置参数：$1”
    echo "二：$2"
    echo "三：$3"
    echo "四：$4"
    echo "五：$5"
    echo "六：$6"
    echo "七：$7"
    echo "八：$8"
    echo "九：$9"

执行命令 chmod u+x parm.sh，再执行./parm.sh A B C D E，输出结果

    这是脚本的名称：./parm.sh
    这是脚本的第一个位置参数：A
    二：B
    三：C
    四：D
    五：E
    六：
    七：
    八：
    九：

作用2：向系统命令传递参数，例如

    #!/bin/bash
    #parm_1.sh
    find /home/perfect/shell -name $1 -print

执行命令 chmod u+x parm_1.sh，再执行./parm_1.sh myfile.txt，其就是等价于命令 find /home/perfect/shell -name myfile.txt -print

2.7.标准变量
-----

bash默认建立了一些标准环境变量，可在/etc/profile中定义。

比如EXINIT、HOME、IFS、PATH、LOGNAME、MAIL、MAILCHECK……

2.8.特殊变量
-----

    $#　传递到脚本的参数个数
    
    $*　以一个单字符串显示所有向脚本传递的参数。与位置变量不同，此选项参数可超过9个
    
    $$　脚本运行的当前的ID号
    
    $!　后台运行的最后一个进程的进程ID
    
    $@　传递到脚本的参数列表，使用时需要加引号，并在引号中返回每个参数
    
    $-　显示shell使用的当前选型，与set命令功能相同
    
    $?　显示最后命令的退出状态。0表示没错误，其他任何职表明有错误
    
看一个实例

    #!/bin/bash
    #parm_2.sh
    echo "显示参数个数：￥#"
    echo "显示脚本的全部参数：$*"
    echo "显示进程ID：$$"
    echo "显示前一个命令运行后状态：$?"

执行命令 chmod u+x parm_2.sh，再执行./parm_2.sh A CHINA chinaitalb 输出结果因不同的机器而不同

    显示参数个数：3
    显示脚本的全部参数：A CHINA chinaitalb
    显示进程ID：7799
    显示前一个命令运行后状态：0

2.9.影响变量的命令
-----

`declare`　用来设置或显示变量。 `-f` 只显示函数名； `-r` 创建只读变量（declare和typeset，不能用 + 颠倒）； `-x` 创建转出变量； `-i` 创建整数变量； 使用 + 代替 - ，可以颠倒选项的含义

`export`　用于创建子shell的变量。 `--` 表明选项结束，所有的后续参数都是实参； `-f` 表明在“名-值”对中的名字是函数名； `-n` 把全局变量转换成局部变量，换句话说，命令的变量不再传给shell； `-p` 显示全局变量列表

`readonly`　用于设置或显示只读变量。 `--` 表明选型结束； `-f` 创建只读变量

`set`　设置或重设各种shell

`shift`　用于移动位置变量，调整位置变量，例如 shift 1将$3的值赋给$2，将$2的值赋给$1，而 shift 2将$3的值赋给$1，规律以此类推

`typeset`　用于显示或设置变量。 是declare的同义词

`unset`　用于取消变量的定义。 `--` 表明选项结束； `-f` 删除只读变量，但不能取消从shell环境中删除指定的变量和函数，如PATH、PS1、PS2、UID、PPID、EUID……的设置

2.10.引号
-----

引号的必要性：变量和替换操作，在脚本里执行变量替换时最容易犯的一个错误就是引用错误。例如 echo ert* 和 echo "ert*" 的区别

2.11.双引号（依然有疑问）
-----

使用双引号可以引用除了字符 $、`（反引号）、\ 之外的的任意字符或字符串

可以测试 

    echo "ert, $SHELL * \nchina`echo itlab`"

输出结果是

    ert, /bin/bash * 
    chinaitlab

　　
2.12.单引号
-----

单引号与双引号类似，不同的是shell会忽略任何引用值。换句话说，如果屏蔽了其特殊含义，会将引号里的所有字符，包括引号都作为一个字符串

可以测试 

    echo 'ert, $SHELL * \nchina`echo itlab`'

输出结果是

    ert, $SHELL * china`echo itlab`

2.13.反引号
----

反引号用于设置系统命令的输出到变量。shell将反引号中的内容作为一个系统命令，并执行其结果，例如可以测试

    echo "* china`echo itlab`"

输出结果是

    * chinaitlab

2.14.反斜杠
-----

如果一个字符有特殊含义，反斜杠防止shell误解其含义，即屏蔽其特殊含义 。下述字符包含有特殊含义：$ & * + ^ ` " | ?

例如 echo \*，其输出结果是 *，而 echo * 是输出当前目录下的所有文件名

2.15.运算符
-----

运算符是一个对计算机发的指令。

运算对象：数字、字符（子面值）；变量；表达式

表达式：运算符和运算对象的结合体

2.16.运算符类型
----

按位运算符：~，<<，>>，&，|，^。与C里面的按位运算符相同

$[ ]表示形式告诉shell对方括号里的表达式求值，例如

    #!/bin/bash
    echo $[ 2+8 ]
    echo $[ 2<<4 ]

执行结果是

    10
    32

逻辑运算符：&&，||，>，<，==，!=。和其他语言也都是一样的，举例

    #!/bin/bash
    echo $[ 2&&2 ]
    echo $[ 2&&0 ]
    echo $[ 2>0 ]

执行结果是

    1
    0
    1　

赋值运算符：=、+=、-=、*=、/=、%=、&=、^=、|=、<<=、>>=。和其他语言也是一样的

    let count = $count + $change

    let count += $change

例子：

    #!/bin/bash
    var=65
    echo $var
    let var+=4
    echo $var

执行结果是 

    65
    69

2.17.表达式替换
-----

$[ ]和$(())。习惯使用$[ ]，所有shell的求值都是用整数完成的

$[ ]可以接受不同基数的数字。$[ base#n ] 中 n 表示基数从2到36的任何基数，例如执行命令 echo $[ 10#8+1 ] ，执行结果是 9。意思是 8 进制的 10 去加 1 操作，所以结果是9

2.18.运算符优先级
------