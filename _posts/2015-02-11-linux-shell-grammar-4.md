---
layout: post
title: Shell编程基础教程4--控制流结构
categories: linux之shell
tags: linux shell 命令行
---


4.1.控制结构
---
        
4.2.if then else语句
---

格式：

    if 条件1        //如果条件1为真
    then
        命令1        //那么，执行命令1
    elif 条件2        //如果条件1不成立，而条件成立
    then
        命令2        //那么，执行命令2
    ……更多的elif then组合……
    else
         命令2        //如果条件1、2……都不成立，那么执行命令3
    fi                //完成，if语句必须以单词fi终止

最简可以是形式：

    if 条件
    then
        命令
    fi

等价于

    if 条件; then
        命令
    //;可以在脚本的一行里写多条命令

例程1：

    #!/bin/bash
    #if test
    if [ "10" -lt "12" ]
    then
        echo "Yes, 10 is less than 12"
    else
        echo "No"
    fi


注意语法细节（在我的ubuntu 12.04 LTS中）：

1. if或elif与[ 之间必须有一个空格，否则会出错！！！
2. 条件的 [ 之后 和 ]之前也必须有一个空格

我的经历

    因为没有注意这个细节，写的脚本会报出类似下面的错误
    ./cpdir.sh: 行 10: if[ 2 -ne 2 ]: 未找到命令
    ./cpdir.sh: 行 11: 未预期的符号 `then' 附近有语法错误
    ./cpdir.sh: 行 11: `then'

例子
    if [ "10" -lt "12" ]在if后有空格、[后有空格、]前有空格，这就是正确的
    if ["10" -lt "12"]在[后没空格、]前没空格就是错误的

if和elif后的判断条件（用 `man test` 查看详细内容）

    [ EXPRESSION ]    EXPRESSION is true
    !EXPRESSION        EXPRESSION is false
    EXPRESSION1 -a EXPRESSION2    both EXPRESSION1 and EXPRESSION2 are true
    EXPRESSION1 -o EXPRESSION2    either EXPRESSION1 or EXPRESSION2 is true
    [ -n ] STRING        the length of STRING is nonzero
    -z STRING     the length of STRING is zero
    STRING1 = STRING2    the STRINGs are equal
    STRING1 != STRING2    the STRINGs are not equal
    INTEGER1 -eq INTEGER2    INTEGER1 is equal to INTEGER2
    INTEGER1 -ge INTEGER2    INTEGER1 is greater than or equal to INTEGER2
    INTEGER1 -gt INTEGER2    INTEGER1 is greater than INTEGER2
    INTEGER1 -le INTEGER2    INTEGER1 is less than or equal to INTEGER2
    INTEGER1 -lt INTEGER2    INTEGER1 is less than INTEGER2
    INTEGER1 -ne INTEGER2    INTEGER1 is not equal to INTEGER2
    FILE1 -ef FILE2        FILE1 and FILE2 have the same device and inode numbers
    FILE1 -nt FILE2        FILE1 is newer(modification data) than FILE2
    FILE1 -ot FILE2        FILE1 is older than FILE2
    -b FILE        FILE exists and is block special
    -c FILE        FILE exists and is character special
    -d FILE        FILE exists and is a directory
    -e FILE        FILE exists
    -f FILE        FILE exists and is a regular file
    -g FILE        FILE exists and is set-group-ID
    -G FILE        FILE exists and is owned by the effective group ID
    -k FILE        FILE exists and has its sticky bit set
    -L FILE        FILE exists and is a symbolic link
    -O FILE        FILE exists and is owned by the effective user ID
    -p FILE        FILE exists and is a named pipe
    -r FILE        FILE exists and is readable
    -s FILE        FILE exists and has a size greater than zero
    -S FILE        FILE exists and is a socket
    -f [ FD ]        file directory FD(stdout by default) is opened on a terminal
    -u FILE        FILE exists and its set-user-ID bit is set
    -w FILE        FILE exists and is writable
    -x FILE        FILE exists and is executable

例程2：

    #!/bin/bash
    #if test2
    echo -n "Enter your name:"
    read NAME
    #did the user just hit return
    if[ "$NAME" = "" ]
    #注意字符串变量比较的具体格式
    then
        echo "You did not enter any information"
    else
        echo "Your name is ${NMAE}"
        #注意字符串变量输出使用的具体格式
    fi

例程3：

    #!/bin/bash
    #if cp test
    if cp myfile.bak myfile
    then
        echo "good copy"
    else
        echo "`basename $0`:error could not copy the files" > &2
    fi

例程4：

    #!/bin/bash
    #if elif
    echo -n "Enter your name:"
    read NAME
    if [ -z $NAME ] || [ "$NMAE"="" ]
    then
        echo "You did not enter a name"
    elif [ "$NMAE"="root" ]
    then
        echo "Hello root"
    elif[ "$NAME"="perfect" ]
    then
        echo "Hello perfect"
    else
        echo "You are not root or perfect, but hi $NAME"
    fi

4.3.case语句
---

格式：

    case 值 in
    模式1)
        命令1
        ;;
    模式2)
        命令2
        ;;
    esac

简介：case取值后面必须为单词 in ，每一模式必须以右括号结束。取值可以是变量或常数。匹配发现取值符合某一模式后，其间所有命令开始执行直至 ;; 。模式匹配符 * 表示任意字符， ? 表示任意单个字符， [ .. ]表示类或范围中任意一个字符

例程1：

    #!/bin/bash
    #case select
    echo -n "Enter a number from 1/2/3/y/Y:"
    read ANS
    case $ANS in
    1)
        echo "You select 1"
        ;;
    2)
        echo "You select 2"
        ;;
    3)
        echo "You select 3"
        ;;
    y|Y)    
        #y或者Y
        echo "you select $ANS"
        ;;
    *)
        echo "`basename $0`:This is not between 1 and 3" > &2
        exit;
        ;;
    esac

例程2：
     
4.4.for循环
---

格式：

    for 变量名 in 列表
    do
        命令1
        命令2
        ……
    done

简介：当变量值列表里，for循环即执行一次所有命令，使用变量名访问列表中取值。命令可为任何有效的shell命令和语句。变量名为任意单词。in列表用法是可选的，如果不用它，for循环使用命令行的位置参数。in列表可以包含替换、字符串和文件名

例程1：

    #!/bin/bash
    #fortest1
    for lop in 1 2 3 4 5
    do
         echo $loop
    done

例程2：

    #!/bin/bash
    #fortest2
    for loop in "orange red blue grey"
    do
        echo $loop
    done
 
区别于

    #!/bin/bash
    #fortest2
    for loop in orange red blue grey
    do
         echo $loop
    done
 

例程3：

    #!/bin/bash
    #fortest3
    for loop in `cat listfile`
    #``反斜杠里可以放shell命令
    #将listfile里面的字符串作为in的列表来用的具体格式（文件中的字符串以空格或回车作为分隔）
    do
        echo $loop
    done

4.5.until循环
---

格式：

    until 条件
    do
        命令1
        命令2
        ……
    done

简介：条件可为任意测试条件，测试发生在循环末尾（先执行do和done中间的命令，再执行until里的判断，直到until里面的条件为真再停止循环），因此循环至少执行一次

例程1（可以用于服务器管理员监控分区，循环执行检查分区使用是不是还在安全范围内，并将结果mail给系统管理员）：

    #!/bin/bash
    #监控分区
    #定义监控的分区
    Part="/backup"
    #得到磁盘使用的百分比
    LOOK_OUT=`df | grep $Part | awk '{print $5}' | sed 's/%//g'`    #这条语句将``里面命令执行的结果赋值给LOOK_OUT
    echo $LOOK_OUT
    until[ "LOOK_OUT" -gt "90" ]
    do
        echo "Filesystem /backup is nearly full" | mail root
        LOOK_OUT=`df | grep $PART | awk '{print $5}' | sed 's/%//g'`
        sleep 3600        #程序休眠一小时
    done

4.6.while循环
---

格式：

    while 命令
    do
        命令1
        命令2
        ……
    done

或

    while 条件
    do
         命令1
         命令2
         ……
    done

注意语法细节：

1. while的条件放在 [] 中
2. while与[ 之间必须有一个空格，否则会出错！！！
3. 条件的 [ 之后 和 ]之前也必须有一个空格

简介：在while和do之间虽然通常指使用一个命令，但可以放几个命令，命令通常用作测试条件

例程1：
---
    #!/bin/bash
    #whileread1
    echo "按住 <ctrl>+D 退出输入"
    while echo -n "输入你最喜欢的电影：";read FILM
    do
         echo "Yeah, ${FILM}是一部好电影"
    done

例程2：

    #!/bin/bash
    #whileread2
    while read LINE
    do
        echo $LINE
    done < names.txt
    #这样是合法的格式，通过在done后面放上 <names.txt 来重定向输入
    
               类比格式：
    
    #!/bin/bash
    #whileread2
    while read LINE < names.txt
    do
        echo $LINE
    done
    #这个格式将会无限循环只输出names.txt的第一行的内容

4.7.break控制
---

格式：
    break [ n ]
    break 其实就是 break 1

简介：
    退出循环
    如果是在一个嵌入循环中，可以指定n来跳出的循环的个数

例程1：

    #!/bin/bash
    #breakout
    while :
    #while : 表示永远为真，注意while和: 之间一定要有一个空格，否则会有语法错误
    do
         echo -n "Enter any number[ 1...5 ]:"
         read ANS
         case $ANS in
         1|2|3|4|5)
              echo "You enter a number between 1 and 5."
              ;;
          *)
              echo "Wrong bumber, Bye."
              break
              #break 用于跳出while循环，而不是用于跳出case，这里不同于C等语言
              ;;
          esac
    done

4.8.continue控制
---

简介：跳出循环步

continue不同于break，continue不会跳出整个循环，而只是跳出这个循环中的某一步而已

例程1：

    #!/bin/bash
    #continue
    while:
    do
        echo -n "Enter any number [ 1...5 ]:"
        read ANS
        case $ANS in
        1|2|3|4|5）
             echo "You enter a number between 1 and 5"
             ;;
         *)
              echo -n "Wrong number, continue(y/n)?:"
              read IS_CONTINUE
              case $IS_CONTINUE in
              y|yes|Y|YES
                   continue
                   ;;
              *)
                   break
                   ;;
              esac
         esac
    done    
    