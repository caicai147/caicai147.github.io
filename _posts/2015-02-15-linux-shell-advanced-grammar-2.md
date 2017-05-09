---
layout: post
title: Linux Shell 高级编程技巧2----shell工具
categories: linux之shell
tags: linux shell 命令行
---


2.1.日志文件
---

简介：创建日志文件是很重要的，记录了重要的信息。一旦出现错误，这些信息对于我们排错是非常有用的；监控的信息也可以记录到日志文件

### 常用的日志文件的方法

#### 以时间为标识的日志文件

例子

    #!/bin/bash
    #当前的日期
    current_date=`date "+%Y%m%d"`
    #今天的日志文件名
    todaylog="log/${current_date}.log"
    #如果日志文件不存在，创建一个
    if [! -f $todaylog ]
    then
        touch $todaylog
    fi
    #输出日志到日志文件
    log_time_format=`date "+%Y-%m-%d %T"`
    echo "${log_time_format} 命令开始" >>$todaylog
    #
    #command blocks
    sleep 4
    #
    #输出日志到日志文件
    log_time_format=`date "+%Y-%m-%d %T"`
    echo "${log_time_format}命令结束" >>$todaylog

#### 以进程号为标识的临时文件

例子

    #!/bin/bash
    #取得当前进程号
    current_PID=$$
    #获得特定进程的进程号并重定向到一个临时文件中
    ps -aux | grep "/user/sbin/httpd" | grep -v "grep" | awk '{print $2}' > /tmp/${current_PID}.txt
    #命令块开始
    for pid in `cat /tmp/${current_PID}.txt`
    do
    {
        echo "kill -9 $pid
        kill -9 $pid
    }
    done
    #命令块结束
    #删除临时文件
    rm -f /tmp/${current_PID}.txt

2.2.信号
---

简介：信号就是系统向脚本或命令发出的消息，告知它们某个事件的发生

kill命令

    kill -l        列出所有的信号

列出一些常用信号

    1    SIGHUP        挂起或父进程被杀死
    2    SIGINT        来自键盘的中断信号，通常是 Ctrl-C
    3    SIGQUIT        从键盘退出
    9    SIGKILL        无条件退出
    11    SIGSEGV        段（内存）冲突
    15    SIGTERM        软件终止（缺省杀进程）

信号0为”退出shell“信号。为了发出信号0，只要从命令行键入exit，或者在一个进程或命令行中使用 Ctrl-D 即可

    kill        发送信号给进程

例子
    kill -s 信号名 进程号    这种格式来给进程发送相应信号
    kill -s SIGKILL 7696    杀死进程号为7696的进程
    kill -信号的编号 进程号        这种格式来给进程发送对应的信号
    kill -9 7696        杀死进程号为7696的进程

2.3.trap捕捉信号
---

简介：信号可以被应用程序或脚本捕获，并依据该进程号（1、2、3和15）采取相应的行动。一些信号不能被捕获。例如，如果一个命令收到了信号9，就无法再捕捉其他信号

捕捉到一个信号后，它可能会采取下面的三种操作之一

1. 不采取任何行动，由系统来进行处理
2. 捕获该信号，但是忽略它
3. 捕捉该信号，并采取相应行动

dtrap可以使你在脚本中捕捉信号。命令格式是：`trap name signal(s)`

* 其中name是捕捉到信号以后所采取的一系列操作。实际中，name一般是一个专门用来处理所捕捉信号的函数。name需要用双引号""引起来。
* signal就是待捕捉的信号

最常见的行动是

1. 清除临时文件
2.忽略该信号    例子：trap "" 2 3
3. 询问用户是否终止该脚本的运行

例子1

    #!/bin/bash
    #捕获信号2，如果捕获到就执行exitprocess
    trap "exitprocess" 2
    LOOP=0
    function exitprocess()
    {
        echo "You just hit <CTRL-C>, at number $LOOP"
        echo "I will now exit"
        exit 1
    }
    while:
    do
        LOOP=$[$LOOP+1]
        echo $LOOP
        sleep 1
    done

例子2

    #!/bin/bash
    LOOP=0
    trap "exitprocess" 2
    HOLD1=/tmp/hold1.$$
    HOLD2=/tmp/hold2.$$
    function exitprocess()
    {
        echo -e "\nRecived Interrupt...."
        echo -n "Do you really wish to exit?(Y/N)"
        read ANS
        case $ANS in
        Y|y)
            rm_tmp_file
            ;;
        N|n)
            ;;
        *)
            exitprocess
            ;;
        esac
    }
    
    function rm_tmp_file()
    {
        echo "<CTRL-c> detected .. Now cleaning up ... Wait"
        rm /tmp/*.$$ 2>/dev/null
        exit 1
    }
    while :
    do
        LOOP=$[$LOOP+1]
        echo $LOOP
        df >> $HOLD1
        ps -xa >> $HOLD2
        sleep 1
    done


2.4.eval
---

简介：eval命令将会首先扫描命令行进行所有的置换，然后再执行该命令。该命令适用于那些一次扫描无法实现其功能的变量

例子，myfile 文件的内容是 ls -l

MYFILE="cat myfile";`eval $MYFILE`

等价于    eval `cat myfile`

也就是执行 cat myfile 输出结果所代表的那条命令
    
2.5.logger
---

简介：logger命令向/var/log/messages文件发送消息

logger命令一般形式是：`logger -p -i message`

* -p    为优先级，这里只涉及到提示用户注意的优先级，这也是缺省值
* -i    在每个信息中记录发送消息的进程号

例子
    logger -p -i "chinaitlab shenzhen"
    查看 /var/log/messages 就可以查看插入了这条信息