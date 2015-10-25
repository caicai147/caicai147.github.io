---
layout: post
title: Linux Shell 高级编程技巧4----几个常用的shell脚本例子
categories: linux之shell
tags: linux shell 命令行
---


4.0.在写脚本规范
---

同样适用在编程的时候，最好写好完善的注释

4.1.kill_processes.sh(一个杀死进程的脚本)
---

    #!/bin/bash
    current_PID=$$
    ps -aux | grep "/usr/sbin/httpd" | grep -v "grep" | awk '{print $2}' > /tmp/${current_PID}.txt
    for pid in `cat /tmp/${current_PID}.txt`
    do
    {
        echo "kill -9 $pid"
        kill -9 $pid
    }
    9done
    rm -f /tmp/${current_PID}.txt
    
4.2.cpdir.sh
---
    
    #!/bin/bash
    #此脚本用于将源目录下的子目录全部复制到目的目录中，不复制源目录中的文件，确保目的目录中的子目录是空目录
    
    #脚本用法函数
    usage()
    {
        echo "cpdir.sh 源目录 目的目录"
    }
    #判断是否为两个参数，否则提示脚本用法
    if[ $# -ne 2 ]
    then
    {
        usage
        exit
    }
    fi
    srcdir=$1
    desdir=$2
    #判断源目录${srcdir}是否为目录，否则提示错误信息和用法
    if [ ! -d $srcdir ]
    then
    {
        usage
        echo "错误：源目录${srcdir}不是目录"
        exit
    }
    fi
    #判断目的目录${srcdir}是否为目录，否则提示错误信息和用法
    if [ ! -d $desdir ]
    then
    {
        usage
        echo "错误：目的目录${desdir}不是目录"
        exit
    }
    fi
    processid=$$;
    #查找源目录下所有的子目录，输出并保存到/tmp/srcdir_进程号.txt文件中
    echo "源目录下${srcdir}所有的子目录"
    echo "------------------------------"
    find $srcdir/* -type d | /usr/bin/tee /tmp/srcdir_tmp_${processid}.txt
    sed "s/^${srcdir}/${desdir}/g" /tmp/srcdir_tmp_${processid}.txt > /tmp/srcdir_${processid}.txt
    #在目的目录下建立空子目录
    rm -rf ${desdir}/*
    for subdir in `cat /tmp/srcdir_${processid}.txt`
    do
    {
        mkdir ${subdir}
    }
    done
    echo ""
    echo "目标目录下${desdir}所有的子目录"
    find $desdir/* -type d | /usr/bin/tee /tmp/desdir_${processid}.txt
    #比较在目的目录下建立空子目录后的差异
    echo ""
    echo "--------------------------"
    diff /tmp/desdir_${processid}.txt /tmp/srcdir_${processid}.txt
    rm -f /tmp/srcdir_${processid}.txt
    rm -f /tmp/desdir_${processid}.txt
    rm -f /tmp/srcdir_tmp_${processid}.txt

4.3.我的疑惑
---

<<mayday

各种信息

mayday

上面的shell代码是什么意思？