---
layout: post
title: Shell编程基础教程7--脚本参数的传递
categories: linux之shell
tags: linux shell 命令行 函数
---


7.1.shift命令
---

简介：`shift n`——每次将参数位置向左偏移n位

例子

    #!/bin/bash
    usage()
    {
        echo "usage:`basename $0` filenames"
    }
    totalline=0
    #下面的语句中 $# 表示参数的个数
    if [ $# -lt 2 ]
    then
        usage
    fi
    while [ $# -ne 0 ]
    do
        line=`cat $1 | wc -l`
    echo "$1:${line}"
    totalline=$ [ $totalline+$line ]
    shift
    done
    echo "---------"
    echo "total:${totalline}"

7.2.getopts
---

简介：获取多个命令行参数

例子

    #!/bin/bash
    ALL=false
    HELP=false
    FILE=false
    VERBOSE=false
    while getopts ahfvc: OPTION
    do
        case $OPTION in
        a)
            ALL=true
            echo "ALL is $ALL"
            ;;
         h)
             HELP=true
             echo "HELP is $HELP"
             ;;
          f)
              FILE=true
              echo "FILE is $FILE"
              ;;
          v)
              VERBOSE=true
              echo "VERBOSE is $VERBOSE"
              ;;
          c)
               echo "c value is $c"
               ;;
          \?)
               c=$OPTARG
               echo "`basename $0` - [ a h f v ] - [ c value ] file"
               ;;
           esac
    done
    
7.3.shift和getopts可以是传递的参数多于9个
---