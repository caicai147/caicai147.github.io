---
layout: post
title: Linux Shell 高级编程技巧1----深入讨论（awk、<<）
categories: linux之shell
tags: linux shell 命令行 awk 
---


1.1.深入讨论awk
---

### 记录和域，模式和动作，正则表达式和元字符

基础教程中已经介绍

### 条件和逻辑操作符

* <    小于
* \>=    大于等于
* <=    小于等于
* ==    等于
* !=    不等于
* ~    匹配正则表达式
* !~    不匹配正则表达式
* &&    and
* ||    or
* !    not
            
例子(注释：www.log是apache的一个日志文件，这是在日常生活中很常遇见的)

    #!/bin/bash
    echo "210-219网段的访问量是：`awk '{if ($1~/^21[0-9]/) print $0}' www.log | wc -l`"
    echo "非210-219网段的访问量是：`awk '{if ($1!~/^21[0-9]/) print $0}' www.log | wc -l`"
    echo "2004年07月07日的访问量是：`awk '{if ($4~/^\[07\/Jul\/2004/) print $0}' www.log | wc -l`"
    echo "2004年07月07日/htm/free_call.php的访问量是：`awk '{if ($4~/^\[07\/Jul\/2004/) print $0}' www.log | awk '{if ($7=="/htm/free_call.php") print $0}' | wc -l`"

### awk内置变量

内置变量

* ARGC        命令行参数个数
* ARGV        命令行参数排列，是一个数组，用ARGV[0]、ARGV[1]……的方式
* ENVIRON        支持队列中系统环境变量的使用
* FILENAME    awk浏览的文件名
* FNR            浏览文件的记录数
* FS            设置输入域分隔符，等价于命令行参数-F选项
* NF            浏览记录的域个数
* NR            已读的记录数
* OFS            输出域分隔符
* ORS            输出记录分隔符
* RS            控制记录分隔符

例子：

    awk -F '#' '{print NF,NR,$0}' grade.txt
    awk -F '#' '{print NF,NR,ENVIRON["USER"],$0}' grade.txt
    awk -F '#' '{print NF,NR,ENVIRON["USER"],$0,FILENAME}' grade.txt
    awk -F '#' '{print NF,NR,ENVIRON["USER"],$0,FILENAME,ARGC}' grade.txt
    awk -F '#' '{print NF,NR,ENVIRON["USER"],$0,FILENAME,ARGC,ARGV[0]}' grade.txt

补充：grade.txt文件内容

    85#senior
    87#junior
    75#senior
    79#senior
    92#senior
    23#junior

### 内置的字符串函数

内置字符串函数

* gsub(r,s)        在整个$0中用s替代r
* gsub(r,s,t)        在整个t中用s替代r
* index(s,t)        返回s中字符串t的第一个位置
* length(s)        返回s的长度
* match(s,r)        测试s是否包含匹配r的字符串
* split(s,a,fs)    在fs上将s分成序列a
* sprint(fmt,exp)    返回经fmt格式化后的exp
* sub(r,s)        用$0中最左边最长的子串代替s
* substr(s,p)        返回字符串s中从p开始的后部分
* substr(s,p,n)    返回字符串s中从p开始长度为n的后部分

例子

    awk -F '#' '{if (gsub("#","||")) print $0}' grade.txt
    awk -F '#' '{if (gsub("s","S",$1)) print $0}' grade.txt
    awk -F '#' '{print (index($2,"e"))}' grade.txt

### awk转义字符

转义字符

* \b        退格键
* \t        tab键
* \f        走纸换页
* \ddd    八进制值
* \n        新行
* \c        任意其他特殊字符，例如\\为反斜线符号
* \r        回车键

例子

    awk -F '#' '{print (index($2,"s")),"\t",$2}' grade.txt
    awk -F '#' '{print (index($2,"s")),"\n",$2}' grade.txt

### printf修饰符

类似于C语言的printf函数

printf修饰符

* %c    ASCII字符
* %d    整数
* %f    浮点数，例如(123.44)
* %e    浮点数，科学计数法
* %f    新行
* %g    awk决定使用哪种浮点数转换e或者f
* %o    八进制数
* %s    字符串
* %x    十六进制数

例子

    awk -F '#' '{printf "%c\n",$1}' grade.txt
    awk -F '#' '{printf "%c\t%d\n",$1,$1}' grade.txt
    awk -F '#' '{printf "%c\t%f\t%d\n",$1,$1,$1}' grade.txt
        
### awk数组

简介:awk数组下标是从 1 而不是 0 开始

例子
    
    awk 'BEGIN {print split("as#qw#1234",array2,"#")}'
    awk 'BEGIN {print split("as#qw#1234",array2,"#"); print array2[1]}'
    awk 'BEGIN {print split("as#qw#1234",array2,"#"); print array2[1],"\t",array2[2],"\t",array2[3]}'

### awk的脚本例子(awk_array.sh 文件)

执行命令:`./awk_array.sh grade.txt`

    #!/bin/awk -f
    BEGIN{
        FS="#"
        score["0-60"]=0
        score["60-70"]=0
        score["70-80"]=0
        score["80-90"]=0
        score["90-100"]=0
        student["junior"]=0
        student["senior"]=0
    }
    {
        {
            if [ $1<60 ]
            score["0-60"]++
        }
        {
            if [ $1<70 ] && [ $1>=60]
            score["60-70"]++
        }
        {
            if [ $1<80 ] && [ $1>=70 ]
            score["70-80"]++
        }
        {
            if [ $1<90 ] && [ $1>=80]
            score["80-90"]++
        }
        {
            if [ $1<=100 ] && [ $1>=90 ]
            score["90-100"]++
        }
    }
    {
        for senior_junior in student
        {
            if [ $2==senior_junior ]
            student[senior_junior]++
        }
    }
    END{
            for number in score
                print "The score",number,"has",score[number],"students"
        }
        {
            for senior_junior in student
                print "The class has",student[senior_junior],senior_junior,"stuents"
        }
    }

1.2.深入讨论<<
---