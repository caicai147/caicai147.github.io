---
layout: post
title: Shell编程基础教程3--输入与输出
categories: linux之shell
tags: linux shell 命令行 io
---


3.1.echo
---

echo命令可以显示文本行或变量，或者把字符串输出到文件

    echo [option] string
    -e：解析转义字符
    -n：回车不换行，linux系统默认回车换行

转义字符：\c，\f，\t，\n……

例子程序

    #!/bin/bash
    #echo
    echo -e "This echo's 3 new lines\n\n\n"
    echo "OK"
    echo
    echo "This echo's 3 new lines \n\n\n"
    echo "This log files have all been done">mylogfile.txt

3.2.read
---

read语句可以从键盘或文件的某一行文本中读入信息，并将其赋值给一个变量

    read varible1 varible2...

如果只指定了一个变量，那么read将会把所有的输入赋值给该变量，直至遇到第一个文件结束符或回车；如果给出了多个变量，它们按顺序分别被赋予不同的变量。shell将用空格作为变量之间的分隔符

例子程序

    #!/bin/bash
    #read
    echo -n "First Name:"
    read firstname
    echo -n "Sub Name and Last Name:"
    read subname lastname
    echo -e "Your First Name is: ${firstname}\n"
    echo -e "Your Sub Name is: ${subname}\n"
    echo -e "Your Last Name is: ${lastname}\n"

3.3.cat
---

cat是一个简单而通用的命令，可以用它来显示文件内容、创建文件，还可以用它来显示控制字符

    cat [options] filename1 ... filename2 ...
    -v：显示控制字符

使用cat命令式要注意，它不会在文件分页符处2停下来；它会一下显示完整个文件。如果希望每次显示一页，可以使用more命令或者把cat命令的输出通过管道传递到另外一个具有分页功能的命令（more、less）中

3.4.管道  |
---

可以通过管道把一个命令的输出传递给另一个命令作为输入。管道用竖杠|表示

    格式：命令1 | 命令2

例子： `ls -l | grep "myfile"`

3.5.tee
---

tee命令把输出的一个副本送到标准输出，另一个副本拷贝到相应的文件中
    tee -a files

如果希望在看到这个输出的同时，也将其存入一个文件，那么这个命令再适合不过了

一般用于管道之后。例子：ls | tee -a ls.txt    既将ls的命令结果输出到标准输出，又输出到ls.txt文件中

3.6.标准输入、输出和错误
---

在shell中执行命令时，每个进程都和三个打开的文件相联系，并使用文件描述符来引用这些文件。由于文件描述符不容易记忆，shell同时给出了相应的文件名

* 输入文件--标准输入：0（缺省是键盘，也可以是文件或其他命令的输出）
* 输出文件--标准输出：1（缺省是屏幕，也可以是文件）
* 错误输出文件--标准错误：2（缺省是屏幕，也可以是文件）

系统中实际上有12个文件描述符，可以任意使用文件描述符3~9

3.7.文件重定向
---

改变程序运行的输入来源和输出地点

* command > filename：把标准输出重定向到一个新文件中
* command >> filename：把标准输出重定向到一个文件中（追加）
* command 1> filename：把标准输出重定向到一个文件中
* command > filename 2>&1：把标准输出和标准错误一起重定向到一个文件中
* command 2> filename：把标准错误重定向到一个文件中
* command 2>> filename：把标准错误重定向到一个文件中（追加）
* command >> filename 2>&1：把标准输出和标准错误一起重定向到一个文件中（追加）
* command < filename1 > filename2：command命令以filename1文件作为标准输入，以filename2作为标准输出
* command < filename：command命令以filename文件作为标准输入
* command << delimiter：从标准输入中读入，直至遇到delimiter分解符
* command <&m：把文件描述符m作为标准输入
* command >&m：把标准输出重定向到文件描述符m中
* command <&-：关闭标准输入

例子：

`cat file } sort > sort.out`    将file的字符串（按每行）排序，并将排序结果输出到sort.out文件中

`>nullfile`    创建一个空文件nullfile

`sort < name.txt > name.out`    将name.txt的内容作为输入，排好序后，将结果输出到name.out

`cat file1 file2 1> file.txt 2> file.err`    将cat file1 file2命令的标准输出结果输出到file.txt，将该命令的标准错误的输出结果输出到file.err，分开输出（测试时，file1存在，file2不存在）

3.8.合并标准输出和标准错误
---

合并标准输出和标准错误的时候，切记shel是从左至右分析相应的命令的

例子：`grep "example" example.txt > grep.out 2>&1`    在example.txt里面查找"example"字符串的内容，标准输出输出到grep.out文件，并将标准错误也输出到grep.out文件

3.9.exec
---

exec命令可以用来替代当前shell；换句话说，并没有启动子shell，使用这一命令时任何现有环境都将被清除，并重启一个shell

exec command：其中的command通常是一个shell脚本

对文件描述符进行操作的时候（也只有在这个时候），它不会覆盖你当前的shell