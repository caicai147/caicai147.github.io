---
layout: post
title: Shell编程基础教程5--文本过滤、正则表达式、相关命令
categories: linux之shell
tags: linux shell 命令行 正则表达式
---


5.1.正则表达式
---

简介：

* 一种用来描述文本模式的特殊语法
* 由普通字符（例如字符a到z）以及特殊字符（成为元字符，如/、*、?等）组成
* 匹配的字符串
* 文本过滤工具在某种情况下都支持正则表达式

基本元字符集及其含义

    ^    只匹配行首，例子 ^a 表示匹配以a开头的
    $    只匹配行尾，例子 txt$ 表示匹配以txt结尾的
    *    匹配0或多个此单字符，
    []    只匹配[]内的字符。可以是一个单字符，也可以是字符序列。可以使用-表示[]内字符序列范围，如用[1-5]代表[12345]
    \    只用来屏蔽一个元字符的特殊含义，例子 \* 可以屏蔽*作为元字符的含义，使其就表示*字符本身，\$、\^等等例子
    .    只匹配任意单字符
    pattern\{n\}    只用来匹配前面pattern出现次数。n为次数
    pattern\{n,\}    含义同上，但是次数最少为n
    pattern\{n,m\}    含义同上，但是pattern出现次数在n与m之间

例子

    . 可以匹配任意单字符
        ...x..x..x
            drwxrwxrw-    不匹配
            -rw-rw-rw-    不匹配
            -rwxr-xr-x    匹配
            -rwxrwxrwx    匹配
        ....XC....
            3452XC763D    匹配
            4352XC433    不匹配
            3452XD7654    不匹配
    ^ 只允许在一行的开始匹配字符或单词
        ^d
            drwxrwxrw-    匹配
            -rwxrwxr-x    不匹配
        ^...1
            3482XC763D    不匹配
            23DDDDxs11    不匹配
            3451XD7633    匹配
            3D11XC9871    匹配
    $ 与 ^ 正相反，它在行尾匹配字符串或字符，$符号放在匹配单词后面
        trouble$    匹配以单词trouble结尾的所有字符
        ^$        匹配空行
        ^.$        匹配只含有一个字符的行
    * 可以匹配任意字符0次或多次重复表达
        10133*1
        101331        匹配
        10133921    匹配
        10133As1    匹配
    \ 用于屏蔽一个特殊字符
        特殊字符有"，'，||，^，*，+等等，元字符基本都是特殊字符
        \*\.pas        该正则表达式匹配以*.pas结尾的所有字符或文件
    
[] 用于匹配一个范围或集合

逗号将括号内要匹配的不同字符串分开

用 - 表示一个字符串范围，表明字符串范围从 - 左边字符开始，到 - 右边字符结束

例子

    [0123456789]或[0-9]        假定要匹配任意一个数字
    [a-z]    任意小写字母
    [A-Za-z]    任意大小写字母
    [A-Za-z0-9]        匹配任意字母或数字
    [S,s]    匹配大小写s
    \{\} 用于匹配模式结果出现的次数
    A\{2\}B        A出现2次，AAB
    A\{4,\}B    A至少出现4次，AAAAB、AAAAAB……
    A\{2,4\}B    A出此案次数为2到4次，AAB、AAAB、AAAAB
    [0-9]\{3\}\.[0-9]\{3\}\.[0-9]\{3\}\.[0-9]\{3\}    匹配IP地址

5.2.find介绍（文件查找）
---

简介

* 一个查找命令
* 查找具有某些特征文件的命令
* 可遍历当前目录甚至整个文件系统来查找某些文件或目录
* 遍历大的文件系统时，因为耗时比较长，一般放在后台执行，并将结果重定向到一个文件

命令格式

    find pathname -options [-print -exec -ok]
        pathname    find命令所查找的目录路径。例如用.表示当前目录，用/来表示系统根目录
        -print    find命令将匹配的文件输出到标准输出
        -exec    find命令将匹配的文件执行该参数所给出的shell命令，相应命令的形式为 command {} \; 。注意{}和\;之间的空格
        -ok和-exec的作用相同，只不过以一种更为安全的模式来执行该参数所给出的shell命令，在执行每一个命令之前，都会给出体术，让用户来确定是否执行

find的命令选项

* -name    按照文件名查找文件
* -perm    按照文件权限来查找文件
* -user    按照文件属主来查找文件
* -group    按照文件所属的组来查找文件
* -mtime -n +n    按照文件的更改时间来查找文件，-n表示文件更改时间距离现在n天以内， +n 表示文件更改时间距离现在n天以前。find命令还有-atime和-ctime选项，但是它们都和-mtime选项相似
* -size n[c]    查找文件长度为n块的文件，带有 c 时表示文件长度以字节计
* -nogroup    查找无有效所属组的文件，即该文件所述的组在/etc/groups中不存在
* -nouser        查找无有效数组的文件，即该文件的属主在/etc/passwd中不存在
* -newer "file1" ! -newer "file2"        查找更改时间比文件file1新但是比file2旧的文件
* -type    查找某一类型的文件，例如：

其中type具体解释如下

| type |      类型    |
| ---- | ----------- |
|  b   |  块设备文件   |
|  d   |      目录    |
|  c   |  字符设备文件 |
|  p   |    管道文件   |
|  l   |  符号链接文件 |
|  f   |    普通文件   |

`man find` 来查看更多更为详细的关于find命令的信息

示例：

    使用name选项
        可以使用某种文件名模式来匹配文件，记住要用引号将文件名模式引起来
            find -name "*.txt" -print        没写任何路径，就是默认当前路径
            find ./ -name "*.txt" -print
            find ./ -name "[A-Z]*" -print
            find /etc -name "host*" -print
    使用perm选项
        find ./ -perm 755 -print
    使用user和nouser选项
        find `pwd` -user root -print    
            反引号里的命令，`pwd`表示执行pwd命令获取的路径作为find查找的路径
        find / -nouser -print
        nohup find / -nouser -print > nouser.log    
            在后台执行（使用nohup）find命令，并将结果重定向到nouser.log文件中
    使用group和nogroup
        find ./ -group perfect -print
        find / -nogroup -print
    按照更改时间查找文件
        find /var -mtime -5 -print
        find /var/ -mtime +3 -print
    查找比某个文件新或旧的文件
        find `pwd` -newer "myfile" ! -new "myfile123" -print
    使用type选项
        find /etc -type d -print
        find /etc -type l -print
    使用size选项
        find ./ -size +1000000c -print    c表示单位是字符、字节
        find ./ -size +10 -print    没有c表示单位是块
    使用depth选项
        使用find命令时，可能希望先匹配所有的文件，再在子目录里查找
        find / -name "CON.FILE" -depth -print
    使用exec或ok来执行shell命令
        find ./ -type f -exec ls -l{} \;    
            查找本目录下的所有的普通文件，并逐个对查找出来的这些文件进行ls -l操作
        find ./ -name "*.log" -mtime +5 -ok rm{} \;    
            常用于删除一定天数之前的日志文件
    
### xargs命令

在使用find命令的-exec选项处理匹配到的文件时，find命令将所有匹配到的文件一起传递给exec。不幸的是，有些系统对能够传递给-exec的命令长度有限制，这样在find命令运行几分钟之后，就会出现一处错误。错误信息通常是“参数列太长”或者“参数列溢出”。这就是xargs命令的用处所在，特别是与find命令一起使用。-exec会发起多个进程，而xargs不会多个，只用一个，就会减少系统资源的消耗

    find ./ -perm -7 -print | xargs chmod o-w    
        xargs的作用是将 find ./ -perm -7 -print 的结果传给 chmod o-w 命令来进行执行
    find ./ -type f print | xargs file

5.3.grep介绍（文本过滤）
---

简介：

* grep是Unix和Linux中使用最广泛的命令之一
* 对文本文件进行模式查找
* 对文本进行按行的匹配

grep有三种变形

* Grep：标砖grep命令
* Egrep：扩展grep，支持基本以及扩展的正则表达式
* Fgrep：快速grep

grep一般格式：`grep [选项] 基本正则表达式 [文件]`

其中基本正则表达式最好采用双引号括起来，一是防止被误认为是shell命令，二是可以用来查找多个单词组成的字符串
 
grep命令选项

* -c    只输出匹配行的计数
* -i    不区分大小写（只适用于单字符）
* -h    查询多文件时不显示文件名
* -H    查询多文件时显示文件名
* -l    查询多文件时只输出包含匹配字符的文件名
* -n    显示匹配行及行号
* -s    不显示不存在或无匹配文本的错误信息
* -v    显示不包含匹配文本的所有行，功能就是过滤某些文本

例子

    grep "jenny" *.txt    从所有 txt 文件中查找存在 jenny 的行有哪些
    grep "sort it" *    在所有文件中查找存在 sort it 的行有哪些
    grep -c "2004" myfile    输出 myfile 中有多少行匹配到 2004
    grep -n "2004" myfile    输出 myfile 中匹配 2004 的行及行号
    grep -i "Jul" myfile    输出 myfile 中匹配 Jul 的行，Jul 不区分大小写
    grep -v "2004:22" myfile    过滤掉myfile中含有 2004:22 的行，只输出不含有 2004:22 的行
    grep "2004:22:5[0-9]" myfile    输出 myfile 中含有 2004:22:50 到 2004:22:59 的行
    grep "^[^210]" myfile    输出 myfile 中开头不是 2 也不是 1 也不是 0 的行
    grep "H*P" myfile    输出匹配正则表达式 H*P 模式的行
    grep "[5-8][6-9][0-3]" myfile
    grep "4\{2\}" myfile
    grep "4\{2,\}" myfile
    grep "4\{2,4\}" myfile    输出匹配正则表达式 4\{2,4\} 模式的行，该模式的含义是 4 连续出现的次数是 2到4 次
    grep "^$" myfile    输出匹配正则表达式 ^$ 模式的行，^$ 的含义是空行
    grep "\?" myfile    \? 转义 ?
    grep "^d" lsout.txt        ^d 表示以 d 开头
    grep "^[^d]" lsout.txt        ^[^d] 表示不以d开头

grep命令类名和等价的正则表达式

    [[:upper:]]        [A-Z]
    [[:alnum:]]        [0-9a-zA-Z]
    [[:lower:]]        [a-z]
    [[:space:]]        空格或tab键
    [[:digit:]]        [0-9]
    [[:alpha:]]        [a-zA-Z]
            
例子

    grep "5[[:digit:]][[:digit:]]" myfile    其中的正则表达式表示 500 到 599 的数字范围


5.4.awk介绍
---

简介：

* 可从文件或字符串中基于指定规则浏览和抽取信息
* 是一种自解释的编程语言

三种方式调用awk：
 
* 命令行方式：awk [-F filed-spearator] 'command' input-files
* awk脚本：所有awk命令插入一个文件，并令awk程序可执行，然后用awk命令解释器作为脚本的首行，以便通过键入脚本名称来调用它
* awk命令插入一个单独文件：awk -f awk-script-file input-files

awk脚本由各种操作和模式组成

模式和动作：

* 模式部分决定动作语句何时触发及出发事件（BEGIN，END）
* 动作对数据进行处理，放在大括号{}内指明（print）

分隔符、域和记录

* awk执行时，其浏览域标记为$1、$2....$n。这种方法称为域标识。$0为所有域
* 例如一行数据 2005 computer is expensive ，默认分隔符是 空格，那么这一行就有4个域，第一个是 2005，第二个是 computer，第三个是 is，第四个是 expensive
* 注意执行时不要混淆符号 $ 和 shell提示符 $ ，它们是不同的

例子1：

    awk '{print $0}' score.txt | tee score.out        
        将score.txt文件中的所有信息打印出来，在屏幕上输出信息，并且也将信息输入到score.out文件中
    awk -F : '{print $1"\t"$4}' score.txt        
        注意 -F : 作用是将分隔符从默认分隔符 空格 变成 : ，这条命令表示在屏幕上输出 score.txt 文件中以:为分隔符分割后每行中第一个域和第四个域的内容，
        第一个域和第四个域的内容在输出信息中以 \t 也就是 tab跳隔符分割，在输出域的时候，分隔符:不会输出
    awk 'BEGIN {print "IP Date\n--------"}{print $1"\t"$4}END{"end-of-report"}' score.txt    
        BEGIN {print "IP Date\n--------"}打印一些报告头，
        END{"end-of-report"}打印一些报告尾，{print $1"\t"$4}打印在score.txt中模式匹配的主体信息

awk特殊字符

* awk中的特殊字符：+、?，+表示匹配任意字符，?表示匹配单个字符
* 匹配操作符：~、!~，~表示匹配，!~表示不匹配

例子2：

    cat score.txt | awk '$0~/218.79.13.196/'    
        在score.txt中查找出所有匹配 218.79.13.196 的行，注意匹配模式放在//里面
    awk '$1!~/218.79.13.196/' score.txt        
        在score.txt中，以默认分隔符空格来划分每一行，输出第一个域不匹配(!~)218.79.13.196的行
    awk '{if($1=="218.79.131.96") print $0}' score.txt        
        在score.txt中，以默认分隔符空格来划分每一行，如果(if)第一个域($1)等于(==)218.79.13.196，就打印这一行的所有信息(也就是这一行的所有域$0)

5.5.sed介绍
---

简介：

* sed不与初始化文件打交道，也就是绝对不会更改原文件的内容，它操作的只是一个拷贝，然后所有的改动如果没有重定向到一个文件，将输出到屏幕
* sed是一个重要的文本过滤工具，使用一行命令或者使用管道与grep与awk相结合
* 非交互性文本流编辑

调用sed的三种方式

* 使用sed命令行格式为：sed [选项] sed命令 输入文件
* 使用sed脚本文件格式为：sed [选项] -f sed脚本文件 输入文件
* sed脚本文件 [选项] 输入文件

补充：不管是使用shell命令行方式或是脚本文件方式，如果没有指定输入文件，sed从标准输入中接受输入，一般是键盘或重定向结果

sed命令选项如下：

* -n    不打印没匹配到的行
* -c    下一命令是编辑命令
* -f    如果正在调用sed脚本文件

sed在文件中查询文本的方式

    使用行号，可以是一个简单数字，或是一个行号范围
        x        x为一行好
        x,y        表示行号范围从x到y
    使用正则表达式
        /pattern/        查询包含模式的行
        /pattern/pattern/    查询包含两个模式的行
        /pattern/,x        在给定行号上查询包含模式的行
        x,/pattern/        通过行号和模式查询匹配行，从第x行开始一直匹配到有/pattern/的行结束
        x,y!            查询不包含执行行号x和y的行
        
基本sed编辑命令

* p    打印匹配行
* =    显示文件名
* a\    在定位行号后附加新文本信息
* i\    在定位行号前附加新文本信息
* d    删除定位行
* c\    用新文本替换定位文本
* s    使用替换模式替换相应模式
* r    从另一个文件中读文本
* w    写文本到一个文件
* q    第一个模式匹配完成后退出或立即退出
* l    显示与八进制ASCII代码等价的控制字符
* {}    在定位行执行的命令组
* n    从另一个文件中读文本下一行，并附加在下一行
* g    将模式2粘贴到/pattern n/
* y    传送字符


例子：

    sed '2p' score.txt        
        基本sed编辑命令p表示打印匹配行，所以这句就是打印第2行，但是没有-n(-n指的是不打印没匹配的行)，所以还是会打印所有的行
    sed -n '2p' score.txt        
        基本sed编辑命令p表示打印匹配行，而且还有-n参数，所以这句就是打印第2行
    sed -n '1,4p' score.txt        
        基本sed编辑命令p表示打印匹配行，所以 1,4p 表示打印第1到第4行，-n表示不打印不匹配的行
    sed -n '/los/p' myfile.txt        
        -n表示不打印不匹配的行，所以就是打印匹配 los 字符串的行，以为/los/后有p编辑命令，就是打印匹配行
    sed -n '4,/los/' myfile.txt
    sed -n '/^$/=' myfile
    sed -n -e '/^$/p' -e '/^$=' myfile
    sed -n '/chinaitlab/a\shenzhen' myfile.txt
    sed -n '/chinaitlab/i\shenzhen' myfile.txt
    sed -n '/chinaitlab/c\chinaitlab shenzhen' myfile.txt
    sed '1,2d, myfile.txt        删除1到2行的内容
    sed 's/chinaitlab/chinaitlab shenzhen/g' myfile.txt
    sed -n 's/chinaitlab/& hello/p' myfile.txt        在有chinaitlab的行前面插入 hello
    sed -n 's/chinaitlab/hello &/p' myfile.txt        在有chinaitlab的行后面插入 hello
    sed 'lr ctrl.txt' myfile.txt
    sed '/china/q' myfile.txt
    sed -n '/china/l' myfile.txt

5.6.合并与分割（sort、uniq、join、cut、paste、split）
--

### sort [options] files
         
许多不同的域按照不同的列顺序排序

* -c    测试文件是否已经分类
* -m    合并两个分类文件
* -u    删除所有复制行
* -o    存储sort结果的输出文件名
* -t    域分隔符；用非空格或tab键分隔域
* +n    n为域名，使用此域名开始分类，sort中的域是从0开始的，不同于awk（awk的域是从1开始的，0表示所有域）
* n    指定分类是域上的数字分类项
* -r    比较逆序

例子：

    sort -c myfile        检查myfile有没有排序
    sort -u myfile        如果myfile有重复的行，就合并这些行为一行
    sort -r myfile        以逆序排序
    sort -t "/" +2 myfile        以/作为分隔符，并以第2个域来排序
    sort -t "/" +2n myfile        
        以/作为分隔符，并以第2个域按数字排序，+2n中的 n 就是指按数字排序，如果没有n，那么173排在19的后面，
        因为是逐个比较每个字符，如果有n，就是比较数字173和19的大小来排序

### uniq [options] files

* 从一个文本文件中去除或禁止重复行，关于重复的行，统计只对邻近的行有效
* -u    只显示不重复行
* -d    只显示有重复数据行，每种重复行只显示其中一行
* -c    打印每一重复行出现次数
* -f n    n为数字，前n个域被忽略，只比较第n以及第n之后的域，如果某一行域的个数少于n，就不输出该行

例子：

    uniq -c myfile.txt        
    uniq -d myfile.txt
    uniq -f 2 myfile.txt
    uniq -d myfile.txt

### join [options] file1 file2

用来讲来自两个分类文本文件的行连接在一起

* -an        n为数字，用于连接时从文件n中显示不匹配行
* -o n.m     连接域，n为文件名，m为域名
* -j n m    n为文件名，m为域名。使用其他域做连接域
* t        域分隔符，用来设置非空格或tab键的域分隔符

例子

    join -a1 -a2 address.txt town
    join -o 2.2,1.1    address.txt town
    join -j1 1 -j2 1 address.txt town
        
### split

用于将大文件分割成小文件

命令格式:`split -output_file-size input-filename output-filename`

* -b n    每个分割文件的大小为n(k,m)
* -C n    每个分割文件一行最多n个字节数
* -l n    每个分割文件的行数
* -n        同-l n

例子

    split -10 ls_out.txt split

### cut

用于从标准输入或文本文件中剪切列或域


### paste

将按行将不同文件行信息放在一行