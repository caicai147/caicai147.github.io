---
layout: post
title: Linux Shell 高级编程技巧3----运行级别脚本介绍
categories: linux之shell
tags: linux shell 命令行
---


3.1.运行级别
---

###运行级别介绍###

* 0    关机
* 1    单用户模式
* 2    多用户模式，没有NFS服务
* 3    多用户模式
* 4    目前还没有使用
* 5    X windows 的启动模式
* 6    重启计算机

###运行级别对应的目录###

/etc/rcN.d，N是数字0、1、2...

###当前运行级别###

用runlevel目录获取当前运行级别（运行级别从0开始）

###运行级别目录文件格式###

X是阿拉伯数字，script是脚本名称

* SXXscript：表示在这种运行级别下面，这个脚本的服务是启动的
* KXXscript：表示在这种运行级别下面，这个脚本的服务是不启动的

例如在/etc/rc5.d文件中用 ls 查看里面的文件，有K01kdcrotate、K15httpd、S80isc...文件。这些文件都是超链接文件，链接到 /etc/init.d/ 下面对应的文件，例如K01kdcrotate链接文件对应的是 /etc/init.d/ 下面的 kdcrotate 文件；S80isc 链接文件对应的是 /etc/init.d/ 下面的 isc 文件

###启动服务的顺序###

3.2.inittab启动文件
---

###运行级别控制文件###

/etc/inittab

该文件是linux系统启动时执行的脚本

###编辑inittab文件###

使用`vim /etc/inittab` 命令可以打开该文件

例子：在inittab文件中添加一条语句，使得系统启动时候就能执行检查系统磁盘情况的shell脚本文件checkdisk.sh

`checkdisk::once:/sbin/checkdisk.sh > /dev/console 2>&1`    表示在每个运行级别中都执行这条命令

如果要制定在第三运行级别可以这样：`checkdisk:3:once:/sbin/checkdisk.sh > /dev/console 2>&1`

3.3.启动应用程序。
---

通过使用启动脚本来使应用程序自动开机启动，不用再手动启动

###启动脚本分析###

start|stop|restart

cat /etc/init.d/crond

###以MySQL应用程序为例###

MySQL有自己的mysqld脚本文件，在安装MySQL应用程序的时候，将mysqld放到/etc/init.d/目录下，再在相应的运行级别下面做相应的KXXscript或SXXscript命名格式的超链接

###启动脚本###

方法1：`service server start|stop|restart|...`        server是指脚本的名字

例子：`service mysald start`

方法2：`script_name start|stop|restart`

创建自己的启动脚本的方法（可以使用这样的方法，比如来启动自己开发的C应用程序）

1. 在/etc/init.d/目录下创建自己的启动脚本文件，参看 /etc/init.d/ 下面已有的启动脚本文件的语法格式来编写自己的启动脚本
2. 在/etc/rcN.d/目录下，也就是在相应的运行级别中用SXXscript或者KXXscript的格式创建对应启动脚本的链接