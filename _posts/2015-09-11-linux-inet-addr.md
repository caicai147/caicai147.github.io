---
layout: post
title: 解决linux使用ifconfig输出只用ipv6地址而没有ipv4地址的问题
categories: 好资源之常见问题解决
tags: linux red-hat ifconfig ip
---


我在虚拟机上安装完红帽之后，使用ifconfig命令来看网卡的IP，但是，输入命令之后，eht0里面只有 inet6 addr 而没有 inet addr。但是我接下来的环境搭建、配置工作必须这个IPv4的地址！

这个是什么情况？要怎么解决？

最后终于找到解决方法了：

方法1
---

使用`dhclient eth0`命令

输入`dhclient eth0`命令来获取IP地址

再输入`ifconfig`命令，就能看到eth0已经有IP地址了

但是有一个问题：每次重启系统之后，使用ifconfig命令又看不到IP地址了，必须每次都使用 dhclient eth0 命令重新分配，很不方便

方法2
---

`vim /etc/sysconfig/network-scripts/ifcfg-eth0`

将里面的ONBOOT属性设置为 yes

再执行`service network restart` 命令，就可以通过 ifconfig 查看eth0 的IP地址了

重点是这样以后每次系统重启之后，都能自动获取IP 地址。所以问题解决了。
