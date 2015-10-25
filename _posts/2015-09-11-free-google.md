---
layout: post
title: 免费访问google
categories: 好资源之学习资源
tags: google
---


更多方法看我之前问过的一个问题：[http://segmentfault.com/q/1010000003737196?_ea=349656](http://segmentfault.com/q/1010000003737196?_ea=349656)

下面只讲其中一个需要比较多步配置的方法
======================

[http://igge.gq/](http://igge.gq/)的左边的“谷歌Hosts[可用]”链接上可以看到实时更新的hosts，这里面会最新的谷歌hosts（文中会有链接），注意实时更新自己的hosts文件。
或者是去看这个网站上的更新的hosts：http://laod.cn/hosts/2015-google-hosts.html

注意：这个链接有时候打不开，但是过一段时间就好了

注意自己的公司的电脑、自己的电脑的windows和linux都要实时更新hosts文件，再也不用百度了！！！

linux
--------

**这样是不行的**：使用超级用户身份去更改` /etc/hosts`文件，添加最新的谷歌hosts

**这样才可以**：使用普通用户执行`sudo gedit /etc/hosts`编辑，然后添加最新的谷歌hosts，然后就可以访问了google了，再不行就去重启系统，注意实时更新。

windows
--------

右键-->以管理员身份打开记事本，然后将最新的谷歌hosts拷贝到里面，然后保存到`C:\windows\system32\drivers\etc`，保存为`hosts`，替换原来的`hosts`，注意没有后缀。然后就可以访问了google了。

特别强调，不能先直接去`C:\windows\system32\drivers\etc`，打开`hosts`文件编辑，否则没办法保存，因为必须要用超级用户保存。

还要说明一下，有时候我用管理员身份打开记事本，但是还是无法保存，然后等到下次重启机器之后再用同样的方式就可以了

是在不知道windows的设计为什么这么逆天、反人类！

假如哪天发现自己突然没办法访问google了，就去[http://igge.gq/](http://igge.gq/)找新的hosts，更改自己的hosts文件！！

比如在2015.10.21，Ｇｏｏｇｌｅ访问不了，于是就去[http://igge.gq/](http://igge.gq/)找新的hosts，更新本地的hosts就可以了