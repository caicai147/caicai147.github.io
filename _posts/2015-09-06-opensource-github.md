---
layout: post
title: 整理一些好的开源项目
categories: 好资源之开源项目
tags: 开源项目
---


首先说明，要想找开源项目，当然是Github！

另外oschina上面也可能有你要的，这个是分类的链接：[http://www.oschina.net/project/tags](http://www.oschina.net/project/tags)，比如这个是一个Python的相关开源资源：[http://www.oschina.net/project/lang/25/python](http://www.oschina.net/project/lang/25/python)

**但是不管是用Github还是oschina上面找，都要首先自己鉴别一下质量，不要随便轻信，最是去找那些知名度高的开源项目！如果自己直接去找，一定要注意项目的质量！**

**通过这些项目你可以大幅度减少不必要的开发而将精力放在更重要的地方。**

以下的整理会持续更新.....

别人整理好的
=======

Github资源收集：[http://segmentfault.com/a/1190000003510001?_ea=340118](http://segmentfault.com/a/1190000003510001?_ea=340118)

C/C++
=====

Linux(C;操作系统)
--------

这个就不用介绍了，地址是：[https://github.com/torvalds/linux](https://github.com/torvalds/linux)

mongodb(C++;数据库)
------------

MongoDB是一个基于分布式文件存储的数据库。由C++语言编写。旨在为WEB应用提供可扩展的高性能数据存储解决方案。

地址是：[https://github.com/mongodb/mongo](https://github.com/mongodb/mongo)

Leetcode OJ（算法）
-----------
    
这是一个算法练习的OJ。

C++的实现版本：[https://github.com/haoel/leetcode](https://github.com/haoel/leetcode)

Java的实现版本：[https://github.com/yuzhangcmu/LeetCode](https://github.com/yuzhangcmu/LeetCode)

muduo(C++;网络)
-------------

陈硕使用C++开发的一款网络编程的代码库。因为C++的一个著名的网络编程的代码库ACE存在很多的问题（参看博客《[学之者生，用之者死----ACE历史与简评](http://blog.csdn.net/solstice/article/details/5364096)》），所以陈皓自己编写了自己的C++网络库

地址：[https://github.com/chenshuo/muduo](https://github.com/chenshuo/muduo)

陈硕的博客：[http://blog.csdn.net/solstice/](http://blog.csdn.net/solstice/)

my_toy_compiler(C++)
-----------------------

一个大神用C++开发的一个“玩具”编译器

地址：[https://github.com/lsegal/my_toy_compiler](https://github.com/lsegal/my_toy_compiler)

SkyNet(C)
---------

云风大侠的成名作风魂，使用C语言编写，详细介绍参看云风的博客：[http://blog.codingnow.com/2012/09/the_design_of_skynet.html](http://blog.codingnow.com/2012/09/the_design_of_skynet.html)

地址：[https://github.com/cloudwu/skynet](https://github.com/cloudwu/skynet)

云风的博客：[http://blog.codingnow.com/](http://blog.codingnow.com/)

git(C)
------

这个就不用介绍了，版本控制工具

地址：[https://github.com/git/git](https://github.com/git/git)

GLib(C)
--------

是一个包含很多有用的 C 程序的开发包，例如树、哈希、列表等。GLib 之前是属于 GTK 工具包的一部分，现在独立出来成为单独项目。

C4(C;编译)
--------

4个函数实现的c编译器，大约500行。基本上已经比较完备了，可以自己编译自己。

SQLite(C;数据库)
--------

SQLite是遵守ACID的关联式数据库管理系统，它包含在一个相对小的C库中。它是D.RichardHipp建立的公有领域项目。SQLite亦可以作为桌面数据库使用。

Python
=====

pssh
------
提供了并行版本的 OpenSSH 工具，特别适合用来控制有大量机器需要连接的情况，包括并行版本的 ssh、scp、rsync 和 kill 命令。该项目是Python写的，代码清晰而简短（5000行左右），数据结构定义的很清楚，看了之后你会称赞的。
 
shadowsocks
-----------

shadowsocks是一个基于 python 的轻量级 socks 代理软件（谁用谁知道）。

Flask 
-----

Flask是一个使用 Python 编写的轻量级 Web 应用框架。其 WSGI 工具箱采用 Werkzeug ，模板引擎则使用 Jinja2 。

Flask号称微框架，0.1的代码才700来行(其中大部分都是注释) 而且代码写得很规范，非常适合学习。Django为了做到功能完备代码必然庞大而复杂不建议单纯的阅读。 

web.py
---------

官网：[http://webpy.org/](http://webpy.org/)

推荐文章：[web.py 十分钟创建简易博客](http://blog.csdn.net/caleng/article/details/5712850)

tornadoweb
------

简介：[http://www.tornadoweb.cn/](http://www.tornadoweb.cn/)

官网：[http://www.tornadoweb.org/en/stable/](http://www.tornadoweb.org/en/stable/)

requests 
-------

虽然Python的标准库中urllib2模块已经包含了平常我们使用的大多数功能，但是它的API使用起来让人实在感觉不好。它已经不适合现在的时代，不适合现代的互联网了。而Requests的诞生让我们有了更好的选择。

正像它的名称所说的，HTTP for Humans,给人类使用的HTTP库！在Python的世界中，一切都应该简单。Requests使用的是urllib3，拥有了它的所有特性，Requests 支持 HTTP 连接保持和连接池，支持使用 cookie 保持会话，支持文件上传，支持自动确定响应内容的编码，支持国际化的 URL 和 POST 数据自动编码。现代、国际化、人性化。

除此之外,Requests的文档非常完备，中文文档也相当不错。Requests能完全满足当前网络的需求，主要支持的功能如下：

   - 国际化域名和 URLs
   - Keep-Alive & 连接池
   - 持续性的 Cookie 会话
   - 类浏览器式的 SSL 加密认证
   - 基本/精简式的身份认证
   - 优雅的键/值 Cookies
   - 自动解压
   - Unicode 编码的响应主体
   - 多段文件上传
   - 连接超时
   - 支持 .netrc
   - 适用于 Python 2.6—3.3
   - 安全的线程使用

地址：[https://github.com/kennethreitz/requests](https://github.com/kennethreitz/requests)

PHP
===

typecho(PHP;博客)
------------

Typecho是由type和echo两个词合成的，来自于开发团队的头脑风暴。Typecho基于PHP5开发，支持多种数据库，是一款内核强健﹑扩展方便﹑体验友好﹑运行流畅的轻量级开源博客程序

地址是：[https://github.com/typecho/typecho](https://github.com/typecho/typecho)

官网：[http://typecho.org/](http://typecho.org/)

论坛：[http://forum.typecho.org/](http://forum.typecho.org/)

WordPress(PHP;博客)
-------

WordPress是一种使用PHP语言开发的博客平台，用户可以在支持PHP和MySQL数据库的服务器上架设属于自己的网站。也可以把 WordPress当作一个内容管理系统（CMS）来使用。WordPress是一款个人博客系统，并逐步演化成一款内容管理系统软件，它是使用PHP语言和MySQL数据库开发的。用户可以在支持 PHP 和 MySQL数据库的服务器上使用自己的博客。

地址是：[https://github.com/WordPress/WordPress](https://github.com/WordPress/WordPress)

中文官网：[http://cn.wordpress.org/](http://cn.wordpress.org/)

官网：[https://wordpress.org/](https://wordpress.org/)

ThinkPHP(PHP;框架)
------- 

ThinkPHP 是一个免费开源的，快速、简单的面向对象的 轻量级PHP开发框架 ，创立于2006年初，遵循Apache2开源协议发布，是为了敏捷WEB应用开发和简化企业应用开发而诞生的。ThinkPHP从诞生以来一直秉承简洁实用的设计原则，在保持出色的性能和至简的代码的同时，也注重易用性。并且拥有众多的原创功能和特性，在社区团队的积极参与下，在易用性、扩展性和性能方面不断优化和改进，已经成长为国内最领先和最具影响力的WEB应用开发框架，众多的典型案例确保可以稳定用于商业以及门户级的开发。

地址是：[https://github.com/liu21st/thinkphp](https://github.com/liu21st/thinkphp)

官网：[http://www.thinkphp.cn/](http://www.thinkphp.cn/)

 
Lisp
=======

compile-make
------

该项目可实现emacs中一键编译的需求。在项目目录下寻找Makefile文件，并执行make命令进行编译。
 

HTML/CSS/JS
=====

jQuery(JS;HTML;CSS)
-----

Jquery是继prototype之后又一个优秀的Javascript库。它是轻量级的js库 ，它兼容CSS3，还兼容各种浏览器（IE 6.0+, FF 1.5+, Safari 2.0+, Opera 9.0+），jQuery2.0及后续版本将不再支持IE6/7/8浏览器。jQuery使用户能更方便地处理HTML（标准通用标记语言下的一个应用）、events、实现动画效果，并且方便地为网站提供AJAX交互。jQuery还有一个比较大的优势是，它的文档说明很全，而且各种应用也说得很详细，同时还有许多成熟的插件可供选择。jQuery能够使用户的html页面保持代码和html内容分离，也就是说，不用再在html里面插入一堆js来调用命令了，只需要定义id即可

地址：[https://github.com/jquery/jquery](https://github.com/jquery/jquery)

bootstrap(JS;HTML;CSS)
------

Bootstrap，来自 Twitter，是目前最受欢迎的前端框架。Bootstrap 是基于 HTML、CSS、JS 的，它简洁灵活，使得 Web 开发更加快捷。它由Twitter的设计师Mark Otto和Jacob Thornton合作开发，是一个CSS/HTML框架。Bootstrap提供了优雅的HTML和CSS规范，它即是由动态CSS语言Less写成。Bootstrap一经推出后颇受欢迎，一直是GitHub上的热门开源项目

地址：[https://github.com/twbs/bootstrap](https://github.com/twbs/bootstrap)

Node.js(JS)
--------

node.js是由Ryan Dahl编写的服务器端js framework，其初衷是为了编写更为高效的web服务器。

地址：[https://github.com/joyent/node](https://github.com/joyent/node)

Koa
--------

Koa 是下一代的 Node.js 的 Web 框架。由 Express 团队设计。旨在提供一个更小型、更富有表现力、更可靠的 Web 应用和 API 的开发基础。

SeaJS
---------

Sea.js是一个遵循CommonJS规范的JavaScript模块加载框架，可以实现JavaScript的模块化开发及加载机制。Sea.js 追求简单、自然的代码书写和组织方式，代码非常精简。
 
Android
=====

关于Android的开源项目，请详见：[https://github.com/Trinea/android-open-project](https://github.com/Trinea/android-open-project)

目前主要包括：

- **Android 开源项目第一篇——个性化控件(View)篇**
包括ListView、ActionBar、Menu、ViewPager、Gallery、GridView、ImageView、ProgressBar、TextView、ScrollView、TimeView、TipView、FlipView、ColorPickView、GraphView、UI Style、其他
- **Android 开源项目第二篇——工具库篇**
包括依赖注入、图片缓存、网络请求、数据库 ORM 工具包、Android 公共库、高版本向低版本兼容库、多媒体、事件总线、传感器、安全、插件化、文件、其他
- **Android 开源项目第三篇——优秀项目篇**
比较有意思的完整的 Android 项目
- **Android 开源项目第四篇——开发及测试工具篇**
包括开发效率工具、开发自测相关、测试工具、开发及编译环境、其他
- **Android 开源项目第五篇——优秀个人和团体篇**
乐于分享并且有一些很不错的开源项目的个人和组织，包括 JakeWharton、Chris Banes、Koushik Dutta 等大牛

iOS
======

参看《github优秀开源项目大全-iOS》：[http://foggry.com/blog/2014/04/25/githubyou-xiu-xiang-mu-ios/](http://foggry.com/blog/2014/04/25/githubyou-xiu-xiang-mu-ios/)


