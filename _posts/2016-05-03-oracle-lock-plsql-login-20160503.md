---
layout: post
title: 正确理解C/C++中的传值调用/传址调用/引用调用
categories: c/c++之函数 c/c++之指针与内存 
tags: c c++ 函数 指针 内存
---

##背景

* 人员管理系统，使用Oracle数据库
* 数据库中有一张表tuser用于存储用户信息，有以下字段uname(用户名)、uno(用户编号)、upwd(用户密码)、ulogintime(上次登录时间)
* 如果在PLSQL中只写这个语句select * from tuser a where a.uno = 1000 for update
* 在PLSQL中没有Commit或者RollBack的情况下，再去人员管理系统的前台界面，使用1000这个用户进行登录，就会出现登录卡死的情况
* 因为PLSQL中得到会话执行这条语句且在没有Commit的情况下，将a.uno=1000的记录进行加锁
* 而前台登录的时候也要对其加锁，比如为了修改ulogintime字段就要加锁
* 但是因为PLSQL中已经对其加锁，所以在系统的前台登录时无法获取锁，就只能等待，所以就表现为登录卡死的现象

##测试1：或者可以这样的方式以模拟这种情况

* 使用PLSQL登录Oracle，File-->New-->SQL Window，在这个编辑框(Window 1)中输入：`select * from tuser a where a.uno = 1000 for update;`，执行
* 再去File-->New-->SQL Windows，在这个编辑框(Window 2)中输入同样的SQL：`select * from tuser a where a.uno = 1000 for update;`，执行
* 可以发现Window 2中的执行卡住了，无法输出查询结果
* 这时候回到Window 1点击Commit或者RollBack按钮，再回到Window 2，发现已经将查询结果显示出来了

##测试2：还可以继续尝试一种场景

* 使用PLSQL登录Oracle，File-->New-->SQL Window，在这个编辑框(Window 1)中输入：`select * from tuser a where a.uno = 1000 for update;`，执行
* 再去File-->New-->SQL Windows，在这个编辑框(Window 2)中输入SQL：`select * from tuser a where a.uno = 1000;`，(只是读，不写)，执行
* 可以发现Window 2正确输出查询结果，并没有卡住，和上面的测试1的效果不同
* 因为在Window 1中对这条记录进行加锁了，但是在Window 2中只是去读，并不要获取写锁，所以就不会等待，不会出现卡住的情况
* 所以上面的“背景”中描述的场景，PLSQL对其加写锁，而使用系统进行登录时并不只是去读取信息，而是也要尝试写锁，所以就会等待出现卡死的情况

类似的关于Oracle死锁的问题可以参见这篇博客[Oracle数据表死锁的解决方法](http://www.xumenger.com/oracle-deadlock-20160218/)


##分析

上述语句存在2个问题：

* 当我们在程序中查询一条记录时，不允许使用 select * into from 这种写法，即使要查询该记录的所有字段，也应该把字段名都列出
	* 因为当给你要查询的表新增一个字段后，此语句很可能在编译阶段不会报错，而是在运行时报错，问题很难排查，比如这个是一个动态执行的语句；
	* 当from后面是多表关联时，很可能出现2个表都是同样字段，如果编译阶段未发现，也会导致问题很难排查
	* 这部分就属于开发、编码的规范性范畴了
* 如果是使用 `select xx into yy from ttt where ... for update;`时，一定是要再加上nowait，即写成：
	* `select xx into yy from ttt where ... for update nowait;`，并同时捕获异常 ora-00054(pro*c里判断sqlcode=54)
	* 这样的好处：当其他会话将你要锁定的记录加锁以后，你的程序里能很快响应，避免无限时的等待(前端表现为卡死)
	* 而且根据错误号，你可以明确提示前端是什么问题导致的、应该如何处理。

##测试3：再去而是使用 no wait

* 以上面的例子为例：如果在PLSQL中执行这个语句：`select * from tuser a where a.uno = 1000 for update;`
	* 在没有commit的情况下，去前台使用该用户登录，就会出现登录卡死的情况
	* 如果有nowait，只要判断返回的是54，就可以提示操作员有其他人在操作本账号，要么等待再重试，要么就怀疑账号泄露要求锁定等等操作
* 在PLSQL中进行模拟
	* 使用PLSQL登录Oracle，File-->New-->SQL Window，在这个编辑框(Window 1)中输入：`select * from tuser a where a.uno = 1000 for update;`，执行
	* 再去File-->New-->SQL Windows，在这个编辑框(Window 2)中输入SQL：`select * from tuser a where a.uno = 1000 for update nowait;`，执行
	* 可以看到在Window 2中并没有出现卡死等待的情况，而是会弹出警告框
	* 提示信息是：ORA-00054:资源正忙，但指定以NOWAIT方式获取资源，或者超时失效
	* 同时并没有任何结果输出
