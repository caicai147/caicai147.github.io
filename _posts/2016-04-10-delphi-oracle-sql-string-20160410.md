---
layout: post
title: Delphi中字符串比较大小 VS Oracle-SQL中字符串比较大小
categories: delphi之字符串 数据库之oracle
tags: delphi 字符串 ascii 字符编码 oracle
---

##Delphi中的字符串比较

* Delphi中字符串比较大小的规则：对应位置的字符按照字符编码值逐个对比，直到遇到可以确定大小关系的就结束比较
* 参考[《Delphi的Ord函数和ASCII码对照表》](http://www.xumenger.com/delphi-ord-20160222/)
* '1'对应字符编码49；'A'对应字符编码65；'a'对应字符编码97
* 比如'abcd' 和 'abd'比较
  * 首先第一位'a'和'a'比较，一样，继续下面的比较
  * 接着第二位'b'和'b'比较，一样，继续下面的比较
  * 接着第三位'c'和'd'比较，'c'<'d'，所以'abcd'<'abd'，结果出来！比较结束
* 列举出常见的比较
  * '101'<'12'为True
  * '101'<'102'为True
  * 'abcd'>'abc'为True，这个需要注意
  * '12345'>'1234'为True，这个需要注意
  * '1'>''，在Delphi中任意为空字符串都大于''

##Oracle SQL中的字符串比较

* 亲自在Oracle中验证过！
* 一般在SQL中的where条件中会用到字符串的比较
* 首先特别注意''，和Delphi有一个很大的不同
  * 任何字符串都不能和''进行比较，因为比较结果既不为True也不为False
  * 如果在select的where判断中有和''的比较会导致选不出任何一条记录
* 除了小心''之外，其余同Delphi：对应位置的字符按照字符编码值逐个对比，直到遇到可以确定大小关系的就结束比较
* 几种常见的字符对应的编码
  * select ascii('1') from dual;       字符编码值 49
  * select ascii('A') from dual;       字符编码值 65
  * select ascii('a') from dual;       字符编码值 97
* 列举出常见的比较
  * 'abcd'>'abc'为True，和Delphi中一样
  * '12345'>'1234'为True，和Delphi中一样
* 其他的数据库的相关规则目前还没有试过，比如MySQL、SQLite……
