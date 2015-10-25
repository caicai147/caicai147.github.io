---
layout: post
title: TADOQuery学习总结 
categories: delphi之数据库编程
tags: delphi 数据库 ado adoquery
---


上一篇讲解了一些TADOQuery的简单的用法，但是还有很多方法没有讲到，这里就直接拿来主义，转载一篇《[TADOQuery学习总结](http://blog.csdn.net/phaze/article/details/5734786)》为我所用。
 

1.Create三种参数的区别
---

TADOQuery.Create(nil) 和TADOQuery.Create(Self)以及TADOQuery.Create(Application)的区别

* 用nil创建，需要自己手动销毁创建的对象
* 用self创建，如果是在窗体单元里，则是在窗体销毁的时候，同时自动销毁创建的对象
* 用Application，则是在整个程序退出的时候自动销毁创建的对象

2.Open方法
---

所要执行的SQL语句必须是有返回结果的，update、delete、insert都不行

3.Close方法
---

关闭连接；基于TADOQuery的操作都不能进行

4.SQL属性
---

存放SQL语句的列表

* Clear：清空SQL语句
* Add：增加SQL语句的内容

5.对于Record的操作 
---

* Prior方法：前一个记录
* Next方法：下一个记录
* First方法：第一个record
* Last方法：最后一个record

6.GetFieldNames方法
---

获取一个新的表的所有名字，例如

    ADOQuery.GetFieldNmes(ListBoxs.Items);

7.修改一个记录可以用update SQL执行
---

    ADOQuery.Close;
    ADOQuery.SQL.Clear;
    ADOQuery.SQL.Add('Update SQL 语句');
    ADOQuery.ExecSQL;

也可以用

    ADOQuery.Edit;
    ADOQuery.FieldByName('uno').AsString:='no1'
    ......
    ADOQuery.FieldByName('uname').AsString:= 'jack';
    TADOQuery.Post;

注意第一种形式不要Post，第二种形式必须Post才能保证修改了数据库。

8.增加一条记录
---

可以使用SQL语句，和update一样。

也可以使用这样的方式，例子

    ADOQuery.Append;
    ADOQuery.FieldByName('uno').AsString:= 'no2';
    ...
    ADOQuery.FieldByName('uname').AsString:= 'name';
    ADOQuery.Post;

9.删除一条记录
---

可以直接删除

    ADOQuery.Delete;

也可以使用SQL删除

    ADOQuery.SQL.Clear;
    ADOQuery.SQL.Add('delete from practice where uname=''jack'' ');
    ADOQuery.ExecSQL;

　　