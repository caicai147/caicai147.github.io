---
layout: post
title: Delphi的Trim函数
categories: delphi之精确使用api
tags: delphi trim api
---


## 三个Trim函数简介

函数原型 `function Trim(const S: string): string;` 将字符串前后的空白及控制字元清掉.

注意Trim函数只能清掉字符串前后的空格及控制字元，不能清除字符串中间的空格

函数原型 `function TrimLeft(const S: string): string;` 将字符串左边的空白及控制字元清掉.

函数原型 `function TrimRight(const S: string): string;` 将字符串右边的空白及控制字元清掉.


## Trim实际应用场景举例

以下有一个使用Delphi的Trim函数的实际的场景，看完也许能让你更为深刻的认知Trim在实际开发中的应用

进行Delphi数据库应用，想做一个用户身份确认的登陆窗口，就是输入用户名和密码然后去数据库取出数据验证用户身份，可是程序运行时候，尽管每次输入的用户名称和密码都是 正确的，可是程序总是说我的密码输入有错误，问题出在何处呢？请看代码

    if ibt_user_pass.RecordCount=1 then
    begin
        if ibt_user_pass.FieldByName('passwd').AsString=edit2.Text then    //****
            form_student_login.Hide
        else
            application.MessageBox('请确认密码是否正确！','密码不匹配',MB_OK);
        end
    else
        application.MessageBox('请确认用户名是否正确！','无此用户',mb_ok)
    end;

才知道原来是加//**** 注释那行的代码存在问题，数据库如果某个字段是Char类型的数据，才存放数据后会自动在字符串后面补上空格，以满足位数的需要，因此表面上看，取得的密码和我输入的密码一样，实际上是不一样的。

### 解决方法：

#### 办法1：

    if ibt_user_pass.FieldByName('passwd').AsString = edit2.Text then

改为

    trim(ibt_user_pass.FieldByName('passwd').AsString=edit2.Text then

Trim 函数作用是去掉字符串中多加上去的空格

#### 办法2：

数据库的char 类型改成 varchar类型