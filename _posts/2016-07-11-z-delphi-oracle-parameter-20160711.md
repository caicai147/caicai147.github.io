---
layout: post
title: Delphi使用绑定变量法操作Oracle
categories: 数据库之oracle delphi之数据库
tags: sql oracle 数据库 delphi 绑定变量
---

##说明

简单通过几个小例子展示使用普通SQL操作数据库、绑定变量法操作数据库的语法区别

本文只是展示绑定变量的语法，还有更多值得研究的：

* Oracle和SQL Server绑定变量的异同
* 绑定变量相比于普通SQL的优势
* 绑定变量在数据库底层的运行原理
* 普通SQL在数据库底层的运行原理
* 等等

**参考文章**

* [《Delphi使用ADO进行数据库编程》](http://www.xumenger.com/delphi-ado-20150825/)
* [《Delphi编程时候诡异地出现ORA-00937错误，记录解决它的思路和方法》](http://www.xumenger.com/delphi-oracle-ado-20150829/)

##基础数据准备

在Oracle中创建用于测试的数据表，并插入2条测试数据

```
create table practice(uno varchar(8), uname varchar(20));

insert into practice values('no1', 'name1');

insert into practice values('no2', 'name2');
```

##Delphi执行普通SQL查询

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, name, ConStr: string;
begin
  sSql := 'select uname from practice where uno = ''no1'' ';
  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql;
      //AdoQry.ExecSQL;      //普通SQL查询的时候如果使用ExecSQL会报错
      AdoQry.Open;
      name := AdoQry.FieldByName('uname').AsString;
      ShowMessage(name);
    except
      ShowMessage('普通SQL查询报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```

##Delphi绑定变量查询

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, name, ConStr: string;
begin
  sSql := 'select uname from practice where uno = :no';
  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql;
      AdoQry.Parameters.ParamByName('no').Value:= 'no2';
      //AdoQry.ExecSQL;   //绑定变量法更新时使用ExecSQL会报错
      AdoQry.Open;
      name := AdoQry.FieldByName('uname').AsString;
      ShowMessage(name);
    except
      ShowMessage('绑定变量法查询报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```

##Delphi普通SQL更新

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, ConStr: string;
begin
  sSql := 'update practice set uname = ''testnm'' WHERE uno = ''no1'' ';
  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql;
      AdoQry.ExecSQL;
    except
      ShowMessage('普通SQL更新报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```

##Delphi存储过程更新

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, ConStr: string;
begin
  sSql := 'update practice set uname = case when :name_condition1 = ''condition'' then :name_result1 else :name_result2 end '
        + 'where uno = :no_condition1;';
        
  //如果是下面这样的，SQL中有; 那么就会导致执行报错：ORA-00911: 无效字符
  //sSql := 'update practice set uname = case when :name_condition1 = ''condition'' then :name_result1 else :name_result2 end '
  //      + 'where uno = :no_condition1;';

  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql;
      AdoQry.Parameters.ParamByName('name_condition1').Value:= 'condition';
      AdoQry.Parameters.ParamByName('name_result1').Value:= 'result1';
      AdoQry.Parameters.ParamByName('name_result2').Value:= 'result2';
      AdoQry.Parameters.ParamByName('no_condition1').Value:= 'filter';
      //实现对practice表中uno='filter'的记录进行有条件的更新

      //AdoQry.Open;    //绑定变量法更新时使用Open会报错
      AdoQry.ExecSQL;   //应该使用ExecSQL
    except
      ShowMessage('绑定变量法更新报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```

【特别小心】如果使用绑定变量法的两个变量同名的话，会导致报错：“不正常的定义参数对象，提供了不一致或不完整的信息”，比如下面例程所展示的

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, ConStr: string;
begin
  sSql := 'update practice set uname = :name where uname = :name';  //两个变量同名，都是:name，会导致报错
  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql;
      AdoQry.Parameters.ParamByName('name').Value:= 'name1'; 
      //AdoQry.Open;    //绑定变量法更新时使用Open会报错
      AdoQry.ExecSQL;   //应该使用ExecSQL
    except
      ShowMessage('绑定变量法更新报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```

##Delphi执行普通SQL插入

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, name, ConStr: string;
begin 
  sSql := 'insert into practice(uno, uname) values(''no4'', ''name4'')';
  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql; 
      AdoQry.ExecSQL;
      //AdoQry.Open;    //普通SQL插入新数据时使用Open会报错
    except
      ShowMessage('普通SQL插入报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```

##Delphi存储过程插入

```
var
  AdoConn: TADOConnection;
  AdoQry: TADOQuery;
  sSql, name, ConStr: string;
begin 
  sSql := 'insert into practice(uno, uname) values(:no, :name)';
  ConStr := 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=trade;Password=trade;Data Source=MINE' ;
  AdoConn := TADOConnection.Create(nil);
  AdoQry := TADOQuery.Create(nil);
  try
    try
      AdoConn.ConnectionString := ConStr;
      AdoConn.Open;
      AdoQry.Connection := AdoConn;
      AdoQry.SQL.Text := sSql;
      AdoQry.Parameters.ParamByName('no').Value:= 'no3';
      AdoQry.Parameters.ParamByName('name').Value:= 'name3';
      AdoQry.ExecSQL;   
      //AdoQry.Open;    //绑定变量法插入新数据时使用Open会报错
    except
      ShowMessage('绑定变量法插入报错');
    end;
  finally
    AdoQry.Free;
    AdoConn.Free;
  end;
end;
```
