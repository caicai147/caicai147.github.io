---
layout: post
title: 数据库锁机制
categories: 数据库之sqlserver 深入学习之数据库原理
tags: SQLServer 锁 sql 死锁 数据库 数据库连接  
---

##基础数据准备

参考博客[《数据库锁》](http://www.cnblogs.com/zhouqianhua/archive/2011/04/15/2017049.html)

本次进行测试的数据库选择SQL Server 2008

先创建一个数据库

```
CREATE DATABASE testLock
```

然后在该数据库中创建用于测试的表

```
USE [testLock]
GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[test](
  [id] [int] NOT NULL,
  [name] [char](10) NULL,
  [job] [char](10) NULL,
  [introduce] [nvarchar](1024) NULL,
 CONSTRAINT [PK_test] PRIMARY KEY CLUSTERED 
(
  [id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

SET ANSI_PADDING OFF
GO
```

然后模拟200万条测试数据

```
DECLARE @i int
SET @i=1
WHILE @i<2000000
   BEGIN   
   INSERT INTO testLock..test (id, name, job, introduce) 
   VALUES(@i, 'xumenger', '神仙', '现在我们来研究一下数据库的锁机制，所以神仙来造一些测试数据')   
   SET @i=@i+1   
END; 
```
