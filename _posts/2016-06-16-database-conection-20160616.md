---
layout: post
title: 数据库连接和会话的设置方法
categories:  数据库之oracle 数据库之sqlserver 数据库之sql
tags: delphi oracle SQLServer 数据库 数据库连接 sql 数据库会话
---

##扩展资料

* [《亲测SQLServer的最大连接数》](http://www.cnblogs.com/wlb/archive/2012/04/08/2437617.html)
* [《SQL Server 和 Oracle 以及 MySQL 有哪些区别？》](https://www.zhihu.com/question/19866767)
* [《SQL Server 查询性能优化 相关文章》](http://www.cnblogs.com/xcsn/p/4929724.html)
* [《W3School SQL 教程》](http://www.w3school.com.cn/sql/index.asp)
* [《ORA-12516: TNS: 监听程序找不到符合协议堆栈要求的可用处理程错误解决方案》](http://www.cnblogs.com/dba_xiaoqi/archive/2010/11/01/1866472.html)

##SQL Server相关命令

查看和设置SQL Server的最大连接数限制

```
--查看当前的数据库连接限制值设置
> select value from master.dbo.sysconfigures where [config]=103

============================================================================================================

--设置show advanced options 值为1 才能允许下面对数据库连接数进行设置
> exec sp_configure 'show advanced options', 1

--执行RECONFIGURE语句使之生效
> RECONFIGURE

--设置连接数限制
> exec sp_configure 'user connections', 100

--重启服务使之生效：重启服务：打开SQL Server Management Studio-->右键数据库实例-->重新启动
```

查看已使用的连接、session信息

```
--查看已经使用的连接数个数
> select count(*) from sys.dm_exec_connections

--查看当前所有连接的详细信息
> select * from sys.dm_exec_connections

============================================================================================================

--查看当前有多少会话，一个连接可以有多个会话
> select count(*) from sys.dm_exec_sessions

--查看当前所有会话的详细信息
> select * from sys.dm_exec_sessions
```

##Oracle相关命令

检查process设置情况

```
> show parameter processes

--输出如下信息
NAME                                   TYPE        VALUE
------------------------------------ ----------- ------------------------------
aq_tm_processes                      integer     0
db_writer_processes                  integer     6
gcs_server_processes                 integer     0
job_queue_processes                  integer     0
log_archive_max_processes            integer     2
processes                            integer     150
```

检查当前已经占有的process情况

```
> select count(*) from v$process;

--输出如下信息
  COUNT(*)
----------
       147
```

检查session的设置情况

```
> show parameter session

--输出如下信息
NAME                                  TYPE        VALUE
------------------------------------ ----------- ------------------------------
java_max_sessionspace_size           integer     0
java_soft_sessionspace_limit         integer     0
license_max_sessions                 integer     0
license_sessions_warning             integer     0
logmnr_max_persistent_sessions       integer     1
session_cached_cursors               integer     20
session_max_open_files               integer     10
sessions                             integer     160

shared_server_sessions               integer
```

检查当前session的占用情况

```
> select count(*) from v$session;

--输出信息
  COUNT(*)
----------
        153
```

修改process和session的最大值设置

```
--修改process的最大限制值
> alter system set processes=300 scope=spfile;

--修改session的最大限制值
> alter system set sessions=335 scope=spfile;
--oracle文档要求，SESSIONS和TRANSACTIONS的初始化参数应该源于PROCESSES参数
--根据默认设置SESSIONS = PROCESSES * 1.1 + 5

--重启数据库后参数修改完成
> shutdown   --如果长时间没反应可能是连接请求没又关闭，也可以使用  abort参数直接关闭
> startup    --可以用 force参数   关闭当前运行数据库后正常启动。
```

##依然存在的疑问

* 连接和会话的关系？
* 长连接和短连接？
* SQL Server连接和Oracle连接？
* 数据库连接和网络连接？
