---
layout: post
title: Delphi测试线程的时间
categories: delphi之时间日期 delphi之多线程
tags: delphi 多线程 时间 日期
---


在16位时代，当我们在Windows3.x下编程时，经常会用到GetTickCount()或者timeGetTime()来判断一段代码的执行时间。示例如下

    var
        StartTime, Total: Longint;
    begin
        StartTime:= GetTickCount;
        {进行一些操作}
        Total:= GetTickCount - StartTime;
    end;

在多线程环境下，这是很困难的，因为在执行程序中间，操作系统可能会把CPU时间片分给别的进程。所以，用上述方法测出的时间并不真实。

为了解决这个问题，Windows NT提供了一个函数GetThreadTimes()，它可以提供详细的时间信息。其声明如下

    function GetThreadTimes(hThread: THandle; var lpCreationTime, lpExitTime,
        lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;

hThread参数是线程的句柄，其他参数都是变参，由GetThreadTimes()函数返回它们的值，其中：

* lpCreationTime：线程创建的时间
* lpExitTime：线程退出的时间。如果线程还在执行，此值无意义
* lpKernelTime：执行操作系统代码所用的时间
* lpUserTime：执行应用程序本身代码所用的时间

以上四个参数都是TFileTime类型。此类型在Windows单元中声明如下

    type
        TFileTime = record
            dwLowDateTime: DWORD;
            dwHighDateTime: DWORD;
        end;

此类型的声明有些不寻常。由dwLowDateTime和dwHighDateTime组成一个64位值，代表从1601年1月1日以来的计数（单位：千万分之一秒）

提示：因为TFileTime的长度是64位的，所以，为了进行数学运算你可以把它转换成Int64。这样，我们就可以对两个TFileTime的值比较大小，例如

    if Int64(UserTime) > Int64(KernelTime) then
        Beep;

为了帮助你学会TFileTime的用法，下面的代码将演示如何把TFileTime和TDateTime相互转换

    function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
    var
        SysTime: TSystemTime;
    begin
        if not FIleTimeToSystemTime(FileTime, SysTime)  then
            raise EConvertError.CreateFmt('FileTimeToSystemTime failed. '+ 'Error code %d', [GetLastError]);
        with SysTime do
            Result:= EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliSeconds)
    end;
    
    function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
    var
        SysTime: TSystemTime;
    begin
        with SysTime do
        begin
            DecodeDate(DateTime, wYear, wMonth, wDay);
            DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
            wDayofWeek:= DayOfWeek(DateTime);
        end;
        id not SysTimeToFileTime(SysTime, Result) then
            raise EConvertError.CreateFmt('SystemTimeToFileTime failed. ' + 'Error code %d', [GetLastError]);
    end;

注意：请记住函数GetThreadTimes()只适合于Windows NT/2000.如果你在Windows95/98下调用它，它总是返回False。非常不幸，Windows 95/98没有提供获取线程时间的手段