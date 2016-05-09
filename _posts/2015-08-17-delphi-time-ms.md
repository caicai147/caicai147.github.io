---
layout: post
title: 怎么使用Delphi获取当前的时间，精确到毫秒
categories: delphi之时间日期
tags: delphi 时间 日期
---

##Add in 2016-05-09

之前总结了这么多，绕了这么大一个弯子才获取到毫秒，之前说FormatDateTime无法获取是错误的。

其实直接使用FormatDateTime就可以获取的，就像下面这样：

```
var
    datetime: string;
begin
    datetime:= FormatDateTime('yyyy-mm-dd hh:mm:ss zzz', Now);
    ShowMessage(datetime);
end;
```

---

2015-08-17时，总结的有一些问题！

##先介绍一个可能比较常用的方法，获取当前时间

```
var
    datetime: string;
begin
    datetime:= FormatDateTime('yyyy-mm-dd hh:mm:ss', Now);
    ShowMessage(datetime);
end;
```

其中的Now 函数是获取当前的时间，FormatDateTime 用于将Now获取的时间信息格式化为想要的格式。

这个方法可以获取当前时间，并且格式化为例如 `2008-09-10 09:10:58` 的格式。


##再介绍能够获取毫秒信息的方法

```
var
  currentTime:TSystemTime;
  year, month, day, hour, minute, second, millisecond: string;
  datetime: string;
begin
  GetSystemTime(currentTime);
  year:= IntToStr(currentTime.wYear);
  month:= IntToStr(currentTime.wMonth);
  day:= IntToStr(currentTime.wDay);
  hour:= IntToStr(currentTime.wHour + 8);
  minute:= IntToStr(currentTime.wMinute);
  second:= IntToStr(currentTime.wSecond);
  millisecond:= IntToStr(currentTime.wMilliseconds);
    
  datetime:= year + '-' + month + '-' + day + ' ' + hour + ':' + minute + ':' + second + ':' + millisecond;
  ShowMessage(datetime);
end;
```

###时间的小时数问题

和C家族的语言（比如C、C++、PHP）一样，这种方式获取的时间，年、月、日、分钟、秒、毫秒都是，但是获取的小时数是晚了8小时的，比如当前时间是19:30，但是使用这种方法得到的小时数是11点，所以在进行开发的时候，这个问题需要单独处理一下。

这也是为什么上面的代码中小时数会加8的原因：hour:= IntToStr(currentTime.wHour + 8);

###需要引入的单元

要使用TSystemTime类型以及GetSystemTime过程需要引用Windows单元。

其中TSystemTime的定义是

```
PSystemTime = ^TSystemTime;
_SYSTEMTIME = record
  wYear: Word;
  wMonth: Word;
  wDayOfWeek: Word;
  wDay: Word;
  wHour: Word;
  wMinute: Word;
  wSecond: Word;
  wMilliseconds: Word;
end;
{$EXTERNALSYM _SYSTEMTIME}
TSystemTime = _SYSTEMTIME;
SYSTEMTIME = _SYSTEMTIME;
{$EXTERNALSYM SYSTEMTIME}
```

其中GetSystemTime的声明是这样的

```
procedure GetSystemTime; external kernel32 name 'GetSystemTime';
```

它的函数原型是

```
procedure GetSystemTime(var lpSystemTime: TSystemTime); stdcall;
```

一看就是使用静态加载的方式加载了DLL，所以在Windows里面也只是引入GetSystemTime过程，并没有实现，具体的实现是在 kernel32.dll这个DLL里面实现的。具体这个DLL是使用C语言还是使用Delphi语言实现的就不得而知了，等我以后研究到再说。（估计是用C实现的，要不为什么获取的时间的小时数会差8个小时，和C语言一样，不过这只是目前的猜测）

###什么是kernel32.dll？

kernel32.dll是Windows 9x/Me中非常重要的32位动态链接库文件，属于内核级文件。它控制着系统的内存管理、数据的输入输出操作和中断处理，当Windows启动时，kernel32.dll就驻留在内存中特定的写保护区域，使别的程序无法占用这个内存区域。
