---
layout: post
title: Delphi和C/C++配合编程时传递结构体的规范
categories: c/c++之函数 c/c++之指针与内存 delphi之字符串 delphi之指针与内存 c/c++之字符串 delphi之dll c/c++之dll 软件质量之内存管理
tags: c c++ 函数 指针 内存 字符串 跨语言 内存管理 结构体 数组越界
---

##前情提要

* 有必要先阅读以下文章
  * [《总结一下最近开发遇到的问题，以及最近需要学习的知识点》](http://www.xumenger.com/learn-plan-20151123/)中关于stdcall关键字的作用说明
  * [《Delphi和C/C++配合编程时的字符串传值规范》](http://www.xumenger.com/delphi-vcpp-string-chararray-20160511/)中关于字符串的使用规范
  * [《从指针和内存角度区别Delphi的record、record类型的指针、类》](http://www.xumenger.com/delphi-pointer-memory-record/)复习Delphi的结构体
* 今天简单整理一下最近在Delphi和C++配合编程时使用结构体遇到的问题
  * 不过暂时先止于此，对于更为深层次的研究暂时还没有遇到问题，也就暂时不做整理
  * 整理一下Delphi和C++配合编程时结构体的传递
* 后续需要整理：
  * Delphi的record和packed record的内存分布情况
  * Delphi和C/C++的结构体中各个字段类型、顺序的不同对于结构体内存分布的影响
  * 如果Delphi端使用packed来“压缩结构体内存”，那么C/C++端怎么与之对应？
* 针对以上“后续需要整理”的几点，可以先参见：
  * [《Delphi中的内存对齐 与 Packed关键字 》](http://blog.csdn.net/procedure1984/article/details/3057730)
  * [《 C++中的内存对齐 》](http://blog.csdn.net/procedure1984/article/details/3057703)
  
##注意

* 如下所示Delphi端的record定义和C/C++端的struct定义必须保证一致
  * 字段顺序一致
  * 相关数组的长度必须一致
  * 对应字段的数据类型必须一致
* 假如哪一步部分出现问题，就会导致Delphi端传到C/C++端的结构体无法在内存上对应起来而出错
  * 比如这个例子，Delphi端定义`IP: array[0..16] of Char;`，C/C++端定义`char	IP[16];`
  * 一个17位，一个16位，就会导致C/C++获取Delphi端传过来的结构体字段解析出错！！
* 还有各种不规范的使用场景，这里就暂不一一介绍
* 就按照下面示例中的规范来进行开发就不会存在问题

##C/C++端代码

C++端导出的dll名称为：test.dll

####函数指针类型 和 结构体定义

```
//定义函数指针类型
//因为在Delphi和C++端需要传递这种回调函数类型，所以要用stdcall关键字
typedef void (__stdcall *TOnReceive)(const char * msg);   

//初始化请求
typedef struct InitReq
{
	char	IP[16];				//IP
	int		port;				  //端口
	char	passwd[30];		//密码
	char  *username;    //用户名
	TOnReceive OnReceive;   //定义函数指针变量
}TInitReq;
```

####导出函数声明

```
__declspec(dllexport) int  __stdcall CallInit(TInitReq *pInitReq);
```

####导出函数实现

```
int  __stdcall CallInit(TInitReq *pInitReq)
{
  //获取Delphi端传入的结构体（以及结构体的各个字段）进行使用
}
```

##Delphi端

####函数指针类型 和 结构体定义

```
type

  //定义导出函数指针类型
  TCallInit = function(intReq: PInitReq): Integer; stdcall;

  //定义回调函数指针类型
  //因为在Delphi和C++端需要传递这种回调函数类型，所以要用stdcall关键字
  TOnReceive = procedure(const msg: PChar); stdcall;

  PInitReq = ^TInitReq;
  TInitReq = record
    IP: array[0..15] of Char;               //IP
    Port: Integer;                          //端口
    passwd: array[0..30] of Char;           //密码
    username: PChar;                        //用户名
    FonReceive: TOnReceive;                 //收到消息的回调函数
  end;
```

####回调函数实现

```
procedure OnReceive(const msg: PChar);
begin
  ShowMessage(msg);
end;
```

####导出函数调用

```
var
  hTestDLL: THandle;
  CallInit: TCallInit;
  initReq: TInitReq;
begin
  hTestDLL := LoadLibrary('./test.dll');
  CallInit := GetProcAddress(hTestDLL, 'CallInit');
  
  StrPLCopy(initReq.IP, '127.0.0.1', SizeOf(initReq.IP));
  initReq.Port := 8000;
  StrPLCopy(initReq.passwd, 'password', SizeOf(initReq.passwd));
  initReq.username := PChar('xumenger');
  initReq.FonReceive := OnReceive;
  
  CallInit(@initReq);
end;
```
