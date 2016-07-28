---
layout: post
title: Delphi和C/C++配合编程时传递字符串的规范
categories: c/c++之函数 c/c++之指针与内存 delphi之字符串 delphi之指针与内存 c/c++之字符串 delphi之dll c/c++之dll 软件质量之内存管理
tags: c c++ 函数 指针 内存 字符串 跨语言 内存管理 汉字 字符 字符编码 数组越界
---

##说明

* Delphi版本是6；Visual C++版本是6
* [《Delphi和VC++使用DLL配合开发【例程】》](http://www.xumenger.com/delphi-cpp-dll-20160412/)有讲到VC++导出DLL，Delphi加载并调用
* 关于Delphi的字符串、字符数组相关的可以参考以下文章：
  * [《Delphi字符串与字符数组之间的转换（初始化的重要性）》](http://www.xumenger.com/delphi-string-pchar-chararray-20150422-02/)
  * [《Delphi的字符串、PChar和字符数组之间的转换》](http://www.xumenger.com/delphi-string-pchar-chararray-20150422-01/)
  * [《Delphi中的各种字符串、String、PChar、Char数组》](http://www.xumenger.com/delphi-string-pchar-chararray-20150415/)
  * [《正确理解Delphi中的传值调用/传址调用》](http://www.xumenger.com/delphi-value-address-func-proc-20160506/)
  * [《正确理解C/C++中的传值调用/传址调用/引用调用》](http://www.xumenger.com/c-cpp-function-value/)
* 下文讲到了Delphi调用VC++导出的方法时，假如在Delphi和C++之间传递字符串应该注意什么？
* 对于Delphi本身也是这样的情况
* 下文展示的是在入参中传递字符串(字符数组)，其实还可以用结构体(包含一个char数组)的方式传递字符串

##每个汉字占用多少字节？

####我的环境使用Delphi测试

```
  len := Length('测试');
  ShowMessage(IntToStr(len));    //值是4不是2
```

####我的环境使用VC++测试

```
int len  = strlen("汉字");       //长度是4
```

####本质原因

这个问题的答案与系统所采用的字符编码方式有关：

* utf-8：如果系统采用的是utf-8，那么strlen("汉字")=6，即一个汉字占用3个字节
  * linux系统默认情况下采用的该种编码方式。
* gb2312：如果系统采用的是gb2312，则strlen("汉字")=4，即一个汉字占用2个字节
   * windows中文版采用的该种编码方式。
* 我的系统是gb2312编码的

##常用的字符串拷贝函数

* Delphi常用的字符串拷贝函数：[《Delphi的字符串拷贝函数》](http://www.xumenger.com/delphi-string-copy-20151119/)
* C/C++常用的字符串拷贝函数：[《C/C++的字符串拷贝函数》](http://www.xumenger.com/c-cpp-string-copy-20160511/)

##本文测试程序的基础代码说明
  
C++端导出的dll名称为：test.dll

C++端导出函数：

```
__declspec(dllexport) int  __stdcall CallCpp(char * resp);
```

Delphi定义函数指针、声明函数指针对象

```
type
  TCallCpp = function(resp: PChar): Integer; stdcall;
  
var
  hTestDLL: THandle;
  CallCpp: TCallCpp;
begin
  hTestDLL := LoadLibrary('./test.dll');
  CallCpp := GetProcAddress(hTestDLL, 'CallCpp');
end;
```

##情况一

>Delphi和C++之间通过类似这样的传递方式是不行的

####Delphi

```
  var
    resp: array[0..64] of Char;
  begin
    CallCpp(resp);
  end;
```

####C++

```
void CallCpp(char *resp)
{
  char * src = '测试应答';
  strncpy(resp, src, strlen(resp));
    //无法获取Delphi端Char数组的长度，strlen(resp)获取的值是0
}
```
  
##应该这样做
    
####Delphi

```
var
  resp: array[0..64] of Char;
begin
  CallCpp(resp);
end;
```
    
####C++

```      
void CallCpp(char *resp)
{
  char * src = '测试应答';
  strncpy(resp, src, strlen(src));
    //应该使用C++端的字符串的长度
}
```

##情况二

>在C++端获取Delphi字符数组的长度

* 其中讲到Delphi端的SizeOf和Length的使用
    
####Delphi

```
var
  resp: array[0..64] of Char;
begin
  StrPLCopy(resp, '测试测试', SizeOf(resp));
    //此时SizeOf(resp)和Length(resp)的值都是65，并不是8(因为汉字不是单字节的)
    //此时resp数组的值： #178,#226,#202,#212,#178,#226,#202,#212,#0,... 
    //其中#178和#226构成汉字'测'，#202和#212构成汉字'试'
  CallCpp(resp);
    //运行到这里，调用C++端的函数，此时resp数组中存储的值是'测试测试测试测试'，
    //对应#178,#226,#202,#212,#178,#226,#202,#212,#178,#226,#202,#212,#178,#226,#202,#212,#0,#0...
end;
```

####C++

```
void CallCpp(char *resp)
{
  char * src = '测试测试测试测试';
    //运行到这里，sizeof(resp)的值是4(因为resp在这里是一个字符指针，而不是字符数组)，strlen的值是8
  strncpy(resp, src, strlen(src));
    //运行到这里，sizeof(resp)的值是4(因为resp在这里是一个字符指针，而不是字符数组)，strlen的值是16
}
```

##情况三

>小心别因为Delphi端定义的字符数组长度小于C++给其拷贝的长度导致越界

####Delphi

```
var
  resp: array[0..10] of Char;
begin
  StrPLCopy(resp, '测试测试', SizeOf(resp));
    //此时SizeOf(resp)和Length(resp)的值都是65，并不是8(因为汉字不是单字节的)
    //此时resp数组的值： #178,#226,#202,#212,#178,#226,#202,#212,#0,.....  
    //其中#178和#226构成汉字'测'，#202和#212构成汉字'试'
  CallCpp(resp);
    //运行到这里，resp的值是：#178,#226,#202,#212,#178,#226,#202,#212,#178,#226,#202[后面就没有了]
    //明显发现因为字符数组太短而少了几个字节，另外正常的字符数组存放字符串的最后一位应该是#0，这里也不对！
    //和上面的例子相比明显是因为越界导致出现的现象
  ShowMessage(resp);
    //这里弹出框中，弹出的数字是'测试测试测'
end;
```
####C++

```
void CallCpp(char *resp)
{
  char * src = '测试测试测试测试';
    //运行到这里，sizeof(resp)的值是4(因为resp在这里是一个字符指针，而不是字符数组)，strlen的值是8
  strncpy(resp, src, strlen(src));
    //运行到这里，sizeof(resp)的值是4(其实就是一个指针类型的长度)，strlen的值是8
}
```

##情况四

>若Delphi端传string给C++，在C++端不能对其进行写操作，否则会报非法地址访问异常
  
####参考：

* [《Delphi字符串与字符数组之间的转换（初始化的重要性）》](http://www.xumenger.com/delphi-string-pchar-chararray-20150422-02/)
* [《Delphi的字符串、PChar和字符数组之间的转换》](http://www.xumenger.com/delphi-string-pchar-chararray-20150422-01/)
* [《Delphi中的各种字符串、String、PChar、Char数组》](http://www.xumenger.com/delphi-string-pchar-chararray-20150415/)

####Delphi

```
var
  resp: string;
begin
  resp := '测试测试';
  //注意是将string强转成PChar传给C++端的
  CallCpp(PChar(resp));  
    //因为CallCpp中有对resp的写操作， 所以运行到这里会出现 内存地址非法访问错误
    //CallCpp抛出异常，接着下面的代码就不再执行
  ShowMessage(resp);
end;
```

####C++

```
void CallCpp(char *resp)
{
  char * src = '测试测试测试测试';
  strncpy(resp, src, strlen(src));
    //因为strncpy相对其进行写操作，所以会出现内存访问错误
    //根本原因还有待研究
}
```
 
##情况五

>若Delphi端传string给C++，在C++端可以进行读操作，没有问题，可以正常传值
    
####Delphi

```
var
  resp: string;
begin
  resp := '测试测试';
  //注意是将string强转成PChar传给C++端的
  CallCpp(PChar(resp));
    //因为C++端对其进行的是读操作，所以没有问题，正常执行，
    //对于Delphi端，所有参数为PChar的方法，可以直接不对string进行转型而使用
  ShowMessage(resp);
    //这里正常弹出框：'测试测试'
        
  CallCpp('测试测试');
    //CallCpp中对char * 入参是只读的，所以这里也是正常调用
    //对C++端的程序进行断点调试，运行的效果和上面使用string传入的效果相同
end;
```
    
####C++

```
void CallCpp(char *resp)
{
  char dest[100];
  strncpy(dest, resp, strlen(dest));
    //dest数组内的内容是'测试测试'
  int n = strlen(dest); //入参为'测试测试',则dest的长度是8
  int m = sizeof(dest)  //注意，因为在C++中对dest定义的是字符数组，而非字符指针变量，所以这里sizeof获取的值是100，而非4
}
```
	  
####简单总结

* 对于方法CallCpp(resp: PChar)，如果CallCpp中没有写操作，只有读操作，可以向下面的几种传值方式

```
var
  s: string;
  CArr: array[0..100] of Char;  
begin
  CallCpp('测试')  		//直接用字符串常量传入
  
  s:= '测试';
  CallCpp(PChar(s));    //直接以Delphi的string传入
  
  CallCpp(CArr);        //以字符数组名传入
end;
```

* 如果CallCpp中有写操作，那么只能`CArr: array[0..100] of Char;  CallCpp(CArr);`
* 所以最好定义要规范
  * 如果是只读的，应该使用关键字const定义为 `int CallCpp(cosnt char * resp);`  
  * 这样开发者调用的时候就不会误传Delphi的string、或者字符串常量了
  * 如果是可写的，则不使用关键字const，定义为 `int CallCpp(cosnt char * resp);`  
  * 这就是告诉开发者，其中会有对字符数组的写操作
* 不只是针对Delphi调用C++导出的方法，Delphi自己定义方法时也是这样的
	* 续[《理解Delphi编程中方法参数定义的关键字const》](http://www.xumenger.com/delphi-z-string-const-20160511/)
