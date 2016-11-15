---
layout: post
title: Delphi网络编程中的阻塞和非阻塞模式
categories: delphi之网络编程
tags: windows delphi socket 网络 TCP
---

[《Delphi使用IdTcpServer/IdTcpClient进行网络编程》](http://www.xumenger.com/windows-delphi-socket-20160920/)和[《Delphi使用ClientSocket/ServerSocket进行网络编程》](http://www.xumenger.com/windows-delphi-socket-20161010/)分别展示了使用IdTcpServer/IdTcpClient 和ServerSocket/ClientSocket 进行网络编程时线程策略的区别

IdTcpServer/IdTcpClient 只能是支持阻塞模式编程

而ServerSocket/ClientSocket 既可以是非阻塞模式，也可以是阻塞模式的。ServerSocket 通过将ServerType 设置成stNonBlocking 或stThreadBlocking 来设置为非阻塞或阻塞；ClientSocket 通过将ClientType 设置成ctNonBlocking 或ctBlocking 来设置为非阻塞或阻塞。如果不设置，ServerSocket/ClientSocket 都默认是非阻塞的

因为他们都是基于TCP协议的，所以以下的组合都是可以通信的

   通信模式         	| ServerSocket 阻塞 | ServerSocket 非阻塞   | IdTcpServer 阻塞  
----------------    	| ------------------| --------------------- |---------------------
**ClientSocket 阻塞**   | 可通信  			| 可通信				| 可通信	
**ClientSocket 非阻塞** | 可通信   			| 可通信				| 可通信	
**IdTcpClient 阻塞**    | 可通信  			| 可通信				| 可通信	

##阻塞和非阻塞的区别

以下的内容是直接从[《阻塞和非阻塞的区别》](http://www.cnblogs.com/orez88/articles/2513460.html)Copy 过来的

简单点说：阻塞就是干不完不准回来；非阻塞就是你先干，我现在看看有其他事没有，完了告诉我一声

我们拿最常用的send 和recv 两个函数来说吧......

比如你调用send 函数发送一定的Byte，在系统内部send 做的工作其实只是把数据传输（Copy）到TCP/IP 协议栈的输出缓冲区，它执行成功并不代表数据已经成功的发送出去了，如果TCP/IP 协议栈没有足够的可用缓冲区来保存你Copy 过来的数据的话...这时候就体现出阻塞和非阻塞的不同之处了：对于阻塞模式的socket send 函数将不返回直到系统缓冲区有足够的空间把你要发送的数据Copy 过去才返回；而对于非阻塞模式的socket 来说send 会立即返回WSAEWOULDDBLOCK 告诉调用者说：“发送操作被阻塞了！你想办法处理吧......”

对于recv 函数，同样道理，该函数的内部工作机制其实是在等待TCP/IP 协议栈的接收缓冲区通知它说：“嗨，你的数据来了！”。对于阻塞模式的socket 来说，如果TCP/IP 协议栈的接收缓冲区没有通知一个结果它就一直不返回，消耗着系统资源；对于非阻塞模式的socket 该函数会马上返回，然后告诉你：WSAEWOULDDBLOCK---“现在没有数据,回头在来看看”

>在进行网络编程时，我们常常见到同步、异步、阻塞和非阻塞四种调用方式，这些方式彼此概念并不好理解。下面是我对这些术语的理解

**同步**

所谓同步，就是在发送一个功能调用时，在没有得到结果之前，该调用就不返回。按照这个定义，其实绝大多数函数都是同步调用的（例如sin、isdigit等）。但是一般而言，我们在说同步、异步的时候，特指那些需要其他部件协作或者需要一定时间完成的任务。最常见的例子就是SendMessage。该函数发送一个消息给某个窗口，在对方处理完消息之前，这个函数不返回。当对方处理完毕以后，该函数才把消息处理函数返回的LRESULT 值返回给调用者

**异步**

异步的概念和同步相对。当一个异步过程调用发出后，调用者不能立刻得到结果。实际处理这个调用的部件完成后，通过状态、通知和回调来通知调用者。以CAsyncSocket 类为例（注意，CSocket 从CAsyncSocket 派生，但是其功能已经由异步转化为同步），当一个客户端通过调用Connect 函数发出一个连接请求后，调用者线程立刻可以向下运行。当连接真正建立起来以后，socket 底层会发送一个消息通知该对象

这里提到执行部件和调用者通过三种途径返回结果：状态、通知和回调。可以使用哪一种依赖于执行部件的实现，除非执行部件提供多种选择，否则不受调用者控制。如果执行部件用状态来通知，那么调用者就需要每隔一定时间检查一次，效率就很低（有些初学多线程编程的人，总喜欢用一个循环去检查某个变量的值，这其实是一种很严重的错误）。如果是使用通知的方式，效率则很高，因为执行部件几乎不需要额外的操作。至于回调函数，其实和通知没有太大区别

**阻塞**

阻塞调用是指调用结果返回之前，当前线程会被挂起。函数只有在得到结果之后才会返回。有人也许会把阻塞调用和同步调用等同起来，实际上它们是不同的。对于同步调用来说，很多时候当前线程还是激活的，只是从逻辑上当前函数没有返回而已

一个同步调用的例子：我们在CSocket 中调用Receive 函数，如果缓冲区中没有数据，这个函数就会一直等待，直到有数据才返回。而此时，当前线程还会继续处理各种各样的消息。如果主窗口和调用函数在同一个线程中，除非你在特殊的界面操作函数中调用，其实主界面还是应该可以刷新

一个阻塞调用的例子：socket 接收数据的另外一个函数recv 则是一个阻塞调用的例子。当socket 工作在阻塞模式的时候，如果没有数据的情况下调用该函数，则当前线程就会被挂起，直到有数据为止，完全无法响应其任何消息

**非阻塞**

非阻塞和阻塞的概念是相对应的，指在不能立刻得到结果之前，该函数不会阻塞当前线程，而是会立刻返回

**对象的阻塞模式和阻塞函数调用**

对象是否处于阻塞模式和函数是不是阻塞调用有很强的相关性，但并不是一一对应的。阻塞对象上可以有非阻塞的调用方式，我们可以通过一定的API 去轮询状态，在适当的时候调用阻塞函数，就可以避免阻塞。而对于非阻塞对象，调用特殊的函数也可以进入阻塞调用。函数select 就是这样的例子

##运行效果展示

因为ServerSocket(非阻塞)/ClientSocket(非阻塞) 通信的运行效果在[《Delphi使用ClientSocket/ServerSocket进行网络编程》](http://www.xumenger.com/windows-delphi-socket-20161010/)中已经展示和说明了，IdTcpServer/IdTcpClient也在[《Delphi使用IdTcpServer/IdTcpClient进行网络编程》](http://www.xumenger.com/windows-delphi-socket-20160920/)中展示和说明了，接下来展示其他两种配合的运行情况，直接使用前两篇文章中的EXE 程序进行测试

   通信模式         | ServerSocket 阻塞 | ServerSocket 非阻塞   | IdTcpServer 阻塞  
------------------- | ------------------| --------------------- |---------------------
ClientSocket 阻塞   |  可通信  			| 可通信				| 可通信
ClientSocket 非阻塞 |  可通信   		| 可通信				| 可通信
IdTcpClient 阻塞    |  可通信  			| 可通信				| 可通信

**ClientSocket配合IdTcpServer**



**IdTcpClient配合ServerSocket**
