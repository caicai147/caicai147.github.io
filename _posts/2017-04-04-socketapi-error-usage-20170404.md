---
layout: post
title: 非阻塞IO网络编程错误示范
categories: delphi之网络编程 delphi之消息机制
tags: delphi网络编程 delphi 网络 ScktComp 消息机制
---

之前在[《Delphi网络编程：阻塞和非阻塞》](http://www.xumenger.com/windows-delphi-socket-20161011/)第一次研究阻塞和非阻塞网络编程的时候说到推荐使用非阻塞IO模式的网络编程，不推荐阻塞式的！

难道真像那里说的那么简单吗？非阻塞虽然节省了线程资源，但就没有其他坑了吗。一般我们在生活或者软件开发领域，对比两个事物或者两个方案，很难说某一个完全优于另一个（当然也有），大部分的情况是A在这方面优于B，但是在另外方面相比于B就有缺陷。其实这里也不例外，虽然非阻塞式IO节省了线程资源，但是其复杂度相比于阻塞式IO网络编程要复杂得多

下面就展示一下最近在使用ScktComp进行非阻塞网络编程时遇到的一些坑！

##Socket API简介

使用Socket API直接接收和发送数据的逻辑大概如下图

![image](../media/image/2017-04-04/01.png)

调用send()其实并不是直接将数据发送出去，而是将数据从用户态拷贝到内核态的发送缓冲区，然后内核态的TCP协议栈来进行发送；与之对应的是接收，调用recv()并不是直接从网络上收数据，而是TCP协议栈收到数据将数据放到内核态的接收缓冲区中，用户态调用recv()其实是从内核态接收缓冲区中读数据！

阻塞式IO和非阻塞式IO的很重要的区别就是读写数据的不同：

* 阻塞式网络编程
	* 发送数据时，如果调用send()要发送n字节数据，但内核态发送缓冲区只有n-m空间，那么就会阻塞当前线程，直到内核将发送缓冲区中的数据发出去，发送缓冲区中腾出n字节的空间，这时候send()方法才会成功将数据写到内核缓冲区，并且返回
	* 对应的，如果调用recv()要读取n字节数据，但内核态接收缓冲区中只收到n-m字节数据，那么就会阻塞当前线程，直到内核继续收齐n字节数据，recv()才会成功读到n字节数据并且返回
* 非阻塞式网络编程
	* 发送数据的时候，如果调用send()发送n字节数据，但内核态发送缓冲区只有n-m空间，其不会阻塞当前线程，而是直接将n-m字节数据写入，剩下的m字节没有写入，通过send()的返回值可以知道具体写入了多少数据。对于剩下的没有写到内核态缓冲区中的数据，就需要开发者控制等到下次可写的时候继续写入
	* 接收数据的时候，如果调用recv()要接收n字节数据，但内核态接收缓冲区只有n-m字节数据，其不会阻塞当前线程，而是直接读出n-m字节数据，通过recv()的返回值可以知道到底本次“收到”多少数据

对于阻塞式IO，当条件不满足时，会阻塞当前线程等待，虽然导致线程阻塞不工作，但是开发者不需要做其他额外的控制就可以保证数据的完整性；可是对于非阻塞式IO，会出现实际“发送”数据比想要“发送”数据少、实际“接收”数据比想要“接收”数据少的情况，那么就需要开发者针对这些情况进行很复杂的处理以保证数据的完整性

##ScktComp简介

ScktComp支持阻塞IO和非阻塞IO两种，非阻塞IO是使用select、WSAAsyncSelect配合事件回调机制来工作的，在[《ScktComp的回调逻辑》](http://www.xumenger.com/scktcomp-test-20170329/)一文中对于这部分的运行现象已经做了简单的介绍

如果是一个完善的非阻塞式网络库，针对上面提到的两种特殊情况（内核态发送缓冲区空间不足以放下要发送的数据、内核态接收区中的数据比要收的数据少）不应该让开发者进行特殊处理，而应该在网络库里面进行封装以方便开发者直接调用，一个简单的处理办法就是：

* 当内核态发送缓冲区空间不足时，应该由网络库申请用户态缓冲区，将剩下的未写入内核态发送缓冲区的数据先放到用户态缓冲区中，等到合适的机会再进行发送，这时候返回“发送成功”；或者发现发送的数据太多导致用户态发送缓冲区也太大，那么就返回发送失败。对于调用网络库发送接口的用户来说只需要关注发送是否成功，处理的数据粒度是每次发送的数据大小，而不需要考虑某次发送数据，前n个字节发送成功，剩下的m个字节发送失败的情况
* 接收部分，应该是由网络库持续先将数据从内核态接收缓冲区中读到用户态接收缓冲区中，然后开发者调用网络库的接收API是直接从用户态接收缓冲区中读数据，如果用户态接收缓冲区中有足够多的数据，那么调用成功，一旦用户态接收缓冲区中的数据不够，哪怕少一个字节都直接返回调用失败，并且不返回任何数据。如此将接收的粒度也控制在每次读的数据大小，而不需要考虑接收一部分的情况

但是我们看一下ScktComp这个网络库的发送和接收API是怎么实现的？！

比如发送数据的sendbuf函数，其中就只是直接调用send()这个Socket API而已，完全没有考虑如果内核发送缓冲区满了的情况去自己在用户态缓冲数据等到合适的时机再发送，还需要开发者自己去处理内核发送缓冲区满了的情况！


```
function TCustomWinSocket.SendBuf(var Buf; Count: Integer): Integer;
var
	ErrorCode: Integer;
begin
	Lock;
	try
		Result := 0;
		if not FConnected then Exit;

		Result := send(FSocket, Buf, Count, 0);
		if Result = SOCKET_ERROR then
		begin
			ErrorCode := WSAGetLastError;

			if (ErrorCode <> WSAEWOULDBLOCK) then
			begin
				Error(Self, eeSend, ErrorCode);
				Disconnect(FSocket);
				if ErrorCode <> 0 then
					raise ESocketError.CreateResFmt(@sWindowsSocketError,
						[SysErrorMessage(ErrorCode), ErrorCode, 'send']);
			end;
		end;
	finally
		Unlock;
	end;
end;
```

接收倒是通过OnRead回调通知开发者，但实际读还是需要开发者通过调用ReceiveBuf来读取内核接收缓冲区中的数据。ReceiveBuf其实还是直接调用recv()这个API，如果内核接收缓冲区中的数据量不足，那么还需要开发者自己做一些特殊处理

```
function TCustomWinSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
var
	ErrorCode: Integer;
begin
	Lock;
	try
		Result := 0;

		if (Count = -1) and FConnected then
			ioctlsocket(FSocket, FIONREAD, Longint(Result))
		else begin
			if not FConnected then Exit;
			Result := recv(FSocket, Buf, Count, 0);
			if Result = SOCKET_ERROR then
			begin
				ErrorCode := WSAGetLastError;
				if ErrorCode <> WSAEWOULDBLOCK then
				begin
					Error(Self, eeReceive, ErrorCode);
					Disconnect(FSocket);
					if ErrorCode <> 0 then
						raise ESocketError.CreateResFmt(@sWindowsSocketError,
							[SysErrorMessage(ErrorCode), ErrorCode, 'recv']);
				end;
			end;
		end;
	finally
		Unlock;
	end;
end;
```

所以我们可以看到ScktComp这个网络库是一个封装不充分的网络库，太多的非阻塞网络处理的细节还需要使用网络库的开发者来进行考虑！








