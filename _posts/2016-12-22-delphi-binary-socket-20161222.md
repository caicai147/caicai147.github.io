---
layout: post
title: Delphi网络编程：发送和接收二进制数据
categories: delphi之网络编程 深入学习之网络原理 深入学习之内存管理 delphi之指针与内存
tags: delphi 二进制 网络 Delphi网络编程 汉字 编码 GBK
---

说到文本文件，可能想到与之对应的是二进制文件。其实它们两并不是对立的关系，还是包含的关系：二进制文件包括文本文件，因为文本最本质也是以二进制（0、1）的形式存在的。文本文件中的内容主要是对人类可读的文本信息，主要是汉字、字母、数字，但其在底层还是通过二进制的0和1来表示的，比如英文字母“a”对应二进制：01100001，字符“A”对应的二进制：01000001，数字“2”对应二进制：00110010，汉字“我”对应二进制（GBK编码）：1100111011010010

其实Socket天然支持的是发送二进制文件，可以看一下Windows SockAPI中send的函数原型

```
int PASCAL FAR send (SOCKET s, const char FAR * buf, int len, int flags);
```

比如可以将buf指向一个结构体变量的首地址，len表示该结构体的大小，然后调用send就可以将结构体变量在进程内存中的二进制内容发送出去了。只不过平时接触的较多的是直接发送字符串，比如HTTP这种文本格式，还有金融开发中常用的FIX包、XML包等文本形式的网络包（至少我是这样的）

>总有一些先入为主的部分事实遮住人的认知，误以为这就是全部事实！

##Delphi发送和接收二进制数据

Delphi的ClientSocket、ServerSocket其实是将底层的WinSockAPI又做了封装然后供开发者调用的，所以其实Delphi在进行网络编程时也可以发送和接收二进制数据

接下来通过一个例子展示客户端发送二进制数据，服务端接收二进制数据。对应的程序源码点击[这里](../download/20161222/Binary.rar)下载

规定发送数据的格式如下：前4个字节是Integer型数据，表示接下来的字符串的长度，然后接下来就是指定长度的字符串

**客户端代码**

```
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScktComp;

type
  TForm1 = class(TForm)
    lblIP: TLabel;
    lblPort: TLabel;
    edtIP: TEdit;
    edtPort: TEdit;
    btnConnect: TButton;
    lblMessage: TLabel;
    edtMessage: TEdit;
    btnSend: TButton;
    mmoMessage: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketRead(Sender: TObject; Socket: TCustomWinSocket);
  end;

var
  Form1: TForm1;
  Client: TClientSocket;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Client := TClientSocket.Create(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
var
  IP: string;
  Port: Integer;
begin
  IP := edtIP.Text;
  Port := StrToInt(edtPort.Text);
  Client.Host := IP;
  Client.Port := Port;
  Client.OnConnect := SocketConnect;
  Client.OnRead := SocketRead;
  Client.Open;
end;

procedure TForm1.btnSendClick(Sender: TObject);
var
  SendMsg: string;
  MsgLen: Integer;
  buf: array of Byte;
begin
  SendMsg := edtMessage.Text;
  MsgLen := Length(SendMsg);
  
  //设置Byte数组长度是字符串长度+整型长度
  SetLength(buf, MsgLen + 4);

  //将整型拷贝到Byte数组中
  Move(MsgLen, buf[0], 4);

  //将字符串拷贝到Byte数组中
  Move(SendMsg[1], buf[4], MsgLen);

  //发送二进制内容
  Client.Socket.SendBuf(buf[0], MsgLen + 4);
end;

procedure TForm1.SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  Form1.mmoMessage.Lines.Add('连接成功');
end;

procedure TForm1.SocketRead(Sender: TObject; Socket: TCustomWinSocket);
var
  buf: array of Byte;
  respTextLen: Integer;
  respText: string;
begin
  try
    if Socket.ReceiveLength > 0 then
    begin
      //前4节，长度
      Socket.ReceiveBuf(respTextLen, 4);
      
      //剩余字节：消息内容
      SetLength(buf, respTextLen);
      Socket.ReceiveBuf(buf[0], respTextLen);

      SetLength(respText, respTextLen);
      Move(buf[0], respText[1], respTextLen);
      
      Form1.mmoMessage.Lines.Add('应答长度：' + IntToStr(respTextLen) + '；内容：' + respText);
    end;
  except
    on E: Exception do
    begin
      Form1.mmoMessage.Lines.Add('接收应答出现异常！');
    end;
  end;
end;

end.
```

**服务端代码**

```
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScktComp;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    lblIP: TLabel;
    edtIP: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    btnStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure OnClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure OnClientRead(Sender: TObject; Socket: TCustomWinSocket);
  end;

var
  Form1: TForm1;
  Server: TServerSocket;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Server := TServerSocket.Create(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Close;
  Server.Free;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var
  IP: string;
  Port: Integer;
begin
  IP := edtIP.Text;
  Port := StrToInt(edtPort.Text);

  Server.Port := Port;
  Server.OnClientConnect := OnClientConnect;       //当有某个客户端连接上后，回调该方法
  Server.OnClientRead := OnClientRead; 
  Server.Open;

  mmo1.Lines.Add('服务器启动成功！');
end;

procedure TForm1.OnClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  mmo1.Lines.Add('接收到客户端的连接！');
end;

procedure TForm1.OnClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  reqLength: Integer;
  reqBuf: array of Byte;
  reqText: string;

  respLength: Integer;
  respBuf: array of Byte;
  respText: string;
begin
  if Socket.ReceiveLength > 0 then
  begin
    //先接收前4个字节
    Socket.ReceiveBuf(reqLength, 4);

    //再接收后续的字符串 
    SetLength(reqBuf, reqLength);
    Socket.ReceiveBuf(reqBuf[0], reqLength);
    SetLength(reqText, reqLength);
    Move(reqBuf[0], reqText[1], reqLength);
    mmo1.Lines.Add('请求长度：' + IntToStr(reqLength) + '；内容：' + reqText);

    //返回应答
    respText := 'Hello Client';
    respLength := Length(respText);
    SetLength(respBuf, respLength + 4);
    Move(respLength, respBuf[0], 4);
    Move(respText[1], respBuf[4], respLength);

    Socket.SendBuf(respBuf[0], respLength + 4);
    mmo1.Lines.Add('返回应答：' + respText);
  end;
end;

end.
```

**运行效果展示**

首先是打开服务端程序，监听8090，开启服务

![image](../media/image/2016-12-22/01.png)

然后客户端连接到服务端

![image](../media/image/2016-12-22/02.png)

输入内容，点击发送

![image](../media/image/2016-12-22/03.png)

然后可以看到服务端收到请求

![image](../media/image/2016-12-22/04.png)

继续看到客户端收到应答

![image](../media/image/2016-12-22/05.png)
