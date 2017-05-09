---
layout: post
title: 实例展示Delphi共享内存的使用【续】
categories: delphi之指针与内存 delphi之消息机制 windows之消息机制 delphi之多进程
tags: delphi 共享内存 指针 内存 消息 消息机制 互斥量 多进程
---

## 情况一测试

* 在[《实例展示Delphi共享内存的使用【例程】》](http://www.xumenger.com/windows-delphi-sharemem-ipc-20160507/)
* 就那篇文章中的例程，想到这样的情况：
  * 假如拷贝一份AppB1，就相当于AppB和AppB1的窗体的Caption都一样
  * 那么AppA使用FindWindow获取其他进程的窗体句柄时应该两个都可以获取
  * AppB和AppB1都会在开始的时候映射到AppA创建的共享内存上
  * 那么AppA发消息时，AppB和AppB1运行的情况是什么样的？
  * AppB和AppB1都能收到消息，还是只有一个能收到消息
  * 如果只有一个能收到消息，那么是哪个能收到消息
  * 如果都能收到消息，那么是不是都可以读取共享内存中的字符串
* 那就实际测试一下
  * 发现同时只能有一个进程收到AppA发送的消息，收到能收到消息的进程只有一个
  * 是最后一个获取鼠标聚焦对的进程可以收到消息，在下面的进程无法获取，也就无发展示共享内存中的值
  * 对于AppB、AppB1不管是谁最后一个获取鼠标聚焦，都能获取共享内存中的值
  
## 情况二测试

* 针对第一种情况再进行深入的测试
* 拷贝一份AppB的源码，原来的AppB的源码不修改做修改
* 修改拷贝出来的另一份工程进行修改，编译生成AppB1
  * AppB1的工程名为AppB.dpr
  * AppB1的窗体的Caption为'测试读共享内存1'
* AppA的源码也做适当修改
  * 往共享内存中写完字符串之后，同时给AppB和AppB1法消息
  * 使用FindWindow，通过'测试读共享内存'找到AppB的窗体
  * 使用FindWindow，通过'测试读共享内存1'找到AppB1的窗体
* 如此修改之后，不管AppB还是AppB1哪个是最后被聚焦的，都能收到消息并展示共享内存中的信息
* 可以点击[这里](../download/20160511/Windows-Delphi-ShareMem-2.zip)下载

## 情况二源码展示

#### AppA

```
unit FormA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;
const
  WM_DATA=WM_User + 1025;

type
  PShareMem = ^TShareMem;
  TShareMem = record
    Data: array[0..255] of Char;
  end;

  TForm1 = class(TForm)
    btn1: TButton;
    mmo1: TMemo;
    lbl1: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pshare: PShareMem;

implementation

{$R *.dfm}
var
  hMapping: THandle;
  hMapMutex: THandle;
const
  mapFileSize = 1000;
  request_TimeOut = 1000;

procedure openMap;
begin
  hMapping := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, SizeOf(TShareMem), PChar('testShareMem'));
  if 0 = hMapping then
  begin
    ShowMessage('创建内存映像文件失败');
    Application.Terminate;
  end;
  //将映像文件映射到内存的地址空间
  pshare := PShareMem(MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0));
  if nil = pshare then
  begin
    CloseHandle(hMapping);
    ShowMessage('显示内存映像文件失败');
    Application.Terminate;
    Exit;
  end;
end;

//关闭共享内存映像
procedure closeMap;
begin
  if nil <> pshare then
  begin
    UnmapViewOfFile(pshare);      //从内存地址空间中释放映像文件
  end;
  if 0 <> hMapping then
  begin
    CloseHandle(hMapping);
  end;
end;

//对共享内存加锁，使用互斥对象，因为互斥对象是系统级资源，是可以跨进程使用的
function LockMap: Boolean;
begin
  Result := True;
  hMapMutex := CreateMutex(nil, False, PChar('Mutex peidw'));
  if 0 = hMapMutex then
  begin
    ShowMessage('创建互斥对象失败');
    Result := False;
  end
  else
  begin
    if WAIT_FAILED = WaitForSingleObject(hMapMutex, request_TimeOut) then
    begin
      ShowMessage('对互斥对象加锁失败');
      Result := False;
    end;
  end;
end;

//释放互斥对象
procedure UnLockMap;
begin
  ReleaseMutex(hMapMutex);
  CloseHandle(hMapMutex);
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  str: PChar;
begin
  str := PChar(mmo1.Text);
  if 256 < Length(str) then
  begin
    ShowMessage('输入的字符串长度超过256');
    Exit;
  end;

//使用CopyMemory方法，假如原来的共享内存中的内容是123456 ，第二次再次赋值为abc
  //这时候看到在共享内存中的内容还是abc456 ，因为使用CopyMemory方法拷贝的长度只有Length(str)
  //所以原来共享内存中字符串是123456 ，长度是6 ，再次拷贝的字符串长度只有3 ，所以只会覆盖前三个字符
  CopyMemory(@(pshare^.Data), str, Length(str));

//通过FindWindows找到另一个进程的某个窗体的handle，FindWindows的两个参数：
  //第一个是要找到的窗口类名，第二个是要找的窗体的Caption，这两个不一定都能找到，但是至少需要一个
  //这个例子中就是没有写明窗体类名，只有另一个进程的主窗体的Caption
//这个程序首先是用于演示通过共享内存在两个进程之间进行通信，但是通过发消息的方式也是可以进行通信
  //这个例子就是程序A通过发消息告诉程序B 共享内存有内容了，可以读取了，然后程序B 从共享内存获取内容进行展示
//如果有同样的窗体类和Caption一样的窗体怎么办？需要好好研究这个FindWindow方法
  PostMessage(FindWindow(nil, '测试读共享内存'), WM_DATA, 1, 1);

  PostMessage(FindWindow(nil, '测试读共享内存1'), WM_DATA, 1, 1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mmo1.Text := '';
  openMap;
  LockMap;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  UnLockMap;
  CloseMap;
end;

end.
```

#### AppB和AppB1的程序源码

* AppB和AppB1的源码几乎相同，在以下几点有区别
* AppB进程对应的Delphi工程名是AppB.dpr；AppB1进程对应的Delphi工程名是AppB1.dpr
* AppB进程的窗体的Caption是'测试读共享内存'；AppB1进程的窗体的Caption是'测试读共享内存1'

```
unit FormB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;
const
  WM_DATA = WM_USER + 1025;

type
  PShareMem = ^TShareMem;
  TShareMem = record
    Data: array[0..255] of Char;
  end;

  TForm1 = class(TForm)
    mmo1: TMemo;
    btn1: TButton;
    lbl1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure getShareInfo(var msg: TMessage); message WM_DATA;
  end;

var
  Form1: TForm1;
  pshare: PShareMem;
  hMapping: THandle;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  CloseHandle(hMapping);
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mmo1.Text := '';
//定位到内存映射文件，其中第三个参数Pchar('testShareMem')
  //对应于创建共享内存的进程调用的 hMapping := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, SizeOf(TShareMem), PChar('testShareMem'));
  //的CreateFileMapping方法的第6 个参数
  hMapping := OpenFileMapping(FILE_MAP_WRITE, False, PChar('testShareMem'));
  if 0 = hMapping then
  begin
    ShowMessage('定位内存映射文件块失败，应该是进程A未开启');
    halt; //异常终止
  end;
  //将映像文件映射到进程的地址空间，这样就是在进程A和进程B之间可以共同访问这块内存了
  pshare := PShareMem(MapViewOfFile(hMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0));
  if nil = pshare then
  begin
    CloseHandle(hMapping);
    ShowMessage('将映像到映射到进程地址空间失败');
    Application.Terminate;
    Exit;
  end;
  FillChar(pshare^, SizeOf(TShareMem), 0);  //初始化地址空间
end;

//用于响应消息WM_DATA，这个消息是从另一个进程发送过来的，所以进程间通信也是可以通过发送消息的方式实现的
procedure TForm1.getShareInfo(var msg: TMessage);
begin
  if 1 = msg.LParam then
  begin
    //获取共享内存中的字符串信息展示到TMemo控件中
    mmo1.Text := pshare^.Data;
  end;  
end;

end.
```
