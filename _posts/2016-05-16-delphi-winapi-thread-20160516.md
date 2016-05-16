---
layout: post
title: Delphi的TThread类和Win32的线程方法
categories: delphi之线程 delphi之系统调用 深入学习之多线程
tags: delphi WinAPI Windows 多线程 线程
---

##Delphi和WinAPI的关系

其实Delphi的TThread实现就是对WinAPI的相关线程方法的封装，比如

####TThread的Create方法实现

注意其中的WinAPI的BeginThread方法：

* 配合TThread.Create调用BeginThread传入@ThreadProc
* 理解ThreadProc通过调用Execute实现（Execute由开发者自己实现）

```
constructor TThread.Create(CreateSuspended: Boolean);
{$IFDEF LINUX}
var
  ErrCode: Integer;
{$ENDIF}
begin
  inherited Create;
  AddThread;
  FSuspended := CreateSuspended;
  FCreateSuspended := CreateSuspended;
{$IFDEF MSWINDOWS}
  FHandle := BeginThread(nil, 0, @ThreadProc, Pointer(Self), CREATE_SUSPENDED, FThreadID);
  if FHandle = 0 then
    raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(GetLastError)]);
{$ENDIF}
{$IFDEF LINUX}
  sem_init(FCreateSuspendedSem, False, 0);
  ErrCode := BeginThread(nil, @ThreadProc, Pointer(Self), FThreadID);
  if ErrCode <> 0 then
    raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(ErrCode)]);
{$ENDIF}
end;
```

####TThread的ThreadProc方法实现

* ThreadProc方法会在TThread.Create调用WinAPI的BeginThread方法中会传入这个方法的地址
* Execute其实就是在ThreadProc中被调用的，我们开发线程的时候就是实现Execute方法

```
function ThreadProc(Thread: TThread): Integer;
var
  FreeThread: Boolean;
begin
{$IFDEF LINUX}
  if Thread.FSuspended then sem_wait(Thread.FCreateSuspendedSem);
{$ENDIF}
  try
    if not Thread.Terminated then
    try
      Thread.Execute;
    except
      Thread.FFatalException := AcquireExceptionObject;
    end;
  finally
    FreeThread := Thread.FFreeOnTerminate;
    Result := Thread.FReturnValue;
    Thread.FFinished := True;
    Thread.DoTerminate;
    if FreeThread then Thread.Free;
{$IFDEF MSWINDOWS}
    EndThread(Result);
{$ENDIF}
{$IFDEF LINUX}
    // Directly call pthread_exit since EndThread will detach the thread causing
    // the pthread_join in TThread.WaitFor to fail.  Also, make sure the EndThreadProc
    // is called just like EndThread would do. EndThreadProc should not return
    // and call pthread_exit itself.
    if Assigned(EndThreadProc) then
      EndThreadProc(Result);
    pthread_exit(Pointer(Result));
{$ENDIF}
  end;
end;
```

####TThread的Resume实现

```
procedure TThread.Resume;
var
  SuspendCount: Integer;
begin
  SuspendCount := ResumeThread(FHandle);
  CheckThreadError(SuspendCount >= 0);
  if SuspendCount = 1 then
    FSuspended := False;
end;
```

####TThread的Suspend实现

```
{$IFDEF MSWINDOWS}
procedure TThread.Suspend;
begin
  FSuspended := True;
  CheckThreadError(Integer(SuspendThread(FHandle)) >= 0);
end;
```

####更多的代码实现请详细研究TThread类

##重点说明线程的挂起和唤醒

通过下面的代码实例，以及代码中的注释深入理解：

* Delphi的Resume和Suspend，以及WinAPI的ResumeThread和SuspendThread！
* Resume和Suspend调用时调用次数的关系
* ResumeThread和SuspendThread的返回值
  * SuspendThread将线程被暂停，将暂停计数增加1，并返回当前的暂停计数
  * 假如线程的运行状态是正在运行，第一次调用SuspendThread，返回暂停计数为0，线程被挂起
  * ResumeThread将唤醒线程，将暂停计数减少1，如果减少后计数是0 了，就不会在减了
  * ResumeThread第一次唤醒线程使其运行时，暂停计数是1，再次唤醒暂停计数变为0，然后暂停计数就不会再变了
* 参见[《关于TThread类的Suspend()方法和Resume()方法》](http://blog.csdn.net/beroy/article/details/1551832)

```
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  tt = class(TThread)
  public
    procedure Execute; override;
  end;

var
  Form1: TForm1;
  t1, t2: tt;

implementation

{$R *.dfm}

{TThread的方法和属性说明
    TThread的Resume方法其实就是调用了WinAPI的ResumeThread方法
    TThread的Suspend方法其实就是调用了WinAPI的SuspendThread方法
    请自己好好研究Delphi中Resume和Suspend的代码实现
    TThread的Suspended属性用于判断当前线程是不是被挂起
 BoolToStr(True)值为 -1   BoolToStr(False)值为 0

 因为调用的WinAPI，所以可以看到虽然线程已经开始运行了，但是Suspended还是True
    因为Suspended的值是在调用Delphi的TThread的Resume和Suspend方法时
    调用ResumeThread和SuspendThread，同时修改Suspended的值
    但是现在直接调用WinAPI，所以Suspended的值不会变化
    所以注意：如果使用ResumeThread或者SuspendThread，那么就不要使用TThread的Suspended属性     }
procedure TForm1.btn1Click(Sender: TObject);
var
  suspendcount: Integer;
begin

  suspendcount := SuspendThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(1)1  -1

  suspendcount := SuspendThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(2)2  -1

  suspendcount := SuspendThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(3)3  -1

  suspendcount := ResumeThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(4)4  -1

  suspendcount := ResumeThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(5)3  -1

  suspendcount := ResumeThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(6)2  -1

  suspendcount := ResumeThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(7)1  -1
                                                                                                     //此时线程已经被唤醒了
                                                                                                     //线程第一次被唤醒时SuspendCount的值为1
                                                                                                     //当ResumeThread的返回值为
  suspendcount := ResumeThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(8)0  -1
                                                                                                     //suspendCount为1时，再次调用

  suspendcount := ResumeThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(9)0  -1
                                                                                                     //SuspendThread会持续增加suspendCount
                                                                                                     //但是ResumeThread将Suspend的值减小到0后便不再减小

  suspendcount := SuspendThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(10)0  -1
                                                                                                     //线程已经被挂起
                                                                                                     //线程第一次被挂起时，SuspendCount为0
  suspendcount := SuspendThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(11)1  -1
                                                                                                     //线程已经被挂起，挂起次数加一
  suspendcount := SuspendThread(t1.Handle);
  ShowMessage('挂起次数:' + IntToStr(suspendcount) + '; 是否挂起:' + BoolToStr(t1.Suspended));       //(12)2  -1
                                                                                                     //线程已经被挂起，挂起次数再加一
end;

{测试调用Delphi封装的Resume和Suspend方法时候的线程运行情况
  实际的线程的运行效果和上面使用WinAPI的效果一致
  从1-12的线程运行状态和上面的btn1Click的1-12的各个运行状态一致
  用于展示DelphiAPI和WinAPI的调用关系}
procedure TForm1.btn2Click(Sender: TObject);
begin
{TThread的Resume方法其实就是调用了WinAPI的ResumeThread方法
 TThread的Suspend方法其实就是调用了WinAPI的SuspendThread方法

 TThread的Suspended属性用于判断当前线程是不是被挂起

 BoolToStr(True)值为 -1   BoolToStr(False)值为 0}

  t2.Suspend;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(1)-1

  t2.Suspend;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(2)-1

  t2.Suspend;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(3)-1

  t2.Resume;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(4)-1

  t2.Resume;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(5)-1

  t2.Resume;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(6)-1

  t2.Resume;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(7)0   线程已经唤醒 ！多次挂起需要多次唤醒线程才能继续运行

  t2.Resume;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(8)0

  t2.Resume;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(9)0

  t2.Suspend;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(10)-1  多次唤醒后，只要一次挂起，线程就停止运行

  t2.Suspend;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(11)-1

  t2.Suspend;
  ShowMessage('是否挂起:' + BoolToStr(t2.Suspended));       //(12)-1
end;

procedure tt.Execute;
begin
  While not Terminated do
  begin

    Sleep(100);
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  t1 := tt.Create(True);            //线程创建时不启动
  t2 := tt.Create(True);            //线程创建时不启动

  btn1.Caption := '测试WinAPI';
  btn2.Caption := '测试Delphi';
end;

end.

```
