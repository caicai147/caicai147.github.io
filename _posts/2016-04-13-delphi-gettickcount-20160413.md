---
layout: post
title: 使用Delphi的GetTickCount函数的注意事项（简单涉及到Timer）
categories: delphi之控件 delphi之函数  delphi之时间日期
tags: delphi 日期 时间 timer
---

##程序运行效果

* 具体的运行效果可以下载该程序编译运行以直观感受
* Timer运行过程中修改配置的Sleep时间，程序是可以及时响应的
* 试着在该实例程序运行的时候从0至33修改sleep的时间，程序将会统计Sleep前和Sleep后的时间间隔
  * 分别在Sleep执行前和执行后调用GetTickCount，然后获取两者的差值以获取时间间隔的
  * 允许调整Sleep的时间为0至33ms，正常设置Sleep(1)的话，最后统计的时间间隔将是1，但是实际运行效果是0
  * 设置Sleep(7)的话，按照预期，最后统计的时间间隔应该是7，但是运行该程序，发现统计的值有0、15、16，但是没有其他值
  * 试了从0到33的所有值，发现无论设置的时间是多少，最后使用GetTickCount统计的时间间隔只有0、15、16、31、32这些值
  * 而且各个值的出现的频率也是根据配置的值不同的而不同
  * 所以猜测GetTickCount获取时间的间隔是15、16ms
* 该博客对应的例程的代码地址[在这里](../download/20160413/GetTickCount.zip)

##GetTickCount函数

* GetTickCount函数是用于获取从开机到现在的时间（单位是毫秒）
* 了解更多可以参见以下的博客
	* [Delphi时间与相关类型(1)-TDateTime与Double](http://www.xumenger.com/delphi-datetime-01-20160304/)
	* [Delphi时间与相关类型(2)-TDate、TTime、TTimeStamp](http://www.xumenger.com/delphi-datetime-02-20160304/)
* 但是每次执行GetTickCount并不能精确到1ms，有可能是15ms

假如实现如下的SleepEx函数用于防止程序的假死，如果以SleepEx(1)方式调用该函数让当前线程休眠，那么不可能休眠1ms，因为GetTickCount的特点，将会休眠15ms。

所以如果使用该方式封装了一个函数去进行休眠需要注意有很严重的误差，因为GetTickCount大概是每15ms获取一次值

```
procedure SleepEx(SleepTime: Cardinal);
var
  t: Cardinal;
begin
  t := GetTickCount;
  while GetTickCount - t < SleepTime do
  begin
    Sleep(1);
    Application.ProcessMessages;
  end;
end;
```

##关于Timer的疑问

* 以这个程序为例，其中实时统计并展示信息是使用Timer控件来实现的
* 使用Timer，当Timer正在运行的时候，如果去拖动窗体，明显感觉到卡顿
* 如果将Timer的Enabled设置为False，也就是其不运行，拖动窗体，没有卡顿
* 以这个程序为例，当Timer正在运行的时候
	* 将鼠标放在关闭进程的X上面，点击但是别放开，明显看到Memo中的数字不再变化，说明其不再运行，不再响应消息
	* 当把鼠标从X上移开之后松开按下的左键，明显Memo中的信息又变化了，说明其又开始响应消息了
* 怎么从消息机制的层面解释这个现象？怎么从消息机制的层面理解Timer的运行原理

##直接展示这个例程的代码

```
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    tmr1: TTimer;
    mmo1: TMemo;
    lbl1: TLabel;
    edt1: TEdit;
    ud1: TUpDown;
    procedure btn1Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure ZeroCountArray;

var
  Form1: TForm1;
  Count: array[0..35] of Integer;
  mointorInfo: string;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  self.BorderStyle := bsDialog;
  self.Caption := '测试GetTickCount函数';
  btn1.Caption := '开始';
  tmr1.Enabled := False;    //开始时不启动Timer
  tmr1.Interval := 1;       //设置Timer响应的时间间隔是1ms
  mmo1.Text := '';

//情况计数数组的所有元素都是0
  ZeroCountArray;

//准备格式化字符串
  for i := 0 to 35 do
  begin
    mointorInfo := mointorInfo +  ' ' + IntToStr(i) + ' ：%' + IntToStr(i) + ':d'#13#10;
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  if tmr1.Enabled then
  begin
    tmr1.Enabled := False;
    btn1.Caption := '开始';
  end
  else
  begin
    ZeroCountArray;
    tmr1.Enabled := True;
    btn1.Caption := '暂停';
  end;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
var
  time, i: Cardinal;
  showStr: string;
begin
  tmr1.Enabled := False;
  time := GetTickCount;       //获取Sleep之前的“时间”
  Sleep(ud1.position);        //按照在界面的配置进行Sleep
  i := GetTickCount - time;   //获取Sleep之后的“时间”
  Inc(Count[i]); 
  showStr := Format(mointorInfo, [Count[0], Count[1], Count[2], Count[3], Count[4], Count[5], Count[6], Count[7], Count[8], Count[9], Count[10], Count[11],
                                  Count[12],Count[13],Count[14],Count[15],Count[16],Count[17],Count[18],Count[19],Count[20],Count[21],Count[22], Count[23],
                                  Count[24],Count[25],Count[26],Count[27],Count[28],Count[29],Count[30],Count[31],Count[32],Count[33],Count[34], Count[35]]);

  mmo1.Text := showStr; 
  tmr1.Enabled := True;
end;

procedure ZeroCountArray;
var
  i: Integer;
begin
  for i:= 0 to 35 do
  begin
    Count[i] := 0;
  end;
end;

end.
```
