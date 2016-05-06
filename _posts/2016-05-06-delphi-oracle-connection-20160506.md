---
layout: post
title: Delphi实现程序用以测试Oracle连接【例程】
categories: delphi之数据库 数据库之oracle delphi之控件
tags: delphi oracle TComboBox
---

##简介

* 该程序实现了测试连接Oracle的功能，在程序的窗体中输入用户名、密码、数据源即可测试数据库连接
* 其中简单涉及到TADOConnection的使用
* 其中简单讲到TComboBox的使用，一些常用的属性
* 注意其中数据库连接串的格式
* 具体的程序实例代码点击[这里](../download/20160506/TestADOConn.zip)进行下载

##程序实例

```
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, ADODB;
const
  ConnStr = 'Provider=%0:s;User ID=%1:s;Password=%2:s;Data Source=%3:s;Persist Security Info=True';

type
  TForm1 = class(TForm)
    btn1: TButton;
    edt1: TEdit;
    edt2: TEdit;
    edt3: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    cbb1: TComboBox;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  ConnectionString: string;
  adoConn: TADOConnection;
  adoQry: TADOQuery;
  sql: string;
begin
  try
    ConnectionString := Format(ConnStr, [cbb1.Text, edt1.Text, edt2.Text, edt3.Text]);
    adoConn:= TADOConnection.Create(nil);
    adoQry:= TADOQuery.Create(nil);
    adoConn.LoginPrompt := False;
    adoConn.ConnectionString := ConnectionString;

    adoConn.Connected := True;
    ShowMessage('连接成功');
    
//    sql := 'select count(*) cccc from tuser';
//    adoQry.Connection := adoConn;
//    adoQry.SQL.Text := sql;
//    adoQry.Open;
//    ShowMessage(PChar(IntToStr(adoQry.FieldByName('cccc').AsInteger)));

//    adoQry.Free;
    adoConn.Free;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbb1.Items.Add('OraOLEDB.Oracle.1');
  cbb1.Items.Add('MSDAORA.1');
  cbb1.ItemIndex := 0;                //程序启动时默认在窗体上显示第一项
  cbb1.Style := csOwnerDrawFixed;     //使下拉框内容不可编辑
//  cbb1.Style := csDropDown;         //使下拉框内容可编辑

  edt1.Text := '';
  edt2.Text := '';
  edt3.Text := '';
end;

end.
```
