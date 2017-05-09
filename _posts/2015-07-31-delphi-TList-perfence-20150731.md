---
layout: post
title: Delphi容器类之---Tlist，TStringlist，THashedStringlist的效率比较
categories: delphi之容器类 软件质量之性能
tags: delphi tlist tobjectlist 
---


转载自：[http://www.ylzx8.cn/windows/delphi/73200.html](http://www.ylzx8.cn/windows/delphi/73200.html)


本人在做一个测试，服务器是IOCP的，我假定最大链接数是50000个。

**测试背景**：如果每个链接之间的数据需要服务器中转的话，那么我需要一个数据容器储存用户的关键数据和连接。

我简单的做了一段简单的代码，关键代码段如下：

    RTest = record
        Key: Integer;
        Name: String[20];
    end;
    PTest = ^RTest;
    TListType = (ltList, ltStrList, ltHashList);
    
    var
        List: TList;
        StrList: TStringList;
        HashList: THAshStringList;
    
    procedure TfmMain.addToList(Target: TListType);
    var
        Total, I: Integer;
        P_test: PTest;
        T, E, Cardinal;
    begin
        Total:= 5000000;    {此处是关键，5万的时候几乎没有任何区别，500万的时候，区别就特别大了}
        T:= GetTickCount;
        case Target of
            ltList:
            begin
                for I:=0 to Total do
                begin
                    New(P_test);
                    P_test.Key:= List.Count + 1;
                    P_test.Name:= 'Tommy' + IntToStr(List.Count + 1);
                    List.Add(P_test);
                end;
                E:= GetTickCount;
                mmoInfo.Lines.Add('添加5W条记录到List中需要' + TntToStr(E) + '-' + IntToStr(T) + '=' + IntToStr(E-T));
                mmoInfo.Lines.Add('List.Count := '+IntToStr(List.Count));
                mmoInfo.Lines.Add('最后一项：'+PTest(List.Last).Name);
                mmoInfo.Lines.Add('');
            end;
            ltStrList:
            begin
                for I:=0 to Total do
                begin
                    New(P_test);
                    P_test.Key:= StrList.Count + 1;
                    P_Test.Name:= 'Tommy' + IntToStr(StrList.Count+1);
                    StrList.AddObject(IntToStr(StrList.Count), TObject(Integer(P_test)));
                end;
                E:= GetTickCount;
                mmoInfo.Lines.Add('添加5W条记录到StrList中需要'+IntToStr(E) + ' - ' + IntToStr(T) + ' = ' + IntToStr(E - T));
                mmoInfo.Lines.Add('StrList.Count := '+IntToStr(StrList.Count));
                mmoInfo.Lines.Add('最后一项：'+ PTest(StrList.Objects[StrList.Count - 1]).Name);
                mmoInfo.Lines.Add('');
            end;
            ltHashList:
            begin
                for I := 0 to Total do
                begin
                    New(P_test);
                    P_test.Key:= HashList.Count + 1;
                    P_test.Name:= 'Tommy' + IntToStr(HashList.Count + 1);
                    HashList.AddObject(IntToStr(HashList.Count),TObject(Integer(P_test)));
                end;
                E:= GetTickCount;
                mmoInfo.Lines.Add('添加5W条记录到HashList中需要'+IntToStr(E) + ' - ' + IntToStr(T) + ' = ' + IntToStr(E - T));
                mmoInfo.Lines.Add('HashList.Count := '+IntToStr(HashList.Count));
                mmoInfo.Lines.Add('最后一项：'+ PTest(HashList.Objects[HashList.Count - 1]).Name);
                mmoInfo.Lines.Add('');
            end;
        end;
    end;  
    
    procedure TfmMain.btnFindClick(Sender: TObject);
    var 
        I,X: Integer;
        P_Tmp: PTest;
        T,E: Cardinal;
    begin
        T:= GetTickCount;
        for I := 100 to 300 do
        begin
            X:= StrList.IndexOf(IntToStr(I));
            if X <> -1 then
            begin
                P_Tmp :=PTest(StrList.Objects[X]); 
            end;
        end;
        E:= GetTickCount;
        mmoInfo.Lines.Add('StrList中查找200项耗时：' +  IntToStr(E - T));
        T:= GetTickCount;
        for I := 100 to 300 do
        begin
            X:= HashList.IndexOf(IntToStr(I));
            if X <> -1 then
            begin
                P_Tmp :=PTest(HashList.Objects[X]);
            end;
        end;
        E:= GetTickCount;
        mmoInfo.Lines.Add('HashList中查找200项耗时：'+  IntToStr(E - T));
    end;
    
    procedure TfmMain.FormCreate(Sender: TObject);
    begin
        List:=     TList.Create;
        StrList:=  TStringList.Create;
        HashList:= THashedStringList.Create;
        addToList(ltList);
        addToList(ltStrList);
        addToList(ltHashList);
    end;
    
    procedure TfmMain.FormDestroy(Sender: TObject);
    begin
        List.Free;
        StrList.Free;
        HashList.Free;
    end;


测试的时候发现，如果是50000 条数据，那么，这样简单的测试任何效率都看不出来，基本都是在0--20毫秒以内完成添加、查询。

当我加大数量时，变成500万条时，测试的输出结果是：

    添加500W条记录到List中需要10015562 - 10014859 = 703
    List.Count := 5000001
    最后一项：Tommy5000001
    
    添加500W条记录到StrList中需要10016703 - 10015562 = 1141
    StrList.Count := 5000001
    最后一项：Tommy5000001
    
    添加500W条记录到HashList中需要10017859 - 10016703 = 1156
    HashList.Count := 5000001
    最后一项：Tommy5000001
    
    StrList中查找200项耗时：0
    HashList中查找200项耗时：1344
    StrList中查找200项耗时：0
    HashList中查找200项耗时：328
    StrList中查找200项耗时：0
    HashList中查找200项耗时：328
    StrList中查找200项耗时：15
    HashList中查找200项耗时：313
    StrList中查找200项耗时：16
    HashList中查找200项耗时：312
    StrList中查找200项耗时：16
    HashList中查找200项耗时：312
    StrList中查找200项耗时：0
    HashList中查找200项耗时：328
    StrList中查找200项耗时：0
    HashList中查找200项耗时：313

　　
### 分析

时间都浪费在new(P_test)、List.add/StrList.Add/HashList.Add（里面实施不停的reallocmem）。

查找是HASH快（自带的不算），hash算法不算难，自己写一个，不然弄个二分法都快过于TStringList。

THashedStringList是继承自TStringList的，只是它覆盖了IndexOf、IndexOfName两个查询方法，扩充易用性，追求效率应该用TStringList，TStringList是直接从TObject继承来的数组链表。

你也可以看看contnrs单元，里面有队列、栈、启发式哈希表。

另外，delphi的list都有capacity属性，用于一次性指定count数，而不是每次重新申请list的长度。因为add时，list发现其count不够，会自动从新分配内存扩充自身长度，这些都是耗时的过程，特别在大长度的循环中，如果长度是可知的，这里你可以在循环前给list.capacity:=total;