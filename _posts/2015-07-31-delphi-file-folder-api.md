---
layout: post
title: Delphi中ExtractFilePath、ParamStr以及更多文件/目录操作涉及的函数
categories: delphi之文件操作
tags: delphi 文件 目录
---


先介绍ExtractFilePath和ParamStr
---

### ParamStr

该函数的原型是：`function paramstr(i: Integer): String;`

对于任何的application，paramstr(0)都默认代表的是应用程序的绝对路径。假如你这个编译号的exe 文件在 d:\delphi下，名字叫做 project1.exe。那么 在该应用程序中paramstr(0)返回：d:\delphi\project1.exe。这个值是变的，exe程序在哪，这个值就变为哪儿。

那么有paramstr(0),就肯定有paramstr(1)，paramstr(2)…了。它们的值又是什么了？我试了下取出的是空值，又不能赋值。

delphi帮助中说Returns a specified parameter from the command-line.从命令行中返回一个特别的参数。折腾了半天才搞清楚，就是在exe文件后面可以跟参数，paramstr 获取的就是exe文件后面跟参数。

如有可执行文件project1.exe 在运行（cmd命令行）中输入 e:\project1.exe 123 456 789。 那么paramstr(1)=’123′ paramstr(2)=’456′ paramstr(3)=’789′

用shellexecute的话可以在parameters参数位置输入值。不同的参数值之间用空格隔开。如 

    shellexecute(application.Handle,’open’,’project4.exe’,’123 456′, ‘e:\’,SW_SHOW);

所以用paramstr可以在应用程序间进行数据传输。
 

### ExtractFilePath

功能：返回完整文件名中的路径。 具体使用的方法参见例子：

    Extractfilepath(‘d:\delphi\project1.exe’) ;
    // 就等于 ‘d:\delphi\’ ，它就是把最后的文件名去掉,只要路径，且路径最后有 \

函数原型如下:`function ExtractFilePath(const FileName: string): string;`

ExtractFilePath和相近函数：

* ExtractFileDrive ：返回完整文件名中的驱动器，如”C:”
* ExtractFilePath：返回完整文件名中的路径，最后带“/”，如”C:\test\”
* ExtractFileDir：返回完整文件名中的路径，最后不带“/” ,如”C:\test”
* ExtractFileName：返回完整文件名中的文件名称 (带扩展名)，如”mytest.doc”
* ExtractFileExt： 返回完整文件名中的文件扩展名（带.），如”.doc”

上面这么多,就是取得当前可执行文件的当前路径，这里就有一个应用的示例：

    myini := TIniFile.Create(ExtractFilePath(ParamStr(0))+’Config.ini’);

config.ini 和你的可执行文件是在同一级目录下的,,这样就找到config.ini文件了.

 
再介绍更多常用的文件操作函数
---

* FileExists：判断文件是否存在
* DirectoryExists：判断文件夹是不是存在
* DeleteFile、Windows.DeleteFile：删除文件
* RemoveDir、RemoveDirectory：删除文件夹
* GetCurrentDir：获取当前文件夹
* SetCurrentDir、ChDir、SetCurrentDirectory：设置当前文件夹
* GetDir：获取指定驱动器的当前路径名
* RenameFile：文件重命名
* CreateDir、CreateDirectory、ForceDirectories：建立文件夹
* RemoveDir、RemoveDirectory：删除空文件夹
* FileCreate：建立新文件
* GetFileVersion：获取当前文件的版本号
* DiskSize、DiskFree：获取磁盘空间
* FindFirst、FindNext、FindClose：搜索文件
* FileGetAttr、FileSetAttr：读取与设置文件属性
* FileAge、FileDateToDateTime：获取文件的创建时间

### 以上函数的代码示例

#### FileExists：判断文件是否存在

    var
        f: String;
    begin
        f:= 'c:/temp/test.txt';
        if not FileExist(f) then
        begin
            //如果文件不存在
        end;
    end;

#### DirectoryExists：判断文件夹（路径）是否存在

    var
        dir: String;
    begin
        dir:= 'c:/temp';
        if not DirectoryExists(dir) then
        begin
            //如果文件夹不存在
        end;
    end;

#### DeleteFile、Windows.DeleteFile：删除文件

    var
        f: String;
    begin
        f:= 'c:/temp/test.txt';
        //DeleteFile(f);    //返回Boolean
        
        //或者使用系统API，也返回Boolean
        Windows.DeleteFile(PChar(f));    
        //注意这里需要对String类型进行类型转换，转换为PChar，我的理解是Windows的API需要兼容C和Delphi，
        //所以使用Char数组类型的字符串正好可以同时满足两种语言的结构
    end;

#### RemoveDir、RemoveDirectory：删除文件夹

    var
        dir: String;
    begin
        dir:= 'c:/temp';
        RemoveDir(dir);    //返回Boolean
    
        //或者使用系统API
        //RemoveDirectory(PChar(dir));    //返回Boolean
    end;

#### GetCurrentDir：获取当前文件夹

    var
        dir: String;
    begin
        dir:= GetCurrentDir;  //  C:/Projects   
        ShowMessage(dir);
    end;

#### SetCurrentDir、ChDir、SetCurrentDirectory：设置当前文件夹

    var
        dir: String;
    begin
        dir:= 'c:/temp';
        if SetCurrentDir(dir) then
            ShowMessage(GetCurrentDir);　　// c:/temp
    
        //或者使用 ChDir(dir); 这个函数无返回值
        //也可以使用系统API：SetCurrentDirectory(PChar(dir));    返回Boolean
    end;

#### GetDir：获取指定驱动器的当前路径名

    var
        dir: String;
        b: Byte;
    begin
        b:= 0;
        getDir(b, dir);
        ShowMessage(dir);
        //第一个参数：1、2、3、4……分别对应：A、B、C、D……
        //0是缺省驱动器
    end;

#### RenameFile：文件改名

    //文件改名 RenameFile   
    var  
      OldName,NewName: string;   
    begin  
      OldName := 'c:/temp/Old.txt';   
      NewName := 'c:/temp/New.txt';   
      
      if RenameFile(OldName,NewName) then  
        ShowMessage('改名成功!');   
      
    //也可以:   
      SetCurrentDir('c:/temp');
    //所以可以看出来，SetCurrentDir是切换目录路径（上面所说的设置路径就是指切换目录路径）   
      OldName := 'Old.txt';   
      NewName := 'New.txt';   
      
      if RenameFile(OldName,NewName) then  
        ShowMessage('改名成功!');   
    end; 

#### CreateDir; CreateDirectory; ForceDirectories： 建立文件夹

    var
        dir: String;
    begin
        dir:= 'c:/temp/delphi';
        if not DirectoryExists(dir) then
            CreateDir(dir);    //返回Boolean
            //也可以直接使用系统API
            //CreateDirectory(PChar(dir), nil);    //返回Boolean
        
        //如果缺少上层目录将自动补齐
        dir:= 'c:/temp/CodeGear/Delphi/2007/万一';
        ForceDirectories(dir);    //返回Boolean
    end;

#### RemoveDir、RemoveDirectory：删除空文件夹

    var
        dir: String;
    begin
        dir:= 'c:/temp/delphi';
        RemoveDir(dir);    //返回Boolean
    
        //也可以使用系统API
        //RemoveDirectory(PChar(dir));    //返回Boolean
    end;

#### FileCreate：建立新文件

    var  
      FileName: string;   
      i: Integer;   
    begin  
      FileName := 'c:/temp/test.dat';   
      i := FileCreate(FileName);   
      //FileCreate返回的是创建的文件的句柄，大于0成功，否则失败
      
      if i>0 then  
        ShowMessage('新文件的句柄是: ' + IntToStr(i))   
      else  
        ShowMessage('创建失败!');   
    end;   

#### GetFileVersion：获取当前文件的版本号

    var
        s:String;
        i: Integer;
    begin
        s:= 'C:/WINDOWS/notepad.exe';
        i:= GetFileVersion(s);
        //如果没有版本号就返回-1
        ShowMessage(IntToStr(i));    //327681 这是当前记事本的版本号(还应该再转换一下)   
    end;

#### DiskSize、DiskFree：获取磁盘空间

    var
        r: Real;
        s: String;
    begin
        r:= DiskSize(3);    //获取C:(序号是3)的总空间，单位是字节
        r:= r/1024/1024/1204;
        Str(r:0:2, s);    //格式为保留两位小数的字符串
        s:='C盘的总空间是：' + s + ' GB';
        ShowMessage(s);    //xx.xx GB
    
        r:= DiskFree(s);    //获取C: 盘的可用空间
        r:= r/1024/1024/1024;
        Str(r:0:2, s);
        s:= 'C盘的可用空间是：' + s + ' GB';
        ShowMessage(s);    //xx.xx GB
    end;

#### FileSearch:查找一个文件

    var
        FileNAme, Dir, s: String;
    begin
        FileName:= 'notepad.exe';
        Dir:= 'c:/windows';
        s:= FileSearch(FileName, Dir);
    
        if s <> '' then
            ShowMessage(s)    // c:/windows/notepad.exe
        else
            ShowMessage('没找到');
    end;

#### FindFirst、FindNext、FindClose：搜索文件

    var  
      sr: TSearchRec;    //定义 TSearchRec 结构变量   
      Attr: Integer;     //文件属性   
      s: string;         //要搜索的内容   
      List: TStringList; //存放搜索结果   
    begin  
      s := 'c:/windows/*.txt';   
      Attr := faAnyFile;             //文件属性值faAnyFile表示是所有文件   
      List := TStringList.Create;    //List建立   
      
      if FindFirst(s,Attr,sr)=0 then //开始搜索,并给 sr 赋予信息, 返回0表示找到第一个   
      begin  
        repeat                       //如果有第一个就继续找   
          List.Add(sr.Name);         //用List记下结果   
        until(FindNext(sr)<>0);      //因为sr已经有了搜索信息, FindNext只要这一个参数, 返回0表示找到   
      end;   
      FindClose(sr);                 //需要结束搜索, 搜索是内含句柄的   
      
      ShowMessage(List.Text);        //显示搜索结果   
      List.Free;                     //释放List   
      
    //更多注释:   
    //TSearchRec 结构是内涵文件大小、名称、属性与时间等信息   
    //TSearchRec 中的属性是一个整数值, 可能的值有:   
    //faReadOnly  1   只读文件   
    //faHidden    2   隐藏文件   
    //faSysFile   4   系统文件   
    //faVolumeID  8   卷标文件   
    //faDirectory 16  目录文件   
    //faArchive   32  归档文件   
    //faSymLink   64  链接文件   
    //faAnyFile   63  任意文件   
      
    //s 的值也可以使用?通配符,好像只支持7个?, 如果没有条件就是*, 譬如: C:/*   
    //实际使用中还应该在 repeat 中提些条件, 譬如判断如果是文件夹就递归搜索等等   
    end;   

#### FileGetAttr、FileSetAttr：读取与设置文件属性

    var  
      FileName: string;   
      Attr: Integer; //属性值是一个整数   
    begin  
      FileName := 'c:/temp/Test.txt';   
      Attr := FileGetAttr(FileName);   
      ShowMessage(IntToStr(Attr)); //32, 存档文件   
      
    //设置为隐藏和只读文件:   
      Attr := FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN;   
      if FileSetAttr(FileName,Attr)=0 then //返回0表示成功   
        ShowMessage('设置成功!');   
      
    //属性可选值(有些用不着):   
    //FILE_ATTRIBUTE_READONLY = 1; 只读   
    //FILE_ATTRIBUTE_HIDDEN = 2; 隐藏   
    //FILE_ATTRIBUTE_SYSTEM = 4; 系统   
    //FILE_ATTRIBUTE_DIRECTORY = 16   
    //FILE_ATTRIBUTE_ARCHIVE = 32; 存档   
    //FILE_ATTRIBUTE_DEVICE = 64   
    //FILE_ATTRIBUTE_NORMAL = 128; 一般   
    //FILE_ATTRIBUTE_TEMPORARY = 256   
    //FILE_ATTRIBUTE_SPARSE_FILE = 512   
    //FILE_ATTRIBUTE_REPARSE_POINT = 1204   
    //FILE_ATTRIBUTE_COMPRESSED = 2048; 压缩   
    //FILE_ATTRIBUTE_OFFLINE = 4096   
    //FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = 8192; 不被索引   
    //FILE_ATTRIBUTE_ENCRYPTED = 16384   
    end;   

#### FileAge、FileDateToDateTime：获取文件的创建时间

    var  
      FileName: string;   
      ti: Integer;   
      dt: TDateTime;   
    begin  
      FileName := 'c:/temp/Test.txt';   
      ti := FileAge(FileName);   
      ShowMessage(IntToStr(ti)); //返回: 931951472, 需要转换   
      
      dt := FileDateToDateTime(ti); //转换   
      ShowMessage(DateTimeToStr(dt)); //2007-12-12 14:27:32   
    end;  
    
　　