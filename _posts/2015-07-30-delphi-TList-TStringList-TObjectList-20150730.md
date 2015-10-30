---
layout: post
title: Delphi容器类之---TList、TStringList、TObjectList，以及一个例程的代码分析
categories: delphi之容器类
tags: delphi tlist tobjectlist 
---


转载自：[http://blog.csdn.net/jqandjq/article/details/5429137](http://blog.csdn.net/jqandjq/article/details/5429137)


看了这里标题，大家可能以为我会谈TListBox控件，那就错了。我要谈的是Delphi提供给我们的具有列表性质的类：TStringList、TList和TObjectList。TStringList用来存放字符串，TList存放指针，而TObjectList则存放对象（Object）

在我们使用Delphi的过程中，有很多数据的存储是要靠 数组解决的。虽然Delphi现在已经支持了可变数组，不过总有那么点缺陷：我们不能在删除一个项之后，使后边的项目自动前靠。因此，说说Delphi现成的List还是很有价值的。

 
###TStringList###

TStringList源码在 classes.pas里面。

####Text属性和Strings属性####

在TStringList里面，那些String被一行一行地存储存。TStringList.Text返回全部的String。如果第一、二、三行分别是/'aa/'、/'bb/'、/'cc/'的话，那么Text返回的是“/'aa/'+#13#10+/'bb/'+#13#10+/'cc/'+#13#10” （不包括双引号 ）。所有的String都被TStringList用回车符和换行符（#13#10）连接起来。如果一次向Text赋值的话，Text就会被自动地分割成储存在TStringList里。这里充分地体现出TStringList的一个很实用的价值：它能让我们逐行处理String。假如我们要操作第4行，只需要操作TStringList[3]。相信大家会问，TStringList明明是一个类，为什么能当数组那样子用呢？其实，我们在写TStringList[3]的时候，就是在写TStringList.Strings[3]。Strings是TStringList的一个缺省属性。数组性的缺省属性就是这样子使用的。如果大家在编写类的时候要使用到这么一个功能的话，刻意参考如方法

    property AProperty[I: Integer] read *** write ***;
    default;

Strings是一个可读写的属性。这也就是说，大家并不仅可以获取第N行的内容，也可以改变第N行的内容。因此我们需要知道TStringList里String的总数。TStringList的属性Count则可以满足我们的需求。

上面已经说过，Text是返回所有字符串的属性。向Text赋值时，TStringList能够自动地把Texxt分成一行一行的，然后存储在TStringList里（当然，TStringList里面并不完全时这么存储的，详细的过程可以参见TStringList和TStrings的代码）。这样，Strings返回的字符串就是没有回车和换行的。但是，如果我们向Strings赋值的字符串有回车和换行，那么会出现什么情况呢？此时，Strings就会把哪个字符串断成几行，插入到原来的位置上。如果TStringListt只有这么些功能的话，那我就不必专门拿出来讲了——我是说，TStringList能让我们任意地插入或删除某行，这就要用到TStringList提供的方法。

TStringList里的每一个字符都有自己的位置标号（从0开始）。

 
####Add和Append方法####

    function Add(const S: String): Integer;
    procedure Append(const S: String);

Add方法向TStringList的尾行添加一行String（在这里和下面我们都假设输入的字符串没有回车和换行，否则Strings将被分割）。参数S代表的是要插入的字符串的内容。Add的返回值代表了新的字符串在TStringList的位置——也就是最后一行的位置，即新的Count减去一。

Append方法和Add唯一不同的地方就是没有返回值。


####Insert方法####

    procedure Insert(Index: Integer; const S: String);

Insert方法向TStringList插入一行字符串。在Insert里，我们可以自由地选择字符串插入的位置。参数S代表要插入的字符串的内容，Index代表要插入的位置。


####Delete方法####

    procedure Delete(Index: Integer);

Delete方法删除某行字符串，我们同样可以自由地选择删除任意一行字符串。参数Index代表要删除的那一行字符串的位置。

0是第一个数据。


####IndexOf方法####

    function IndexOf(const S: String): Integer;

IndexOf查找某一字符串在TStringList里的位置。参数S代表要查找的字符串。如果TStringList里面不存在S的话，则返回-1。


####Move和Exchange方法####

    procedure Move(CurIndex, NewInddex: Integer);
    procedure Exchange(Index1, Index2: Integer);

TStringList另外还提供了两个方法——Move和Exchange。

Move方法能把一行字符串抽出来并插入到另一个指定的为位置上，参数CurIndex代表要移动的字符串的位置，NewIndex代表字符串新的位置。

Exchange方法则能将随便两个字符串交换。参数Index1和Index2代表两行需要交换的字符串的位置。


####LoadFromFile和SaveToFile方法####

    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);

TStringList的LoadFromFile和SaveToFile两个方法，使得我们对文本文件得操作变得非常方便。参数FileName代表目标文本文件的文件名。

例如我们在编写记事本的时候，用到的TMemo.Lines就是TString（TStringList的父类，功能几乎相同，只是因为TString时虚类，我们无法创建并使用）。在保存的时候只需要一行代码：

    TMemo.Lines.SaveToFile(FileName);

非常方便。

 
####Names和Values属性####

TStringList还有一项特殊功能：可以把TStringList当成ini 控制器使用。不过它没有Section。现在我就来介绍TStringList的两个属性：Names和Values。

如果TStringList的内容是这样子的：

    ID=1
    Name=Vczh
    PositionID=8
    Tel=00000000

那么，Names[2]就返回"positionID"，Values[/'PositionID/']就返回"8"。其中"Names"是只读属性，而"Values"则可写。TStringList使我们不必拘泥于ini 和注册表。关于TStringLisst没有Section这个问题，我们完全可以在Names里面你做点手脚，只要程序能够识别就行。

TStringList还有一个可以存放Object的功能。但是我个人认为使用TObjectList比较好，因为TObjectList在这方面提供了比TStringList更多的功能。

下面我提供一个例程来介绍Values属性。

新建一个工程保存，并在dpr文件所在的文件夹里建立一个叫做"Config.txt"的文件，并输入一下内容

    Name=VCZH
    Address=Somewhere
    Email=vczh@162.com

然后，建立一个窗体。并在TForm1的Private区段里建立变量：

    SL:TstringList;

这个例程的功能实施编辑Config.txt里的Name、Address、Email。SL在程序启动时读入Config.txt文件；按下Cancel按钮则退出；按下OK按钮则改变SL的内容并保存在Config.txt文件中；当程序再次运行时，改变后的内容就会显示再三个文本框里 。代码如下

    procedure TForm1.FormCreate(Sender: TObject);
    begin
        SL:= TStringList.Create;
        {获取当前程序文件所在的文件夹名称以获得Config.txt文件的路径}
        SL.LoadFromFile(ExtractFilePath(paramStr(0))+/'Config.txt/');　　{  ExtractFilePath(paramStr(0))获取当前执行的程序所在的路径  }
    end;
    
    procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
        SL.Free;
    end;
    
    procedure TForm1.btnCancelClick(Sender: TObject);
    begin
        Close;
    end;
    
    procedure TForm1.FormShow(Sender: TObject);
    begin
        //通过上面介绍的Values属性获得各个字段的内容
        edtName.Text:= SL.Value[/'Name/'];
        edtAddress.Text:= SL.Values[/'Address/'];
        edtEmail.Text:= SL.Values[/'Email/'];
    end;
    
    procedure TForm1.btnOKClick(Sender: TObject);
    begin
        SL.Values[/'Name/']:= edtName.Text;
        SL.Values[/'Address/']:= edtAddress.Text;
        SL.Values[/'Email/']:= edtEmail.Text;
        SL.SaveToFile(ExtractFilePath(ParamStr(0))+/'Config.txt/');
        Close;
    end;

　　
###TList###

####TList简介####

TList所在位置：Classes.pas

在我刚接触TList的时候，TList搞得我迷雾重重，都是Capacity属性惹的祸。我查了Delphi的帮助，它说Capacity是Tlist的最大容量，又在什么地方说MaxInt div 4是TList的最大容量。最后我搞明白了，Capacity是临时的，MaxInt div 4 才是真正的最大容量。只要你的内存受得了就行，算起来一共是4G。

在TList内部有一个FList指针指向一个Pointer数组，Capacity就是这个数组的大小。奇怪的是Capacity是可写的。我当时就在想，如果一直使用Add直到超出Capacity的范围，会怎么样呢？为了解决这个问题，我特地打开了TList的代码，结果发现如下几行（注释是我自己加的）：

    function TList.Add(Item: Pointer): Integer;
    begin
        //返回Item所在的位置
        Result:= FCount;
        //如果FList数组被填满了装不下新的Item，那么TList自动增加Capacity，也就是FList指向的数组的大小
        if Result = FCapacity then
            Grow;
        //扩大了FList的大小之后，就能把Item放进数组了
        FList^[Result]:= Item;
        Inc(FCount);
        if Item <> nil then
            Notify(Item, InAdded);
        //给TList一个信号，通知TList已经加上一个新的Item
    end;
    
    procedure TList.Grow;
    var
        Delta: Integer;
    begin
        //增加的规则是，如果数量小于或等于8，那么增加4；那么增加在8之上，小于或等于64，那么增加16；
        //如果数量比64还大，那么一次增加大约1/4的空间
        if FCapacity > 64 then
            Delta:= FCapacity div 4
        else
            Delta:= 4;
        //改变数组的大小
        SetCapacity(FCapacity + Delta);
    end;

既然Capacity会自动增加，那么还要Capacity干什么呢？还不如使用链表。不过我后来意识到，在使用链表额时候，取得某个位置的指针比数组困难，要用比较费时间的循环。TList刚好解决了这个问题。我们既可以把TList当成数组，也可以把它当成链表。


####TList的Items属性和List属性####

TList除了保存的对象是指针之外，其他地方都与TStringList很像，所以下面只介绍两者的不同之处。我们同样可以使用TList或者TList.Items获得某一位置的指针。如果嫌TList.Items是属性没有效率的话，这里还有一个List属性，指向内部的FList，可以这样使用：

    TList.List^

介绍到TList的Items，就要提到TList的缺省数组属性： property Items[Index: Integer]: Pointer read Get write Put; default;

见下面的代码

    uses Contnrs;  // 这个单元包含Delphi新增的容器对象，比如TObjectList
    
    
    // TList的缺省数组属性 property Items[Index: Integer]: Pointer read Get write Put; default;
    
    procedure TForm1.Button1Click(Sender: TObject);
    var
      List: TList; 
    begin
      List := TObjectList.Create;  //我很懒，释放的工作由TObjectList代劳
      try
        List.Add(TObject.Create);
        List.Add(TObject.Create);
        ShowMessage(TObject(List[0]).ClassName);  // 以数组下标方式访问，等同于List.Items[0]
    finally
        List.Free;
      end;
    end;


####TList的First和Last属性####

TList提供了First和Last两个属性，分别返回第一个和最后一个指针。


####TList的Delete和Remove方法####

TList也提供了一个Remove方法。与Delete不同的是，Delete删除的是已知位置的指针，Remove删除的是已知指针。只要TList包含有你想删除的指针，你就可以使用

    Remove(Pointer);

Remove的返回值是指针在还没有被删除之前的位置，使用方法如下

    procedure Delete(Index: Integer);
    function Remove(Item: Pointer): Integer;

####Tlist的Pack方法####

TList还有一个Pack方法。它能够把所有不是nil的指针聚在一起，同时把Count的值改变，这样，所有没有用的指针就会被删除，但是并不会减少Capacity。如果你想把没有用的空间都释放的话，可以把Capacity设置成Count。


####TList的Notify####

最后，我想说的是Protected里的Notify。大家在Add的代码里面就能看到，在Insert、Delete之类的代码里我们也能看到Notify的踪迹。既然FList的内容已经被改变了，Notify还要做什么工作呢？看一下Notify的代码：

    TListNotification = (InAdded, InExtracted, InDeleted);
    procedure TList.Notify(Ptr: Pointer; Action: TListNotification);
    begin
    
    end;

留着一个空的Notify有什么用呢？再看它的声明：

    procedure Notify(Ptr: Pointer; Action: TListNotification);
    virtual;

原来Notify是一个虚函数，当我们因为有特殊要求而继承TList累的话，只要TList的内容一改变，我们就能得到通知。不过前提是我们要覆盖Notify这个procedure。

 
###TObject###

####TObjectList简介####

TObjectList所在位置：Contnrs.pas

TObjectList中有一个不可缺少的属性：OwnsObjects。如果OwnsObjects是True（缺省值）的话，那么TObjectList会在适当的时候把它从列表中的Object释放掉。

 
###TStringList和TList的一个示例讲解###

现在，让我们用一个例子来结束我对Delphi的List的介绍。这个例子是一个管理人员信息的程序。不过因为这只是一个示例，所以这个程序只是简单的ConsoleApplication。使用ConsoleApplication来做示例程序可以免掉一些界面设计的工作。这个程序需通过Index来管理人员信息。人员信息包括Name、Telephone和Address。

程序通过TStringList来读取人员信息文件，然后用那些指向Record的指针把信息存放在TList里并修改。代码如下：

    program Info[color=#0000ff];
    {APPTYEP CONSOLE}
    uses
    	SysUtils, Classes;
    type
    	PInfo = ^TInnfo;
    	TInfo = record
    	begin
    		Name:String;
    		Tel: String;
    		Address: String;
    	end;
    
    var
    	SL: TStringList;
    	List: TLIst;
    	AppPath: String;
    	
    	{开辟一个新的PInfo指针，填入新信息并返回指针将在Command_Delete或FinaInfo里释放}
    	function MakeInfo(Name, Tel, Address: String): PInfo;
    	var
    		P: PInfo;
    	begin
    		New(P);
    		P^.Name:= Name;
    		P^.Tel:= Tel;
    		P^.Address:= Address;
    		{返回的指针将被保存在List里面}
    		Result:= P;
    	end;
    		
    	{在屏幕上打印所有可用的命令}
    	procedure PrintMenu;
    	begin
    		writeln(/'=====菜单=====/');
    		writeln(/'V---查看所有人员的信息/');
    		writeln(/'A---增添新的人员信息/');
    		writeln(/'D---删除人员/');
    		writeln(/'E---修改人员信息/');
    		writeln(/'M---查看所有可用命令/');
    		writeln(/'X---推出程序/');
    	end;
    	
    	{修改人员信息的程序}
    	procedure Command_Edit;
    	var
    		I: Integer;
    		Name, Tel, Address: String;
    		P: Pinfo;
    	begin
    		writeln(/'请输入要修改的人员信息的序号：/');
    		readln(I);
    		
    		if(I<0) or (I>=List.Count) then
    			writeln(/'输入超出范围/')
    		else
    		begin
    			{取得某个人员信息的指针}
    			P:= List.Items[I];
    			writeln(/'开始输入人员信息（若某项信息不需要修改则留空）：/');
    			writeln(/'姓名：/');
    			readln(Name);
    			writeln(/'电话号码：/');
    			readln(Tel);
    			writeln(/'地址：/');
    			readln(Address);
    		
    			{保存输入的信息}
    			if Name<>/'/' then
    				P^.Name:= Name;
    			if Tel<>/'/' then
    				P^.Tel:= Tel;
    			if Address<>/'/' then
    				P^Address:= Address;
    			writeln(/'修改人员信息执行完毕。/');
    		end;
    	end;
    	
    	{增加人员信息程序}
    	procedure Command_Add;
    	var
    		Name, Tel, Address: String;
    	begin
    		writeln(/'开始输入人员信息：/');
    		writeln(/'姓名：/');
    		readln(Name);
    		writeln(/'电话号码：/');
    		readln(Tel);
    		writeln(/'地址：/');
    		readln(Address);
    		
    		{使用MakeInfo生成TInfo的指针，并且加入到TList中}
    		List.Add(MakeInfo(Name, Tel, Address));
    		
    		writeln(/'增加人员信息执行完毕/');
    	end;
    	
    	{打印所有人员信息的程序}
    	procedure Command_View;
    	var
    		I: Integer;
    		P: PInfo;
    	begin
    		writeln(/'人员信息列表：/');
    		for I:=0 to List.Count-1 do
    		begin
    			P:= List.Items[I];
    			writeln(IntToStr(I)+ /'号==============/');
    			writeln(/'姓名：/' + P^.Name);
    			writeln(/'电话：/' + P^.Tel);
    			writeln(/'地址：/' + P^.Address);
    			
    			{满6个就暂停，刚好填满一个屏幕}
    			if I mod 6 = 5 then
    			begin
    				writeln(/'按回车键继续 。/');
    				readln;
    			end;
    		end;
    		writeln;
    	end;
    	
    	{删除人员信息的程序}
    	procedure Command_Delete;
    	var
    		I: Integer;
    		P: PInfo;
    	begin
    		writeln(/'请输入要删除的人员信息的序号：/');
    		readln(I);
    		if(I<0) or(I>=List.Count) then
    			writeln(/'输入超出范围/')
    		else
    		begin
    			P:= List.Items[I];
    			List.Delete(I);
    			Dispose(P);
    			writeln(/'删除执行完毕/');
    			writeln;
    		end;
    	end;
    	
    	{处理用户输入的命令，返回False表示退出}
    	function GetCommand: Boolean;
    	var
    		C: Char;
    	begin
    		writeln(/'输入命令并回车：/');
    		readln(C);
    		result:= True;
    		case C of
    			/'V/', /'v/': Command_View;
    			/'A/',/'a/':Command_Add;
    			/'D/',/'d/':Command_Delete;
    			/'M/',/'m/':PrintMenu;
    			/'X/',/'x/':result:=False;
    			/'E/',/'e/':Command_Edit;
    			else
    				writeln(/'未知命令。/');
    		end;
    	end;
    	
    	{从Info.txt把人员信息加载到List}
    	procedure LoadInfo;
    	var
    		I: Integer;
    		Name, Tel, Address, Index: String;
    	begin
    		SL.LoadFromFile(AppPath+/'Info.txt/');
    		for I:=0 to SL.Count div 3 - 1 do
    		begin
    			Index:= IntToStr(I)+/'./';
    			{文件格式：Index.Field=Value，在这里使用Index.X区分不同序号的人员信息的字段名称然后通过Values属性读取信息}
    			Name:= SL.Values[Index + /'Name/'];
    			Tel:= SL.Values[Index + /'Tel/'];
    			Address:= SL.Values[Index + /'Address/'];
    			List.Add(MakeInfo(Name, Tel, Address));
    		end;
    	end;
    	
    	{把TList里的人员信息保存到Info.txt}
    	procedure SqveInfo;
    	var
    		I: Integer;
    		P: PInfo;
    	begin
    		SL.Clear;{清空TStringList}
    		for I:=0 to List.Count-1 do
    		begin
    			P:= List.Items;
    			SL.Add(IntToStr(I) + /'.Name = /' + P^.Name);
    			SL.Add(IntToStr(I) + /'.Tel = /' + P^.Tel);
    			SL.Add(IntToStr(I) + /'.Address = /' + P^.Address);
    		end;
    		SL.SaveToFile(AppPath + /'Info.txt/');
    	end;
    	
    	{初始化程序}
    	procedure InitInfo;
    	begin
    		SL:= TStringList.Create;
    		List:= TList.Create;
    		{获得本程序的路径}
    		AppPath:= ExtractFilePath(ParamStr(0));
    	end;
    	
    	{清空程序使用的内存}
    	procedure FinalInfo;
    	var
    		I: Integer;
    	begin
    		for I:=0 to List.Count-1 do
    			Dispose(PInfo(List.Items));
    		List.Free;
    		SL.Free;
    	end;
    	
    begin
    {TODO -oUser -cConsole Main:Insert code here}
    	InitInfo;
    	LoadInfo;
    	Writeln(/'Information Editor V1.0 by VCZH/');
    	PrintMenu;
    	
    	while GetCommand do;{循环直到返回False}
    	
    	saveInfo;
    	FinalInfo;
    	
    	writeln(/'谢谢使用。请按回车键退出程序/');
    	readln;
    end;

在这个程序测试完毕（由于时间关系，并没有执行严格的测试，所以一些小细节可能会没有注意到，不过不会影响程序功能的正确性）时，Info.txt 的内容如下

    0.Name=vczh
    0.Tel=1234567
    0.Address=Jupiter
    1.Name= 陈梓瀚
    1.Tel=8888888
    1.Address=Venus
    2.Name=chenzihan
    2.Tel=9999999
    2.Address=Mars
    3.Name=Victor
    3.Tel=0000000
    3.Address=Saturn


在循环开始时，I的值被指向0，然后Index的值就变成"0."。

在执行Name:= SL.Values[Index + /'Name/']时，就等于执行Name:= SL.Values[/'0.Name/']，于是，"0.Name"这个字段的内容就被读取，然后Name就变成了"vczh".

接着进入下一个循环。

程序就是通过在字段前加"Index." 这个字段的方法区分不同人员的信息。在读取完一个人的信息之后，程序执行 List.Add(MakeInfo(Name, Tel, Address))，信息便存放在List里了。

程序通过操作List增加、删除或修改人员信息。在用户退出程序时，长城些许将List里的所有信息保存到"Info.txt" 里。

过程就是这样的：程序先清空SL 里的内容，然后按照Info.txt 原来的文件格式填写信息。因为Info.txt里的人员数目是会改变的，因此便不能使用TStringLiist.Values属性修爱。而必须在清空TStringList后手动构建字段并填写信息。

----

经过这样的介绍，大家对Delphi 包含的与List,有相似性质的类都有了一定的认识。Delphi VCL 开发组为我们准备了这么多既实用又有效率的功能。而List在庞大的Delphi VCL 里只是沧海一粟。所以我们今后应当继续研究Delphi VCL，充分利用 VCL，为自己的项目增添光辉