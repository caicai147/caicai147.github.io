---
layout: post
title: Delphi的指针为什么可以多次New，始于队列TQueue对指针的Push和Pop的思考
categories: delphi之指针与内存
tags: delphi 指针 内存
---


下面的思考是关于Delphi的指针、内存的，其实C、C++的指针、内存大概也是这样的原理，不过可能正在具体的语法……方面有所不同，要结合在一起理解、也要区分开别弄混了！

多次New，然后存入队列
--------

今天在使用Delphi的队列的时候，需要在每次将一个record压入队列的时候对该record进行New操作（分配内存空间），然后将该指针压入到队列，大概的代码如下

    unit Unit1;
    
    interface
    
    uses
      Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
      Dialogs, StdCtrls, OrderList, Contnrs;
      {虽然本单元中引入了OrderList，而OrderList里面引入了Contnrs，但是还是需要再引入一次}
    
    type
      TForm1 = class(TForm)
        edt1: TEdit;
        lbl1: TLabel;
        btn1: TButton;
        btn2: TButton;
        lbl2: TLabel;
        edt2: TEdit;
        procedure btn1Click(Sender: TObject);
        procedure btn2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
      private
        { Private declarations }
      public
        { Public declarations }
      end;
    
      PStudent = ^TStudent;
      TStudent = record
        name: string;
        age: Integer;
      end;
    
    var
      Form1: TForm1;
    
    implementation
    
    {$R *.dfm}
    var
      queue: TQueue;
    
    procedure TForm1.btn1Click(Sender: TObject);
    var
      student: PStudent;
    begin
      New(student);
      student.name:= edt1.Text;
      student.age:= StrToInt(edt2.Text);
    
      try
        queue.Push(student);
      except
        on e: Exception do
        begin
          ShowMessage(e.Message);
        end;
      end;
    end;
    
    procedure TForm1.btn2Click(Sender: TObject);
    var
      student: PStudent;
    begin
      if queue.Count > 0 then
      begin
        New(student);
        student:= queue.Pop;
    
        Application.MessageBox(PChar(student.name+'--'+IntToStr(student.age)), 'OK', MB_OK);
      end
      else
      //在这里加个判断队列是不是为空，就不会导致为空时出现异常
      begin
        Application.MessageBox('队列已经空了', 'OK?', MB_OKCANCEL);
      end;
    end;
    
    procedure TForm1.FormCreate(Sender: TObject);
    begin
      queue:= TQueue.Create;  //之前没有Create queue就直接进行Push，必然会出错
    end;
    
    procedure TForm1.FormDestroy(Sender: TObject);
    begin
      queue.Free;
    end;
    
    end.

主要是其中 TForm1.btn1Click(Sender: TObject) 方法中关于New一个指针以及将该指针Push进入队列的相关代码，假设出现这样的情况：我多次点击按钮，那是不是会多次对一个指针进行New，另外为什么不需要进行Dispose呢？　　

到底Push进入队列是一个什么样的原理呢？

我猜想是一个指针可以New多次，如果对一个指针进行New之后，将会在内存中分配一个空间，如果不进行Dispose，也不保存该指针的值（也就是该指针指向的位置），再次进行New的话，将会重新在内存中分配一块空间，但是之前分配的那块空间会因为地址丢失（主要是因为在New之后，原来的指针将会指向新分配的内存）而造成内存的泄露。

所以是可以对一个指针多次New的，但是最好在每次New的时候，保存之前一次New的指针（或者继续使用其内容并在用完之后Dispose，或者直接Dispose），不要造成内存泄露。

于是我就开始设计一个简单的程序验证我得猜想

    unit Unit1;
    
    interface
    
    uses
      Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
      Dialogs, StdCtrls;
    
    type
      TForm1 = class(TForm)
        btn1: TButton;
        procedure btn1Click(Sender: TObject);
      private
        { Private declarations }
      public
        { Public declarations }
      end;
    
      PStudent = ^TStudent;
      TStudent = record
        name: string;
        age: Integer;
      end;
    
    var
      Form1: TForm1;
    
    implementation
    
    {$R *.dfm}
    
    procedure TForm1.btn1Click(Sender: TObject);
    var
      student: PStudent;
      st1: PStudent;
      st2: PStudent;
    begin
      New(student);　　//使用New分配内存空间，并且student指针指向该内存空间
      student.name:= 'st1';　　//通过指针student对该内存空间的结构体进行赋值操作
      student.age:= 10;
    
      st1:= student;　　//使用新的指针来指向student所指向的内存空间，保存原来内存空间的地址
    
      New(student);　　//再次New来分配空间，这时候student会指向新的内存空间，
                      //而因为之前进行了 st1:= student; 的操作，所以使用st1保存了
                      //原来的地址，而不会造成内存的丢失
      student.name:= 'st2';　　//通过指针student 对新内存中的结构体赋值
      student.age:= 20;
    
      st2:= student;　　//再用st2 指针来保存新的内存地址
    
      ShowMessage(st1.name + '与' + st2.name);
    
      Dispose(st1);　　//使用完成后，使用Dispose(st1); 来释放原来的内存空间
      Dispose(st2);　　//其实此时Dispose(st2) 等价与 Dispose(student) 因为没有在上面
                      //进行 st2:= student 之后再次对student 进行New，所以st2与
                      //student指针都指向一个内存地址    
    end;
    
    end.

注意其中的 TForm1.btn1Click(Sender: TObject) 方法:

1. 先对一个student 指针进行New，于是就在内存中分配了一个内存空间，并且student 指针指向这块内存
2. 然后对该结构体进行赋值（就是代码中的对它的name、age赋值），然后将该指针赋给 st1指针，这个时候student 和st1 指针都同时指向了之前分配的内存空间
3. 然后在对 student指针进行New，这个时候重新分配了内存，而student 将指向该内存空间，不再指向之前的那块内存空间；第二次对student 进行New操作，不是将student之前指向的内存清空，而是重新分配一块内存，并且使student指针不再指向原来的内存，而是指向新分配的内存。 注意因为之前已经用 st1指向原来的内尺寸空间，所以之前分配的内存空间并没有丢失，虽然不能通过student 访问和操作原来的内存（因为这时候重新用New 分配了新空间，并且student指向了新的空间），但是因为用st1 指向了这块空间，所以不会造成内存泄露，这样就可以在使用好原来的内存中的数据之后，通过st1 指针来释放内存空间。
4. 再对student指针指向的新的结构体赋值（在新分配的内存中的操作），同样为了防止再次对student 进行New，所以使用另一个指针st2 来保存这次的内存地址，保证以后可以操作以及Dispose，而不会造成内存泄露
5. 最后使用完之后，通过st1、st2来对之前的内存进行释放（使用Dispose）

所以也就解释了上面的关于队列的问题（其他的容器类比如栈、链表……存放指针也都是类似的原理）：

每次New一个指针就分配了一个内存空间，且令该指针指向这块内存空间，然后将这个指针Push 到队列中，这就相当于在队列中保存了指向该地址的指针，因为队列保存了指向该地址空间的指针主要是为了通过指针访问这块内存空间以使用其中的数据，所以当然不能使用Dispose来释放该内存空间

另外将指针Push进队列的操作就相当于在队列中记录下该指针的值，所以就有新的可知的指针（也就是队列中的指针）指向这块内存空间，就可以通过队列中的指针来访问这块内存，进行相关操作，就不会丢失这块内存的信息，就不会造成内存泄露。因为队列中已经有指针来指向原来的内存，所以就可以再对那个指针New来分配新的内存空间以存放新的数据

然后再次对这个指针New的时候，就又分配了新的内存空间，然后这个指针就指向这个新的内存空间（注意原来的内存空间的地址已经保存在队列中了），然后再将这个指针Push 到队列中，这也就相当于又在队列中记录了该内存地址

所以就可以再次对这个指针New，来分配新的内存空间以存放新的数据

.........　　　　

怎样才是规范的释放内存的方法
----------------

另外如果要释放队列，不能直接是`queue.Free`，因为这样只是释放了队列的内存空间，但是队列中存储的只是指针，而这些指针所指向的实体的内存并没有被释放，这时候就出现了内存泄露的情况。

所以正确的方法是这样的（对于Delphi中存储指针的队列、链表都是这样的，C/C++中类似的存储指针的队列、链表应该也是这样的）：先逐个的将队列或链表中的指针取出，然后释放这些指针所指向的内存空间，所有的指针都取出来并释放了对应的内存之后，然后在对队列或链表本身进行Free！

另外还有这样的情况，一个结构体A，结构体本身的一个属性(假如叫bOfa)是另外一个结构体B指针的变量，那么这样的代码：
    
    var
        aa: ^A;
        bb: ^B;
    begin
        New(bb);
        New(aa);
        aa.bOfa:= bb;
        {进行一些操作，然后释放内存}
        Dispose(bOfa);        //先将a中的那个结构体指针指向的内存释放了
        Dispose(aa);          //最后在释放aa，不能不进行前面的释放而直接释放aa，否则会造成内存泄露
    end;


区分指针和真正的内存实体
------------ 

要想理解这个问题，就要理解指针和真正的内存实体的关系：

拿一个record结构体和它的指针为例

    type
        PStudent = ^TStudent;
        TStudent = record
            name: string;
            age: Integer;
        end;
    var
        student: PStudent;
    
    begin
        New(student);
        ....
    end;

比如上面这段简单的代码，student 是一个指针，必须为其分配一个内存空间（可以使用New实现），才能在其中保存一个学生的name和age值。这块内存空间是真正的存储数据的地方，而指针只是指向这块空间的一个标记