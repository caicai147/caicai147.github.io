---
layout: post
title: Delphi容器类之---TOrderedList、TStack、TQueue、TObjectStack、TObjectQueue
categories: delphi之容器类
tags: delphi tlist tobjectlist tstack tqueue
---


###TOrderedList、TStack、TQueue###

Contnrs单元还定义了其他三个类：TOrderedList、TStack、TQueue

####TOrderedList####

    TOrderedList = class(TObject)
    private
        FList: TList;
    protected
        procedure PushItem(AItem: Pointer); virtual; abstract;
        ...
    public
        function Count: Integer;
        function AtLeast(ACount: Integer): Boolean;
        procedure Push(AItem: Pointer);
        function Pop: Pointer;
        function Peek: Pointer;
    end;

####TStack####

    TStack = class(TOrderedList)
    protected
        procedure PushItem(AItem: Pointer); override;
    end;

####TQueue####

    TQueue = class(TOrderedList)
    protected
        procedure PushItem(AItem: Pointer); override;
    end;


要注意，虽然TOrderedList并不是从TList继承的，但是它在内部的实现时，使用了TList来存储指针。另外注意TOrderedList类的 PushItem过程是一个抽象过程，所以我们无法实例化TOrderList 类，而应该从TOrderList继承新的类，并实现抽象的PushItem 方法。

因此TStack、TQueue继承自TOrderedList，所以TStack、TQueue的指针也是存在其内部的TList中的。

TStack和TQueue正是实现了PushItem 抽象方法的类，我们可以实例化TStack和TQueue类作为后进先出的堆栈（LIFO）和先进先出的队列（FIFO）。下面是这两个类的方法的使用说明：

* Count　　返回列表中的项目数。
* AtLeast　　可以用来检检查链表的大小，判断当前列表中的指针数目是否大于传递的参数值，如果为True便是列表中的项目数大于传来的参数。
* Push　　对于TStack类，Push方法将指针添加到链表的最后，对于TQueue类，Push方法则将指针插入到链表的开始。
* Pop　　返回链表的末端指针，并将其从链表中删除。
* Peek　　返回链表的末端指针，但是不将其从链表中删除。


###TObjectStack、TObjectQueue###

Contnrs单元中最后两个类是TObjectStack和TObjectQueue类，类的定义如下

####TObjectStack####

    TObjectStack = class(TStack)
    public
        procedure Push(AObject: TObject);
        function Pop: TObject;
        function Peek: TObject;
    end;

####TObjectQueue####

    TObjectQueue = class(TQueue)
    public
        procedure Push(AObject: TObject);
        function Pop: TObject;
        function Peek: TObject;
    end;

这两个类只是TStack和TQueue 类的简单扩展，在链表中保存的是TObject的对象引用，而不是简单的指针。