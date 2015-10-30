---
layout: post
title: Delphi线程简介---Create及其参数、Resume、Suspend
categories: delphi之多线程 delphi之面向对象
tags: delphi 多线程 
---


TThread在Classes单元里的声明如下

    type
        TThread = class
        private
            FHandle: THandle;
            FThreadID: THandle;
            FTerminated: Boolean;
            FSuspended: Boolean;
            FFreeOnTerminate: Boolean;
            FFinished: Boolean;
            FReturnValue: Integer;
            FOnterminate: TNotifyEvent;
            FMethod: TThreadMethod;
            FSynchronizeException: TObject;
            procedure CallOnTerminate;
            function GetPriority: TThreadPriority;
            procedure SetPriority(Value: TThreadPriority);
            procedure SetSuspended(value: Boolean);
        protected
            procedure DoTerminate; virtual;
            procedure Execute; virtual; abstract;
            procedure Synchronize(Method: TThreadMethod);
            property ReturnValue: Integer read FReturnValue write FReturnValue;
            property Terminated: Boolean read FTerminated;
        public 
            constructor Create(CreateSuspended: Boolean);
            destructor Destroy; override;
            procedure Resume;
            procedure Terminate;
            function WaitFor: Integer;
            property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
            property Handle: THandle read FHandle;
            property Priority: TThreadPriority read GetPriority write SetPriority;
            property Suspended: Boolean read FSuspended write Suspended;
            property ThreadID: THandle read FThreadID;
            property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
        end;

###先说一下TThread的Create的参数###

当TThread的Create()被调用的时候，需要传递一个布尔型的参数CreateSuspended。如果把这个参数设为False，那么当调用Create()之后，Execute()会被自动调用，也就是自动地执行线程代码。如果该参数设为True，这样创建了线程，但是线程创建完成之后是挂起的，需要调用TThread的Resume()来唤醒线程执行。

一般情况下，当你调用Create()后，还会有一些其他的属性要求设置。所以，这种情况下需要把CreateSuspended参数设置为True，因为在TThread已经执行的情况下设置TThread的属性可能引起麻烦。

同时注意TThread的Create方法是没有默认参数的，所以你在创建的一个继承自TThread的线程类没有重写Create方法的话，调用Create的时候必须传入True或False。不过如果你实现一个继承自TThread的类，并且可以自己override（重写）TThread的Create方法，这时候你当然就可以自己定制，比如可以设计一个默认参数为CreateSuspended为True或False。

在讲的深一点，在构造函数Create()中隐含调用一个RTL例程BeginThread()，而它又调用了一个API函数CreateThread()来创建一个线程对象的实例。CreateSuspended参数表明是否传递CREATE_SUSPENDED标志给CreateThread()。

###再说一下Suspend和Resume###

