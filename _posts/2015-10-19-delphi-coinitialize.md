---
layout: post
title: OleInitialize、CoInitialize、CoInitializeEx和AfxOleInit区别
categories: delphi之应用程序框架
tags: delphi oleinitialize coinitialize coinitializeex afxoleinit api windows com ole 多线程
---


CoInitialize CoInitializeEx 是用来初始化COM运行环境的。

OleInitialize是初始化Ole的运行环境，Ole是在Com的基础上作的扩展，是ActiveX运行的基础，OleInitialize肯定会调用CoInitialize。

CoInitialize、CoInitializeEx都是windows的API，主要是告诉windows以什么方式为程序创建COM对象，原因是程序调用com库函数（除CoGetMalloc和内存分配函数）之前必须初始化com库。

* CoInitialize指明以单线程方式创建。
* CoInitializeEx可以指定COINIT_MULTITHREADED以多线程方式创建。
* 创建单线程方式的COM服务器时不用考虑串行化问题，多线程COM服务器就要考虑。
* CoInitialize并不装载com库，这个函数只是用来初始化当前线程使用什么样的套间。当使用这个函数以后，线程就和一个套间建立了对应关系。
* 线程的套间模式决定了该线程如何调用com对象，是否需要列集等
* 套间是com中用来解决并发调用冲突的很有效的办法

Before calling any COM functions, a thread needs to call CoInitialize to load the COM infrastructure (and to enter an apartment). Once a thread calls CoInitialize, the thread is free to call COM APIs.

CoInitializeEx provides the same functionality as CoInitialize and also provides a parameter to explicitly specify the thread's concurrency model. The current implementation of CoInitialize calls CoInitializeEx and specifies the concurrency model as single-thread apartment. Applications developed today should call CoInitializeEx rather than CoInitialize.

>注：新的应用程序应该调用CoInitializeEx而不是CoInitialize，否则就会有必要在之后每个调用Com的线程中调用CoInitialize来初始化出每个线程自己的套间。

AfxOleInit实际上调用了OleInitialize，虽然它在内部也调用了CoInitializeEx，但它只能处理单线程，这是AfxOleInit和CoInitialize主要区别：   

OleInitialize   calls   CoInitializeEx   internally   to   initialize   the   COM   library   on   the   current   apartment.   Because   OLE   operations   are   not   thread-safe,   OleInitialize   specifies   the   concurrency   model   as   single-thread   apartment.     

Once   the   concurrency   model   for   an   apartment   is   set,   it   cannot   be   changed.   A   call   to   OleInitialize   on   an   apartment   that   was   previously   initialized   as   multithreaded   will   fail   and   return   RPC_E_CHANGED_MODE.

###应用场景：###
1. CoInitialize 仅仅初始化Com，支持多线程。也就是说如果多线程调用Com接口，必须在每个线程中都调用CoInitialize。
2. OleInitialize 初始化Com（其实也是调用CoInitializeEx），支持多线程。比CoInitialize多了以下内容：**A) Clipboard**；**B) Drag and drop**；**C) Object linking and embedding (OLE)**；**D) In-place activation**。如果不需要这些，用CoInitialize就可以。
3. AfxOleInit是MFC对OleInitialize的封装。貌似不支持多线程，也就是说只能在主进程调用该函数，如果线程需要使用Com必须调用上面的两个来实现初始化。而且MSDN明确标明AfxOleInit不能在MFC的DLL中调用，否则也会造成初始化失败。
4. OleInitialize和OleUninitialize()成对使用；CoInitialize和CoUninitialize成对使用；CoInitializeEx和CoUninitialize成对使用；AfxOleInit()由MFC自动释放。

VC++使用ADO访问ACCESS时，出现_RecordsetPtr Open卡或者_ConnectionPtr Excute卡或者_CommandPtr Excute一直卡住等莫名其妙的情况时，而语法又没有错误时，请确定初始化COM函数。

MFC程序建议使用AfxOleInit()。


补充：CoInitialize 和 CoUninitialize 的调用时机
----

参考《[CoInitialize 和 CoUninitialize 的调用时机问题](http://www.cppblog.com/Streamlet/archive/2010/04/02/111363.aspx)》

最近有个东西，需要读 XML 配置文件，于是用 msxml 做了。msxml 是基于 COM 的，使用之前需要 CoInitialize，使用之后需要 CoUninitialize。于是我写成了：

    void foo()
    {
        CoInitialize(NULL);
    
        // Reading configuration
    
        CoUninitialize();
    }

刚才我正乐此不彼的把类似这样的东西改成：

    void foo()
    {
        CoInitialize(NULL);
        LOKI_ON_BLOCK_EXIT(CoUninitialize);
    
        // Reading configuration
    }


###我和同事的讨论###

前面的同事过来看到了，说：你不该在这里调用 CoInitialize 和 CoUninitialize。如果有的地方也在用 COM，你这里 CoUninitialize 一下，别的地方就会出错了，上次的某个 Bug 就是。

我狡辩道：我假定这里没有多线程环境（实际上也是），并且约定别的地方用 COM 的时候调用 CoInitialize 时不要判断返回值。

同事：应该和大众习惯保持一致，最好就是全项目最开始的时候 CoInitialize 一次，结束的时候 CoUninitialize 一次。

我：我这里是较底层功能函数。

同事：可以以文档的方式注明，使用该模块前必须自己 CoInitialize，使用完毕后自己 CoUninitialize。

我：我只是想要用起来方便一点，用的时候不要有那么多先决条件和后置条件。再说，人家本来可以不知道我用了 COM，我这么一说明，就暴露了内部信息了不是？

其实我被动摇了。

###各位大大，你们怎么处理呢？###

------------------------------华丽的分割线（13:27 p.m. 增加）----------------------------------

好，既然 CoInitialize 和 CoUninitialize 有引用计数机制，那么这个具体问题已经解决。

那么，有没有类似的成对使用的 API，会对进程全局产生影响的呢？如果有，在底层要用到的时候该怎么处理？
