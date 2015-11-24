---
layout: post
title: 内存碎片之如何产生【转载】
categories: 深入学习之内存管理 C/C++之指针与内存 
tags: c c++ 内存碎片 指针 内存
---

* 转载自：[http://blog.csdn.net/flowshell/article/details/5996843](http://blog.csdn.net/flowshell/article/details/5996843)
* 建议参考[http://www.cnblogs.com/zhaoyl/p/3820852.html](http://www.cnblogs.com/zhaoyl/p/3820852.html)
* 建议参考[http://blog.csdn.net/xuzhonghai/article/details/7285821](http://blog.csdn.net/xuzhonghai/article/details/7285821)

malloc/free或new/delete大量使用后回造成内存碎片，那么这种碎片形成的机理是什么？ 

如果机理是申请的内存空间大小（太小）所形成的，那么，申请多大的区域能够最大限度的避免内存碎片呢？（这里的避免不是绝对的避免，只是一种概率）

内存碎片一般是由于空闲的连续空间比要申请的空间小，导致这些小内存块不能被利用。产生内存碎片的方法很简单，举个例： 

* 假设有一块一共有100个单位的连续空闲内存空间，范围是0~99。如果你从中申请一块内存，如10个单位，那么申请出来的内存块就为0~9区间。这时候你继续申请一块内存，比如说5个单位大，第二块得到的内存块就应该为10~14区间。 
* 如果你把第一块内存块释放，然后再申请一块大于10个单位的内存块，比如说20个单位。因为刚被释放的内存块不能满足新的请求，所以只能从15开始分配出20个单位的内存块。 
* 现在整个内存空间的状态是0~9空闲，10~14被占用，15~24被占用，25~99空闲。其中0~9就是一个内存碎片了。如果10~14一直被占用，而以后申请的空间都大于10个单位，那么0~9就永远用不上了，造成内存浪费。 

如果你每次申请内存的大小，都比前一次释放的内村大小要小，那么就申请就总能成功。 

###解决思路一

在C++中我们可以重写operator new/operator delete来减少内存碎片出现的机会，并在那些需要动态分配大量的但很小的对象的应用程序里效率会很好，如以下的例子：

``` 
//FreeListBase.h
#define NULL 0
class FreeListBase
{
public:
	FreeListBase(void){}
	virtual ~FreeListBase(void){}
public:
	void* operator new(size_t size);
	void operator delete(void* p,size_t size);
private:
	static FreeListBase* freelist;
	FreeListBase* next;
};
//FreeListBase.cpp
void* FreeListBase::operator new(size_t size)
{
	if(freelist != NULL)
	{
		FreeListBase* p = freelist;
		freelist = freelist->next;
		return p;
	}
	else
		return ::operator new(size);
}
void FreeListBase::operator delete(void* vp,size_t size)
{
	FreeListBase* p = static_cast<FreeListBase*>(vp);
	p->next = freelist;
	freelist = p;
}
//TemperatureUsingFreeList.h
#include "freelistbase.h"
class TemperatureUsingFreeList :
	public FreeListBase
{
public:
	TemperatureUsingFreeList(void){}
	~TemperatureUsingFreeList(void){}
	inline int average() { return (maxTemp + minTemp)/2; }
private:
	int ID;
	int maxTemp;
	int minTemp;
	int currentTemp;
};&nbsp;
```

当然不一定要用继承的形式，组合也是可以的。

###解决思路二

也有的人喜欢自己编写内存管理模块，程序一开始就申请一大块内存（内存池），然后以后申请内存都在这个大内存中取，配合一定的技巧来减少内存碎片问题。 例如：

```
//BigChunkStack.H
#define NULL 0
#define INITSIZE 30
class BigChunkStack
{
	struct elem
	{
		int id;                 
		int previousElemSize;    //上一个元素的大小
		int namesize;            //name的大小
		char* name;
	};
public:
	BigChunkStack(void);
	~BigChunkStack(void);
public:
	void push(const char* s,const int nr);
	void pop(char* s,int& nr);
    int grow();
	int shrink();
private:
	int totalSize;
	int emptyElemSize;
	int lastElemSize;     //最后一个加入的元素的大小
	char* pool;           //内存池指针
	int MAXSIZE;          //内存池的大小
};
//BigChunkStack.cpp
BigChunkStack::BigChunkStack(void)
{
	totalSize = 0;
	emptyElemSize = sizeof(elem);
	lastElemSize = 0;
	pool = NULL;
	MAXSIZE = 0;
}
BigChunkStack::~BigChunkStack(void)
{
}
void BigChunkStack::push(const char* s,const int nr)
{
	assert(s != NULL);
	int newStringSize = strlen(s) + 1;
	int newElemSize = newStringSize + emptyElemSize;
	if((totalSize + newElemSize) > MAXSIZE)
	{
		if(!grow())
		{
			cerr<<"Error,Stack Overflow!"<<endl;
			return ;
		}
	}
	elem* newElem = (elem*)(pool+totalSize);
	newElem->name = (char*)(pool+totalSize+emptyElemSize);
	newElem->id = nr;
	newElem->namesize = newStringSize;
	newElem->previousElemSize = lastElemSize;
	strcpy(newElem->name,s);
	lastElemSize = newElemSize;
	totalSize += newElemSize;
}
void BigChunkStack::pop(char* s,int& nr)
{
	if(totalSize*4 <= MAXSIZE)
		shrink();
	if(totalSize != 0)
	{
		totalSize -= lastElemSize;
		elem* popElem = (elem*)(pool+totalSize);
		lastElemSize = popElem->previousElemSize;
		strcpy(s,popElem->name);
		nr = popElem->id;
	}
	else
	{
		cerr<<"Error,Stack Underflow!!"<<endl;
	}
}
int BigChunkStack::grow()
{
	if(MAXSIZE == 0)
	{
		pool = new char[INITSIZE];
		if(pool == NULL)
			return 0;
		MAXSIZE = INITSIZE;
		return 1;
	}
	else
	{
		MAXSIZE *= 2;
		char* tempPool = (char*)realloc(pool,MAXSIZE);
		if(tempPool == NULL)
			return 0;
		pool = tempPool;
		return 1;
	}
}
int BigChunkStack::shrink()
{
	if(MAXSIZE/2 >= INITSIZE)
	{
		char* tempPool = (char*)realloc(pool,MAXSIZE/2);
		if(tempPool == NULL) return 0;
		pool = tempPool;
		MAXSIZE /= 2;
		return 1;
	}
}
```
