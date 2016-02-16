---
layout: post
title: 重学算法与数据结构之单链表
categories: 深入学习之算法 深入学习之数据结构 重学算法与数据结构
tags: 算法 数据结构 c c++ 单链表 指针 内存
---

这是重学算法与数据结构的第一篇，会坚持将大学里的算法与数据结构的知识重学一遍，然后再去学习在大学中没有学习的更为高深的算法和数据结构的知识。除了算法与数据结构的知识之外，大学中的操作系统原理、网络原理、数据库原理、编译原理等知识都是要重学的！

直接结合代码、相关说明、注释来进行理解。

##介绍

单链表是最简单的链表，元素之间由一个单独的指针链接，尾元素的指针置为NULL表示链表结束。这种结构的链表允许从第一个元素开始遍历至最后一个元素。

##接口定义

```
/*list.h*/

#ifndef LIST_H
#define LIST_H

#include <stdlib.h>

//定义链表的单个元素结构体
typedef struct ListElmt_
{
    void *data;     //数据成员
    struct ListElmt_ *next; //指向链表中下一个元素的指针
}ListElmt;

//定义链表这个数据结构的结构体
typedef struct List_
{
    int size;   //链表中元素的个数
    int (*match)(const void *key1, const void *key2);
        //match并不由链表本身使用，而是由从链表数据结构派生而来的新类型所用
    void (*destroy)(void *data);    
        //destroy是封装之后传递给list_init的析构函数
    ListElmt *head; //指向链表头的指针
    ListElmt *tail; //指向链表尾的指针
}List;

/*以下是定义的接口*/
void list_init(List *list, void(*destroy)(void *data));
void list_destroy(List *list);
int list_ins_next(List *list, ListElmt *element, const void *data);
int list_rem_next(List *list, ListElmt *element, void **data);
#define list_size(list) ((list)->size)

#define list_head(list) ((list)->head)
#define list_tail(list) ((list)->tail)
#define list_is_head(list, element) ((element) == (list)->head ? 1 : 0)
#define list_is_tail(list, element) ((element)->next == NULL ? 1 : 0)
#define list_data(element) ((element)->data)
#define list_next(element) ((element)->next)

/*注意Ｃ语言中的宏的定义，尤其是括号的使用技巧*/
```

##代码实现

```
/*list.c*/
#include <stdlib.h>
#include <string.h>

#include "list.h"

/*list_init*/
void list_init(List* list, void (*destroy)(void *data)){
    /*初始化链表*/
    
    list->size = 0;
    list->destroy = destroy;
    list->head = NULL;
    list->tail = NULL;
    
    return;
}

/*list_destroy*/
void list_destroy(List* list){
    void *data;
    /*删除链表中的所有元素*/
    
    while(list_size(list) > 0){
        if(list_rem_next(list, NULL, (void **)&data) == 0 && list->destroy != NULL){
            list->destroy(data);
        }
    }
    
    memset(list, 0, sizeof(List));
    /*顺便学习一下memset函数*/
    /*将s所指向的某一块内存中的每个字节的内容全部设置为ch指定的ASCII值,
        块的大小由第三个参数指定,这个函数通常为新申请的内存做初始化工作
        用 法: void *memset(void *s, char ch, unsigned n);
    */
    
    return;
}

/*list_ins_next*/
int list_ins_next(List *list, ListElmt *element, const void *data){
    ListElmt *new_element;
    
    if((new_element = (ListElmt *)malloc(sizeof(ListElmt))) ==NULL)
        return -1;
        
    new_element->data = (void *)data;
    if(element == NULL){
        if(list_size(list) == 0)
            list->tail = new_element;
            
        new_element->next = list->head;
        list->head = new_element;
    }
    else{
        if(element->next == NULL)
            list->tail = new_element;
            
        new_element->next = element->next;
        element->next = new_element;
    }
    
    list->size++;
    return 0;
}

/*list_rem_next*/
int list_rem_next(List *list, ListElmt *element, void **data){
    ListElmt *old_element;
    
    if(element == NULL){
        *data = list->head->data;
        old_element = list->head;
        list->head = list->head->next;
        
        if(list_size(list) == 1)
            list->tail = NULL;
    }
    else{
        if(element->next == NULL)
            return -1;
        
        *data = element->next->data;
        old_element = element->next;
        element->next = element->next->next;
        
        if(element->next == NULL)
            list->tail = element;
    }
    
    free(old_element);
    
    list->size--;
    return 0;
}
```
















