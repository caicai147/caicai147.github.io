---
layout: post
title: Python之函数对象
categories: python之基础 python之函数
tags: python 函数 函数对象
---

转载自[Vamei：Python进阶07 函数对象](http://www.cnblogs.com/vamei/archive/2012/07/10/2582772.html)

秉承着一切皆对象的理念，我们再次回头看函数(function)。函数也是一个对象，具有属性(可以使用dir()查询)。作为对象，它还可以赋值给其他对象名，或者作为参数传递。

## lambda函数

在展开之前，我们先提一下lambda函数。可以利用lambda函数的语法，定义函数。lambda例子如下：

```
func = lambda x, y: x+ y
print func(3,4)
```

lambda生成一个函数对象。该函数参数为x,y，返回值为x+y。函数对象赋值给func。func的调用与正常函数无异

以上定义可以写成以下形式：

```
def func(x, y):
    return x + y
```

## 函数作为参数传递

函数可以作为一个对象，进行参数传递。函数名(比如func)即该对象。比如说：

```
def test(f, a, b):
    print 'test'
    print f(a, b)

test(func, 3, 5)
```

test函数的第一个参数f就是一个函数对象。将func传递给f，test中的f()就拥有了func()的功能

我们因此可以提高程序的灵活性。可以使用上面的test函数，带入不同的函数参数，比如：

```
test((lambda x, y: x**2 +y), 6, 9)
```

## map()函数

map()是Python的内置函数。它的第一个参数是一个函数对象。

```
re = map((lambda x: x+3), [1,3,5,6])
print re

#输出
[4,6,8,9]
```

这里，map()有两个参数，一个是lambda所定义的函数对象，一个是包含有多个元素的表。map()的功能是将函数对象依次作用于表的每一个表，每次作用的结果储存于返回的表re中。map通过读入的函数（这里是lambda函数）来操作数据（这里“数据”是表中的每一个元素，“操作”是对每个数据加３）。

在Python3.X中，map()的返回值是一个循环对象。可以利用list()函数，将该循环对象转换成表

如果作为参数的函数对象有多个参数，可使用下面的方式，向map()传递函数参数的多个参数：

```
re = map((lambda x, y: x+y), [1,2,3],[6,7,8])
print re

#输出为
[7, 9, 11]
```

map()将每次从两个表中分别取出一个元素，带入lambda所定义的函数

## filter()函数

filter函数的第一个参数也是一个函数对象。它也是将作为参数的函数对象作用于多个元素。如果函数对象返回的是True，则该次的元素被储存于返回的表中。filter通过读入的函数来筛选数据。同样，在Python3.X中，filter返回的不是表，而是循环对象

filter函数的使用如下：

```
def func(a):
    if a > 100:
        return True
    else:
        return False
        
print filter(func,[10, 56,101,500])

#输出是
[101, 500]
```

## reduce()函数

reduce()函数的第一个参数也是函数，但有一个要求，就是这个函数自身能接收两个参数。reduce可以累进地将函数作用于各个参数。如下例：

```
print reduce((lambda x, y: x+y), [1,2,5,7,9])

#输出
24
```

reduce的第一个参数是lambda函数，它接收两个参数x, y，返回x+y。

reduce将表中的两个元素（１和２）传递给lambda函数，得到３。该返回值（３）将作为lambda函数的第一个参数，而表中的下一个元素（５）作为lambda函数的第二个参数，进行下一次的对lambda函数的调用，得到8。依次调用lambda函数，每次lambda函数的第一个参数是上一次运算结果，而第二个参数为表中的下一个元素，直到表中没有剩余元素。

上面例子，相当于(((1+2)+5)+7)+9

>reduce()函数在3.0里面不能直接用的，它被定义在了functools包里面，需要引入包。
