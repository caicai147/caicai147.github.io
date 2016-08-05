---
layout: post
title: Delphi的Format格式化函数
categories: delphi之精确使用api
tags: delphi format api
---


转载自：[http://www.cnblogs.com/mumble/archive/2011/05/25/2056462.html](http://www.cnblogs.com/mumble/archive/2011/05/25/2056462.html)
 
Format是一个很常用，却又似乎很烦的方法，本人试图对这个方法的帮助进行一些翻译，让它有一个完整的概貌，以供大家查询之用：

其实在看Delphi的Format函数的时候，可以与C 里面的printf 函数的格式化相关的知识进行类比。

#首先看它的声明#

    function Format(const Format: string; const Args: array of const): string; overload;

事实上Format方法有两个种形式，另外一种是三个参数的，主要区别在于它是线程安全的，但并不多用，所以这里只对第一个介绍：

    function Format(const Format: string; const Args: array of const): string; overload;

Format参数是一个格式字符串，用于格式化Args里面的值的。Args又是什么呢，它是一个变体数组，即它里面可以有多个参数，而且每个参数可以不同。如以下例子：

    Format('my name is %6s',['wind']);
    //返回后就是my name is wind


#再来看Format参数的详细情况#

Format里面可以写普通的字符串，比如'my name is',但有些格式指令字符具有特殊意义，比如"%6s"格式指令具有以下的形式：

    "%" [index ":"] ["-"] [width] ["." prec] type

它是以"%"开始,而以type结束，type表示一个具体的类型。中间是用来格式化type类型的指令字符，是可选的。


##先来看看type,type可以是以下字符##

###d###

十制数，表示一个整型值

###u###

和d一样是整型值，但它是无符号的，而如果它对应的值是负的，则返回时是一个2的32次方减去这个绝对值的数,如：

    Format('this is %u',[－2]);
    //返回的是：this is 4294967294

###f###

对应浮点数

###e###

科学表示法，对应整型数和浮点数，比如

    Format('this is %e',[-2.22]);
    //返回的是：this is -2.22000000000000E+000,等一下再说明如果将数的精度缩小

###g###

这个只能对应浮点型，且它会将值中多余的数去掉,比如

    Format('this is %g',[02.200]);
    //返回的是：this is 2.2

###n###

只能对应浮点型，将值转化为号码的形式。看一个例子就明白了

    Format('this is %n',[4552.2176]);
    //返回的是this is 4,552.22

>注意有两点，一是只表示到小数后两位，等一下说怎么消除这种情况, 二是，即使小数没有被截断，它也不会也像整数部分一样有逗号来分开的

###m###

钱币类型，但关于货币类型有更好的格式化方法，这里只是简单的格式化,另外它只对应于浮点值

    Format('this is %m',[9552.21]);
    //返回：this is ￥9,552.21

###p###

对应于指针类型，返回的值是指针的地址，以十六进制的形式来表示,例如：

    var X:integer;
    p:^integer;
    begin
    X:=99;
    p:=@X;
    Edit1.Text:=Format('this is %p',[p]);
    end;
    //Edit1的内容是：this is 0012F548

###s###

对应字符串类型，不用多说了吧

###x###

必须是一个整形值，以十六进制的形式返回

    Edit1.Text:=Format('this is %X',[15]);
    //返回是：this is F


##类型讲述完毕，下面介绍格式化Type的指令##

###[index ":"]###

[index ":"]这个要怎么表达呢，看一个例子

    Format('this is %d %d',[12,13]);
    //其中第一个%d的索引是0，第二个%d是1，所以字符显示的时候是这样 this is 12 13

而如果你这样定义：

    Format('this is %1:d %0:d',[12,13]);
    //那么返回的字符串就变成了this is 13 12。
    
现在明白了吗，[index ":"] 中的index指示Args中参数显示的顺序还有一种情况，如果这样

    Format('%d %d %d %0:d %d', [1, 2, 3, 4])
    //将返回1 2 3 1 2。

如果你想返回的是1 2 3 1 4，必须这样定：

    Format('%d %d %d %0:d %3:d', [1, 2, 3, 4])

但用的时候要注意，索引不能超出Args中的个数，不然会引起异常（因为可能引起异常，所以在进行format的时候应该使用 try ... except... 方法来捕获可能出现的异常并处理）如

    Format('this is %2:d %0:d',[12,13]);

由于Args中只有12 13 两个数，所以Index只能是0或1，这里为2就错了[width] 指定将被格式化的值占的宽度，看一个例子就明白了

    Format('this is %4d',[12]);
    //输出是：this is   12,这个是比较容易，不过如果Width的值小于参数的长度，则没有效果。

如：

    Format('this is %1d',[12]);
    //输出是：this is 12

###["-"]###

["-"]这个指定参数向左齐，和[width]合在一起最可以看到效果：

    Format('this is %-4d,yes',[12]);
    //输出是：this is 12   ,yes

###["." prec]###

["." prec]指定精度，对于浮点数效果最佳：

    Format('this is %.2f',['1.1234]);
    //输出 this is 1.12
    Format('this is %.7f',['1.1234]);
    //输出了 this is 1.1234000

而对于整型数，如果prec比如整型的位数小，则没有效果反之比整形值的位数大，则会在整型值的前面以0补之

    Format('this is %.7d',[1234]);
    //输出是：this is 0001234]

对于字符型，刚好和整型值相反，如果prec比字符串型的长度大则没有效果，反之比字符串型的长度小，则会截断尾部的字符

    Format('this is %.2s',['1234']);
    //输出是 this is 12,而上面说的这个例子：

或者

    Format('this is %e',[-2.22]);
    //返回的是：this is -2.22000000000000E+000,怎么去掉多余的0呢，这个就行啦

或者

    Format('this is %.2e',[-2.22]);


好了，第一个总算讲完了，应该对它的应用很熟悉了吧
