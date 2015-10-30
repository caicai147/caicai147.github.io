---
layout: post
title: markdown简要语法
categories: 好资源之开发神器
tags: markdown 排版 博客
---


Markdown 是一种书写结构化文档的轻量标记语言，它定制了一种原文本书写格式，利用这种格式书写的文档内容结构清晰，查看方便，书写也很简单，没有多少标签符号，不像 HTML 那样需要写大量标签标记，也不需要用额外的 IDE 来编辑，随便一个文本编辑器就可开工，可以说是一种很轻巧、灵活、随性的内容记录标记语言。

以前在写博客的时候，就在纠结为什么自己的博客的排版这么差，现在使用markdown之后，只需要简单的一些控制，最后出来的博客的效果非常的统一、一致、整洁，真的很好！

目前我在ubuntu上使用的markdown编辑器是：ReText。

转义
----

使用 \\ 来对下面的 \* 、 \` 等这样的特殊字符进行转义，保证可以输出这些字符。

多说一点，这篇博客是使用markdown来说明markdown的语法，为了保证最后的排版效果是ok的——也就是可以显示这些特殊字符，所以需要对其中的特殊字符转义。

不过本文因为特殊字符特别多，所以很多也并没有使用到转义字符，而是通过代码块的形式展示这些语法，但是如果是使用\`\`\`的话导致最终的的效果还是不好，所以就选择的是使用tab键的方式来展示“代码块”，所以最终可以看到本文的排版效果。

>总之，markdown是很简单的，但是就像研究其他所有的编程语言一样，要想比较熟练掌握markdown，不能说只是单纯的看这些语法，一定要多用，慢慢摸索

斜体  
----

        *emphasize*

也可以写成  

        _emphasize_


加粗  
-----

        **emphasize**

也可以写成  

        __emphasize__


链接  
-----

        [谷歌网址](http://http://www.google.com/ "谷歌网址")


Email  
------

        Email Written <xumenger@126.com>


图片  
------

        ![img](/image_path/image.jpg "Image Title")


标题  
-----

        标题1  
        =====  
        标题2  
        -----  

也可以这样写：  

        # 标题 1 #  
        ## 标题 2 ##  
        ### 标题 3 ###  
        #### 标题 4 ####  
        ##### 标题 5  
        ###### 标题 6  


列表  
-----

有序列表（注意1.和One之间有一个空格）  

        1. One  
        2. Two  
        3. Three  

无序列表（注意*和list1之间有一个空格）  

        * list1  
        * list2  
        * list3  
    
使用`-`符号是和使用`*`符号是相同的


引用  
----

        > Just a test!


内联代码  
----

        Just a `test`  
        Just a `` `test` ``

代码块  
----

缩进4个空格或者1个制表符tab  

        this is a block code!


另一种写法：代码块周围各加3个 \`  

        ```
        this is a block code!
        ```


横线  
---

3个或3个以上的破折号或星号  

        ---  
         
        * * *  
         
        - - - -


强制换行  
-----

行尾加2个或2个以上的空格

        break line  
        new line

因为有的时候直接在编辑markdown文档的时候使用一个回车换行，可是最后出来的效果并没有换行（如果使用两个回车连续两次换行，最后的显示出来的效果还是很好的），所以需要在第一行的最后加上两个空格，以保证第二行在最终在网页上显示的时候能够真正换行。

删除线  
----

        ~~Strikethrough~~


表格  
----

        First Header | Second Header | Third Header  
        ------------ | ------------- | ------------  
        Content Cell | Content Cell  | Content Cell  
        Content Cell | Content Cell  | Content Cell

或者  

        | First Header | Second Header | Third Header |  
        | ------------ | ------------- | ------------ |  
        | Content Cell | Content Cell  | Content Cell |  
        | Content Cell | Content Cell  | Content Cell |
