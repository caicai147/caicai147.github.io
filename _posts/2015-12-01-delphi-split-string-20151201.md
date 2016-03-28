---
layout: post
title: Delphi怎么分割字符串
categories: delphi之字符串 delphi之精确使用api
tags: delphi 函数 字符串
---

##补充

**Add in 2016-03-10**

下面说到ExtractStrings这个函数的缺陷（当出现连续两个分割符，即分割符之间没有没有字符串，就不会按照预期得到分割的效果），同时提供了一个使用TStringList的解决方案，但是使用这个TStringList的解决方案也是有缺陷的，下面来通过代码展示一下

```
procedure splitString(Separators: char; Content: PChar; Strings: TStrings) ;
begin
  Strings.Delimiter:= Separators;
  Strings.DelimitedText:= Content;
end;

var
  s, resultStr: string;
  slist: TStrings;
  i: Integer;
begin
  s:= '|str1|str2 str3||str4|';
  slist:= TStringList.Create;
  splitString('|', PChar(s), slist);
  //预期最后解析到slist中的是：
  //第一个元素是空字符串''，第二个元素是'str1'，第三个元素是'str2 str3'，
  //第四个元素是''，第五个元素是'str4'，第六个元素是''
  
  resultStr:= '';
  for i:= 0 to slist.Count - 1 do
  begin
    resultStr:= resultStr + '-' + slist[i];
  end;
  
  //按照上面的描述，预期得到的结果是：'--str1-str str3--str4-'
  ShowMessage(resultStr);
  //但是实际输出结果是：'--str1-str2-str3--str4-'
end;
```

可以看出来这种分割字符串的方式，在遇到字符串中有空格的情况也会默认将空格作为分割符，虽然你指定的分割符不是空格。如果在一个报文格式定义中使用'|'作为分割符，但是实际报文字符串中又可能存在空格，那么使用TStringList可能就会得到和报文格式规范有出入的结果了。

现在ExtractStrings函数分割字符串、使用TStringList分割字符串都有问题，需要寻找一个合适的方法来解决，目前暂时想到的是使用TStringList的方式，在分割之前先将字符串中的空格替换成一个特殊的字符（不是空格、不和分割符重复、不和字符串中的其他任何字符重复），然后通过分割符分割，分割完之后再对每个子字符串进行将这个特殊字符替换为空格的处理！

不过这就有点搞复杂了，是不是还有什么更好的方法，等到以后在实际开发中遇到问题的时候再去找到好的方案吧！

---

**Add in 2016-03-28**

继续说明一下，使用TStringList在处理空格时还有一种特殊情况，直接见下面的代码说明

```
procedure splitString(Separators: char; Content: PChar; Strings: TStrings) ;
begin
  Strings.Delimiter:= Separators;
  Strings.DelimitedText:= Content;
end;

var
  s, resultStr: string;
  slist: TStrings;
  i: Integer;
begin
  s:= '|  str1  |str2 str3||str4|';
  slist:= TStringList.Create;
  splitString('|', PChar(s), slist);    //使用 '|'作为分隔符
  //预期最后解析到slist中的是：
  //第一个元素是空字符串''，第二个元素是'str1'，第三个元素是'str2 str3'，
  //第四个元素是''，第五个元素是'str4'，第六个元素是''
  
  resultStr:= '';
  for i:= 0 to slist.Count - 1 do
  begin
    resultStr:= resultStr + '-' + slist[i];
  end;
  
  //'|  str1  |str2 str3||str4|' 字符串的 str1 子串前后各有两个空格，分割的结果应该类似 '|||str1|||str2 str3||str4|'
  //按照 “Add in 2016-03-10” 的描述，预期得到的结果是：'----str1---str2-str3--str4-'
  ShowMessage(resultStr);
  //但是实际输出结果还是：'--str1-str2-str3--str4-'
end;
```

---

---

##正文开始

* 说到Delphi中的分割字符串的函数，大多数人都会想到ExtractStrings，确实它的功能很强大
* 但是有一些场景ExtractStrings却是心有余而力不足
* 这时候就需要考虑其他的解决方案，比如想想自己实现一个简单的函数

###先介绍一下ExtractStrings

下面的附录中有Delphi的ExtractStrings的官方说明和其代码实现

首先在[万一的博客](http://www.cnblogs.com/del/archive/2007/12/11/991479.html)中有简单的讲过ExtractStrings的使用

```
//分割字符串 ExtractStrings
var
  s: String;
  List: TStringList;
begin
  s := 'about: #delphi; #pascal, programming';
  List := TStringList.Create;
  ExtractStrings([';',',',':'],['#',' '],PChar(s),List);
  //第一个参数是分割符; 第二个参数是开头被忽略的字符

  ShowMessage(List.Text);  
                           //about
                           //delphi
                           //pascal
                           //programming
  List.Free;
end;
```

###但是下面这样的场景就有限制

首先看看ExtractStrings在帮助文档中有这样的说明:

>**Note**:	ExtractStrings does not add empty strings to the list.

也就是说使用ExtractStrings解析字符串的时候，不会将其中的空格放到链表中，比如这样的应用场景：需要解析类似这样的字符串'|str1|str2||str4|'，希望以'|'为分割符号进行解析，可能就会直接这样使用ExtractStrings：

```
var
  s1, s2, s3, s4: string;
  i: Integer;
  slist1, slist2, slist3, slist4: TStrings;
begin
  slist:= TStringList.Create;
  s1:= '|str1|str2||str4|';
  s2:= 'str1|str2||str4|';
  s3:= 'str1|str2||str3|str4';
  s4:= 'str1|str2|str3|str4';
  
  ExtractStrings(['|'],  [],  PChar(s1), slist1);
  //最后解析出来的slist中只有三个子字符串：'str1'，'str2'，'str4'，
  //所以显然第一个'|'之前的空字符串部分没有解析，中间的两个||之间的空字符串没有解析，最后一个'|'后面的空字符串没有解析
  
  ExtractStrings(['|'],  [],  PChar(s2), slist2);
  //同上，只解析非空的子串放到slist2中，对于几个空字符串是没有解析放到slist2中的
  
  ExtractStrings(['|'],  [],  PChar(s3), slist3);
  //同上，只解析非空的子串放到slist2中，对于几个空字符串是没有解析放到slist3中的
  
  ExtractStrings(['|'],  [],  PChar(s4), slist4);
  //只有在这种分割符前后没有空字符串的情况下，才能解析出 “分割符个数 + 1” 个的子串
end;
```

但是这种时候非得要保留那些空字符串，希望将其解析到list中，这时候ExtractStrings是无能为力的。所以可以自己封装一个函数，没有ExtractStrings功能大，但是可以弥补它在这种情境下的不足，见下面的代码

```
procedure splitString(Separators: char; Content: PChar; Strings: TStrings) ;
begin
  Strings.Delimiter:= Separators;
  Strings.DelimitedText:= Content;
end;

var
  s, resultStr: string;
  slist: TStrings;
  i: Integer;
begin
  s:= '|str1|str2||str4|';
  slist:= TStringList.Create;
  splitString('|', PChar(s), slist);
  //最后解析到slist中的是：
  //第一个元素是空字符串''，第二个元素是'str1'，第三个元素是'str2'，
  //第四个元素是''，第五个元素是'str4'，第六个元素是''
  
  resultStr:= '';
  for i:= 0 to slist.Count - 1 do
  begin
    resultStr:= resultStr + '-' + slist[i];
  end;
  ShowMessage(resultStr);
  //输出结果是：'--str1-str2--str4-'
end;
```

###附录

**Delphi的官方帮助文档的解释是：**

>Fills a string list with substrings parsed from a delimited list.

>**Unit**

>Classes

>**Category**

>string handling routines (null-terminated)

>`function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings): Integer;`

>**Description**

>Use ExtractStrings to fill a string list with the substrings of the null-terminated string specified by Content.

>Separators is a set of characters that are used as delimiters, separating the substrings. Carriage returns, newline characters, and quote characters (single or double) are always treated as separators. Separators are ignored when inside a quoted string until the final end quote. (Note that quoted characters can appear in a quoted string if the quote character is doubled.)

>WhiteSpace is a set of characters to be ignored when parsing Content if they occur at the beginning of a string.

>Content is the null-terminated string to parse into substrings.

>Strings is a string list to which all substrings parsed from Content are added. The string list is not cleared by ExtractStrings, so any strings already in the string list are preserved.

>ExtractStrings returns the number of strings added to the Strings parameter.

>**Note**:	ExtractStrings does not add empty strings to the list.

**ExtractStrings的代码实现如下**

```
function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings): Integer;
var
  Head, Tail: PChar;
  EOS, InQuote: Boolean;
  QuoteChar: Char;
  Item: string;
begin
  Result := 0;
  if (Content = nil) or (Content^=#0) or (Strings = nil) then Exit;
  Tail := Content;
  InQuote := False;
  QuoteChar := #0;
  Strings.BeginUpdate;
  try
    repeat
      while Tail^ in WhiteSpace + [#13, #10] do Inc(Tail);
      Head := Tail;
      while True do
      begin
        while (InQuote and not (Tail^ in ['''', '"', #0])) or
          not (Tail^ in Separators + [#0, #13, #10, '''', '"']) do Inc(Tail);
        if Tail^ in ['''', '"'] then
        begin
          if (QuoteChar <> #0) and (QuoteChar = Tail^) then
            QuoteChar := #0
          else QuoteChar := Tail^;
          InQuote := QuoteChar <> #0;
          Inc(Tail);
        end else Break;
      end;
      EOS := Tail^ = #0;
      if (Head <> Tail) and (Head^ <> #0) then
      begin
        if Strings <> nil then
        begin
          SetString(Item, Head, Tail - Head);
          Strings.Add(Item);
        end;
        Inc(Result);
      end;
      Inc(Tail);
    until EOS;
  finally
    Strings.EndUpdate;
  end;
end;
```
