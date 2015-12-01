---
layout: post
title: delphi怎么分割字符串
categories: delphi之字符串 delphi之精确使用api
tags: delphi 函数 字符串
---

* 说到Delphi中的分割字符串的函数，大多数人都会想到ExtractStrings，确实它的功能很强大
* 但是有一些场景ExtractStrings却是心有余而力不足
* 这时候就需要考虑其他的解决方案，比如想想自己实现一个简单的函数

###先介绍一下ExtractStrings

下面的附录中有Delphi的ExtractStrings的官方说明和其代码实现

###附录

**Delphi的官方帮助文档的解释是：**

>Fills a string list with substrings parsed from a delimited list.

>**Unit**

>Classes

>**Category**

>string handling routines (null-terminated)

>function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings): Integer;

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
