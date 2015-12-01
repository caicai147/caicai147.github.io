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

Delphi的官方帮助文档的解释是：

```
Fills a string list with substrings parsed from a delimited list.

Unit

Classes

Category

string handling routines (null-terminated)

function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings): Integer;

Description

Use ExtractStrings to fill a string list with the substrings of the null-terminated string specified by Content.

Separators is a set of characters that are used as delimiters, separating the substrings. Carriage returns, newline characters, and quote characters (single or double) are always treated as separators. Separators are ignored when inside a quoted string until the final end quote. (Note that quoted characters can appear in a quoted string if the quote character is doubled.)

WhiteSpace is a set of characters to be ignored when parsing Content if they occur at the beginning of a string.

Content is the null-terminated string to parse into substrings.

Strings is a string list to which all substrings parsed from Content are added. The string list is not cleared by ExtractStrings, so any strings already in the string list are preserved.

ExtractStrings returns the number of strings added to the Strings parameter.

Note:	ExtractStrings does not add empty strings to the list.
```
