# Compiler-lab2-doc

## handle comments

词法分析器能够处理嵌套的注释。通过状态切换来跟踪多层注释嵌套，当遇到 `/*` 时进入 `COMMENT`状态，并根据注释层级调整。当遇到 `*/` 时，如果当前注释层级大于 0，表示还有未结束的嵌套注释，继续减少层级，否则回到初始状态。

进入 `COMMENT` 状态后，所有字符和换行符都被忽略。

嵌套的注释通过 `comment_level_` 变量来追踪，处理嵌套时，遇到新的 `/*` 会增加嵌套层数，遇到 `*/` 会减少层数，直到层数为 0 时退出注释状态。

代码如下：

```lex
// 处理注释的开始
"/*" {adjust(); comment_level_ = 0; begin(StartCondition_::COMMENT); }
<COMMENT>{
    // 处理注释结束
    "*/"  {adjustStr(); if (comment_level_ > 0) comment_level_--; else begin(StartCondition_::INITIAL); } 
    // 忽略注释中的字符和换行
    \n|.  {adjustStr(); }
    // 处理嵌套的注释层
    "/*"  {adjustStr(); comment_level_++; }
}
```

## handle strings

字符串处理时，词法分析器使用 `STR` 状态来处理字符串的内容。它能够处理字符串中的常见转义字符，如换行符 `\n`、制表符 `\t`，以及转义的引号 `\"` 和反斜杠 `\\`。

转义字符（如 `\\n`、`\\t`）会被正确处理并存储在 `string_buf_` 中。

支持 ASCII 码的转义字符（例如 `\123`）。

结束符 `"` 会结束字符串状态并返回 `Parser::STRING`。

```lex
// 处理字符串的开始
\" {adjust(); begin(StartCondition_::STR); string_buf_.clear(); }
<STR>{
    // 字符串结束处理
    \"    {adjustStr(); begin(StartCondition_::INITIAL); setMatched(string_buf_); return Parser::STRING; }
    // 处理字符串中的换行符
    \\n   {adjustStr(); string_buf_ += '\n'; }
    // 处理字符串中的制表符
    \\t   {adjustStr(); string_buf_ += '\t'; }
    
    // 处理转义的引号
    \\\"  {adjustStr(); string_buf_ += '\"'; }
    // 处理转义的反斜杠
    \\\\  {adjustStr(); string_buf_ += '\\'; }
    // 处理ASCII码的转义
    \\[0-9]{3} {adjustStr(); string_buf_ += static_cast<char>(atoi(matched().c_str() + 1)); }
    // 忽略某些转义字符
    \\[ \f\n\t]+\\  {adjustStr(); }
    // 处理控制字符，如 ^C
    \\\^[A-Z] {adjustStr(); string_buf_ += matched()[2] - 'A' + 1; }
    // 处理所有其他字符
    . {adjustStr(); string_buf_ += matched(); }
}
```

## error handling

对于无法识别的字符，词法分析器会捕获并报告错误。通过调用 `errormsg_->Error` 函数，将错误位置和错误信息传递给错误处理模块。

```lex
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token");}
```

## end-of-file handling

当词法分析器遇到文件结束符（EOF）时，返回 `0`，表示输入已经处理完毕，词法分析结束。

```lex
<<EOF>> return 0;
```

