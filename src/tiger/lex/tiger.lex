%filenames = "scanner"

 /*
  * Please don't modify the lines above.
  */

 /* You can add lex definitions here. */
 /* FINISHED: Put your lab2 code here */

%x COMMENT STR IGNORE

%%

 /*
  * Below is examples, which you can wipe out
  * and write regular expressions and actions of your own.
  *
  * All the tokens:
  *   Parser::ID
  *   Parser::STRING
  *   Parser::INT
  *   Parser::COMMA
  *   Parser::COLON
  *   Parser::SEMICOLON
  *   Parser::LPAREN
  *   Parser::RPAREN
  *   Parser::LBRACK
  *   Parser::RBRACK
  *   Parser::LBRACE
  *   Parser::RBRACE
  *   Parser::DOT
  *   Parser::PLUS
  *   Parser::MINUS
  *   Parser::TIMES
  *   Parser::DIVIDE
  *   Parser::EQ
  *   Parser::NEQ
  *   Parser::LT
  *   Parser::LE
  *   Parser::GT
  *   Parser::GE
  *   Parser::AND
  *   Parser::OR
  *   Parser::ASSIGN
  *   Parser::ARRAY
  *   Parser::IF
  *   Parser::THEN
  *   Parser::ELSE
  *   Parser::WHILE
  *   Parser::FOR
  *   Parser::TO
  *   Parser::DO
  *   Parser::LET
  *   Parser::IN
  *   Parser::END
  *   Parser::OF
  *   Parser::BREAK
  *   Parser::NIL
  *   Parser::FUNCTION
  *   Parser::VAR
  *   Parser::TYPE
  */

 /* reserved words */
"array" {adjust(); return Parser::ARRAY;}
 /* FINISHED: Put your lab2 code here */
"if"       { adjust(); return Parser::IF; }
"then"     { adjust(); return Parser::THEN; }
"else"     { adjust(); return Parser::ELSE; }
"while"    { adjust(); return Parser::WHILE; }
"for"      { adjust(); return Parser::FOR; }
"to"       { adjust(); return Parser::TO; }
"do"       { adjust(); return Parser::DO; }
"let"      { adjust(); return Parser::LET; }
"in"       { adjust(); return Parser::IN; }
"end"      { adjust(); return Parser::END; }
"of"       { adjust(); return Parser::OF; }
"break"    { adjust(); return Parser::BREAK; }
"nil"      { adjust(); return Parser::NIL; }
"function" { adjust(); return Parser::FUNCTION; }
"var"      { adjust(); return Parser::VAR; }
"type"     { adjust(); return Parser::TYPE; }

[a-zA-Z][_a-zA-Z0-9]* { adjust(); return Parser::ID; }  // 标识符
[0-9]+                 { adjust(); return Parser::INT; } // 整数
","    { adjust(); return Parser::COMMA; }
":"    { adjust(); return Parser::COLON; }
";"    { adjust(); return Parser::SEMICOLON; }
"("    { adjust(); return Parser::LPAREN; }
")"    { adjust(); return Parser::RPAREN; }
"["    { adjust(); return Parser::LBRACK; }
"]"    { adjust(); return Parser::RBRACK; }
"{"    { adjust(); return Parser::LBRACE; }
"}"    { adjust(); return Parser::RBRACE; }
"."    { adjust(); return Parser::DOT; }
"+"    { adjust(); return Parser::PLUS; }
"-"    { adjust(); return Parser::MINUS; }
"*"    { adjust(); return Parser::TIMES; }
"/"    { adjust(); return Parser::DIVIDE; }
"="    { adjust(); return Parser::EQ; }
"<>"   { adjust(); return Parser::NEQ; }
"<"    { adjust(); return Parser::LT; }
"<="   { adjust(); return Parser::LE; }
">"    { adjust(); return Parser::GT; }
">="   { adjust(); return Parser::GE; }
"&"    { adjust(); return Parser::AND; }
"|"    { adjust(); return Parser::OR; }
":="   { adjust(); return Parser::ASSIGN; }

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

 /*
  * skip white space chars.
  * space, tabs and LF
  */
[ \t]+ {adjust();}
\n {adjust(); errormsg_->Newline();}
<<EOF>>  return 0;
 /* illegal input */
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token");}
