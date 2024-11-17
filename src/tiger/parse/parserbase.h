// Generated by Bisonc++ V6.04.03 on Sun, 10 Nov 2024 08:26:48 +0000

// hdr/includes
#ifndef ParserBase_h_included
#define ParserBase_h_included

#include <exception>
#include <vector>
#include <iostream>
// $insert preincludes
#include "tiger/absyn/absyn.h"

// hdr/baseclass

namespace // anonymous
{
    struct PI_;
}



// $insert parserbase
class ParserBase
{
    public:
        enum DebugMode_
        {
            OFF           = 0,
            ON            = 1 << 0,
            ACTIONCASES   = 1 << 1
        };

// $insert tokens

    // Symbolic tokens:
    enum Tokens_
    {
        ID = 257,
        STRING,
        INT,
        COMMA,
        COLON,
        SEMICOLON,
        LPAREN,
        RPAREN,
        LBRACK,
        RBRACK,
        LBRACE,
        RBRACE,
        DOT,
        ASSIGN,
        ARRAY,
        IF,
        THEN,
        ELSE,
        WHILE,
        FOR,
        TO,
        DO,
        LET,
        IN,
        END,
        OF,
        BREAK,
        NIL,
        FUNCTION,
        VAR,
        TYPE,
        AND,
        OR,
        EQ,
        NEQ,
        LT,
        LE,
        GT,
        GE,
        PLUS,
        MINUS,
        TIMES,
        DIVIDE,
        UMINUS,
    };

// $insert STYPE
union STYPE_
{
 int ival;
 std::string* sval;
 sym::Symbol *sym;
 absyn::Exp *exp;
 absyn::ExpList *explist;
 absyn::Var *var;
 absyn::DecList *declist;
 absyn::Dec *dec;
 absyn::EFieldList *efieldlist;
 absyn::EField *efield;
 absyn::NameAndTyList *tydeclist;
 absyn::NameAndTy *tydec;
 absyn::FieldList *fieldlist;
 absyn::Field *field;
 absyn::FunDecList *fundeclist;
 absyn::FunDec *fundec;
 absyn::Ty *ty;
 };


    private:
                        // state  semval
        typedef std::pair<size_t, STYPE_> StatePair;
                       // token   semval
        typedef std::pair<int,    STYPE_> TokenPair;

        int d_stackIdx = -1;
        std::vector<StatePair> d_stateStack;
        StatePair  *d_vsp = 0;       // points to the topmost value stack
        size_t      d_state = 0;

        TokenPair   d_next;
        int         d_token;

        bool        d_terminalToken = false;
        bool        d_recovery = false;


    protected:
        enum Return_
        {
            PARSE_ACCEPT_ = 0,   // values used as parse()'s return values
            PARSE_ABORT_  = 1
        };
        enum ErrorRecovery_
        {
            UNEXPECTED_TOKEN_,
        };

        bool        d_actionCases_ = false;    // set by options/directives
        bool        d_debug_ = true;
        size_t      d_requiredTokens_;
        size_t      d_nErrors_;                // initialized by clearin()
        size_t      d_acceptedTokens_;
        STYPE_     d_val_;


        ParserBase();

        void ABORT() const;
        void ACCEPT() const;
        void ERROR() const;

        STYPE_ &vs_(int idx);             // value stack element idx 
        int  lookup_() const;
        int  savedToken_() const;
        int  token_() const;
        size_t stackSize_() const;
        size_t state_() const;
        size_t top_() const;
        void clearin_();
        void errorVerbose_();
        void lex_(int token);
        void popToken_();
        void pop_(size_t count = 1);
        void pushToken_(int token);
        void push_(size_t nextState);
        void redoToken_();
        bool recovery_() const;
        void reduce_(int rule);
        void shift_(int state);
        void startRecovery_();

    public:
        void setDebug(bool mode);
        void setDebug(DebugMode_ mode);
}; 

// hdr/abort
inline void ParserBase::ABORT() const
{
    throw PARSE_ABORT_;
}

// hdr/accept
inline void ParserBase::ACCEPT() const
{
    throw PARSE_ACCEPT_;
}


// hdr/error
inline void ParserBase::ERROR() const
{
    throw UNEXPECTED_TOKEN_;
}

// hdr/savedtoken
inline int ParserBase::savedToken_() const
{
    return d_next.first;
}

// hdr/opbitand
inline ParserBase::DebugMode_ operator&(ParserBase::DebugMode_ lhs,
                                     ParserBase::DebugMode_ rhs)
{
    return static_cast<ParserBase::DebugMode_>(
            static_cast<int>(lhs) & rhs);
}

// hdr/opbitor
inline ParserBase::DebugMode_ operator|(ParserBase::DebugMode_ lhs, 
                                     ParserBase::DebugMode_ rhs)
{
    return static_cast<ParserBase::DebugMode_>(static_cast<int>(lhs) | rhs);
};

// hdr/recovery
inline bool ParserBase::recovery_() const
{
    return d_recovery;
}

// hdr/stacksize
inline size_t ParserBase::stackSize_() const
{
    return d_stackIdx + 1;
}

// hdr/state
inline size_t ParserBase::state_() const
{
    return d_state;
}

// hdr/token
inline int ParserBase::token_() const
{
    return d_token;
}

// hdr/vs
inline ParserBase::STYPE_ &ParserBase::vs_(int idx) 
{
    return (d_vsp + idx)->second;
}


// hdr/tail
// For convenience, when including ParserBase.h its symbols are available as
// symbols in the class Parser.
#define Parser ParserBase



#endif



