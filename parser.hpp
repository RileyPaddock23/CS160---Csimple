/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_HPP_INCLUDED
# define YY_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    BOOL = 258,                    /* BOOL  */
    CHAR = 259,                    /* CHAR  */
    INT = 260,                     /* INT  */
    STR = 261,                     /* STR  */
    INTPTR = 262,                  /* INTPTR  */
    CHARPTR = 263,                 /* CHARPTR  */
    IF = 264,                      /* IF  */
    ELSE = 265,                    /* ELSE  */
    WHILE = 266,                   /* WHILE  */
    VAR = 267,                     /* VAR  */
    PROC = 268,                    /* PROC  */
    RETURN = 269,                  /* RETURN  */
    AND = 270,                     /* AND  */
    REF = 271,                     /* REF  */
    EQ = 272,                      /* EQ  */
    IS = 273,                      /* IS  */
    DIV = 274,                     /* DIV  */
    LEQ = 275,                     /* LEQ  */
    GEQ = 276,                     /* GEQ  */
    GT = 277,                      /* GT  */
    LT = 278,                      /* LT  */
    MINUS = 279,                   /* MINUS  */
    NEQ = 280,                     /* NEQ  */
    NOT = 281,                     /* NOT  */
    OR = 282,                      /* OR  */
    PLUS = 283,                    /* PLUS  */
    TIMES = 284,                   /* TIMES  */
    DEREF = 285,                   /* DEREF  */
    SEMI = 286,                    /* SEMI  */
    COLON = 287,                   /* COLON  */
    COMMA = 288,                   /* COMMA  */
    ABS = 289,                     /* ABS  */
    BRACKO = 290,                  /* BRACKO  */
    BRACKC = 291,                  /* BRACKC  */
    PARENO = 292,                  /* PARENO  */
    PARENC = 293,                  /* PARENC  */
    SBRACKO = 294,                 /* SBRACKO  */
    SBRACKC = 295,                 /* SBRACKC  */
    BOOL_VAL = 296,                /* BOOL_VAL  */
    INT_VAL = 297,                 /* INT_VAL  */
    CHAR_VAL = 298,                /* CHAR_VAL  */
    STRING_VAL = 299,              /* STRING_VAL  */
    ID = 300,                      /* ID  */
    KNULL = 301                    /* KNULL  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PARSER_HPP_INCLUDED  */
