/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     KW_AND = 258,
     KW_OR = 259,
     KW_NOT = 260,
     KW_EQUAL = 261,
     KW_LESS = 262,
     KW_NIL = 263,
     KW_LIST = 264,
     KW_APPEND = 265,
     KW_CONCAT = 266,
     KW_SET = 267,
     KW_DEFFUN = 268,
     KW_FOR = 269,
     KW_IF = 270,
     KW_EXIT = 271,
     KW_LOAD = 272,
     KW_PRINT = 273,
     KW_TRUE = 274,
     KW_FALSE = 275,
     OP_OP = 276,
     OP_CP = 277,
     OP_PLUS = 278,
     OP_MINUS = 279,
     OP_MULT = 280,
     OP_DIV = 281,
     VALUEI = 282,
     VALUEF = 283,
     IDENTIFIER = 284
   };
#endif
/* Tokens.  */
#define KW_AND 258
#define KW_OR 259
#define KW_NOT 260
#define KW_EQUAL 261
#define KW_LESS 262
#define KW_NIL 263
#define KW_LIST 264
#define KW_APPEND 265
#define KW_CONCAT 266
#define KW_SET 267
#define KW_DEFFUN 268
#define KW_FOR 269
#define KW_IF 270
#define KW_EXIT 271
#define KW_LOAD 272
#define KW_PRINT 273
#define KW_TRUE 274
#define KW_FALSE 275
#define OP_OP 276
#define OP_CP 277
#define OP_PLUS 278
#define OP_MINUS 279
#define OP_MULT 280
#define OP_DIV 281
#define VALUEI 282
#define VALUEF 283
#define IDENTIFIER 284




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

