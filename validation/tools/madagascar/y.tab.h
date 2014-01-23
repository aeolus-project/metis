/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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
     rwMINIMIZE = 258,
     rwMETRIC = 259,
     rwINCREASE = 260,
     rwEITHER = 261,
     rwCONSTANTS = 262,
     rwLENGTH = 263,
     rwEXISTS = 264,
     EQUA = 265,
     rwPROBLEM = 266,
     rwFORALL = 267,
     rwIMPLY = 268,
     rwNOT = 269,
     rwWHEN = 270,
     rwOR = 271,
     rwAND = 272,
     rwTYPING = 273,
     rwDOMAIN = 274,
     rwGOAL = 275,
     rwINIT = 276,
     rwOBJECTS = 277,
     rwTYPES = 278,
     rwREQUIREMENTS = 279,
     rwPREDICATES = 280,
     rwPRECOND = 281,
     rwEFFECT = 282,
     rwPARAMS = 283,
     rwACTION = 284,
     rwDEFINE = 285,
     DASH = 286,
     LPAREN = 287,
     RPAREN = 288,
     INT = 289,
     VAR = 290,
     ID = 291,
     rwFUNCTIONS = 292
   };
#endif
/* Tokens.  */
#define rwMINIMIZE 258
#define rwMETRIC 259
#define rwINCREASE 260
#define rwEITHER 261
#define rwCONSTANTS 262
#define rwLENGTH 263
#define rwEXISTS 264
#define EQUA 265
#define rwPROBLEM 266
#define rwFORALL 267
#define rwIMPLY 268
#define rwNOT 269
#define rwWHEN 270
#define rwOR 271
#define rwAND 272
#define rwTYPING 273
#define rwDOMAIN 274
#define rwGOAL 275
#define rwINIT 276
#define rwOBJECTS 277
#define rwTYPES 278
#define rwREQUIREMENTS 279
#define rwPREDICATES 280
#define rwPRECOND 281
#define rwEFFECT 282
#define rwPARAMS 283
#define rwACTION 284
#define rwDEFINE 285
#define DASH 286
#define LPAREN 287
#define RPAREN 288
#define INT 289
#define VAR 290
#define ID 291
#define rwFUNCTIONS 292




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 16 "parser.y"

  int i;
  intlist *intlistp;
  atomlist *atomlistp;
  atom *atomp;
  Sfma *Sfmap;
  Sfmalist *Sfmalistp;
  Seff *Seffp;
  Sefflist *Sefflistp;
  typedvarlist *typedvarlistp;



/* Line 2068 of yacc.c  */
#line 138 "y.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


