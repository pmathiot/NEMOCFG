/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         fortran_parse
#define yylex           fortran_lex
#define yyerror         fortran_error
#define yylval          fortran_lval
#define yychar          fortran_char
#define yydebug         fortran_debug
#define yynerrs         fortran_nerrs

/* Copy the first part of user declarations.  */
/* Line 371 of yacc.c  */
#line 36 "fortran.y"

#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

extern int line_num_input;
extern char *fortran_text;

char c_selectorname[LONG_M];
char ligne[LONG_M];
char truename[LONG_VNAME];
char identcopy[LONG_VNAME];
int c_selectorgiven=0;
listvar *curlistvar;
typedim c_selectordim;
listcouple *coupletmp;
int removeline=0;
listvar *test;

int fortran_error(const char *s)
{
    printf("%s line %d, file %s motclef = |%s|\n", s, line_num_input, cur_filename, fortran_text);
    exit(1);
}


/* Line 371 of yacc.c  */
#line 104 "fortran.tab.c"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int fortran_debug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOK_NEQV = 258,
     TOK_EQV = 259,
     TOK_XOR = 260,
     TOK_OR = 261,
     TOK_AND = 262,
     TOK_NOT = 263,
     TOK_NE = 264,
     TOK_EQ = 265,
     TOK_GE = 266,
     TOK_LE = 267,
     TOK_GT = 268,
     TOK_LT = 269,
     TOK_DSLASH = 270,
     TOK_SLASH = 271,
     TOK_DASTER = 272,
     TOK_SEMICOLON = 273,
     TOK_PARAMETER = 274,
     TOK_RESULT = 275,
     TOK_ONLY = 276,
     TOK_INCLUDE = 277,
     TOK_SUBROUTINE = 278,
     TOK_PROGRAM = 279,
     TOK_FUNCTION = 280,
     TOK_FORMAT = 281,
     TOK_MAX = 282,
     TOK_TANH = 283,
     TOK_WHERE = 284,
     TOK_ELSEWHEREPAR = 285,
     TOK_ELSEWHERE = 286,
     TOK_ENDWHERE = 287,
     TOK_MAXVAL = 288,
     TOK_TRIM = 289,
     TOK_NULL_PTR = 290,
     TOK_SUM = 291,
     TOK_SQRT = 292,
     TOK_CASE = 293,
     TOK_SELECTCASE = 294,
     TOK_FILE = 295,
     TOK_UNIT = 296,
     TOK_FMT = 297,
     TOK_NML = 298,
     TOK_END = 299,
     TOK_EOR = 300,
     TOK_ERR = 301,
     TOK_EXIST = 302,
     TOK_MIN = 303,
     TOK_FLOAT = 304,
     TOK_EXP = 305,
     TOK_COS = 306,
     TOK_COSH = 307,
     TOK_ACOS = 308,
     TOK_NINT = 309,
     TOK_CYCLE = 310,
     TOK_SIN = 311,
     TOK_SINH = 312,
     TOK_ASIN = 313,
     TOK_EQUIVALENCE = 314,
     TOK_BACKSPACE = 315,
     TOK_LOG = 316,
     TOK_TAN = 317,
     TOK_ATAN = 318,
     TOK_RECURSIVE = 319,
     TOK_ABS = 320,
     TOK_MOD = 321,
     TOK_SIGN = 322,
     TOK_MINLOC = 323,
     TOK_MAXLOC = 324,
     TOK_EXIT = 325,
     TOK_MINVAL = 326,
     TOK_PUBLIC = 327,
     TOK_PRIVATE = 328,
     TOK_ALLOCATABLE = 329,
     TOK_RETURN = 330,
     TOK_THEN = 331,
     TOK_ELSEIF = 332,
     TOK_ELSE = 333,
     TOK_ENDIF = 334,
     TOK_PRINT = 335,
     TOK_PLAINGOTO = 336,
     TOK_LOGICALIF = 337,
     TOK_PLAINDO = 338,
     TOK_CONTAINS = 339,
     TOK_ENDDO = 340,
     TOK_MODULE = 341,
     TOK_ENDMODULE = 342,
     TOK_WHILE = 343,
     TOK_CONCURRENT = 344,
     TOK_ALLOCATE = 345,
     TOK_OPEN = 346,
     TOK_CLOSE = 347,
     TOK_INQUIRE = 348,
     TOK_WRITE = 349,
     TOK_FLUSH = 350,
     TOK_READ = 351,
     TOK_REWIND = 352,
     TOK_DEALLOCATE = 353,
     TOK_NULLIFY = 354,
     TOK_DIMENSION = 355,
     TOK_ENDSELECT = 356,
     TOK_EXTERNAL = 357,
     TOK_INTENT = 358,
     TOK_INTRINSIC = 359,
     TOK_NAMELIST = 360,
     TOK_DEFAULT = 361,
     TOK_OPTIONAL = 362,
     TOK_POINTER = 363,
     TOK_CONTINUE = 364,
     TOK_SAVE = 365,
     TOK_TARGET = 366,
     TOK_IMPLICIT = 367,
     TOK_NONE = 368,
     TOK_CALL = 369,
     TOK_STAT = 370,
     TOK_POINT_TO = 371,
     TOK_COMMON = 372,
     TOK_GLOBAL = 373,
     TOK_LEFTAB = 374,
     TOK_RIGHTAB = 375,
     TOK_PAUSE = 376,
     TOK_PROCEDURE = 377,
     TOK_STOP = 378,
     TOK_REAL8 = 379,
     TOK_FOURDOTS = 380,
     TOK_HEXA = 381,
     TOK_ASSIGNTYPE = 382,
     TOK_OUT = 383,
     TOK_INOUT = 384,
     TOK_IN = 385,
     TOK_USE = 386,
     TOK_TRUE = 387,
     TOK_FALSE = 388,
     TOK_LABEL = 389,
     TOK_TYPE = 390,
     TOK_TYPEPAR = 391,
     TOK_ENDTYPE = 392,
     TOK_REAL = 393,
     TOK_INTEGER = 394,
     TOK_LOGICAL = 395,
     TOK_DOUBLEPRECISION = 396,
     TOK_ENDSUBROUTINE = 397,
     TOK_ENDFUNCTION = 398,
     TOK_ENDPROGRAM = 399,
     TOK_ENDUNIT = 400,
     TOK_CHARACTER = 401,
     TOK_CHAR_CONSTANT = 402,
     TOK_CHAR_CUT = 403,
     TOK_DATA = 404,
     TOK_CHAR_MESSAGE = 405,
     TOK_CSTREAL = 406,
     TOK_COMPLEX = 407,
     TOK_DOUBLECOMPLEX = 408,
     TOK_NAME = 409,
     TOK_CSTINT = 410
   };
#endif


#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 387 of yacc.c  */
#line 65 "fortran.y"

    char        na[LONG_M];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;


/* Line 387 of yacc.c  */
#line 310 "fortran.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE fortran_lval;

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int fortran_parse (void *YYPARSE_PARAM);
#else
int fortran_parse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int fortran_parse (void);
#else
int fortran_parse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 338 "fortran.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(N) (N)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   6268

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  169
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  206
/* YYNRULES -- Number of rules.  */
#define YYNRULES  597
/* YYNRULES -- Number of states.  */
#define YYNSTATES  1051

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   410

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     166,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   168,     2,     2,
     162,   163,    21,    19,     3,    20,     2,   167,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
     164,     5,   165,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    11,    14,    16,    19,
      21,    25,    28,    31,    33,    37,    41,    44,    47,    51,
      53,    54,    55,    57,    58,    61,    66,    69,    75,    78,
      80,    83,    85,    87,    88,    91,    95,    96,    99,   103,
     105,   109,   111,   113,   116,   121,   124,   127,   132,   135,
     137,   139,   141,   143,   145,   147,   149,   151,   153,   158,
     162,   166,   169,   172,   173,   175,   177,   179,   181,   183,
     185,   187,   189,   191,   193,   195,   197,   199,   201,   203,
     205,   207,   209,   211,   213,   215,   217,   219,   221,   223,
     225,   229,   233,   239,   241,   245,   249,   252,   257,   259,
     263,   264,   266,   269,   273,   275,   277,   280,   282,   286,
     291,   296,   305,   307,   311,   314,   318,   324,   328,   330,
     331,   334,   336,   341,   345,   348,   352,   356,   360,   364,
     365,   367,   370,   374,   380,   384,   386,   392,   398,   401,
     405,   408,   412,   414,   416,   420,   424,   427,   431,   437,
     439,   442,   444,   448,   451,   453,   457,   458,   460,   462,
     466,   470,   473,   475,   479,   482,   485,   491,   498,   499,
     502,   505,   509,   513,   514,   517,   522,   526,   530,   535,
     538,   540,   542,   544,   546,   548,   550,   552,   553,   556,
     558,   562,   563,   566,   570,   572,   576,   579,   583,   585,
     587,   589,   591,   593,   594,   598,   599,   601,   605,   607,
     611,   613,   615,   617,   620,   622,   627,   629,   631,   633,
     635,   637,   639,   641,   643,   645,   647,   648,   652,   654,
     658,   660,   662,   665,   668,   672,   674,   676,   678,   680,
     682,   686,   690,   694,   699,   704,   708,   713,   718,   722,
     727,   732,   737,   742,   747,   752,   757,   762,   767,   772,
     777,   782,   787,   791,   796,   800,   805,   810,   812,   816,
     818,   820,   822,   825,   828,   831,   833,   835,   838,   841,
     844,   847,   850,   853,   856,   859,   862,   866,   870,   873,
     876,   879,   882,   885,   888,   891,   894,   897,   900,   901,
     903,   906,   909,   912,   914,   916,   918,   920,   921,   923,
     926,   931,   937,   942,   947,   951,   953,   956,   958,   962,
     964,   966,   970,   976,   981,   985,   988,   991,   993,   995,
     997,   999,  1001,  1003,  1005,  1007,  1010,  1013,  1015,  1018,
    1020,  1022,  1023,  1025,  1031,  1032,  1034,  1036,  1038,  1039,
    1042,  1045,  1051,  1054,  1059,  1066,  1073,  1075,  1077,  1081,
    1085,  1087,  1091,  1095,  1097,  1099,  1101,  1103,  1105,  1107,
    1109,  1111,  1113,  1116,  1118,  1120,  1123,  1126,  1129,  1133,
    1136,  1142,  1148,  1151,  1154,  1157,  1160,  1162,  1167,  1169,
    1172,  1175,  1178,  1181,  1183,  1185,  1187,  1189,  1195,  1202,
    1203,  1207,  1208,  1213,  1214,  1219,  1224,  1226,  1228,  1230,
    1232,  1234,  1238,  1243,  1245,  1248,  1250,  1253,  1254,  1255,
    1258,  1262,  1264,  1269,  1271,  1273,  1278,  1281,  1287,  1291,
    1295,  1297,  1302,  1305,  1312,  1321,  1327,  1331,  1333,  1335,
    1337,  1339,  1341,  1344,  1351,  1352,  1354,  1357,  1361,  1362,
    1364,  1367,  1371,  1379,  1385,  1391,  1398,  1400,  1403,  1405,
    1408,  1414,  1419,  1420,  1422,  1425,  1429,  1436,  1441,  1444,
    1448,  1450,  1453,  1457,  1459,  1461,  1465,  1467,  1470,  1473,
    1477,  1479,  1481,  1483,  1485,  1487,  1489,  1491,  1493,  1495,
    1496,  1501,  1509,  1511,  1515,  1518,  1521,  1524,  1525,  1529,
    1530,  1532,  1535,  1538,  1540,  1542,  1546,  1548,  1551,  1553,
    1555,  1556,  1558,  1561,  1564,  1565,  1568,  1572,  1576,  1580,
    1582,  1586,  1588,  1590,  1594,  1596,  1598,  1600,  1604,  1610,
    1615,  1618,  1621,  1622,  1624,  1626,  1628,  1630,  1632,  1634,
    1636,  1638,  1640,  1644,  1646,  1648,  1652,  1656,  1660,  1664,
    1667,  1671,  1674,  1677,  1680,  1683,  1686,  1689,  1692,  1696,
    1698,  1700,  1702,  1704,  1708,  1711,  1716,  1719,  1722,  1728,
    1729,  1731,  1734,  1736,  1738,  1740,  1742,  1746,  1750,  1754,
    1758,  1762,  1766,  1772,  1778,  1784,  1790,  1798,  1807,  1810,
    1812,  1816,  1818,  1820,  1824,  1825,  1830,  1832
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     170,     0,    -1,    -1,   170,   171,    -1,   172,    -1,   173,
      -1,   140,   173,    -1,     1,    -1,   166,   175,    -1,    24,
      -1,   172,   166,   175,    -1,   172,    24,    -1,   172,   140,
      -1,   174,    -1,   173,    24,   166,    -1,   173,    24,   174,
      -1,   178,   175,    -1,   186,   175,    -1,    28,   181,   175,
      -1,   282,    -1,    -1,    -1,    70,    -1,    -1,    26,   183,
      -1,   176,    29,   180,   182,    -1,    30,   180,    -1,   176,
      31,   180,   182,   177,    -1,    92,   160,    -1,   161,    -1,
     179,   161,    -1,   160,    -1,   153,    -1,    -1,   162,   163,
      -1,   162,   184,   163,    -1,    -1,   162,   163,    -1,   162,
     184,   163,    -1,   185,    -1,   184,     3,   185,    -1,   160,
      -1,    21,    -1,   228,   196,    -1,   141,   187,   195,   336,
      -1,   143,   336,    -1,   114,   190,    -1,   198,   162,   221,
     163,    -1,   198,   221,    -1,   215,    -1,   203,    -1,   225,
      -1,   211,    -1,   213,    -1,   212,    -1,   276,    -1,   223,
      -1,   209,    -1,    66,   162,   249,   163,    -1,   108,   195,
     214,    -1,   110,   195,   189,    -1,    65,   191,    -1,   199,
     166,    -1,    -1,   244,    -1,    42,    -1,    34,    -1,    39,
      -1,    54,    -1,    77,    -1,    40,    -1,    43,    -1,    60,
      -1,    55,    -1,    56,    -1,    57,    -1,    58,    -1,    59,
      -1,    62,    -1,    63,    -1,    64,    -1,    67,    -1,    68,
      -1,    69,    -1,    72,    -1,    73,    -1,    74,    -1,    75,
      -1,   160,    -1,   188,    -1,   189,     3,   188,    -1,   162,
     194,   163,    -1,   190,     3,   162,   194,   163,    -1,   192,
      -1,   191,     3,   192,    -1,   162,   193,   163,    -1,   267,
     245,    -1,   193,     3,   267,   245,    -1,   249,    -1,   194,
       3,   249,    -1,    -1,   131,    -1,   226,   227,    -1,   197,
     180,   182,    -1,    31,    -1,    25,    -1,   155,   200,    -1,
     201,    -1,   200,   220,   201,    -1,   160,    22,   202,    22,
      -1,   206,    22,   202,    22,    -1,   162,   257,     3,   368,
     163,    22,   202,    22,    -1,   207,    -1,   207,     3,   202,
      -1,   204,   205,    -1,   204,   219,   205,    -1,   203,   220,
     219,   220,   205,    -1,   203,     3,   205,    -1,   116,    -1,
      -1,   160,   245,    -1,   160,    -1,   160,   162,   249,   163,
      -1,   206,     3,   206,    -1,   208,   268,    -1,   207,    19,
     207,    -1,   207,    20,   207,    -1,   207,    21,   207,    -1,
     207,   167,   207,    -1,    -1,   253,    -1,   111,   267,    -1,
     111,   219,   267,    -1,   209,   220,   219,   220,   267,    -1,
     209,     3,   267,    -1,   106,    -1,   210,   220,   160,   245,
     233,    -1,   211,     3,   160,   245,   233,    -1,    79,   166,
      -1,    79,   195,   214,    -1,    78,   166,    -1,    78,   195,
     214,    -1,   160,    -1,   133,    -1,   214,     3,   160,    -1,
     214,     3,   133,    -1,   216,   217,    -1,   216,   219,   217,
      -1,   215,   220,   219,   220,   217,    -1,   123,    -1,   124,
     123,    -1,   218,    -1,   217,     3,   218,    -1,   160,   245,
      -1,    18,    -1,    22,   160,    22,    -1,    -1,     3,    -1,
     222,    -1,   221,     3,   222,    -1,   160,     5,   249,    -1,
     128,   224,    -1,   160,    -1,   224,     3,   160,    -1,   118,
     119,    -1,   118,   130,    -1,   240,   160,   245,   233,   274,
      -1,   226,     3,   160,   245,   233,   274,    -1,    -1,   232,
     235,    -1,   231,   229,    -1,   232,    21,   161,    -1,   142,
     237,   163,    -1,    -1,    21,   161,    -1,    21,   162,   230,
     163,    -1,   162,   230,   163,    -1,   160,   238,   239,    -1,
     160,     5,   238,   239,    -1,   238,   239,    -1,   152,    -1,
     145,    -1,   146,    -1,   144,    -1,   158,    -1,   159,    -1,
     147,    -1,    -1,    21,   234,    -1,   249,    -1,   162,    21,
     163,    -1,    -1,    21,   236,    -1,   162,   237,   163,    -1,
     249,    -1,   162,    21,   163,    -1,   160,   238,    -1,   160,
       5,   238,    -1,   160,    -1,   161,    -1,   133,    -1,   249,
      -1,    21,    -1,    -1,     3,   160,   238,    -1,    -1,   131,
      -1,     3,   241,   131,    -1,   242,    -1,   241,     3,   242,
      -1,    25,    -1,   244,    -1,    80,    -1,   106,   245,    -1,
     108,    -1,   109,   162,   243,   163,    -1,   110,    -1,   113,
      -1,   114,    -1,   116,    -1,   117,    -1,   136,    -1,   134,
      -1,   135,    -1,    78,    -1,    79,    -1,    -1,   162,   246,
     163,    -1,   247,    -1,   246,     3,   247,    -1,   248,    -1,
       4,    -1,   249,     4,    -1,     4,   249,    -1,   249,     4,
     248,    -1,    21,    -1,   249,    -1,   252,    -1,   275,    -1,
     250,    -1,   162,   249,   163,    -1,    42,   251,   163,    -1,
      33,   251,   163,    -1,    34,   162,   251,   163,    -1,    39,
     162,   251,   163,    -1,    54,   251,   163,    -1,    77,   162,
     251,   163,    -1,    40,   162,   249,   163,    -1,    43,   249,
     163,    -1,   144,   162,   251,   163,    -1,    60,   162,   249,
     163,    -1,    55,   162,   249,   163,    -1,    56,   162,   249,
     163,    -1,    57,   162,   249,   163,    -1,    58,   162,   249,
     163,    -1,    59,   162,   249,   163,    -1,    62,   162,   249,
     163,    -1,    63,   162,   249,   163,    -1,    64,   162,   249,
     163,    -1,    67,   162,   249,   163,    -1,    68,   162,   249,
     163,    -1,    69,   162,   249,   163,    -1,    71,   249,   163,
      -1,    72,   162,   251,   163,    -1,    73,   251,   163,    -1,
      74,   162,   251,   163,    -1,    75,   162,   251,   163,    -1,
     249,    -1,   251,     3,   249,    -1,   257,    -1,   268,    -1,
     262,    -1,   249,   254,    -1,   253,   249,    -1,    11,   249,
      -1,    19,    -1,    20,    -1,    19,   249,    -1,    20,   249,
      -1,    21,   249,    -1,    23,   249,    -1,    13,   249,    -1,
       7,   249,    -1,    16,   249,    -1,   165,   249,    -1,   164,
     249,    -1,   165,     5,   249,    -1,   164,     5,   249,    -1,
      17,   249,    -1,    14,   249,    -1,    15,   249,    -1,    12,
     249,    -1,     6,   249,    -1,     8,   249,    -1,     9,   249,
      -1,    10,   249,    -1,    22,   255,    -1,     5,   256,    -1,
      -1,   249,    -1,     5,   249,    -1,    22,   249,    -1,     5,
     249,    -1,   249,    -1,   267,    -1,   261,    -1,   259,    -1,
      -1,   260,    -1,   260,   271,    -1,   261,   162,   263,   163,
      -1,   261,   162,   263,   163,   271,    -1,   267,   162,   263,
     163,    -1,   257,   168,   356,   257,    -1,   125,   366,   126,
      -1,   258,    -1,   258,   264,    -1,   265,    -1,   264,     3,
     265,    -1,   249,    -1,   266,    -1,   249,     4,   249,    -1,
     249,     4,   249,     4,   249,    -1,     4,   249,     4,   249,
      -1,     4,     4,   249,    -1,     4,   249,    -1,   249,     4,
      -1,     4,    -1,   160,    -1,   138,    -1,   139,    -1,    41,
      -1,   161,    -1,   157,    -1,   132,    -1,   268,   160,    -1,
     269,   270,    -1,   153,    -1,   269,   153,    -1,   156,    -1,
     154,    -1,    -1,   271,    -1,   162,   272,     4,   272,   163,
      -1,    -1,   249,    -1,   166,    -1,   249,    -1,    -1,     5,
     249,    -1,   122,   249,    -1,   162,   252,     3,   252,   163,
      -1,   277,   160,    -1,   277,   160,     3,   278,    -1,   277,
     160,     3,    27,     4,   166,    -1,   277,   160,     3,    27,
       4,   280,    -1,   137,    -1,   279,    -1,   278,     3,   279,
      -1,   160,   122,   160,    -1,   281,    -1,   280,     3,   281,
      -1,   160,   122,   160,    -1,   160,    -1,   283,    -1,   331,
      -1,   284,    -1,   300,    -1,   320,    -1,   310,    -1,   287,
      -1,   115,    -1,   338,   340,    -1,   369,    -1,   341,    -1,
     357,   352,    -1,   359,   350,    -1,   100,   352,    -1,   100,
     352,   366,    -1,   103,   353,    -1,    96,   162,   370,   373,
     163,    -1,   104,   162,   372,   373,   163,    -1,    76,   272,
      -1,    81,   273,    -1,    61,   273,    -1,   348,   273,    -1,
     339,    -1,   105,   162,   374,   163,    -1,   333,    -1,   334,
     336,    -1,   332,   336,    -1,   335,   336,    -1,    93,   336,
      -1,   319,    -1,   286,    -1,    90,    -1,   249,    -1,    35,
     162,   294,   163,   293,    -1,   291,   172,   288,   289,   290,
     297,    -1,    -1,   288,   292,   172,    -1,    -1,   289,   295,
     172,   288,    -1,    -1,   290,   296,   172,   288,    -1,    35,
     162,   294,   163,    -1,   293,    -1,   286,    -1,   287,    -1,
     285,    -1,   249,    -1,    36,   294,   163,    -1,    36,   294,
     163,   160,    -1,    37,    -1,    37,   160,    -1,    38,    -1,
      38,   160,    -1,    -1,    -1,   299,   282,    -1,   299,   282,
     172,    -1,   301,    -1,   302,   172,   307,   308,    -1,   303,
      -1,   304,    -1,   160,     4,    89,   179,    -1,    89,   179,
      -1,   160,     4,    89,   179,   305,    -1,    89,   179,   305,
      -1,   160,     4,    89,    -1,    89,    -1,   160,     4,    89,
     305,    -1,    89,   305,    -1,   220,   306,     5,   249,     3,
     249,    -1,   220,   306,     5,   249,     3,   249,     3,   249,
      -1,   220,    94,   162,   249,   163,    -1,   220,    95,   298,
      -1,   267,    -1,   299,    -1,   309,    -1,   330,    -1,    91,
      -1,    91,   160,    -1,   315,   172,   299,   311,   313,   318,
      -1,    -1,   312,    -1,   311,   312,    -1,   316,   172,   299,
      -1,    -1,   314,    -1,   313,   312,    -1,   317,   172,   299,
      -1,   160,     4,    88,   162,   249,   163,    82,    -1,    88,
     162,   249,   163,    82,    -1,    83,   162,   249,   163,    82,
      -1,    83,   162,   249,   163,    82,   160,    -1,    84,    -1,
      84,   160,    -1,    85,    -1,    85,   160,    -1,    88,   162,
     249,   163,   284,    -1,   323,   172,   321,   325,    -1,    -1,
     322,    -1,   321,   322,    -1,   324,   172,   299,    -1,   160,
       4,    45,   162,   249,   163,    -1,    45,   162,   249,   163,
      -1,    44,   326,    -1,    44,   326,   160,    -1,   107,    -1,
     107,   160,    -1,   162,   327,   163,    -1,   112,    -1,   328,
      -1,   327,     3,   328,    -1,   329,    -1,   329,     4,    -1,
       4,   329,    -1,   329,     4,   329,    -1,   249,    -1,   115,
      -1,    32,    -1,   148,    -1,   151,    -1,   150,    -1,   149,
      -1,   166,    -1,   160,    -1,    -1,   267,   337,   245,   245,
      -1,   338,   168,   356,   267,   337,   245,   245,    -1,   161,
      -1,   339,     3,   161,    -1,     5,   249,    -1,   122,   249,
      -1,   344,   342,    -1,    -1,   162,   343,   163,    -1,    -1,
     346,    -1,   345,   101,    -1,   345,   160,    -1,   120,    -1,
     347,    -1,   346,     3,   347,    -1,   249,    -1,    21,   161,
      -1,   127,    -1,   129,    -1,    -1,   363,    -1,   352,   349,
      -1,   358,   351,    -1,    -1,     3,   363,    -1,   162,   354,
     163,    -1,   162,   267,   163,    -1,   162,   161,   163,    -1,
     161,    -1,   162,   252,   163,    -1,   160,    -1,   355,    -1,
     354,     3,   355,    -1,   360,    -1,    21,    -1,    23,    -1,
     267,   249,   245,    -1,   267,   249,   168,   356,   338,    -1,
     267,   162,   266,   163,    -1,   267,    21,    -1,   267,    23,
      -1,    -1,    97,    -1,    98,    -1,   101,    -1,   361,    -1,
      21,    -1,   102,    -1,    99,    -1,    86,    -1,   361,    -1,
     162,   360,   163,    -1,   257,    -1,   268,    -1,   360,   362,
     360,    -1,   360,    21,   360,    -1,   360,    22,   360,    -1,
     360,    23,   360,    -1,   362,   360,    -1,   360,    18,   360,
      -1,    46,   249,    -1,    47,   249,    -1,    49,   249,    -1,
      48,   249,    -1,    53,   249,    -1,    52,   249,    -1,    50,
     249,    -1,   160,     5,   249,    -1,   250,    -1,    19,    -1,
      20,    -1,   364,    -1,   363,     3,   364,    -1,   257,   365,
      -1,   162,   363,   163,   365,    -1,   250,   365,    -1,   268,
     365,    -1,   162,   363,     3,   368,   163,    -1,    -1,   254,
      -1,   365,   254,    -1,   275,    -1,   250,    -1,   252,    -1,
     367,    -1,   252,     3,   249,    -1,   252,     3,   367,    -1,
     367,     3,   249,    -1,   367,     3,   367,    -1,   366,     3,
     249,    -1,   366,     3,   367,    -1,   162,   252,     3,   368,
     163,    -1,   162,   366,     3,   368,   163,    -1,   162,   367,
       3,   368,   163,    -1,   160,     5,   249,     3,   249,    -1,
     160,     5,   249,     3,   249,     3,   249,    -1,    87,   162,
     249,     3,   249,   163,     3,   249,    -1,    87,   161,    -1,
     371,    -1,   370,     3,   371,    -1,   257,    -1,   371,    -1,
     372,     3,   371,    -1,    -1,     3,   121,     5,   257,    -1,
     267,    -1,   374,     3,   267,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   313,   313,   314,   316,   317,   318,   319,   322,   323,
     324,   325,   326,   329,   330,   331,   334,   335,   336,   344,
     347,   350,   351,   354,   355,   358,   366,   375,   392,   411,
     412,   415,   417,   419,   420,   421,   423,   424,   425,   427,
     439,   451,   452,   454,   455,   456,   457,   458,   475,   492,
     493,   498,   499,   536,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   570,   571,   578,   579,   580,   581,   582,
     583,   584,   585,   586,   587,   588,   589,   590,   591,   592,
     593,   594,   595,   596,   597,   598,   599,   600,   601,   604,
     605,   608,   609,   612,   613,   615,   618,   619,   622,   623,
     625,   626,   629,   695,   710,   712,   716,   719,   720,   723,
     729,   734,   748,   749,   752,   753,   754,   755,   758,   760,
     761,   764,   765,   766,   769,   770,   771,   772,   773,   775,
     776,   779,   780,   781,   782,   785,   792,   802,   814,   815,
     818,   819,   822,   823,   824,   825,   828,   834,   842,   852,
     853,   856,   857,   860,   868,   874,   881,   882,   885,   886,
     889,   905,   908,   909,   912,   921,   923,   945,   970,   972,
     973,   974,   975,   977,   978,   979,   980,   983,   984,   985,
     987,   990,   991,   992,   993,   994,   995,   997,   998,  1001,
    1002,  1004,  1005,  1006,  1008,  1009,  1012,  1013,  1023,  1024,
    1025,  1028,  1029,  1031,  1032,  1034,  1035,  1036,  1038,  1039,
    1042,  1043,  1044,  1045,  1046,  1047,  1049,  1050,  1051,  1052,
    1053,  1056,  1057,  1058,  1061,  1062,  1064,  1065,  1073,  1079,
    1086,  1087,  1088,  1089,  1090,  1093,  1094,  1096,  1097,  1098,
    1099,  1103,  1104,  1105,  1106,  1107,  1108,  1109,  1110,  1111,
    1112,  1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,  1121,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  1130,  1131,  1133,
    1134,  1135,  1136,  1137,  1138,  1140,  1141,  1145,  1146,  1147,
    1148,  1149,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,
    1158,  1159,  1160,  1161,  1162,  1163,  1164,  1165,  1167,  1168,
    1169,  1170,  1173,  1174,  1177,  1178,  1179,  1183,  1194,  1195,
    1196,  1197,  1200,  1209,  1216,  1219,  1220,  1223,  1224,  1227,
    1228,  1231,  1232,  1233,  1234,  1235,  1236,  1237,  1239,  1285,
    1286,  1287,  1288,  1289,  1290,  1291,  1293,  1296,  1297,  1298,
    1299,  1301,  1302,  1305,  1307,  1308,  1311,  1312,  1314,  1315,
    1321,  1329,  1332,  1352,  1379,  1399,  1439,  1446,  1450,  1457,
    1467,  1468,  1476,  1486,  1498,  1499,  1504,  1505,  1506,  1507,
    1508,  1513,  1514,  1515,  1516,  1517,  1518,  1519,  1520,  1521,
    1522,  1523,  1524,  1525,  1526,  1527,  1528,  1529,  1530,  1567,
    1576,  1587,  1595,  1617,  1618,  1619,  1657,  1661,  1665,  1668,
    1669,  1672,  1673,  1676,  1677,  1682,  1686,  1687,  1688,  1692,
    1696,  1701,  1702,  1707,  1708,  1713,  1714,  1718,  1722,  1723,
    1724,  1729,  1734,  1739,  1740,  1745,  1746,  1747,  1748,  1753,
    1754,  1755,  1756,  1761,  1762,  1763,  1764,  1768,  1772,  1776,
    1777,  1782,  1783,  1787,  1790,  1791,  1792,  1796,  1799,  1800,
    1801,  1804,  1809,  1810,  1815,  1816,  1821,  1822,  1827,  1828,
    1832,  1836,  1839,  1840,  1841,  1844,  1849,  1850,  1855,  1856,
    1861,  1862,  1867,  1868,  1872,  1873,  1878,  1879,  1880,  1881,
    1885,  1889,  1893,  1897,  1905,  1912,  1919,  1926,  1927,  1930,
    1933,  1944,  1950,  1951,  1954,  1955,  1957,  1970,  1971,  1973,
    1974,  1977,  1978,  2000,  2003,  2004,  2007,  2015,  2018,  2019,
    2022,  2023,  2026,  2027,  2029,  2030,  2032,  2035,  2036,  2037,
    2038,  2039,  2042,  2043,  2046,  2047,  2048,  2049,  2050,  2051,
    2052,  2053,  2056,  2059,  2060,  2061,  2063,  2064,  2067,  2068,
    2069,  2072,  2073,  2076,  2077,  2078,  2079,  2080,  2081,  2082,
    2083,  2084,  2085,  2086,  2087,  2088,  2089,  2090,  2091,  2092,
    2094,  2095,  2097,  2098,  2104,  2105,  2106,  2107,  2108,  2110,
    2111,  2112,  2115,  2116,  2117,  2118,  2119,  2120,  2121,  2122,
    2123,  2124,  2127,  2128,  2129,  2131,  2132,  2134,  2135,  2138,
    2139,  2142,  2145,  2146,  2148,  2149,  2152,  2153
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "','", "':'", "'='", "TOK_NEQV",
  "TOK_EQV", "TOK_XOR", "TOK_OR", "TOK_AND", "TOK_NOT", "TOK_NE", "TOK_EQ",
  "TOK_GE", "TOK_LE", "TOK_GT", "TOK_LT", "TOK_DSLASH", "'+'", "'-'",
  "'*'", "TOK_SLASH", "TOK_DASTER", "TOK_SEMICOLON", "TOK_PARAMETER",
  "TOK_RESULT", "TOK_ONLY", "TOK_INCLUDE", "TOK_SUBROUTINE", "TOK_PROGRAM",
  "TOK_FUNCTION", "TOK_FORMAT", "TOK_MAX", "TOK_TANH", "TOK_WHERE",
  "TOK_ELSEWHEREPAR", "TOK_ELSEWHERE", "TOK_ENDWHERE", "TOK_MAXVAL",
  "TOK_TRIM", "TOK_NULL_PTR", "TOK_SUM", "TOK_SQRT", "TOK_CASE",
  "TOK_SELECTCASE", "TOK_FILE", "TOK_UNIT", "TOK_FMT", "TOK_NML",
  "TOK_END", "TOK_EOR", "TOK_ERR", "TOK_EXIST", "TOK_MIN", "TOK_FLOAT",
  "TOK_EXP", "TOK_COS", "TOK_COSH", "TOK_ACOS", "TOK_NINT", "TOK_CYCLE",
  "TOK_SIN", "TOK_SINH", "TOK_ASIN", "TOK_EQUIVALENCE", "TOK_BACKSPACE",
  "TOK_LOG", "TOK_TAN", "TOK_ATAN", "TOK_RECURSIVE", "TOK_ABS", "TOK_MOD",
  "TOK_SIGN", "TOK_MINLOC", "TOK_MAXLOC", "TOK_EXIT", "TOK_MINVAL",
  "TOK_PUBLIC", "TOK_PRIVATE", "TOK_ALLOCATABLE", "TOK_RETURN", "TOK_THEN",
  "TOK_ELSEIF", "TOK_ELSE", "TOK_ENDIF", "TOK_PRINT", "TOK_PLAINGOTO",
  "TOK_LOGICALIF", "TOK_PLAINDO", "TOK_CONTAINS", "TOK_ENDDO",
  "TOK_MODULE", "TOK_ENDMODULE", "TOK_WHILE", "TOK_CONCURRENT",
  "TOK_ALLOCATE", "TOK_OPEN", "TOK_CLOSE", "TOK_INQUIRE", "TOK_WRITE",
  "TOK_FLUSH", "TOK_READ", "TOK_REWIND", "TOK_DEALLOCATE", "TOK_NULLIFY",
  "TOK_DIMENSION", "TOK_ENDSELECT", "TOK_EXTERNAL", "TOK_INTENT",
  "TOK_INTRINSIC", "TOK_NAMELIST", "TOK_DEFAULT", "TOK_OPTIONAL",
  "TOK_POINTER", "TOK_CONTINUE", "TOK_SAVE", "TOK_TARGET", "TOK_IMPLICIT",
  "TOK_NONE", "TOK_CALL", "TOK_STAT", "TOK_POINT_TO", "TOK_COMMON",
  "TOK_GLOBAL", "TOK_LEFTAB", "TOK_RIGHTAB", "TOK_PAUSE", "TOK_PROCEDURE",
  "TOK_STOP", "TOK_REAL8", "TOK_FOURDOTS", "TOK_HEXA", "TOK_ASSIGNTYPE",
  "TOK_OUT", "TOK_INOUT", "TOK_IN", "TOK_USE", "TOK_TRUE", "TOK_FALSE",
  "TOK_LABEL", "TOK_TYPE", "TOK_TYPEPAR", "TOK_ENDTYPE", "TOK_REAL",
  "TOK_INTEGER", "TOK_LOGICAL", "TOK_DOUBLEPRECISION", "TOK_ENDSUBROUTINE",
  "TOK_ENDFUNCTION", "TOK_ENDPROGRAM", "TOK_ENDUNIT", "TOK_CHARACTER",
  "TOK_CHAR_CONSTANT", "TOK_CHAR_CUT", "TOK_DATA", "TOK_CHAR_MESSAGE",
  "TOK_CSTREAL", "TOK_COMPLEX", "TOK_DOUBLECOMPLEX", "TOK_NAME",
  "TOK_CSTINT", "'('", "')'", "'<'", "'>'", "'\\n'", "'/'", "'%'",
  "$accept", "input", "line", "line-break", "suite_line_list",
  "suite_line", "fin_line", "opt_recursive", "opt_result", "entry",
  "label", "name_routine", "filename", "arglist", "arglist_after_result",
  "args", "arg", "spec", "opt_spec", "name_intrinsic",
  "use_intrinsic_list", "list_couple", "list_expr_equi", "expr_equi",
  "list_expr_equi1", "list_expr", "opt_sep", "after_type",
  "before_function", "before_parameter", "data_stmt", "data_stmt_set_list",
  "data_stmt_set", "data_stmt_value_list", "save", "before_save",
  "varsave", "datanamelist", "expr_data", "opt_signe", "namelist",
  "before_dimension", "dimension", "private", "public", "use_name_list",
  "common", "before_common", "var_common_list", "var_common", "comblock",
  "opt_comma", "paramlist", "paramitem", "module_proc_stmt",
  "proc_name_list", "implicit", "dcl", "nodimsgiven", "type", "c_selector",
  "c_attribute", "before_character", "typespec", "lengspec",
  "proper_lengspec", "selector", "proper_selector", "attribute", "clause",
  "opt_clause", "options", "attr_spec_list", "attr_spec", "intent_spec",
  "access_spec", "dims", "dimlist", "dim", "ubound", "expr",
  "predefinedfunction", "minmaxlist", "uexpr", "signe", "operation",
  "after_slash", "after_equal", "lhs", "beforefunctionuse",
  "array_ele_substring_func_ref", "begin_array", "structure_component",
  "vec", "funarglist", "funargs", "funarg", "triplet", "ident",
  "simple_const", "string_constant", "opt_substring", "substring",
  "optexpr", "opt_expr", "initial_value", "complex_const", "use_stat",
  "word_use", "rename_list", "rename_name", "only_list", "only_name",
  "execution-part-construct", "executable-construct", "action-stmt",
  "assignment-stmt", "where-stmt", "where-construct",
  "opt-where-body-construct", "opt-masked-elsewhere-construct",
  "opt-elsewhere-construct", "where-construct-stmt",
  "where-body-construct", "where-assignment-stmt", "mask-expr",
  "masked-elsewhere-stmt", "elsewhere-stmt", "end-where-stmt",
  "forall-header", "block", "do-construct", "block-do-construct",
  "do-stmt", "label-do-stmt", "nonlabel-do-stmt", "loop-control",
  "do-variable", "do-block", "end-do", "end-do-stmt", "if-construct",
  "opt-else-if-stmt-block", "else-if-stmt-block", "opt-else-stmt-block",
  "else-stmt-block", "if-then-stmt", "else-if-stmt", "else-stmt",
  "end-if-stmt", "if-stmt", "case-construct", "opt_case-stmt-block",
  "case-stmt-block", "select-case-stmt", "case-stmt", "end-select-stmt",
  "case-selector", "case-value-range-list", "case-value-range",
  "case-value", "continue-stmt", "format-stmt", "word_endsubroutine",
  "word_endunit", "word_endprogram", "word_endfunction", "opt_name",
  "before_dims", "ident_dims", "int_list", "after_ident_dims", "call",
  "opt_call", "opt_callarglist", "keywordcall", "before_call",
  "callarglist", "callarg", "stop", "option_inlist", "option_read",
  "opt_inlist", "ioctl", "after_rewind", "ctllist", "ioclause",
  "declare_after_percent", "iofctl", "infmt", "read", "fexpr",
  "unpar_fexpr", "addop", "inlist", "inelt", "opt_operation", "outlist",
  "other", "dospec", "goto", "allocation_list", "allocate_object",
  "allocate_object_list", "opt_stat_spec", "pointer_name_list", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    44,    58,    61,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,    43,
      45,    42,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     379,   380,   381,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,    40,    41,    60,    62,    10,    47,    37
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   169,   170,   170,   171,   171,   171,   171,   172,   172,
     172,   172,   172,   173,   173,   173,   174,   174,   174,   174,
     175,   176,   176,   177,   177,   178,   178,   178,   178,   179,
     179,   180,   181,   182,   182,   182,   183,   183,   183,   184,
     184,   185,   185,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   187,   187,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   189,
     189,   190,   190,   191,   191,   192,   193,   193,   194,   194,
     195,   195,   196,   196,   197,   198,   199,   200,   200,   201,
     201,   201,   202,   202,   203,   203,   203,   203,   204,   205,
     205,   206,   206,   206,   207,   207,   207,   207,   207,   208,
     208,   209,   209,   209,   209,   210,   211,   211,   212,   212,
     213,   213,   214,   214,   214,   214,   215,   215,   215,   216,
     216,   217,   217,   218,   219,   219,   220,   220,   221,   221,
     222,   223,   224,   224,   225,   225,   226,   226,   227,   228,
     228,   228,   228,   229,   229,   229,   229,   230,   230,   230,
     231,   232,   232,   232,   232,   232,   232,   233,   233,   234,
     234,   235,   235,   235,   236,   236,   237,   237,   237,   237,
     237,   238,   238,   239,   239,   240,   240,   240,   241,   241,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   243,   243,   243,   244,   244,   245,   245,   246,   246,
     247,   247,   247,   247,   247,   248,   248,   249,   249,   249,
     249,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   251,   251,   252,
     252,   252,   252,   252,   252,   253,   253,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   255,   255,
     255,   255,   256,   256,   257,   257,   257,   258,   259,   259,
     259,   259,   260,   261,   262,   263,   263,   264,   264,   265,
     265,   266,   266,   266,   266,   266,   266,   266,   267,   268,
     268,   268,   268,   268,   268,   268,   268,   269,   269,   269,
     269,   270,   270,   271,   272,   272,   273,   273,   274,   274,
     274,   275,   276,   276,   276,   276,   277,   278,   278,   279,
     280,   280,   281,   281,   282,   282,   283,   283,   283,   283,
     283,   284,   284,   284,   284,   284,   284,   284,   284,   284,
     284,   284,   284,   284,   284,   284,   284,   284,   284,   284,
     284,   284,   284,   284,   284,   284,   285,   286,   287,   288,
     288,   289,   289,   290,   290,   291,   292,   292,   292,   293,
     294,   295,   295,   296,   296,   297,   297,   298,   299,   299,
     299,   300,   301,   302,   302,   303,   303,   303,   303,   304,
     304,   304,   304,   305,   305,   305,   305,   306,   307,   308,
     308,   309,   309,   310,   311,   311,   311,   312,   313,   313,
     313,   314,   315,   315,   316,   316,   317,   317,   318,   318,
     319,   320,   321,   321,   321,   322,   323,   323,   324,   324,
     325,   325,   326,   326,   327,   327,   328,   328,   328,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   336,   337,
     338,   338,   339,   339,   340,   340,   341,   342,   342,   343,
     343,   344,   344,   345,   346,   346,   347,   347,   348,   348,
     349,   349,   350,   350,   351,   351,   352,   353,   353,   353,
     353,   353,   354,   354,   355,   355,   355,   355,   355,   355,
     355,   355,   356,   357,   357,   357,   358,   358,   359,   359,
     359,   360,   360,   361,   361,   361,   361,   361,   361,   361,
     361,   361,   361,   361,   361,   361,   361,   361,   361,   361,
     362,   362,   363,   363,   364,   364,   364,   364,   364,   365,
     365,   365,   366,   366,   366,   366,   366,   366,   366,   366,
     366,   366,   367,   367,   367,   368,   368,   369,   369,   370,
     370,   371,   372,   372,   373,   373,   374,   374
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     2,     1,     2,     1,
       3,     2,     2,     1,     3,     3,     2,     2,     3,     1,
       0,     0,     1,     0,     2,     4,     2,     5,     2,     1,
       2,     1,     1,     0,     2,     3,     0,     2,     3,     1,
       3,     1,     1,     2,     4,     2,     2,     4,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     3,
       3,     2,     2,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     5,     1,     3,     3,     2,     4,     1,     3,
       0,     1,     2,     3,     1,     1,     2,     1,     3,     4,
       4,     8,     1,     3,     2,     3,     5,     3,     1,     0,
       2,     1,     4,     3,     2,     3,     3,     3,     3,     0,
       1,     2,     3,     5,     3,     1,     5,     5,     2,     3,
       2,     3,     1,     1,     3,     3,     2,     3,     5,     1,
       2,     1,     3,     2,     1,     3,     0,     1,     1,     3,
       3,     2,     1,     3,     2,     2,     5,     6,     0,     2,
       2,     3,     3,     0,     2,     4,     3,     3,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     0,     2,     1,
       3,     0,     2,     3,     1,     3,     2,     3,     1,     1,
       1,     1,     1,     0,     3,     0,     1,     3,     1,     3,
       1,     1,     1,     2,     1,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     1,     3,
       1,     1,     2,     2,     3,     1,     1,     1,     1,     1,
       3,     3,     3,     4,     4,     3,     4,     4,     3,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     3,     4,     3,     4,     4,     1,     3,     1,
       1,     1,     2,     2,     2,     1,     1,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     3,     3,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     0,     1,
       2,     2,     2,     1,     1,     1,     1,     0,     1,     2,
       4,     5,     4,     4,     3,     1,     2,     1,     3,     1,
       1,     3,     5,     4,     3,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     0,     1,     5,     0,     1,     1,     1,     0,     2,
       2,     5,     2,     4,     6,     6,     1,     1,     3,     3,
       1,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     2,     2,     2,     3,     2,
       5,     5,     2,     2,     2,     2,     1,     4,     1,     2,
       2,     2,     2,     1,     1,     1,     1,     5,     6,     0,
       3,     0,     4,     0,     4,     4,     1,     1,     1,     1,
       1,     3,     4,     1,     2,     1,     2,     0,     0,     2,
       3,     1,     4,     1,     1,     4,     2,     5,     3,     3,
       1,     4,     2,     6,     8,     5,     3,     1,     1,     1,
       1,     1,     2,     6,     0,     1,     2,     3,     0,     1,
       2,     3,     7,     5,     5,     6,     1,     2,     1,     2,
       5,     4,     0,     1,     2,     3,     6,     4,     2,     3,
       1,     2,     3,     1,     1,     3,     1,     2,     2,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       4,     7,     1,     3,     2,     2,     2,     0,     3,     0,
       1,     2,     2,     1,     1,     3,     1,     2,     1,     1,
       0,     1,     2,     2,     0,     2,     3,     3,     3,     1,
       3,     1,     1,     3,     1,     1,     1,     3,     5,     4,
       2,     2,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     1,     1,     3,     3,     3,     3,     2,
       3,     2,     2,     2,     2,     2,     2,     2,     3,     1,
       1,     1,     1,     3,     2,     4,     2,     2,     5,     0,
       1,     2,     1,     1,     1,     1,     3,     3,     3,     3,
       3,     3,     5,     5,     5,     5,     7,     8,     2,     1,
       3,     1,     1,     3,     0,     4,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     7,     9,   105,     0,     0,   482,     0,
       0,     0,     0,     0,    22,   344,   100,   100,     0,   540,
       0,     0,   156,   395,     0,     0,     0,   533,   534,   539,
       0,   535,   538,     0,     0,     0,   135,   100,   100,     0,
       0,   371,   118,     0,   503,   149,     0,   508,     0,   509,
     356,    21,    63,     0,     0,   183,   181,   182,   186,   483,
     486,   485,   484,   180,     0,   184,   185,   328,   492,    20,
       3,     4,     5,    13,     0,    20,    20,     0,     0,    50,
     119,    57,   156,    52,    54,    53,    49,     0,    56,    51,
     205,   173,   191,   489,    55,     0,    19,   364,   366,   394,
     370,     0,   367,   421,     0,   423,   424,   369,     0,   393,
     368,     0,   365,     0,   388,     0,     0,     0,   386,   374,
     497,     0,     0,     0,     0,   373,    32,    20,    31,    26,
       0,     0,     0,   275,   276,     0,     0,     0,     0,   331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   334,   329,   330,     0,   337,   340,   339,   333,
     328,   332,     0,   346,   347,   239,   237,     0,   269,   306,
     308,   305,   271,   304,   270,   341,   384,   238,     0,    61,
      93,     0,   345,   382,   101,   140,     0,   138,     0,   383,
     588,     0,     0,   157,    29,   156,     0,   432,    28,   488,
     487,   392,     0,     0,   377,   521,   519,     0,   379,     0,
       0,     0,     0,   154,     0,     0,   131,     0,    46,   164,
     165,   150,   162,   161,     6,   224,   225,   100,    64,   200,
     198,   199,     0,    45,   121,     0,   156,   107,     0,     0,
       8,    11,    12,    20,    21,     0,     0,    16,    17,     0,
       0,    48,   158,    62,   119,     0,   226,   114,   119,   157,
       0,     0,     0,     0,   226,   146,   151,     0,     0,   104,
     206,    43,     0,   168,     0,     0,     0,   170,     0,     0,
     169,   226,   352,   399,   418,   418,   462,   390,   389,   391,
       0,     0,   532,   372,     0,   499,   496,   501,   502,   385,
     375,   560,   561,   537,     0,     0,     0,     0,     0,     0,
       0,   328,     0,   559,   543,   544,   376,   510,   514,     0,
     536,     0,    18,   410,     0,     0,   274,   267,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   573,   574,   572,     0,   575,
       0,     0,   237,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   298,     0,
       0,     0,   272,   273,   532,   344,   309,   307,   307,   335,
     338,   336,   342,     0,   226,     0,     0,   143,   142,   141,
     139,     0,     0,    30,   428,     0,   417,   437,     0,   591,
     594,   589,   525,   526,     0,   304,     0,   522,   524,   541,
     378,   332,   237,   304,   592,   594,   596,     0,    59,    66,
      67,    70,    65,    71,    68,    73,    74,    75,    76,    77,
      72,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    69,    88,    89,    60,     0,   132,     0,    98,     0,
       0,     0,     0,   202,   196,   201,   172,   129,     0,     0,
       0,     0,   129,     0,     0,   156,    10,    14,    15,    33,
      33,     0,     0,     0,   117,   156,     0,   120,   115,   134,
     156,   226,   226,   156,   153,     0,   147,   210,   212,   226,
     214,     0,   216,   217,   218,   219,   220,     0,   208,   211,
      33,     0,   102,   226,   174,     0,   328,     0,   203,   332,
       0,   192,   194,     0,   226,     0,   401,   438,     0,   444,
       0,     0,   463,     0,   494,   495,     0,   493,     0,   506,
       0,   500,   504,   551,   552,   554,   553,   557,   556,   555,
       0,   524,     0,   569,   569,   569,   512,   511,   562,     0,
     513,     0,     0,     0,     0,     0,   549,   405,   467,     0,
     242,     0,     0,     0,   241,   248,   245,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   262,
       0,   264,     0,     0,     0,   237,     0,     0,     0,     0,
     314,     0,     0,   240,     0,     0,   303,   297,   292,   282,
     293,   294,   295,   291,   281,   289,   290,   283,   288,   277,
     278,   279,     0,     0,   299,   296,   280,     0,   285,     0,
     284,     0,     0,   315,     0,     0,     0,    95,    96,    94,
      58,     0,     0,     0,     0,   436,     0,     0,     0,     0,
     530,   531,   307,   226,     0,   516,   518,   520,   517,     0,
       0,     0,   387,     0,   155,     0,    91,     0,   163,    44,
     197,     0,   112,     0,   130,     0,     0,   108,   121,   123,
       0,     0,     0,   156,   431,     0,    25,    23,   160,    47,
     159,   119,   231,   235,     0,   228,   230,   236,     0,   187,
     187,     0,   152,   213,     0,     0,   207,   103,   226,   187,
       0,     0,   203,   176,     0,   179,     0,   193,   490,     0,
       0,   353,   357,   396,   409,   407,   408,   403,     0,   406,
     419,   441,   481,   422,   439,   440,     0,   448,   445,     0,
     473,     0,   468,   470,   464,   461,   418,   489,   507,   498,
       0,   558,   542,     0,   570,   566,   564,   567,     0,   515,
     550,   546,   547,   548,   545,   397,   268,   243,   244,   247,
     251,   252,   253,   254,   255,   250,   256,   257,   258,   259,
     260,   261,   263,   265,   266,   246,     0,     0,     0,   576,
     577,   580,   581,   578,   579,   249,   237,   302,   300,   301,
     287,   286,   313,   344,   327,   319,   316,   317,   320,   310,
     312,   226,   145,   144,     0,     0,   453,     0,   460,     0,
       0,     0,   590,   380,     0,     0,   532,   527,   523,   593,
     381,   597,    90,    99,     0,   109,   129,   129,   129,   129,
     129,   124,   122,     0,     0,   110,     0,     0,   427,    42,
      41,    34,     0,    39,    36,    27,   116,   233,     0,   227,
     232,   133,     0,   136,   137,   148,   222,   223,   221,     0,
     209,   187,   348,   175,   203,   177,     0,   195,     0,     0,
       0,     0,     0,     0,   400,   420,   442,     0,   456,   446,
       0,   449,     0,   418,     0,   480,     0,   474,   476,   469,
     471,   465,   226,   505,     0,   569,   571,   563,   328,     0,
       0,     0,   351,     0,     0,   325,   326,     0,   311,    97,
       0,     0,     0,   435,     0,     0,   529,     0,    92,   113,
     125,   126,   127,   128,     0,     0,   466,     0,     0,    35,
       0,    24,   229,   234,   236,     0,   188,   189,   215,   348,
       0,     0,   166,   178,   204,   363,   354,   355,   360,   359,
     358,     0,   413,   415,     0,   398,   399,     0,   457,   458,
     450,   443,   418,   447,   478,     0,   472,   477,   226,     0,
     565,   582,   583,   584,   343,   324,     0,   321,   318,     0,
       0,     0,   433,   595,   528,     0,   129,   452,    40,    37,
       0,     0,   167,   349,   350,     0,     0,   411,   414,   416,
     399,   402,     0,   459,   451,   475,   479,   491,   568,   323,
       0,   587,     0,     0,     0,     0,     0,    38,   190,   362,
     361,   412,   404,   454,   322,   434,   585,   111,   455,     0,
     586
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    70,    71,    72,    73,   250,    74,   865,    75,
     205,   129,   127,   696,   951,   862,   863,    76,   237,   463,
     464,   228,   189,   190,   403,   467,   196,   281,   282,    77,
      78,   246,   247,   681,    79,    80,   267,   248,   682,   683,
      81,    82,    83,    84,    85,   409,    86,    87,   275,   276,
     225,   206,   261,   262,    88,   233,    89,   283,   522,    90,
     287,   527,    91,    92,   873,   956,   290,   531,   242,   528,
     725,   284,   517,   518,   879,   519,   497,   704,   705,   706,
     337,   175,   338,   176,   177,   392,   635,   617,   178,   643,
     179,   180,   181,   182,   645,   816,   817,   818,   183,   184,
     185,   401,   396,   193,   186,   962,   187,    94,    95,   731,
     732,   967,   968,   740,    97,    98,   734,    99,   100,   536,
     737,   892,   101,   738,   739,   334,   893,   974,   975,   655,
     537,   102,   103,   104,   105,   106,   207,   418,   538,   743,
     744,   107,   747,   748,   900,   901,   108,   749,   902,   981,
     109,   110,   541,   542,   111,   543,   755,   752,   906,   907,
     908,   745,   112,   113,   114,   115,   116,   211,   291,   117,
     118,   303,   119,   306,   550,   120,   121,   551,   552,   122,
     566,   326,   570,   214,   218,   426,   427,   546,   123,   328,
     124,   428,   429,   331,   567,   568,   765,   368,   369,   854,
     125,   420,   421,   435,   658,   437
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -845
static const yytype_int16 yypact[] =
{
    -845,   967,  -845,  -845,  -845,  -845,   -22,    45,  -845,    94,
     186,  3621,   199,   238,  -845,  4658,    16,    36,  3621,  -845,
     369,   256,    56,  -845,   299,   280,   268,  -845,  -845,  -845,
     306,  -845,  -845,   409,   311,   350,  -845,   357,   357,   119,
     354,  -845,  -845,    53,  -845,  -845,   420,  -845,   386,  -845,
    -845,  5758,   363,   274,   280,  -845,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,  -845,   237,  -845,  -845,   549,  -845,  -845,
    -845,   122,   563,  -845,   440,  -845,  -845,   319,   433,   436,
     159,   495,   622,   624,  -845,  -845,   523,   329,  -845,  -845,
     398,   102,   163,  -845,  -845,   471,  -845,  -845,  -845,  -845,
    -845,   215,  -845,  -845,   215,  -845,  -845,  -845,   215,  -845,
    -845,   215,  -845,   280,  -845,   280,   280,    20,   630,  -845,
     472,    44,  3621,   306,  5477,  -845,  -845,  -845,  -845,  -845,
    4658,  4658,  4658,  -845,  -845,  4658,   473,   474,   477,  -845,
    4658,  4658,  4658,   478,   484,   486,   487,   488,   494,   496,
     497,   498,   499,   500,   501,  4658,   505,  4658,   507,   508,
     511,  4767,  -845,  -845,  -845,   512,  -845,  -845,  -845,  -845,
    -845,  -845,  4658,  -845,  2490,  -845,  -845,  4658,   470,  -845,
     513,   515,  -845,   516,   522,   270,  -845,  -845,   524,   654,
    -845,  4658,  2490,  -845,  -845,  -845,   277,  -845,   277,  -845,
    -845,  4658,  4658,  -845,  -845,   110,   326,  -845,  -845,  -845,
    -845,  -845,   524,  5347,  4767,  -845,  -845,  4876,  -845,   524,
     524,   277,  2573,  -845,   527,   524,  -845,  4658,   665,  -845,
    -845,  -845,  -845,   680,   563,  -845,  -845,   357,  -845,  -845,
    2967,  -845,   526,  -845,    88,   524,    32,  -845,   384,    82,
    -845,  -845,  -845,  -845,  5621,    45,    45,  -845,  -845,   687,
     533,   691,  -845,  -845,   535,   442,   534,  -845,   535,   524,
     442,   537,   539,   442,   534,   699,  -845,   545,   847,  -845,
    -845,  -845,    45,   714,   564,   434,  3874,  -845,  4985,   274,
    -845,   534,   727,   122,   122,   122,   258,  -845,  -845,  -845,
    4658,  4658,  -845,  -845,   571,  3983,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,  -845,  4658,  4658,  4658,  4658,  4658,  4658,
    4658,   726,  5347,  -845,   470,   522,  -845,  1249,   730,   555,
     561,  5523,  -845,  2490,   572,  1332,   875,  2490,    38,  4658,
    4658,  4658,    48,  1353,    61,  4658,  4658,  4658,  4658,  4658,
    4658,  4658,  4658,  4658,  4658,  4658,  4658,  1514,  4658,    68,
    4658,  4658,  4658,  4767,  2490,  1533,   753,  1552,   278,   731,
    4658,  1616,   733,  3294,  4658,  4658,  4658,  4658,  4658,  4658,
    4658,  4658,  4658,  4658,  4658,  4658,  4658,  4658,  3076,  4658,
    3403,  3512,  -845,   185,  -845,  4658,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,    69,   534,   199,  1635,  -845,  -845,   734,
     734,  1129,  1713,  -845,  -845,   576,  -845,  -845,   736,   470,
     739,  -845,  -845,  -845,  5523,  3765,    79,  -845,   555,  -845,
     740,   581,   582,   439,  -845,   743,  -845,   105,   734,  -845,
    -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,  -845,   744,   717,  -845,   106,  2490,   588,
     591,   280,  4092,  -845,  -845,  2490,  -845,   598,  4658,    31,
     237,   594,   598,   595,   602,   125,  -845,  -845,  -845,   609,
     609,  4658,   109,   533,  -845,   622,  2531,  -845,  -845,  -845,
     622,   534,   534,   622,  -845,   545,   699,  -845,  -845,   534,
    -845,   615,  -845,  -845,  -845,  -845,  -845,   123,  -845,  -845,
     609,   618,  -845,   534,  -845,  3874,  3185,   585,   750,    34,
    4201,  -845,  2490,   592,   534,    80,  4331,  5955,    78,  5879,
      13,    85,  -845,   215,  2490,  2490,   524,  -845,   619,  2490,
     616,   778,  -845,  2490,  2490,  2490,  2490,  2490,  2490,  2490,
    4658,   337,  1249,  2490,   706,  1732,  -845,   779,  -845,  1249,
    -845,  5523,  5523,  5523,  5523,  5523,   760,  4658,  -845,  4658,
    -845,   111,   115,  1751,  -845,  -845,  -845,  1796,  1815,  1834,
    1912,  1931,  1995,  2014,  2033,  2092,  2111,  2130,  2194,  -845,
     121,  -845,   130,   136,   149,   781,   785,   786,  4767,  4767,
    -845,  4767,   150,  -845,  4658,  4658,  2490,  -845,   542,   542,
     806,   806,   875,   887,   887,   887,   887,   887,   887,   164,
     164,   185,  4658,  4658,  2490,  -845,   185,  4658,   887,  4658,
     887,   524,   787,  2640,   627,   629,   524,  -845,  -845,  -845,
    -845,   295,  4658,  6031,  4658,  -845,  4658,    19,   631,   337,
    -845,  -845,  2640,    83,  5347,  -845,  -845,  -845,  -845,    19,
     632,   524,  -845,  2573,  -845,  4658,  -845,  4658,  -845,  -845,
    -845,   771,    42,   547,  -845,  2213,   636,  -845,   637,  -845,
     776,  4658,  4658,   135,  -845,   242,  -845,   774,  2490,  -845,
    -845,   535,  4658,  -845,   151,  -845,  -845,  1253,   524,   780,
     780,   545,  -845,  -845,   476,   847,  -845,  -845,   534,   780,
     641,  4092,   750,  -845,   645,  -845,   644,  -845,  -845,   804,
     688,   809,  -845,  2490,  -845,  -845,  -845,   773,   215,  -845,
     215,   653,  -845,  -845,  -845,  -845,   652,   536,  -845,   215,
    -845,  2749,   655,   664,  -845,  -845,   122,  -845,  -845,  -845,
    3983,  2490,  -845,   152,  -845,  2490,  2490,  2490,  1249,   779,
     570,   760,   760,   760,   593,  -845,  2490,  -845,  -845,  -845,
    -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,  -845,  -845,  -845,  5094,  5094,  5094,  2490,
    -845,  2490,  -845,  2490,  -845,  -845,   667,   887,   887,   185,
     887,   887,   470,  4658,  2858,  1415,   828,  -845,  -845,   513,
    -845,   534,  -845,  -845,  2291,   670,  -845,   671,  -845,  2310,
    1150,   829,  -845,  -845,  1233,   672,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,  2490,   153,  -845,   598,   598,   598,   598,
     598,   522,  -845,   832,   676,  -845,  2329,  2374,  -845,  -845,
    -845,  -845,   154,  -845,   678,  -845,  -845,  2490,  2531,  -845,
    4440,  -845,  5203,  -845,  -845,   699,  -845,  -845,  -845,   679,
    -845,   780,   231,  -845,   750,  -845,  4092,  -845,   291,   681,
     683,  4658,   586,   215,   122,   122,  -845,  4658,   685,  -845,
     454,  -845,   215,   122,  4658,  2490,   155,  -845,   842,  -845,
    -845,  5955,   534,  -845,  1831,  2490,  -845,  -845,   832,   684,
     686,   689,  -845,   690,  4658,  1435,  4658,  2640,  -845,  -845,
     845,  4658,  4658,  -845,  4658,   524,  -845,   524,  -845,  -845,
      66,    66,   693,   143,  4658,   833,  -845,   768,    60,  -845,
     259,  -845,  -845,  -845,  2490,  4549,  -845,  2490,  -845,   231,
    4658,  4658,  -845,  -845,  -845,   729,  -845,   851,  -845,  -845,
    -845,   695,   696,   702,   215,  -845,   122,  2393,  -845,   703,
    -845,  -845,   122,  5955,  -845,  2749,  -845,  4658,   534,   701,
    2490,  -845,  -845,  -845,  -845,  2490,  4658,  1455,  -845,  4658,
     704,  2412,  1171,   470,   697,  1192,   598,  -845,  -845,  -845,
     157,   705,  -845,  2490,  2490,   709,   713,   716,  -845,  -845,
     122,  4331,   784,  -845,  5955,  -845,  -845,  -845,  -845,  2490,
    4658,  2490,  4658,  6107,  4658,  4658,   855,  -845,  -845,  -845,
    -845,  -845,  4331,   719,  2490,  2490,  1213,  -845,  -845,  4658,
    2490
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -845,  -845,  -845,   -96,   830,   628,   -26,  -845,  -845,  -845,
     399,   156,  -845,   -94,  -845,   -67,   -63,  -845,  -845,   232,
    -845,  -845,  -845,   509,  -845,   234,    30,  -845,  -845,  -845,
    -845,  -845,   444,  -464,  -845,  -845,  -238,   438,  -242,  -845,
    -845,  -845,  -845,  -845,  -845,   -47,  -845,  -845,  -263,   417,
     -27,   -50,   668,   437,  -845,  -845,  -845,  -845,  -845,  -845,
    -845,   404,  -845,  -845,  -671,  -845,  -845,  -845,   642,  -231,
    -656,  -845,  -845,   217,  -845,   882,  -236,  -845,    67,    70,
     -11,   -78,    55,  -144,  -458,  -507,  -845,  -845,   -97,  -845,
    -845,  -845,  -845,  -845,   540,  -845,     9,   279,     1,   -70,
    -845,  -845,  -182,  -382,   365,   -20,  -128,  -845,  -845,  -845,
      52,  -845,   -73,    43,  -845,  -630,  -845,  -515,  -514,  -844,
    -845,  -845,  -845,  -845,  -566,  -718,  -845,  -845,  -845,  -845,
    -294,  -845,  -845,  -845,  -845,  -845,  -185,  -845,  -845,  -845,
    -845,  -845,  -845,  -663,  -845,  -845,  -845,  -845,  -845,  -845,
    -845,  -845,  -845,   405,  -845,  -845,  -845,  -845,  -845,   -40,
    -787,  -845,  -845,  -845,  -845,  -845,  -845,   -38,   190,    11,
    -845,  -845,  -845,  -845,  -845,  -845,  -845,  -845,   191,  -845,
    -845,  -845,  -845,   506,  -845,  -845,   288,  -384,  -845,  -845,
    -845,   -39,   834,  -250,  -126,  -699,  -522,  -140,  -287,  -465,
    -845,  -845,  -213,  -845,   519,  -845
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -542
static const yytype_int16 yytable[] =
{
     174,   539,    93,   402,   192,   293,   434,   174,   294,   474,
     641,   775,   295,   642,   506,   296,   243,   366,   690,   684,
     414,   735,   736,   828,   684,   300,   494,   324,   372,   265,
     498,   270,   271,   367,   686,   203,   273,  -171,   504,   874,
     226,   579,   766,   767,    96,   846,   323,   198,   882,   257,
     258,   579,    93,   268,   325,   534,   764,   764,   764,   203,
     277,   847,   848,   849,   579,  -171,   885,   221,   222,   917,
     366,   579,   646,   432,   430,   297,   607,   298,   299,   575,
    -430,   859,   664,   365,   899,   329,   367,   849,   373,   374,
     375,   376,   377,   378,    96,   379,   380,   381,   382,   383,
     384,   332,   385,   386,   387,   388,   389,   729,   671,   675,
     477,   174,   493,   203,   579,   419,   324,   984,   579,   333,
     335,   336,   419,   285,   579,   750,   715,   483,   203,   540,
     343,   126,  1021,   579,  -426,   323,   365,   223,   203,   579,
     831,   224,   301,   325,   357,   307,   251,   194,   479,  -429,
     364,   410,   579,   579,   868,   914,   675,   948,   985,  -425,
     948,   371,   847,   848,   849,  -171,   393,   194,   648,   741,
     484,   485,   229,   971,   438,   751,  1042,   223,   575,   170,
     406,   224,   195,   230,   288,   387,   388,   389,   302,   404,
     411,   412,   753,   742,  -171,   342,   480,   344,  -106,   394,
    1026,   580,   197,   364,   308,   128,   364,   417,   389,   850,
     959,   584,   359,  1000,   425,   917,   468,   204,   433,   605,
     860,   436,  -430,   606,   586,   324,   466,   486,   963,   475,
     564,   601,   647,   850,   324,   367,   960,   980,   495,     4,
     730,   680,   665,   500,   323,   496,   503,   390,   391,   563,
     478,   836,   325,   323,   716,    93,   130,   565,   916,   916,
     916,   325,   252,   859,   286,   709,   710,   471,   672,   676,
     499,   413,   699,   713,   777,   475,  -426,   532,   778,   170,
     859,   609,   251,   561,   792,   365,   204,   719,   253,   544,
     545,  -429,   576,   793,   549,   722,   413,    96,   728,   794,
     694,  -425,   540,   553,   554,   555,   556,   557,   558,   559,
     850,   575,   795,   805,   869,   915,   938,   949,   986,   266,
    1037,   800,   802,   425,   804,   289,   575,   324,   390,   391,
     583,   919,   920,   921,   587,   588,   589,   590,   591,   592,
     593,   594,   595,   596,   597,   598,   323,   223,   131,   390,
     391,   224,   371,   961,   325,   571,   311,   312,   572,   573,
     574,   188,   616,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   634,   636,   638,
     640,    69,   939,   199,   192,   659,   372,   481,   684,   684,
     684,   684,   684,   990,   581,   582,   697,   244,   252,   245,
     191,   278,   860,   828,   610,   861,   482,   239,   764,   575,
     407,   489,   490,   600,   663,   602,   603,   604,   202,   860,
     415,   416,  1009,   400,   253,   612,   717,   837,   822,   279,
     212,   923,   395,   679,   240,   241,   763,   408,   520,   264,
     209,   235,   236,   769,   832,   701,   210,   756,   875,   989,
     708,   965,   937,   711,  -156,   823,   839,   966,  -156,   208,
     223,   475,   911,   866,   224,   564,   775,   685,   213,   255,
     806,   256,   564,   219,   324,   324,   324,   324,   324,   259,
     698,   260,   881,   916,   563,   707,   170,   309,   194,   274,
     884,   563,   565,   323,   323,   323,   323,   323,   269,   565,
     762,   325,   325,   325,   325,   325,   735,   736,   858,   800,
     802,   804,   220,  -156,   475,   475,   227,  -156,   372,   371,
     575,   575,   575,   575,   575,   733,   203,   735,   736,   280,
     200,   201,   770,   771,   772,   773,   774,   746,    93,   979,
      93,  -156,  1036,   231,   812,  -156,   232,   757,   684,   761,
     376,   377,   378,   249,   379,   380,   381,   382,   383,   384,
     419,   385,   386,   387,   388,   389,   733,   324,   776,   215,
     216,   217,   419,   571,   311,   312,   572,   573,   574,  -541,
    -541,  -541,  -541,  -541,  -541,   929,   323,   254,   139,   311,
     312,   572,   573,   574,   325,   524,   525,   799,   801,   263,
     803,   398,   668,   364,   807,   940,   941,   942,   943,   983,
     876,   877,   878,   851,   572,   573,   574,   133,   134,   746,
     898,   808,   809,   972,   973,   203,   810,   272,   811,   310,
     327,   292,   815,   304,   305,   339,   340,   928,   394,   341,
     345,   824,   894,   829,   895,   830,   346,   821,   347,   348,
     349,   834,   806,   903,    93,   964,   350,   405,   351,   352,
     353,   354,   355,   356,   843,   425,   468,   358,   469,   360,
     361,   564,   841,   362,   370,   395,   988,   397,   398,   162,
     856,   857,   399,   470,   170,   163,   164,   465,  1024,   476,
     563,   867,   491,   259,   493,   266,   496,   501,   565,   502,
     166,   167,   505,   168,   169,   274,   390,   391,   171,   871,
     475,   373,   374,   375,   376,   377,   378,   521,   379,   380,
     381,   382,   383,   384,   523,   385,   386,   387,   388,   389,
     535,   560,   547,   569,   611,   577,   614,   651,   654,   674,
     905,   656,   657,   609,   666,   667,   669,   673,   723,   549,
     677,   678,  1027,   724,   688,   727,   608,   691,  -237,  -237,
    -237,  -237,  -237,  -237,   692,  -237,  -237,  -237,  -237,  -237,
    -237,   695,  -237,  -237,  -237,  -237,  -237,   714,   718,   759,
     758,   760,   768,   574,   796,   799,   801,   803,   797,   798,
     819,   813,   820,   845,   833,   840,   853,   976,   855,   478,
     864,   872,   192,   925,   883,   886,   982,   887,   888,   891,
     889,   372,   890,   896,   897,   909,   378,   564,   379,   380,
     381,   382,   383,   384,   910,   385,   386,   387,   388,   389,
     922,   927,   931,   932,   935,   936,   563,   944,  1003,   945,
     950,   969,   958,   730,   565,   978,   987,   991,   999,   992,
    1007,  1015,   993,   994,  1016,  1006,  1018,   707,  1017,   954,
     850,   957,  1019,  1023,  1028,   302,  1043,  1032,  1038,  1039,
     390,   391,   507,   965,   394,   475,  1041,  1047,  1020,  1048,
     333,   234,   488,  1010,   693,  1008,   977,   379,   380,   381,
     382,   383,   384,   905,   385,   386,   387,   388,   389,  -542,
    -542,  -542,  -542,  -542,  -542,   842,   385,   386,   387,   388,
     389,   844,    93,   995,   649,   997,   815,  -237,  -237,   689,
     333,  1001,   712,  1002,   687,   235,   236,   508,   492,   720,
     700,   533,   880,  1005,   238,   952,   998,   644,    93,  1012,
     953,   835,   970,  1040,   371,  1025,   754,   912,  1004,  1013,
    1014,   913,   838,   509,   670,   510,   511,   512,   330,     0,
     513,   514,     0,   515,   516,     0,     0,     2,     3,     0,
     390,   391,     0,     0,   905,     0,   905,     0,     0,     0,
       0,     0,     0,     0,    93,  1029,     0,     0,  1031,     0,
       0,     4,     5,     0,     0,     6,   -21,     7,   -21,     8,
       0,     0,     9,     0,     0,     0,     0,     0,     0,     0,
     733,     0,    10,     0,     0,     0,     0,     0,     0,  1044,
       0,   733,     0,  1045,  1046,    93,     0,     0,    11,     0,
       0,   733,    12,    13,    93,     0,     0,    14,  1050,   390,
     391,     0,     0,    15,     0,    16,    17,     0,    18,     0,
       0,   390,   391,    19,    20,    21,    22,    23,     0,    24,
      25,     0,     0,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,     0,    37,     0,    38,    39,     0,
       0,    40,    41,    42,     0,    43,     0,    44,     0,     0,
      45,    46,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,    65,    66,    67,    68,     0,
       0,     0,   652,    69,   373,   374,   375,   376,   377,   378,
       0,   379,   380,   381,   382,   383,   384,     0,   385,   386,
     387,   388,   389,   934,     0,   373,   374,   375,   376,   377,
     378,     0,   379,   380,   381,   382,   383,   384,     0,   385,
     386,   387,   388,   389,  1034,     0,   373,   374,   375,   376,
     377,   378,     0,   379,   380,   381,   382,   383,   384,     0,
     385,   386,   387,   388,   389,  1035,     0,   373,   374,   375,
     376,   377,   378,     0,   379,   380,   381,   382,   383,   384,
       0,   385,   386,   387,   388,   389,  1049,     0,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,   926,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,   870,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,     0,     0,     0,
       0,     0,   135,   136,     0,     0,     0,     0,   137,   138,
     139,   140,   141,   390,   391,     0,     0,     0,     0,     0,
       0,     0,     0,   142,   143,   144,   145,   146,   147,   148,
       0,   149,   150,   151,   390,   391,   152,   153,   154,     0,
     155,   156,   157,   158,   159,     0,   160,     0,     0,     0,
       0,     0,     0,     0,     0,   390,   391,   373,   374,   375,
     376,   377,   378,     0,   379,   380,   381,   382,   383,   384,
       0,   385,   386,   387,   388,   389,   390,   391,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,   390,   391,     0,
       0,   162,     0,     0,     0,     0,     0,   163,   164,     0,
       0,     0,     0,   165,     0,     0,   613,   390,   391,     0,
       0,     0,   166,   167,     0,   168,   169,     0,     0,   170,
     171,   562,     0,     0,     0,     0,     0,   390,   391,   926,
     373,   374,   375,   376,   377,   378,     0,   379,   380,   381,
     382,   383,   384,     0,   385,   386,   387,   388,   389,   996,
     373,   374,   375,   376,   377,   378,     0,   379,   380,   381,
     382,   383,   384,     0,   385,   386,   387,   388,   389,  1030,
     373,   374,   375,   376,   377,   378,     0,   379,   380,   381,
     382,   383,   384,     0,   385,   386,   387,   388,   389,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   578,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   585,   390,   391,   373,
     374,   375,   376,   377,   378,     0,   379,   380,   381,   382,
     383,   384,     0,   385,   386,   387,   388,   389,  -239,  -239,
    -239,  -239,  -239,  -239,     0,  -239,  -239,  -239,  -239,  -239,
    -239,     0,  -239,  -239,  -239,  -239,  -239,  -238,  -238,  -238,
    -238,  -238,  -238,     0,  -238,  -238,  -238,  -238,  -238,  -238,
       0,  -238,  -238,  -238,  -238,  -238,     0,     0,     0,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   390,
     391,   373,   374,   375,   376,   377,   378,     0,   379,   380,
     381,   382,   383,   384,     0,   385,   386,   387,   388,   389,
     373,   374,   375,   376,   377,   378,     0,   379,   380,   381,
     382,   383,   384,     0,   385,   386,   387,   388,   389,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   599,   390,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -239,  -239,  -239,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -238,  -238,  -238,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,   373,   374,   375,
     376,   377,   378,     0,   379,   380,   381,   382,   383,   384,
       0,   385,   386,   387,   388,   389,   373,   374,   375,   376,
     377,   378,     0,   379,   380,   381,   382,   383,   384,     0,
     385,   386,   387,   388,   389,     0,     0,     0,     0,   613,
     390,   391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   650,   390,
     391,   373,   374,   375,   376,   377,   378,     0,   379,   380,
     381,   382,   383,   384,     0,   385,   386,   387,   388,   389,
     373,   374,   375,   376,   377,   378,     0,   379,   380,   381,
     382,   383,   384,     0,   385,   386,   387,   388,   389,   373,
     374,   375,   376,   377,   378,     0,   379,   380,   381,   382,
     383,   384,     0,   385,   386,   387,   388,   389,     0,     0,
       0,     0,     0,     0,   135,   136,     0,     0,     0,     0,
     137,   138,   139,   140,   141,     0,   653,   390,   391,     0,
       0,     0,     0,     0,     0,   142,   143,   144,   145,   146,
     147,   148,   399,   149,   150,   151,   390,   391,   152,   153,
     154,     0,   155,   156,   157,   158,   159,     0,   160,     0,
       0,     0,     0,     0,   779,   390,   391,   373,   374,   375,
     376,   377,   378,     0,   379,   380,   381,   382,   383,   384,
       0,   385,   386,   387,   388,   389,   373,   374,   375,   376,
     377,   378,     0,   379,   380,   381,   382,   383,   384,     0,
     385,   386,   387,   388,   389,     0,     0,     0,     0,   780,
     390,   391,     0,   162,     0,     0,     0,     0,     0,   163,
     164,     0,     0,     0,     0,   165,     0,     0,   781,   390,
     391,     0,     0,     0,   166,   167,     0,   168,   169,     0,
       0,   918,   171,   562,     0,     0,     0,   782,   390,   391,
     373,   374,   375,   376,   377,   378,     0,   379,   380,   381,
     382,   383,   384,     0,   385,   386,   387,   388,   389,   373,
     374,   375,   376,   377,   378,     0,   379,   380,   381,   382,
     383,   384,     0,   385,   386,   387,   388,   389,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   783,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   784,   390,   391,   373,   374,   375,
     376,   377,   378,     0,   379,   380,   381,   382,   383,   384,
       0,   385,   386,   387,   388,   389,   373,   374,   375,   376,
     377,   378,     0,   379,   380,   381,   382,   383,   384,     0,
     385,   386,   387,   388,   389,   373,   374,   375,   376,   377,
     378,     0,   379,   380,   381,   382,   383,   384,     0,   385,
     386,   387,   388,   389,     0,     0,     0,     0,   785,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   786,   390,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   787,   390,   391,   373,
     374,   375,   376,   377,   378,     0,   379,   380,   381,   382,
     383,   384,     0,   385,   386,   387,   388,   389,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   788,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   789,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   790,   390,   391,   373,   374,   375,   376,
     377,   378,     0,   379,   380,   381,   382,   383,   384,     0,
     385,   386,   387,   388,   389,   373,   374,   375,   376,   377,
     378,     0,   379,   380,   381,   382,   383,   384,     0,   385,
     386,   387,   388,   389,   373,   374,   375,   376,   377,   378,
       0,   379,   380,   381,   382,   383,   384,     0,   385,   386,
     387,   388,   389,     0,     0,     0,     0,   791,   390,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   852,   390,   391,   373,
     374,   375,   376,   377,   378,     0,   379,   380,   381,   382,
     383,   384,     0,   385,   386,   387,   388,   389,   373,   374,
     375,   376,   377,   378,     0,   379,   380,   381,   382,   383,
     384,     0,   385,   386,   387,   388,   389,   373,   374,   375,
     376,   377,   378,     0,   379,   380,   381,   382,   383,   384,
       0,   385,   386,   387,   388,   389,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   930,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   933,   390,   391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   946,   390,   391,   373,   374,   375,   376,   377,
     378,     0,   379,   380,   381,   382,   383,   384,     0,   385,
     386,   387,   388,   389,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   702,     0,   947,   390,   391,
       0,     0,   132,     0,     0,     0,     0,     0,     0,     0,
     133,   134,   703,     0,     0,     0,  1022,   390,   391,     0,
       0,     0,     0,     0,   135,   136,     0,     0,     0,     0,
     137,   138,   139,   140,   141,  1033,   390,   391,     0,     0,
       0,     0,     0,     0,     0,   142,   143,   144,   145,   146,
     147,   148,     0,   149,   150,   151,     0,     0,   152,   153,
     154,     0,   155,   156,   157,   158,   159,   439,   160,     0,
       0,     0,   440,   441,     0,   442,   443,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   444,   445,   446,
     447,   448,   449,   450,     0,   451,   452,   453,     0,     0,
     454,   455,   456,     0,   814,   457,   458,   459,   460,     0,
     461,   132,     0,     0,   390,   391,   161,     0,     0,   133,
     134,     0,     0,   162,     0,     0,     0,     0,     0,   163,
     164,     0,     0,   135,   136,   165,     0,     0,     0,   137,
     138,   139,   140,   141,   166,   167,     0,   168,   169,     0,
       0,   170,   171,   172,   142,   143,   144,   145,   146,   147,
     148,     0,   149,   150,   151,     0,     0,   152,   153,   154,
       0,   155,   156,   157,   158,   159,     0,   160,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   904,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,   161,     0,     0,   133,   134,
       0,     0,   162,     0,     0,     0,     0,     0,   163,   164,
       0,     0,   135,   136,   165,     0,     0,     0,   137,   138,
     139,   140,   141,   166,   167,     0,   168,   169,     0,     0,
     170,   171,   172,   142,   143,   144,   145,   146,   147,   148,
       0,   149,   150,   151,     0,     0,   152,   153,   154,     0,
     155,   156,   157,   158,   159,     0,   160,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   924,     0,     0,     0,     0,     0,     0,   132,
       0,     0,     0,     0,   161,     0,     0,   133,   134,     0,
       0,   162,     0,     0,     0,     0,     0,   163,   164,     0,
       0,   135,   136,   165,     0,     0,     0,   137,   138,   139,
     140,   141,   166,   167,     0,   168,   169,     0,     0,   170,
     171,   172,   142,   143,   144,   145,   146,   147,   148,     0,
     149,   150,   151,     0,     0,   152,   153,   154,     0,   155,
     156,   157,   158,   159,     0,   160,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   472,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,   161,     0,     0,   133,   134,   473,     0,
     162,     0,     0,     0,     0,     0,   163,   164,     0,     0,
     135,   136,   165,     0,     0,     0,   137,   138,   139,   140,
     141,   166,   167,     0,   168,   169,     0,     0,   170,   171,
     172,   142,   143,   144,   145,   146,   147,   148,     0,   149,
     150,   151,     0,     0,   152,   153,   154,     0,   155,   156,
     157,   158,   159,     0,   160,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   632,     0,     0,     0,     0,     0,   132,     0,     0,
       0,     0,   161,     0,     0,   133,   134,     0,   633,   162,
       0,     0,     0,     0,     0,   163,   164,     0,     0,   135,
     136,   165,     0,     0,     0,   137,   138,   139,   140,   141,
     166,   167,     0,   168,   169,     0,     0,   170,   171,   172,
     142,   143,   144,   145,   146,   147,   148,     0,   149,   150,
     151,     0,     0,   152,   153,   154,     0,   155,   156,   157,
     158,   159,     0,   160,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     721,     0,     0,     0,     0,     0,   132,     0,     0,     0,
       0,   161,     0,     0,   133,   134,   473,     0,   162,     0,
       0,     0,     0,     0,   163,   164,     0,     0,   135,   136,
     165,     0,     0,     0,   137,   138,   139,   140,   141,   166,
     167,     0,   168,   169,     0,     0,   170,   171,   172,   142,
     143,   144,   145,   146,   147,   148,     0,   149,   150,   151,
       0,     0,   152,   153,   154,     0,   155,   156,   157,   158,
     159,     0,   160,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   615,
       0,     0,     0,     0,     0,   132,     0,     0,     0,     0,
     161,     0,     0,   133,   134,     0,     0,   162,     0,     0,
       0,     0,     0,   163,   164,     0,     0,   135,   136,   165,
       0,     0,     0,   137,   138,   139,   140,   141,   166,   167,
       0,   168,   169,     0,     0,   170,   171,   172,   142,   143,
     144,   145,   146,   147,   148,     0,   149,   150,   151,     0,
       0,   152,   153,   154,     0,   155,   156,   157,   158,   159,
       0,   160,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   637,     0,
       0,     0,     0,     0,   132,     0,     0,     0,     0,   161,
       0,     0,   133,   134,     0,     0,   162,     0,     0,     0,
       0,     0,   163,   164,     0,     0,   135,   136,   165,     0,
       0,     0,   137,   138,   139,   140,   141,   166,   167,     0,
     168,   169,     0,     0,   170,   171,   172,   142,   143,   144,
     145,   146,   147,   148,     0,   149,   150,   151,     0,     0,
     152,   153,   154,     0,   155,   156,   157,   158,   159,     0,
     160,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   639,     0,     0,
       0,     0,     0,   132,     0,     0,     0,     0,   161,     0,
       0,   133,   134,     0,     0,   162,     0,     0,     0,     0,
       0,   163,   164,     0,     0,   135,   136,   165,     0,     0,
       0,   137,   138,   139,   140,   141,   166,   167,     0,   168,
     169,     0,     0,   170,   171,   172,   142,   143,   144,   145,
     146,   147,   148,     0,   149,   150,   151,     0,     0,   152,
     153,   154,     0,   155,   156,   157,   158,   159,     0,   160,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,   161,     0,     0,
     133,   134,     0,     0,   162,     0,     0,     0,     0,     0,
     163,   164,     0,     0,   135,   136,   165,     0,     0,     0,
     137,   138,   139,   140,   141,   166,   167,     0,   168,   169,
       0,     0,   170,   171,   172,   142,   143,   144,   145,   146,
     147,   148,     0,   149,   150,   151,     0,     0,   152,   153,
     154,     0,   155,   156,   157,   158,   159,     0,   160,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   161,     0,     0,     0,
       0,     0,     0,   162,     0,     0,     0,     0,     0,   163,
     164,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,   166,   167,   132,   168,   169,     0,
       0,   170,   171,   172,   133,   134,   660,   173,   661,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   135,   136,
       0,     0,     0,     0,   137,   138,   139,   140,   141,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   142,
     143,   144,   145,   146,   147,   148,     0,   149,   150,   151,
       0,     0,   152,   153,   154,     0,   155,   156,   157,   158,
     159,     0,   160,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,     0,     0,     0,
     161,     0,     0,   133,   134,   473,     0,   162,     0,     0,
       0,     0,     0,   163,   164,     0,     0,   135,   136,   165,
       0,     0,     0,   137,   138,   139,   140,   141,   166,   167,
       0,   168,   169,     0,     0,   170,   171,   662,   142,   143,
     144,   145,   146,   147,   148,     0,   149,   150,   151,     0,
       0,   152,   153,   154,     0,   155,   156,   157,   158,   159,
       0,   160,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,     0,     0,     0,     0,   161,
       0,     0,   133,   134,   548,     0,   162,     0,     0,     0,
       0,     0,   163,   164,     0,     0,   135,   136,   165,     0,
       0,     0,   137,   138,   139,   140,   141,   166,   167,     0,
     168,   169,     0,     0,   526,   171,   172,   142,   143,   144,
     145,   146,   147,   148,     0,   149,   150,   151,     0,     0,
     152,   153,   154,     0,   155,   156,   157,   158,   159,     0,
     160,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   132,     0,     0,     0,     0,   161,     0,
       0,   133,   134,   473,     0,   162,     0,     0,     0,     0,
       0,   163,   164,     0,     0,   135,   136,   165,     0,     0,
       0,   137,   138,   139,   140,   141,   166,   167,     0,   168,
     169,     0,     0,   170,   171,   172,   142,   143,   144,   145,
     146,   147,   148,     0,   149,   150,   151,     0,     0,   152,
     153,   154,     0,   155,   156,   157,   158,   159,     0,   160,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,   161,     0,     0,
     133,   134,   726,     0,   162,     0,     0,     0,     0,     0,
     163,   164,     0,     0,   135,   136,   165,     0,     0,     0,
     137,   138,   139,   140,   141,   166,   167,     0,   168,   169,
       0,     0,   170,   171,   172,   142,   143,   144,   145,   146,
     147,   148,     0,   149,   150,   151,     0,     0,   152,   153,
     154,     0,   155,   156,   157,   158,   159,     0,   160,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   161,     0,     0,     0,
       0,     0,     0,   162,     0,     0,     0,     0,     0,   163,
     164,     0,   132,     0,     0,   165,     0,     0,     0,     0,
     133,   134,     0,     0,   166,   167,     0,   168,   169,     0,
       0,   170,   171,   172,   135,   136,     9,     0,     0,     0,
     137,   138,   139,   140,   141,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   142,   143,   144,   145,   146,
     147,   148,     0,   149,   150,   151,     0,     0,   152,   153,
     154,     0,   155,   156,   157,   158,   159,     0,   160,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,     0,     0,     0,   161,     0,     0,   133,
     134,   703,     0,   162,     0,     0,     0,     0,     0,   163,
     164,     0,     0,   135,   136,   165,     0,     0,     0,   137,
     138,   139,   140,   141,   166,   167,     0,   168,   169,     0,
       0,   170,   171,   172,   142,   143,   144,   145,   146,   147,
     148,     0,   149,   150,   151,     0,     0,   152,   153,   154,
       0,   155,   156,   157,   158,   159,     0,   160,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,   161,     0,     0,   133,   134,
    1011,     0,   162,     0,     0,     0,     0,     0,   163,   164,
       0,     0,   135,   136,   165,     0,     0,     0,   137,   138,
     139,   140,   141,   166,   167,     0,   168,   169,     0,     0,
     170,   171,   172,   142,   143,   144,   145,   146,   147,   148,
       0,   149,   150,   151,     0,     0,   152,   153,   154,     0,
     155,   156,   157,   158,   159,     0,   160,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   132,
       0,     0,     0,     0,   161,     0,     0,   133,   134,     0,
       0,   162,     0,     0,     0,     0,     0,   163,   164,     0,
       0,   135,   136,   165,     0,     0,     0,   137,   138,   139,
     140,   141,   166,   167,     0,   168,   169,     0,     0,   170,
     171,   172,   142,   143,   144,   145,   146,   147,   148,     0,
     149,   150,   151,     0,     0,   152,   153,   154,     0,   155,
     156,   157,   158,   159,     0,   160,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,   161,     0,     0,   133,   134,     0,     0,
     162,     0,     0,     0,     0,     0,   163,   164,     0,     0,
     135,   136,   165,     0,     0,     0,   137,   138,   139,   140,
     141,   166,   167,     0,   168,   169,     0,     0,   170,   171,
     172,   142,   143,   144,   145,   146,   147,   148,     0,   149,
     150,   151,     0,     0,   152,   153,   154,     0,   155,   156,
     157,   158,   159,     0,   160,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   132,     0,     0,
       0,     0,   161,     0,     0,   133,   134,     0,     0,   162,
       0,     0,     0,     0,     0,   163,   164,     0,     0,   135,
     136,   165,     0,     0,     0,   137,   138,   139,   140,   141,
     166,   167,     0,   168,   169,     0,     0,   170,   171,   363,
     142,   143,   144,   145,   146,   147,   148,     0,   149,   150,
     151,     0,     0,   152,   153,   154,     0,   155,   156,   157,
     158,   159,     0,   160,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   132,     0,     0,     0,
       0,   161,     0,     0,   133,   134,     0,     0,   162,     0,
       0,     0,     0,     0,   163,   164,     0,     0,   135,   136,
     165,     0,     0,     0,   137,   138,   139,   140,   141,   166,
     167,     0,   168,   169,     0,     0,   170,   431,   172,   142,
     143,   144,   145,   146,   147,   148,     0,   149,   150,   151,
       0,     0,   152,   153,   154,     0,   155,   156,   157,   158,
     159,     0,   160,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,     0,     0,     0,
     161,     0,     0,   133,   134,     0,     0,   162,     0,     0,
       0,     0,     0,   163,   164,     0,     0,   135,   136,   165,
       0,     0,     0,   137,   138,   139,   140,   141,   166,   167,
       0,   168,   169,     0,     0,   170,   529,   530,   142,   143,
     144,   145,   146,   147,   148,     0,   149,   150,   151,     0,
       0,   152,   153,   154,     0,   155,   156,   157,   158,   159,
       0,   160,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,     0,     0,     0,     0,   161,
       0,     0,   133,   134,     0,     0,   162,     0,     0,     0,
       0,     0,   163,   164,     0,     0,   135,   136,   165,     0,
       0,     0,   137,   138,   139,   140,   141,   166,   167,     0,
     168,   169,     0,     0,   918,   171,   363,   142,   143,   144,
     145,   146,   147,   148,     0,   149,   150,   151,     0,     0,
     152,   153,   154,     0,   155,   156,   157,   158,   159,     0,
     160,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   161,     0,
       0,     0,     0,     0,     0,   162,     0,     0,     0,     0,
       0,   163,   164,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,   166,   167,     0,   168,
     169,     0,     0,   170,   171,   955,   311,   312,   422,     0,
     423,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     135,   136,     0,     0,     0,     0,   137,   138,   139,   140,
     141,     0,     0,   314,   315,   316,   317,   318,     0,   319,
     320,   142,   143,   144,   145,   146,   147,   148,     0,   149,
     150,   151,     0,     0,   152,   153,   154,     0,   155,   156,
     157,   158,   159,     0,   160,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   162,
       0,     0,     0,     0,     0,   163,   164,     0,     0,     0,
       0,   165,     0,     0,     0,     0,   311,   312,   313,     0,
     166,   167,     0,   168,   169,     0,     0,   321,   171,   424,
     135,   136,     0,     0,     0,     0,   137,   138,   139,   140,
     141,     0,     0,   314,   315,   316,   317,   318,     0,   319,
     320,   142,   143,   144,   145,   146,   147,   148,     0,   149,
     150,   151,   311,   312,   152,   153,   154,     0,   155,   156,
     157,   158,   159,     0,   160,     0,   135,   136,     0,     0,
       0,     0,   137,   138,   139,   140,   141,     0,     0,   314,
     315,   316,   317,   318,     0,   319,   320,   142,   143,   144,
     145,   146,   147,   148,     0,   149,   150,   151,     0,     0,
     152,   153,   154,     0,   155,   156,   157,   158,   159,     0,
     160,     0,     0,     0,     0,     0,     0,     0,     0,   162,
       0,     0,     0,     0,     0,   163,   164,     0,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
     166,   167,     0,   168,   169,     0,     0,   321,   171,   322,
       0,     0,     0,     0,     0,     0,     5,     0,     0,     6,
       0,     7,     0,     8,     0,   162,     9,     0,     0,     0,
       0,   163,   164,     0,     0,     0,    10,   165,     0,     0,
       0,     0,     0,     0,     0,     0,   166,   167,     0,   168,
     169,     0,    11,   321,   171,   424,    12,    13,     0,     0,
       0,    14,     0,     0,     0,     0,     0,    15,     0,    16,
      17,     0,    18,     0,     0,     0,     0,    19,    20,    21,
      22,    23,     0,    24,    25,     0,     0,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,     0,    37,
       0,    38,    39,     0,     0,    40,    41,    42,     0,    43,
       0,    44,     0,     0,    45,    46,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     0,     0,    65,
      66,    67,    68,     5,     0,     0,     6,   487,     7,     0,
       8,     0,     0,     9,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    10,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    11,
       0,     0,     0,    12,    13,     0,     0,     0,    14,     0,
       0,     0,     0,     0,    15,     0,    16,    17,     0,    18,
       0,     0,     0,     0,    19,    20,    21,    22,    23,     0,
      24,    25,     0,     0,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,     0,    37,     0,    38,    39,
       0,     0,    40,    41,    42,     0,    43,     0,    44,     0,
       0,    45,    46,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,     8,     0,    64,     9,     0,    65,    66,    67,    68,
       0,     0,     0,     0,    10,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      11,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    15,     0,     0,     0,     0,
      18,     0,   746,     0,     0,    19,    20,    21,    22,    23,
       0,     0,    25,     0,     0,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,     0,     0,     8,     0,     0,
       9,     0,     0,     0,    41,     0,     0,     0,     0,    44,
      10,     0,     0,     0,     0,     0,    47,     0,    49,     0,
       0,     0,     0,     0,     0,     0,    11,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    59,    60,    61,
      62,    15,     0,     0,     0,     0,    18,     0,     0,    67,
      68,    19,    20,    21,    22,    23,     0,     0,    25,     0,
       0,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,     0,     0,     0,     0,     0,   825,     0,     0,     0,
      41,     0,     0,     0,     0,    44,     0,     0,     0,     0,
       0,     0,    47,     0,    49,     0,     0,     0,     0,     0,
       0,     0,    11,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    59,    60,    61,    62,    15,     0,     0,
       0,     0,    18,   826,     0,    67,    68,    19,    20,   827,
       0,    23,     0,     0,    25,     0,     0,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,     0,     0,     0,
       0,     0,   825,     0,     0,     0,    41,     0,     0,     0,
       0,    44,     0,     0,     0,     0,     0,     0,    47,     0,
      49,     0,     0,     0,     0,     0,     0,     0,    11,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      60,    61,    62,    15,     0,     0,     0,     0,    18,     0,
       0,   170,    68,    19,    20,   827,     0,    23,     0,     0,
      25,     0,     0,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    41,     0,     0,     0,     0,    44,     0,     0,
       0,     0,     0,     0,    47,     0,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,    61,    62,     0,
       0,     0,     0,     0,     0,     0,     0,   170,    68
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-845)))

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-542)))

static const yytype_int16 yycheck[] =
{
      11,   295,     1,   185,    15,   101,   219,    18,   104,   240,
     394,   577,   108,   395,   277,   111,    54,   161,   482,   477,
     205,   536,   536,   653,   482,     5,   264,   124,   172,    79,
     268,    81,    82,   161,     3,     3,    86,     3,   274,   710,
      39,     3,   564,   565,     1,     3,   124,    17,   719,    75,
      76,     3,    51,    80,   124,   291,   563,   564,   565,     3,
      87,    19,    20,    21,     3,    31,   722,    37,    38,   768,
     214,     3,     3,   217,   214,   113,   363,   115,   116,   329,
      24,    21,     3,   161,   747,   124,   214,    21,     5,     6,
       7,     8,     9,    10,    51,    12,    13,    14,    15,    16,
      17,   127,    19,    20,    21,    22,    23,    27,     3,     3,
      22,   122,     3,     3,     3,   212,   213,   904,     3,   130,
     131,   132,   219,    21,     3,   112,     3,    45,     3,    44,
     141,   153,   976,     3,    24,   213,   214,    18,     3,     3,
     121,    22,   122,   213,   155,   101,    24,   131,   245,    24,
     161,   198,     3,     3,     3,     3,     3,     3,     3,    24,
       3,   172,    19,    20,    21,   131,   177,   131,   404,    91,
      88,    89,   119,   891,   221,   162,  1020,    18,   428,   160,
     191,    22,   166,   130,    21,    21,    22,    23,   168,   188,
     201,   202,   107,   115,   160,   140,   246,   142,   166,   168,
     987,   163,   166,   214,   160,   160,   217,   206,    23,   167,
     881,   163,   157,   931,   213,   914,   227,   161,   217,   363,
     160,   220,   166,   363,   163,   322,   225,   253,   884,   240,
     327,   163,   163,   167,   331,   363,     5,   900,   265,    24,
     160,   472,   163,   270,   322,   162,   273,   164,   165,   327,
     162,   168,   322,   331,   131,   254,   162,   327,   765,   766,
     767,   331,   140,    21,   162,   501,   502,   237,   163,   163,
     269,   161,   163,   509,   163,   286,   166,   288,   163,   160,
      21,     3,    24,   322,   163,   363,   161,   523,   166,   300,
     301,   166,   331,   163,   305,   526,   161,   254,   534,   163,
     485,   166,    44,   314,   315,   316,   317,   318,   319,   320,
     167,   561,   163,   163,   163,   163,   163,   163,   163,   160,
     163,   608,   609,   322,   611,   162,   576,   424,   164,   165,
     341,   796,   797,   798,   345,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   424,    18,   162,   164,
     165,    22,   363,   122,   424,    18,    19,    20,    21,    22,
      23,   162,   373,   374,   375,   376,   377,   378,   379,   380,
     381,   382,   383,   384,   385,   386,   387,   388,   389,   390,
     391,   166,   846,    18,   395,   424,   530,     3,   846,   847,
     848,   849,   850,   915,   339,   340,   490,   160,   140,   162,
     162,     3,   160,  1033,   126,   163,    22,   133,   915,   659,
     133,   255,   256,   358,   425,   360,   361,   362,   162,   160,
      94,    95,   163,   153,   166,   370,   520,   663,   133,    31,
     162,   813,   162,   471,   160,   161,   562,   160,   282,     3,
     160,    78,    79,   569,   657,   495,   166,   543,   711,   914,
     500,   160,   836,   503,    18,   160,   669,   166,    22,   160,
      18,   472,   756,   701,    22,   562,  1032,   478,   162,    29,
     614,    31,   569,   162,   571,   572,   573,   574,   575,   160,
     491,   162,   718,   990,   562,   496,   160,   122,   131,   160,
     721,   569,   562,   571,   572,   573,   574,   575,     3,   569,
     163,   571,   572,   573,   574,   575,  1021,  1021,   693,   796,
     797,   798,   162,    18,   525,   526,   162,    22,   662,   530,
     770,   771,   772,   773,   774,   536,     3,  1042,  1042,   131,
     161,   162,   571,   572,   573,   574,   575,    83,   537,    85,
     539,    18,  1006,   123,   641,    22,   160,   546,  1006,   560,
       8,     9,    10,     4,    12,    13,    14,    15,    16,    17,
     657,    19,    20,    21,    22,    23,   577,   664,   579,   160,
     161,   162,   669,    18,    19,    20,    21,    22,    23,    18,
      19,    20,    21,    22,    23,   821,   664,    24,    41,    19,
      20,    21,    22,    23,   664,   161,   162,   608,   609,   166,
     611,   162,   163,   614,   615,   847,   848,   849,   850,   903,
     134,   135,   136,   683,    21,    22,    23,    19,    20,    83,
      84,   632,   633,    37,    38,     3,   637,     3,   639,   123,
     124,   160,   643,     3,   162,   162,   162,   819,   168,   162,
     162,   652,   738,   654,   740,   656,   162,   646,   162,   162,
     162,   662,   796,   749,   653,   886,   162,     3,   162,   162,
     162,   162,   162,   162,   675,   664,   677,   162,     3,   162,
     162,   768,   671,   162,   162,   162,   912,   162,   162,   132,
     691,   692,   160,     3,   160,   138,   139,   160,   982,   163,
     768,   702,     5,   160,     3,   160,   162,   160,   768,   160,
     153,   154,     3,   156,   157,   160,   164,   165,   161,   708,
     721,     5,     6,     7,     8,     9,    10,     3,    12,    13,
      14,    15,    16,    17,   160,    19,    20,    21,    22,    23,
       3,     5,   161,     3,     3,   163,     3,     3,   162,    22,
     751,     5,     3,     3,   163,   163,     3,     3,   163,   760,
     162,   160,   988,     3,   160,   163,     3,   162,     5,     6,
       7,     8,     9,    10,   162,    12,    13,    14,    15,    16,
      17,   162,    19,    20,    21,    22,    23,   162,   160,   163,
     161,     3,     3,    23,     3,   796,   797,   798,     3,     3,
     163,     4,   163,    22,   163,   163,   160,   893,    22,   162,
      26,    21,   813,   814,   163,   160,   902,   163,     4,    36,
     122,   955,     3,   160,   162,   160,    10,   914,    12,    13,
      14,    15,    16,    17,   160,    19,    20,    21,    22,    23,
     163,     3,   162,   162,     5,   163,   914,     5,   935,   163,
     162,   160,   163,   160,   914,   160,     4,   163,     3,   163,
      82,   122,   163,   163,     3,    22,   160,   868,   163,   870,
     167,   872,   160,   160,   163,   168,    82,   163,   163,   160,
     164,   165,    25,   160,   168,   886,   160,    22,   974,   160,
     891,    51,   254,   950,   485,   948,   897,    12,    13,    14,
      15,    16,    17,   904,    19,    20,    21,    22,    23,    12,
      13,    14,    15,    16,    17,   673,    19,    20,    21,    22,
      23,   677,   911,   924,   405,   926,   927,   164,   165,   481,
     931,   932,   505,   934,   480,    78,    79,    80,   260,   525,
     493,   289,   715,   944,    52,   868,   927,   397,   937,   959,
     870,   662,   890,  1016,   955,   985,   541,   757,   937,   960,
     961,   760,   664,   106,   435,   108,   109,   110,   124,    -1,
     113,   114,    -1,   116,   117,    -1,    -1,     0,     1,    -1,
     164,   165,    -1,    -1,   985,    -1,   987,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   983,   996,    -1,    -1,   999,    -1,
      -1,    24,    25,    -1,    -1,    28,    29,    30,    31,    32,
      -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1021,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,  1030,
      -1,  1032,    -1,  1034,  1035,  1024,    -1,    -1,    61,    -1,
      -1,  1042,    65,    66,  1033,    -1,    -1,    70,  1049,   164,
     165,    -1,    -1,    76,    -1,    78,    79,    -1,    81,    -1,
      -1,   164,   165,    86,    87,    88,    89,    90,    -1,    92,
      93,    -1,    -1,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,    -1,   108,    -1,   110,   111,    -1,
      -1,   114,   115,   116,    -1,   118,    -1,   120,    -1,    -1,
     123,   124,    -1,    -1,   127,   128,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,    -1,    -1,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
      -1,    -1,   155,    -1,    -1,   158,   159,   160,   161,    -1,
      -1,    -1,     3,   166,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,     3,    -1,     5,     6,     7,     8,     9,
      10,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,     3,    -1,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,     3,    -1,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     3,    -1,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     4,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     4,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    -1,    -1,    -1,    39,    40,
      41,    42,    43,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    54,    55,    56,    57,    58,    59,    60,
      -1,    62,    63,    64,   164,   165,    67,    68,    69,    -1,
      71,    72,    73,    74,    75,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,   164,   165,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,   164,   165,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,   138,   139,    -1,
      -1,    -1,    -1,   144,    -1,    -1,   163,   164,   165,    -1,
      -1,    -1,   153,   154,    -1,   156,   157,    -1,    -1,   160,
     161,   162,    -1,    -1,    -1,    -1,    -1,   164,   165,     4,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     4,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     4,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   165,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     165,     5,     6,     7,     8,     9,    10,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    20,    21,    22,    23,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,   165,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
     165,     5,     6,     7,     8,     9,    10,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    20,    21,    22,    23,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,    -1,    -1,
      -1,    -1,    -1,    -1,    33,    34,    -1,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    54,    55,    56,    57,    58,
      59,    60,   160,    62,    63,    64,   164,   165,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,    -1,    77,    -1,
      -1,    -1,    -1,    -1,   163,   164,   165,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,   163,
     164,   165,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,
     139,    -1,    -1,    -1,    -1,   144,    -1,    -1,   163,   164,
     165,    -1,    -1,    -1,   153,   154,    -1,   156,   157,    -1,
      -1,   160,   161,   162,    -1,    -1,    -1,   163,   164,   165,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,   165,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,     5,     6,     7,     8,     9,
      10,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,    -1,    -1,    -1,    -1,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   165,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,   165,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,     5,     6,     7,     8,     9,
      10,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,    -1,    -1,    -1,    -1,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,   165,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,   165,     5,     6,     7,     8,     9,
      10,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,   163,   164,   165,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    20,    21,    -1,    -1,    -1,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    33,    34,    -1,    -1,    -1,    -1,
      39,    40,    41,    42,    43,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    54,    55,    56,    57,    58,
      59,    60,    -1,    62,    63,    64,    -1,    -1,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,    34,    77,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    56,
      57,    58,    59,    60,    -1,    62,    63,    64,    -1,    -1,
      67,    68,    69,    -1,     4,    72,    73,    74,    75,    -1,
      77,    11,    -1,    -1,   164,   165,   125,    -1,    -1,    19,
      20,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,
     139,    -1,    -1,    33,    34,   144,    -1,    -1,    -1,    39,
      40,    41,    42,    43,   153,   154,    -1,   156,   157,    -1,
      -1,   160,   161,   162,    54,    55,    56,    57,    58,    59,
      60,    -1,    62,    63,    64,    -1,    -1,    67,    68,    69,
      -1,    71,    72,    73,    74,    75,    -1,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    -1,    -1,   125,    -1,    -1,    19,    20,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,   139,
      -1,    -1,    33,    34,   144,    -1,    -1,    -1,    39,    40,
      41,    42,    43,   153,   154,    -1,   156,   157,    -1,    -1,
     160,   161,   162,    54,    55,    56,    57,    58,    59,    60,
      -1,    62,    63,    64,    -1,    -1,    67,    68,    69,    -1,
      71,    72,    73,    74,    75,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    19,    20,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,   138,   139,    -1,
      -1,    33,    34,   144,    -1,    -1,    -1,    39,    40,    41,
      42,    43,   153,   154,    -1,   156,   157,    -1,    -1,   160,
     161,   162,    54,    55,    56,    57,    58,    59,    60,    -1,
      62,    63,    64,    -1,    -1,    67,    68,    69,    -1,    71,
      72,    73,    74,    75,    -1,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    19,    20,    21,    -1,
     132,    -1,    -1,    -1,    -1,    -1,   138,   139,    -1,    -1,
      33,    34,   144,    -1,    -1,    -1,    39,    40,    41,    42,
      43,   153,   154,    -1,   156,   157,    -1,    -1,   160,   161,
     162,    54,    55,    56,    57,    58,    59,    60,    -1,    62,
      63,    64,    -1,    -1,    67,    68,    69,    -1,    71,    72,
      73,    74,    75,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    19,    20,    -1,    22,   132,
      -1,    -1,    -1,    -1,    -1,   138,   139,    -1,    -1,    33,
      34,   144,    -1,    -1,    -1,    39,    40,    41,    42,    43,
     153,   154,    -1,   156,   157,    -1,    -1,   160,   161,   162,
      54,    55,    56,    57,    58,    59,    60,    -1,    62,    63,
      64,    -1,    -1,    67,    68,    69,    -1,    71,    72,    73,
      74,    75,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    19,    20,    21,    -1,   132,    -1,
      -1,    -1,    -1,    -1,   138,   139,    -1,    -1,    33,    34,
     144,    -1,    -1,    -1,    39,    40,    41,    42,    43,   153,
     154,    -1,   156,   157,    -1,    -1,   160,   161,   162,    54,
      55,    56,    57,    58,    59,    60,    -1,    62,    63,    64,
      -1,    -1,    67,    68,    69,    -1,    71,    72,    73,    74,
      75,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    19,    20,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,   138,   139,    -1,    -1,    33,    34,   144,
      -1,    -1,    -1,    39,    40,    41,    42,    43,   153,   154,
      -1,   156,   157,    -1,    -1,   160,   161,   162,    54,    55,
      56,    57,    58,    59,    60,    -1,    62,    63,    64,    -1,
      -1,    67,    68,    69,    -1,    71,    72,    73,    74,    75,
      -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,   125,
      -1,    -1,    19,    20,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,   138,   139,    -1,    -1,    33,    34,   144,    -1,
      -1,    -1,    39,    40,    41,    42,    43,   153,   154,    -1,
     156,   157,    -1,    -1,   160,   161,   162,    54,    55,    56,
      57,    58,    59,    60,    -1,    62,    63,    64,    -1,    -1,
      67,    68,    69,    -1,    71,    72,    73,    74,    75,    -1,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    19,    20,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,   138,   139,    -1,    -1,    33,    34,   144,    -1,    -1,
      -1,    39,    40,    41,    42,    43,   153,   154,    -1,   156,
     157,    -1,    -1,   160,   161,   162,    54,    55,    56,    57,
      58,    59,    60,    -1,    62,    63,    64,    -1,    -1,    67,
      68,    69,    -1,    71,    72,    73,    74,    75,    -1,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      19,    20,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
     138,   139,    -1,    -1,    33,    34,   144,    -1,    -1,    -1,
      39,    40,    41,    42,    43,   153,   154,    -1,   156,   157,
      -1,    -1,   160,   161,   162,    54,    55,    56,    57,    58,
      59,    60,    -1,    62,    63,    64,    -1,    -1,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,
     139,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,    11,   156,   157,    -1,
      -1,   160,   161,   162,    19,    20,    21,   166,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,
      55,    56,    57,    58,    59,    60,    -1,    62,    63,    64,
      -1,    -1,    67,    68,    69,    -1,    71,    72,    73,    74,
      75,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    19,    20,    21,    -1,   132,    -1,    -1,
      -1,    -1,    -1,   138,   139,    -1,    -1,    33,    34,   144,
      -1,    -1,    -1,    39,    40,    41,    42,    43,   153,   154,
      -1,   156,   157,    -1,    -1,   160,   161,   162,    54,    55,
      56,    57,    58,    59,    60,    -1,    62,    63,    64,    -1,
      -1,    67,    68,    69,    -1,    71,    72,    73,    74,    75,
      -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,   125,
      -1,    -1,    19,    20,    21,    -1,   132,    -1,    -1,    -1,
      -1,    -1,   138,   139,    -1,    -1,    33,    34,   144,    -1,
      -1,    -1,    39,    40,    41,    42,    43,   153,   154,    -1,
     156,   157,    -1,    -1,   160,   161,   162,    54,    55,    56,
      57,    58,    59,    60,    -1,    62,    63,    64,    -1,    -1,
      67,    68,    69,    -1,    71,    72,    73,    74,    75,    -1,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    19,    20,    21,    -1,   132,    -1,    -1,    -1,    -1,
      -1,   138,   139,    -1,    -1,    33,    34,   144,    -1,    -1,
      -1,    39,    40,    41,    42,    43,   153,   154,    -1,   156,
     157,    -1,    -1,   160,   161,   162,    54,    55,    56,    57,
      58,    59,    60,    -1,    62,    63,    64,    -1,    -1,    67,
      68,    69,    -1,    71,    72,    73,    74,    75,    -1,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      19,    20,    21,    -1,   132,    -1,    -1,    -1,    -1,    -1,
     138,   139,    -1,    -1,    33,    34,   144,    -1,    -1,    -1,
      39,    40,    41,    42,    43,   153,   154,    -1,   156,   157,
      -1,    -1,   160,   161,   162,    54,    55,    56,    57,    58,
      59,    60,    -1,    62,    63,    64,    -1,    -1,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,
     139,    -1,    11,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      19,    20,    -1,    -1,   153,   154,    -1,   156,   157,    -1,
      -1,   160,   161,   162,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    54,    55,    56,    57,    58,
      59,    60,    -1,    62,    63,    64,    -1,    -1,    67,    68,
      69,    -1,    71,    72,    73,    74,    75,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    11,    -1,    -1,    -1,    -1,   125,    -1,    -1,    19,
      20,    21,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,
     139,    -1,    -1,    33,    34,   144,    -1,    -1,    -1,    39,
      40,    41,    42,    43,   153,   154,    -1,   156,   157,    -1,
      -1,   160,   161,   162,    54,    55,    56,    57,    58,    59,
      60,    -1,    62,    63,    64,    -1,    -1,    67,    68,    69,
      -1,    71,    72,    73,    74,    75,    -1,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    -1,    -1,   125,    -1,    -1,    19,    20,
      21,    -1,   132,    -1,    -1,    -1,    -1,    -1,   138,   139,
      -1,    -1,    33,    34,   144,    -1,    -1,    -1,    39,    40,
      41,    42,    43,   153,   154,    -1,   156,   157,    -1,    -1,
     160,   161,   162,    54,    55,    56,    57,    58,    59,    60,
      -1,    62,    63,    64,    -1,    -1,    67,    68,    69,    -1,
      71,    72,    73,    74,    75,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    19,    20,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,   138,   139,    -1,
      -1,    33,    34,   144,    -1,    -1,    -1,    39,    40,    41,
      42,    43,   153,   154,    -1,   156,   157,    -1,    -1,   160,
     161,   162,    54,    55,    56,    57,    58,    59,    60,    -1,
      62,    63,    64,    -1,    -1,    67,    68,    69,    -1,    71,
      72,    73,    74,    75,    -1,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    19,    20,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,   138,   139,    -1,    -1,
      33,    34,   144,    -1,    -1,    -1,    39,    40,    41,    42,
      43,   153,   154,    -1,   156,   157,    -1,    -1,   160,   161,
     162,    54,    55,    56,    57,    58,    59,    60,    -1,    62,
      63,    64,    -1,    -1,    67,    68,    69,    -1,    71,    72,
      73,    74,    75,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    19,    20,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,   138,   139,    -1,    -1,    33,
      34,   144,    -1,    -1,    -1,    39,    40,    41,    42,    43,
     153,   154,    -1,   156,   157,    -1,    -1,   160,   161,   162,
      54,    55,    56,    57,    58,    59,    60,    -1,    62,    63,
      64,    -1,    -1,    67,    68,    69,    -1,    71,    72,    73,
      74,    75,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    19,    20,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,   138,   139,    -1,    -1,    33,    34,
     144,    -1,    -1,    -1,    39,    40,    41,    42,    43,   153,
     154,    -1,   156,   157,    -1,    -1,   160,   161,   162,    54,
      55,    56,    57,    58,    59,    60,    -1,    62,    63,    64,
      -1,    -1,    67,    68,    69,    -1,    71,    72,    73,    74,
      75,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    19,    20,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,   138,   139,    -1,    -1,    33,    34,   144,
      -1,    -1,    -1,    39,    40,    41,    42,    43,   153,   154,
      -1,   156,   157,    -1,    -1,   160,   161,   162,    54,    55,
      56,    57,    58,    59,    60,    -1,    62,    63,    64,    -1,
      -1,    67,    68,    69,    -1,    71,    72,    73,    74,    75,
      -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,   125,
      -1,    -1,    19,    20,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,   138,   139,    -1,    -1,    33,    34,   144,    -1,
      -1,    -1,    39,    40,    41,    42,    43,   153,   154,    -1,
     156,   157,    -1,    -1,   160,   161,   162,    54,    55,    56,
      57,    58,    59,    60,    -1,    62,    63,    64,    -1,    -1,
      67,    68,    69,    -1,    71,    72,    73,    74,    75,    -1,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,   138,   139,    -1,    -1,    -1,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,   156,
     157,    -1,    -1,   160,   161,   162,    19,    20,    21,    -1,
      23,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    34,    -1,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    -1,    46,    47,    48,    49,    50,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    62,
      63,    64,    -1,    -1,    67,    68,    69,    -1,    71,    72,
      73,    74,    75,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,   138,   139,    -1,    -1,    -1,
      -1,   144,    -1,    -1,    -1,    -1,    19,    20,    21,    -1,
     153,   154,    -1,   156,   157,    -1,    -1,   160,   161,   162,
      33,    34,    -1,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    -1,    46,    47,    48,    49,    50,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    62,
      63,    64,    19,    20,    67,    68,    69,    -1,    71,    72,
      73,    74,    75,    -1,    77,    -1,    33,    34,    -1,    -1,
      -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,    46,
      47,    48,    49,    50,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    62,    63,    64,    -1,    -1,
      67,    68,    69,    -1,    71,    72,    73,    74,    75,    -1,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,   138,   139,    -1,    -1,    -1,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   154,    -1,   156,   157,    -1,    -1,   160,   161,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    28,
      -1,    30,    -1,    32,    -1,   132,    35,    -1,    -1,    -1,
      -1,   138,   139,    -1,    -1,    -1,    45,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,   156,
     157,    -1,    61,   160,   161,   162,    65,    66,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    76,    -1,    78,
      79,    -1,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    90,    -1,    92,    93,    -1,    -1,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,    -1,   108,
      -1,   110,   111,    -1,    -1,   114,   115,   116,    -1,   118,
      -1,   120,    -1,    -1,   123,   124,    -1,    -1,   127,   128,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,    -1,    -1,   155,    -1,    -1,   158,
     159,   160,   161,    25,    -1,    -1,    28,   166,    30,    -1,
      32,    -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    61,
      -1,    -1,    -1,    65,    66,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    78,    79,    -1,    81,
      -1,    -1,    -1,    -1,    86,    87,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,   108,    -1,   110,   111,
      -1,    -1,   114,   115,   116,    -1,   118,    -1,   120,    -1,
      -1,   123,   124,    -1,    -1,   127,   128,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,    32,    -1,   155,    35,    -1,   158,   159,   160,   161,
      -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      -1,    -1,    93,    -1,    -1,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,    -1,    -1,    32,    -1,    -1,
      35,    -1,    -1,    -1,   115,    -1,    -1,    -1,    -1,   120,
      45,    -1,    -1,    -1,    -1,    -1,   127,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,   150,
     151,    76,    -1,    -1,    -1,    -1,    81,    -1,    -1,   160,
     161,    86,    87,    88,    89,    90,    -1,    -1,    93,    -1,
      -1,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,    -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,
     115,    -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,
      -1,    -1,   127,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,   150,   151,    76,    -1,    -1,
      -1,    -1,    81,    82,    -1,   160,   161,    86,    87,    88,
      -1,    90,    -1,    -1,    93,    -1,    -1,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,   115,    -1,    -1,    -1,
      -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,   127,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,   150,   151,    76,    -1,    -1,    -1,    -1,    81,    -1,
      -1,   160,   161,    86,    87,    88,    -1,    90,    -1,    -1,
      93,    -1,    -1,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    -1,    -1,    -1,    -1,   120,    -1,    -1,
      -1,    -1,    -1,    -1,   127,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,   150,   151,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   170,     0,     1,    24,    25,    28,    30,    32,    35,
      45,    61,    65,    66,    70,    76,    78,    79,    81,    86,
      87,    88,    89,    90,    92,    93,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   108,   110,   111,
     114,   115,   116,   118,   120,   123,   124,   127,   128,   129,
     137,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   155,   158,   159,   160,   161,   166,
     171,   172,   173,   174,   176,   178,   186,   198,   199,   203,
     204,   209,   210,   211,   212,   213,   215,   216,   223,   225,
     228,   231,   232,   267,   276,   277,   282,   283,   284,   286,
     287,   291,   300,   301,   302,   303,   304,   310,   315,   319,
     320,   323,   331,   332,   333,   334,   335,   338,   339,   341,
     344,   345,   348,   357,   359,   369,   153,   181,   160,   180,
     162,   162,    11,    19,    20,    33,    34,    39,    40,    41,
      42,    43,    54,    55,    56,    57,    58,    59,    60,    62,
      63,    64,    67,    68,    69,    71,    72,    73,    74,    75,
      77,   125,   132,   138,   139,   144,   153,   154,   156,   157,
     160,   161,   162,   166,   249,   250,   252,   253,   257,   259,
     260,   261,   262,   267,   268,   269,   273,   275,   162,   191,
     192,   162,   249,   272,   131,   166,   195,   166,   195,   273,
     161,   162,   162,     3,   161,   179,   220,   305,   160,   160,
     166,   336,   162,   162,   352,   160,   161,   162,   353,   162,
     162,   195,   195,    18,    22,   219,   267,   162,   190,   119,
     130,   123,   160,   224,   173,    78,    79,   187,   244,   133,
     160,   161,   237,   336,   160,   162,   200,   201,   206,     4,
     175,    24,   140,   166,    24,    29,    31,   175,   175,   160,
     162,   221,   222,   166,     3,   220,   160,   205,   219,     3,
     220,   220,     3,   220,   160,   217,   218,   219,     3,    31,
     131,   196,   197,   226,   240,    21,   162,   229,    21,   162,
     235,   337,   160,   172,   172,   172,   172,   336,   336,   336,
       5,   122,   168,   340,     3,   162,   342,   101,   160,   273,
     352,    19,    20,    21,    46,    47,    48,    49,    50,    52,
      53,   160,   162,   250,   257,   268,   350,   352,   358,   360,
     361,   362,   175,   249,   294,   249,   249,   249,   251,   162,
     162,   162,   251,   249,   251,   162,   162,   162,   162,   162,
     162,   162,   162,   162,   162,   162,   162,   249,   162,   251,
     162,   162,   162,   162,   249,   250,   252,   275,   366,   367,
     162,   249,   252,     5,     6,     7,     8,     9,    10,    12,
      13,    14,    15,    16,    17,    19,    20,    21,    22,    23,
     164,   165,   254,   249,   168,   162,   271,   162,   162,   160,
     153,   270,   271,   193,   267,     3,   249,   133,   160,   214,
     214,   249,   249,   161,   305,    94,    95,   267,   306,   257,
     370,   371,    21,    23,   162,   267,   354,   355,   360,   361,
     366,   161,   252,   267,   371,   372,   267,   374,   214,    34,
      39,    40,    42,    43,    54,    55,    56,    57,    58,    59,
      60,    62,    63,    64,    67,    68,    69,    72,    73,    74,
      75,    77,   160,   188,   189,   160,   267,   194,   249,     3,
       3,   195,     5,    21,   238,   249,   163,    22,   162,   257,
     220,     3,    22,    45,    88,    89,   175,   166,   174,   180,
     180,     5,   221,     3,   205,   219,   162,   245,   205,   267,
     219,   160,   160,   219,   245,     3,   217,    25,    80,   106,
     108,   109,   110,   113,   114,   116,   117,   241,   242,   244,
     180,     3,   227,   160,   161,   162,   160,   230,   238,   161,
     162,   236,   249,   237,   245,     3,   288,   299,   307,   299,
      44,   321,   322,   324,   249,   249,   356,   161,    21,   249,
     343,   346,   347,   249,   249,   249,   249,   249,   249,   249,
       5,   360,   162,   250,   257,   268,   349,   363,   364,     3,
     351,    18,    21,    22,    23,   362,   360,   163,   163,     3,
     163,   251,   251,   249,   163,   163,   163,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   163,
     251,   163,   251,   251,   251,   252,   366,   367,     3,     3,
     126,     3,   251,   163,     3,     5,   249,   256,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,     5,    22,   249,   255,   249,     5,   249,     5,
     249,   356,   272,   258,   263,   263,     3,   163,   245,   192,
     163,     3,     3,   163,   162,   298,     5,     3,   373,   360,
      21,    23,   162,   249,     3,   163,   163,   163,   163,     3,
     373,     3,   163,     3,    22,     3,   163,   162,   160,   336,
     238,   202,   207,   208,   253,   249,     3,   201,   160,   206,
     202,   162,   162,   179,   305,   162,   182,   182,   249,   163,
     222,   220,     4,    21,   246,   247,   248,   249,   220,   245,
     245,   220,   218,   245,   162,     3,   131,   182,   160,   245,
     230,     5,   238,   163,     3,   239,    21,   163,   245,    27,
     160,   278,   279,   249,   285,   286,   287,   289,   292,   293,
     282,    91,   115,   308,   309,   330,    83,   311,   312,   316,
     112,   162,   326,   107,   322,   325,   172,   267,   161,   163,
       3,   249,   163,   363,   254,   365,   365,   365,     3,   363,
     360,   360,   360,   360,   360,   293,   249,   163,   163,   163,
     163,   163,   163,   163,   163,   163,   163,   163,   163,   163,
     163,   163,   163,   163,   163,   163,     3,     3,     3,   249,
     367,   249,   367,   249,   367,   163,   252,   249,   249,   249,
     249,   249,   257,     4,     4,   249,   264,   265,   266,   163,
     163,   267,   133,   160,   249,    35,    82,    88,   284,   249,
     249,   121,   371,   163,   249,   266,   168,   245,   355,   371,
     163,   267,   188,   249,   194,    22,     3,    19,    20,    21,
     167,   268,   163,   160,   368,    22,   249,   249,   305,    21,
     160,   163,   184,   185,    26,   177,   205,   249,     3,   163,
       4,   267,    21,   233,   233,   217,   134,   135,   136,   243,
     242,   245,   233,   163,   238,   239,   160,   163,     4,   122,
       3,    36,   290,   295,   172,   172,   160,   162,    84,   312,
     313,   314,   317,   172,     4,   249,   327,   328,   329,   160,
     160,   299,   337,   347,     3,   163,   254,   364,   160,   368,
     368,   368,   163,   272,     4,   249,     4,     3,   271,   245,
     163,   162,   162,   163,     3,     5,   163,   356,   163,   202,
     207,   207,   207,   207,     5,   163,   163,   163,     3,   163,
     162,   183,   247,   248,   249,   162,   234,   249,   163,   233,
       5,   122,   274,   239,   238,   160,   166,   280,   281,   160,
     279,   294,    37,    38,   296,   297,   172,   249,   160,    85,
     312,   318,   172,   299,   329,     3,   163,     4,   245,   368,
     365,   163,   163,   163,   163,   249,     4,   249,   265,     3,
     294,   249,   249,   257,   338,   249,    22,    82,   185,   163,
     184,    21,   274,   249,   249,   122,     3,   163,   160,   160,
     172,   288,   163,   160,   299,   328,   329,   245,   163,   249,
       4,   249,   163,   163,     3,     3,   202,   163,   163,   160,
     281,   160,   288,    82,   249,   249,   249,    22,   160,     3,
     249
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))

/* Error token number */
#define YYTERROR	1
#define YYERRCODE	256


/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */
#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
        break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}




/* The lookahead symbol.  */
int yychar;


#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval YY_INITIAL_VALUE(yyval_default);

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 7:
/* Line 1807 of yacc.c  */
#line 319 "fortran.y"
    {yyerrok;yyclearin;}
    break;

  case 18:
/* Line 1807 of yacc.c  */
#line 337 "fortran.y"
    {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
    break;

  case 20:
/* Line 1807 of yacc.c  */
#line 347 "fortran.y"
    { pos_cur = setposcur(); }
    break;

  case 21:
/* Line 1807 of yacc.c  */
#line 350 "fortran.y"
    { isrecursive = 0; }
    break;

  case 22:
/* Line 1807 of yacc.c  */
#line 351 "fortran.y"
    { isrecursive = 1; }
    break;

  case 23:
/* Line 1807 of yacc.c  */
#line 354 "fortran.y"
    { is_result_present = 0; }
    break;

  case 24:
/* Line 1807 of yacc.c  */
#line 355 "fortran.y"
    { is_result_present = 1; }
    break;

  case 25:
/* Line 1807 of yacc.c  */
#line 359 "fortran.y"
    {
            insubroutinedeclare = 1;
            if ( firstpass )
                Add_SubroutineArgument_Var_1((yyvsp[(4) - (4)].l));
            else
                WriteBeginof_SubLoop();
        }
    break;

  case 26:
/* Line 1807 of yacc.c  */
#line 367 "fortran.y"
    {
            insubroutinedeclare = 1;
            inprogramdeclare = 1;
            /* in the second step we should write the head of       */
            /*    the subroutine sub_loop_<subroutinename>          */
            if ( ! firstpass )
                WriteBeginof_SubLoop();
        }
    break;

  case 27:
/* Line 1807 of yacc.c  */
#line 376 "fortran.y"
    {
            insubroutinedeclare = 1;
            strcpy(DeclType, "");
            /* we should to list of the subroutine argument the  */
            /*    name of the function which has to be defined   */
            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1((yyvsp[(4) - (5)].l));
                if ( ! is_result_present )
                    Add_FunctionType_Var_1((yyvsp[(3) - (5)].na));
            }
            else
            /* in the second step we should write the head of    */
            /*    the subroutine sub_loop_<subroutinename>       */
                WriteBeginof_SubLoop();
        }
    break;

  case 28:
/* Line 1807 of yacc.c  */
#line 393 "fortran.y"
    {
            GlobalDeclaration = 0;
            strcpy(curmodulename,(yyvsp[(2) - (2)].na));
            strcpy(subroutinename,"");
            Add_NameOfModule_1((yyvsp[(2) - (2)].na));
            if ( inmoduledeclare == 0 )
            {
                /* To know if there are in the module declaration    */
                inmoduledeclare = 1;
                /* to know if a module has been met                  */
                inmodulemeet = 1;
                /* to know if we are after the keyword contains      */
                aftercontainsdeclare = 0 ;
            }
        }
    break;

  case 31:
/* Line 1807 of yacc.c  */
#line 415 "fortran.y"
    { strcpy((yyval.na), (yyvsp[(1) - (1)].na)); strcpy(subroutinename, (yyvsp[(1) - (1)].na)); }
    break;

  case 32:
/* Line 1807 of yacc.c  */
#line 417 "fortran.y"
    { Add_Include_1((yyvsp[(1) - (1)].na)); }
    break;

  case 33:
/* Line 1807 of yacc.c  */
#line 419 "fortran.y"
    { if ( firstpass ) (yyval.l)=NULL; }
    break;

  case 34:
/* Line 1807 of yacc.c  */
#line 420 "fortran.y"
    { if ( firstpass ) (yyval.l)=NULL; }
    break;

  case 35:
/* Line 1807 of yacc.c  */
#line 421 "fortran.y"
    { if ( firstpass ) (yyval.l)=(yyvsp[(2) - (3)].l); }
    break;

  case 38:
/* Line 1807 of yacc.c  */
#line 425 "fortran.y"
    { if ( firstpass ) Add_SubroutineArgument_Var_1((yyvsp[(2) - (3)].l)); }
    break;

  case 39:
/* Line 1807 of yacc.c  */
#line 428 "fortran.y"
    {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[(1) - (1)].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                (yyval.l) = settype("",curlistvar);
            }
        }
    break;

  case 40:
/* Line 1807 of yacc.c  */
#line 440 "fortran.y"
    {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[(3) - (3)].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                (yyval.l) = insertvar((yyvsp[(1) - (3)].l),curvar);
            }
        }
    break;

  case 41:
/* Line 1807 of yacc.c  */
#line 451 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));  }
    break;

  case 42:
/* Line 1807 of yacc.c  */
#line 452 "fortran.y"
    { strcpy((yyval.na),"*"); }
    break;

  case 44:
/* Line 1807 of yacc.c  */
#line 455 "fortran.y"
    { inside_type_declare = 1; }
    break;

  case 45:
/* Line 1807 of yacc.c  */
#line 456 "fortran.y"
    { inside_type_declare = 0; }
    break;

  case 47:
/* Line 1807 of yacc.c  */
#line 459 "fortran.y"
    {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1((yyvsp[(3) - (4)].l));
                    else                        Add_GlobalParameter_Var_1((yyvsp[(3) - (4)].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out, pos_cur_decl, pos_end-pos_cur_decl);
                }
            }
            VariableIsParameter =  0 ;
        }
    break;

  case 48:
/* Line 1807 of yacc.c  */
#line 476 "fortran.y"
    {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1((yyvsp[(2) - (2)].l));
                    else                        Add_GlobalParameter_Var_1((yyvsp[(2) - (2)].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                }
            }
            VariableIsParameter =  0 ;
        }
    break;

  case 50:
/* Line 1807 of yacc.c  */
#line 494 "fortran.y"
    {
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
        }
    break;

  case 52:
/* Line 1807 of yacc.c  */
#line 500 "fortran.y"
    {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[(1) - (1)].l));
                    /* if variableparamlists has been declared in a subroutine   */
                    if ( insubroutinedeclare )     Add_Dimension_Var_1((yyvsp[(1) - (1)].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
                }
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
    break;

  case 53:
/* Line 1807 of yacc.c  */
#line 537 "fortran.y"
    {
            if (firstpass == 0)
            {
                if ((yyvsp[(1) - (1)].lnn))
                {
                    removeglobfromlist(&((yyvsp[(1) - (1)].lnn)));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic((yyvsp[(1) - (1)].lnn));
                }
            }
        }
    break;

  case 62:
/* Line 1807 of yacc.c  */
#line 558 "fortran.y"
    {
            /* we should remove the data declaration                */
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curdata,pos_end-pos_curdata);

            if ( aftercontainsdeclare == 1  && firstpass == 0 )
            {
                ReWriteDataStatement_0(fortran_out);
                pos_end = setposcur();
            }
        }
    break;

  case 64:
/* Line 1807 of yacc.c  */
#line 572 "fortran.y"
    {
            PublicDeclare = 0 ;
            PrivateDeclare = 0 ;
        }
    break;

  case 102:
/* Line 1807 of yacc.c  */
#line 630 "fortran.y"
    {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( ! inside_type_declare )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01((yyvsp[(1) - (2)].l));
                pos_cur_decl = setposcur();
                if ( firstpass == 0 && GlobalDeclaration == 0
                                    && insubroutinedeclare == 0 )
                {
                    fprintf(fortran_out,"\n#include \"Module_Declar_%s.h\"\n", curmodulename);
                    sprintf(ligne, "Module_Declar_%s.h", curmodulename);
                    module_declar = open_for_write(ligne);
                    GlobalDeclaration = 1 ;
                    pos_cur_decl = setposcur();
                }
                (yyval.l) = (yyvsp[(1) - (2)].l);

                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[(1) - (2)].l));
                    if ( insubroutinedeclare )
                    {
                        if ( pointerdeclare ) Add_Pointer_Var_From_List_1((yyvsp[(1) - (2)].l));
                        Add_Parameter_Var_1((yyvsp[(1) - (2)].l));
                    }
                    else
                        Add_GlobalParameter_Var_1((yyvsp[(1) - (2)].l));

                    /* If there's a SAVE declaration in module's subroutines we should    */
                    /*    remove it from the subroutines declaration and add it in the    */
                    /*    global declarations                                             */
                    if ( aftercontainsdeclare && SaveDeclare )
                    {
                        if ( inmodulemeet ) Add_SubroutineDeclarationSave_Var_1((yyvsp[(1) - (2)].l));
                        else                Add_Save_Var_dcl_1((yyvsp[(1) - (2)].l));
                    }
                }
            }
            else
            {
                (yyval.l) = (listvar *) NULL;
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            GlobalDeclarationType = 0;
        }
    break;

  case 103:
/* Line 1807 of yacc.c  */
#line 696 "fortran.y"
    {
            insubroutinedeclare = 1;

            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                Add_FunctionType_Var_1((yyvsp[(2) - (3)].na));
            }
            else
                WriteBeginof_SubLoop();

            strcpy(nameinttypename,"");
        }
    break;

  case 104:
/* Line 1807 of yacc.c  */
#line 710 "fortran.y"
    { functiondeclarationisdone = 1; }
    break;

  case 105:
/* Line 1807 of yacc.c  */
#line 712 "fortran.y"
    { VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
    break;

  case 109:
/* Line 1807 of yacc.c  */
#line 724 "fortran.y"
    {
            createstringfromlistname(ligne,(yyvsp[(3) - (4)].lnn));
            if (firstpass == 1) Add_Data_Var_1(&List_Data_Var,(yyvsp[(1) - (4)].na),ligne);
            else                Add_Data_Var_1(&List_Data_Var_Cur,(yyvsp[(1) - (4)].na),ligne);
        }
    break;

  case 110:
/* Line 1807 of yacc.c  */
#line 730 "fortran.y"
    {
            if (firstpass == 1)  Add_Data_Var_Names_01(&List_Data_Var,(yyvsp[(1) - (4)].lnn),(yyvsp[(3) - (4)].lnn));
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,(yyvsp[(1) - (4)].lnn),(yyvsp[(3) - (4)].lnn));
        }
    break;

  case 111:
/* Line 1807 of yacc.c  */
#line 735 "fortran.y"
    {
            createstringfromlistname(ligne,(yyvsp[(7) - (8)].lnn));
            printf("###################################################################################################################\n");
            printf("## CONV Error : data_implied_do statements (R537) are not yet supported. Please complain to the proper authorities.\n");
            printf("l.%4d -- data_stmt_set : ( lhs , dospec ) /data_stmt_value_list/ -- lhs=|%s| dospec=|%s| data_stmt_value_list=|%s|\n",
                line_num_input,(yyvsp[(2) - (8)].na),(yyvsp[(4) - (8)].na),ligne);
            printf("## But, are you SURE you NEED a DATA construct ?\n");
            printf("###################################################################################################################\n");
            exit(1);
        }
    break;

  case 112:
/* Line 1807 of yacc.c  */
#line 748 "fortran.y"
    { (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].na),0); }
    break;

  case 113:
/* Line 1807 of yacc.c  */
#line 749 "fortran.y"
    { (yyval.lnn) = Insertname((yyvsp[(3) - (3)].lnn),(yyvsp[(1) - (3)].na),1);   }
    break;

  case 118:
/* Line 1807 of yacc.c  */
#line 758 "fortran.y"
    { pos_cursave = setposcur()-4; }
    break;

  case 120:
/* Line 1807 of yacc.c  */
#line 761 "fortran.y"
    { if ( ! inside_type_declare ) Add_Save_Var_1((yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].d)); }
    break;

  case 121:
/* Line 1807 of yacc.c  */
#line 764 "fortran.y"
    { (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].na),0); }
    break;

  case 122:
/* Line 1807 of yacc.c  */
#line 765 "fortran.y"
    { printf("l.%4d -- INSTRUCTION NON TRAITEE : INITIALISATION DE DATA AVEC EXPRESSION\n",line_num_input); exit(0); }
    break;

  case 123:
/* Line 1807 of yacc.c  */
#line 766 "fortran.y"
    { (yyval.lnn) = concat_listname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].lnn)); }
    break;

  case 124:
/* Line 1807 of yacc.c  */
#line 769 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na));  }
    break;

  case 125:
/* Line 1807 of yacc.c  */
#line 770 "fortran.y"
    { sprintf((yyval.na),"%s+%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 126:
/* Line 1807 of yacc.c  */
#line 771 "fortran.y"
    { sprintf((yyval.na),"%s-%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 127:
/* Line 1807 of yacc.c  */
#line 772 "fortran.y"
    { sprintf((yyval.na),"%s*%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 128:
/* Line 1807 of yacc.c  */
#line 773 "fortran.y"
    { sprintf((yyval.na),"%s/%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 129:
/* Line 1807 of yacc.c  */
#line 775 "fortran.y"
    { strcpy((yyval.na),""); }
    break;

  case 130:
/* Line 1807 of yacc.c  */
#line 776 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 135:
/* Line 1807 of yacc.c  */
#line 786 "fortran.y"
    {
            positioninblock = 0;
            pos_curdimension = setposcur()-9;
        }
    break;

  case 136:
/* Line 1807 of yacc.c  */
#line 793 "fortran.y"
    {
            printf("l.%4d -- dimension : before_dimension opt_comma TOK_NAME = |%s| -- MHCHECK\n",line_num_input,(yyvsp[(3) - (5)].na));
            if ( inside_type_declare ) break;
            curvar = createvar((yyvsp[(3) - (5)].na),(yyvsp[(4) - (5)].d));
            CreateAndFillin_Curvar("", curvar);
            curlistvar=insertvar(NULL, curvar);
            (yyval.l) = settype("",curlistvar);
            strcpy(vallengspec,"");
        }
    break;

  case 137:
/* Line 1807 of yacc.c  */
#line 803 "fortran.y"
    {
            printf("l.%4d -- dimension : dimension ',' TOK_NAME dims lengspec = |%s| -- MHCHECK\n",line_num_input,(yyvsp[(3) - (5)].na));
            if ( inside_type_declare ) break;
            curvar = createvar((yyvsp[(3) - (5)].na),(yyvsp[(4) - (5)].d));
            CreateAndFillin_Curvar("", curvar);
            curlistvar = insertvar((yyvsp[(1) - (5)].l), curvar);
            (yyval.l) = curlistvar;
            strcpy(vallengspec,"");
        }
    break;

  case 140:
/* Line 1807 of yacc.c  */
#line 818 "fortran.y"
    { (yyval.lnn) = (listname *) NULL; }
    break;

  case 141:
/* Line 1807 of yacc.c  */
#line 819 "fortran.y"
    { (yyval.lnn) = (yyvsp[(3) - (3)].lnn); }
    break;

  case 142:
/* Line 1807 of yacc.c  */
#line 822 "fortran.y"
    { (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].na),0); }
    break;

  case 143:
/* Line 1807 of yacc.c  */
#line 823 "fortran.y"
    { (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].na),0); }
    break;

  case 144:
/* Line 1807 of yacc.c  */
#line 824 "fortran.y"
    { (yyval.lnn) = Insertname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].na),0);   }
    break;

  case 145:
/* Line 1807 of yacc.c  */
#line 825 "fortran.y"
    { (yyval.lnn) = Insertname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].na),0);   }
    break;

  case 146:
/* Line 1807 of yacc.c  */
#line 829 "fortran.y"
    {
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
    break;

  case 147:
/* Line 1807 of yacc.c  */
#line 835 "fortran.y"
    {
            if ( inside_type_declare ) break;
            sprintf(charusemodule,"%s",(yyvsp[(2) - (3)].na));
            Add_NameOfCommon_1((yyvsp[(2) - (3)].na),subroutinename);
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
    break;

  case 148:
/* Line 1807 of yacc.c  */
#line 843 "fortran.y"
    {
            if ( inside_type_declare ) break;
            sprintf(charusemodule,"%s",(yyvsp[(3) - (5)].na));
            Add_NameOfCommon_1((yyvsp[(3) - (5)].na),subroutinename);
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
    break;

  case 149:
/* Line 1807 of yacc.c  */
#line 852 "fortran.y"
    { positioninblock = 0; pos_curcommon = setposcur()-6;   }
    break;

  case 150:
/* Line 1807 of yacc.c  */
#line 853 "fortran.y"
    { positioninblock = 0; pos_curcommon = setposcur()-6-7; }
    break;

  case 151:
/* Line 1807 of yacc.c  */
#line 856 "fortran.y"
    { if ( ! inside_type_declare ) Add_Common_var_1(); }
    break;

  case 152:
/* Line 1807 of yacc.c  */
#line 857 "fortran.y"
    { if ( ! inside_type_declare ) Add_Common_var_1(); }
    break;

  case 153:
/* Line 1807 of yacc.c  */
#line 861 "fortran.y"
    {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[(1) - (2)].na));
            commondim = (yyvsp[(2) - (2)].d);
        }
    break;

  case 154:
/* Line 1807 of yacc.c  */
#line 869 "fortran.y"
    {
            strcpy((yyval.na),"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
    break;

  case 155:
/* Line 1807 of yacc.c  */
#line 875 "fortran.y"
    {
            strcpy((yyval.na),(yyvsp[(2) - (3)].na));
            positioninblock=0;
            strcpy(commonblockname,(yyvsp[(2) - (3)].na));
        }
    break;

  case 158:
/* Line 1807 of yacc.c  */
#line 885 "fortran.y"
    { (yyval.l)=insertvar(NULL,(yyvsp[(1) - (1)].v)); }
    break;

  case 159:
/* Line 1807 of yacc.c  */
#line 886 "fortran.y"
    { (yyval.l)=insertvar((yyvsp[(1) - (3)].l),(yyvsp[(3) - (3)].v));   }
    break;

  case 160:
/* Line 1807 of yacc.c  */
#line 890 "fortran.y"
    {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,(yyvsp[(1) - (3)].na));
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            strcpy(curvar->v_initialvalue,(yyvsp[(3) - (3)].na));
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length((yyvsp[(3) - (3)].na),14);
            (yyval.v) = curvar;
        }
    break;

  case 164:
/* Line 1807 of yacc.c  */
#line 913 "fortran.y"
    {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_end-13,13);
            }
        }
    break;

  case 166:
/* Line 1807 of yacc.c  */
#line 924 "fortran.y"
    {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar((yyvsp[(2) - (5)].na),curdim);
                else                curvar = createvar((yyvsp[(2) - (5)].na),(yyvsp[(3) - (5)].d));
                CreateAndFillin_Curvar(DeclType, curvar);
                curlistvar = insertvar(NULL, curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                (yyval.l)=settype(DeclType,curlistvar);
            }
            strcpy(vallengspec,"");
        }
    break;

  case 167:
/* Line 1807 of yacc.c  */
#line 946 "fortran.y"
    {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar((yyvsp[(3) - (6)].na), curdim);
                else                curvar = createvar((yyvsp[(3) - (6)].na), (yyvsp[(4) - (6)].d));
                CreateAndFillin_Curvar((yyvsp[(1) - (6)].l)->var->v_typevar,curvar);
                strcpy(curvar->v_typevar, (yyvsp[(1) - (6)].l)->var->v_typevar);
                curvar->v_catvar = get_cat_var(curvar);
                curlistvar = insertvar((yyvsp[(1) - (6)].l), curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                (yyval.l)=curlistvar;
            }
            strcpy(vallengspec,"");
        }
    break;

  case 168:
/* Line 1807 of yacc.c  */
#line 970 "fortran.y"
    { dimsgiven = 0; }
    break;

  case 169:
/* Line 1807 of yacc.c  */
#line 972 "fortran.y"
    { strcpy(DeclType,(yyvsp[(1) - (2)].na));  }
    break;

  case 170:
/* Line 1807 of yacc.c  */
#line 973 "fortran.y"
    { strcpy(DeclType,"character");  }
    break;

  case 171:
/* Line 1807 of yacc.c  */
#line 974 "fortran.y"
    { strcpy(DeclType,(yyvsp[(1) - (3)].na)); strcpy(nameinttypename,(yyvsp[(3) - (3)].na));  }
    break;

  case 172:
/* Line 1807 of yacc.c  */
#line 975 "fortran.y"
    { strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
    break;

  case 174:
/* Line 1807 of yacc.c  */
#line 978 "fortran.y"
    { c_selectorgiven = 1; strcpy(c_selectorname,(yyvsp[(2) - (2)].na)); }
    break;

  case 175:
/* Line 1807 of yacc.c  */
#line 979 "fortran.y"
    { c_star = 1;}
    break;

  case 180:
/* Line 1807 of yacc.c  */
#line 987 "fortran.y"
    { pos_cur_decl = setposcur()-9; }
    break;

  case 181:
/* Line 1807 of yacc.c  */
#line 990 "fortran.y"
    { strcpy((yyval.na),"integer"); pos_cur_decl = setposcur()-7; }
    break;

  case 182:
/* Line 1807 of yacc.c  */
#line 991 "fortran.y"
    { strcpy((yyval.na),"logical"); pos_cur_decl = setposcur()-7; }
    break;

  case 183:
/* Line 1807 of yacc.c  */
#line 992 "fortran.y"
    { strcpy((yyval.na),"real");    pos_cur_decl = setposcur()-4; }
    break;

  case 184:
/* Line 1807 of yacc.c  */
#line 993 "fortran.y"
    { strcpy((yyval.na),"complex"); pos_cur_decl = setposcur()-7; }
    break;

  case 185:
/* Line 1807 of yacc.c  */
#line 994 "fortran.y"
    { strcpy((yyval.na),"double complex"); pos_cur_decl = setposcur()-14; }
    break;

  case 186:
/* Line 1807 of yacc.c  */
#line 995 "fortran.y"
    { pos_cur_decl = setposcur()-16; strcpy((yyval.na),"real"); strcpy(nameinttypename,"8"); }
    break;

  case 188:
/* Line 1807 of yacc.c  */
#line 998 "fortran.y"
    {strcpy(vallengspec,(yyvsp[(2) - (2)].na));}
    break;

  case 189:
/* Line 1807 of yacc.c  */
#line 1001 "fortran.y"
    { sprintf((yyval.na),"*%s",(yyvsp[(1) - (1)].na)); }
    break;

  case 190:
/* Line 1807 of yacc.c  */
#line 1002 "fortran.y"
    { strcpy((yyval.na),"*(*)"); }
    break;

  case 197:
/* Line 1807 of yacc.c  */
#line 1014 "fortran.y"
    {
            if ( strstr((yyvsp[(3) - (3)].na),"0.d0") )
            {
                strcpy(nameinttypename,"8");
                strcpy(NamePrecision,"");
            }
            else
                sprintf(NamePrecision,"%s = %s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));
        }
    break;

  case 198:
/* Line 1807 of yacc.c  */
#line 1023 "fortran.y"
    { strcpy(NamePrecision,(yyvsp[(1) - (1)].na)); }
    break;

  case 199:
/* Line 1807 of yacc.c  */
#line 1024 "fortran.y"
    { strcpy(NamePrecision,(yyvsp[(1) - (1)].na)); }
    break;

  case 200:
/* Line 1807 of yacc.c  */
#line 1025 "fortran.y"
    { strcpy(NamePrecision,(yyvsp[(1) - (1)].na)); }
    break;

  case 201:
/* Line 1807 of yacc.c  */
#line 1028 "fortran.y"
    { strcpy(CharacterSize,(yyvsp[(1) - (1)].na));  strcpy((yyval.na),(yyvsp[(1) - (1)].na));  }
    break;

  case 202:
/* Line 1807 of yacc.c  */
#line 1029 "fortran.y"
    { strcpy(CharacterSize,"*"); strcpy((yyval.na),"*"); }
    break;

  case 210:
/* Line 1807 of yacc.c  */
#line 1042 "fortran.y"
    { VariableIsParameter = 1; }
    break;

  case 212:
/* Line 1807 of yacc.c  */
#line 1044 "fortran.y"
    { Allocatabledeclare = 1; }
    break;

  case 213:
/* Line 1807 of yacc.c  */
#line 1045 "fortran.y"
    { dimsgiven = 1; curdim = (yyvsp[(2) - (2)].d); }
    break;

  case 214:
/* Line 1807 of yacc.c  */
#line 1046 "fortran.y"
    { ExternalDeclare = 1; }
    break;

  case 215:
/* Line 1807 of yacc.c  */
#line 1048 "fortran.y"
    { strcpy(IntentSpec,(yyvsp[(3) - (4)].na)); }
    break;

  case 217:
/* Line 1807 of yacc.c  */
#line 1050 "fortran.y"
    { optionaldeclare = 1 ; }
    break;

  case 218:
/* Line 1807 of yacc.c  */
#line 1051 "fortran.y"
    { pointerdeclare = 1 ; }
    break;

  case 219:
/* Line 1807 of yacc.c  */
#line 1052 "fortran.y"
    { SaveDeclare = 1 ; }
    break;

  case 220:
/* Line 1807 of yacc.c  */
#line 1053 "fortran.y"
    { Targetdeclare = 1; }
    break;

  case 221:
/* Line 1807 of yacc.c  */
#line 1056 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 222:
/* Line 1807 of yacc.c  */
#line 1057 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 223:
/* Line 1807 of yacc.c  */
#line 1058 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 224:
/* Line 1807 of yacc.c  */
#line 1061 "fortran.y"
    { PublicDeclare = 1;  }
    break;

  case 225:
/* Line 1807 of yacc.c  */
#line 1062 "fortran.y"
    { PrivateDeclare = 1; }
    break;

  case 226:
/* Line 1807 of yacc.c  */
#line 1064 "fortran.y"
    { (yyval.d) = (listdim*) NULL; }
    break;

  case 227:
/* Line 1807 of yacc.c  */
#line 1066 "fortran.y"
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=(yyvsp[(2) - (3)].d);
        }
    break;

  case 228:
/* Line 1807 of yacc.c  */
#line 1074 "fortran.y"
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[(1) - (1)].dim1));
        }
    break;

  case 229:
/* Line 1807 of yacc.c  */
#line 1080 "fortran.y"
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[(1) - (3)].d),(yyvsp[(3) - (3)].dim1));
        }
    break;

  case 230:
/* Line 1807 of yacc.c  */
#line 1086 "fortran.y"
    { strcpy((yyval.dim1).first,"1"); strcpy((yyval.dim1).last,(yyvsp[(1) - (1)].na)); Save_Length((yyvsp[(1) - (1)].na),1); }
    break;

  case 231:
/* Line 1807 of yacc.c  */
#line 1087 "fortran.y"
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");                    }
    break;

  case 232:
/* Line 1807 of yacc.c  */
#line 1088 "fortran.y"
    { strcpy((yyval.dim1).first,(yyvsp[(1) - (2)].na));  Save_Length((yyvsp[(1) - (2)].na),2); strcpy((yyval.dim1).last,""); }
    break;

  case 233:
/* Line 1807 of yacc.c  */
#line 1089 "fortran.y"
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,(yyvsp[(2) - (2)].na)); Save_Length((yyvsp[(2) - (2)].na),1); }
    break;

  case 234:
/* Line 1807 of yacc.c  */
#line 1090 "fortran.y"
    { strcpy((yyval.dim1).first,(yyvsp[(1) - (3)].na));  Save_Length((yyvsp[(1) - (3)].na),2); strcpy((yyval.dim1).last,(yyvsp[(3) - (3)].na)); Save_Length((yyvsp[(3) - (3)].na),1); }
    break;

  case 235:
/* Line 1807 of yacc.c  */
#line 1093 "fortran.y"
    { strcpy((yyval.na),"*"); }
    break;

  case 236:
/* Line 1807 of yacc.c  */
#line 1094 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));  }
    break;

  case 237:
/* Line 1807 of yacc.c  */
#line 1096 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 238:
/* Line 1807 of yacc.c  */
#line 1097 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 239:
/* Line 1807 of yacc.c  */
#line 1098 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 240:
/* Line 1807 of yacc.c  */
#line 1099 "fortran.y"
    { sprintf((yyval.na),"(%s)",(yyvsp[(2) - (3)].na)); }
    break;

  case 241:
/* Line 1807 of yacc.c  */
#line 1103 "fortran.y"
    { sprintf((yyval.na),"SUM(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 242:
/* Line 1807 of yacc.c  */
#line 1104 "fortran.y"
    { sprintf((yyval.na),"MAX(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 243:
/* Line 1807 of yacc.c  */
#line 1105 "fortran.y"
    { sprintf((yyval.na),"TANH(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 244:
/* Line 1807 of yacc.c  */
#line 1106 "fortran.y"
    { sprintf((yyval.na),"MAXVAL(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 245:
/* Line 1807 of yacc.c  */
#line 1107 "fortran.y"
    { sprintf((yyval.na),"MIN(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 246:
/* Line 1807 of yacc.c  */
#line 1108 "fortran.y"
    { sprintf((yyval.na),"MINVAL(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 247:
/* Line 1807 of yacc.c  */
#line 1109 "fortran.y"
    { sprintf((yyval.na),"TRIM(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 248:
/* Line 1807 of yacc.c  */
#line 1110 "fortran.y"
    { sprintf((yyval.na),"SQRT(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 249:
/* Line 1807 of yacc.c  */
#line 1111 "fortran.y"
    { sprintf((yyval.na),"REAL(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 250:
/* Line 1807 of yacc.c  */
#line 1112 "fortran.y"
    { sprintf((yyval.na),"NINT(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 251:
/* Line 1807 of yacc.c  */
#line 1113 "fortran.y"
    { sprintf((yyval.na),"FLOAT(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 252:
/* Line 1807 of yacc.c  */
#line 1114 "fortran.y"
    { sprintf((yyval.na),"EXP(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 253:
/* Line 1807 of yacc.c  */
#line 1115 "fortran.y"
    { sprintf((yyval.na),"COS(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 254:
/* Line 1807 of yacc.c  */
#line 1116 "fortran.y"
    { sprintf((yyval.na),"COSH(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 255:
/* Line 1807 of yacc.c  */
#line 1117 "fortran.y"
    { sprintf((yyval.na),"ACOS(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 256:
/* Line 1807 of yacc.c  */
#line 1118 "fortran.y"
    { sprintf((yyval.na),"SIN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 257:
/* Line 1807 of yacc.c  */
#line 1119 "fortran.y"
    { sprintf((yyval.na),"SINH(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 258:
/* Line 1807 of yacc.c  */
#line 1120 "fortran.y"
    { sprintf((yyval.na),"ASIN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 259:
/* Line 1807 of yacc.c  */
#line 1121 "fortran.y"
    { sprintf((yyval.na),"LOG(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 260:
/* Line 1807 of yacc.c  */
#line 1122 "fortran.y"
    { sprintf((yyval.na),"TAN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 261:
/* Line 1807 of yacc.c  */
#line 1123 "fortran.y"
    { sprintf((yyval.na),"ATAN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 262:
/* Line 1807 of yacc.c  */
#line 1124 "fortran.y"
    { sprintf((yyval.na),"ABS(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 263:
/* Line 1807 of yacc.c  */
#line 1125 "fortran.y"
    { sprintf((yyval.na),"MOD(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 264:
/* Line 1807 of yacc.c  */
#line 1126 "fortran.y"
    { sprintf((yyval.na),"SIGN(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 265:
/* Line 1807 of yacc.c  */
#line 1127 "fortran.y"
    { sprintf((yyval.na),"MINLOC(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 266:
/* Line 1807 of yacc.c  */
#line 1128 "fortran.y"
    { sprintf((yyval.na),"MAXLOC(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 267:
/* Line 1807 of yacc.c  */
#line 1130 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 268:
/* Line 1807 of yacc.c  */
#line 1131 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 269:
/* Line 1807 of yacc.c  */
#line 1133 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 270:
/* Line 1807 of yacc.c  */
#line 1134 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 271:
/* Line 1807 of yacc.c  */
#line 1135 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 272:
/* Line 1807 of yacc.c  */
#line 1136 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 273:
/* Line 1807 of yacc.c  */
#line 1137 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 274:
/* Line 1807 of yacc.c  */
#line 1138 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 275:
/* Line 1807 of yacc.c  */
#line 1140 "fortran.y"
    { strcpy((yyval.na),"+"); }
    break;

  case 276:
/* Line 1807 of yacc.c  */
#line 1141 "fortran.y"
    { strcpy((yyval.na),"-"); }
    break;

  case 277:
/* Line 1807 of yacc.c  */
#line 1145 "fortran.y"
    { sprintf((yyval.na),"+%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 278:
/* Line 1807 of yacc.c  */
#line 1146 "fortran.y"
    { sprintf((yyval.na),"-%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 279:
/* Line 1807 of yacc.c  */
#line 1147 "fortran.y"
    { sprintf((yyval.na),"*%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 280:
/* Line 1807 of yacc.c  */
#line 1148 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 281:
/* Line 1807 of yacc.c  */
#line 1149 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 282:
/* Line 1807 of yacc.c  */
#line 1150 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 283:
/* Line 1807 of yacc.c  */
#line 1151 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 284:
/* Line 1807 of yacc.c  */
#line 1152 "fortran.y"
    { sprintf((yyval.na)," > %s",(yyvsp[(2) - (2)].na)); }
    break;

  case 285:
/* Line 1807 of yacc.c  */
#line 1153 "fortran.y"
    { sprintf((yyval.na)," < %s",(yyvsp[(2) - (2)].na)); }
    break;

  case 286:
/* Line 1807 of yacc.c  */
#line 1154 "fortran.y"
    { sprintf((yyval.na)," >= %s",(yyvsp[(3) - (3)].na)); }
    break;

  case 287:
/* Line 1807 of yacc.c  */
#line 1155 "fortran.y"
    { sprintf((yyval.na)," <= %s",(yyvsp[(3) - (3)].na)); }
    break;

  case 288:
/* Line 1807 of yacc.c  */
#line 1156 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 289:
/* Line 1807 of yacc.c  */
#line 1157 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 290:
/* Line 1807 of yacc.c  */
#line 1158 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 291:
/* Line 1807 of yacc.c  */
#line 1159 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 292:
/* Line 1807 of yacc.c  */
#line 1160 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 293:
/* Line 1807 of yacc.c  */
#line 1161 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 294:
/* Line 1807 of yacc.c  */
#line 1162 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 295:
/* Line 1807 of yacc.c  */
#line 1163 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 296:
/* Line 1807 of yacc.c  */
#line 1164 "fortran.y"
    { sprintf((yyval.na),"%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 297:
/* Line 1807 of yacc.c  */
#line 1165 "fortran.y"
    { sprintf((yyval.na),"%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 298:
/* Line 1807 of yacc.c  */
#line 1167 "fortran.y"
    { strcpy((yyval.na),""); }
    break;

  case 299:
/* Line 1807 of yacc.c  */
#line 1168 "fortran.y"
    { sprintf((yyval.na),"/%s",(yyvsp[(1) - (1)].na)); }
    break;

  case 300:
/* Line 1807 of yacc.c  */
#line 1169 "fortran.y"
    { sprintf((yyval.na),"/= %s",(yyvsp[(2) - (2)].na));}
    break;

  case 301:
/* Line 1807 of yacc.c  */
#line 1170 "fortran.y"
    { sprintf((yyval.na),"//%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 302:
/* Line 1807 of yacc.c  */
#line 1173 "fortran.y"
    { sprintf((yyval.na),"==%s",(yyvsp[(2) - (2)].na)); }
    break;

  case 303:
/* Line 1807 of yacc.c  */
#line 1174 "fortran.y"
    { sprintf((yyval.na),"= %s",(yyvsp[(1) - (1)].na)); }
    break;

  case 304:
/* Line 1807 of yacc.c  */
#line 1177 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 305:
/* Line 1807 of yacc.c  */
#line 1178 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 306:
/* Line 1807 of yacc.c  */
#line 1179 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 307:
/* Line 1807 of yacc.c  */
#line 1183 "fortran.y"
    {
            agrif_parentcall = 0;
            if ( !strcasecmp(identcopy, "Agrif_Parent") )   agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
    break;

  case 308:
/* Line 1807 of yacc.c  */
#line 1194 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
    break;

  case 309:
/* Line 1807 of yacc.c  */
#line 1195 "fortran.y"
    { sprintf((yyval.na)," %s %s ",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 310:
/* Line 1807 of yacc.c  */
#line 1196 "fortran.y"
    { sprintf((yyval.na)," %s ( %s )",(yyvsp[(1) - (4)].na),(yyvsp[(3) - (4)].na)); }
    break;

  case 311:
/* Line 1807 of yacc.c  */
#line 1197 "fortran.y"
    { sprintf((yyval.na)," %s ( %s ) %s ",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na)); }
    break;

  case 312:
/* Line 1807 of yacc.c  */
#line 1201 "fortran.y"
    {
            if ( inside_type_declare ) break;
            sprintf((yyval.na)," %s ( %s )",(yyvsp[(1) - (4)].na),(yyvsp[(3) - (4)].na));
            ModifyTheAgrifFunction_0((yyvsp[(3) - (4)].na));
            agrif_parentcall = 0;
        }
    break;

  case 313:
/* Line 1807 of yacc.c  */
#line 1210 "fortran.y"
    {
            sprintf((yyval.na)," %s %% %s ",(yyvsp[(1) - (4)].na),(yyvsp[(4) - (4)].na));
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
    break;

  case 314:
/* Line 1807 of yacc.c  */
#line 1216 "fortran.y"
    { sprintf((yyval.na),"(/%s/)",(yyvsp[(2) - (3)].na)); }
    break;

  case 315:
/* Line 1807 of yacc.c  */
#line 1219 "fortran.y"
    { strcpy((yyval.na)," "); }
    break;

  case 316:
/* Line 1807 of yacc.c  */
#line 1220 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(2) - (2)].na)); }
    break;

  case 317:
/* Line 1807 of yacc.c  */
#line 1223 "fortran.y"
    {  strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 318:
/* Line 1807 of yacc.c  */
#line 1224 "fortran.y"
    {  sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 319:
/* Line 1807 of yacc.c  */
#line 1227 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 320:
/* Line 1807 of yacc.c  */
#line 1228 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 321:
/* Line 1807 of yacc.c  */
#line 1231 "fortran.y"
    {  sprintf((yyval.na),"%s :%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));}
    break;

  case 322:
/* Line 1807 of yacc.c  */
#line 1232 "fortran.y"
    {  sprintf((yyval.na),"%s :%s :%s",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));}
    break;

  case 323:
/* Line 1807 of yacc.c  */
#line 1233 "fortran.y"
    {  sprintf((yyval.na),":%s :%s",(yyvsp[(2) - (4)].na),(yyvsp[(4) - (4)].na));}
    break;

  case 324:
/* Line 1807 of yacc.c  */
#line 1234 "fortran.y"
    {  sprintf((yyval.na),": : %s",(yyvsp[(3) - (3)].na));}
    break;

  case 325:
/* Line 1807 of yacc.c  */
#line 1235 "fortran.y"
    {  sprintf((yyval.na),":%s",(yyvsp[(2) - (2)].na));}
    break;

  case 326:
/* Line 1807 of yacc.c  */
#line 1236 "fortran.y"
    {  sprintf((yyval.na),"%s :",(yyvsp[(1) - (2)].na));}
    break;

  case 327:
/* Line 1807 of yacc.c  */
#line 1237 "fortran.y"
    {  sprintf((yyval.na),":");}
    break;

  case 328:
/* Line 1807 of yacc.c  */
#line 1240 "fortran.y"
    {
            if ( afterpercent == 0 )
            {
                if ( Agrif_in_Tok_NAME((yyvsp[(1) - (1)].na)) ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp((yyvsp[(1) - (1)].na),"Agrif_Parent") )   agrif_parentcall = 1;
                if ( VariableIsFunction((yyvsp[(1) - (1)].na)) )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp((yyvsp[(1) - (1)].na),identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,(yyvsp[(1) - (1)].na));
                    pointedvar = 0;

                    if (variscoupled_0((yyvsp[(1) - (1)].na))) strcpy(truename, getcoupledname_0((yyvsp[(1) - (1)].na)));
                    else                    strcpy(truename, (yyvsp[(1) - (1)].na));

                    if ( VarIsNonGridDepend(truename) == 0 && (! Variableshouldberemoved(truename)) )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,(yyvsp[(1) - (1)].na)) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen((yyvsp[(1) - (1)].na)));
                            }
                        }
                        if ( inagrifcallargument != 1 || sameagrifargument ==1 )
                        {
                            Add_UsedInSubroutine_Var_1(truename);
                        }
                    }
                    NotifyAgrifFunction_0(truename);
                }
            }
            else
            {
                afterpercent = 0;
            }
        }
    break;

  case 329:
/* Line 1807 of yacc.c  */
#line 1285 "fortran.y"
    { strcpy((yyval.na),".TRUE.");}
    break;

  case 330:
/* Line 1807 of yacc.c  */
#line 1286 "fortran.y"
    { strcpy((yyval.na),".FALSE.");}
    break;

  case 331:
/* Line 1807 of yacc.c  */
#line 1287 "fortran.y"
    { strcpy((yyval.na),"NULL()"); }
    break;

  case 332:
/* Line 1807 of yacc.c  */
#line 1288 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 333:
/* Line 1807 of yacc.c  */
#line 1289 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 334:
/* Line 1807 of yacc.c  */
#line 1290 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 335:
/* Line 1807 of yacc.c  */
#line 1292 "fortran.y"
    { sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 337:
/* Line 1807 of yacc.c  */
#line 1296 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 339:
/* Line 1807 of yacc.c  */
#line 1298 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 340:
/* Line 1807 of yacc.c  */
#line 1299 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 341:
/* Line 1807 of yacc.c  */
#line 1301 "fortran.y"
    { strcpy((yyval.na)," ");}
    break;

  case 342:
/* Line 1807 of yacc.c  */
#line 1302 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 343:
/* Line 1807 of yacc.c  */
#line 1305 "fortran.y"
    { sprintf((yyval.na),"(%s :%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));}
    break;

  case 344:
/* Line 1807 of yacc.c  */
#line 1307 "fortran.y"
    { strcpy((yyval.na)," ");}
    break;

  case 345:
/* Line 1807 of yacc.c  */
#line 1308 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 346:
/* Line 1807 of yacc.c  */
#line 1311 "fortran.y"
    { strcpy((yyval.na)," ");}
    break;

  case 347:
/* Line 1807 of yacc.c  */
#line 1312 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 348:
/* Line 1807 of yacc.c  */
#line 1314 "fortran.y"
    { InitialValueGiven = 0; }
    break;

  case 349:
/* Line 1807 of yacc.c  */
#line 1316 "fortran.y"
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[(2) - (2)].na));
            InitialValueGiven = 1;
        }
    break;

  case 350:
/* Line 1807 of yacc.c  */
#line 1322 "fortran.y"
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[(2) - (2)].na));
            InitialValueGiven = 2;
        }
    break;

  case 351:
/* Line 1807 of yacc.c  */
#line 1329 "fortran.y"
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 352:
/* Line 1807 of yacc.c  */
#line 1333 "fortran.y"
    {
            /* if variables has been declared in a subroutine       */
            sprintf(charusemodule, "%s", (yyvsp[(2) - (2)].na));
            if ( firstpass )
            {
                Add_NameOfModuleUsed_1((yyvsp[(2) - (2)].na));
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuse_0((yyvsp[(2) - (2)].na));

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
                }
            }
        }
    break;

  case 353:
/* Line 1807 of yacc.c  */
#line 1353 "fortran.y"
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    Add_CouplePointed_Var_1((yyvsp[(2) - (4)].na),(yyvsp[(4) - (4)].lc));
                    coupletmp = (yyvsp[(4) - (4)].lc);
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne, coupletmp->c_namevar);
                        strcat(ligne, " => ");
                        strcat(ligne, coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                    sprintf(charusemodule,"%s",(yyvsp[(2) - (4)].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[(2) - (4)].na));
            }
            if ( inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
            }
        }
    break;

  case 354:
/* Line 1807 of yacc.c  */
#line 1380 "fortran.y"
    {
            /* if variables has been declared in a subroutine       */
            sprintf(charusemodule,"%s",(yyvsp[(2) - (6)].na));
            if ( firstpass )
            {
                Add_NameOfModuleUsed_1((yyvsp[(2) - (6)].na));
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuseonly_0((yyvsp[(2) - (6)].na));

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
                }
            }
        }
    break;

  case 355:
/* Line 1807 of yacc.c  */
#line 1400 "fortran.y"
    {
            /* if variables has been declared in a subroutine      */
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    Add_CouplePointed_Var_1((yyvsp[(2) - (6)].na),(yyvsp[(6) - (6)].lc));
                    coupletmp = (yyvsp[(6) - (6)].lc);
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne,coupletmp->c_namevar);
                        if ( strcasecmp(coupletmp->c_namepointedvar,"") )   strcat(ligne," => ");
                        strcat(ligne,coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                    sprintf(charusemodule,"%s",(yyvsp[(2) - (6)].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[(2) - (6)].na));
            }
            else /* if ( firstpass == 0 ) */
            {
                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curuse,pos_end-pos_curuse);
                    if (oldfortran_out)  variableisglobalinmodule((yyvsp[(6) - (6)].lc),(yyvsp[(2) - (6)].na),oldfortran_out,pos_curuseold);
                }
                else
                {
                    /* if we are in the module declare and if the    */
                    /* onlylist is a list of global variable         */
                    variableisglobalinmodule((yyvsp[(6) - (6)].lc), (yyvsp[(2) - (6)].na), fortran_out,pos_curuse);
                }
            }
        }
    break;

  case 356:
/* Line 1807 of yacc.c  */
#line 1440 "fortran.y"
    {
            pos_curuse = setposcur()-strlen((yyvsp[(1) - (1)].na));
            if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);
        }
    break;

  case 357:
/* Line 1807 of yacc.c  */
#line 1447 "fortran.y"
    {
            (yyval.lc) = (yyvsp[(1) - (1)].lc);
        }
    break;

  case 358:
/* Line 1807 of yacc.c  */
#line 1451 "fortran.y"
    {
            /* insert the variable in the list $1                 */
            (yyvsp[(3) - (3)].lc)->suiv = (yyvsp[(1) - (3)].lc);
            (yyval.lc) = (yyvsp[(3) - (3)].lc);
        }
    break;

  case 359:
/* Line 1807 of yacc.c  */
#line 1458 "fortran.y"
    {
            coupletmp = (listcouple *) calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[(1) - (3)].na));
            strcpy(coupletmp->c_namepointedvar,(yyvsp[(3) - (3)].na));
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
    break;

  case 360:
/* Line 1807 of yacc.c  */
#line 1467 "fortran.y"
    {  (yyval.lc) = (yyvsp[(1) - (1)].lc); }
    break;

  case 361:
/* Line 1807 of yacc.c  */
#line 1469 "fortran.y"
    {
            /* insert the variable in the list $1                 */
            (yyvsp[(3) - (3)].lc)->suiv = (yyvsp[(1) - (3)].lc);
            (yyval.lc) = (yyvsp[(3) - (3)].lc);
        }
    break;

  case 362:
/* Line 1807 of yacc.c  */
#line 1477 "fortran.y"
    {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[(1) - (3)].na));
            strcpy(coupletmp->c_namepointedvar,(yyvsp[(3) - (3)].na));
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
            pointedvar = 1;
            Add_UsedInSubroutine_Var_1((yyvsp[(1) - (3)].na));
        }
    break;

  case 363:
/* Line 1807 of yacc.c  */
#line 1487 "fortran.y"
    {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[(1) - (1)].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
    break;

  case 380:
/* Line 1807 of yacc.c  */
#line 1522 "fortran.y"
    { inallocate = 0; }
    break;

  case 381:
/* Line 1807 of yacc.c  */
#line 1523 "fortran.y"
    { inallocate = 0; }
    break;

  case 388:
/* Line 1807 of yacc.c  */
#line 1531 "fortran.y"
    {
            GlobalDeclaration = 0 ;
            if ( firstpass == 0 && strcasecmp(subroutinename,"") )
            {
                if ( module_declar && insubroutinedeclare == 0 )    fclose(module_declar);
            }
            if ( strcasecmp(subroutinename,"") )
            {
                if ( inmodulemeet == 1 )
                {
                    /* we are in a module                                */
                    if ( insubroutinedeclare == 1 )
                    {
                        /* it is like an end subroutine <name>            */
                        insubroutinedeclare = 0 ;
                        pos_cur = setposcur();
                        closeandcallsubloopandincludeit_0(1);
                        functiondeclarationisdone = 0;
                    }
                    else
                    {
                        /* it is like an end module <name>                */
                        inmoduledeclare = 0 ;
                        inmodulemeet = 0 ;
                    }
                }
                else
                {
                    insubroutinedeclare = 0;
                    pos_cur = setposcur();
                    closeandcallsubloopandincludeit_0(2);
                    functiondeclarationisdone = 0;
                }
            }
            strcpy(subroutinename,"");
        }
    break;

  case 389:
/* Line 1807 of yacc.c  */
#line 1568 "fortran.y"
    {
            insubroutinedeclare = 0;
            inprogramdeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(3);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");
        }
    break;

  case 390:
/* Line 1807 of yacc.c  */
#line 1577 "fortran.y"
    {
            if ( strcasecmp(subroutinename,"") )
            {
                insubroutinedeclare = 0;
                pos_cur = setposcur();
                closeandcallsubloopandincludeit_0(1);
                functiondeclarationisdone = 0;
                strcpy(subroutinename,"");
            }
        }
    break;

  case 391:
/* Line 1807 of yacc.c  */
#line 1588 "fortran.y"
    {
            insubroutinedeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(0);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");
        }
    break;

  case 392:
/* Line 1807 of yacc.c  */
#line 1596 "fortran.y"
    {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, strlen((yyvsp[(2) - (2)].na))+11);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
    break;

  case 395:
/* Line 1807 of yacc.c  */
#line 1620 "fortran.y"
    {
            if ( inside_type_declare ) break;
            if ( inmoduledeclare )
            {
                if ( firstpass == 0 )
                {
                    RemoveWordCUR_0(fortran_out,9);   // Remove word 'contains'
                    Write_Closing_Module(0);
                }
                inmoduledeclare = 0 ;
                aftercontainsdeclare = 1;
            }
            else if ( insubroutinedeclare )
            {
                incontainssubroutine = 1;
                insubroutinedeclare  = 0;
                incontainssubroutine = 0;
                functiondeclarationisdone = 0;

                if ( firstpass )
                    List_ContainsSubroutine = Addtolistnom(subroutinename, List_ContainsSubroutine, 0);
                else
                    closeandcallsubloop_contains_0();

                strcpy(subroutinename, "");
            }
            else printf("l.%4d -- TOK_CONTAINS -- MHCHECK\n",line_num_input);
        }
    break;

  case 483:
/* Line 1807 of yacc.c  */
#line 1898 "fortran.y"
    {
            strcpy((yyval.na),(yyvsp[(1) - (1)].na));
            pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].na));
            functiondeclarationisdone = 0;
        }
    break;

  case 484:
/* Line 1807 of yacc.c  */
#line 1906 "fortran.y"
    {
            strcpy((yyval.na),(yyvsp[(1) - (1)].na));
            pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].na));
        }
    break;

  case 485:
/* Line 1807 of yacc.c  */
#line 1913 "fortran.y"
    {
            strcpy((yyval.na),(yyvsp[(1) - (1)].na));
            pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].na));
        }
    break;

  case 486:
/* Line 1807 of yacc.c  */
#line 1920 "fortran.y"
    {
            strcpy((yyval.na),(yyvsp[(1) - (1)].na));
            pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].na));
        }
    break;

  case 487:
/* Line 1807 of yacc.c  */
#line 1926 "fortran.y"
    {strcpy((yyval.na),"");}
    break;

  case 488:
/* Line 1807 of yacc.c  */
#line 1927 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 489:
/* Line 1807 of yacc.c  */
#line 1930 "fortran.y"
    { created_dimensionlist = 0; }
    break;

  case 490:
/* Line 1807 of yacc.c  */
#line 1934 "fortran.y"
    {
            created_dimensionlist = 1;
            if ( ((yyvsp[(3) - (4)].d) == NULL) || ((yyvsp[(4) - (4)].d) == NULL) ) break;
            if  ( agrif_parentcall == 1 )
            {
                ModifyTheAgrifFunction_0((yyvsp[(3) - (4)].d)->dim.last);
                agrif_parentcall = 0;
                fprintf(fortran_out," = ");
            }
        }
    break;

  case 491:
/* Line 1807 of yacc.c  */
#line 1945 "fortran.y"
    {
            created_dimensionlist = 1;
        }
    break;

  case 496:
/* Line 1807 of yacc.c  */
#line 1958 "fortran.y"
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
    break;

  case 502:
/* Line 1807 of yacc.c  */
#line 1979 "fortran.y"
    {
            if (!strcasecmp((yyvsp[(2) - (2)].na),"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp((yyvsp[(2) - (2)].na),"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber((yyvsp[(2) - (2)].na)) == 1 )
            {
                incalldeclare = 1;
                inagrifcallargument = 1 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
    break;

  case 503:
/* Line 1807 of yacc.c  */
#line 2000 "fortran.y"
    { pos_curcall=setposcur()-4; }
    break;

  case 506:
/* Line 1807 of yacc.c  */
#line 2008 "fortran.y"
    {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[(1) - (1)].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
    break;

  case 532:
/* Line 1807 of yacc.c  */
#line 2056 "fortran.y"
    { afterpercent = 1; }
    break;

  case 572:
/* Line 1807 of yacc.c  */
#line 2115 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 573:
/* Line 1807 of yacc.c  */
#line 2116 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 574:
/* Line 1807 of yacc.c  */
#line 2117 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 575:
/* Line 1807 of yacc.c  */
#line 2118 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 576:
/* Line 1807 of yacc.c  */
#line 2119 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 577:
/* Line 1807 of yacc.c  */
#line 2120 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 578:
/* Line 1807 of yacc.c  */
#line 2121 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 579:
/* Line 1807 of yacc.c  */
#line 2122 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 580:
/* Line 1807 of yacc.c  */
#line 2123 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 581:
/* Line 1807 of yacc.c  */
#line 2124 "fortran.y"
    { sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 582:
/* Line 1807 of yacc.c  */
#line 2127 "fortran.y"
    { sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 583:
/* Line 1807 of yacc.c  */
#line 2128 "fortran.y"
    { sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 584:
/* Line 1807 of yacc.c  */
#line 2129 "fortran.y"
    { sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 585:
/* Line 1807 of yacc.c  */
#line 2131 "fortran.y"
    { sprintf((yyval.na),"%s=%s,%s)",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));}
    break;

  case 586:
/* Line 1807 of yacc.c  */
#line 2132 "fortran.y"
    { sprintf((yyval.na),"%s=%s,%s,%s)",(yyvsp[(1) - (7)].na),(yyvsp[(3) - (7)].na),(yyvsp[(5) - (7)].na),(yyvsp[(7) - (7)].na));}
    break;

  case 591:
/* Line 1807 of yacc.c  */
#line 2142 "fortran.y"
    { Add_Allocate_Var_1((yyvsp[(1) - (1)].na),curmodulename); }
    break;


/* Line 1807 of yacc.c  */
#line 6110 "fortran.tab.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


/* Line 2055 of yacc.c  */
#line 2156 "fortran.y"


void process_fortran(const char *input_file)
{
    extern FILE *fortran_in;
    extern FILE *fortran_out;

    char output_file[LONG_FNAME];
    char input_fullpath[LONG_FNAME];

    if ( todebug == 1 ) printf("Firstpass == %d \n", firstpass);

     yydebug=0;
/******************************************************************************/
/*  1-  Open input file                                                       */
/******************************************************************************/

    strcpy(cur_filename, input_file);
    sprintf(input_fullpath, "%s/%s", input_dir, input_file);

    fortran_in = fopen(input_fullpath, "r");
    if (! fortran_in)
    {
        printf("Error : File %s does not exist\n", input_fullpath);
        exit(1);
    }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

    line_num_input = 1;
    PublicDeclare = 0;
    PrivateDeclare = 0;
    ExternalDeclare = 0;
    SaveDeclare = 0;
    pointerdeclare = 0;
    optionaldeclare = 0;
    incalldeclare = 0;
    inside_type_declare = 0;
    Allocatabledeclare = 0 ;
    Targetdeclare = 0 ;
    VariableIsParameter =  0 ;
    strcpy(NamePrecision,"");
    c_star = 0 ;
    functiondeclarationisdone = 0;
    insubroutinedeclare = 0 ;
    strcpy(subroutinename," ");
    isrecursive = 0;
    InitialValueGiven = 0 ;
    GlobalDeclarationType = 0;
    inmoduledeclare = 0;
    incontainssubroutine = 0;
    afterpercent = 0;
    aftercontainsdeclare = 1;
    strcpy(nameinttypename,"");

/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/

    sprintf(output_file, "%s/%s", output_dir, input_file);

    if (firstpass == 0) fortran_out = fopen(output_file,"w");

    fortran_parse();

    if (firstpass == 0) NewModule_Creation_0();
    if (firstpass == 0) fclose(fortran_out);
}
#line 2 "fortran.yy.c"

#line 4 "fortran.yy.c"

#define  YY_INT_ALIGNED short int

/* A lexical scanner generated by flex */

#define yy_create_buffer fortran__create_buffer
#define yy_delete_buffer fortran__delete_buffer
#define yy_flex_debug fortran__flex_debug
#define yy_init_buffer fortran__init_buffer
#define yy_flush_buffer fortran__flush_buffer
#define yy_load_buffer_state fortran__load_buffer_state
#define yy_switch_to_buffer fortran__switch_to_buffer
#define yyin fortran_in
#define yyleng fortran_leng
#define yylex fortran_lex
#define yylineno fortran_lineno
#define yyout fortran_out
#define yyrestart fortran_restart
#define yytext fortran_text
#define yywrap fortran_wrap
#define yyalloc fortran_alloc
#define yyrealloc fortran_realloc
#define yyfree fortran_free

#define FLEX_SCANNER
#define YY_FLEX_MAJOR_VERSION 2
#define YY_FLEX_MINOR_VERSION 5
#define YY_FLEX_SUBMINOR_VERSION 35
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

/* First, we deal with  platform-specific or compiler-specific issues. */

/* begin standard C headers. */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

/* end standard C headers. */

/* flex integer type definitions */

#ifndef FLEXINT_H
#define FLEXINT_H

/* C99 systems have <inttypes.h>. Non-C99 systems may or may not. */

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#include <inttypes.h>
typedef int8_t flex_int8_t;
typedef uint8_t flex_uint8_t;
typedef int16_t flex_int16_t;
typedef uint16_t flex_uint16_t;
typedef int32_t flex_int32_t;
typedef uint32_t flex_uint32_t;
typedef uint64_t flex_uint64_t;
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;
#endif /* ! C99 */

/* Limits of integral types. */
#ifndef INT8_MIN
#define INT8_MIN               (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN              (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN              (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX               (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX              (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX              (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX              (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX             (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX             (4294967295U)
#endif

#endif /* ! FLEXINT_H */

#ifdef __cplusplus

/* The "const" storage-class-modifier is valid. */
#define YY_USE_CONST

#else	/* ! __cplusplus */

/* C99 requires __STDC__ to be defined as 1. */
#if defined (__STDC__)

#define YY_USE_CONST

#endif	/* defined (__STDC__) */
#endif	/* ! __cplusplus */

#ifdef YY_USE_CONST
#define yyconst const
#else
#define yyconst
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an unsigned
 * integer for use as an array index.  If the signed char is negative,
 * we want to instead treat it as an 8-bit unsigned char, hence the
 * double cast.
 */
#define YY_SC_TO_UI(c) ((unsigned int) (unsigned char) c)

/* Enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN.
 */
#define BEGIN (yy_start) = 1 + 2 *

/* Translate the current start state into a value that can be later handed
 * to BEGIN to return to the state.  The YYSTATE alias is for lex
 * compatibility.
 */
#define YY_START (((yy_start) - 1) / 2)
#define YYSTATE YY_START

/* Action number for EOF rule of a given start state. */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)

/* Special action meaning "start processing a new file". */
#define YY_NEW_FILE fortran_restart(fortran_in  )

#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

/* The state buf must be large enough to hold one state per character in the main buffer.
 */
#define YY_STATE_BUF_SIZE   ((YY_BUF_SIZE + 2) * sizeof(yy_state_type))

#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

extern yy_size_t fortran_leng;

extern FILE *fortran_in, *fortran_out;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2

    #define YY_LESS_LINENO(n)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up fortran_text again */ \
		} \
	while ( 0 )

#define unput(c) yyunput( c, (yytext_ptr)  )

#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	yy_size_t yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	yy_size_t yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */
    
	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via fortran_restart()), so that the user can continue scanning by
	 * just pointing fortran_in at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = 0; /**< Stack as an array. */

/* We provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state".
 *
 * Returns the top of the stack, or NULL.
 */
#define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                          ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                          : NULL)

/* Same as previous macro, but useful when we know that the buffer stack is not
 * NULL or when we need an lvalue. For internal use only.
 */
#define YY_CURRENT_BUFFER_LVALUE (yy_buffer_stack)[(yy_buffer_stack_top)]

/* yy_hold_char holds the character lost when fortran_text is formed. */
static char yy_hold_char;
static yy_size_t yy_n_chars;		/* number of characters read into yy_ch_buf */
yy_size_t fortran_leng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = (char *) 0;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow fortran_wrap()'s to do buffer switches
 * instead of setting up a fresh fortran_in.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void fortran_restart (FILE *input_file  );
void fortran__switch_to_buffer (YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE fortran__create_buffer (FILE *file,int size  );
void fortran__delete_buffer (YY_BUFFER_STATE b  );
void fortran__flush_buffer (YY_BUFFER_STATE b  );
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer  );
void fortran_pop_buffer_state (void );

static void fortran_ensure_buffer_stack (void );
static void fortran__load_buffer_state (void );
static void fortran__init_buffer (YY_BUFFER_STATE b,FILE *file  );

#define YY_FLUSH_BUFFER fortran__flush_buffer(YY_CURRENT_BUFFER )

YY_BUFFER_STATE fortran__scan_buffer (char *base,yy_size_t size  );
YY_BUFFER_STATE fortran__scan_string (yyconst char *yy_str  );
YY_BUFFER_STATE fortran__scan_bytes (yyconst char *bytes,yy_size_t len  );

void *fortran_alloc (yy_size_t  );
void *fortran_realloc (void *,yy_size_t  );
void fortran_free (void *  );

#define yy_new_buffer fortran__create_buffer

#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}

#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}

#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

#define fortran_wrap(n) 1
#define YY_SKIP_YYWRAP

typedef unsigned char YY_CHAR;

FILE *fortran_in = (FILE *) 0, *fortran_out = (FILE *) 0;

typedef int yy_state_type;

extern int fortran_lineno;

int fortran_lineno = 1;

extern char *fortran_text;
#define yytext_ptr fortran_text

static yy_state_type yy_get_previous_state (void );
static yy_state_type yy_try_NUL_trans (yy_state_type current_state  );
static int yy_get_next_buffer (void );
static void yy_fatal_error (yyconst char msg[]  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up fortran_text.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	fortran_leng = (yy_size_t) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;

#define YY_NUM_RULES 176
#define YY_END_OF_BUFFER 177
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static yyconst flex_int16_t yy_accept[1132] =
    {   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  177,  176,  166,  164,  175,  176,  155,  158,
      176,  176,  157,  157,  157,  160,  156,  144,  154,  157,
      159,  162,  161,  163,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  166,  164,  166,  175,  154,  151,
      151,  151,  151,  151,  151,  176,  176,  172,  176,  176,
      176,  157,  151,    0,    0,  166,    0,    0,  175,  175,
      175,    0,  148,    0,    0,    0,  168,    0,    0,    0,
        0,    0,  147,    0,    0,  141,   25,    0,  153,    0,

        0,    0,    0,    0,    0,    0,  142,    0,  154,    0,
      140,   23,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,   42,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  100,  151,   89,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,    0,  166,  166,    0,  167,    0,
        0,    0,    0,    0,    0,  165,  166,    0,  175,  174,
      175,  175,  175,  167,  154,    0,  151,  151,  151,  151,

       89,  151,  151,    0,  172,    0,    0,    0,    0,    0,
        0,  173,   25,    0,    0,    0,  151,  151,  151,  151,
      151,    0,    0,    0,  175,  175,    0,    0,    0,    0,
        0,    0,    0,    0,  146,    0,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,  152,  152,
        0,  153,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  123,  151,  151,  151,
      151,  151,  151,  151,   14,  151,  151,  151,  151,  122,
      151,  151,  151,  151,  151,  151,  151,    0,  151,  151,
      151,  151,  151,  151,  129,  151,  151,  134,  151,  151,

      151,  151,  151,  151,  151,  151,   93,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  126,  151,  151,  151,  151,  151,  130,  151,  151,
      151,  151,  151,  151,  151,    0,  166,  166,    0,    0,
        0,    0,    0,    0,    0,    0,    0,  166,    0,  167,
      175,  175,  175,  154,    0,  151,  151,  151,  151,  151,
      151,  151,    0,    0,    0,    0,  173,    0,    0,    0,
      151,  151,  151,  151,  151,    0,    0,    0,  175,  175,
        0,    0,    0,    0,    0,    0,    0,    0,  153,    0,
       27,    0,   29,   28,   31,   30,   33,    0,    0,   35,

        0,    0,  133,  125,  151,  151,  128,  151,  131,  151,
      151,   20,  151,  151,  151,  151,  151,  151,  124,  151,
      151,  151,  151,  151,  151,   98,    0,  115,  151,  151,
      151,  151,  151,  151,  151,  151,    0,  116,  151,    0,
      117,  151,  151,  151,  151,  151,  151,    0,  113,  151,
      151,    0,   92,  151,  151,  151,  151,  151,  151,  151,
        0,  102,  151,  151,    0,  119,  151,  151,  151,  151,
      120,    0,  114,   19,  151,   63,   77,  151,  151,  151,
      151,  151,  151,  151,  151,   82,   43,  151,  151,  151,
      151,   72,  151,  151,  127,  151,   76,   57,  151,    0,

      101,  103,  151,   96,  105,  151,  151,  151,  151,   47,
      166,  166,    0,    0,    0,    0,    0,    0,    0,  166,
        0,  167,  175,  175,  175,  154,    0,  108,  151,  151,
      151,  151,  151,   16,    0,    0,    0,    0,    0,    0,
      151,  151,  151,  151,    0,    0,    0,  175,  175,    0,
        0,    0,    0,    0,    0,   37,   26,    0,   34,   36,
      151,  151,  151,  151,  151,  151,   52,  151,  151,  151,
      151,  132,  151,  151,  151,  151,  151,    0,  151,  151,
        0,    0,    0,    0,    0,    0,    0,    0,   41,  151,
       99,  151,  151,  151,  151,  151,  151,  151,  151,   79,

       79,  151,    0,  111,  121,   85,  151,  151,   92,  151,
      151,   94,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,    0,    0,  151,  151,  151,   55,  151,
       80,  151,  151,  151,    0,  151,  151,  151,  151,  151,
        0,  135,  106,  151,  151,    0,  112,   58,   39,   84,
      166,  166,  108,    0,    0,    0,    0,    0,  166,    0,
      167,  175,  175,  175,  154,    0,  108,  151,   90,  151,
      151,   74,   73,   74,    0,    0,    0,    0,    0,  151,
       52,  151,  132,    0,   21,    0,  175,   21,    0,   21,
       21,    0,   21,    0,   21,   21,   21,   32,  151,  151,

      151,   21,  151,  151,   66,  151,  151,  151,  151,  151,
      151,  151,  145,    0,    0,   97,  151,   41,    0,   99,
        0,    0,    0,    0,    0,    0,  151,  151,  151,  151,
      151,  151,  151,  151,    0,  118,  151,  151,  151,  151,
      151,  151,  151,   69,  151,  151,  137,  104,  136,  138,
       38,  151,    0,    6,  151,  151,  151,  151,  151,  151,
       87,    0,  151,    8,   78,   17,  151,  151,   86,  166,
      166,    0,    0,    0,  166,  175,  175,   21,    0,  151,
      151,  151,    0,    0,    0,   21,    0,  151,   21,   22,
        0,  169,   22,   22,   22,   22,   22,   22,   22,   22,

      151,  151,  151,  151,   50,  151,  151,  151,  109,  151,
        0,  151,  151,   97,    0,  151,    0,    0,    0,    0,
        0,    0,    0,  151,  151,  151,  151,  151,   75,  151,
      151,  151,    0,    0,  151,  151,   15,   53,   44,  151,
       45,    0,  151,  151,    5,  151,  151,   70,   88,    3,
        0,    0,  151,    0,  151,  151,    0,    0,    0,  175,
       22,    0,  151,   67,  151,    0,    0,   22,    0,   22,
      151,    4,  151,  151,  151,  151,   91,  151,  151,    0,
        0,  151,  151,    0,  151,    0,    0,    0,    0,    0,
       75,    0,  151,  151,  151,  151,  151,   59,  151,   68,

        0,    0,    0,    0,  143,    9,   18,  151,    0,  151,
       83,   71,  151,    0,  151,    0,  151,  151,    0,    0,
      175,    0,   62,  151,    0,    0,    0,  151,  151,  139,
       46,  151,  151,   54,    0,    0,  151,  151,    0,   61,
        0,    0,    0,    0,    0,   59,  151,   11,  151,  110,
      151,  151,    0,    0,    0,    0,    0,  143,   95,    0,
      151,   64,    0,   65,    0,  151,  151,   62,    0,  175,
        0,  149,    0,    0,    0,  151,  151,   40,    7,    0,
        0,  151,  151,   61,    0,   60,    0,   11,    0,  110,
        0,  151,   10,  151,  151,    0,    0,    0,  151,    0,

        0,  107,    2,  149,  175,    0,    0,    0,    0,   51,
        0,    0,    0,    0,  151,  151,    0,   10,    0,   13,
      151,   56,    0,    0,    0,  151,    0,  107,  175,    0,
        0,    0,    0,    0,    0,    0,    0,  151,  151,   13,
        0,  151,    0,    0,    0,  151,    0,  175,    0,    0,
        0,    0,    0,   24,    0,    0,   49,  151,    0,   12,
        0,    0,    0,  151,    0,  175,    0,    0,    0,  150,
        0,   49,    0,  151,   12,    0,    0,    0,    0,  151,
        0,  175,    0,    0,    0,    0,   48,    0,    0,    0,
       81,    1,  175,    0,    0,    0,   48,   81,  175,    0,

        0,    0,  175,    0,    0,    0,  175,    0,    0,    0,
      175,    0,    0,    0,  175,    0,    0,    0,  175,  170,
        0,  170,    0,    0,  170,    0,    0,    0,    0,  171,
        0
    } ;

static yyconst flex_int32_t yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
       23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
       33,   34,    1,    1,   35,   36,   37,   38,   39,   40,
       41,   42,   43,   44,   45,   46,   47,   48,   49,   50,
       51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
       61,    1,   62,    1,   63,    1,   64,   65,   66,   67,

       68,   69,   70,   71,   72,   44,   73,   74,   75,   76,
       77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
       87,   88,    1,   89,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static yyconst flex_int32_t yy_meta[90] =
    {   0,
        1,    1,    2,    1,    1,    3,    1,    1,    1,    1,
        1,    4,    1,    1,    1,    1,    1,    3,    1,    5,
        5,    5,    5,    5,    5,    5,    5,    5,    5,    1,
        1,    1,    1,    1,    6,    7,    7,    5,    5,    8,
        9,   10,   11,   11,   11,    9,   11,    9,   12,   11,
       13,    9,    9,    9,    9,   11,   11,   11,   11,   11,
        1,    1,   11,    6,    7,    7,    5,    5,    8,    9,
       10,   11,   11,    9,   11,    9,   12,   11,   13,    9,
        9,    9,    9,   11,   11,   11,   11,   11,    3
    } ;

static yyconst flex_int16_t yy_base[1171] =
    {   0,
        0,   88,    0,    0,    0,    0,  984,   93,    0,   85,
        0,    0,  891,   64,   98,  103,   80,  129,   96,   99,
      134,  137,  145,  133,  168,  135,  249,  180,  318,  136,
      156,  172,  212,  239,  246,  297,  366,  341,  414,  461,
      247,  294,  398,  334,  407,  476,  506,  517,  459,  566,
      572,  522,  571,  367,  653,  226,  656,  658,  737,  638,
      785,  655,  698,  806,  652, 4012,  878, 4012,  241,  116,
      124,  455,  893,   59,   76,  229,  253,  256,    0,  122,
      127,  872, 4012,  157,  188,  869,  866,  327,  302,  349,
      740,  416, 4012,  768,  592, 4012, 4012,  981,  730,  147,

      184,  438,  495,  328,  184,  190, 4012, 1048,  335,  969,
     4012, 4012,    0,  269,  315,  149,  319,  486,  462,  248,
      318,  325,  269,  426,  736,  336,  767,  470,  500,  518,
      512,  403,  516,  525,  535,  801,  544,  620,  565,  570,
      589,  768,  483,  589,  810,  592,  613,  638,  650,  664,
      678,  736,  737,  740,  741,  949,  736,  788,  646,  956,
      756,  999,  739,  753,  804,  749, 1043,  972,  962,  769,
      813,  963,  972,  969,    0, 1057,    0,  839,  238, 1044,
      986,  789,  956, 1030,  965,  429, 1109, 1136,  830, 4012,
     1061, 1087, 1091, 1162, 1188,  990, 1095, 1120, 1006, 1042,

     1170, 1050, 1046,  823, 4012, 1201, 1099, 1115, 1190,  797,
      483, 4012,  790, 1167, 1100, 1194, 1269, 1358, 1255, 1254,
     1195, 1103, 1098,  625, 1148, 1163, 1217, 1237, 1222, 1238,
     1095, 1305, 1325, 1234, 4012, 1334, 1302, 1326, 1107, 1182,
      738,  735,  731,  730, 1244, 1166,  713, 1242, 4012, 1428,
     1374, 1389, 1419, 1244, 1252, 1261, 1263, 1192, 1266, 1301,
     1333, 1333, 1352, 1352, 1387, 1392, 1388, 1389, 1390, 1404,
     1401, 1405, 1407, 1404, 1506, 1471, 1417, 1476, 1430,    0,
     1422, 1437, 1442, 1432, 1484, 1440, 1454, 1497, 1444, 1448,
     1451, 1443, 1445, 1475, 1459, 1530, 1536, 1464, 1477, 1470,

     1531, 1490, 1491, 1482, 1509, 1511,    0, 1526, 1513, 1519,
     1524, 1527, 1524, 1555, 1516, 1519, 1522, 1546, 1557, 1564,
     1558, 1565, 1555, 1557, 1562, 1561, 1615, 1573, 1575, 1573,
     1576, 1570, 1574, 1582, 1576,  719,  351,  666, 1572,  713,
     1580, 1587, 1584, 1593, 1589, 1596, 1608, 1658, 1684, 1710,
     1652, 1645, 1661, 1741, 1626, 1629, 1635, 1634, 1637, 1703,
     1653,  709, 1654, 1744,  707,  998, 4012, 1712, 1674, 1715,
     1664, 1708, 1725, 1738, 1749, 1622, 1684, 1379, 1672, 1735,
     1743, 1771, 1723, 1772, 1787, 1804, 1815, 1807, 1821,  656,
     4012,  646, 4012, 4012, 4012, 4012, 4012, 1735,  614, 4012,

      613, 1849, 4012,    0, 1741, 1751,    0, 1796,    0, 1799,
     1799,    0, 1820, 1819, 1811, 1816, 1808, 1821,    0, 1826,
     1822, 1814, 1822, 1825, 1819, 1871, 1955, 4012, 1825, 1821,
     1837, 1830, 1829, 1850, 1823, 1844, 1899, 4012, 1836, 1909,
     4012, 1841, 2040, 1856, 1915, 1858, 1874, 1920, 4012, 1886,
     1872, 1878,    0, 1886, 1879, 1881, 1888, 1896, 1895, 1902,
     1967, 4012, 1897, 1906, 1971, 4012, 1900, 1912, 1906, 1915,
        0, 1980, 4012,    0, 1988,    0,    0, 1929, 1939, 1926,
     1933, 1936, 1946, 1939, 1953,    0,  602, 1949, 1958, 1955,
     1963,    0, 1960, 2063,    0,  598,    0,    0, 1985, 2064,

     4012,    0, 1960,    0,    0, 2019, 2022, 2024, 2025, 4012,
      792,  880, 2030, 2023, 2043, 2043, 2041, 2042,  581, 2092,
     2118, 2144, 2079, 2080, 2084, 2170, 2045, 1005, 2060, 1022,
     2071, 2134, 2199,  574, 2082, 2174, 2108, 2147, 2148, 2101,
     2154, 2184, 2187, 2190, 2087,    0, 1422, 2144,    0, 2200,
      138, 2202,  571, 2221, 2233, 4012, 4012,  514, 4012, 4012,
     2174, 2192, 2051, 2289, 2080, 2193,    0, 2109, 2194, 2193,
     2199,    0, 2201, 2207, 2204, 2195, 2198, 2270, 2240, 2240,
     2215, 2229, 2246, 2239, 2258, 2275, 2258, 2276,    0, 2276,
        0, 2287, 2279, 2283, 2295, 2283, 2295, 2300, 2335, 4012,

        0, 2293, 2342, 4012,    0,    0, 2294, 2293, 4012, 2316,
     2321,    0, 2297, 2323, 2312, 2319, 2335, 2334, 2327, 2337,
     2333, 2341, 2338,  197,  451, 2343, 2337, 2347,    0, 2348,
        0, 2335, 2355, 2355,  477, 2343, 2343, 2350, 2355, 2348,
     2275, 4012, 4012, 2349, 2351, 2404, 4012,    0,    0,    0,
     1140, 1249, 1797, 2376, 2082, 2379, 2366, 2432, 2263,  424,
      569, 2417, 2420,  499, 2486, 2384, 2087, 2385, 2427, 2389,
     2398, 2437, 4012, 2448, 2444, 2439, 2452,  479, 2451, 2431,
     2410, 2565, 2416,    0, 4012, 2653,    0,    0,  475,  445,
     4012,  441,  436, 2458, 2462, 2468, 4012, 4012, 2682, 2404,

     2428,    0, 2441, 2444,    0, 2442, 2449, 2447, 2480, 2465,
     2479, 2553,    0, 2487, 2489,    0, 2493, 4012, 2486, 4012,
     2497, 2509, 2513, 2527, 2535, 2549, 2552, 2538, 2550, 2555,
     2548, 2562, 2552, 2559, 2604, 4012, 2572, 2480, 2561, 2568,
     2573, 2575, 2564,    0, 2569, 2578,    0,    0,    0,    0,
     2660, 2572,  508, 4012, 2583, 2594, 2591, 2650, 2664, 2658,
        0, 2536, 2663,    0,    0,    0, 2715, 2654,    0, 2539,
     2479, 2668, 2663, 2678, 2644, 2682,  426,  421, 2680, 2677,
     2540, 2686, 2701, 2720,  411,  365, 2721, 2790, 2691, 4012,
     2725, 4012,    0,  343, 4012,  307, 2753, 2755, 4012,    0,

     2747, 2692, 2702, 2706,    0, 2707, 2709, 2719,    0, 2685,
     2776, 2708, 2732, 4012, 2748, 2737, 2773, 2763, 2773, 2780,
     2773, 2788, 2777, 2776, 2785, 2780, 2796, 2786,    0, 2797,
     2798, 2794, 2647, 2867, 2796, 2791,    0,    0,    0, 2795,
        0, 2845, 2798, 2801,    0, 2815, 2825,    0,    0,    0,
     2872, 2840, 2825, 2883, 2847, 2843, 2840, 2548, 2855,  827,
      303,  239, 2841, 2765, 2855, 1020, 2888,  288, 1687, 2861,
     2861,    0, 2851, 2861, 2850, 2855,    0, 2851, 2858, 2858,
     2856, 2863, 2873, 2862, 2878, 2870, 2882, 2880, 2881, 2888,
     4012, 2904, 2901, 2906, 2913, 2885, 2895,    0, 2903,    0,

     2951, 2978, 2955, 2982, 2994,    0,    0, 2933, 2910, 2917,
        0,    0, 2923,  248, 2932, 2941, 2930, 2939, 2936, 2956,
     2998, 2964,    0, 2967, 3005, 3006, 3007, 2966, 2959,    0,
        0, 2960, 2979,    0, 2973, 2982, 2973, 2987, 2986, 3025,
     2983, 2989, 2995, 2978, 2978, 4012, 2985,    0, 2991,    0,
     2985, 3006, 3066, 3074, 3078, 3090, 3094, 3106,    0, 3009,
     3019,    0, 3041,    0, 3018, 3050, 3056, 4012, 3062, 3076,
     3056,    0, 3087, 3103, 3099, 3072, 3111,    0,    0, 3064,
     3080, 3075, 3079, 3124, 3125, 4012, 3082, 4012, 3085, 4012,
     3080, 3093,    0, 3096, 3104, 3152, 3171, 3113, 3118,  245,

     3119,    0,    0, 4012, 1737,  184, 3142, 3141, 1922,    0,
     3181, 3186, 3115, 3122, 3127, 3111, 3122, 4012, 3128,    0,
     3129,    0, 3208, 3211, 3152, 3158, 3150, 4012, 3194, 3154,
     2097, 3179, 3201, 3224, 3231, 3169, 3153, 3159, 3179, 4012,
     3183, 3184, 3251, 3258, 3191, 3183,  207, 3229, 3207, 3237,
      194, 3233, 3261, 4012, 3209, 3200,    0, 3220, 3211,    0,
     3309, 3283, 3218, 3228, 3221, 3285, 3236, 3273,  142, 4012,
     3286, 4012, 3243, 3250, 4012, 3339, 3342, 3367, 3248, 3286,
      108, 2482,   40, 2606, 2838, 3273,    0, 3395, 3399, 3290,
        0, 4012, 3302, 3294, 3311, 3350, 4012, 4012, 3299, 3320,

     3357, 3358, 3375, 3318, 3359, 3380, 3404, 3376, 3300, 3412,
     3362, 3363, 3370, 3415, 3418, 3423, 3420, 3426, 3433, 4012,
     3447, 4012, 3428, 3451, 4012, 3430, 3439, 3454, 3457, 4012,
     4012, 3521, 3534, 3547, 3560, 3573, 3586, 3595, 3608, 3621,
     3634, 3647, 3656, 3669, 3678, 3686, 3699, 3712, 3725, 3738,
     3751, 3764, 3777, 3790, 3803, 3816, 3829, 3842, 3855, 3868,
     3881, 3894, 3907, 3920, 3933, 3946, 3959, 3972, 3985, 3998
    } ;

static yyconst flex_int16_t yy_def[1171] =
    {   0,
     1131,    1, 1132, 1132,    1,    2, 1133, 1133,    1,    2,
        1,    2, 1131, 1131, 1131, 1131, 1134, 1135, 1131, 1131,
     1136, 1137, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138,   40, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1131, 1131,   55, 1139, 1131,   37,
     1138, 1138, 1138, 1138, 1138, 1131, 1140, 1131, 1140, 1140,
     1140, 1141, 1131, 1131, 1131, 1131, 1131, 1131, 1134, 1134,
     1134, 1135, 1131, 1135, 1135, 1136, 1131, 1136, 1136, 1137,
     1142, 1137, 1131, 1137, 1137, 1131, 1131, 1131, 1143, 1131,

     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1144,   29, 1131,
     1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1145,   55,  176, 1146, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131,  176, 1131, 1139, 1131,
     1139, 1139, 1139, 1131, 1131, 1131, 1138, 1138, 1138, 1138,

     1138, 1138, 1138, 1140, 1131, 1140, 1140, 1140, 1140, 1147,
     1147, 1131, 1147, 1147, 1147, 1147, 1148, 1148,  218,  218,
      218, 1131, 1131, 1131, 1134, 1134, 1135, 1135, 1136, 1136,
     1142, 1142, 1142, 1142, 1131, 1137, 1137, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1144,
     1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1131, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,

     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1145,  176,  176, 1131, 1146,
     1131, 1131, 1131, 1131, 1131, 1131, 1131,  187, 1131, 1131,
     1139, 1139, 1139, 1131, 1131, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1140, 1140, 1147, 1147, 1131, 1147, 1147, 1147,
      218,  218,  218,  218,  218, 1131, 1131, 1131, 1134, 1134,
     1135, 1135, 1136, 1136, 1142, 1137, 1137, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,

     1131, 1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1131, 1131, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1131, 1131, 1138, 1131,
     1131, 1138, 1149, 1138, 1138, 1138, 1138, 1131, 1131, 1138,
     1138, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1131, 1131, 1138, 1138, 1131, 1131, 1138, 1138, 1138, 1138,
     1138, 1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1131,

     1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1131,
      176,  176, 1131, 1131, 1131, 1131, 1131, 1131, 1131,  187,
     1131, 1131, 1139, 1139, 1139, 1131, 1131, 1138, 1138, 1138,
     1138, 1138, 1138, 1131, 1140, 1140, 1140, 1147, 1147, 1147,
      218,  218,  218,  218, 1131, 1150, 1131, 1134, 1151, 1135,
     1152, 1136, 1153, 1137, 1154, 1131, 1131, 1131, 1131, 1131,
     1138, 1138, 1138, 1155, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1131, 1138, 1138,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1131,

     1138, 1138, 1131, 1131, 1138, 1138, 1138, 1138, 1131, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1131, 1131, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1131, 1138, 1138, 1138, 1138, 1138,
     1131, 1131, 1131, 1138, 1138, 1131, 1131, 1138, 1138, 1138,
      176,  176, 1131, 1131, 1131, 1131, 1131, 1131,  176, 1131,
     1131, 1139, 1139, 1156, 1131, 1131, 1131, 1138, 1131, 1138,
     1138, 1131, 1131, 1131, 1140, 1140, 1147, 1157, 1147,  218,
      218, 1158,  218, 1159, 1131, 1131, 1160, 1134, 1161, 1135,
     1131, 1162, 1136, 1163, 1137, 1137, 1131, 1131, 1164, 1138,

     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1131, 1131, 1138, 1138, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1131, 1131, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138,  176,
      176, 1131, 1131, 1131,  176, 1139, 1165, 1139, 1131, 1138,
     1138, 1138, 1140, 1140, 1166, 1147, 1147, 1167,  218, 1131,
     1131, 1131, 1134, 1135, 1131, 1136, 1137, 1137, 1131, 1138,

     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1131, 1138, 1138, 1131, 1131, 1138, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1131, 1168, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1131, 1131, 1138, 1131, 1138, 1138, 1131, 1131, 1131, 1139,
     1139, 1131, 1138, 1131, 1138, 1140, 1140, 1147, 1147,  218,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1131,
     1131, 1138, 1138, 1131, 1138, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,

     1168, 1168, 1131, 1169, 1168, 1138, 1138, 1138, 1131, 1138,
     1138, 1138, 1138, 1131, 1138, 1131, 1138, 1138, 1131, 1131,
     1139, 1131, 1138, 1138, 1140, 1140, 1147, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1131, 1131, 1138, 1138, 1131, 1138,
     1131, 1131, 1131, 1131, 1131, 1131, 1138, 1138, 1138, 1138,
     1138, 1138, 1131, 1169, 1169, 1168, 1169, 1169, 1138, 1131,
     1138, 1138, 1131, 1138, 1131, 1138, 1138, 1131, 1131, 1139,
     1131, 1138, 1140, 1140, 1147, 1138, 1138, 1138, 1138, 1131,
     1131, 1138, 1138, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1138, 1138, 1138, 1138, 1131, 1168, 1131, 1138, 1131,

     1131, 1138, 1138, 1131, 1139, 1131, 1140, 1140, 1147, 1138,
     1131, 1131, 1131, 1131, 1138, 1138, 1131, 1131, 1131, 1138,
     1138, 1138, 1131, 1168, 1131, 1138, 1131, 1131, 1139, 1131,
     1140, 1140, 1147, 1131, 1131, 1131, 1131, 1138, 1138, 1131,
     1131, 1138, 1131, 1168, 1131, 1138, 1131, 1139, 1131, 1140,
     1170, 1147, 1131, 1131, 1131, 1131, 1138, 1138, 1131, 1138,
     1131, 1168, 1131, 1138, 1131, 1139, 1131, 1140, 1170, 1131,
     1147, 1131, 1131, 1138, 1131, 1131, 1168, 1168, 1131, 1138,
     1131, 1139, 1131, 1140, 1147, 1131, 1138, 1168, 1169, 1131,
     1138, 1131, 1139, 1131, 1140, 1147, 1131, 1131, 1139, 1131,

     1140, 1147, 1139, 1131, 1140, 1147, 1139, 1131, 1140, 1147,
     1139, 1131, 1140, 1147, 1139, 1131, 1140, 1147, 1139, 1131,
     1131, 1131, 1140, 1147, 1131, 1140, 1140, 1140, 1140, 1131,
        0, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131
    } ;

static yyconst flex_int16_t yy_nxt[4102] =
    {   0,
       14,   15,   16,   15,   17,   18,   14,   19,   20,   21,
       22,   23,   24,   25,   24,   26,   24,   27,   28,   29,
       29,   29,   29,   29,   29,   29,   29,   29,   29,   30,
       31,   32,   33,   34,   35,   36,   37,   38,   39,   40,
       41,   42,   43,   42,   42,   44,   45,   46,   47,   48,
       42,   49,   50,   51,   52,   42,   53,   42,   42,   54,
       24,   24,   42,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   42,   44,   45,   46,   47,   48,   42,   49,
       50,   51,   52,   42,   53,   42,   42,   54,   14,   55,
       56,   57,   58,  222,   69,   68,   69,   70,   72,   76,

       77,   76, 1094,   74,   78,   77,   78,   59,   59,   59,
       59,   59,   59,   59,   59,   59,   59,   75,  205,   80,
     1092,   73,  222,  208,   60,   61,  205,  223,   62,   70,
       63,   71,   74,   81,   83,   74,   87,   74,   74,   91,
       82,   64,   65,  691, 1070,   75,   92,   93,   80,   75,
       73,   75,   75,   60,   61,  223,  225,   62,   70,   63,
       71,   81,   83,   96,   74,  111,   74,   74,   84,   64,
       65,  209,   74,   88,   74,   74,   94,   75,  226,   75,
       75,   97,   85,  255,   74,  225,   75,   89,   75,   75,
       95,  227,  107,   83,  239,   74, 1070,   84,   75,  209,

      624,   74,   88,   74,   74,   94,  226,   74,  625,   75,
       85,   74,  255,   74,   75,   89,   75,   75,   95,   74,
      227,   75,  239, 1065,   74,   75,   75,   78,   77,   78,
       76,   77,   76,   75,  240,  247,   74,   75,  248,  228,
       74,  179,  206,  205,  206,  112, 1030,  179,   74,   75,
       98,   74,   98,   75,   78,   77,   78,  224,   77,  224,
     1027,   75,  240,  247,  963,   75,  248,  228,   99,   99,
       99,   99,   99,   99,   99,   99,   99,   99,   74,  207,
       74,  114,  115,  100,  261,  116,  116,  101,   74,  102,
      212,  117,   75,   75,  103,  142,  104,  105,  118,  119,

      121,  922,   75,  263,   87,  190,  106,   74,  207,   87,
      114,  115,  100,  261,  116,  116,  101,   74,  102,  117,
       75,  253,  103,  142,  104,  105,  118,  119,  121,   87,
       75,  120,  263,  116,  106,  108,  116,  109,  109,  109,
      109,  109,  109,  109,  109,  109,  109,  121,   83,  253,
      121,   91,  511,  230,  512,  110,  110,   74,   92,   93,
      120,  229,  116,  254,  256,  116,  245,  212,  110,  260,
      262,   75,  268,  116, 1131,  121,  246,  175,  121,  127,
      116,  230,  146,  128,  110,  110,   74,  121, 1131,  129,
      229,  254,  256,  130,  121,  245,  110,  260,  262,   75,

      122,  268,  116, 1131,  246,  116,  116,  123,  127,  116,
      146,  124,  128,  212,  125,  121, 1131,  129,   91,  121,
      121,  130,  121,  190,  126,   92,   93,  179,  190,  122,
       78,   77,   78,  179,  116,  116,  123,  143,   87,  124,
      275,  147,  125,   87,  144,  145,  116,  121,  121,  148,
       83,  121,  126,  116,  753,  149,  211,  212,  211,  131,
      121,  132,  133,  754,  134,  135,  143,  121,  213,  275,
      147,  136,  144,  145,  264,  116,  241,   82,  148,  121,
      795,  212,  116,  149,  366,  367,  366,  131,  121,  132,
      133,  242,  134,  135,  214,  121,  259,  162,  116,  136,

      116,  190,  264,  137,  762,  241,  138,  139,  215,  140,
      150,  753,  121,  260,  121,  141,  271,  255,  151,  242,
      754,  113,  152,  214,  153,  259,  162,  116,  257,  116,
      154,  698,  137,  243,  138,  139,  215,  140,  258,  150,
      121,  260,  121,  141,  271,  116,  255,  151,  244,  113,
      152,  158,  153,  155,  272,  156,  116,  257,  154,  121,
      157,  116,  243,  273,  274,  159,  258,  276,  160,  172,
      121,  161,  179,   87,  116,  121,  244,  534,  179,  277,
      158,  155,  272,  156,  534,  116,  278,  121,  157,  282,
      116,  273,  274,  159,   91,  276,  160,  172,  121,  161,

      163,   92,   93,  121,  164,  116,  169,  277,  165,  643,
      116,  116,  173,  170,  278,  635,  166,  282,  285,  167,
      168,  286,  174,  171,  121,  121,  378,   77,  378,  163,
      560,  559,  295,  164,  116,  169,  287,  165,  290,  116,
      116,  173,  170,  237,  166,  196,  285,  167,  168,  286,
      174,  171,  121,  121,  176,   77,  177,  178,  186,  187,
      190,  295,  179,  557,  287,  191,  290,  511,  283,  512,
      296,  237,  197,  556,  284,  188,  188,  188,  188,  188,
      188,  188,  188,  188,  188,  297,  198,  298,  310,  180,
      181,  116,   74,  182,  116,  183,  283,  192,  296,  172,

      200,  197,  284,  142,  203,  121,  184,  185,  121,  212,
      299,  193,  534,  297,  198,  190,  298,  310,  180,  181,
      116,   74,  182,  116,  183,  300,  192,  172,  200,  510,
      400,  142,  203,  121,  184,  185,  121,  143,  299,  193,
      194,  232,  233,  232,  144,  201,  179,  396,  395,  234,
      235,  121,  394,  300,  108,  393,  195,  195,  195,  195,
      195,  195,  195,  195,  195,  195,  143,  238,  238,  288,
       91,  288,  144,  201,  110,  110,   74,   92,   93,  121,
      238,  301,  265,  266,  302,  303,  304,  110,  267,  307,
       75,  313,  212,  651,  319,  652,  238,  238,  320,  212,

      323,  269,  236,  110,  110,   74,  270,  330,  238,  301,
      265,  266,  302,  303,  304,  110,  267,  307,   75,  199,
      313,  289,  319,  127,  116,  205,  320,  128,  323,  190,
      269,  236,  190,  129,  344,  270,  330,  130,  121,  308,
      169,  190,  309,  279,  321,  116,  291,  170,  199,  289,
      280,  322,  127,  116,  281,  331,  128,  171,  292,  121,
      293,  129,  344,  294,  202,  130,  121,  308,   87,  169,
      309,   87,  279,  321,  116,  291,  170,   83,  280,  322,
      205,  651,  281,  652,  331,  171,  292,  121,  293,  921,
     1131,  294,  202,  210,  211,  212,  211,  210,  210,  210,

      216,  210,  210,  210,  210,  210,  210,  210,  210,  210,
      210,  210,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  210,  210,  210,  210,  210,  197,  217,  217,
      217,  217,  218,  217,  123,  217,  217,  217,  219,  217,
      217,  198,  217,  217,  217,  217,  220,  217,  217,  217,
      217,  221,  217,  210,  210,  217,  197,  217,  217,  217,
      217,  218,  217,  123,  217,  217,  219,  217,  217,  198,
      217,  217,  217,  217,  220,  217,  217,  217,  217,  221,
      217,  210,   98,  251,   98,  251,   68,  305,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  311,  366,

      367,  366,  306,  345,  312,  332,  667,  326,  667,  328,
      333,  335, 1131,  329,  334,  100,  305,  347,  327,  101,
      343,  102,  205,  669,  355,  669,  103,  311,  104,  105,
      306,  345,  312,  314,  332,  315,  326,  328,  106,  333,
      335,  329, 1131,  334,  100,  347,  327, 1131,  101,  343,
      102,  316,  317,  355,  103,  318,  104,  105,  337,  358,
      338, 1131,  314,  190,  315, 1131,  106,  250,  250,  250,
      250,  250,  250,  250,  250,  250,  250,  324,  341,  316,
      317,  223,  925,  318,  362,  110,  110,  358,  346,  190,
      359,  325,  342,  190,  260,  351, 1131,  233,  110,  361,

     1131,  205,  212, 1131,  385,  235,  324,  341, 1131,  223,
      339,  186,  348,  362,  110,  110,  346,  205,  359,  325,
      342,  352,  260, 1131,  351, 1131,  110,  361,  349,  349,
      349,  349,  349,  349,  349,  349,  349,  349,  339,  350,
      262,  770,  353,  771,  390,  179,  209,  356,  376,  363,
      352,  369,  377, 1131, 1131,  349,  349,  349,  349,  349,
      349,  349,  349,  349,  349,  350,  265,  357,  262,  212,
      353,  179,  267,  390,  209,  356,  376, 1131,  363,  369,
      377,  349,  349,  349,  349,  349,  349,  349,  349,  349,
      349,  350,  205,  379,  265,  357,  212,  179, 1131,  391,

      267,  368,  206,  205,  206,  108,  291,  354,  354,  354,
      354,  354,  354,  354,  354,  354,  354,  380,  292,  399,
      293,  379,   83,  360,   87,  110,  110,  364,  370,  371,
      368,  375, 1131, 1131,  408,  291,  233,  392,  110,  207,
       87, 1131,   83,  385,  235,  380,  292,  399,  293, 1131,
      770,  360,  771, 1131,  110,  110,  364,  370,  371, 1131,
      375,  397,  381,  408, 1131,  392,  110,  383,  207,  365,
      365,  212,  365,  365,  365,  365,  365,  365,  365,  365,
      365,  365,  365,  365,  365,  365,  365,  365,  371,  371,
      381,  382,  384,  401,  398,  383,  404,  405,  365,  365,

      365,  365,  365,  373,   91,  374,  232,  233,  232,  406,
      407,   92,   93,  409,  234,  235, 1131,  371,  371,  382,
      384,  401,  398, 1131,  404,  405,  232,  233,  232,  365,
      365,  373, 1131,  374,  234,  235,   91,  406,  407, 1131,
      388,  409,  388,   92,   93,  389,  389,  389,  389,  389,
      389,  389,  389,  389,  389,  410,  387,  365,  365,  365,
      212,  365,  365,  365,  365,  365,  365,  365,  365,  365,
      365,  365,  365,  365,  365,  365,  365,  411,  412,  386,
      547,   77,  547,  410,  387, 1131, 1131,  365,  365,  365,
      365,  365,  372,  252,  252,  252,  252,  252,  252,  252,

      252,  252,  252,  413,  414,  411,  412,  386,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  365,  365,
      402,  372,  402,  686,   77,  686, 1131, 1131,  417,  419,
      403,  413,  414,  415,  420,  421,  416, 1131,  422,  423,
      424,  425,  426, 1131, 1131,  418,  365,  250,  250,  250,
      250,  250,  250,  250,  250,  250,  250,  417,  419,  439,
      444,  415,  420,  421,  416,  238,  238,  422,  423,  424,
      425,  426,  437,  418,  437,  445,  446,  440,  238,  440,
     1131, 1131,  442,  443,  447,  448,  450,  448,  439,  444,
      451, 1131,  453,  454,  238,  238,  455,  456,  288,  457,

      288,  460, 1131,  438,  445,  446,  238,  427,  441,  427,
      442,  443,  447,  458,  450,  470,  449, 1131,  469,  451,
      453,  454, 1131,  471,  455,  456,  459,  457,  474, 1131,
      460,  461,  472,  461,  472, 1131,  475,  465,  428,  465,
      476,  462,  458,  429,  470,  430,  469,  466,  431, 1131,
      452,  471,  432,  478,  459,  433,  477,  474,  434,  435,
      479, 1131,  436,  473,  475,  480,  481,  484,  476,  485,
      488,  482,  429,  489,  430,  463,  490,  431,  452,  483,
      432,  467,  478,  433,  477,  464,  434,  435,  491,  479,
      436,  468,  486,  480,  481,  492,  484,  485,  488,  482,

      487,  489,  493,  463,  490,  494,  495,  483,  496,  467,
      497,  498,  499,  464,  502,  503,  500,  491,  500,  468,
      504,  486,  505,  506,  492,  507,  501,  508,  487,  509,
      346,  493,  513,  494,  514,  495,  496,  515,  497,  498,
      499,  516,  517,  502,  503,  518,  519,  190,  504, 1131,
      505,  506, 1131,  507,  190,  508,  205,  509,  346,  511,
      513,  520,  514,  190, 1131,  515,  527,  528,  530,  516,
      517,  417,  531,  518,  545,  519,  212,  521,  521,  521,
      521,  521,  521,  521,  521,  521,  521,  522,  529,  212,
      524,  533,  523,  179,  535,  527,  528,  530,  371, 1131,

      417,  531,  545,  521,  521,  521,  521,  521,  521,  521,
      521,  521,  521,  522,  212,  525,  529,  212,  524,  179,
      533,  523,  546,  535,  548,   87, 1131,  371,  539,  521,
      521,  521,  521,  521,  521,  521,  521,  521,  521,  190,
     1131,  532,  371,  525,  522,  536,  205,  536,   83,  927,
      179,  546,  548,  541,  459,  540,  539,  538,  108,  371,
      526,  526,  526,  526,  526,  526,  526,  526,  526,  526,
      532,  371,  371,  549,   87,  552,   83,  542,  110,  110,
     1131,  541,  459,  371,  540,  538,  537,  562,  371,  233,
      558,  110,  543,  561,  544,  550,  385,  235,  667, 1029,

      667,  371,  549,  552, 1131,  542,   91,  110,  110,  551,
      553, 1131,  371,   92,   93,  537,  562,   91,  558,  110,
      543,  561,  544,  550,   92,   93,  389,  389,  389,  389,
      389,  389,  389,  389,  389,  389,  563,  564,  551,  553,
      389,  389,  389,  389,  389,  389,  389,  389,  389,  389,
      402,  565,  402,  555,  566, 1131,  554,  567, 1131,  568,
      403,  569,  570,  571,  572,  563,  564,  573,  574,  575,
      576,  577,  578,  589,  578,  590,  591, 1131,  592,  565,
      593,  596,  555,  566,  554,  597,  567,  568,  594,  569,
      570,  598,  571,  572,  599,  573,  574,  575,  576,  577,

      437,  589,  437,  590,  595,  591,  592,  602,  593,  596,
      440,  605,  440,  579,  597,  606,  603,  594,  603,  598,
      607,  448,  599,  448,  212,  608,  609,  580,  610, 1131,
      613,  438,  595,  611,  612,  602,  614,  616,  617,  605,
      619,  441,  579,  615,  606,  618,  621,  604,  620,  607,
     1131,  622,  449,  608,  609,  580,  427,  610,  427,  613,
      623,  611,  612, 1131,  629,  614,  616,  617,  461,  619,
      461,  615,  465,  618,  465,  621,  620,  627,  462,  622,
      632,  472,  466,  472, 1033,  628,  630,  428,  623,  631,
      633,  624,  581,  629,  582,  634,  640,  583,  645,  625,

      636,  584, 1131,  637,  585,  627,  638,  586,  587,  632,
      639,  588,  473,  628,  630, 1131, 1131,  631,  633, 1131,
      646,  581,  646,  582,  634,  640,  583,  645,  636,  584,
      626,  637,  585,  644,  638,  586,  587, 1131,  639,  588,
      600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
      600,  647,  600,  600,  600,  600,  600,  600,  600,  626,
      648,  644,  649,  650,  641,  500,  641,  500,  653,  600,
      600,  600,  600,  600,  642,  501,  654,  655,  656,  657,
      658,  190,  190,  669,  205,  669,  190, 1131,  667,  648,
      667,  649,  650,  651,  668,  659,  666,  653,  701,  205,

      600,  600,  571,  212,  654,  670,  655,  656,  657,  658,
      205,  660,  660,  660,  660,  660,  660,  660,  660,  660,
      660,  661,  664,  668,  666,  684,  701,  179,  600,  703,
      662,  571,  663,  675,  670, 1131, 1131,  660,  660,  660,
      660,  660,  660,  660,  660,  660,  660,  661, 1131,  212,
      212,  664,  679,  179,  684,  676,  705,  703,  662, 1050,
      663,  675, 1131,  660,  660,  660,  660,  660,  660,  660,
      660,  660,  660,  661,  614,  536,  205,  536, 1131,  179,
      679,  615,  687,  676,  705,  671,  678,  108,  371,  665,
      665,  665,  665,  665,  665,  665,  665,  665,  665,  677,

      672, 1131,  672,  614,   87,   83,  680,  110,  110,  615,
      673,  687,  699,  671,  674,  678,  537,  371,  371, 1131,
      110,  371,  681,   91,  371,  682,  700,  677,  683,  704,
       92,   93,  706,  712,  680,   91,  110,  110,  689,  713,
      692,  699,  696,  697,  707,  537,  708,  371,  110,  709,
      371,  681,  710,  371,  682,  700,  711,  683,  704,  694,
     1131,  706,  712,  718,  770,  186,  775,  689,  713,  692,
     1131,  578,  707,  578,  708, 1131,  641,  709,  641,  716,
      710,  717, 1131,  719,  711,  720,  642,  721,  694,  685,
      685,  718,  685,  685,  685,  685,  685,  685,  685,  685,

      685,  685,  685,  685,  685,  685,  685,  685,  716,  722,
      717,  719,  714,  723,  720,  721,  725,  726,  685,  685,
      685,  685,  685,  727,  728, 1131,  715,  729,  730,  724,
      731, 1131,  732,  733,  734,  739,  735,  722,  735, 1131,
      737,  714,  723,  603,  725,  603,  726,  738,  742,  685,
      685,  727,  740,  728,  715,  729,  730,  724,  741,  731,
      732,  743,  733,  734,  739,  744,  745,  736,  737,  746,
      747, 1131,  748,  749,  604,  738,  742,  685,  750,  751,
      752,  740,  755, 1131,  756,  757,  758,  741,  759,  760,
      743,  761,  766,  744,  745,  763,  764,  765,  746,  747,

      748,  767,  749,  768,  769,  646,  750,  646,  751,  752,
      772,  755,  756,  773,  757,  758,  759,  774,  760,  190,
      761,  766,  190,  763,  764,  765,  779,  780,  669,  767,
      669,  768,  769,  672,  781,  672,  647,  782,  672,  772,
      672,  205,  773,  673,  371,  774,  205,  674,  673,  674,
      371,  674,  674,  212,  212,  779,  780,  801,  777,  776,
       91, 1131,  781,  674,   91,  371,  782,  798,  799,  788,
       91,   92,   93,  371,  802,  803, 1131,   92,   93,  371,
      770,  833,  771,  833,  190,  801,  783,  777,  776,  179,
      785,  834,  784,  787,  371,  179, 1131,  804,  788,  805,

      806,  807,  802,  108,  803,  109,  109,  109,  109,  109,
      109,  109,  109,  109,  109,  783,  808, 1131,  809,  785,
      784,  810,  787,  110,  110,  804,  814,  805,  806,  807,
      815,  816, 1131,  817,  818, 1131,  110,  851, 1131,  851,
      770,  864,  770,  864, 1093,  808,  809,  852, 1131,  864,
      810,  864,  110,  110,  811,  814,  811,  819,  820,  815,
      816,  817,  821,  818,  110,  786,  786,  212,  786,  786,
      786,  786,  786,  786,  786,  786,  786,  786,  786,  786,
      786,  786,  786,  786,  822,  819,  820,  823,  824,  812,
      826,  821,  825,  827,  786,  786,  786,  786,  786,  828,

      829, 1131,  813,  830,  831,  735,  832,  735,  205,  835,
      836,  837,  822,  838, 1131,  839,  823,  824,  812,  826,
      825,  840,  827,  841,  844,  786,  786,  828,  846,  829,
      813,  830,  831, 1131, 1131,  832,  736,  835, 1131,  836,
      837,  845,  838,  839,  847,  770,  186,  775,  833,  840,
      833,  841,  844,  786,  791,   77,  791,  846,  834,  792,
      792,  842,  792,  842, 1131, 1131,  792,  792, 1095,  845,
      792, 1131,  847,  792,  792,  792,  792,  792,  792,  792,
      792,  792,  790,  790,  190,  790,  790,  790,  790,  790,
      790,  790,  790,  790,  790,  790,  790,  790,  790,  790,

      790,  848,  849,  205,  850,  853, 1131,  856,  858,  843,
      857,  790,  790,  790,  790,  790,  854,  859,  854,  862,
      865,  860,  205,  212,  863,  371,  791,   77,  791,  848,
      873,  849,  850,  879,  853,  856,  858,  843,  874,  857,
      866, 1131,  790,  790,  875,  876,  859,  877,  862,  865,
      860,  855,  863,  878,  371,   91,  882,   91,  867,  873,
      869,  879,   92,   93,   92,   93,  864,  874,  864,  866,
      790, 1131, 1131,  875,  876, 1131,  877,  811, 1131,  811,
      855,  871,  878,  883,  882,  872,  884,  867,  885,  869,
      868,  868,  212,  868,  868,  868,  868,  868,  868,  868,

      868,  868,  868,  868,  868,  868,  868,  868,  868,  886,
      871,  883,  880,  888,  872,  884,  885,  887,  889,  868,
      868,  868,  868,  868,  890,  881,  891, 1131,  892,  893,
      894,  895,  896, 1131,  897,  898,  899,  908,  886,  900,
      212,  880,  888,  906,  907,  887,  842,  889,  842,  910,
      868,  868,  890,  881,  911,  891,  892,  893,  894,  895,
      912,  896,  897,  913,  898,  899,  908,  900,  902,  903,
      902,  906,  907,  851,  914,  851,  904,  910,  868,  905,
      915,  917,  911,  852,  854,  918,  854,  919,  912,  920,
      205,  924,  913,  923,  909,  371,  928, 1131,  929,  930,

     1096,  931,  932,  914,  933,  934,  935,  936,  915,  937,
      917,  938, 1131,  939,  918,  919,  940,  944,  920,  916,
      924,  923,  909,  941,  371,  928,  929,  942,  930,  931,
      932,  943,  933,  934,  935,  936,  945,  937,  950,  926,
      938,  939,  946,  947,  948,  940,  944,  949,  916,  951,
      952,  941,  902,  903,  902,  942,  953,  903,  953,  943,
      904,  960, 1131,  905,  945,  961,  950,  926, 1131,  959,
      964,  946,  947,  948,  962,  965,  949,  951,  952,  902,
      903,  902,  966,  955,  956,  955,  967,  904,  968,  960,
      905,  957,  969,  961,  958,  902,  903,  902,  959,  964,

      190,  971,  962,  904,  965,  972,  905,  205,  205,  212,
      966,  976,  977,  978,  967, 1131,  968,  979, 1131,  980,
      981,  969,  982,  983,  984,  987,  985,  988,  985,  989,
      971,  990,  991,  992,  972,  970,  986,  993,  994,  976,
      977,  978,  995,  973,  975,  974,  979,  980, 1131,  981,
      982, 1131,  983,  984,  987,  999,  988,  998,  989,  990,
      991,  992, 1131, 1131,  970,  993,  994,  996,  903,  996,
     1001,  995,  973,  975,  974,  955,  956,  955,  190,  955,
      956,  955, 1000,  957,  999,  998,  958,  957, 1002,  205,
      958,  997,  956,  997, 1003,  955,  956,  955, 1001,  904,

     1004,  212,  905,  957, 1006,  205,  958,  955,  956,  955,
     1010, 1000, 1011, 1013, 1011,  957, 1014, 1002,  958, 1131,
     1015, 1016, 1012, 1003, 1005,  985,  985,  985,  985, 1004,
     1017, 1018, 1006, 1019, 1007,  986,  986, 1008, 1021, 1010,
     1020, 1013, 1022,  205,  205, 1014, 1131, 1009, 1015, 1025,
     1016, 1131, 1005, 1023,  903, 1023, 1026, 1028, 1017, 1018,
     1036, 1019, 1007, 1039, 1037, 1038, 1008, 1021, 1020, 1040,
     1041, 1022, 1024,  903, 1024, 1009, 1042, 1032, 1025, 1031,
      904,  205, 1011,  905, 1011, 1026, 1028, 1034, 1036, 1034,
     1045, 1039, 1012, 1037, 1038, 1046,  190, 1040, 1047, 1041,

     1035, 1049, 1035,  212, 1042, 1056, 1032, 1055, 1031, 1043,
      903, 1043, 1044,  903, 1044, 1131, 1057, 1051, 1035, 1045,
      904, 1058, 1060,  905, 1046, 1034, 1047, 1034, 1063, 1049,
     1059,  190, 1053, 1056, 1053,  212, 1055, 1064, 1035,  205,
     1035, 1048, 1073, 1054, 1057, 1035, 1051, 1035, 1052, 1075,
     1058, 1060, 1061,  903, 1061, 1067, 1035, 1063, 1059, 1062,
      903, 1062, 1053, 1035, 1053, 1064, 1072,  904, 1074, 1048,
      905, 1073, 1079, 1054, 1068,  205, 1052, 1066, 1075, 1080,
     1081, 1071, 1131, 1067, 1078,  903, 1078,  190,  212, 1083,
     1131, 1086,  904, 1131, 1072,  905, 1074, 1087, 1131, 1090,

     1079,  190,  205, 1068,  190, 1066, 1131, 1080, 1081, 1071,
     1076,  903, 1076,  205, 1131, 1077, 1077, 1083, 1077, 1086,
     1097, 1084, 1077, 1077, 1091, 1087, 1077, 1090, 1098, 1077,
     1077, 1077, 1077, 1077, 1077, 1077, 1077, 1077, 1082, 1085,
     1076,  903, 1076,  902,  903,  902, 1131, 1100, 1097, 1084,
     1103,  904,  212, 1091,  905, 1099, 1108, 1098, 1101,  205,
      212,  205, 1113, 1131,  190, 1131, 1082, 1085,  902,  903,
      902, 1104,  205, 1088, 1088, 1100, 1089,  190, 1103,  905,
     1088, 1088,  212, 1099, 1088, 1108, 1101, 1088, 1088, 1088,
     1088, 1088, 1088, 1088, 1088, 1088,  902,  903,  902, 1104,

      955,  956,  955, 1102,  904, 1105,  190,  905,  957, 1106,
     1112,  958, 1109, 1107,  212, 1115, 1116,  212, 1110, 1119,
     1120, 1119,  205, 1117, 1121, 1122, 1121, 1124, 1125, 1124,
      205, 1102,  205, 1105, 1119, 1120, 1119, 1106, 1111, 1112,
     1109,  205, 1107, 1115, 1116, 1131, 1114, 1110, 1121, 1122,
     1121, 1117, 1124, 1125, 1124, 1129, 1130, 1129, 1129, 1130,
     1129, 1131, 1131, 1131, 1127, 1131, 1126, 1111, 1118, 1131,
     1131, 1123, 1131, 1131, 1131, 1114, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1128, 1127, 1131, 1126, 1118, 1131, 1131, 1123,

     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1128,   66,   66,   66,   66,   66,   66,   66,   66,   66,
       66,   66,   66,   66,   67,   67,   67,   67,   67,   67,
       67,   67,   67,   67,   67,   67,   67,   79, 1131,   79,
       79,   79,   79,   79,   79,   79,   79,   79,   79,   79,
       82,   82,   82,   82,   82,   82,   82,   82,   82,   82,
       82,   82,   82,   86,   86,   86,   86,   86,   86,   86,
       86,   86,   86,   86,   86,   86,   90,   90,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,   90,  113,

      113,  113,  113,  113,  113,  113,  113,  113,  189,  189,
      189,  189,  189,  189,  189,  189,  189,  189,  189,  189,
      189,  204,  204,  204,  204,  204,  204,  204,  204,  204,
      204,  204,  204,  204,  210,  210,  210,  210,  210, 1131,
      210,  210,  210, 1131,  210, 1131,  210,  231,  231,  231,
      231,  231,  231,  231,  231,  231,  231,  231,  231,  231,
       99, 1131, 1131, 1131, 1131, 1131, 1131, 1131,   99,  249,
      249, 1131,  249,  249, 1131,  249, 1131, 1131,  249,  249,
     1131,  249,  336,  336,  336,  336,  340,  340,  340,  340,
      340,  340,  340,  340,  340,  340,  340,  340,  340,  365,

      365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
      365,  365,  371,  371,  371,  371,  371,  371,  371,  371,
      371,  371,  371,  371,  371,  601,  601,  601, 1131,  601,
      601,  601,  601,  601,  601,  601,  601,  601,  685, 1131,
      685,  685,  685,  685,  685,  685,  685,  685,  685,  685,
      685,  688, 1131,  688,  688,  688,  688,  688,  688,  688,
      688,  688,  688,  688,  690,  690,  690,  690,  690,  690,
      690,  690,  690,  690,  690,  690,  690,  693,  693,  693,
      693,  693,  693,  693,  693,  693,  693,  693,  693,  693,
      695,  695,  695,  695,  695,  695,  695,  695,  695,  695,

      695,  695,  695,  702, 1131,  702,  702,  702,  702,  702,
      702,  702,  702,  702,  702,  702,  778,  778,  778,  778,
      778,  778,  778,  778,  778,  778,  778,  778,  778,  786,
      786,  786,  786,  786,  786,  786,  786,  786,  786,  786,
      786,  786,  789,  789,  789,  789,  789,  789,  789,  789,
      789,  789,  789,  789,  789,  790, 1131,  790,  790,  790,
      790,  790,  790,  790,  790,  790,  790,  790,  793, 1131,
      793,  793,  793,  793,  793,  793,  793,  793,  793,  793,
      793,  794,  794,  794,  794,  794,  794,  794,  794,  794,
      794,  794,  794,  794,  796,  796,  796,  796,  796,  796,

      796,  796,  796,  796,  796,  796,  796,  797,  797,  797,
      797,  797,  797,  797,  797,  797,  797,  797,  797,  797,
      800, 1131,  800,  800,  800,  800,  800,  800,  800,  800,
      800,  800,  800,  861,  861,  861,  861,  861,  861,  861,
      861,  861,  861,  861,  861,  861,  868,  868,  868,  868,
      868,  868,  868,  868,  868,  868,  868,  868,  868,  870,
      870,  870,  870,  870,  870,  870,  870,  870,  870,  870,
      870,  870,  901,  901,  901,  901,  901,  901,  901,  901,
      901,  901,  901,  901,  901,  954,  954,  954,  954,  954,
      954,  954,  954,  954,  954,  954,  954,  954, 1069, 1069,

     1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069,
     1069,   13, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,

     1131
    } ;

static yyconst flex_int16_t yy_chk[4102] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        2,    2,    2,   74,    8,    8,    8,    8,   10,   15,

       15,   15, 1083,   14,   16,   16,   16,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,   14,   70,   17,
     1081,   10,   74,   70,    2,    2,   71,   75,    2,    8,
        2,    8,   14,   17,   18,   19,   21,   15,   20,   22,
      551,    2,    2,  551, 1069,   14,   22,   22,   17,   19,
       10,   15,   20,    2,    2,   75,   80,    2,    8,    2,
        8,   17,   84,   23,   19,   30,   15,   20,   18,    2,
        2,   71,   24,   21,   26,   30,   22,   19,   81,   15,
       20,   25,   18,  116,   23,   80,   24,   21,   26,   30,
       22,   84,   28,   85,  100,   31, 1051,   18,   23,   71,

      624,   24,   21,   26,   30,   22,   81,   25,  624,   31,
       18,   32,  116,   23,   24,   21,   26,   30,   22,   28,
       84,   25,  100, 1047,   31,   32,   23,   56,   56,   56,
       76,   76,   76,   28,  101,  105,   25,   31,  106,   85,
       32,  179,   69,   69,   69,   33, 1006,  179,   28,   25,
       27,   33,   27,   32,   77,   77,   77,   78,   78,   78,
     1000,   28,  101,  105,  914,   33,  106,   85,   27,   27,
       27,   27,   27,   27,   27,   27,   27,   27,   34,   69,
       33,   35,   35,   27,  120,   35,   41,   27,   27,   27,
      868,   35,   34,   33,   27,   41,   27,   27,   35,   35,

       41,  862,   27,  123,   89,  861,   27,   34,   69,  796,
       35,   35,   27,  120,   35,   41,   27,   27,   27,   35,
       34,  114,   27,   41,   27,   27,   35,   35,   41,   88,
       27,   36,  123,   42,   27,   29,   36,   29,   29,   29,
       29,   29,   29,   29,   29,   29,   29,   42,  794,  114,
       36,   90,  337,   89,  337,   29,   29,   29,   90,   90,
       36,   88,   42,  115,  117,   36,  104,  786,   29,  121,
      122,   29,  126,   44,  109,   42,  104,   54,   36,   38,
       38,   89,   44,   38,   29,   29,   29,   44,  109,   38,
       88,  115,  117,   38,   38,  104,   29,  121,  122,   29,

       37,  126,   44,  109,  104,   37,   54,   37,   38,   38,
       44,   37,   38,  785,   37,   44,  109,   38,   92,   37,
       54,   38,   38,  778,   37,   92,   92,  660,  777,   37,
      186,  186,  186,  660,   37,   54,   37,   43,  693,   37,
      132,   45,   37,  692,   43,   43,   45,   37,   54,   45,
      690,   43,   37,   39,  625,   45,   72,   72,   72,   39,
       45,   39,   39,  625,   39,   39,   43,   39,   72,  132,
       45,   39,   43,   43,  124,   45,  102,  689,   45,   43,
      689,  678,   39,   45,  211,  211,  211,   39,   45,   39,
       39,  102,   39,   39,   72,   39,  119,   49,   49,   39,

       40,  664,  124,   40,  635,  102,   40,   40,   72,   40,
       46,  753,   49,  119,   40,   40,  128,  143,   46,  102,
      753,   46,   46,   72,   46,  119,   49,   49,  118,   40,
       46,  558,   40,  103,   40,   40,   72,   40,  118,   46,
       49,  119,   40,   40,  128,   47,  143,   46,  103,   46,
       46,   48,   46,   47,  129,   47,   48,  118,   46,   47,
       47,   52,  103,  130,  131,   48,  118,  133,   48,   52,
       48,   48,  661,  553,   47,   52,  103,  534,  661,  134,
       48,   47,  129,   47,  519,   48,  135,   47,   47,  137,
       52,  130,  131,   48,   95,  133,   48,   52,   48,   48,

       50,   95,   95,   52,   50,   50,   51,  134,   50,  496,
       53,   51,   53,   51,  135,  487,   50,  137,  139,   50,
       50,  140,   53,   51,   53,   51,  224,  224,  224,   50,
      401,  399,  146,   50,   50,   51,  141,   50,  144,   53,
       51,   53,   51,   95,   50,   60,  139,   50,   50,  140,
       53,   51,   53,   51,   55,   55,   55,   55,   57,   57,
       58,  146,   55,  392,  141,   58,  144,  338,  138,  338,
      147,   95,   60,  390,  138,   57,   57,   57,   57,   57,
       57,   57,   57,   57,   57,  148,   60,  149,  159,   55,
       55,   65,   55,   55,   62,   55,  138,   58,  147,   65,

       62,   60,  138,   62,   65,   65,   55,   55,   62,  365,
      150,   58,  362,  148,   60,  340,  149,  159,   55,   55,
       65,   55,   55,   62,   55,  151,   58,   65,   62,  336,
      247,   62,   65,   65,   55,   55,   62,   63,  150,   58,
       59,   91,   91,   91,   63,   63,   59,  244,  243,   91,
       91,   63,  242,  151,   59,  241,   59,   59,   59,   59,
       59,   59,   59,   59,   59,   59,   63,   99,   99,  142,
       94,  142,   63,   63,   59,   59,   59,   94,   94,   63,
       99,  152,  125,  125,  153,  154,  155,   59,  125,  157,
       59,  161,  213,  511,  163,  511,   99,   99,  164,  210,

      166,  127,   94,   59,   59,   59,  127,  170,   99,  152,
      125,  125,  153,  154,  155,   59,  125,  157,   59,   61,
      161,  142,  163,   61,   61,  204,  164,   61,  166,  860,
      127,   94,  189,   61,  182,  127,  170,   61,   61,  158,
       64,  178,  158,  136,  165,   64,  145,   64,   61,  142,
      136,  165,   61,   61,  136,  171,   61,   64,  145,   64,
      145,   61,  182,  145,   64,   61,   61,  158,   87,   64,
      158,   86,  136,  165,   64,  145,   64,   82,  136,  165,
       67,  512,  136,  512,  171,   64,  145,   64,  145,  860,
       13,  145,   64,   73,   73,   73,   73,   73,   73,   73,

       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   98,  110,   98,  110,    7,  156,  110,  110,
      110,  110,  110,  110,  110,  110,  110,  110,  160,  366,

      366,  366,  156,  183,  160,  172,  528,  168,  528,  169,
      173,  174,    0,  169,  173,   98,  156,  185,  168,   98,
      181,   98,  866,  530,  196,  530,   98,  160,   98,   98,
      156,  183,  160,  162,  172,  162,  168,  169,   98,  173,
      174,  169,    0,  173,   98,  185,  168,    0,   98,  181,
       98,  162,  162,  196,   98,  162,   98,   98,  176,  199,
      176,    0,  162,  191,  162,    0,   98,  108,  108,  108,
      108,  108,  108,  108,  108,  108,  108,  167,  180,  162,
      162,  184,  866,  162,  203,  108,  108,  199,  184,  192,
      200,  167,  180,  193,  167,  191,  176,  231,  108,  202,

        0,  207,  215,    0,  231,  231,  167,  180,    0,  184,
      176,  187,  187,  203,  108,  108,  184,  208,  200,  167,
      180,  192,  167,    0,  191,  176,  108,  202,  187,  187,
      187,  187,  187,  187,  187,  187,  187,  187,  176,  188,
      197,  651,  193,  651,  239,  188,  207,  197,  222,  208,
      192,  215,  223,    0,    0,  188,  188,  188,  188,  188,
      188,  188,  188,  188,  188,  194,  198,  198,  197,  214,
      193,  194,  198,  239,  207,  197,  222,    0,  208,  215,
      223,  194,  194,  194,  194,  194,  194,  194,  194,  194,
      194,  195,  209,  225,  198,  198,  216,  195,    0,  240,

      198,  214,  206,  206,  206,  195,  201,  195,  195,  195,
      195,  195,  195,  195,  195,  195,  195,  226,  201,  246,
      201,  225,  227,  201,  229,  195,  195,  209,  216,  221,
      214,  221,    0,    0,  258,  201,  234,  240,  195,  206,
      230,    0,  228,  234,  234,  226,  201,  246,  201,    0,
      652,  201,  652,    0,  195,  195,  209,  216,  221,    0,
      221,  245,  227,  258,    0,  240,  195,  229,  206,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  220,  219,
      227,  228,  230,  248,  245,  229,  254,  255,  217,  217,

      217,  217,  217,  219,  237,  220,  232,  232,  232,  256,
      257,  237,  237,  259,  232,  232,    0,  220,  219,  228,
      230,  248,  245,    0,  254,  255,  233,  233,  233,  217,
      217,  219,    0,  220,  233,  233,  236,  256,  257,    0,
      238,  259,  238,  236,  236,  238,  238,  238,  238,  238,
      238,  238,  238,  238,  238,  260,  237,  217,  218,  218,
      218,  218,  218,  218,  218,  218,  218,  218,  218,  218,
      218,  218,  218,  218,  218,  218,  218,  261,  262,  236,
      378,  378,  378,  260,  237,    0,    0,  218,  218,  218,
      218,  218,  218,  251,  251,  251,  251,  251,  251,  251,

      251,  251,  251,  263,  264,  261,  262,  236,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  218,  218,
      253,  218,  253,  547,  547,  547,    0,    0,  266,  267,
      253,  263,  264,  265,  268,  269,  265,    0,  270,  271,
      272,  273,  274,    0,    0,  266,  218,  250,  250,  250,
      250,  250,  250,  250,  250,  250,  250,  266,  267,  277,
      281,  265,  268,  269,  265,  250,  250,  270,  271,  272,
      273,  274,  276,  266,  276,  282,  283,  278,  250,  278,
        0,    0,  279,  279,  284,  285,  286,  285,  277,  281,
      287,    0,  289,  290,  250,  250,  291,  292,  288,  293,

      288,  295,    0,  276,  282,  283,  250,  275,  278,  275,
      279,  279,  284,  294,  286,  299,  285,    0,  298,  287,
      289,  290,    0,  300,  291,  292,  294,  293,  302,    0,
      295,  296,  301,  296,  301,    0,  303,  297,  275,  297,
      304,  296,  294,  275,  299,  275,  298,  297,  275,    0,
      288,  300,  275,  306,  294,  275,  305,  302,  275,  275,
      308,    0,  275,  301,  303,  309,  310,  312,  304,  313,
      315,  311,  275,  316,  275,  296,  317,  275,  288,  311,
      275,  297,  306,  275,  305,  296,  275,  275,  318,  308,
      275,  297,  314,  309,  310,  319,  312,  313,  315,  311,

      314,  316,  320,  296,  317,  321,  322,  311,  323,  297,
      324,  325,  326,  296,  328,  329,  327,  318,  327,  297,
      330,  314,  331,  332,  319,  333,  327,  334,  314,  335,
      339,  320,  341,  321,  342,  322,  323,  343,  324,  325,
      326,  344,  345,  328,  329,  346,  347,  352,  330,    0,
      331,  332,    0,  333,  351,  334,  363,  335,  339,  348,
      341,  348,  342,  353,    0,  343,  355,  356,  358,  344,
      345,  357,  359,  346,  376,  347,  369,  348,  348,  348,
      348,  348,  348,  348,  348,  348,  348,  349,  357,  869,
      352,  361,  351,  349,  363,  355,  356,  358,  371,    0,

      357,  359,  376,  349,  349,  349,  349,  349,  349,  349,
      349,  349,  349,  350,  368,  353,  357,  370,  352,  350,
      361,  351,  377,  363,  379,  383,    0,  371,  369,  350,
      350,  350,  350,  350,  350,  350,  350,  350,  350, 1005,
        0,  360,  372,  353,  354,  364,  364,  364,  381,  869,
      354,  377,  379,  372,  360,  370,  369,  368,  354,  373,
      354,  354,  354,  354,  354,  354,  354,  354,  354,  354,
      360,  372,  374,  380,  384,  383,  382,  373,  354,  354,
        0,  372,  360,  375,  370,  368,  364,  406,  373,  385,
      398,  354,  374,  405,  375,  381,  385,  385,  653, 1005,

      653,  374,  380,  383,    0,  373,  386,  354,  354,  382,
      384,    0,  375,  386,  386,  364,  406,  387,  398,  354,
      374,  405,  375,  381,  387,  387,  388,  388,  388,  388,
      388,  388,  388,  388,  388,  388,  408,  410,  382,  384,
      389,  389,  389,  389,  389,  389,  389,  389,  389,  389,
      402,  411,  402,  387,  413,    0,  386,  414,    0,  415,
      402,  416,  417,  418,  420,  408,  410,  421,  422,  423,
      424,  425,  426,  429,  426,  430,  431,    0,  432,  411,
      433,  435,  387,  413,  386,  436,  414,  415,  434,  416,
      417,  439,  418,  420,  442,  421,  422,  423,  424,  425,

      437,  429,  437,  430,  434,  431,  432,  444,  433,  435,
      440,  446,  440,  426,  436,  447,  445,  434,  445,  439,
      450,  448,  442,  448, 1009,  451,  452,  426,  454,    0,
      457,  437,  434,  455,  456,  444,  458,  459,  460,  446,
      464,  440,  426,  458,  447,  463,  468,  445,  467,  450,
        0,  469,  448,  451,  452,  426,  427,  454,  427,  457,
      470,  455,  456,    0,  480,  458,  459,  460,  461,  464,
      461,  458,  465,  463,  465,  468,  467,  478,  461,  469,
      483,  472,  465,  472, 1009,  479,  481,  427,  470,  482,
      484,  475,  427,  480,  427,  485,  493,  427,  503,  475,

      488,  427,    0,  489,  427,  478,  490,  427,  427,  483,
      491,  427,  472,  479,  481,    0,    0,  482,  484,    0,
      506,  427,  506,  427,  485,  493,  427,  503,  488,  427,
      475,  489,  427,  499,  490,  427,  427,    0,  491,  427,
      443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
      443,  506,  443,  443,  443,  443,  443,  443,  443,  475,
      507,  499,  508,  509,  494,  500,  494,  500,  513,  443,
      443,  443,  443,  443,  494,  500,  514,  515,  516,  517,
      518,  523,  524,  655,  535,  655,  525,    0,  667,  507,
      667,  508,  509,  520,  529,  520,  527,  513,  563, 1031,

      443,  443,  529,  540,  514,  531,  515,  516,  517,  518,
      537,  520,  520,  520,  520,  520,  520,  520,  520,  520,
      520,  521,  525,  529,  527,  545,  563,  521,  443,  565,
      523,  529,  524,  535,  531,    0,    0,  521,  521,  521,
      521,  521,  521,  521,  521,  521,  521,  522,    0,  538,
      539,  525,  540,  522,  545,  537,  568,  565,  523, 1031,
      524,  535,    0,  522,  522,  522,  522,  522,  522,  522,
      522,  522,  522,  526,  532,  536,  536,  536,    0,  526,
      540,  532,  548,  537,  568,  532,  539,  526,  541,  526,
      526,  526,  526,  526,  526,  526,  526,  526,  526,  538,

      533,    0,  533,  532,  552,  550,  541,  526,  526,  532,
      533,  548,  561,  532,  533,  539,  536,  541,  542,    0,
      526,  543,  542,  554,  544,  543,  562,  538,  544,  566,
      554,  554,  569,  576,  541,  555,  526,  526,  550,  577,
      552,  561,  555,  555,  570,  536,  571,  542,  526,  573,
      543,  542,  574,  544,  543,  562,  575,  544,  566,  554,
        0,  569,  576,  581,  659,  659,  659,  550,  577,  552,
        0,  578,  570,  578,  571,    0,  641,  573,  641,  579,
      574,  580,    0,  582,  575,  583,  641,  584,  554,  564,
      564,  581,  564,  564,  564,  564,  564,  564,  564,  564,

      564,  564,  564,  564,  564,  564,  564,  564,  579,  585,
      580,  582,  578,  586,  583,  584,  587,  588,  564,  564,
      564,  564,  564,  590,  592,    0,  578,  593,  594,  586,
      595,    0,  596,  597,  598,  608,  599,  585,  599,    0,
      602,  578,  586,  603,  587,  603,  588,  607,  613,  564,
      564,  590,  610,  592,  578,  593,  594,  586,  611,  595,
      596,  614,  597,  598,  608,  615,  616,  599,  602,  617,
      618,    0,  619,  620,  603,  607,  613,  564,  621,  622,
      623,  610,  626,    0,  627,  628,  630,  611,  632,  633,
      614,  634,  639,  615,  616,  636,  637,  638,  617,  618,

      619,  640,  620,  644,  645,  646,  621,  646,  622,  623,
      654,  626,  627,  656,  628,  630,  632,  657,  633,  662,
      634,  639,  663,  636,  637,  638,  666,  668,  669,  640,
      669,  644,  645,  658,  670,  658,  646,  671,  672,  654,
      672,  676,  656,  658,  681,  657,  675,  658,  672,  674,
      683,  674,  672,  679,  677,  666,  668,  700,  663,  662,
      694,    0,  670,  674,  695,  680,  671,  694,  694,  680,
      696,  695,  695,  681,  701,  703,    0,  696,  696,  683,
      771,  738,  771,  738, 1082,  700,  675,  663,  662,  665,
      677,  738,  676,  679,  680,  665,    0,  704,  680,  706,

      707,  708,  701,  665,  703,  665,  665,  665,  665,  665,
      665,  665,  665,  665,  665,  675,  709,    0,  710,  677,
      676,  711,  679,  665,  665,  704,  714,  706,  707,  708,
      715,  717,    0,  719,  721,    0,  665,  762,    0,  762,
      770,  781,  770,  781, 1082,  709,  710,  762,  770,  858,
      711,  858,  665,  665,  712,  714,  712,  722,  723,  715,
      717,  719,  724,  721,  665,  682,  682,  682,  682,  682,
      682,  682,  682,  682,  682,  682,  682,  682,  682,  682,
      682,  682,  682,  682,  725,  722,  723,  726,  727,  712,
      729,  724,  728,  730,  682,  682,  682,  682,  682,  731,

      732,    0,  712,  733,  734,  735,  737,  735, 1084,  739,
      740,  741,  725,  742,    0,  743,  726,  727,  712,  729,
      728,  745,  730,  746,  752,  682,  682,  731,  756,  732,
      712,  733,  734,    0,    0,  737,  735,  739,    0,  740,
      741,  755,  742,  743,  757,  775,  775,  775,  833,  745,
      833,  746,  752,  682,  686,  686,  686,  756,  833,  686,
      686,  751,  686,  751,    0,    0,  686,  686, 1084,  755,
      686,    0,  757,  686,  686,  686,  686,  686,  686,  686,
      686,  686,  699,  699,  776,  699,  699,  699,  699,  699,
      699,  699,  699,  699,  699,  699,  699,  699,  699,  699,

      699,  758,  759,  783,  760,  763,    0,  768,  773,  751,
      772,  699,  699,  699,  699,  699,  767,  774,  767,  779,
      782,  776,  784,  787,  780,  789,  791,  791,  791,  758,
      802,  759,  760,  810,  763,  768,  773,  751,  803,  772,
      783,    0,  699,  699,  804,  806,  774,  807,  779,  782,
      776,  767,  780,  808,  789,  797,  812,  798,  784,  802,
      787,  810,  797,  797,  798,  798,  864,  803,  864,  783,
      699,    0,    0,  804,  806,    0,  807,  811,    0,  811,
      767,  801,  808,  813,  812,  801,  815,  784,  816,  787,
      788,  788,  788,  788,  788,  788,  788,  788,  788,  788,

      788,  788,  788,  788,  788,  788,  788,  788,  788,  817,
      801,  813,  811,  819,  801,  815,  816,  818,  820,  788,
      788,  788,  788,  788,  821,  811,  822,    0,  823,  824,
      825,  826,  827,    0,  828,  830,  831,  840,  817,  832,
     1085,  811,  819,  835,  836,  818,  842,  820,  842,  843,
      788,  788,  821,  811,  844,  822,  823,  824,  825,  826,
      846,  827,  828,  847,  830,  831,  840,  832,  834,  834,
      834,  835,  836,  851,  852,  851,  834,  843,  788,  834,
      853,  855,  844,  851,  854,  856,  854,  857,  846,  859,
      867,  865,  847,  863,  842,  870,  871,    0,  873,  874,

     1085,  875,  876,  852,  878,  879,  880,  881,  853,  882,
      855,  883,    0,  884,  856,  857,  885,  889,  859,  854,
      865,  863,  842,  886,  870,  871,  873,  887,  874,  875,
      876,  888,  878,  879,  880,  881,  890,  882,  896,  867,
      883,  884,  892,  893,  894,  885,  889,  895,  854,  897,
      899,  886,  901,  901,  901,  887,  903,  903,  903,  888,
      901,  909,    0,  901,  890,  910,  896,  867,    0,  908,
      915,  892,  893,  894,  913,  916,  895,  897,  899,  902,
      902,  902,  917,  904,  904,  904,  918,  902,  919,  909,
      902,  904,  920,  910,  904,  905,  905,  905,  908,  915,

      921,  922,  913,  905,  916,  924,  905,  925,  926,  927,
      917,  928,  929,  932,  918,    0,  919,  933,    0,  935,
      936,  920,  937,  938,  939,  941,  940,  942,  940,  943,
      922,  944,  945,  947,  924,  921,  940,  949,  951,  928,
      929,  932,  952,  925,  927,  926,  933,  935,    0,  936,
      937,    0,  938,  939,  941,  961,  942,  960,  943,  944,
      945,  947,    0,    0,  921,  949,  951,  953,  953,  953,
      965,  952,  925,  927,  926,  954,  954,  954,  970,  955,
      955,  955,  963,  954,  961,  960,  954,  955,  966,  973,
      955,  956,  956,  956,  967,  957,  957,  957,  965,  956,

      969,  975,  956,  957,  971,  974,  957,  958,  958,  958,
      976,  963,  977,  980,  977,  958,  981,  966,  958,    0,
      982,  983,  977,  967,  970,  984,  985,  984,  985,  969,
      987,  989,  971,  991,  973,  984,  985,  974,  994,  976,
      992,  980,  995, 1008, 1007,  981,    0,  975,  982,  998,
      983,    0,  970,  996,  996,  996,  999, 1001,  987,  989,
     1013,  991,  973, 1016, 1014, 1015,  974,  994,  992, 1017,
     1019,  995,  997,  997,  997,  975, 1021, 1008,  998, 1007,
      997, 1032, 1011,  997, 1011,  999, 1001, 1012, 1013, 1012,
     1025, 1016, 1011, 1014, 1015, 1026, 1029, 1017, 1027, 1019,

     1012, 1030, 1012, 1033, 1021, 1037, 1008, 1036, 1007, 1023,
     1023, 1023, 1024, 1024, 1024,    0, 1038, 1032, 1012, 1025,
     1024, 1039, 1042, 1024, 1026, 1034, 1027, 1034, 1045, 1030,
     1041, 1048, 1035, 1037, 1035, 1052, 1036, 1046, 1034, 1050,
     1034, 1029, 1056, 1035, 1038, 1035, 1032, 1035, 1033, 1059,
     1039, 1042, 1043, 1043, 1043, 1049, 1034, 1045, 1041, 1044,
     1044, 1044, 1053, 1035, 1053, 1046, 1055, 1044, 1058, 1029,
     1044, 1056, 1063, 1053, 1050, 1068, 1033, 1048, 1059, 1064,
     1065, 1052,    0, 1049, 1062, 1062, 1062, 1066, 1071, 1067,
        0, 1073, 1062,    0, 1055, 1062, 1058, 1074,    0, 1079,

     1063, 1099, 1109, 1050, 1093, 1048,    0, 1064, 1065, 1052,
     1061, 1061, 1061, 1095,    0, 1061, 1061, 1067, 1061, 1073,
     1086, 1068, 1061, 1061, 1080, 1074, 1061, 1079, 1090, 1061,
     1061, 1061, 1061, 1061, 1061, 1061, 1061, 1061, 1066, 1071,
     1076, 1076, 1076, 1077, 1077, 1077,    0, 1094, 1086, 1068,
     1099, 1077, 1096, 1080, 1077, 1093, 1104, 1090, 1095, 1101,
     1102, 1105, 1109,    0, 1111,    0, 1066, 1071, 1078, 1078,
     1078, 1100, 1113, 1078, 1078, 1094, 1078, 1103, 1099, 1078,
     1078, 1078, 1106, 1093, 1078, 1104, 1095, 1078, 1078, 1078,
     1078, 1078, 1078, 1078, 1078, 1078, 1088, 1088, 1088, 1100,

     1089, 1089, 1089, 1096, 1088, 1101, 1107, 1088, 1089, 1102,
     1108, 1089, 1105, 1103, 1110, 1111, 1112, 1114, 1106, 1115,
     1115, 1115, 1117, 1113, 1116, 1116, 1116, 1118, 1118, 1118,
     1123, 1096, 1126, 1101, 1119, 1119, 1119, 1102, 1107, 1108,
     1105, 1127, 1103, 1111, 1112,    0, 1110, 1106, 1121, 1121,
     1121, 1113, 1124, 1124, 1124, 1128, 1128, 1128, 1129, 1129,
     1129,    0,    0,    0, 1126,    0, 1123, 1107, 1114,    0,
        0, 1117,    0,    0,    0, 1110,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0, 1127, 1126,    0, 1123, 1114,    0,    0, 1117,

        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     1127, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1133, 1133, 1133, 1133, 1133, 1133,
     1133, 1133, 1133, 1133, 1133, 1133, 1133, 1134,    0, 1134,
     1134, 1134, 1134, 1134, 1134, 1134, 1134, 1134, 1134, 1134,
     1135, 1135, 1135, 1135, 1135, 1135, 1135, 1135, 1135, 1135,
     1135, 1135, 1135, 1136, 1136, 1136, 1136, 1136, 1136, 1136,
     1136, 1136, 1136, 1136, 1136, 1136, 1137, 1137, 1137, 1137,
     1137, 1137, 1137, 1137, 1137, 1137, 1137, 1137, 1137, 1138,

     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1139, 1139,
     1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139,
     1139, 1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140,
     1140, 1140, 1140, 1140, 1141, 1141, 1141, 1141, 1141,    0,
     1141, 1141, 1141,    0, 1141,    0, 1141, 1142, 1142, 1142,
     1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142,
     1143,    0,    0,    0,    0,    0,    0,    0, 1143, 1144,
     1144,    0, 1144, 1144,    0, 1144,    0,    0, 1144, 1144,
        0, 1144, 1145, 1145, 1145, 1145, 1146, 1146, 1146, 1146,
     1146, 1146, 1146, 1146, 1146, 1146, 1146, 1146, 1146, 1147,

     1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147,
     1147, 1147, 1148, 1148, 1148, 1148, 1148, 1148, 1148, 1148,
     1148, 1148, 1148, 1148, 1148, 1149, 1149, 1149,    0, 1149,
     1149, 1149, 1149, 1149, 1149, 1149, 1149, 1149, 1150,    0,
     1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150,
     1150, 1151,    0, 1151, 1151, 1151, 1151, 1151, 1151, 1151,
     1151, 1151, 1151, 1151, 1152, 1152, 1152, 1152, 1152, 1152,
     1152, 1152, 1152, 1152, 1152, 1152, 1152, 1153, 1153, 1153,
     1153, 1153, 1153, 1153, 1153, 1153, 1153, 1153, 1153, 1153,
     1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154,

     1154, 1154, 1154, 1155,    0, 1155, 1155, 1155, 1155, 1155,
     1155, 1155, 1155, 1155, 1155, 1155, 1156, 1156, 1156, 1156,
     1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156, 1157,
     1157, 1157, 1157, 1157, 1157, 1157, 1157, 1157, 1157, 1157,
     1157, 1157, 1158, 1158, 1158, 1158, 1158, 1158, 1158, 1158,
     1158, 1158, 1158, 1158, 1158, 1159,    0, 1159, 1159, 1159,
     1159, 1159, 1159, 1159, 1159, 1159, 1159, 1159, 1160,    0,
     1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160,
     1160, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1162, 1162, 1162, 1162, 1162, 1162,

     1162, 1162, 1162, 1162, 1162, 1162, 1162, 1163, 1163, 1163,
     1163, 1163, 1163, 1163, 1163, 1163, 1163, 1163, 1163, 1163,
     1164,    0, 1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164,
     1164, 1164, 1164, 1165, 1165, 1165, 1165, 1165, 1165, 1165,
     1165, 1165, 1165, 1165, 1165, 1165, 1166, 1166, 1166, 1166,
     1166, 1166, 1166, 1166, 1166, 1166, 1166, 1166, 1166, 1167,
     1167, 1167, 1167, 1167, 1167, 1167, 1167, 1167, 1167, 1167,
     1167, 1167, 1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168,
     1168, 1168, 1168, 1168, 1168, 1169, 1169, 1169, 1169, 1169,
     1169, 1169, 1169, 1169, 1169, 1169, 1169, 1169, 1170, 1170,

     1170, 1170, 1170, 1170, 1170, 1170, 1170, 1170, 1170, 1170,
     1170, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131, 1131,

     1131
    } ;

static yy_state_type yy_last_accepting_state;
static char *yy_last_accepting_cpos;

extern int fortran__flex_debug;
int fortran__flex_debug = 0;

/* The intent behind this definition is that it'll catch
 * any uses of REJECT which flex missed.
 */
#define REJECT reject_used_but_not_detected
#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0
#define YY_RESTORE_YY_MORE_OFFSET
char *fortran_text;
#line 1 "fortran.lex"
/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/





#line 44 "fortran.lex"
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * fortran_in;
#define MAX_INCLUDE_DEPTH 30
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_input = 1;
int newlinef90 = 0;
char tmpc;
#define PRINT_LINE_NUM()     // { fprintf(stderr,"== Parsing l.%4d...\n", line_num_input); }
#define INCREMENT_LINE_NUM() { line_num_input++; PRINT_LINE_NUM(); }

/******************************************************************************/
/**************PETITS PB NON PREVUS *******************************************/
/******************************************************************************/
/* NEXTLINF77 un ligne fortran 77 peut commencer par -      &a=b or on        */
/*            a prevu seulement       & a=b avec l'espace entre le symbole    */
/*            de la 7eme et le debut de la ligne de commande                  */
/*            le ! est aussi interdit comme symbole de la 7 eme colonne       */
/*            Normalement NEXTLINEF77 \n+[ ]{5}[^ ]                           */
/******************************************************************************/
#define YY_USER_ACTION  if (firstpass == 0) ECHO;

void out_of_donottreat(void);

#line 1826 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define fortran77style 4
#define fortran90style 5

#ifndef YY_NO_UNISTD_H
/* Special case for "unistd.h", since it is non-ANSI. We include it way
 * down here because we want the user's section 1 to have been scanned first.
 * The user has a chance to override it with an option.
 */
#include <unistd.h>
#endif

#ifndef YY_EXTRA_TYPE
#define YY_EXTRA_TYPE void *
#endif

static int yy_init_globals (void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int fortran_lex_destroy (void );

int fortran_get_debug (void );

void fortran_set_debug (int debug_flag  );

YY_EXTRA_TYPE fortran_get_extra (void );

void fortran_set_extra (YY_EXTRA_TYPE user_defined  );

FILE *fortran_get_in (void );

void fortran_set_in  (FILE * in_str  );

FILE *fortran_get_out (void );

void fortran_set_out  (FILE * out_str  );

yy_size_t fortran_get_leng (void );

char *fortran_get_text (void );

int fortran_get_lineno (void );

void fortran_set_lineno (int line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int fortran_wrap (void );
#else
extern int fortran_wrap (void );
#endif
#endif

    static void yyunput (int c,char *buf_ptr  );
    
#ifndef yytext_ptr
static void yy_flex_strncpy (char *,yyconst char *,int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * );
#endif

#ifndef YY_NO_INPUT

#ifdef __cplusplus
static int yyinput (void );
#else
static int input (void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#define YY_READ_BUF_SIZE 8192
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO fwrite( fortran_text, fortran_leng, 1, fortran_out )
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		yy_size_t n; \
		for ( n = 0; n < max_size && \
			     (c = getc( fortran_in )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( fortran_in ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = fread(buf, 1, max_size, fortran_in))==0 && ferror(fortran_in)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(fortran_in); \
			} \
		}\
\

#endif

/* No semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#ifndef yyterminate
#define yyterminate() return YY_NULL
#endif

/* Number of entries by which start-condition stack grows. */
#ifndef YY_START_STACK_INCR
#define YY_START_STACK_INCR 25
#endif

/* Report a fatal error. */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) yy_fatal_error( msg )
#endif

/* end tables serialization structures and prototypes */

/* Default declaration of generated scanner - a define so the user can
 * easily add parameters.
 */
#ifndef YY_DECL
#define YY_DECL_IS_OURS 1

extern int fortran_lex (void);

#define YY_DECL int fortran_lex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after fortran_text and fortran_leng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK break;
#endif

#define YY_RULE_SETUP \
	if ( fortran_leng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(fortran_text[fortran_leng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	register yy_state_type yy_current_state;
	register char *yy_cp, *yy_bp;
	register int yy_act;
    
#line 97 "fortran.lex"

  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

#line 2021 "fortran.yy.c"

	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! fortran_in )
			fortran_in = stdin;

		if ( ! fortran_out )
			fortran_out = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			fortran_ensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				fortran__create_buffer(fortran_in,YY_BUF_SIZE );
		}

		fortran__load_buffer_state( );
		}

	while ( 1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of fortran_text. */
		*yy_cp = (yy_hold_char);

		/* yy_bp points to the position in yy_ch_buf of the start of
		 * the current run.
		 */
		yy_bp = yy_cp;

		yy_current_state = (yy_start);
		yy_current_state += YY_AT_BOL();
yy_match:
		do
			{
			register YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)];
			if ( yy_accept[yy_current_state] )
				{
				(yy_last_accepting_state) = yy_current_state;
				(yy_last_accepting_cpos) = yy_cp;
				}
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1132 )
					yy_c = yy_meta[(unsigned int) yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 4012 );

yy_find_action:
		yy_act = yy_accept[yy_current_state];
		if ( yy_act == 0 )
			{ /* have to back up */
			yy_cp = (yy_last_accepting_cpos);
			yy_current_state = (yy_last_accepting_state);
			yy_act = yy_accept[yy_current_state];
			}

		YY_DO_BEFORE_ACTION;

do_action:	/* This label is used only to access EOF actions. */

		switch ( yy_act )
	{ /* beginning of action switch */
			case 0: /* must back up */
			/* undo the effects of YY_DO_BEFORE_ACTION */
			*yy_cp = (yy_hold_char);
			yy_cp = (yy_last_accepting_cpos);
			yy_current_state = (yy_last_accepting_state);
			goto yy_find_action;

case 1:
YY_RULE_SETUP
#line 101 "fortran.lex"
{ return TOK_REAL8; }
	YY_BREAK
case 2:
YY_RULE_SETUP
#line 102 "fortran.lex"
{ return TOK_SUBROUTINE; }
	YY_BREAK
case 3:
YY_RULE_SETUP
#line 103 "fortran.lex"
{ return TOK_PROGRAM; }
	YY_BREAK
case 4:
YY_RULE_SETUP
#line 104 "fortran.lex"
{ inallocate = 1; return TOK_ALLOCATE; }
	YY_BREAK
case 5:
YY_RULE_SETUP
#line 105 "fortran.lex"
{ return TOK_NULLIFY; }
	YY_BREAK
case 6:
YY_RULE_SETUP
#line 106 "fortran.lex"
{ return TOK_NULL_PTR; }
	YY_BREAK
case 7:
YY_RULE_SETUP
#line 107 "fortran.lex"
{ inallocate = 1; return TOK_DEALLOCATE; }
	YY_BREAK
case 8:
YY_RULE_SETUP
#line 108 "fortran.lex"
{ return TOK_RESULT; }
	YY_BREAK
case 9:
YY_RULE_SETUP
#line 109 "fortran.lex"
{ return TOK_FUNCTION; }
	YY_BREAK
case 10:
YY_RULE_SETUP
#line 110 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDPROGRAM;}
	YY_BREAK
case 11:
YY_RULE_SETUP
#line 111 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDMODULE; }
	YY_BREAK
case 12:
YY_RULE_SETUP
#line 112 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDSUBROUTINE;}
	YY_BREAK
case 13:
YY_RULE_SETUP
#line 113 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDFUNCTION;}
	YY_BREAK
case 14:
YY_RULE_SETUP
#line 114 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
	YY_BREAK
case 15:
YY_RULE_SETUP
#line 115 "fortran.lex"
{ pos_curinclude = setposcur()-9; return TOK_INCLUDE;}
	YY_BREAK
case 16:
YY_RULE_SETUP
#line 116 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                              tmpc = (char) input(); unput(tmpc);
                              if ( ( tmpc >= 'a' && tmpc <= 'z' ) ||
                                   ( tmpc >= 'A' && tmpc <= 'Z' )  )  return TOK_USE;
                              else                                    return TOK_NAME;
                            }
	YY_BREAK
case 17:
YY_RULE_SETUP
#line 122 "fortran.lex"
{ return TOK_REWIND; }
	YY_BREAK
case 18:
YY_RULE_SETUP
#line 123 "fortran.lex"
{ return TOK_IMPLICIT; }
	YY_BREAK
case 19:
YY_RULE_SETUP
#line 124 "fortran.lex"
{ return TOK_NONE; }
	YY_BREAK
case 20:
YY_RULE_SETUP
#line 125 "fortran.lex"
{ return TOK_CALL; }
	YY_BREAK
case 21:
YY_RULE_SETUP
#line 126 "fortran.lex"
{ return TOK_TRUE; }
	YY_BREAK
case 22:
YY_RULE_SETUP
#line 127 "fortran.lex"
{ return TOK_FALSE; }
	YY_BREAK
case 23:
YY_RULE_SETUP
#line 128 "fortran.lex"
{ return TOK_POINT_TO; }
	YY_BREAK
case 24:
YY_RULE_SETUP
#line 129 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ASSIGNTYPE;}
	YY_BREAK
case 25:
YY_RULE_SETUP
#line 130 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DASTER; }
	YY_BREAK
case 26:
YY_RULE_SETUP
#line 131 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQV; }
	YY_BREAK
case 27:
YY_RULE_SETUP
#line 132 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQ;  }
	YY_BREAK
case 28:
YY_RULE_SETUP
#line 133 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GT;  }
	YY_BREAK
case 29:
YY_RULE_SETUP
#line 134 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GE;  }
	YY_BREAK
case 30:
YY_RULE_SETUP
#line 135 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LT;  }
	YY_BREAK
case 31:
YY_RULE_SETUP
#line 136 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LE;  }
	YY_BREAK
case 32:
YY_RULE_SETUP
#line 137 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NEQV;}
	YY_BREAK
case 33:
YY_RULE_SETUP
#line 138 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NE;  }
	YY_BREAK
case 34:
YY_RULE_SETUP
#line 139 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NOT; }
	YY_BREAK
case 35:
YY_RULE_SETUP
#line 140 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_OR;  }
	YY_BREAK
case 36:
YY_RULE_SETUP
#line 141 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_XOR; }
	YY_BREAK
case 37:
YY_RULE_SETUP
#line 142 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_AND; }
	YY_BREAK
case 38:
YY_RULE_SETUP
#line 143 "fortran.lex"
{ return TOK_MODULE; }
	YY_BREAK
case 39:
YY_RULE_SETUP
#line 144 "fortran.lex"
{ return TOK_WHILE; }
	YY_BREAK
case 40:
YY_RULE_SETUP
#line 145 "fortran.lex"
{ return TOK_CONCURRENT; }
	YY_BREAK
case 41:
YY_RULE_SETUP
#line 146 "fortran.lex"
{ return TOK_ENDDO; }
	YY_BREAK
case 42:
YY_RULE_SETUP
#line 147 "fortran.lex"
{ return TOK_PLAINDO;}
	YY_BREAK
case 43:
YY_RULE_SETUP
#line 148 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_REAL; }
	YY_BREAK
case 44:
YY_RULE_SETUP
#line 149 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
	YY_BREAK
case 45:
YY_RULE_SETUP
#line 150 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
	YY_BREAK
case 46:
YY_RULE_SETUP
#line 151 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
	YY_BREAK
case 47:
YY_RULE_SETUP
#line 152 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_HEXA;}
	YY_BREAK
case 48:
YY_RULE_SETUP
#line 153 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
	YY_BREAK
case 49:
YY_RULE_SETUP
#line 154 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
	YY_BREAK
case 50:
YY_RULE_SETUP
#line 155 "fortran.lex"
{ return TOK_COMPLEX; }
	YY_BREAK
case 51:
YY_RULE_SETUP
#line 156 "fortran.lex"
{ return TOK_ALLOCATABLE; }
	YY_BREAK
case 52:
YY_RULE_SETUP
#line 157 "fortran.lex"
{ return TOK_CLOSE; }
	YY_BREAK
case 53:
YY_RULE_SETUP
#line 158 "fortran.lex"
{ return TOK_INQUIRE; }
	YY_BREAK
case 54:
YY_RULE_SETUP
#line 159 "fortran.lex"
{ return TOK_DIMENSION; }
	YY_BREAK
case 55:
YY_RULE_SETUP
#line 160 "fortran.lex"
{ return TOK_PAUSE; }
	YY_BREAK
case 56:
YY_RULE_SETUP
#line 161 "fortran.lex"
{ return TOK_EQUIVALENCE; }
	YY_BREAK
case 57:
YY_RULE_SETUP
#line 162 "fortran.lex"
{ return TOK_STOP; }
	YY_BREAK
case 58:
YY_RULE_SETUP
#line 163 "fortran.lex"
{ return TOK_WHERE; }
	YY_BREAK
case 59:
YY_RULE_SETUP
#line 164 "fortran.lex"
{ return TOK_ENDWHERE; }
	YY_BREAK
case 60:
YY_RULE_SETUP
#line 165 "fortran.lex"
{ return TOK_ELSEWHEREPAR; }
	YY_BREAK
case 61:
YY_RULE_SETUP
#line 166 "fortran.lex"
{ return TOK_ELSEWHERE; }
	YY_BREAK
case 62:
YY_RULE_SETUP
#line 167 "fortran.lex"
{ return TOK_CONTAINS; }
	YY_BREAK
case 63:
YY_RULE_SETUP
#line 168 "fortran.lex"
{ return TOK_ONLY; }
	YY_BREAK
case 64:
YY_RULE_SETUP
#line 169 "fortran.lex"
{ return TOK_PARAMETER; }
	YY_BREAK
case 65:
YY_RULE_SETUP
#line 170 "fortran.lex"
{ return TOK_RECURSIVE; }
	YY_BREAK
case 66:
YY_RULE_SETUP
#line 171 "fortran.lex"
{ return TOK_COMMON; }
	YY_BREAK
case 67:
YY_RULE_SETUP
#line 172 "fortran.lex"
{ return TOK_GLOBAL; }
	YY_BREAK
case 68:
YY_RULE_SETUP
#line 173 "fortran.lex"
{ return TOK_EXTERNAL; }
	YY_BREAK
case 69:
YY_RULE_SETUP
#line 174 "fortran.lex"
{ return TOK_INTENT; }
	YY_BREAK
case 70:
YY_RULE_SETUP
#line 175 "fortran.lex"
{ return TOK_POINTER; }
	YY_BREAK
case 71:
YY_RULE_SETUP
#line 176 "fortran.lex"
{ return TOK_OPTIONAL; }
	YY_BREAK
case 72:
YY_RULE_SETUP
#line 177 "fortran.lex"
{ return TOK_SAVE; }
	YY_BREAK
case 73:
YY_RULE_SETUP
#line 178 "fortran.lex"
{ pos_cur_decl = setposcur()-5; return TOK_TYPEPAR; }
	YY_BREAK
case 74:
YY_RULE_SETUP
#line 179 "fortran.lex"
{ return TOK_TYPE; }
	YY_BREAK
case 75:
YY_RULE_SETUP
#line 180 "fortran.lex"
{ return TOK_ENDTYPE; }
	YY_BREAK
case 76:
YY_RULE_SETUP
#line 181 "fortran.lex"
{ if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
	YY_BREAK
case 77:
YY_RULE_SETUP
#line 182 "fortran.lex"
{ return TOK_OPEN; }
	YY_BREAK
case 78:
YY_RULE_SETUP
#line 183 "fortran.lex"
{ return TOK_RETURN; }
	YY_BREAK
case 79:
/* rule 79 can match eol */
YY_RULE_SETUP
#line 184 "fortran.lex"
{ return TOK_EXIT; }
	YY_BREAK
case 80:
YY_RULE_SETUP
#line 185 "fortran.lex"
{ return TOK_PRINT; }
	YY_BREAK
case 81:
YY_RULE_SETUP
#line 186 "fortran.lex"
{ return TOK_PROCEDURE; }
	YY_BREAK
case 82:
YY_RULE_SETUP
#line 187 "fortran.lex"
{ return TOK_READ; }
	YY_BREAK
case 83:
YY_RULE_SETUP
#line 188 "fortran.lex"
{ return TOK_NAMELIST; }
	YY_BREAK
case 84:
YY_RULE_SETUP
#line 189 "fortran.lex"
{ return TOK_WRITE; }
	YY_BREAK
case 85:
YY_RULE_SETUP
#line 190 "fortran.lex"
{ return TOK_FLUSH; }
	YY_BREAK
case 86:
YY_RULE_SETUP
#line 191 "fortran.lex"
{ return TOK_TARGET; }
	YY_BREAK
case 87:
YY_RULE_SETUP
#line 192 "fortran.lex"
{ return TOK_PUBLIC; }
	YY_BREAK
case 88:
YY_RULE_SETUP
#line 193 "fortran.lex"
{ return TOK_PRIVATE; }
	YY_BREAK
case 89:
YY_RULE_SETUP
#line 194 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_IN; }
	YY_BREAK
case 90:
YY_RULE_SETUP
#line 195 "fortran.lex"
{ pos_curdata = setposcur()-strlen(fortran_text); Init_List_Data_Var(); return TOK_DATA; }
	YY_BREAK
case 91:
YY_RULE_SETUP
#line 196 "fortran.lex"
{ return TOK_CONTINUE; }
	YY_BREAK
case 92:
YY_RULE_SETUP
#line 197 "fortran.lex"
{ return TOK_PLAINGOTO; }
	YY_BREAK
case 93:
YY_RULE_SETUP
#line 198 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_OUT; }
	YY_BREAK
case 94:
YY_RULE_SETUP
#line 199 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INOUT; }
	YY_BREAK
case 95:
YY_RULE_SETUP
#line 200 "fortran.lex"
{ return TOK_INTRINSIC; }
	YY_BREAK
case 96:
YY_RULE_SETUP
#line 201 "fortran.lex"
{ return TOK_THEN; }
	YY_BREAK
case 97:
YY_RULE_SETUP
#line 202 "fortran.lex"
{ return TOK_ELSEIF; }
	YY_BREAK
case 98:
YY_RULE_SETUP
#line 203 "fortran.lex"
{ return TOK_ELSE; }
	YY_BREAK
case 99:
YY_RULE_SETUP
#line 204 "fortran.lex"
{ return TOK_ENDIF; }
	YY_BREAK
case 100:
YY_RULE_SETUP
#line 205 "fortran.lex"
{ return TOK_LOGICALIF; }
	YY_BREAK
case 101:
YY_RULE_SETUP
#line 206 "fortran.lex"
{ return TOK_SUM; }
	YY_BREAK
case 102:
YY_RULE_SETUP
#line 207 "fortran.lex"
{ return TOK_MAX; }
	YY_BREAK
case 103:
YY_RULE_SETUP
#line 208 "fortran.lex"
{ return TOK_TANH; }
	YY_BREAK
case 104:
YY_RULE_SETUP
#line 209 "fortran.lex"
{ return TOK_MAXVAL; }
	YY_BREAK
case 105:
YY_RULE_SETUP
#line 210 "fortran.lex"
{ return TOK_TRIM; }
	YY_BREAK
case 106:
YY_RULE_SETUP
#line 211 "fortran.lex"
{ return TOK_SQRT; }
	YY_BREAK
case 107:
YY_RULE_SETUP
#line 212 "fortran.lex"
{ return TOK_SELECTCASE; }
	YY_BREAK
case 108:
YY_RULE_SETUP
#line 213 "fortran.lex"
{ return TOK_CASE; }
	YY_BREAK
case 109:
YY_RULE_SETUP
#line 214 "fortran.lex"
{ return TOK_DEFAULT; }
	YY_BREAK
case 110:
YY_RULE_SETUP
#line 215 "fortran.lex"
{ return TOK_ENDSELECT; }
	YY_BREAK
case 111:
YY_RULE_SETUP
#line 216 "fortran.lex"
{ return TOK_FILE; }
	YY_BREAK
case 112:
YY_RULE_SETUP
#line 217 "fortran.lex"
{ return TOK_UNIT; }
	YY_BREAK
case 113:
YY_RULE_SETUP
#line 218 "fortran.lex"
{ return TOK_FMT; }
	YY_BREAK
case 114:
YY_RULE_SETUP
#line 219 "fortran.lex"
{ return TOK_NML; }
	YY_BREAK
case 115:
YY_RULE_SETUP
#line 220 "fortran.lex"
{ return TOK_END; }
	YY_BREAK
case 116:
YY_RULE_SETUP
#line 221 "fortran.lex"
{ return TOK_EOR; }
	YY_BREAK
case 117:
YY_RULE_SETUP
#line 222 "fortran.lex"
{ return TOK_ERR; }
	YY_BREAK
case 118:
YY_RULE_SETUP
#line 223 "fortran.lex"
{ return TOK_EXIST; }
	YY_BREAK
case 119:
YY_RULE_SETUP
#line 224 "fortran.lex"
{ return TOK_MIN; }
	YY_BREAK
case 120:
YY_RULE_SETUP
#line 225 "fortran.lex"
{ return TOK_NINT; }
	YY_BREAK
case 121:
YY_RULE_SETUP
#line 226 "fortran.lex"
{ return TOK_FLOAT; }
	YY_BREAK
case 122:
YY_RULE_SETUP
#line 227 "fortran.lex"
{ return TOK_EXP; }
	YY_BREAK
case 123:
YY_RULE_SETUP
#line 228 "fortran.lex"
{ return TOK_COS; }
	YY_BREAK
case 124:
YY_RULE_SETUP
#line 229 "fortran.lex"
{ return TOK_COSH; }
	YY_BREAK
case 125:
YY_RULE_SETUP
#line 230 "fortran.lex"
{ return TOK_ACOS; }
	YY_BREAK
case 126:
YY_RULE_SETUP
#line 231 "fortran.lex"
{ return TOK_SIN; }
	YY_BREAK
case 127:
YY_RULE_SETUP
#line 232 "fortran.lex"
{ return TOK_SINH; }
	YY_BREAK
case 128:
YY_RULE_SETUP
#line 233 "fortran.lex"
{ return TOK_ASIN; }
	YY_BREAK
case 129:
YY_RULE_SETUP
#line 234 "fortran.lex"
{ return TOK_LOG; }
	YY_BREAK
case 130:
YY_RULE_SETUP
#line 235 "fortran.lex"
{ return TOK_TAN; }
	YY_BREAK
case 131:
YY_RULE_SETUP
#line 236 "fortran.lex"
{ return TOK_ATAN; }
	YY_BREAK
case 132:
YY_RULE_SETUP
#line 237 "fortran.lex"
{ return TOK_CYCLE; }
	YY_BREAK
case 133:
YY_RULE_SETUP
#line 238 "fortran.lex"
{ return TOK_ABS; }
	YY_BREAK
case 134:
YY_RULE_SETUP
#line 239 "fortran.lex"
{ return TOK_MOD; }
	YY_BREAK
case 135:
YY_RULE_SETUP
#line 240 "fortran.lex"
{ return TOK_SIGN; }
	YY_BREAK
case 136:
YY_RULE_SETUP
#line 241 "fortran.lex"
{ return TOK_MINLOC; }
	YY_BREAK
case 137:
YY_RULE_SETUP
#line 242 "fortran.lex"
{ return TOK_MAXLOC; }
	YY_BREAK
case 138:
YY_RULE_SETUP
#line 243 "fortran.lex"
{ return TOK_MINVAL; }
	YY_BREAK
case 139:
YY_RULE_SETUP
#line 244 "fortran.lex"
{ return TOK_BACKSPACE; }
	YY_BREAK
case 140:
YY_RULE_SETUP
#line 245 "fortran.lex"
{ return TOK_FOURDOTS;  }
	YY_BREAK
case 141:
YY_RULE_SETUP
#line 246 "fortran.lex"
{ return TOK_LEFTAB; }
	YY_BREAK
case 142:
YY_RULE_SETUP
#line 247 "fortran.lex"
{ return TOK_RIGHTAB; }
	YY_BREAK
case 143:
/* rule 143 can match eol */
YY_RULE_SETUP
#line 248 "fortran.lex"
{
                              return TOK_FORMAT; }
	YY_BREAK
case 144:
YY_RULE_SETUP
#line 250 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASH; }
	YY_BREAK
case 145:
YY_RULE_SETUP
#line 251 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
	YY_BREAK
case 146:
/* rule 146 can match eol */
YY_RULE_SETUP
#line 252 "fortran.lex"
{
                              strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
	YY_BREAK
case 147:
/* rule 147 can match eol */
YY_RULE_SETUP
#line 254 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
	YY_BREAK
case 148:
/* rule 148 can match eol */
YY_RULE_SETUP
#line 255 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
	YY_BREAK
case 149:
YY_RULE_SETUP
#line 256 "fortran.lex"
{ BEGIN(donottreat); }
	YY_BREAK
case 150:
/* rule 150 can match eol */
YY_RULE_SETUP
#line 257 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 151:
YY_RULE_SETUP
#line 258 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME; }
	YY_BREAK
case 152:
/* rule 152 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 259 "fortran.lex"
{  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 153:
YY_RULE_SETUP
#line 261 "fortran.lex"
{  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 154:
YY_RULE_SETUP
#line 263 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CSTINT; }
	YY_BREAK
case 155:
YY_RULE_SETUP
#line 264 "fortran.lex"
{}
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 265 "fortran.lex"
{}
	YY_BREAK
case 157:
YY_RULE_SETUP
#line 266 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 158:
YY_RULE_SETUP
#line 267 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 159:
YY_RULE_SETUP
#line 268 "fortran.lex"
{ return TOK_SEMICOLON; }
	YY_BREAK
case 160:
YY_RULE_SETUP
#line 269 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 161:
YY_RULE_SETUP
#line 270 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 162:
YY_RULE_SETUP
#line 271 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 163:
YY_RULE_SETUP
#line 272 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 164:
/* rule 164 can match eol */
YY_RULE_SETUP
#line 273 "fortran.lex"
{ INCREMENT_LINE_NUM() ; return '\n'; }
	YY_BREAK
case 165:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 274 "fortran.lex"
{}
	YY_BREAK
case 166:
YY_RULE_SETUP
#line 275 "fortran.lex"
{}
	YY_BREAK
case 167:
YY_RULE_SETUP
#line 276 "fortran.lex"
{ if (newlinef90 == 0) return TOK_LABEL; else newlinef90 = 0; }
	YY_BREAK
case 168:
/* rule 168 can match eol */
YY_RULE_SETUP
#line 277 "fortran.lex"
{ INCREMENT_LINE_NUM() ; newlinef90=1; }
	YY_BREAK
case 169:
/* rule 169 can match eol */
YY_RULE_SETUP
#line 278 "fortran.lex"
{ INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 170:
/* rule 170 can match eol */
YY_RULE_SETUP
#line 280 "fortran.lex"
{ INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
	YY_BREAK
case 171:
/* rule 171 can match eol */
YY_RULE_SETUP
#line 281 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 172:
/* rule 172 can match eol */
YY_RULE_SETUP
#line 282 "fortran.lex"
{ INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 173:
/* rule 173 can match eol */
YY_RULE_SETUP
#line 283 "fortran.lex"
{ INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 174:
/* rule 174 can match eol */
YY_RULE_SETUP
#line 284 "fortran.lex"
{ INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 175:
YY_RULE_SETUP
#line 285 "fortran.lex"
{}
	YY_BREAK
case 176:
YY_RULE_SETUP
#line 286 "fortran.lex"
ECHO;
	YY_BREAK
#line 3015 "fortran.yy.c"
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
	yyterminate();

	case YY_END_OF_BUFFER:
		{
		/* Amount of text matched not including the EOB char. */
		int yy_amount_of_matched_text = (int) (yy_cp - (yytext_ptr)) - 1;

		/* Undo the effects of YY_DO_BEFORE_ACTION. */
		*yy_cp = (yy_hold_char);
		YY_RESTORE_YY_MORE_OFFSET

		if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_NEW )
			{
			/* We're scanning a new file or input source.  It's
			 * possible that this happened because the user
			 * just pointed fortran_in at a new source and called
			 * fortran_lex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = fortran_in;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status = YY_BUFFER_NORMAL;
			}

		/* Note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the
		 * end-of-buffer state).  Contrast this with the test
		 * in input().
		 */
		if ( (yy_c_buf_p) <= &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			{ /* This was really a NUL. */
			yy_state_type yy_next_state;

			(yy_c_buf_p) = (yytext_ptr) + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state(  );

			/* Okay, we're now positioned to make the NUL
			 * transition.  We couldn't have
			 * yy_get_previous_state() go ahead and do it
			 * for us because it doesn't know how to deal
			 * with the possibility of jamming (and we don't
			 * want to build jamming into it because then it
			 * will run more slowly).
			 */

			yy_next_state = yy_try_NUL_trans( yy_current_state );

			yy_bp = (yytext_ptr) + YY_MORE_ADJ;

			if ( yy_next_state )
				{
				/* Consume the NUL. */
				yy_cp = ++(yy_c_buf_p);
				yy_current_state = yy_next_state;
				goto yy_match;
				}

			else
				{
				yy_cp = (yy_c_buf_p);
				goto yy_find_action;
				}
			}

		else switch ( yy_get_next_buffer(  ) )
			{
			case EOB_ACT_END_OF_FILE:
				{
				(yy_did_buffer_switch_on_eof) = 0;

				if ( fortran_wrap( ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * fortran_text, we can now set up
					 * yy_c_buf_p so that if some total
					 * hoser (like flex itself) wants to
					 * call the scanner after we return the
					 * YY_NULL, it'll still work - another
					 * YY_NULL will get returned.
					 */
					(yy_c_buf_p) = (yytext_ptr) + YY_MORE_ADJ;

					yy_act = YY_STATE_EOF(YY_START);
					goto do_action;
					}

				else
					{
					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
					}
				break;
				}

			case EOB_ACT_CONTINUE_SCAN:
				(yy_c_buf_p) =
					(yytext_ptr) + yy_amount_of_matched_text;

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_match;

			case EOB_ACT_LAST_MATCH:
				(yy_c_buf_p) =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)];

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_find_action;
			}
		break;
		}

	default:
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	} /* end of action switch */
		} /* end of scanning one token */
} /* end of fortran_lex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	register char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	register char *source = (yytext_ptr);
	register int number_to_move, i;
	int ret_val;

	if ( (yy_c_buf_p) > &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] )
		YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

	if ( YY_CURRENT_BUFFER_LVALUE->yy_fill_buffer == 0 )
		{ /* Don't try to fill the buffer, so this is an EOF. */
		if ( (yy_c_buf_p) - (yytext_ptr) - YY_MORE_ADJ == 1 )
			{
			/* We matched a single character, the EOB, so
			 * treat this as a final EOF.
			 */
			return EOB_ACT_END_OF_FILE;
			}

		else
			{
			/* We matched some text prior to the EOB, first
			 * process it.
			 */
			return EOB_ACT_LAST_MATCH;
			}
		}

	/* Try to read more data. */

	/* First move last chars to start of buffer. */
	number_to_move = (int) ((yy_c_buf_p) - (yytext_ptr)) - 1;

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			yy_size_t num_to_read =
			YY_CURRENT_BUFFER_LVALUE->yy_buf_size - number_to_move - 1;

		while ( num_to_read <= 0 )
			{ /* Not enough room in the buffer - grow it. */

			/* just a shorter name for the current buffer */
			YY_BUFFER_STATE b = YY_CURRENT_BUFFER;

			int yy_c_buf_p_offset =
				(int) ((yy_c_buf_p) - b->yy_ch_buf);

			if ( b->yy_is_our_buffer )
				{
				yy_size_t new_size = b->yy_buf_size * 2;

				if ( new_size <= 0 )
					b->yy_buf_size += b->yy_buf_size / 8;
				else
					b->yy_buf_size *= 2;

				b->yy_ch_buf = (char *)
					/* Include room in for 2 EOB chars. */
					fortran_realloc((void *) b->yy_ch_buf,b->yy_buf_size + 2  );
				}
			else
				/* Can't grow it, we don't own it. */
				b->yy_ch_buf = 0;

			if ( ! b->yy_ch_buf )
				YY_FATAL_ERROR(
				"fatal error - scanner input buffer overflow" );

			(yy_c_buf_p) = &b->yy_ch_buf[yy_c_buf_p_offset];

			num_to_read = YY_CURRENT_BUFFER_LVALUE->yy_buf_size -
						number_to_move - 1;

			}

		if ( num_to_read > YY_READ_BUF_SIZE )
			num_to_read = YY_READ_BUF_SIZE;

		/* Read in more data. */
		YY_INPUT( (&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move]),
			(yy_n_chars), num_to_read );

		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	if ( (yy_n_chars) == 0 )
		{
		if ( number_to_move == YY_MORE_ADJ )
			{
			ret_val = EOB_ACT_END_OF_FILE;
			fortran_restart(fortran_in  );
			}

		else
			{
			ret_val = EOB_ACT_LAST_MATCH;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status =
				YY_BUFFER_EOF_PENDING;
			}
		}

	else
		ret_val = EOB_ACT_CONTINUE_SCAN;

	if ((yy_size_t) ((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		yy_size_t new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) fortran_realloc((void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf,new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
	}

	(yy_n_chars) += number_to_move;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] = YY_END_OF_BUFFER_CHAR;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] = YY_END_OF_BUFFER_CHAR;

	(yytext_ptr) = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[0];

	return ret_val;
}

/* yy_get_previous_state - get the state just before the EOB char was reached */

    static yy_state_type yy_get_previous_state (void)
{
	register yy_state_type yy_current_state;
	register char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		register YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		if ( yy_accept[yy_current_state] )
			{
			(yy_last_accepting_state) = yy_current_state;
			(yy_last_accepting_cpos) = yy_cp;
			}
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1132 )
				yy_c = yy_meta[(unsigned int) yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
		}

	return yy_current_state;
}

/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *	next_state = yy_try_NUL_trans( current_state );
 */
    static yy_state_type yy_try_NUL_trans  (yy_state_type yy_current_state )
{
	register int yy_is_jam;
    	register char *yy_cp = (yy_c_buf_p);

	register YY_CHAR yy_c = 1;
	if ( yy_accept[yy_current_state] )
		{
		(yy_last_accepting_state) = yy_current_state;
		(yy_last_accepting_cpos) = yy_cp;
		}
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1132 )
			yy_c = yy_meta[(unsigned int) yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
	yy_is_jam = (yy_current_state == 1131);

	return yy_is_jam ? 0 : yy_current_state;
}

    static void yyunput (int c, register char * yy_bp )
{
	register char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up fortran_text */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		register yy_size_t number_to_move = (yy_n_chars) + 2;
		register char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		register char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#ifndef YY_NO_INPUT
#ifdef __cplusplus
    static int yyinput (void)
#else
    static int input  (void)
#endif

{
	int c;
    
	*(yy_c_buf_p) = (yy_hold_char);

	if ( *(yy_c_buf_p) == YY_END_OF_BUFFER_CHAR )
		{
		/* yy_c_buf_p now points to the character we want to return.
		 * If this occurs *before* the EOB characters, then it's a
		 * valid NUL; if not, then we've hit the end of the buffer.
		 */
		if ( (yy_c_buf_p) < &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			/* This was really a NUL. */
			*(yy_c_buf_p) = '\0';

		else
			{ /* need more input */
			yy_size_t offset = (yy_c_buf_p) - (yytext_ptr);
			++(yy_c_buf_p);

			switch ( yy_get_next_buffer(  ) )
				{
				case EOB_ACT_LAST_MATCH:
					/* This happens because yy_g_n_b()
					 * sees that we've accumulated a
					 * token and flags that we need to
					 * try matching the token before
					 * proceeding.  But for input(),
					 * there's no matching to consider.
					 * So convert the EOB_ACT_LAST_MATCH
					 * to EOB_ACT_END_OF_FILE.
					 */

					/* Reset buffer status. */
					fortran_restart(fortran_in );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( fortran_wrap( ) )
						return 0;

					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
#ifdef __cplusplus
					return yyinput();
#else
					return input();
#endif
					}

				case EOB_ACT_CONTINUE_SCAN:
					(yy_c_buf_p) = (yytext_ptr) + offset;
					break;
				}
			}
		}

	c = *(unsigned char *) (yy_c_buf_p);	/* cast for 8-bit char's */
	*(yy_c_buf_p) = '\0';	/* preserve fortran_text */
	(yy_hold_char) = *++(yy_c_buf_p);

	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = (c == '\n');

	return c;
}
#endif	/* ifndef YY_NO_INPUT */

/** Immediately switch to a different input stream.
 * @param input_file A readable stream.
 * 
 * @note This function does not reset the start condition to @c INITIAL .
 */
    void fortran_restart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        fortran_ensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            fortran__create_buffer(fortran_in,YY_BUF_SIZE );
	}

	fortran__init_buffer(YY_CURRENT_BUFFER,input_file );
	fortran__load_buffer_state( );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void fortran__switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		fortran_pop_buffer_state();
	 *		fortran_push_buffer_state(new_buffer);
     */
	fortran_ensure_buffer_stack ();
	if ( YY_CURRENT_BUFFER == new_buffer )
		return;

	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	YY_CURRENT_BUFFER_LVALUE = new_buffer;
	fortran__load_buffer_state( );

	/* We don't actually know whether we did this switch during
	 * EOF (fortran_wrap()) processing, but the only time this flag
	 * is looked at is after fortran_wrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void fortran__load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	fortran_in = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE fortran__create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) fortran_alloc(b->yy_buf_size + 2  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_is_our_buffer = 1;

	fortran__init_buffer(b,file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with fortran__create_buffer()
 * 
 */
    void fortran__delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		fortran_free((void *) b->yy_ch_buf  );

	fortran_free((void *) b  );
}

#ifndef __cplusplus
extern int isatty (int );
#endif /* __cplusplus */
    
/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a fortran_restart() or at EOF.
 */
    static void fortran__init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	fortran__flush_buffer(b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then fortran__init_buffer was _probably_
     * called from fortran_restart() or through yy_get_next_buffer.
     * In that case, we don't want to reset the lineno or column.
     */
    if (b != YY_CURRENT_BUFFER){
        b->yy_bs_lineno = 1;
        b->yy_bs_column = 0;
    }

        b->yy_is_interactive = file ? (isatty( fileno(file) ) > 0) : 0;
    
	errno = oerrno;
}

/** Discard all buffered characters. On the next scan, YY_INPUT will be called.
 * @param b the buffer state to be flushed, usually @c YY_CURRENT_BUFFER.
 * 
 */
    void fortran__flush_buffer (YY_BUFFER_STATE  b )
{
    	if ( ! b )
		return;

	b->yy_n_chars = 0;

	/* We always need two end-of-buffer characters.  The first causes
	 * a transition to the end-of-buffer state.  The second causes
	 * a jam in that state.
	 */
	b->yy_ch_buf[0] = YY_END_OF_BUFFER_CHAR;
	b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;

	b->yy_buf_pos = &b->yy_ch_buf[0];

	b->yy_at_bol = 1;
	b->yy_buffer_status = YY_BUFFER_NEW;

	if ( b == YY_CURRENT_BUFFER )
		fortran__load_buffer_state( );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	fortran_ensure_buffer_stack();

	/* This block is copied from fortran__switch_to_buffer. */
	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	/* Only push if top exists. Otherwise, replace top. */
	if (YY_CURRENT_BUFFER)
		(yy_buffer_stack_top)++;
	YY_CURRENT_BUFFER_LVALUE = new_buffer;

	/* copied from fortran__switch_to_buffer. */
	fortran__load_buffer_state( );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void fortran_pop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	fortran__delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		fortran__load_buffer_state( );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void fortran_ensure_buffer_stack (void)
{
	yy_size_t num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
		num_to_alloc = 1;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_alloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );
								  
		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));
				
		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		int grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_realloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );

		/* zero only the new slots.*/
		memset((yy_buffer_stack) + (yy_buffer_stack_max), 0, grow_size * sizeof(struct yy_buffer_state*));
		(yy_buffer_stack_max) = num_to_alloc;
	}
}

/** Setup the input buffer state to scan directly from a user-specified character buffer.
 * @param base the character buffer
 * @param size the size in bytes of the character buffer
 * 
 * @return the newly allocated buffer state object. 
 */
YY_BUFFER_STATE fortran__scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return 0;

	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_buffer()" );

	b->yy_buf_size = size - 2;	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = 0;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	fortran__switch_to_buffer(b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to fortran_lex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       fortran__scan_bytes() instead.
 */
YY_BUFFER_STATE fortran__scan_string (yyconst char * yystr )
{
    
	return fortran__scan_bytes(yystr,strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to fortran_lex() will
 * scan from a @e copy of @a bytes.
 * @param bytes the byte buffer to scan
 * @param len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE fortran__scan_bytes  (yyconst char * yybytes, yy_size_t  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n, i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = _yybytes_len + 2;
	buf = (char *) fortran_alloc(n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = fortran__scan_buffer(buf,n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in fortran__scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yy_fatal_error (yyconst char* msg )
{
    	(void) fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		fortran_text[fortran_leng] = (yy_hold_char); \
		(yy_c_buf_p) = fortran_text + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		fortran_leng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int fortran_get_lineno  (void)
{
        
    return fortran_lineno;
}

/** Get the input stream.
 * 
 */
FILE *fortran_get_in  (void)
{
        return fortran_in;
}

/** Get the output stream.
 * 
 */
FILE *fortran_get_out  (void)
{
        return fortran_out;
}

/** Get the length of the current token.
 * 
 */
yy_size_t fortran_get_leng  (void)
{
        return fortran_leng;
}

/** Get the current token.
 * 
 */

char *fortran_get_text  (void)
{
        return fortran_text;
}

/** Set the current line number.
 * @param line_number
 * 
 */
void fortran_set_lineno (int  line_number )
{
    
    fortran_lineno = line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param in_str A readable stream.
 * 
 * @see fortran__switch_to_buffer
 */
void fortran_set_in (FILE *  in_str )
{
        fortran_in = in_str ;
}

void fortran_set_out (FILE *  out_str )
{
        fortran_out = out_str ;
}

int fortran_get_debug  (void)
{
        return fortran__flex_debug;
}

void fortran_set_debug (int  bdebug )
{
        fortran__flex_debug = bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from fortran_lex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = 0;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = (char *) 0;
    (yy_init) = 0;
    (yy_start) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    fortran_in = stdin;
    fortran_out = stdout;
#else
    fortran_in = (FILE *) 0;
    fortran_out = (FILE *) 0;
#endif

    /* For future reference: Set errno on error, since we are called by
     * fortran_lex_init()
     */
    return 0;
}

/* fortran_lex_destroy is for both reentrant and non-reentrant scanners. */
int fortran_lex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		fortran__delete_buffer(YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		fortran_pop_buffer_state();
	}

	/* Destroy the stack itself. */
	fortran_free((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * fortran_lex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, yyconst char * s2, int n )
{
	register int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * s )
{
	register int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *fortran_alloc (yy_size_t  size )
{
	return (void *) malloc( size );
}

void *fortran_realloc  (void * ptr, yy_size_t  size )
{
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return (void *) realloc( (char *) ptr, size );
}

void fortran_free (void * ptr )
{
	free( (char *) ptr );	/* see fortran_realloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 286 "fortran.lex"



void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}

