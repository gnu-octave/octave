/* parse.y						-*- text -*-

Copyright (C) 1992, 1993, 1994 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Parser for Octave.

/*
 * C decarations.
 */
%{
#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "SLStack.h"

#include "Matrix.h"

#include "error.h"
#include "variables.h"
#include "octave-hist.h"
#include "user-prefs.h"
#include "input.h"
#include "utils.h"
#include "tree.h"
#include "tree-plot.h"
#include "tree-const.h"
#include "symtab.h"
#include "builtins.h"
#include "octave.h"
#include "parse.h"
#include "lex.h"
#include "token.h"

// Nonzero means we're in the middle of defining a function.
int defining_func = 0;

// Nonzero means we're in the middle of defining a loop.
int looping = 0;

// Nonzero means we're in the middle of defining a conditional expression.
int iffing = 0;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
int maybe_screwed = 0;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
int maybe_screwed_again = 0;

// Temporary symbol table pointer used to cope with bogus function syntax.
symbol_table *tmp_local_sym_tab = (symbol_table *) NULL;

// Stack to hold list of literal matrices.
SLStack <tree_matrix *> ml;

// A nonzero element corresponding to an element of ml means we just
// started reading a new matrix.  This should probably be part of a
// new struct for matrix lists... 
SLStack <int> mlnm;

// The current input line number.
int input_line_number = 0;

// The column of the current token.
int current_input_column = 1;

// Buffer for help text snagged from function files.
// Probably shouldn't be a fixed size...
char help_buf [HELP_BUF_LENGTH];

// Nonzero means we're working on a plot command.
int plotting = 0;

// Nonzero means we've seen something that means we must be past the
// range part of a plot command.
int past_plot_range = 0;

// Nonzero means we're looking at the range part of a plot command.
int in_plot_range = 0;

// Nonzero means we're looking at the using part of a plot command.
int in_plot_using = 0;

// Nonzero means we're looking at the style part of a plot command.
int in_plot_style = 0;

// Forward declarations for some functions defined at the bottom of
// the file.

// Generic error messages.
static void yyerror (char *s);

// Error mesages for mismatched end statements.
static void end_error (char *type, token::end_tok_type ettype, int l, int c);

// Check to see that end statements are properly matched.
static int check_end (token *tok, token::end_tok_type expected);

// Try to figure out early if an expression should become an
// assignment to the builtin variable ans.
static tree_expression *maybe_convert_to_ans_assign (tree_expression *expr);

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.
static void maybe_warn_assign_as_truth_value (tree_expression *expr);

#define ABORT_PARSE \
  do \
    { \
      global_command = NULL_TREE; \
      reset_parser (); \
      yyerrok; \
      if (interactive) \
	YYACCEPT; \
      else \
	YYABORT; \
    } \
  while (0)

%}

/*
 * Bison declarations.
 */
%union
{
// The type of the basic tokens returned by the lexer.
  token *tok_val;

// Types for the nonterminals we generate.
  tree *tree_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
  tree_matrix *tree_matrix_type;
  tree_identifier *tree_identifier_type;
  tree_function *tree_function_type;
  tree_index_expression *tree_index_expression_type;
  tree_colon_expression *tree_colon_expression_type;
  tree_argument_list *tree_argument_list_type;
  tree_parameter_list *tree_parameter_list_type;
  tree_command *tree_command_type;
  tree_if_command *tree_if_command_type;
  tree_global_command *tree_global_command_type;
  tree_command_list *tree_command_list_type;
  tree_plot_command *tree_plot_command_type;
  tree_subplot_list *tree_subplot_list_type;
  tree_plot_limits *tree_plot_limits_type;
  tree_plot_range *tree_plot_range_type;
  tree_subplot_using *tree_subplot_using_type;
  tree_subplot_style *tree_subplot_style_type;
}

// Tokens with line and column information.
%token <tok_val> '=' ':' '-' '+' '*' '/'
%token <tok_val> EXPR_AND_AND EXPR_OR_OR
%token <tok_val> EXPR_AND EXPR_OR EXPR_NOT
%token <tok_val> EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%token <tok_val> LEFTDIV EMUL EDIV ELEFTDIV QUOTE TRANSPOSE
%token <tok_val> PLUS_PLUS MINUS_MINUS POW EPOW
%token <tok_val> NUM IMAG_NUM
%token <tok_val> NAME SCREW
%token <tok_val> END
%token <tok_val> PLOT
%token <tok_val> TEXT STYLE
%token <tok_val> FOR WHILE IF ELSEIF ELSE BREAK CONTINUE FUNC_RET

// Other tokens.
%token FCN SCREW_TWO
%token GLOBAL
%token ELLIPSIS
%token END_OF_INPUT
%token USING TITLE WITH COLON OPEN_BRACE CLOSE_BRACE

// Nonterminals we construct.
%type <tree_type> input command 
%type <tree_expression_type> expression simple_expr simple_expr1
%type <tree_expression_type> ans_expression title
%type <tree_matrix_type> matrix
%type <tree_identifier_type> identifier
%type <tree_function_type> func_def func_def1 func_def2 func_def3
%type <tree_index_expression_type> variable word_list_cmd
%type <tree_colon_expression_type> colon_expr
%type <tree_argument_list_type> arg_list arg_list1 word_list word_list1
%type <tree_parameter_list_type> param_list param_list1 func_def1a 
%type <tree_command_type> statement
%type <tree_if_command_type> elseif
%type <tree_global_command_type> global_decl global_decl1
%type <tree_command_list_type> simple_list simple_list1 list list1 opt_list
%type <tree_plot_command_type> plot_command 
%type <tree_subplot_list_type> plot_command1 plot_command2 plot_options
%type <tree_plot_limits_type> ranges
%type <tree_plot_range_type> ranges1 
%type <tree_subplot_using_type> using using1 
%type <tree_subplot_style_type> style

// Precedence and associativity.
%left ';' ',' '\n'
%right '='
%left EXPR_AND_AND EXPR_OR_OR
%left EXPR_AND EXPR_OR
%left EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%left ':'
%left '-' '+'
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%left QUOTE TRANSPOSE
%left UNARY PLUS_PLUS MINUS_MINUS EXPR_NOT
%right POW EPOW

// There are 19 shift/reduce conflicts, ok?
%expect 19

// Where to start.
%start input

/*
 * Grammar rules.
 */
%%

input		: '\n'
		  {
		    global_command = NULL_TREE;
		    promptflag = 1;
		    YYACCEPT;
		  }
		| END_OF_INPUT
		  {
		    global_command = NULL_TREE;
		    promptflag = 1;
		    YYABORT;
		  }
		| simple_list
		  {
		    global_command = $1;
		    promptflag = 1;
		    YYACCEPT;
		  }
		| simple_list '\n'
		  {
		    global_command = $1;
		    promptflag = 1;
		    YYACCEPT;
		  }
		| simple_list END_OF_INPUT
		  {
		    global_command = $1;
		    promptflag = 1;
		    YYACCEPT;
		  }
		| error
		  { ABORT_PARSE; }
		| error '\n'
		  { ABORT_PARSE; }
		| error END_OF_INPUT
		  { ABORT_PARSE; }
		| simple_list error
		  { ABORT_PARSE; }
		| simple_list error '\n'
		  { ABORT_PARSE; }
		| simple_list error END_OF_INPUT
		  { ABORT_PARSE; }
		;

simple_list	: semi_comma
		  { $$ = (tree_command_list *) NULL; }
		| comma_semi
		  { $$ = (tree_command_list *) NULL; }
		| simple_list1
		  { $$ = $1->reverse (); }
		| simple_list1 semi_comma
		  {
		    $1->set_print_flag (0);
		    $$ = $1->reverse ();
		  }
		| simple_list1 comma_semi
		  { $$ = $1->reverse (); }
		;

simple_list1	: command
		  { $$ = new tree_command_list ($1); }
		| semi_comma command
		  { $$ = new tree_command_list ($2); }
		| comma_semi command
		  { $$ = new tree_command_list ($2); }
		| simple_list1 semi_comma command
		  {
		    $1->set_print_flag (0);
		    $$ = $1->chain ($3);
		  }
		| simple_list1 comma_semi command
		  { $$ = $1->chain ($3); }
		;

semi_comma	: ';'
		| semi_comma ','
		| semi_comma ';'
		;

comma_semi	: ','
		| comma_semi ';'
		| comma_semi ','
		;

comma_nl_sep	: ','
		| '\n'
		| comma_nl_sep sep
		;

semi_sep	: ';'
		| semi_sep sep
		;

opt_list	: // empty
		  { $$ = new tree_command_list (); }
		| list
		  { $$ = $1; }
		;

list		: list1
		  { $$ = $1->reverse (); }
		| list1 comma_nl_sep
		  { $$ = $1->reverse (); }
		| list1 semi_sep
		  {
		    $1->set_print_flag (0);
		    $$ = $1->reverse ();
		  }
		;

list1		: command
		  {
		    beginning_of_function = 0;
		    $$ = new tree_command_list ($1);
		  }
		| list1 comma_nl_sep command
		  { $$ = $1->chain ($3); }
		| list1 semi_sep command
		  {
		    $1->set_print_flag (0);
		    $$ = $1->chain ($3);
		  }
		;

command		: plot_command
		  { $$ = $1; }
		| statement
		  { $$ = $1; }
		| ans_expression
		  { $$ = $1; }
		| func_def
		  { $$ = $1; }
		| global_decl
		  { $$ = $1; }
		;

plot_command	: PLOT plot_command1
		  {
		    tree_subplot_list *tmp = (tree_subplot_list *) NULL;
		    if ($2 != (tree_subplot_list *) NULL)
		      tmp = $2->reverse ();

		    if (tmp == (tree_subplot_list *) NULL
			&& $1->pttype () != token::replot)
		      {
			yyerror ("must have something to plot");
			ABORT_PARSE;
		      }
		    else
		      {
			$$ = new tree_plot_command (tmp, $1->pttype ());
			plotting = 0;
			past_plot_range = 0;
			in_plot_range = 0;
			in_plot_using = 0;
			in_plot_style = 0;
		      }
		  }
		| PLOT ranges plot_command1
		  {
		    if ($1->pttype () == token::replot)
		      {
			yyerror ("cannot specify new ranges with replot");
			ABORT_PARSE;
		      }
		    else
		      {
			tree_subplot_list *tmp = $3->reverse ();
			$$ = new tree_plot_command (tmp, $2, $1->pttype ());
			plotting = 0;
			past_plot_range = 0;
			in_plot_range = 0;
			in_plot_using = 0;
			in_plot_style = 0;
		      }
		  }
		;

ranges		: ranges1
		  { $$ = new tree_plot_limits ($1); }
		| ranges1 ranges1
		  { $$ = new tree_plot_limits ($1, $2); }
		| ranges1 ranges1 ranges1
		  { $$ = new tree_plot_limits ($1, $2, $3); }
		;

ranges1		: OPEN_BRACE expression COLON expression CLOSE_BRACE
		  { $$ = new tree_plot_range ($2, $4); }
		| OPEN_BRACE COLON expression CLOSE_BRACE
		  { $$ = new tree_plot_range (NULL, $3); }
		| OPEN_BRACE expression COLON CLOSE_BRACE
		  { $$ = new tree_plot_range ($2, NULL); }
		| OPEN_BRACE COLON CLOSE_BRACE
		  { $$ = new tree_plot_range (); }
		| OPEN_BRACE CLOSE_BRACE
		  { $$ = new tree_plot_range (); }
		;

plot_command1	: // empty
		  { $$ = (tree_subplot_list *) NULL; }
		| plot_command2
		  { $$ = $1; }
		| plot_command1 ',' plot_command2
		  { $$ = $1->chain ($3); }
		;

plot_command2	: expression
		  { $$ = new tree_subplot_list ($1); }
		| expression plot_options
		  { $$ = $2->set_data ($1); }
		;

plot_options	: using
		  { $$ = new tree_subplot_list ($1, NULL, NULL); }
		| title
		  { $$ = new tree_subplot_list (NULL, $1, NULL); }
		| style
		  { $$ = new tree_subplot_list (NULL, NULL, $1); }
		| using title
		  { $$ = new tree_subplot_list ($1, $2, NULL); }
		| title using
		  { $$ = new tree_subplot_list ($2, $1, NULL); }
		| using style
		  { $$ = new tree_subplot_list ($1, NULL, $2); }
		| style using
		  { $$ = new tree_subplot_list ($2, NULL, $1); }
		| title style
		  { $$ = new tree_subplot_list (NULL, $1, $2); }
		| style title
		  { $$ = new tree_subplot_list (NULL, $2, $1); }
		| using title style
		  { $$ = new tree_subplot_list ($1, $2, $3); }
		| using style title
		  { $$ = new tree_subplot_list ($1, $3, $2); }
		| title using style
		  { $$ = new tree_subplot_list ($2, $1, $3); }
		| title style using
		  { $$ = new tree_subplot_list ($3, $1, $2); }
		| style using title
		  { $$ = new tree_subplot_list ($2, $3, $1); }
		| style title using
		  { $$ = new tree_subplot_list ($3, $2, $1); }
		;

using		: using1
		  {
		    in_plot_using = 0;
		    $$ = $1;
		  }
		| using1 expression
		  {
		    in_plot_using = 0;
		    $$ = $1->set_format ($2);
		  }
		;

using1		: USING expression
		  {
		    tree_subplot_using *tmp = new tree_subplot_using ();
		    $$ = tmp->add_qualifier ($2);
		  }
		| using1 COLON expression
		  { $$ = $1->add_qualifier ($3); }
		;

title		: TITLE expression
		  { $$ = $2; }
		;

style		: WITH STYLE
		  { $$ = new tree_subplot_style ($2->string ()); }
		| WITH STYLE expression
		  { $$ = new tree_subplot_style ($2->string (), $3); }
		| WITH STYLE expression bogus_syntax expression
		  { $$ = new tree_subplot_style ($2->string (), $3, $5); }
		;

bogus_syntax	: // empty
		;

ans_expression	: expression
		  { $$ = maybe_convert_to_ans_assign ($1); }
		;

global_decl	: GLOBAL global_decl1
		  { $$ = $2->reverse (); }
		| GLOBAL global_decl1 ','
		  { $$ = $2->reverse (); }
		;

global_decl1	: NAME
		  {
		    $$ = new tree_global_command
			   ($1->sym_rec (), $1->line (), $1->column ());
		  }
		| NAME '=' expression
		  {
		    $$ = new tree_global_command
			   ($1->sym_rec (), $3, $1->line (), $1->column ());
		  }
		| global_decl1 optcomma NAME
		  {
		    $$ = $1->chain ($3->sym_rec (), $3->line (),
				    $3->column ());
		  }
		| global_decl1 optcomma NAME '=' expression
		  {
		    $$ = $1->chain ($3->sym_rec (), $5, $3->line (),
				    $3->column ());
		  }
		;

optcomma	: // empty
		| ','
		  {
		    if (user_pref.warn_comma_in_global_decl)
		      warning ("comma in global declaration not\
 interpreted as a command separator");
		  }
		;

statement	: WHILE expression optsep opt_list END
		  {
		    maybe_warn_assign_as_truth_value ($2);
		    if (check_end ($5, token::while_end))
		      ABORT_PARSE;
		    looping--;
		    $$ = new tree_while_command ($2, $4, $1->line (),
						 $1->column ());
		  }
		| FOR variable '=' expression optsep opt_list END
		  {
		    if (check_end ($7, token::for_end))
		      ABORT_PARSE;
		    looping--;
		    $$ = new tree_for_command ($2, $4, $6,
					       $1->line (), $1->column ());
		  }
		| IF expression optsep opt_list END
		  {
		    maybe_warn_assign_as_truth_value ($2);
		    if (check_end ($5, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    $$ = new tree_if_command ($2, $4,
					      $1->line (), $1->column ());
		  }
		| IF expression optsep opt_list ELSE optsep opt_list END
		  {
		    maybe_warn_assign_as_truth_value ($2);
		    if (check_end ($8, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    tree_if_command *t1 = new tree_if_command
					    ($7, $5->line (), $5->column ());
		    $$ = t1->chain ($2, $4, $1->line (), $1->column ());
		  }
		| IF expression optsep opt_list elseif END
		  {
		    maybe_warn_assign_as_truth_value ($2);
		    if (check_end ($6, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    tree_if_command *t1 = $5->reverse ();
		    // Add the if list to the new head of the elseif
		    // list, and return the list.
		    $$ = t1->chain ($2, $4, $1->line (), $1->column ());
		  }
		| IF expression optsep opt_list elseif ELSE optsep opt_list END
		  {
		    maybe_warn_assign_as_truth_value ($2);
		    if (check_end ($9, token::if_end))
		      ABORT_PARSE;
		    iffing--;
		    // Add the else list to the head of the elseif list,
		    // then reverse the list.
		    tree_if_command *t1 = $5->chain ($8, $6->line (),
						     $6->column ());
		    t1 = t1->reverse ();
		    // Add the if list to the new head of the elseif
		    // list, and return the list.
		    $$ = t1->chain ($2, $4, $1->line (), $1->column ());
		  }
		| BREAK
		  {
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("break: only meaningful within a `for'\
 or `while' loop");
			ABORT_PARSE;
		      }
		    $$ = new tree_break_command ($1->line (), $1->column ());
		  }
		| CONTINUE
		  {
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("continue: only meaningful within a\
 `for' or `while' loop");
			ABORT_PARSE;
		      }
		    $$ = new tree_continue_command ($1->line (),
						    $1->column ());
		  }
		| FUNC_RET
		  {
		    if (!defining_func)
		      {
			yyerror ("parse error");
			error ("return: only meaningful within a function");
			ABORT_PARSE;
		      }
		    $$ = new tree_return_command ($1->line (), $1->column ());
		  }
		;

elseif		: ELSEIF optsep expression optsep opt_list
		  {
		    maybe_warn_assign_as_truth_value ($3);
		    $$ = new tree_if_command ($3, $5, $1->line (),
					      $1->column ());
		  }
		| elseif ELSEIF optsep expression optsep opt_list
		  {
		    maybe_warn_assign_as_truth_value ($4);
		    $$ = $1->chain ($4, $6, $2->line (), $2->column ());
		  }
		;

optsep		: // empty
		| sep
		;

sep		: ','
		| ';'
		| '\n'
		| sep ','
		| sep ';'
		| sep '\n'
		;

screwed_again	: // empty
		  { maybe_screwed_again++; }
		;

expression	: variable '=' expression
		  { $$ = new tree_simple_assignment_expression
		      ($1, $3, $2->line (), $2->column ()); }
		| '[' screwed_again matrix_row SCREW_TWO '=' expression
		  {

// Will need a way to convert the matrix list to a list of
// identifiers.	 If that fails, we can abort here, without losing
// anything -- no other possible syntax is valid if we've seen the
// equals sign as the next token after the `]'.

		    $$ = (tree_multi_assignment_expression *) NULL;
		    maybe_screwed_again--;
		    tree_matrix *tmp = ml.pop ();
		    tmp = tmp->reverse ();
		    tree_return_list *id_list = tmp->to_return_list ();
		    if (id_list == NULL_TREE)
		      {
			yyerror ("parse error");
			error ("invalid identifier list for assignment");
			$$ = (tree_multi_assignment_expression *) NULL;
			ABORT_PARSE;
		      }
		    else
		      $$ = new tree_multi_assignment_expression
			(id_list, $6, $5->line (), $5->column ());
		  }
		| NUM '=' expression
		  {
		    yyerror ("parse error");
		    error ("invalid assignment to a number");
		    $$ = (tree_simple_assignment_expression *) NULL;
		    ABORT_PARSE;
		  }
		| simple_expr
		  { $$ = $1; }
		;

simple_expr	: simple_expr1
		  { $$ = $1; }
		| identifier PLUS_PLUS
		  { $$ = new tree_postfix_expression
		      ($1, tree::increment, $2->line (), $2->column ()); }
		| identifier MINUS_MINUS
		  { $$ = new tree_postfix_expression
		      ($1, tree::decrement, $2->line (), $2->column ()); }
		| simple_expr QUOTE
		  { $$ = new tree_unary_expression
		      ($1, tree::hermitian, $2->line (), $2->column ()); }
		| simple_expr TRANSPOSE
		  { $$ = new tree_unary_expression
		      ($1, tree::transpose, $2->line (), $2->column ()); }
		| simple_expr POW simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::power, $2->line (), $2->column ()); }
		| simple_expr EPOW simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::elem_pow, $2->line (), $2->column ()); }
		| simple_expr '+' simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::add, $2->line (), $2->column ()); }
		| simple_expr '-' simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::subtract, $2->line (), $2->column ()); }
		| simple_expr '*' simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::multiply, $2->line (), $2->column ()); }
		| simple_expr '/' simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::divide, $2->line (), $2->column ()); }
		| simple_expr EMUL simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::el_mul, $2->line (), $2->column ()); }
		| simple_expr EDIV simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::el_div, $2->line (), $2->column ()); }
		| simple_expr LEFTDIV simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::leftdiv, $2->line (), $2->column ()); }
		| simple_expr ELEFTDIV simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::el_leftdiv, $2->line (), $2->column ()); }
		| simple_expr EXPR_LT simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::cmp_lt, $2->line (), $2->column ()); }
		| simple_expr EXPR_LE simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::cmp_le, $2->line (), $2->column ()); }
		| simple_expr EXPR_EQ simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::cmp_eq, $2->line (), $2->column ()); }
		| simple_expr EXPR_GE simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::cmp_ge, $2->line (), $2->column ()); }
		| simple_expr EXPR_GT simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::cmp_gt, $2->line (), $2->column ()); }
		| simple_expr EXPR_NE simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::cmp_ne, $2->line (), $2->column ()); }
		| simple_expr EXPR_AND_AND simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::and_and, $2->line (), $2->column ()); }
		| simple_expr EXPR_OR_OR simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::or_or, $2->line (), $2->column ()); }
		| simple_expr EXPR_AND simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::and, $2->line (), $2->column ()); }
		| simple_expr EXPR_OR simple_expr
		  { $$ = new tree_binary_expression
		      ($1, $3, tree::or, $2->line (), $2->column ()); }
		;

simple_expr1	: NUM
		  { $$ = new tree_constant ($1->number ()); }
		| IMAG_NUM
		  { $$ = new tree_constant (Complex (0.0, $1->number ())); }
		| TEXT
		  { $$ = new tree_constant ($1->string ()); }
		| '(' expression ')'
		  {
		    if ($2->is_assignment_expression ())
		      ((tree_assignment_expression *) $2) -> in_parens++;
		    $$ = $2;
		  }
		| word_list_cmd
		  { $$ = $1; }
		| variable
		  { $$ = $1; }
		| matrix
		  { $$ = $1; }
		| '[' ']'
		  {
		    mlnm.pop ();
		    $$ = new tree_constant (Matrix ());
		  }
		| '[' ';' ']'
		  {
		    mlnm.pop ();
		    $$ = new tree_constant (Matrix ());
		  }
		| colon_expr
		  { $$ = $1; }
		| PLUS_PLUS identifier %prec UNARY
		  { $$ = new tree_prefix_expression
		      ($2, tree::increment, $1->line (), $1->column ()); }
		| MINUS_MINUS identifier %prec UNARY
		  { $$ = new tree_prefix_expression
		      ($2, tree::decrement, $1->line (), $1->column ()); }
		| EXPR_NOT simple_expr
		  { $$ = new tree_unary_expression
		      ($2, tree::not, $1->line (), $1->column ()); }
		| '+' simple_expr %prec UNARY
		  { $$ = $2; }
		| '-' simple_expr %prec UNARY
		  { $$ = new tree_unary_expression
		      ($2, tree::uminus, $1->line (), $1->column ()); }
		;

colon_expr	: simple_expr ':' simple_expr
		  { $$ = new tree_colon_expression
		      ($1, $3, $2->line (), $2->column ()); }
		| colon_expr ':' simple_expr
		  {
		    $$ = $1->chain ($3);
		    if ($$ == (tree_colon_expression *) NULL)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  }
		;

word_list_cmd	: identifier word_list
		  {
		    $$ = new tree_index_expression
			   ($1, $2, $1->line (), $1->column ());
		  }
		;

word_list	: word_list1
		  { $$ = $1->reverse (); }
		;

word_list1	: TEXT
		  {
		    tree_constant *tmp = new tree_constant ($1->string ());
		    $$ = new tree_argument_list (tmp);
		  }
		| word_list1 TEXT
		  {
		    tree_constant *tmp = new tree_constant ($2->string ());
		    $$ = $1->chain (tmp);
		  }
		;

// This is truly disgusting.

g_symtab	: // empty
		  { curr_sym_tab = global_sym_tab; }
		;

local_symtab	: // empty
		  { curr_sym_tab = tmp_local_sym_tab; }
		;

safe		: // empty
		  { maybe_screwed = 0; }
		;

are_we_screwed	: // empty
		  { maybe_screwed = 1; }
		;

func_def	: FCN g_symtab are_we_screwed func_def1
		  {
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    $$ = (tree_function *) NULL;
		  }
		| FCN g_symtab are_we_screwed func_def2
		  {
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    $$ = (tree_function *) NULL;
		  }
		;

func_def1	: SCREW safe g_symtab '=' func_def2
		  {
		    tree_identifier *tmp = new tree_identifier
		      ($1->sym_rec (), $1->line (), $1->column ());
		    tree_parameter_list *tpl = new tree_parameter_list (tmp);
		    tpl = tpl->reverse ();
		    tpl->mark_as_formal_parameters ();
		    $$ = $5->define_ret_list (tpl);
		  }
		| func_def1a ']' g_symtab '=' func_def2
		  {
		    tree_parameter_list *tpl = $1->reverse ();
		    tpl->mark_as_formal_parameters ();
		    $$ = $5->define_ret_list (tpl);
		  }
		;

func_def1a	: '[' safe local_symtab identifier
		  { $$ = new tree_parameter_list ($4); }
		| func_def1a ',' identifier
		  { $$ = $1->chain ($3); }
		;

func_def2	: identifier safe local_symtab func_def3
		  {
		    char *id_name = $1->name ();
//		    if (is_text_function_name (id_name))
//		      {
//			yyerror ("parse error");
//			error ("invalid use of reserved word %s", id_name);
//			ABORT_PARSE;
//		      }

// If input is coming from a file, issue a warning if the name of the
// file does not match the name of the function stated in the file.
// Matlab doesn't provide a diagnostic (it ignores the stated name).

		    if (reading_fcn_file)
		      {
			if (strcmp (curr_fcn_file_name, id_name) != 0)
			  {
			    warning ("function name `%s' does not agree\
 with function file name `%s.m'", id_name, curr_fcn_file_name);

			    $1->rename (curr_fcn_file_name);
			    id_name = $1->name ();
			  }

			$4->stash_fcn_file_name (curr_fcn_file_name);
			$4->stash_fcn_file_time (time ((time_t *) NULL));
			$4->mark_as_system_fcn_file ();
		      }
		    else if (! (input_from_tmp_history_file
				|| input_from_startup_file)
			     && reading_script_file
			     && strcmp (curr_fcn_file_name, id_name) == 0)
		      {
			warning ("function `%s' defined within\
 script file `%s.m'", id_name, curr_fcn_file_name);
		      }

		    top_level_sym_tab->clear (id_name);

		    $4->stash_function_name (id_name);

		    $1->define ($4);
		    $1->document (help_buf);

		    $$ = $4;
		  }
		;

func_def3	: param_list optsep opt_list fcn_end_or_eof
		  {
		    tree_function *fcn = new tree_function ($3, curr_sym_tab);
		    $$ = fcn->define_param_list ($1);
		  }
		| optsep opt_list fcn_end_or_eof
		  { $$ = new tree_function ($2, curr_sym_tab); }
		;

fcn_end_or_eof	: END
		  {
		    if (check_end ($1, token::function_end))
			ABORT_PARSE;

		    if (reading_fcn_file)
		      check_for_garbage_after_fcn_def ();
		  }
		| END_OF_INPUT
		  {
		    if (! (reading_fcn_file || reading_script_file))
		      YYABORT;
		  }
		;

variable	: identifier
		  {
		    $$ = new tree_index_expression
			   ($1, $1->line (), $1->column ());
		  }
		| identifier '(' arg_list ')'
		  {
		    $$ = new tree_index_expression
			   ($1, $3, $1->line (), $1->column ());
		  }
		| identifier '(' ')'
		  {
		    $$ = new tree_index_expression
		           ($1, (tree_argument_list *) NULL,
			    $1->line (), $1->column ());
		  }
		| identifier '['
		  {
		    yyerror ("parse error");
		    error ("use `(\' and `)\' as index operators, not\
 `[\' and `]\'"); 
		    $$ = (tree_index_expression *) NULL;
		    ABORT_PARSE;
		  }
		;

param_list	: '(' ')'
		  {
		    quote_is_transpose = 0;
		    $$ = (tree_parameter_list *) NULL;
		  }
		| '(' ELLIPSIS ')'
		  {
		    quote_is_transpose = 0;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| param_list1 ')'
		  {
		    quote_is_transpose = 0;
		    tree_parameter_list *tmp = $1->reverse ();
		    tmp->mark_as_formal_parameters ();
		    $$ = tmp;
		  }
		| param_list1 ',' ELLIPSIS ')'
		  {
		    quote_is_transpose = 0;
		    tree_parameter_list *tmp = $1->reverse ();
		    tmp->mark_as_formal_parameters ();
		    tmp->mark_varargs ();
		    $$ = tmp;
		  }
		;

param_list1	: '(' identifier
		  { $$ = new tree_parameter_list ($2); }
		| param_list1 ',' identifier
		  { $$ = $1->chain ($3); }
		| '(' error
		  {
		    yyerror ("parse error");
		    error ("invalid parameter list");
		    ABORT_PARSE;
		  }
		| param_list1 ',' error
		  {
		    yyerror ("parse error");
		    error ("invalid parameter list");
		    ABORT_PARSE;
		  }
		;

identifier	: NAME
		  {
		    $$ = new tree_identifier
		      ($1->sym_rec (), $1->line (), $1->column ());
		  }
		;

arg_list	: arg_list1
		  { $$ = $1->reverse (); }
		;

arg_list1	: ':'
		  {
		    tree_constant *colon;
		    colon = new tree_constant (tree_constant_rep::magic_colon);
		    $$ = new tree_argument_list (colon);
		  }
		| arg_list1 ',' ':'
		  {
		    tree_constant *colon;
		    colon = new tree_constant (tree_constant_rep::magic_colon);
		    $$ = $1->chain (colon);
		    if ($$ == NULL_TREE)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  }
		| expression
		  { $$ = new tree_argument_list ($1); }
		| arg_list1 ',' expression
		  {
		    $$ = $1->chain ($3);
		    if ($$ == NULL_TREE)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
		  }
		;

matrix		: '[' screwed_again rows ']'
		  {
		    mlnm.pop ();
		    maybe_screwed_again--;
		    tree_matrix *tmp = ml.pop ();
		    $$ = tmp->reverse ();
		  }
		;

rows		: matrix_row
		| rows ';'	// Ignore trailing semicolon.
		| rows ';' matrix_row
		;

matrix_row	: expression		// First element on row.
		  {
		    if (mlnm.top ())
		      {
			mlnm.pop ();
			mlnm.push (0);
			tree_matrix *tmp = new tree_matrix ($1, tree::md_none);
			ml.push (tmp);
		      }
		    else
		      {
			tree_matrix *tmp = ml.pop ();
			tmp = tmp->chain ($1, tree::md_down);
			ml.push (tmp);
		      }
		  }
		| matrix_row ','		// Ignore trailing comma.
		| matrix_row ',' expression
		  {
		    tree_matrix *tmp = ml.pop ();
		    tmp = tmp->chain ($3, tree::md_right);
		    ml.push (tmp);
		  }
		;

%%

static void
yyerror (char *s)
{
  char *line = current_input_line;
  int err_col = current_input_column - 1;
  if (err_col == 0 && line != (char *) NULL)
    err_col = strlen (line) + 1;

// Print a message like `parse error'.
  fprintf (stderr, "\n%s", s);

// Maybe print the line number and file name.
  if (reading_fcn_file || reading_script_file)
    fprintf (stderr, " near line %d of file %s.m", input_line_number,
	     curr_fcn_file_name);

  if (line != (char *) NULL)
    {
      int len = strlen (line);
      if (line[len-1] == '\n')
        {
          len--;
          line[len] = '\0';
        }
// Print the line, maybe with a pointer near the error token.
      if (err_col > len)
        fprintf (stderr, ":\n\n  %s\n\n", line);
      else
        fprintf (stderr, ":\n\n  %s\n  %*s\n\n", line, err_col, "^");
    }
  else
    fprintf (stderr, "\n\n");
}

static void
end_error (char *type, token::end_tok_type ettype, int l, int c)
{
  static char *fmt = "%s command matched by `%s' near line %d column %d";

  switch (ettype)
    {
    case token::simple_end:
      error (fmt, type, "end", l, c);
      break;
    case token::for_end:
      error (fmt, type, "endfor", l, c);
      break;
    case token::function_end:
      error (fmt, type, "endfunction", l, c);
      break;
    case token::if_end:
      error (fmt, type, "endif", l, c);
      break;
    case token::while_end:
      error (fmt, type, "endwhile", l, c); 
      break;
    default:
      panic_impossible ();
      break;
    }
}

static int
check_end (token *tok, token::end_tok_type expected)
{
  token::end_tok_type ettype = tok->ettype ();
  if (ettype != expected && ettype != token::simple_end)
    {
      yyerror ("parse error");

      int l = tok->line ();
      int c = tok->column ();

      switch (expected)
	{
	case token::for_end:
	  end_error ("for", ettype, l, c);
	  break;
	case token::function_end:
	  end_error ("function", ettype, l, c);
	  break;
	case token::if_end:
	  end_error ("if", ettype, l, c);
	  break;
	case token::while_end:
	  end_error ("while", ettype, l, c);
	  break;
	default:
	  panic_impossible ();
	  break;
	}
      return 1;
    }
  else
    return 0;
}

/*
 * Need to make sure that the expression isn't already an identifier
 * that has a name, or an assignment expression.
 *
 * Note that an expression can't be just an identifier anymore -- it
 * must at least be an index expression (see the definition of the
 * non-terminal `variable' above).
 *
 * XXX FIXME XXX.  This isn't quite sufficient.  For example, try the
 * command `x = 4, x' for `x' previously undefined.
 *
 * XXX FIXME XXX -- we should probably delay doing this until eval-time.
 */
static tree_expression *
maybe_convert_to_ans_assign (tree_expression *expr)
{
  if (expr->is_index_expression ())
    {
      expr->mark_for_possible_ans_assign ();
      return expr;
    }
  else if (expr->is_assignment_expression ()
	   || expr->is_prefix_expression ())
    {
      return expr;
    }
  else
    {
      symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

      assert (sr != (symbol_record *) NULL);
      
      tree_identifier *ans = new tree_identifier (sr);

      return new tree_simple_assignment_expression (ans, expr);
    }
}

static void
maybe_warn_assign_as_truth_value (tree_expression *expr)
{
  if (user_pref.warn_assign_as_truth_value
      && expr->is_assignment_expression ()
      && ((tree_assignment_expression *) expr) -> in_parens < 2)
    {
      warning ("suggest parenthesis around assignment used as truth value");
    }
}
