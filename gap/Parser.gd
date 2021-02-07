# Parser.gd
# Parser for evaluating right quasigroup terms and identities
# =============================================================================

#! @Chapter Properties of right quasigroups, quasigroups and loops

#! @Section Parser

#! <P/>&RightQuasigroups; contains a simple, custom-built parser for
#! parsing of right quasigroup, quasigroup and loop terms and for verification
#! of right quasigroup, quasigroup and loop identities. For the purposes of the parser:
#! <List>
#! <Item>variables are lower case characters `a`, ..., `z`,</Item>
#! <Item>the multiplication operation is denoted by `*`,</Item>
#! <Item>the right division operation is denoted by `/`,</Item>
#! <Item>the left division operation is denoted by `|` (since `\` behaves badly in &GAP; strings)
#!      and is only allowed in quasigroup and loop terms,</Item>
#! <Item>the neutral element is denoted by `1` and is only allowed in loop terms,</Item>
#! <Item>parentheses and brackets `(`, `[`, `{` and  `)`, `]`, `}` are allowed and they are all treated as `(` and `)`, respectively,</Item>
#! <Item>the three binary operations have the same priority and if terms are not fully parenthesized, the term is parsed from right to left,
#! (e.g., `x*y\z` is parsed as `(x*y)\z`) in accordance with &GAP; conventions for parsing of products,</Item>
#! <Item>spaces are allowed and ignorred,</Item>
#! <Item>no other characters are allowed.</Item>
#! </List>

# auxiliary function that builds a term tree from a string
DeclareGlobalFunction( "RQ_TermFromString" ); # s

# auxiliry funciton that checks for category and  builds a term tree from a string
DeclareGlobalFunction( "RQ_AlgebraTermFromString" ); # category, s

#! @BeginGroup
#! @GroupTitle Building terms from strings

#! @Arguments s
#! @Returns the right quasigroup (quasigroup, loop) term corresponding to
#! the string `s`. The term is returned as a structured list mimicking
#! the infix tree for the term. 
DeclareOperation( "RightQuasigroupTermFromString", [ IsString ]);

#! @Arguments s
DeclareOperation( "QuasigroupTermFromString", [ IsString ] );

#! @Arguments s
DeclareOperation( "LoopTermFromString", [ IsString ] );

#! @EndGroup

# auxiliary function that evaluates the term t
# values of variables must be set globally before the evaluation takes place
DeclareGlobalFunction( "RQ_EvaluateTerm" ); # t

# auxiliary function that checks if an identity t holds in Q
DeclareGlobalFunction( "RQ_AlgebraSatisfiesIdentity" ); # Q, t

#! <P/>It is possible to verify identities directly in &GAP;. For instance,
#! `ForAll( Q, x -> ForAll( Q, y -> ForAll( Q, z -> x*(y*z) = (x*y)*z ) ) )`
#! returns `true` iff `Q` is associative.

#! <P/>For the convenience of the user, we support checking of identities
#! in right quasigroups, quasigroups and loops by the custom-built parser.
#! Despite some effort to make the checking fast, it is about half as fast
#! as the direct check using &GAP; code. 

#! @BeginGroup
#! @GroupTitle Verifying identities

#! @Arguments Q, s
#! @Returns `true` if the right quasigroup (quasigroup, loop) `Q` satisfies
#! the right quasigroup (quasigroup, loop) identity represented by the string `s`.
#! If `Q` does not satisfy `s`, the function returns a list of variables of `s`
#! and their values in `Q` that violate `s`.
DeclareOperation( "RightQuasigroupSatisfiesIdentity", [ IsRightQuasigroup, IsString ] );

#! @Arguments Q, s
DeclareOperation( "QuasigroupSatisfiesIdentity", [ IsQuasigroup, IsString ] );

#! @Arguments Q, s
DeclareOperation( "LoopSatisfiesIdentity", [ IsLoop, IsString ] );

#! @BeginExampleSession
#! gap> Q := AsLoop( CyclicGroup( 6 ) );;
#! gap> LoopSatisfiesIdentity( Q, "x*y=y*x" );
#! true
#! gap> LoopSatisfiesIdentity( Q, "x*x=1" );
#! [ [ 'x', lf1 ] ]
#! gap> RightQuasigroupTermFromString("x*(y/z)");
#! [ 24, '*', [ 25, '/', 26 ] ]
#! gap> QuasigroupTermFromString("(a|b)/(c*d)"); # note the symbol | for left division
#! [ [ 1, '|', 2 ], '/', [ 3, '*', 5 ] ]
#! @EndExampleSession

#! @EndGroup

#! @Subsection *How evaluation of identities with the parser works

#!<P/>To speed up evaluation of identities, the parser produces terms in which 
#! variables have been replaced by their positions in the global variable string
#! `RQ_parserVarNames = "abcedfghijklmnopqrstuvwxyz"`. The current values
#! of all variables are stored in the global list `RQ_parserVars` of length 26.
#! The current value of the neutral element is stored in the global variable
#! `RQ_parserOne`. A term `t` produced by `RightQuasigroupTermFromString`, 
#! `QuasigroupTermFromString` or `LoopTermFromString` is then evaluated via
#! `RQ_EvaluateTerm( t )`.

BindGlobal( "RQ_parserVarNames", "abcdefghijklmnopqrstuvwxyz" );
BindGlobal( "RQ_parserVars", List([1..26], i -> 0) );
MakeReadWriteGlobal( "RQ_parserVars" );
BindGlobal( "RQ_parserOne", 0 );
MakeReadWriteGlobal( "RQ_parserOne" );