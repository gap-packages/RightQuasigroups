# Convert.gd
# Converting between numerical bases
# =============================================================================

# RQ_conversionString 
# PROG: Suitable characters to represent digits in GAP are found in the interval
# CHAR_INT(35)..CHAR_INT(126), except for CHAR_INT[92] = '\\'.
# This leads to natural base 91 = 126-35.
# To implement binary, decimal and hexadecimal numbers naturally,
# we reorder the suitable 91 characters to read as follows:
BindGlobal(
    "RQ_conversionString",
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%&'()*+,-./:;<=>?@[]^_`{|}~"
);

# RQ_DigitToChar( d )
# Converts an integer <d> in the range [0..90] to a character.
DeclareOperation( "RQ_DigitToChar", [ IsInt ] );

# RQ_CharToDigit( c )
# Converts a character <c> to an integer in the range [0..90].
DeclareOperation( "RQ_CharToDigit", [ IsChar ] );

# RQ_EncodeCayleyTable( category, ct ) 
# RQ_DecodeCayleyTable( category, str )
# Auxiliary routines for encoding and decoding of Cayley tables
# of order in [1..91], using characters instead of numbers.
# When category = IsLoop and n>2, the first row and column are ignored
# because they can be reconstructed from the rest of the table.
# When <ct> is commutative, only "half" of the table is saved.
# This can be detected from <str> and decoded appropriately.
DeclareOperation( "RQ_EncodeCayleyTable", [ IsObject, IsList ] );
DeclareOperation( "RQ_DecodeCayleyTable", [ IsObject, IsString ] );

# RQ_ConvertToDecimal( s, n )
# Converts an <n>-ary number represented by a string <s> to a decimal
# number represented as integer.
DeclareOperation( "RQ_ConvertToDecimal", [ IsString, IsInt ] );

# RQ_ConvertFromDecimal( arg )
# arg = [ <d>, <m>, optional <k> ]
# Converts a decimal number <d> to a number in base <m>.
# Optional parameter <k> is the minimal required number of "digits"
# of the output in new base <m>, including zeros at the beginning
# Returns the corresponding number as a string in base <m> (of length at least <k>).
DeclareGlobalFunction( "RQ_ConvertFromDecimal" ); # has variable number of arguments

# RQ_ConvertBase( arg )
# arg = [ s, n, m, optional k ]
# s is a string that represents a number in base n
# m is a new base
# optional parameter k is the minimal required number of "digits"
# of the output in new base m, including zeros at the beginning
# Returns the corresponding number in base m (with at least k digits).
DeclareGlobalFunction( "RQ_ConvertBase" );

# RQ_EncodeCocycle( coc, values ) 
# RQ_DecodeCocycle( ecoc, values ) 
#   
# Given loops F and K, a cocycle is a mapping from F x F to K.
# Let n=|F| and b=|K|.
# Cocycles are represented as n x n arrays with entries in a b-element set.
# These are auxiliary routines for encoding and decoding of cocycles.
# <coc> is a cocycle, an n x n matrix
# <values> is a set of cardinality b
#     every entry in <coc> must lie in <values> but not vice versa
# <ecoc> is an encoded cocycle, a list of the form [n, is_comm, data],
#     where n=|F|, is_comm is true iff <coc> is commutative, and
#     data is an encoded cocycle table
# Note: The encoded cocycle has default values in [0..b-1]. The argument
# <values> can be used to populate the cocycle with other values.
DeclareOperation( "RQ_EncodeCocycle", [ IsList, IsList ] );
DeclareOperation( "RQ_DecodeCocycle", [ IsList, IsList ] );
