"
    File: 01_literals_numbers.st
    Purpose: Test cases for various number literals in GNU Smalltalk.
"

" --- Integers --- "
" Simple Integers "
123.
0.
-456.

" Integers with _ separators "
1_000_000.
-2_500.

" --- Radix Integers --- "
" Binary (2r) "
2r1011.      " 11 "
-2r100.      " -4 "
2r1111_0000. " 240 "

" Octal (8r) "
8r777.       " 511 "
-8r10.       " -8 "
8r1_7.       " 15 "

" Hexadecimal (16r) "
16rFF.       " 255 "
-16rA0.      " -160 "
16rCAFE_BABE. " 3405691582 "
16rff.       " 255 (lowercase) "

" Other Radix (e.g., 36r) "
36rSMALLTALK. " 80465390982035363 "

" --- Floats --- "
" FLOATD_LITERAL (Double-precision) - default, or with 'd' suffix "
1.0.
-0.5.
123.456.
1.23456789d.
-2.5d.
1_000.500_000d.
2r101.01d.   " 5.25 "
16rF.Fd.     " 15.8125 "

" Exponent notation for doubles "
1.0e2.       " 100.0 "
1.0d2.       " 100.0 "
-3.14e-1.    " -0.314 "
12.34E5.     " 1234000.0 "

" FLOATE_LITERAL (Single-precision) - with 'e' suffix if no decimal point, or 'e' exponent "
" Note: GST typically promotes to double if 'e' is used as exponent with a decimal point.
  The 'e' suffix for single precision is more for integer-like numbers.
  1e is a float, 1.0e is a float.
  1e0 is a float.
  123e. " " This form (integer followed by 'e' alone) might be lexed as IDENTIFIER if not careful, GST specific "
123e2.      " 12300.0 (parsed as float) "
123e-1.     " 12.3 (parsed as float) "
1.0e.       " This is 1.0, a double "

" FLOATQ_LITERAL (Quad-precision) - with 'q' suffix "
1.0q.
-2.5q.
1.234567890123456789q.
1.0q2.       " 100.0 (quad) "
-3.14q-1.    " -0.314 (quad) "

" --- Scaled Decimals --- "
" Simple scaled decimals "
1s.          " 1 with scale 0 "
1.2s.        " 1.2 with scale 1 "
-3.14s.       " -3.14 with scale 2 "

" Scaled decimals with explicit scale "
1s0.         " 1 with scale 0 "
123s2.       " 123 with scale 2 (effectively 1.23 if it were float, but it's precise) "
1.2s2.       " 1.2 with scale 2 "
-0.5s1.       " -0.5 with scale 1 "
1_000.50s3.

" Radix with scaled decimals "
16rA.Bs2.    " 10.6875 with scale 2 "

" End of Number Literals Test "
!
