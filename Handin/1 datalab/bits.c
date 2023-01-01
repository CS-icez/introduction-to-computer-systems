/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2022 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* 
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y) {
  /* Solution:
      Core idea: De Morgan's laws.
  */
  return ~(~x|~y);
}
/* 
 * bitConditional - x ? y : z for each bit respectively
 *   Example: bitConditional(0b00110011, 0b01010101, 0b00001111) = 0b00011101
 *   Legal ops: & | ^ ~
 *   Max ops: 8
 *   Rating: 1
 */
int bitConditional(int x, int y, int z) {
  /* Solution 1:
      Core idea: dual mask.
      return (y&x)^(z&~x);
  */
  /* Solution 2:
      Core idea: someone told me it can be conquered with 3 ops, and I enumerate till finding a cure.
  */
 return ((y^z)&x)^z;
}
/* 
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
    /* Solution 1 -- works, but been improved.
        Core idea: left and right shifts, clear and fill bytes.
        int a = x>>(n<<=3)&0xff;
        int b = x>>(m<<=3)&0xff;
        return (x&~((0xff<<n)|(0xff<<m)))|(a<<m)|(b<<n);
    */

    /* Solution 2 (the following code):
        Core idea: t = x^y; x^t->y, y^t->x;
    */
    int t;
    n <<= 3, m <<= 3;
    t = ((x>>n)^(x>>m))&0xff;
    return x^(t<<m)^(t<<n);
}
/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n) {
  /* Solution 1 -- acceptable, but not optimal
      Core idea: clear sign bit, arithmetic shift, restore sign bit.
      int s = x>>31&1;
      int x1 = x&~(1<<31);
      return (x1>>n)|s<<(31+(~n+1));
  */

  /* Solution 2 (the following code):
      Core idea: arithmetic shift, then set mask
  */
  return ~(1<<31>>n<<1)&x>>n;
}
/* 
 * cleanConsecutive1 - change any consecutive 1 to zeros in the binary form of x.
 *   Consecutive 1 means a set of 1 that contains more than one 1.
 *   Examples cleanConsecutive1(0x10) = 0x10
 *            cleanConsecutive1(0xF0) = 0x0
 *            cleanConsecutive1(0xFFFF0001) = 0x1
 *            cleanConsecutive1(0x4F4F4F4F) = 0x40404040
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 4
 */
int cleanConsecutive1(int x){
    /* Solution 1:
        Core idea: logical left and right shifts.
        return x&~(x<<1)&((1<<31)|~(x>>1));

       Solution 2:
        An abnormal solution.
    */
    int t = x&(x<<1);
    return (t|t>>1)^x;
}
/* 
 * countTrailingZero - return the number of consecutive 0 from the lowest bit of 
 *   the binary form of x.
 *   YOU MAY USE BIG CONST IN THIS PROBLEM, LIKE 0xFFFF0000
 *   YOU MAY USE BIG CONST IN THIS PROBLEM, LIKE 0xFFFF0000
 *   YOU MAY USE BIG CONST IN THIS PROBLEM, LIKE 0xFFFF0000
 *   Examples countTrailingZero(0x0) = 32, countTrailingZero(0x1) = 0,
 *            countTrailingZero(0xFFFF0000) = 16,
 *            countTrailingZero(0xFFFFFFF0) = 8,
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int countTrailingZero(int x){
    /* Solution 1:
        Core idea: 1. bisection(binary lifting? divide and conquer? whatever).
                   2. generate a map (with truth table and observation) when length of x is small.
        int cnt = 0;
        int m;
        m = (!(x&0xffff)<<31>>31)&16;
        cnt = m, x >>= m;
        m = (!(x&0xff)<<31>>31)&8;
        cnt += m, x >>= m;
        m = (!(x&0xf)<<31>>31)&4;
        cnt += m, x >>= m;
        m = (!(x&0x3)<<31>>31)&2;
        cnt += m, x >>= m;
        cnt += !(x&1)+!x;
        return cnt;
       
       Solution 2 (my roommate's idea, record it here for future reference):
        Procedure: perform x = x&(-x)-1,
                which maps x = ****1000
                        to     00000111.
                Then, calculate how many 1s there are.
        (No code)

       Solution 3 (my roommate's idea, record it here for future reference):
        Ouch, I forget all about it!!! XD
        (No code)
      
       Solution 4:
        Core idea: exactly sol1, use logically equivalent expressions with less operators.
            int cnt = 0;
            int m;
            m = !(x&0xffff)<<4;
            cnt = m, x >>= m;
            m = !(x&0xff)<<3;
            cnt += m, x >>= m;
            m = !(x&0xf)<<2;
            cnt += m, x >>= m;
            m = !(x&0x3)<<1;
            cnt += m, x >>= m;
            cnt += !(x&1)+!x;
            return cnt;

       Solution 5:
        Core idea: enlighted by sol2, a similar solution using less ops.
        Procedure: perform x = x&(-x),
                which maps x = ****1000
                        to     00001000.
                Then, find out the index of the remaining 1.
                The thing is, there's only one non-zero bit, which allows us to do some clever
                tricks to avoid condition stream.
            int y = x&(~x+1), cnt = 0;
            cnt = !(y&0x0000ffff)<<4;
            cnt += !(y&0x00ff00ff)<<3;
            cnt += !(y&0x0f0f0f0f)<<2;
            cnt += !(y&0x33333333)<<1;
            cnt += !(y&0x55555555);
            cnt += !y;
            return cnt;
    */

    int y = x&(~x+1), cnt = 0;
    cnt = !(y&0x0000ffff)<<4;
    cnt += !(y&0x00ff00ff)<<3;
    cnt += !(y&0x0f0f0f0f)<<2;
    cnt += !(y&0x33333333)<<1;
    cnt += !(y&0x55555555);
    cnt += !y;
    return cnt;
}
/* 
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n) {
    /* Solution:
        Core idea: bias b = x < 0? (1<<n)-1: 0, like taught in CSAPP.
        And, I figured out a good way to simulate condition stream and calculate (1<<n)-1 all at once.
    */
    int b = x>>31;
    b = b^(b<<n);
    return (x+b)>>n;
}
/* 
 * oneMoreThan - return 1 if y is one more than x, and 0 otherwise
 *   Examples oneMoreThan(0, 1) = 1, oneMoreThan(-1, 1) = 0
 *   Legal ops: ~ & ! ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int oneMoreThan(int x, int y) {
  /* Solution: 
      Core idea: mathematical observation.
      Analysis:
        y-x = 1  <=> y+(~x)+1 = 1 (is that true?)
                 <=> y+(~x) = 0
      Note that the equation fails when (x, y) = (TMax, TMin), so we need to rid out such case.
      There's no other corner cases. I know it because I passed all the tests. I cannot give a mathematical proof.
  */
  return !((~x+y)|!(y^1<<31));
}
/*
 * satMul3 - multiplies by 3, saturating to Tmin or Tmax if overflow
 *  Examples: satMul3(0x10000000) = 0x30000000
 *            satMul3(0x30000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0x70000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0xD0000000) = 0x80000000 (Saturate to TMin)
 *            satMul3(0xA0000000) = 0x80000000 (Saturate to TMin)
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 3
 */
int satMul3(int x) {
    /* Solution:
        Core idea: x*3 overflows iff x+x overflows or (x+x)+x overflows.
        CSAPP says: x+y overflows iff x, y have the same signs but x+y has a different one.
        TMin won't cause trouble.
        Important optimization:
          note that (x&y)^(x|z) equals to ~y when x = 111...111, z when x = 0.
          This allows us to perform condition stream and bitwise negation at the same time.
    */
    int TMin = 1<<31;
    int y = x+x, z = y+x;
    int sx = x>>31;
    int f = ((x^y)|(x^z))>>31;  // f = y's or z's sign differs x's? 111...111: 000...000
    int M = sx^TMin; // M = TMax when x is negative, TMin otherwise
    return (f&M)^(f|z); // bless of god
}
/* 
 * subOK - Determine if can compute x-y without overflow
 *   Example: subOK(0x80000000,0x80000000) = 1,
 *            subOK(0x80000000,0x70000000) = 0, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int subOK(int x, int y) {
  /* Solution:
      Core idea: classified discussion.
      Analysis:
        case 1: x,y of same signs. By all means won't overflow.
        case 2: x,y of different signs. Overflows iff (x-y)'s sign differs x's.
      TMin won't cause trouble. Not that I proved it, but that I passed it.
  */
  int z = x+(~y+1);
  return !(((x^y)&(x^z))>>31);
}
/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
  /* Solution 1 -- good, but not best
      Core idea: classified discussion.
      Analysis:
        case 1: x,y of difference signs. x <= y iff x < 0 && y >= 0.
        case 2: x,y of same signs. x <= y iff y-x >= 0.
      Note that you cannot perform y-x in case 1, overflow will catch ya.
      return ((x&~y) | ~((x^y)|(~x+y+1)))>>31&1;
  */
  
  /* Solution 2 (the following code):
      Core idea: same as sol1, but one step further.
      Analysis:
        case 1: x,y of difference signs. x <= y iff x < 0 && y >= 0, i.e. x's sign bit is 1.
        case 2: x,y of same signs. x <= y iff y-x >= 0.
          y-x >= 0 <=> x-y <= 0
                   <=> x-y < 1
                   <=> x+(~y)+1 < 1
                   <=> x+(~y) < 0
                   <=> (x+(~y))'s sign bit is 1.
        So, we want to find t = (x,y of same signs? ~y: 0), so that case 1,2  amounts to (x+t)'s sign bit is 1.
        Using sign bit mask trick, we can easily find such t. :)
      TMin and overflow won't cause trouble, as in Sol1.
  */
  int s = (x^y)>>31;
  int t = ~(s|y);
  return (x+t)>>31&1;
}
/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int trueThreeFourths(int x)
{
  /* Solution 1 -- available, but not optimal
      Core idea: Euclidean division.
      int t = (x+((x>>31)&3))>>2;
      int y = t+t+t;
      int r = x+~(t<<2)+1;
      int l = r+r+r;
      t = (l+((l>>31)&3))>>2;
      return y+t;
  */
  
  /* Solution 2:
      Core idea: x*(3/4) = x-x/4
      But, as LHS rounds towards 0, RHS has to round against 0.
      For negative x, that amounts to rounding down.
      For positive x which is not a multiple of 4, an additional 1 must be added.
      
      int t = x>>2; // t = x/4, rounding down
      int s = x>>31;  // sign bit
      int e = !(x&0x3);  // e == 1 <=> x is a multiple of 4
      t += !(s|e);  // now t = x/4, rounding against 0
      return x+~t+1;

    Solution 3:
      Core idea: logical equivalence to sol2, saving one more op.
  */

  int t = x>>2;
  int s = x>>31;
  int e = !(x&0x3); 
  int d = (s|e)&1; 
  return x+~t+d;
}
/* 
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {
  /*
    I don't what to comment on this.
    I don't know what I was typing either.
  */
  // const unsigned m_exp = 0x7f800000;  // (1<<30)-(1<<23)
  // unsigned m_frac = 0x7fffff; // (1<<23)-1
  // unsigned n_frac = 0xff800000; // ~m_frac
  // unsigned exp = m_exp&uf, frac = m_frac&uf;
  // unsigned res;
  // const unsigned sp = 0x7f000000;

  // res = uf+0x800000;
  // switch (exp){
  //   case 0x7f800000:  return uf;
  //   case 0:           return (uf&n_frac)+(frac<<1);
  //   case 0x7f000000:  res &= n_frac;
  //   default:          return res;
  // }

  const unsigned m_sexp = 0xff800000;  // (1<<30)-(1<<23)
  unsigned sexp = m_sexp&uf;

  switch (sexp){
    case 0x7f800000:
    case 0xff800000:  return uf;
    case 0x00000000:           
    case 0x80000000:  return sexp|(uf<<1);
    case 0x7f000000:  return 0x7f800000;
    case 0xff000000:  return 0xff800000;
    default:          return uf+0x800000;
  }
}
/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
  /*
    I don't what to comment on this.
    I don't know what I was typing either.
    Damn floating point.
  */
  unsigned m = 0x800000; // (1<<23)
  unsigned TMin = 0x80000000;
  unsigned frac;
  unsigned exp = 0x4f000000;
  //unsigned s = x&TMin;
  unsigned x1;
  unsigned mid = 0x40;
  int f = 1;

  switch (x){
    case 0x80000000:  return 0xcf000000;
    case 0:           return 0;
  }

  //if (s) x = -x;
  
  while (1){
    if (x&0x80000000){
      if (f){
        exp = 0xcf000000;
        x = -x;
      }
      else break;
    }  
    switch (exp){
      case 0x4f000000: exp = 0x4e800000; break;
      case 0x4e800000: exp = 0x4e000000; break;
      case 0x4e000000: exp = 0x4d800000; break;
      case 0x4d800000: exp = 0x4d000000; break;
      case 0x4d000000: exp = 0x4c800000; break;
      case 0x4c800000: exp = 0x4c000000; break;
      case 0x4c000000: exp = 0x4b800000; break;
      case 0x4b800000: exp = 0x4b000000; break;
      case 0x4b000000: exp = 0x4a800000; break;
      case 0x4a800000: exp = 0x4a000000; break;
      case 0x4a000000: exp = 0x49800000; break;
      case 0x49800000: exp = 0x49000000; break;
      case 0x49000000: exp = 0x48800000; break;
      case 0x48800000: exp = 0x48000000; break;
      case 0x48000000: exp = 0x47800000; break;
      case 0x47800000: exp = 0x47000000; break;
      case 0x47000000: exp = 0x46800000; break;
      case 0x46800000: exp = 0x46000000; break;
      case 0x46000000: exp = 0x45800000; break;
      case 0x45800000: exp = 0x45000000; break;
      case 0x45000000: exp = 0x44800000; break;
      case 0x44800000: exp = 0x44000000; break;
      case 0x44000000: exp = 0x43800000; break;
      case 0x43800000: exp = 0x43000000; break;
      case 0x43000000: exp = 0x42800000; break;
      case 0x42800000: exp = 0x42000000; break;
      case 0x42000000: exp = 0x41800000; break;
      case 0x41800000: exp = 0x41000000; break;
      case 0x41000000: exp = 0x40800000; break;
      case 0x40800000: exp = 0x40000000; break;
      case 0x40000000: exp = 0x3f800000; break;
      case 0x3f800000: exp = 0x3f000000; break;
      case 0xcf000000: exp = 0xce800000; break;
      case 0xce800000: exp = 0xce000000; break;
      case 0xce000000: exp = 0xcd800000; break;
      case 0xcd800000: exp = 0xcd000000; break;
      case 0xcd000000: exp = 0xcc800000; break;
      case 0xcc800000: exp = 0xcc000000; break;
      case 0xcc000000: exp = 0xcb800000; break;
      case 0xcb800000: exp = 0xcb000000; break;
      case 0xcb000000: exp = 0xca800000; break;
      case 0xca800000: exp = 0xca000000; break;
      case 0xca000000: exp = 0xc9800000; break;
      case 0xc9800000: exp = 0xc9000000; break;
      case 0xc9000000: exp = 0xc8800000; break;
      case 0xc8800000: exp = 0xc8000000; break;
      case 0xc8000000: exp = 0xc7800000; break;
      case 0xc7800000: exp = 0xc7000000; break;
      case 0xc7000000: exp = 0xc6800000; break;
      case 0xc6800000: exp = 0xc6000000; break;
      case 0xc6000000: exp = 0xc5800000; break;
      case 0xc5800000: exp = 0xc5000000; break;
      case 0xc5000000: exp = 0xc4800000; break;
      case 0xc4800000: exp = 0xc4000000; break;
      case 0xc4000000: exp = 0xc3800000; break;
      case 0xc3800000: exp = 0xc3000000; break;
      case 0xc3000000: exp = 0xc2800000; break;
      case 0xc2800000: exp = 0xc2000000; break;
      case 0xc2000000: exp = 0xc1800000; break;
      case 0xc1800000: exp = 0xc1000000; break;
      case 0xc1000000: exp = 0xc0800000; break;
      case 0xc0800000: exp = 0xc0000000; break;
      case 0xc0000000: exp = 0xbf800000; break;
      case 0xbf800000: exp = 0xbf000000; break;
    }
    x <<= 1;
    f = 0;
    // --exp;
  }

  frac = (x^TMin)>>8;

  x1 = x&0x1fe;
  // x1 = x&0x7f;
  // if (x1 > mid) delta = 1;
  // else if (x1 == mid) delta = frac&1;


  switch (x1){
    case 0x1fe:
    case 0x1fc:
    case 0x1fa:
    case 0x1f8:
    case 0x1f6:
    case 0x1f4:
    case 0x1f2:
    case 0x1f0:
    case 0x1ee:
    case 0x1ec:
    case 0x1ea:
    case 0x1e8:
    case 0x1e6:
    case 0x1e4:
    case 0x1e2:
    case 0x1e0:
    case 0x1de:
    case 0x1dc:
    case 0x1da:
    case 0x1d8:
    case 0x1d6:
    case 0x1d4:
    case 0x1d2:
    case 0x1d0:
    case 0x1ce:
    case 0x1cc:
    case 0x1ca:
    case 0x1c8:
    case 0x1c6:
    case 0x1c4:
    case 0x1c2:
    case 0x1c0:
    case 0x1be:
    case 0x1bc:
    case 0x1ba:
    case 0x1b8:
    case 0x1b6:
    case 0x1b4:
    case 0x1b2:
    case 0x1b0:
    case 0x1ae:
    case 0x1ac:
    case 0x1aa:
    case 0x1a8:
    case 0x1a6:
    case 0x1a4:
    case 0x1a2:
    case 0x1a0:
    case 0x19e:
    case 0x19c:
    case 0x19a:
    case 0x198:
    case 0x196:
    case 0x194:
    case 0x192:
    case 0x190:
    case 0x18e:
    case 0x18c:
    case 0x18a:
    case 0x188:
    case 0x186:
    case 0x184:
    case 0x182:
    case 0x180:
    case 0xfe:
    case 0xfc:
    case 0xfa:
    case 0xf8:
    case 0xf6:
    case 0xf4:
    case 0xf2:
    case 0xf0:
    case 0xee:
    case 0xec:
    case 0xea:
    case 0xe8:
    case 0xe6:
    case 0xe4:
    case 0xe2:
    case 0xe0:
    case 0xde:
    case 0xdc:
    case 0xda:
    case 0xd8:
    case 0xd6:
    case 0xd4:
    case 0xd2:
    case 0xd0:
    case 0xce:
    case 0xcc:
    case 0xca:
    case 0xc8:
    case 0xc6:
    case 0xc4:
    case 0xc2:
    case 0xc0:
    case 0xbe:
    case 0xbc:
    case 0xba:
    case 0xb8:
    case 0xb6:
    case 0xb4:
    case 0xb2:
    case 0xb0:
    case 0xae:
    case 0xac:
    case 0xaa:
    case 0xa8:
    case 0xa6:
    case 0xa4:
    case 0xa2:
    case 0xa0:
    case 0x9e:
    case 0x9c:
    case 0x9a:
    case 0x98:
    case 0x96:
    case 0x94:
    case 0x92:
    case 0x90:
    case 0x8e:
    case 0x8c:
    case 0x8a:
    case 0x88:
    case 0x86:
    case 0x84:
    case 0x82:
      switch (exp){
        case 0x4e800000: exp = 0x4e800001; break;
        case 0x4e000000: exp = 0x4e000001; break;
        case 0x4d800000: exp = 0x4d800001; break;
        case 0x4d000000: exp = 0x4d000001; break;
        case 0x4c800000: exp = 0x4c800001; break;
        case 0x4c000000: exp = 0x4c000001; break;
        case 0x4b800000: exp = 0x4b800001; break;
        case 0x4b000000: exp = 0x4b000001; break;
        case 0x4a800000: exp = 0x4a800001; break;
        case 0x4a000000: exp = 0x4a000001; break;
        case 0x49800000: exp = 0x49800001; break;
        case 0x49000000: exp = 0x49000001; break;
        case 0x48800000: exp = 0x48800001; break;
        case 0x48000000: exp = 0x48000001; break;
        case 0x47800000: exp = 0x47800001; break;
        case 0x47000000: exp = 0x47000001; break;
        case 0x46800000: exp = 0x46800001; break;
        case 0x46000000: exp = 0x46000001; break;
        case 0x45800000: exp = 0x45800001; break;
        case 0x45000000: exp = 0x45000001; break;
        case 0x44800000: exp = 0x44800001; break;
        case 0x44000000: exp = 0x44000001; break;
        case 0x43800000: exp = 0x43800001; break;
        case 0x43000000: exp = 0x43000001; break;
        case 0x42800000: exp = 0x42800001; break;
        case 0x42000000: exp = 0x42000001; break;
        case 0x41800000: exp = 0x41800001; break;
        case 0x41000000: exp = 0x41000001; break;
        case 0x40800000: exp = 0x40800001; break;
        case 0x40000000: exp = 0x40000001; break;
        case 0x3f800000: exp = 0x3f800001; break;
        case 0x3f000000: exp = 0x3f000001; break;
        case 0xce800000: exp = 0xce800001; break;
        case 0xce000000: exp = 0xce000001; break;
        case 0xcd800000: exp = 0xcd800001; break;
        case 0xcd000000: exp = 0xcd000001; break;
        case 0xcc800000: exp = 0xcc800001; break;
        case 0xcc000000: exp = 0xcc000001; break;
        case 0xcb800000: exp = 0xcb800001; break;
        case 0xcb000000: exp = 0xcb000001; break;
        case 0xca800000: exp = 0xca800001; break;
        case 0xca000000: exp = 0xca000001; break;
        case 0xc9800000: exp = 0xc9800001; break;
        case 0xc9000000: exp = 0xc9000001; break;
        case 0xc8800000: exp = 0xc8800001; break;
        case 0xc8000000: exp = 0xc8000001; break;
        case 0xc7800000: exp = 0xc7800001; break;
        case 0xc7000000: exp = 0xc7000001; break;
        case 0xc6800000: exp = 0xc6800001; break;
        case 0xc6000000: exp = 0xc6000001; break;
        case 0xc5800000: exp = 0xc5800001; break;
        case 0xc5000000: exp = 0xc5000001; break;
        case 0xc4800000: exp = 0xc4800001; break;
        case 0xc4000000: exp = 0xc4000001; break;
        case 0xc3800000: exp = 0xc3800001; break;
        case 0xc3000000: exp = 0xc3000001; break;
        case 0xc2800000: exp = 0xc2800001; break;
        case 0xc2000000: exp = 0xc2000001; break;
        case 0xc1800000: exp = 0xc1800001; break;
        case 0xc1000000: exp = 0xc1000001; break;
        case 0xc0800000: exp = 0xc0800001; break;
        case 0xc0000000: exp = 0xc0000001; break;
        case 0xbf800000: exp = 0xbf800001; break;
        case 0xbf000000: exp = 0xbf000001; break;
      }
  }
  
  return exp+frac;
}
/* 
 * float_f2i - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int float_f2i(unsigned uf) {
  /*
    I don't what to comment on this.
    I don't know what I was typing either.
    Damn floating point.
  */
  unsigned TMin = 0x80000000;
  unsigned TMax = 0x7fffffff;
  unsigned MEXP = 0xff800000;
  unsigned frac = (uf<<8)|TMin;
  unsigned exp; //= (uf&TMax)>>23;
  unsigned s = 0;
  int res;

  //if (uf == 0xcf000000) return 0x80000000;

  // exp = 158-((uf&TMax)>>23);
  switch(uf&MEXP){
    case 0xff800000: exp = 0xffffff9f; s = 1; break;
    case 0xff000000: exp = 0xffffffa0; s = 1; break;
    case 0xfe800000: exp = 0xffffffa1; s = 1; break;
    case 0xfe000000: exp = 0xffffffa2; s = 1; break;
    case 0xfd800000: exp = 0xffffffa3; s = 1; break;
    case 0xfd000000: exp = 0xffffffa4; s = 1; break;
    case 0xfc800000: exp = 0xffffffa5; s = 1; break;
    case 0xfc000000: exp = 0xffffffa6; s = 1; break;
    case 0xfb800000: exp = 0xffffffa7; s = 1; break;
    case 0xfb000000: exp = 0xffffffa8; s = 1; break;
    case 0xfa800000: exp = 0xffffffa9; s = 1; break;
    case 0xfa000000: exp = 0xffffffaa; s = 1; break;
    case 0xf9800000: exp = 0xffffffab; s = 1; break;
    case 0xf9000000: exp = 0xffffffac; s = 1; break;
    case 0xf8800000: exp = 0xffffffad; s = 1; break;
    case 0xf8000000: exp = 0xffffffae; s = 1; break;
    case 0xf7800000: exp = 0xffffffaf; s = 1; break;
    case 0xf7000000: exp = 0xffffffb0; s = 1; break;
    case 0xf6800000: exp = 0xffffffb1; s = 1; break;
    case 0xf6000000: exp = 0xffffffb2; s = 1; break;
    case 0xf5800000: exp = 0xffffffb3; s = 1; break;
    case 0xf5000000: exp = 0xffffffb4; s = 1; break;
    case 0xf4800000: exp = 0xffffffb5; s = 1; break;
    case 0xf4000000: exp = 0xffffffb6; s = 1; break;
    case 0xf3800000: exp = 0xffffffb7; s = 1; break;
    case 0xf3000000: exp = 0xffffffb8; s = 1; break;
    case 0xf2800000: exp = 0xffffffb9; s = 1; break;
    case 0xf2000000: exp = 0xffffffba; s = 1; break;
    case 0xf1800000: exp = 0xffffffbb; s = 1; break;
    case 0xf1000000: exp = 0xffffffbc; s = 1; break;
    case 0xf0800000: exp = 0xffffffbd; s = 1; break;
    case 0xf0000000: exp = 0xffffffbe; s = 1; break;
    case 0xef800000: exp = 0xffffffbf; s = 1; break;
    case 0xef000000: exp = 0xffffffc0; s = 1; break;
    case 0xee800000: exp = 0xffffffc1; s = 1; break;
    case 0xee000000: exp = 0xffffffc2; s = 1; break;
    case 0xed800000: exp = 0xffffffc3; s = 1; break;
    case 0xed000000: exp = 0xffffffc4; s = 1; break;
    case 0xec800000: exp = 0xffffffc5; s = 1; break;
    case 0xec000000: exp = 0xffffffc6; s = 1; break;
    case 0xeb800000: exp = 0xffffffc7; s = 1; break;
    case 0xeb000000: exp = 0xffffffc8; s = 1; break;
    case 0xea800000: exp = 0xffffffc9; s = 1; break;
    case 0xea000000: exp = 0xffffffca; s = 1; break;
    case 0xe9800000: exp = 0xffffffcb; s = 1; break;
    case 0xe9000000: exp = 0xffffffcc; s = 1; break;
    case 0xe8800000: exp = 0xffffffcd; s = 1; break;
    case 0xe8000000: exp = 0xffffffce; s = 1; break;
    case 0xe7800000: exp = 0xffffffcf; s = 1; break;
    case 0xe7000000: exp = 0xffffffd0; s = 1; break;
    case 0xe6800000: exp = 0xffffffd1; s = 1; break;
    case 0xe6000000: exp = 0xffffffd2; s = 1; break;
    case 0xe5800000: exp = 0xffffffd3; s = 1; break;
    case 0xe5000000: exp = 0xffffffd4; s = 1; break;
    case 0xe4800000: exp = 0xffffffd5; s = 1; break;
    case 0xe4000000: exp = 0xffffffd6; s = 1; break;
    case 0xe3800000: exp = 0xffffffd7; s = 1; break;
    case 0xe3000000: exp = 0xffffffd8; s = 1; break;
    case 0xe2800000: exp = 0xffffffd9; s = 1; break;
    case 0xe2000000: exp = 0xffffffda; s = 1; break;
    case 0xe1800000: exp = 0xffffffdb; s = 1; break;
    case 0xe1000000: exp = 0xffffffdc; s = 1; break;
    case 0xe0800000: exp = 0xffffffdd; s = 1; break;
    case 0xe0000000: exp = 0xffffffde; s = 1; break;
    case 0xdf800000: exp = 0xffffffdf; s = 1; break;
    case 0xdf000000: exp = 0xffffffe0; s = 1; break;
    case 0xde800000: exp = 0xffffffe1; s = 1; break;
    case 0xde000000: exp = 0xffffffe2; s = 1; break;
    case 0xdd800000: exp = 0xffffffe3; s = 1; break;
    case 0xdd000000: exp = 0xffffffe4; s = 1; break;
    case 0xdc800000: exp = 0xffffffe5; s = 1; break;
    case 0xdc000000: exp = 0xffffffe6; s = 1; break;
    case 0xdb800000: exp = 0xffffffe7; s = 1; break;
    case 0xdb000000: exp = 0xffffffe8; s = 1; break;
    case 0xda800000: exp = 0xffffffe9; s = 1; break;
    case 0xda000000: exp = 0xffffffea; s = 1; break;
    case 0xd9800000: exp = 0xffffffeb; s = 1; break;
    case 0xd9000000: exp = 0xffffffec; s = 1; break;
    case 0xd8800000: exp = 0xffffffed; s = 1; break;
    case 0xd8000000: exp = 0xffffffee; s = 1; break;
    case 0xd7800000: exp = 0xffffffef; s = 1; break;
    case 0xd7000000: exp = 0xfffffff0; s = 1; break;
    case 0xd6800000: exp = 0xfffffff1; s = 1; break;
    case 0xd6000000: exp = 0xfffffff2; s = 1; break;
    case 0xd5800000: exp = 0xfffffff3; s = 1; break;
    case 0xd5000000: exp = 0xfffffff4; s = 1; break;
    case 0xd4800000: exp = 0xfffffff5; s = 1; break;
    case 0xd4000000: exp = 0xfffffff6; s = 1; break;
    case 0xd3800000: exp = 0xfffffff7; s = 1; break;
    case 0xd3000000: exp = 0xfffffff8; s = 1; break;
    case 0xd2800000: exp = 0xfffffff9; s = 1; break;
    case 0xd2000000: exp = 0xfffffffa; s = 1; break;
    case 0xd1800000: exp = 0xfffffffb; s = 1; break;
    case 0xd1000000: exp = 0xfffffffc; s = 1; break;
    case 0xd0800000: exp = 0xfffffffd; s = 1; break;
    case 0xd0000000: exp = 0xfffffffe; s = 1; break;
    case 0xcf800000: exp = 0xffffffff; s = 1; break;
    case 0xcf000000: exp = 0x0; s = 1; break;
    case 0xce800000: exp = 0x1; s = 1; break;
    case 0xce000000: exp = 0x2; s = 1; break;
    case 0xcd800000: exp = 0x3; s = 1; break;
    case 0xcd000000: exp = 0x4; s = 1; break;
    case 0xcc800000: exp = 0x5; s = 1; break;
    case 0xcc000000: exp = 0x6; s = 1; break;
    case 0xcb800000: exp = 0x7; s = 1; break;
    case 0xcb000000: exp = 0x8; s = 1; break;
    case 0xca800000: exp = 0x9; s = 1; break;
    case 0xca000000: exp = 0xa; s = 1; break;
    case 0xc9800000: exp = 0xb; s = 1; break;
    case 0xc9000000: exp = 0xc; s = 1; break;
    case 0xc8800000: exp = 0xd; s = 1; break;
    case 0xc8000000: exp = 0xe; s = 1; break;
    case 0xc7800000: exp = 0xf; s = 1; break;
    case 0xc7000000: exp = 0x10; s = 1; break;
    case 0xc6800000: exp = 0x11; s = 1; break;
    case 0xc6000000: exp = 0x12; s = 1; break;
    case 0xc5800000: exp = 0x13; s = 1; break;
    case 0xc5000000: exp = 0x14; s = 1; break;
    case 0xc4800000: exp = 0x15; s = 1; break;
    case 0xc4000000: exp = 0x16; s = 1; break;
    case 0xc3800000: exp = 0x17; s = 1; break;
    case 0xc3000000: exp = 0x18; s = 1; break;
    case 0xc2800000: exp = 0x19; s = 1; break;
    case 0xc2000000: exp = 0x1a; s = 1; break;
    case 0xc1800000: exp = 0x1b; s = 1; break;
    case 0xc1000000: exp = 0x1c; s = 1; break;
    case 0xc0800000: exp = 0x1d; s = 1; break;
    case 0xc0000000: exp = 0x1e; s = 1; break;
    case 0xbf800000: exp = 0x1f; s = 1; break;
    case 0xbf000000: exp = 0x20; s = 1; break;
    case 0xbe800000: exp = 0x21; s = 1; break;
    case 0xbe000000: exp = 0x22; s = 1; break;
    case 0xbd800000: exp = 0x23; s = 1; break;
    case 0xbd000000: exp = 0x24; s = 1; break;
    case 0xbc800000: exp = 0x25; s = 1; break;
    case 0xbc000000: exp = 0x26; s = 1; break;
    case 0xbb800000: exp = 0x27; s = 1; break;
    case 0xbb000000: exp = 0x28; s = 1; break;
    case 0xba800000: exp = 0x29; s = 1; break;
    case 0xba000000: exp = 0x2a; s = 1; break;
    case 0xb9800000: exp = 0x2b; s = 1; break;
    case 0xb9000000: exp = 0x2c; s = 1; break;
    case 0xb8800000: exp = 0x2d; s = 1; break;
    case 0xb8000000: exp = 0x2e; s = 1; break;
    case 0xb7800000: exp = 0x2f; s = 1; break;
    case 0xb7000000: exp = 0x30; s = 1; break;
    case 0xb6800000: exp = 0x31; s = 1; break;
    case 0xb6000000: exp = 0x32; s = 1; break;
    case 0xb5800000: exp = 0x33; s = 1; break;
    case 0xb5000000: exp = 0x34; s = 1; break;
    case 0xb4800000: exp = 0x35; s = 1; break;
    case 0xb4000000: exp = 0x36; s = 1; break;
    case 0xb3800000: exp = 0x37; s = 1; break;
    case 0xb3000000: exp = 0x38; s = 1; break;
    case 0xb2800000: exp = 0x39; s = 1; break;
    case 0xb2000000: exp = 0x3a; s = 1; break;
    case 0xb1800000: exp = 0x3b; s = 1; break;
    case 0xb1000000: exp = 0x3c; s = 1; break;
    case 0xb0800000: exp = 0x3d; s = 1; break;
    case 0xb0000000: exp = 0x3e; s = 1; break;
    case 0xaf800000: exp = 0x3f; s = 1; break;
    case 0xaf000000: exp = 0x40; s = 1; break;
    case 0xae800000: exp = 0x41; s = 1; break;
    case 0xae000000: exp = 0x42; s = 1; break;
    case 0xad800000: exp = 0x43; s = 1; break;
    case 0xad000000: exp = 0x44; s = 1; break;
    case 0xac800000: exp = 0x45; s = 1; break;
    case 0xac000000: exp = 0x46; s = 1; break;
    case 0xab800000: exp = 0x47; s = 1; break;
    case 0xab000000: exp = 0x48; s = 1; break;
    case 0xaa800000: exp = 0x49; s = 1; break;
    case 0xaa000000: exp = 0x4a; s = 1; break;
    case 0xa9800000: exp = 0x4b; s = 1; break;
    case 0xa9000000: exp = 0x4c; s = 1; break;
    case 0xa8800000: exp = 0x4d; s = 1; break;
    case 0xa8000000: exp = 0x4e; s = 1; break;
    case 0xa7800000: exp = 0x4f; s = 1; break;
    case 0xa7000000: exp = 0x50; s = 1; break;
    case 0xa6800000: exp = 0x51; s = 1; break;
    case 0xa6000000: exp = 0x52; s = 1; break;
    case 0xa5800000: exp = 0x53; s = 1; break;
    case 0xa5000000: exp = 0x54; s = 1; break;
    case 0xa4800000: exp = 0x55; s = 1; break;
    case 0xa4000000: exp = 0x56; s = 1; break;
    case 0xa3800000: exp = 0x57; s = 1; break;
    case 0xa3000000: exp = 0x58; s = 1; break;
    case 0xa2800000: exp = 0x59; s = 1; break;
    case 0xa2000000: exp = 0x5a; s = 1; break;
    case 0xa1800000: exp = 0x5b; s = 1; break;
    case 0xa1000000: exp = 0x5c; s = 1; break;
    case 0xa0800000: exp = 0x5d; s = 1; break;
    case 0xa0000000: exp = 0x5e; s = 1; break;
    case 0x9f800000: exp = 0x5f; s = 1; break;
    case 0x9f000000: exp = 0x60; s = 1; break;
    case 0x9e800000: exp = 0x61; s = 1; break;
    case 0x9e000000: exp = 0x62; s = 1; break;
    case 0x9d800000: exp = 0x63; s = 1; break;
    case 0x9d000000: exp = 0x64; s = 1; break;
    case 0x9c800000: exp = 0x65; s = 1; break;
    case 0x9c000000: exp = 0x66; s = 1; break;
    case 0x9b800000: exp = 0x67; s = 1; break;
    case 0x9b000000: exp = 0x68; s = 1; break;
    case 0x9a800000: exp = 0x69; s = 1; break;
    case 0x9a000000: exp = 0x6a; s = 1; break;
    case 0x99800000: exp = 0x6b; s = 1; break;
    case 0x99000000: exp = 0x6c; s = 1; break;
    case 0x98800000: exp = 0x6d; s = 1; break;
    case 0x98000000: exp = 0x6e; s = 1; break;
    case 0x97800000: exp = 0x6f; s = 1; break;
    case 0x97000000: exp = 0x70; s = 1; break;
    case 0x96800000: exp = 0x71; s = 1; break;
    case 0x96000000: exp = 0x72; s = 1; break;
    case 0x95800000: exp = 0x73; s = 1; break;
    case 0x95000000: exp = 0x74; s = 1; break;
    case 0x94800000: exp = 0x75; s = 1; break;
    case 0x94000000: exp = 0x76; s = 1; break;
    case 0x93800000: exp = 0x77; s = 1; break;
    case 0x93000000: exp = 0x78; s = 1; break;
    case 0x92800000: exp = 0x79; s = 1; break;
    case 0x92000000: exp = 0x7a; s = 1; break;
    case 0x91800000: exp = 0x7b; s = 1; break;
    case 0x91000000: exp = 0x7c; s = 1; break;
    case 0x90800000: exp = 0x7d; s = 1; break;
    case 0x90000000: exp = 0x7e; s = 1; break;
    case 0x8f800000: exp = 0x7f; s = 1; break;
    case 0x8f000000: exp = 0x80; s = 1; break;
    case 0x8e800000: exp = 0x81; s = 1; break;
    case 0x8e000000: exp = 0x82; s = 1; break;
    case 0x8d800000: exp = 0x83; s = 1; break;
    case 0x8d000000: exp = 0x84; s = 1; break;
    case 0x8c800000: exp = 0x85; s = 1; break;
    case 0x8c000000: exp = 0x86; s = 1; break;
    case 0x8b800000: exp = 0x87; s = 1; break;
    case 0x8b000000: exp = 0x88; s = 1; break;
    case 0x8a800000: exp = 0x89; s = 1; break;
    case 0x8a000000: exp = 0x8a; s = 1; break;
    case 0x89800000: exp = 0x8b; s = 1; break;
    case 0x89000000: exp = 0x8c; s = 1; break;
    case 0x88800000: exp = 0x8d; s = 1; break;
    case 0x88000000: exp = 0x8e; s = 1; break;
    case 0x87800000: exp = 0x8f; s = 1; break;
    case 0x87000000: exp = 0x90; s = 1; break;
    case 0x86800000: exp = 0x91; s = 1; break;
    case 0x86000000: exp = 0x92; s = 1; break;
    case 0x85800000: exp = 0x93; s = 1; break;
    case 0x85000000: exp = 0x94; s = 1; break;
    case 0x84800000: exp = 0x95; s = 1; break;
    case 0x84000000: exp = 0x96; s = 1; break;
    case 0x83800000: exp = 0x97; s = 1; break;
    case 0x83000000: exp = 0x98; s = 1; break;
    case 0x82800000: exp = 0x99; s = 1; break;
    case 0x82000000: exp = 0x9a; s = 1; break;
    case 0x81800000: exp = 0x9b; s = 1; break;
    case 0x81000000: exp = 0x9c; s = 1; break;
    case 0x80800000: exp = 0x9d; s = 1; break;
    case 0x80000000: exp = 0x9e; s = 1; break;
    case 0x7f800000: exp = 0xffffff9f; break;
    case 0x7f000000: exp = 0xffffffa0; break;
    case 0x7e800000: exp = 0xffffffa1; break;
    case 0x7e000000: exp = 0xffffffa2; break;
    case 0x7d800000: exp = 0xffffffa3; break;
    case 0x7d000000: exp = 0xffffffa4; break;
    case 0x7c800000: exp = 0xffffffa5; break;
    case 0x7c000000: exp = 0xffffffa6; break;
    case 0x7b800000: exp = 0xffffffa7; break;
    case 0x7b000000: exp = 0xffffffa8; break;
    case 0x7a800000: exp = 0xffffffa9; break;
    case 0x7a000000: exp = 0xffffffaa; break;
    case 0x79800000: exp = 0xffffffab; break;
    case 0x79000000: exp = 0xffffffac; break;
    case 0x78800000: exp = 0xffffffad; break;
    case 0x78000000: exp = 0xffffffae; break;
    case 0x77800000: exp = 0xffffffaf; break;
    case 0x77000000: exp = 0xffffffb0; break;
    case 0x76800000: exp = 0xffffffb1; break;
    case 0x76000000: exp = 0xffffffb2; break;
    case 0x75800000: exp = 0xffffffb3; break;
    case 0x75000000: exp = 0xffffffb4; break;
    case 0x74800000: exp = 0xffffffb5; break;
    case 0x74000000: exp = 0xffffffb6; break;
    case 0x73800000: exp = 0xffffffb7; break;
    case 0x73000000: exp = 0xffffffb8; break;
    case 0x72800000: exp = 0xffffffb9; break;
    case 0x72000000: exp = 0xffffffba; break;
    case 0x71800000: exp = 0xffffffbb; break;
    case 0x71000000: exp = 0xffffffbc; break;
    case 0x70800000: exp = 0xffffffbd; break;
    case 0x70000000: exp = 0xffffffbe; break;
    case 0x6f800000: exp = 0xffffffbf; break;
    case 0x6f000000: exp = 0xffffffc0; break;
    case 0x6e800000: exp = 0xffffffc1; break;
    case 0x6e000000: exp = 0xffffffc2; break;
    case 0x6d800000: exp = 0xffffffc3; break;
    case 0x6d000000: exp = 0xffffffc4; break;
    case 0x6c800000: exp = 0xffffffc5; break;
    case 0x6c000000: exp = 0xffffffc6; break;
    case 0x6b800000: exp = 0xffffffc7; break;
    case 0x6b000000: exp = 0xffffffc8; break;
    case 0x6a800000: exp = 0xffffffc9; break;
    case 0x6a000000: exp = 0xffffffca; break;
    case 0x69800000: exp = 0xffffffcb; break;
    case 0x69000000: exp = 0xffffffcc; break;
    case 0x68800000: exp = 0xffffffcd; break;
    case 0x68000000: exp = 0xffffffce; break;
    case 0x67800000: exp = 0xffffffcf; break;
    case 0x67000000: exp = 0xffffffd0; break;
    case 0x66800000: exp = 0xffffffd1; break;
    case 0x66000000: exp = 0xffffffd2; break;
    case 0x65800000: exp = 0xffffffd3; break;
    case 0x65000000: exp = 0xffffffd4; break;
    case 0x64800000: exp = 0xffffffd5; break;
    case 0x64000000: exp = 0xffffffd6; break;
    case 0x63800000: exp = 0xffffffd7; break;
    case 0x63000000: exp = 0xffffffd8; break;
    case 0x62800000: exp = 0xffffffd9; break;
    case 0x62000000: exp = 0xffffffda; break;
    case 0x61800000: exp = 0xffffffdb; break;
    case 0x61000000: exp = 0xffffffdc; break;
    case 0x60800000: exp = 0xffffffdd; break;
    case 0x60000000: exp = 0xffffffde; break;
    case 0x5f800000: exp = 0xffffffdf; break;
    case 0x5f000000: exp = 0xffffffe0; break;
    case 0x5e800000: exp = 0xffffffe1; break;
    case 0x5e000000: exp = 0xffffffe2; break;
    case 0x5d800000: exp = 0xffffffe3; break;
    case 0x5d000000: exp = 0xffffffe4; break;
    case 0x5c800000: exp = 0xffffffe5; break;
    case 0x5c000000: exp = 0xffffffe6; break;
    case 0x5b800000: exp = 0xffffffe7; break;
    case 0x5b000000: exp = 0xffffffe8; break;
    case 0x5a800000: exp = 0xffffffe9; break;
    case 0x5a000000: exp = 0xffffffea; break;
    case 0x59800000: exp = 0xffffffeb; break;
    case 0x59000000: exp = 0xffffffec; break;
    case 0x58800000: exp = 0xffffffed; break;
    case 0x58000000: exp = 0xffffffee; break;
    case 0x57800000: exp = 0xffffffef; break;
    case 0x57000000: exp = 0xfffffff0; break;
    case 0x56800000: exp = 0xfffffff1; break;
    case 0x56000000: exp = 0xfffffff2; break;
    case 0x55800000: exp = 0xfffffff3; break;
    case 0x55000000: exp = 0xfffffff4; break;
    case 0x54800000: exp = 0xfffffff5; break;
    case 0x54000000: exp = 0xfffffff6; break;
    case 0x53800000: exp = 0xfffffff7; break;
    case 0x53000000: exp = 0xfffffff8; break;
    case 0x52800000: exp = 0xfffffff9; break;
    case 0x52000000: exp = 0xfffffffa; break;
    case 0x51800000: exp = 0xfffffffb; break;
    case 0x51000000: exp = 0xfffffffc; break;
    case 0x50800000: exp = 0xfffffffd; break;
    case 0x50000000: exp = 0xfffffffe; break;
    case 0x4f800000: exp = 0xffffffff; break;
    case 0x4f000000: exp = 0x0; break;
    case 0x4e800000: exp = 0x1; break;
    case 0x4e000000: exp = 0x2; break;
    case 0x4d800000: exp = 0x3; break;
    case 0x4d000000: exp = 0x4; break;
    case 0x4c800000: exp = 0x5; break;
    case 0x4c000000: exp = 0x6; break;
    case 0x4b800000: exp = 0x7; break;
    case 0x4b000000: exp = 0x8; break;
    case 0x4a800000: exp = 0x9; break;
    case 0x4a000000: exp = 0xa; break;
    case 0x49800000: exp = 0xb; break;
    case 0x49000000: exp = 0xc; break;
    case 0x48800000: exp = 0xd; break;
    case 0x48000000: exp = 0xe; break;
    case 0x47800000: exp = 0xf; break;
    case 0x47000000: exp = 0x10; break;
    case 0x46800000: exp = 0x11; break;
    case 0x46000000: exp = 0x12; break;
    case 0x45800000: exp = 0x13; break;
    case 0x45000000: exp = 0x14; break;
    case 0x44800000: exp = 0x15; break;
    case 0x44000000: exp = 0x16; break;
    case 0x43800000: exp = 0x17; break;
    case 0x43000000: exp = 0x18; break;
    case 0x42800000: exp = 0x19; break;
    case 0x42000000: exp = 0x1a; break;
    case 0x41800000: exp = 0x1b; break;
    case 0x41000000: exp = 0x1c; break;
    case 0x40800000: exp = 0x1d; break;
    case 0x40000000: exp = 0x1e; break;
    case 0x3f800000: exp = 0x1f; break;
    case 0x3f000000: exp = 0x20; break;
    case 0x3e800000: exp = 0x21; break;
    case 0x3e000000: exp = 0x22; break;
    case 0x3d800000: exp = 0x23; break;
    case 0x3d000000: exp = 0x24; break;
    case 0x3c800000: exp = 0x25; break;
    case 0x3c000000: exp = 0x26; break;
    case 0x3b800000: exp = 0x27; break;
    case 0x3b000000: exp = 0x28; break;
    case 0x3a800000: exp = 0x29; break;
    case 0x3a000000: exp = 0x2a; break;
    case 0x39800000: exp = 0x2b; break;
    case 0x39000000: exp = 0x2c; break;
    case 0x38800000: exp = 0x2d; break;
    case 0x38000000: exp = 0x2e; break;
    case 0x37800000: exp = 0x2f; break;
    case 0x37000000: exp = 0x30; break;
    case 0x36800000: exp = 0x31; break;
    case 0x36000000: exp = 0x32; break;
    case 0x35800000: exp = 0x33; break;
    case 0x35000000: exp = 0x34; break;
    case 0x34800000: exp = 0x35; break;
    case 0x34000000: exp = 0x36; break;
    case 0x33800000: exp = 0x37; break;
    case 0x33000000: exp = 0x38; break;
    case 0x32800000: exp = 0x39; break;
    case 0x32000000: exp = 0x3a; break;
    case 0x31800000: exp = 0x3b; break;
    case 0x31000000: exp = 0x3c; break;
    case 0x30800000: exp = 0x3d; break;
    case 0x30000000: exp = 0x3e; break;
    case 0x2f800000: exp = 0x3f; break;
    case 0x2f000000: exp = 0x40; break;
    case 0x2e800000: exp = 0x41; break;
    case 0x2e000000: exp = 0x42; break;
    case 0x2d800000: exp = 0x43; break;
    case 0x2d000000: exp = 0x44; break;
    case 0x2c800000: exp = 0x45; break;
    case 0x2c000000: exp = 0x46; break;
    case 0x2b800000: exp = 0x47; break;
    case 0x2b000000: exp = 0x48; break;
    case 0x2a800000: exp = 0x49; break;
    case 0x2a000000: exp = 0x4a; break;
    case 0x29800000: exp = 0x4b; break;
    case 0x29000000: exp = 0x4c; break;
    case 0x28800000: exp = 0x4d; break;
    case 0x28000000: exp = 0x4e; break;
    case 0x27800000: exp = 0x4f; break;
    case 0x27000000: exp = 0x50; break;
    case 0x26800000: exp = 0x51; break;
    case 0x26000000: exp = 0x52; break;
    case 0x25800000: exp = 0x53; break;
    case 0x25000000: exp = 0x54; break;
    case 0x24800000: exp = 0x55; break;
    case 0x24000000: exp = 0x56; break;
    case 0x23800000: exp = 0x57; break;
    case 0x23000000: exp = 0x58; break;
    case 0x22800000: exp = 0x59; break;
    case 0x22000000: exp = 0x5a; break;
    case 0x21800000: exp = 0x5b; break;
    case 0x21000000: exp = 0x5c; break;
    case 0x20800000: exp = 0x5d; break;
    case 0x20000000: exp = 0x5e; break;
    case 0x1f800000: exp = 0x5f; break;
    case 0x1f000000: exp = 0x60; break;
    case 0x1e800000: exp = 0x61; break;
    case 0x1e000000: exp = 0x62; break;
    case 0x1d800000: exp = 0x63; break;
    case 0x1d000000: exp = 0x64; break;
    case 0x1c800000: exp = 0x65; break;
    case 0x1c000000: exp = 0x66; break;
    case 0x1b800000: exp = 0x67; break;
    case 0x1b000000: exp = 0x68; break;
    case 0x1a800000: exp = 0x69; break;
    case 0x1a000000: exp = 0x6a; break;
    case 0x19800000: exp = 0x6b; break;
    case 0x19000000: exp = 0x6c; break;
    case 0x18800000: exp = 0x6d; break;
    case 0x18000000: exp = 0x6e; break;
    case 0x17800000: exp = 0x6f; break;
    case 0x17000000: exp = 0x70; break;
    case 0x16800000: exp = 0x71; break;
    case 0x16000000: exp = 0x72; break;
    case 0x15800000: exp = 0x73; break;
    case 0x15000000: exp = 0x74; break;
    case 0x14800000: exp = 0x75; break;
    case 0x14000000: exp = 0x76; break;
    case 0x13800000: exp = 0x77; break;
    case 0x13000000: exp = 0x78; break;
    case 0x12800000: exp = 0x79; break;
    case 0x12000000: exp = 0x7a; break;
    case 0x11800000: exp = 0x7b; break;
    case 0x11000000: exp = 0x7c; break;
    case 0x10800000: exp = 0x7d; break;
    case 0x10000000: exp = 0x7e; break;
    case 0xf800000: exp = 0x7f; break;
    case 0xf000000: exp = 0x80; break;
    case 0xe800000: exp = 0x81; break;
    case 0xe000000: exp = 0x82; break;
    case 0xd800000: exp = 0x83; break;
    case 0xd000000: exp = 0x84; break;
    case 0xc800000: exp = 0x85; break;
    case 0xc000000: exp = 0x86; break;
    case 0xb800000: exp = 0x87; break;
    case 0xb000000: exp = 0x88; break;
    case 0xa800000: exp = 0x89; break;
    case 0xa000000: exp = 0x8a; break;
    case 0x9800000: exp = 0x8b; break;
    case 0x9000000: exp = 0x8c; break;
    case 0x8800000: exp = 0x8d; break;
    case 0x8000000: exp = 0x8e; break;
    case 0x7800000: exp = 0x8f; break;
    case 0x7000000: exp = 0x90; break;
    case 0x6800000: exp = 0x91; break;
    case 0x6000000: exp = 0x92; break;
    case 0x5800000: exp = 0x93; break;
    case 0x5000000: exp = 0x94; break;
    case 0x4800000: exp = 0x95; break;
    case 0x4000000: exp = 0x96; break;
    case 0x3800000: exp = 0x97; break;
    case 0x3000000: exp = 0x98; break;
    case 0x2800000: exp = 0x99; break;
    case 0x2000000: exp = 0x9a; break;
    case 0x1800000: exp = 0x9b; break;
    case 0x1000000: exp = 0x9c; break;
    case 0x800000: exp = 0x9d; break;
    case 0x0: exp = 0x9e; break;
  }

  switch (exp){
    case 0xffffff9f:
    case 0xffffffa0:
    case 0xffffffa1:
    case 0xffffffa2:
    case 0xffffffa3:
    case 0xffffffa4:
    case 0xffffffa5:
    case 0xffffffa6:
    case 0xffffffa7:
    case 0xffffffa8:
    case 0xffffffa9:
    case 0xffffffaa:
    case 0xffffffab:
    case 0xffffffac:
    case 0xffffffad:
    case 0xffffffae:
    case 0xffffffaf:
    case 0xffffffb0:
    case 0xffffffb1:
    case 0xffffffb2:
    case 0xffffffb3:
    case 0xffffffb4:
    case 0xffffffb5:
    case 0xffffffb6:
    case 0xffffffb7:
    case 0xffffffb8:
    case 0xffffffb9:
    case 0xffffffba:
    case 0xffffffbb:
    case 0xffffffbc:
    case 0xffffffbd:
    case 0xffffffbe:
    case 0xffffffbf:
    case 0xffffffc0:
    case 0xffffffc1:
    case 0xffffffc2:
    case 0xffffffc3:
    case 0xffffffc4:
    case 0xffffffc5:
    case 0xffffffc6:
    case 0xffffffc7:
    case 0xffffffc8:
    case 0xffffffc9:
    case 0xffffffca:
    case 0xffffffcb:
    case 0xffffffcc:
    case 0xffffffcd:
    case 0xffffffce:
    case 0xffffffcf:
    case 0xffffffd0:
    case 0xffffffd1:
    case 0xffffffd2:
    case 0xffffffd3:
    case 0xffffffd4:
    case 0xffffffd5:
    case 0xffffffd6:
    case 0xffffffd7:
    case 0xffffffd8:
    case 0xffffffd9:
    case 0xffffffda:
    case 0xffffffdb:
    case 0xffffffdc:
    case 0xffffffdd:
    case 0xffffffde:
    case 0xffffffdf:
    case 0xffffffe0:
    case 0xffffffe1:
    case 0xffffffe2:
    case 0xffffffe3:
    case 0xffffffe4:
    case 0xffffffe5:
    case 0xffffffe6:
    case 0xffffffe7:
    case 0xffffffe8:
    case 0xffffffe9:
    case 0xffffffea:
    case 0xffffffeb:
    case 0xffffffec:
    case 0xffffffed:
    case 0xffffffee:
    case 0xffffffef:
    case 0xfffffff0:
    case 0xfffffff1:
    case 0xfffffff2:
    case 0xfffffff3:
    case 0xfffffff4:
    case 0xfffffff5:
    case 0xfffffff6:
    case 0xfffffff7:
    case 0xfffffff8:
    case 0xfffffff9:
    case 0xfffffffa:
    case 0xfffffffb:
    case 0xfffffffc:
    case 0xfffffffd:
    case 0xfffffffe:
    case 0xffffffff:
    case 0x0: return 0x80000000u;
    case 0x1:
    case 0x2:
    case 0x3:
    case 0x4:
    case 0x5:
    case 0x6:
    case 0x7:
    case 0x8:
    case 0x9:
    case 0xa:
    case 0xb:
    case 0xc:
    case 0xd:
    case 0xe:
    case 0xf:
    case 0x10:
    case 0x11:
    case 0x12:
    case 0x13:
    case 0x14:
    case 0x15:
    case 0x16:
    case 0x17:
    case 0x18:
    case 0x19:
    case 0x1a:
    case 0x1b:
    case 0x1c:
    case 0x1d:
    case 0x1e:
    case 0x1f: res = frac>>exp; break;
    default: return 0;
  }

  if (s) res = -res;
  return res;
}
/* 
 * float_pwr2 - Return bit-level equivalent of the expression 2.0^x
 *   (2.0 raised to the power x) for any 32-bit integer x.
 *
 *   The unsigned value that is returned should have the identical bit
 *   representation as the single-precision floating-point number 2.0^x.
 *   If the result is too small to be represented as a denorm, return
 *   0. If too large, return +INF.
 * 
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. Also if, while 
 *   Max ops: 30 
 *   Rating: 4
 */
unsigned float_pwr2(int x) {
    /*
      I don't what to comment on this.
      I don't know what I was typing either.
      Damn floating point.
    */
    // int e = x+127;
     if (x >= 128) return 0x7f800000;
    // if (e <= 0){
    //   int t = e+22;
    //   if (t < 0) return 0;
    //   return 1<<t;
    // }
    // return e<<23;

    switch (x){
      case 127: return 0x7f000000;
      case 126: return 0x7e800000;
      case 125: return 0x7e000000;
      case 124: return 0x7d800000;
      case 123: return 0x7d000000;
      case 122: return 0x7c800000;
      case 121: return 0x7c000000;
      case 120: return 0x7b800000;
      case 119: return 0x7b000000;
      case 118: return 0x7a800000;
      case 117: return 0x7a000000;
      case 116: return 0x79800000;
      case 115: return 0x79000000;
      case 114: return 0x78800000;
      case 113: return 0x78000000;
      case 112: return 0x77800000;
      case 111: return 0x77000000;
      case 110: return 0x76800000;
      case 109: return 0x76000000;
      case 108: return 0x75800000;
      case 107: return 0x75000000;
      case 106: return 0x74800000;
      case 105: return 0x74000000;
      case 104: return 0x73800000;
      case 103: return 0x73000000;
      case 102: return 0x72800000;
      case 101: return 0x72000000;
      case 100: return 0x71800000;
      case 99: return 0x71000000;
      case 98: return 0x70800000;
      case 97: return 0x70000000;
      case 96: return 0x6f800000;
      case 95: return 0x6f000000;
      case 94: return 0x6e800000;
      case 93: return 0x6e000000;
      case 92: return 0x6d800000;
      case 91: return 0x6d000000;
      case 90: return 0x6c800000;
      case 89: return 0x6c000000;
      case 88: return 0x6b800000;
      case 87: return 0x6b000000;
      case 86: return 0x6a800000;
      case 85: return 0x6a000000;
      case 84: return 0x69800000;
      case 83: return 0x69000000;
      case 82: return 0x68800000;
      case 81: return 0x68000000;
      case 80: return 0x67800000;
      case 79: return 0x67000000;
      case 78: return 0x66800000;
      case 77: return 0x66000000;
      case 76: return 0x65800000;
      case 75: return 0x65000000;
      case 74: return 0x64800000;
      case 73: return 0x64000000;
      case 72: return 0x63800000;
      case 71: return 0x63000000;
      case 70: return 0x62800000;
      case 69: return 0x62000000;
      case 68: return 0x61800000;
      case 67: return 0x61000000;
      case 66: return 0x60800000;
      case 65: return 0x60000000;
      case 64: return 0x5f800000;
      case 63: return 0x5f000000;
      case 62: return 0x5e800000;
      case 61: return 0x5e000000;
      case 60: return 0x5d800000;
      case 59: return 0x5d000000;
      case 58: return 0x5c800000;
      case 57: return 0x5c000000;
      case 56: return 0x5b800000;
      case 55: return 0x5b000000;
      case 54: return 0x5a800000;
      case 53: return 0x5a000000;
      case 52: return 0x59800000;
      case 51: return 0x59000000;
      case 50: return 0x58800000;
      case 49: return 0x58000000;
      case 48: return 0x57800000;
      case 47: return 0x57000000;
      case 46: return 0x56800000;
      case 45: return 0x56000000;
      case 44: return 0x55800000;
      case 43: return 0x55000000;
      case 42: return 0x54800000;
      case 41: return 0x54000000;
      case 40: return 0x53800000;
      case 39: return 0x53000000;
      case 38: return 0x52800000;
      case 37: return 0x52000000;
      case 36: return 0x51800000;
      case 35: return 0x51000000;
      case 34: return 0x50800000;
      case 33: return 0x50000000;
      case 32: return 0x4f800000;
      case 31: return 0x4f000000;
      case 30: return 0x4e800000;
      case 29: return 0x4e000000;
      case 28: return 0x4d800000;
      case 27: return 0x4d000000;
      case 26: return 0x4c800000;
      case 25: return 0x4c000000;
      case 24: return 0x4b800000;
      case 23: return 0x4b000000;
      case 22: return 0x4a800000;
      case 21: return 0x4a000000;
      case 20: return 0x49800000;
      case 19: return 0x49000000;
      case 18: return 0x48800000;
      case 17: return 0x48000000;
      case 16: return 0x47800000;
      case 15: return 0x47000000;
      case 14: return 0x46800000;
      case 13: return 0x46000000;
      case 12: return 0x45800000;
      case 11: return 0x45000000;
      case 10: return 0x44800000;
      case 9: return 0x44000000;
      case 8: return 0x43800000;
      case 7: return 0x43000000;
      case 6: return 0x42800000;
      case 5: return 0x42000000;
      case 4: return 0x41800000;
      case 3: return 0x41000000;
      case 2: return 0x40800000;
      case 1: return 0x40000000;
      case 0: return 0x3f800000;
      case 0xffffffff: return 0x3f000000;
      case 0xfffffffe: return 0x3e800000;
      case 0xfffffffd: return 0x3e000000;
      case 0xfffffffc: return 0x3d800000;
      case 0xfffffffb: return 0x3d000000;
      case 0xfffffffa: return 0x3c800000;
      case 0xfffffff9: return 0x3c000000;
      case 0xfffffff8: return 0x3b800000;
      case 0xfffffff7: return 0x3b000000;
      case 0xfffffff6: return 0x3a800000;
      case 0xfffffff5: return 0x3a000000;
      case 0xfffffff4: return 0x39800000;
      case 0xfffffff3: return 0x39000000;
      case 0xfffffff2: return 0x38800000;
      case 0xfffffff1: return 0x38000000;
      case 0xfffffff0: return 0x37800000;
      case 0xffffffef: return 0x37000000;
      case 0xffffffee: return 0x36800000;
      case 0xffffffed: return 0x36000000;
      case 0xffffffec: return 0x35800000;
      case 0xffffffeb: return 0x35000000;
      case 0xffffffea: return 0x34800000;
      case 0xffffffe9: return 0x34000000;
      case 0xffffffe8: return 0x33800000;
      case 0xffffffe7: return 0x33000000;
      case 0xffffffe6: return 0x32800000;
      case 0xffffffe5: return 0x32000000;
      case 0xffffffe4: return 0x31800000;
      case 0xffffffe3: return 0x31000000;
      case 0xffffffe2: return 0x30800000;
      case 0xffffffe1: return 0x30000000;
      case 0xffffffe0: return 0x2f800000;
      case 0xffffffdf: return 0x2f000000;
      case 0xffffffde: return 0x2e800000;
      case 0xffffffdd: return 0x2e000000;
      case 0xffffffdc: return 0x2d800000;
      case 0xffffffdb: return 0x2d000000;
      case 0xffffffda: return 0x2c800000;
      case 0xffffffd9: return 0x2c000000;
      case 0xffffffd8: return 0x2b800000;
      case 0xffffffd7: return 0x2b000000;
      case 0xffffffd6: return 0x2a800000;
      case 0xffffffd5: return 0x2a000000;
      case 0xffffffd4: return 0x29800000;
      case 0xffffffd3: return 0x29000000;
      case 0xffffffd2: return 0x28800000;
      case 0xffffffd1: return 0x28000000;
      case 0xffffffd0: return 0x27800000;
      case 0xffffffcf: return 0x27000000;
      case 0xffffffce: return 0x26800000;
      case 0xffffffcd: return 0x26000000;
      case 0xffffffcc: return 0x25800000;
      case 0xffffffcb: return 0x25000000;
      case 0xffffffca: return 0x24800000;
      case 0xffffffc9: return 0x24000000;
      case 0xffffffc8: return 0x23800000;
      case 0xffffffc7: return 0x23000000;
      case 0xffffffc6: return 0x22800000;
      case 0xffffffc5: return 0x22000000;
      case 0xffffffc4: return 0x21800000;
      case 0xffffffc3: return 0x21000000;
      case 0xffffffc2: return 0x20800000;
      case 0xffffffc1: return 0x20000000;
      case 0xffffffc0: return 0x1f800000;
      case 0xffffffbf: return 0x1f000000;
      case 0xffffffbe: return 0x1e800000;
      case 0xffffffbd: return 0x1e000000;
      case 0xffffffbc: return 0x1d800000;
      case 0xffffffbb: return 0x1d000000;
      case 0xffffffba: return 0x1c800000;
      case 0xffffffb9: return 0x1c000000;
      case 0xffffffb8: return 0x1b800000;
      case 0xffffffb7: return 0x1b000000;
      case 0xffffffb6: return 0x1a800000;
      case 0xffffffb5: return 0x1a000000;
      case 0xffffffb4: return 0x19800000;
      case 0xffffffb3: return 0x19000000;
      case 0xffffffb2: return 0x18800000;
      case 0xffffffb1: return 0x18000000;
      case 0xffffffb0: return 0x17800000;
      case 0xffffffaf: return 0x17000000;
      case 0xffffffae: return 0x16800000;
      case 0xffffffad: return 0x16000000;
      case 0xffffffac: return 0x15800000;
      case 0xffffffab: return 0x15000000;
      case 0xffffffaa: return 0x14800000;
      case 0xffffffa9: return 0x14000000;
      case 0xffffffa8: return 0x13800000;
      case 0xffffffa7: return 0x13000000;
      case 0xffffffa6: return 0x12800000;
      case 0xffffffa5: return 0x12000000;
      case 0xffffffa4: return 0x11800000;
      case 0xffffffa3: return 0x11000000;
      case 0xffffffa2: return 0x10800000;
      case 0xffffffa1: return 0x10000000;
      case 0xffffffa0: return 0xf800000;
      case 0xffffff9f: return 0xf000000;
      case 0xffffff9e: return 0xe800000;
      case 0xffffff9d: return 0xe000000;
      case 0xffffff9c: return 0xd800000;
      case 0xffffff9b: return 0xd000000;
      case 0xffffff9a: return 0xc800000;
      case 0xffffff99: return 0xc000000;
      case 0xffffff98: return 0xb800000;
      case 0xffffff97: return 0xb000000;
      case 0xffffff96: return 0xa800000;
      case 0xffffff95: return 0xa000000;
      case 0xffffff94: return 0x9800000;
      case 0xffffff93: return 0x9000000;
      case 0xffffff92: return 0x8800000;
      case 0xffffff91: return 0x8000000;
      case 0xffffff90: return 0x7800000;
      case 0xffffff8f: return 0x7000000;
      case 0xffffff8e: return 0x6800000;
      case 0xffffff8d: return 0x6000000;
      case 0xffffff8c: return 0x5800000;
      case 0xffffff8b: return 0x5000000;
      case 0xffffff8a: return 0x4800000;
      case 0xffffff89: return 0x4000000;
      case 0xffffff88: return 0x3800000;
      case 0xffffff87: return 0x3000000;
      case 0xffffff86: return 0x2800000;
      case 0xffffff85: return 0x2000000;
      case 0xffffff84: return 0x1800000;
      case 0xffffff83: return 0x1000000;
      case 0xffffff82: return 0x800000;
      case 0xffffff81: return 0x400000;
      case 0xffffff80: return 0x200000;
      case 0xffffff7f: return 0x100000;
      case 0xffffff7e: return 0x80000;
      case 0xffffff7d: return 0x40000;
      case 0xffffff7c: return 0x20000;
      case 0xffffff7b: return 0x10000;
      case 0xffffff7a: return 0x8000;
      case 0xffffff79: return 0x4000;
      case 0xffffff78: return 0x2000;
      case 0xffffff77: return 0x1000;
      case 0xffffff76: return 0x800;
      case 0xffffff75: return 0x400;
      case 0xffffff74: return 0x200;
      case 0xffffff73: return 0x100;
      case 0xffffff72: return 0x80;
      case 0xffffff71: return 0x40;
      case 0xffffff70: return 0x20;
      case 0xffffff6f: return 0x10;
      case 0xffffff6e: return 0x8;
      case 0xffffff6d: return 0x4;
      case 0xffffff6c: return 0x2;
      case 0xffffff6b: return 0x1;
      default: return 0;
    }
}
