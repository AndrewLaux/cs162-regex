// This file uses the 'pimp my library' pattern to add builder methods and regex
// operations to Regex.

package edu.ucsb.cs.cs162.regex

object `package` {
  import Regex._

  // Convenient methods to build regular expressions.
  implicit class RegexBuilder(val re: Regex) extends AnyVal {
    //----------------------------------------------------------------------------
    // Public API.
    //----------------------------------------------------------------------------

    // Concatenate 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def ~(other: Regex): Regex = 
      if (re == ∅ || other == ∅) return ∅ 
      else if (re == ε) return other
      else if (other == ε) return re
      else return Concatenate(re, other)

    // Union 're' with 'other', simplifying if possible (assumes that 're' and
    // 'other' have already been simplified).
     def |(other: Regex): Regex = (re, other) match {
      case (r, ∅) => r
      case (∅, r) => r
      case (KleeneStar(r), ε) => KleeneStar(r)
      case (ε, KleeneStar(r)) => KleeneStar(r)
      case (KleeneStar(α), r) => KleeneStar(α)
      case (r, KleeneStar(α)) => KleeneStar(α)
      case (r1, r2) if r1 == r2 => r1
      case (r1, r2) => Union(r1, r2)
    }
 
      
      // if (re == ∅) return other
      // else if(other == ∅) return re
      // else if(re == _* && other == ε) return re
      // else if(other == _* && re == ε) return other
      // else return Union(re, other)

    // Apply the Kleene star to 're', simplifying if possible (assumes that 're'
    // has already been simplified).
    def * : Regex = re match {
      case ∅ => ε
      case EmptyString => EmptyString
      case KleeneStar(r) => KleeneStar(r)
      case r => KleeneStar(r)
    }

    // Complement 're', simplifying if possible (assumes that 're' has already
    // been simplified).
    def unary_! : Regex = re match {
      case ∅ => KleeneStar(α)
      case ε if(ε == EmptyString) => α.+
      case Complement(r) => r
      case r => Complement(r)
    }

    // Intersect 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def & (other: Regex): Regex = (re, other) match {
      case (r, ∅) => ∅
      case (∅, r) => ∅
      case (r, KleeneStar(α)) => r
      case (KleeneStar(α), r) => r
      case (r, s) if (r==s) => r
      case (r, s) => Intersect(r, s)
    }

    // Shorthand for 1 or more repetitions of re regex.
    def + : Regex = return re~(KleeneStar(re))

    // Shorthand for 0 or 1 instances of re regex.
    def ? : Regex = return ε | (re)

    // Shorthand for exactly 'num' repetitions of re regex.
    def ^(num: Int): Regex = if (num == 0) return ε else return (re^(num-1))~re

    // Shorthand for at least 'min' repetitions of re regex.
    def >=(min: Int): Regex = return (re^min)~(KleeneStar(re))

    // Shorthand for at most 'max' repetitions of re regex.
    def <=(max: Int): Regex = 
      if (max == 1) return ε | (re)
      else return (re<=(max-1))|(re^max)

    // Shorthand for at least 'min' but at most 'max' repetitions of re regex.
    def <>(min: Int, max: Int): Regex = (re>=min)&(re<=max)
  }

  // Add convenient methods to String for building simple regular expressions.
  implicit class StringToRegex(val str: String) extends AnyVal {
    // Builds the concatenation of each character in 'str' in sequence. Example:
    // "abc".concatenate == Chars('a') ~ Chars('b') ~ Chars('c').
    def concatenate: Regex =
      str.foldLeft(ε: Regex)((acc, char) => acc ~ Chars(char))

    // Builds a charset containing each character in 'str'. Example:
    // "abc".charset == Chars('a', 'b', 'c').
    def charset: Regex =
      if (str.isEmpty) ε else Chars(str.toSeq: _*)
  }

  // Operations on regular expressions.
  implicit class RegexOps(val re: Regex) extends AnyVal {
    // Returns ε if 're' is nullable, otherwise returns ∅.
    def nullable: Regex = re match {
      case `ε` | _: KleeneStar => ε
      case `∅` | _: Chars => ∅
      case Concatenate(re1, re2) => re1.nullable ~ re2.nullable
      case Union(re1, re2) => re1.nullable | re2.nullable
      case Complement(re1) => if (re1.nullable == ε) ∅ else ε
      case Intersect(re1, re2) => re1.nullable & re2.nullable
    }
  }
}
