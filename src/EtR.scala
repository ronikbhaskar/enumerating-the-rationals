/*
 *
 *
 * Authors: Ronik Bhaskar, insert names here
 *
 */

import scala.io.Source

/* types and constructors */

type Rational = (Int, Int)

def mkRat(num: Int, den: Int): Rational = 
  if num <= 0 || den <= 0 then throw new Exception(s"mkRat: unable to construct positive rational with $num and $den")
  else (num, den)

def rational2str(q: Rational): String = s"${q(0)}/${q(1)}"

def rationals2str(qs: List[Rational]): String = qs.map(rational2str).mkString(", ")

/* === 2 Greatest Common Divisor === */

/* gcd - calculates the gcd of two natural numbers using Euclid's Algorithm
 *     - does not check for garbage input
 *
 * m : Int - first natural
 *
 * n : Int - second natural
 *
 * Returns: Int - gcd
 */
def gcd(m: Int, n: Int): Int = 
  if m < n then gcd(m, n - m) 
  else if m > n then gcd(m - n, n)
  else m

/* igcd - instrumented version of gcd (accumulates list of moves as specified by paper)
 *
 * m : Int - first natural
 *
 * n : Int - second natural
 *
 * Returns: (Int, List[Boolean]) - gcd, list of moves (false for m < n, true for m > n)
 */
def igcd(m: Int, n: Int): (Int, List[Boolean]) = 
  def step(b: Boolean, dBs: (Int, List[Boolean])): (Int, List[Boolean]) = dBs match {
    case (d, bs) => (d, b::bs)
  } 
  if m < n then step(false, igcd(m, n - m))
  else if m > n then step(true, igcd(m - n, n))
  else (m, Nil)

/* pgcd - execution path of gcd algorithm
 *
 * m : Int - first natural
 *
 * n : Int - second natural
 *
 * Returns: List[Boolean] - execution path as described in paper
 */
def pgcd(m: Int, n: Int): List[Boolean] = igcd(m, n)(1)

/* ungcd - constructs two naturals given their gcd and the gcd execution path
 * 
 * d : Int - gcd
 *
 * bs : Booleanean - execution path
 *
 * Returns: (Int, Int) - ordered tuple of naturals given a particular gcd and execution path
 */
def ungcd(d: Int, bs: List[Boolean]): (Int, Int) = 
  def undo(b: Boolean, ns: (Int, Int)): (Int, Int) = ns match {
    case (m, n) => b match {
      case false => (m, n + m)
      case true => (m + n, n)
    }
  }
  bs.foldRight((d, d))(undo)

/* boolseqs - enumerates the first 2^(n+1) - 1 boolean sequences
 *
 * n : Int - used to create bound on number of boolean sequences generated
 *
 * Returns: List[List[Boolean]] - list of boolean sequences in increasing lexicographic order
 */
def boolseqs(n: Int): List[List[Boolean]] = 
  def bsaccumulate(m: Int, bseqs: List[List[Boolean]]): List[List[Boolean]] = m match {
    case 1 => bseqs
    case _ => bsaccumulate(m - 1, bseqs ::: bseqs.map(x => false :: x) ::: bseqs.map(x => true :: x))
  }
  if n <= 0 then throw new Exception("boolseqs: n must be positive") 
  else (Nil) :: bsaccumulate(n, (false :: Nil) :: (true :: Nil) ::Nil)
  
/* enumRationals - enumerates the rationals using finite boolean sequences
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals(n: Int): List[Rational] = boolseqs(n).map(ungcd.curried(1))

/* === 3 The Stern-Brocot Tree === */

