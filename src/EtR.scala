/*
 *
 *
 * Authors: Ronik Bhaskar, insert names here
 *
 */

import scala.io.Source

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

/* === 3 The Stern-Brocot Tree === */