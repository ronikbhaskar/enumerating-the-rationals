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
  if den == 0 then throw new Exception(s"mkRat: denominator is zero")
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
  
/* enumRationals4 - enumerates the rationals using finite boolean sequences
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals3(n: Int): List[Rational] = boolseqs(n).map(ungcd.curried(1))

/* === 3 The Stern-Brocot Tree === */

enum BinTree[T]:
  case Node(n:T, left:BinTree[T], right:BinTree[T])
  case Empty(t:Option[T]) // always leave None, this just binds the type of Empty

/* foldt - folds centrally over a tree of type T and returns an S
 *
 * f : T, S, S => S - the function we are folding over the tree
 *
 * acc : S - the default for an empty tree (the accumulator)
 *
 * node : BinTree[T] - the tree being folded over
 *
 * Returns: S
 */
def foldt[T, S](f:(T, S, S) => S, acc: S, node: BinTree[T]): S = node match {
  case BinTree.Empty(t) => acc
  case BinTree.Node(a, t1, t2) => f(a, foldt[T, S](f, acc, t1), foldt[T, S](f, acc, t2))
}

/* unfoldt - generates tree from a single starting value x and a function f, inverse of foldt
 * WARNING: infinite recursion depth, theoretical use only
 *
 * f : S => (T, S, S) - function used to generate tree
 *
 * x : S - starting value
 *
 * Returns: BinTree[T]
 */
def unfoldt[S, T](f:S => (T, S, S), x:S): BinTree[T] = f(x) match {
  case (a, y, z) => BinTree.Node(a, unfoldt(f, y), unfoldt(f, z))
}

/* unfoldtn - generates tree of fixed depth from given starting point
 * practical version of unfoldt
 *
 * f : S => (T, S, S) - function used to generate tree
 *
 * x : S - starting value
 *
 * n : Int - recursion depth
 *
 * Returns: BinTree[T]
 */
def unfoldtn[S, T](f:S => (T, S, S), x:S, n:Int): BinTree[T] = 
  if n <= 0 then BinTree.Empty(None) 
  else f(x) match {
    case (a, y, z) => BinTree.Node(a, unfoldtn(f, y, n-1), unfoldtn(f, z, n-1))
  }

/* adj - helper function defined in paper, takes two tuples and adds corresponding elements
 * 
 * p1 : (Int, Int) - first int pair
 *
 * p2 : (Int, Int) - second int pair
 *
 * Returns: (Int, Int)
 */
def adj(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = (p1(0) + p2(0), p1(1) + p2(1))

/* bf - lists the elements of the tree inorder
 *
 * tree : BinTree[T] - tree being traversed
 *
 * Returns: List[T]
 */
def bf[T](tree:BinTree[T]): List[T] = 
  def zipWithListConcat(xs: List[List[T]], ys: List[List[T]]): List[List[T]] = xs match {
    case x :: xs => ys match {
      case y :: ys => (x ::: y) :: zipWithListConcat(xs, ys)
      case _ => throw new Exception(s"zipWithListConcat: xs is longer than ys")
    }
    case Nil => ys match {
      case Nil => Nil
      case _ => throw new Exception(s"zipWithListConcat: ys is longer than xs")
    }
  }
  def glue(a:T, xs: List[List[T]], ys: List[List[T]]): List[List[T]] = 
    List(a) :: zipWithListConcat(xs, ys)
  foldt(glue, Nil, tree).flatten

/* helper */
def step(lr: ((Int, Int), (Int, Int))): 
  (Rational, ((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))) = lr match {
    case (l, r) => adj(l, r) match {
      case m => (mkRat(m(0), m(1)), (l, m), (m, r))
    }
  }

/* enumRationals4 - enumerates the rationals by constructing and destroying the Stern-Brocot Tree
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals4(n: Int): List[Rational] = 
  bf(unfoldtn(step, ((0,1), (1,0)), n))

/* enumRationals4_1 - the code for rats_4 as specified in the code generates a pre-order list
 * to preserve the order of the tree, we will use inorder traversal, which the paper specifies but doesn't use
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals4_1(n: Int) = 
  def bf_improved[T](tree:BinTree[T]): List[T] = 
    def glue(a:T, xs: List[List[T]], ys: List[List[T]]): List[List[T]] = 
      xs ::: (List(a) :: ys)
    foldt(glue, Nil, tree).flatten
  bf_improved(unfoldtn(step, ((0,1), (1,0)), n))

/* unfolds - unfolds list given starting value and generating function
 *
 * f : S => (T, S) - used to generate new elements of list
 *
 * a : S - starting input of f
 *
 * Returns: List[T]
 */
def unfolds[S, T](f: S => (T, S), a:S): List[T] = f(a) match {
  case (b, a_prime) => b :: unfolds(f, a_prime)
}

/* unfoldsn - unfolds list given starting value and generating function
 *
 * f : S => (T, S) - used to generate new elements of list
 *
 * a : S - starting input of f
 *
 * n : Int - maximum length of list
 *
 * Returns: List[T]
 */
def unfoldsn[S, T](f: S => (T, S), a:S, n: Int): List[T] = 
  if n <= 0 then Nil
  else f(a) match {
    case (b, a_prime) => b :: unfoldsn(f, a_prime, n-1)
  }

/* interleave - alternates elements of lists, starting with xs
 *
 * xs : List[T] - first list being interleaved
 *
 * ys: List[T] - second list being interleaved
 *
 * Returns: List[T]
 */
def interleave[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case x::xs => x :: interleave(ys, xs)
  case Nil => ys match {
    case Nil => Nil
    case _ => throw new Exception(s"interleave: undefined behavior")
  }
}

/* infill - TODO - fill in details
 *
 */
def infill(xs: List[(Int, Int)]): (List[(Int, Int)], List[(Int, Int)]) = 
  /* used Haskell docs to be more sure of behavior
   * https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:zipWith
   */
  def zipWithAdj(xs: List[(Int, Int)], ys: List[(Int, Int)]): List[(Int, Int)] = xs match {
    case x::xs => ys match {
      case Nil => Nil
      case y::ys => adj(x, y) :: zipWithAdj(xs, ys)
    }
    case Nil => Nil
  }
  zipWithAdj(xs, xs.tail) match {
    case ys => (ys, interleave(xs, ys))
  }

/* enumRationals5 - deforested version of enumRationals4 (supposedly faster)
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals5(n: Int): List[Rational] = 
  unfoldsn(infill, (0,1) :: (1, 0) :: Nil, n).flatten

/* === 4 The Calkin-Wilf Tree === */

/* helper */
def stepCW(mn: (Int, Int)): (Rational, (Int, Int), (Int, Int)) = mn match {
  case (m, n) => (mn, (m, m+n), (n+m, n)) 
}

/* enumRationals6 - Calkin-Wilf enumration using reverse binary sequences to position the elements in the tree
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals6(n: Int): List[Rational] = 
  bf(unfoldtn(stepCW, (1,1), n))

/* === 5 Iterating Through The Rationals === */

/* iterate - iterates on x using an interation function f
 *
 * f : T -> T - iteration function
 *
 * x : input for f
 *
 * Returns: List[T] - x :: f(x) :: f(f(x)) :: f(f(f(x))) :: ...
 */
def iterate[T](f: T => T, x : T): List[T] = x :: iterate(f, f(x))

/* iteraten - iterates with bound n
 *
 * f : T -> T - iteration function
 *
 * x : input for f
 *
 * n : Int - max iterations
 *
 * Returns: List[T] - x :: f(x) :: f(f(x)) :: f(f(f(x))) :: ... :: f^(n-1)(x) :: Nil
 */
def iteraten[T](f: T => T, x : T, n : Int): List[T] = 
  if n <= 0 then Nil
  else x :: iteraten(f, f(x), n-1)

/* as specified by paper */
def properFraction(x: Rational): (Int, Rational) = x match {
  case (n, d) if d < 0 => properFraction((-n, -d))
  case (n, d) => (n / d, (n % d, d))
}

/* as specified by paper */
def recip(x: Rational): Rational = x match {
  case (0, _) => throw new Exception(s"recip: taking reciprocal of 0")
  case (n, d) => (d, n)
}

def next(x: Rational): Rational = properFraction(x) match {
  case (n, y) => y match {
    /* gcd(ynum, yden) = 1 implies gcd((n + 1) * yden + ynum, yden) = 1 by Bezout's theorem */
    case (ynum, yden) => recip(((n + 1) * yden - ynum, yden))
  }
}

/* enumRationals7 - very clever "next" function that needs only the previous rational to keep generating them
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals7(n: Int): List[Rational] = iteraten(next, (1,1), n)

/* enumRationals8 - same enumration as rats_7, but with 0 and negatives included
 *
 * n : Int - used to create bound on number of boolean sequences generated
 * 
 * Returns: List[Rational] - enumrated list of rationals
 */
def enumRationals8(n: Int): List[Rational] = 
  def next_prime(x: Rational): Rational = x match {
    case (0, _) => (1, 1)
    /* condition is true if rational is positive */
    case (n, d) if (n < 0) == (d < 0) => (-n, d)
    case (n, d) => next((-n, d))
  }
  iteraten(next_prime, (0,1), n)
