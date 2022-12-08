# Enumerating the Rationals
Final Project for Programming Languages

## Summary

This small repository reproduces the code described in the paper ["Enumerating the Rationals"](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf) by Jeremy Gibbons, David Lester, and Richard Bird with two small changes.

1. The original paper is in Haskell, while our repository is in Scala. The syntax is quite different, but the ideas are the same.
2. The original paper describes infinite data structures and never-ending functions because infinite behaviour is necessary to enumerate an infinite set.
For the sake of practicality, we introduce parameters that control recursion depth for the infinite functions to study the behaviour of finite sections of output.

