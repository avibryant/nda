# nda
N-Dimensional Arrays in Scala

This is a toy implementation of TensorFlow-style multidimensional arrays in Scala,
including a static computation graph with automatic derivation of gradients, but
with compile-time enforcement and derivation of array shapes and the TF/NumPy broadcasting rules.

As a proof of concept, it currently only supports a handful of basic arithmetic operations (+, *, -, and reduce by sum),
and a dense-Array CPU-based evaluator, but is easily extensible to support more operations, GPU evaluation, etc.

See src/examples/Regression.scala for a simple demonstration.

@avibryant