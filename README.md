# MathOptSetDistances

[![Build Status](https://github.com/matbesancon/MathOptSetDistances.jl/workflows/CI/badge.svg)](https://github.com/matbesancon/MathOptSetDistances.jl/actions)
[![Coverage](https://codecov.io/gh/matbesancon/MathOptSetDistances.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/matbesancon/MathOptSetDistances.jl)

## Distance to set

`set_distance(d::D, v::V, s::S)` defines the distance of a point `v` to a set `s`. The distance is always 0 if `v ∈ s`.
`S` is a `MOI.AbstractSet`, `v` is a scalar or vector value and `d` a type of distance that is a subtype of `AbstractDistance`.

New sets should implement at least `set_distance(::DefaultDistance, v::V, s::MySet)`.

## Projection on set

`projection_on_set(d::D, v, s::S)` returns the point on `S` that is closest to `v` with respect to the distance `d`.
`projection_gradient_on_set` returns the gradient of this projection, i.e. the transpose of the Jacobian.

## Gradients as ChainRules

Gradients `projection_gradient_on_set` eagerly computes the full derivative matrix.
This is often simpler to test and implement, but leads to unnecessary allocations and expensive operations.
They are also implemented using [ChainRulesCore.jl](https://github.com/JuliaDiff/ChainRulesCore.jl)
methods `rrule` and `frule`.
Both methods should be implemented for each derivative and tested against `projection_gradient_on_set`
and `FiniteDifferences.jl`.

## Special matrix types

When some gradients or projections have structural zeros (sparsity patterns),
they can and should return non-standard matrices including `FillArrays` `Zeros, Eyes, Ones, Fill`,
sparse arrays and `LinearAlgebra.Diagonal`.
