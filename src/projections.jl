# find expression of projections on cones and their derivatives here:
#   https://stanford.edu/~boyd/papers/pdf/cone_prog_refine.pdf

"""
    projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Zeros) where {T}

projection of vector `v` on zero cone i.e. K = {0}
"""
function projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Zeros) where {T}
    return zeros(T, size(v))
end

"""
    projection_on_set(::AbstractDistance, ::MOI.Reals, v::Array{T}) where {T}

projection of vector `v` on real cone i.e. K = R
"""
function projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Reals) where {T}
    return v
end

function projection_on_set(::DefaultDistance, v::T, set::MOI.EqualTo) where {T}
    return zero(T) .+ set.value
end

"""
    projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Nonnegatives) where {T}

projection of vector `v` on Nonnegative cone i.e. K = R+
"""
function projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Nonnegatives) where {T}
    return max.(v, zero(T))
end

"""
    projection_on_set(::NormedEpigraphDistance{p}, v::AbstractVector{T}, ::MOI.SecondOrderCone) where {T}

projection of vector `v` on second order cone i.e. K = {(t, x) ∈ R+ × Rn |  ||x|| ≤ t }
"""
function projection_on_set(::NormedEpigraphDistance{p}, v::AbstractVector{T}, ::MOI.SecondOrderCone) where {p, T}
    t = v[1]
    x = v[2:length(v)]
    norm_x = LinearAlgebra.norm(x, p)
    if norm_x <= t
        return copy(v)
    elseif norm_x <= -t
        return zeros(T, size(v))
    else
        result = zeros(T, size(v))
        result[1] = one(T)
        result[2:length(v)] = x / norm_x
        result *= (norm_x + t) / 2.0
        return result
    end
end

function projection_on_set(::DefaultDistance, v::AbstractVector{T}, cone::MOI.SecondOrderCone) where {T}
    return projection_on_set(NormedEpigraphDistance{2}(), v, cone)
end

"""
    projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.PositiveSemidefiniteConeTriangle) where {T}

projection of vector `v` on positive semidefinite cone i.e. K = S^n⨥
"""
function projection_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.PositiveSemidefiniteConeTriangle) where {T}
    dim = isqrt(2*length(v))
    X = unvec_symm(v, dim)
    λ, U = LinearAlgebra.eigen(X)
    D = LinearAlgebra.Diagonal(max.(λ, 0))
    return vec_symm(U * D * U')
end

"""
    unvec_symm(x, dim)

Returns a dim-by-dim symmetric matrix corresponding to `x`.

`x` is a vector of length dim*(dim + 1)/2, corresponding to a symmetric matrix
X = [ X11 X12 ... X1k
        X21 X22 ... X2k
        ...
        Xk1 Xk2 ... Xkk ],
where
vec(X) = (X11, X21, ..., Xk1, X22, X32, ..., Xkk)

NOTE: Now this is not specific to SCS/Mosek solver
Earlier used to be: vec(X) = (X11, sqrt(2)*X21, ..., sqrt(2)*Xk1, X22, sqrt(2)*X32, ..., Xkk)
"""
function unvec_symm(x, dim)
    X = zeros(dim, dim)
    idx = 1
    for i in 1:dim
        for j in 1:i
            # @inbounds X[j,i] = X[i,j] = x[(i-1)*dim-div((i-1)*i, 2)+j]
            @inbounds X[j,i] = X[i,j] = x[idx]
            idx += 1 
        end
    end
#     for i in 1:dim
#         for j in i+1:dim
#             X[i, j] /= √2
#             X[j, i] /= √2
#         end
#     end
    return X
end

"""
    vec_symm(X)

Returns a vectorized representation of a symmetric matrix `X`.
`vec(X) = (X11, X21, ..., Xk1, X22, X32, ..., Xkk)`

NOTE: Now vectorization and scaling is not per SCS.
Earlier used to be `vec(X) = (X11, sqrt(2)*X21, ..., sqrt(2)*Xk1, X22, sqrt(2)*X32, ..., Xkk)`
"""
function vec_symm(X)
    return X[LinearAlgebra.tril(trues(size(X)))']
end

"""
    projection_on_set(::DefaultDistance, v::AbstractVector{T}, cones::Array{<:MOI.AbstractSet})

Projection onto `K`, a product of convex cones

Find expression of projections on cones and their derivatives here: https://stanford.edu/~boyd/papers/pdf/cone_prog_refine.pdf
"""
function projection_on_set(::DefaultDistance, v::AbstractVector{T}, cones::Array{<:MOI.AbstractSet}) where {T}
    length(v) == length(cones) || throw(DimensionMismatch("Mismatch between value and set"))
    return vcat([projection_on_set(DefaultDistance(), v[i], cones[i]) for i in eachindex(cones)]...)
end

"""
    projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Zeros) where {T}

derivative of projection of vector `v` on zero cone i.e. K = {0}
"""
function projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Zeros) where {T}
    y = zeros(T, size(v))
    return reshape(y, length(y), 1)
end

"""
    projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Reals) where {T}

derivative of projection of vector `v` on real cone i.e. K = R
"""
function projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Reals) where {T}
    y = ones(T, size(v))
    return reshape(y, length(y), 1)
end

"""
    projection_gradient_on_set(::DefaultDistance, v::T, ::MOI.EqualTo)
"""
function projection_gradient_on_set(::DefaultDistance, ::T, ::MOI.EqualTo) where {T}
    y = zeros(T, 1)
    return reshape(y, length(y), 1)
end

"""
    projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Nonnegatives) where {T}

derivative of projection of vector `v` on Nonnegative cone i.e. K = R+
"""
function projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.Nonnegatives) where {T}
    y = (sign.(v) .+ one(T))/2
    n = length(y)
    result = zeros(T, n, n)
    result[LinearAlgebra.diagind(result)] .= y
    return result
end

"""
    projection_gradient_on_set(::NormedEpigraphDistance{p}, v::AbstractVector{T}, ::MOI.SecondOrderCone) where {T}

derivative of projection of vector `v` on second order cone i.e. K = {(t, x) ∈ R+ × Rn |  ||x|| ≤ t }
"""
function projection_gradient_on_set(::NormedEpigraphDistance{p}, v::AbstractVector{T}, ::MOI.SecondOrderCone) where {p,T}
    n = length(v)
    t = v[1]
    x = v[2:n]
    norm_x = LinearAlgebra.norm(x, p)
    if norm_x <= t
        return Matrix{T}(LinearAlgebra.I,n,n)
    elseif norm_x <= -t
        return zeros(T, n,n)
    else
        result = [
            norm_x     x';
            x          (norm_x + t)*Matrix{T}(LinearAlgebra.I,n-1,n-1) - (t/(norm_x^2))*(x*x')
        ]
        result /= (2.0 * norm_x)
        return result
    end
end

function projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, cone::MOI.SecondOrderCone) where {T}
    return projection_gradient_on_set(NormedEpigraphDistance{2}(), v, cone)
end

"""
    projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, cone::MOI.PositiveSemidefiniteConeTriangle) where {T}

derivative of projection of vector `v` on positive semidefinite cone i.e. K = S^n⨥
"""
function projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, ::MOI.PositiveSemidefiniteConeTriangle) where {T}
    n = length(v)
    dim = isqrt(2*n)
    X = unvec_symm(v, dim)
    λ, U = LinearAlgebra.eigen(X)

    # if all the eigenvalues are >= 0
    if all(λi ≥ zero(λi) for λi in λ)
        return Matrix{T}(LinearAlgebra.I, n, n)
    end

    # k is the number of negative eigenvalues in X minus ONE
    k = count(λ .< 1e-4)

    y = zeros(T, n)
    D = zeros(T, n, n)

    for idx in 1:n
        # set eigenvector
        y[idx] = one(T)

        # defining matrix B
        X̃ = unvec_symm(y, dim)
        B = U' * X̃ * U

        for i in 1:size(B)[1] # do the hadamard product
            for j in 1:size(B)[2]
                if (i <= k && j <= k)
                    @inbounds B[i, j] = 0
                elseif (i > k && j <= k)
                    λpi = max(λ[i], zero(T))
                    λmj = -min(λ[j], zero(T))
                    @inbounds B[i, j] *= λpi / (λmj + λpi)
                elseif (i <= k && j > k)
                    λmi = -min(λ[i], zero(T))
                    λpj = max(λ[j], zero(T))
                    @inbounds B[i, j] *= λpj / (λmi + λpj)
                end
            end
        end
        @inbounds D[idx, :] = vec_symm(U * B * U')
        # reset eigenvector
        @inbounds y[idx] = zero(T)
    end
    return D
end

"""
    projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, cones::Array{<:MOI.AbstractSet})

Derivative of the projection of vector `v` on product of `cones`
projection_gradient_on_set[i,j] = ∂projection_on_set[i] / ∂v[j] where `projection_on_set` denotes projection of `v` on `cone`

Find expression of projections on cones and their derivatives here: https://stanford.edu/~boyd/papers/pdf/cone_prog_refine.pdf
"""
function projection_gradient_on_set(::DefaultDistance, v::AbstractVector{T}, cones::Array{<:MOI.AbstractSet}) where {T}
    length(v) == length(cones) || throw(DimensionMismatch("Mismatch between value and set"))
    return BlockDiagonal([projection_gradient_on_set(DefaultDistance(), v[i], cones[i]) for i in eachindex(cones)])
end
