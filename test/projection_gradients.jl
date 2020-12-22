using Test
using MathOptInterface
const MOI = MathOptInterface

using MathOptSetDistances
const MOD = MathOptSetDistances

using FiniteDifferences
using LinearAlgebra

const bfdm = FiniteDifferences.backward_fdm(5, 1)
const ffdm = FiniteDifferences.forward_fdm(5, 1)

"""
A multivariate Gaussian generator without points too close to 0
"""
function safe_randn(n)
    v = 2 * randn(n)
    for i in eachindex(v)
        while v[i] ≈ 0
            v[i] = 2 * randn()
        end
    end
    return v
end

@testset "Test gradients with finite differences" begin
    Ntrials = 10
    @testset "Dimension $n" for n in (1, 3, 10)
        vector_sets = (
            MOI.Zeros(n),
            MOI.Reals(n),
            MOI.Nonnegatives(n),
            MOI.Nonpositives(n),
        )
        vs = [safe_randn(n) for _ in 1:Ntrials]
        for _ in 1:Ntrials
            push!(vs, rand(-5:2:5, n))
        end
        @testset "Vector set $s" for s in vector_sets
            @testset "Vector $v" for v in vs
                dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                grad_fdm1 = FiniteDifferences.jacobian(bfdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                grad_fdm2 = FiniteDifferences.jacobian(ffdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                @test size(grad_fdm1) == size(grad_fdm2) == size(dΠ)
                # finite diff can mess up for low v
                @test all(eachindex(v)) do idx
                    dΠ[idx,idx] ≈ grad_fdm1[idx,idx] || dΠ[idx,idx] ≈ grad_fdm2[idx,idx]
                end
            end
        end
        @testset "PSD cone" begin
            s = MOI.PositiveSemidefiniteConeTriangle(n)
            for _ in 1:Ntrials
                L = 3 * tril(rand(n, n))
                M = L * L'
                @testset "Positive definite" begin
                    v = MOD.vec_symm(M)
                    dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                    grad_fdm1 = FiniteDifferences.jacobian(ffdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    grad_fdm2 = FiniteDifferences.jacobian(bfdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    @test size(grad_fdm1) == size(grad_fdm2) == size(dΠ)
                    @test dΠ ≈ I
                end
                @testset "Negative definite" begin
                    v = MOD.vec_symm(-M)
                    dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                    grad_fdm1 = FiniteDifferences.jacobian(ffdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    grad_fdm2 = FiniteDifferences.jacobian(bfdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    @test size(grad_fdm1) == size(grad_fdm2) == size(dΠ)
                    if !isapprox(det(M), 0, atol=10e-6)
                        @test all(dΠ .≈ 0)
                    end
                end
            end
        end
        @testset "SOC" begin
            s = MOI.SecondOrderCone(n+1)
            for _ in 1:Ntrials
                x = safe_randn(n)
                @testset "SOC interior and negative bound" begin
                    t = LinearAlgebra.norm2(x) + 2 * rand()
                    v = vcat(t, x)
                    dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                    grad_fdm1 = FiniteDifferences.jacobian(ffdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    grad_fdm2 = FiniteDifferences.jacobian(bfdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    @test size(grad_fdm1) == size(grad_fdm2) == size(dΠ)
                    @test dΠ ≈ grad_fdm1 || dΠ ≈ grad_fdm2
                    v = vcat(-t, x)
                    dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                    grad_fdm1 = FiniteDifferences.jacobian(ffdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    grad_fdm2 = FiniteDifferences.jacobian(bfdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                    @test size(grad_fdm1) == size(grad_fdm2) == size(dΠ)
                    @test dΠ ≈ grad_fdm1 || dΠ ≈ grad_fdm2
                end
                @testset "Out of cone point" begin
                    for tscale in (0.1, 0.5, 0.9)
                        t = tscale * LinearAlgebra.norm2(x)
                        v = vcat(t, x)
                        dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                        grad_fdm1 = FiniteDifferences.jacobian(ffdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                        grad_fdm2 = FiniteDifferences.jacobian(bfdm, x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)[1]'
                        @test size(grad_fdm1) == size(grad_fdm2) == size(dΠ)
                        @test dΠ ≈ grad_fdm1 || dΠ ≈ grad_fdm2
                        t = tscale
                        xr = x / norm(x)
                        v = vcat(t, xr)
                        dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
                        # theoretical expression for unit vector
                        @test 2dΠ ≈ [
                            1 xr'
                            xr ((t + 1) * I - t * xr * xr')
                        ]
                    end
                end
            end
        end
    end
    @testset "Indefinite matrix" begin
        s = MOI.PositiveSemidefiniteConeTriangle(2)
        for _ in 1:Ntrials
            # scale factor
            f = 20 * rand() + 0.5
            A = [
                -f 0
                0 f
            ]
            Q = [
                1 0
                0 -1
            ]
            Qi = Q'
            Λ = Diagonal([-f, f])
            Λp = Diagonal([0, f])
            Q * Λ * Qi
            @test A ≈ Q * Λ * Qi
            v = MOD.vec_symm(A)
            Πv = MOD.projection_on_set(MOD.DefaultDistance(), v, s)
            Π = MOD.unvec_symm(Πv, 2)
            @test Π ≈ Q * Λp * Qi
            B = [
                0 1/2
                1/2 1
            ]
            DΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), 1.0v, s)
            for _ in 1:50
                Xd = randn(2,2)
                xd = MOD.vec_symm(Xd)
                @test DΠ * xd ≈ MOD.vec_symm(Q * (B .* (Q' * Xd * Q)) * Q)
            end
        end
    end
    @testset "Scalar $ST" for ST in (MOI.LessThan, MOI.GreaterThan, MOI.EqualTo)
        s = ST(10 * randn())
        for v in 1:Ntrials
            v = randn()
            while v ≈ MOI.constant(s)
                v += 2 * randn()
            end
            dfor = ffdm(x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)
            dback = bfdm(x -> MOD.projection_on_set(MOD.DefaultDistance(), x, s), v)
            dΠ = MOD.projection_gradient_on_set(MOD.DefaultDistance(), v, s)
            @test ≈(dfor, dΠ, atol=1e-5) || ≈(dback, dΠ, atol=1e-5)
        end
    end
end
