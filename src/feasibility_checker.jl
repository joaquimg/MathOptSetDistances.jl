const MOD = MathOptSetDistances
const MOIU = MOI.Utilities

variable_primal(model) = x -> MOI.get(model, MOI.VariablePrimal(), x)
variable_primal_start(model) = x -> MOI.get(model, MOI.VariablePrimalStart(), x)

"""
    FeasibilityCheckerOptions(;names = true, index = true)

Options for the function `constraint_violation_report`.

* `names`: is false, constraint names are not displayed in the violation report.

* `index`: is false, constraint indexes are not displayed in the violation report.
"""
struct FeasibilityCheckerOptions
    names::Bool
    index::Bool
    function FeasibilityCheckerOptions(;names = true, index = true)
        return new(
            names,
            index,
        )
    end
end

"""
    constraint_violation_report(model::MOI.ModelLike)

Given a Model `model` return a string with a detailed report of the constraint
violations in the problem.

constraint_violation_report(model::MOI.ModelLike;
                         varval::Function = variable_primal(model),
                         distance::MOD.AbstractDistance = MOD.DefaultDistance(),
                         tol = 0.0,
                         options = FeasibilityCheckerOptions())

Expanded method to prodive more flexibility to the user.

* `varval` can be used to define a map `vi -> value` where `vi` is a
`MOI.VariableIndex` in the model. This map should be defined for all variables
tha appear in teh constraint.

* `distance_map` is an abstract dictionary mapping function-set pairs `(F, S)`
to `distance`s that are instances of `AbstractDistance`. The function
`distance_to_set(distance, v, set)` must be implemented for this type for the
set of the constraint `con`. Is some function-set pair `(F, S)` is not found in
the keys of the dictionay then the MOD.DefaultDistance() is used.

* `tol` is used to ignore violations larger than its value.

* `options` is an instance of FeasibilityCheckerOptions, see its docs for more
information.
"""
function constraint_violation_report(model::MOI.ModelLike;
    varval::Function = variable_primal(model),
    distance_map::AbstractDict = Dict(),
    tol = 0.0,
    options = FeasibilityCheckerOptions())

    largest, vec = constraint_violation(model, varval = varval, distance_map = distance_map)

    str =
        """
        Feasibility report

        * Maximum overall violation = $(largest)

        * Maximum violation per constraint type

        """

    # sort!(vec)

    for (val, ref) in vec
        if val >= tol
            str *= _violation_string(model, ref, val, distance_map, options)
        end
    end

    return str
end

"""
    _violation_string(model, ref::MOI.ConstraintIndex{F, S},
                      val, distance_map, options)

Internal method to build lines of the `constraint_violation_report`.
"""
function _violation_string(model, ref::MOI.ConstraintIndex{F, S},
    val, distance_map, options
) where {F, S}
    str = " ($F, $S) = $(val)"
    if haskey(distance_map, (F, S))
        str *= " [$(distance_map[F, S])]"
    end
    str *= "\n"
    if options.index
        str *= "     Index: $(ref)\n"
    end
    if options.names && MOI.supports(model, MOI.ConstraintName(),
                                     MOI.ConstraintIndex{F, S})
        name = try
            MOI.get(model, MOI.ConstraintName(), ref)
        catch
            nothing
        end
        if name === nothing 
            str *= "     Name: $(name)\n"
        end
    end
    return str
end

"""
    constraint_violation(model::MOI.ModelLike)

Given a Model `model` return the largest violation value of all constraints and 
a vector with tuples, with the largest violation by constraint type and the
constraint that attains that violation value.

    constraint_violation(model::MOI.ModelLike;
                         varval::Function = variable_primal(model),
                         distance_map::MOD.AbstractDistance = MOD.DefaultDistance())

Expanded method to prodive more flexibility to the user.

* `varval` can be used to define a map `vi -> value` where `vi` is a
`MOI.VariableIndex` in the model. This map should be defined for all variables
tha appear in teh constraint.

* `distance_map` is an abstract dictionary mapping function-set pairs `(F, S)`
to `distance`s that are instances of `AbstractDistance`. The function
`distance_to_set(distance, v, set)` must be implemented for this type for the
set of the constraint `con`. Is some function-set pair `(F, S)` is not found in
the keys of the dictionay then the MOD.DefaultDistance() is used.
"""
function constraint_violation(model::MOI.ModelLike;
    varval::Function = variable_primal(model),
    distance_map::AbstractDict = Dict()
)
    vec = Any[]
    largest = 0.0
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        distance = if haskey(distance_map, (F, S))
            distance_map[F, S]
        else
            MOD.DefaultDistance()
        end
        val, ref = constraint_violation(model, F, S, varval = varval, distance = distance)
        push!(vec, (val, ref))
        if val >= largest
            largest = val
        end
    end
    return largest, vec
end

"""
    constraint_violation(model::MOI.ModelLike, F, S)

Given a Model `model` and one funtions `F` and set `S` pair, return the largest
violation value and the reference to the constraint with largest violation of
the constraint type define by the pair with respect the currently
available solution and the default distance for the constraint set.

    constraint_violation(model::MOI.ModelLike, F, S;
                         varval::Function = variable_primal(model),
                         distance::MOD.AbstractDistance = MOD.DefaultDistance())

Expanded method to prodive more flexibility to the user.

* `varval` can be used to define a map `vi -> value` where `vi` is a
`MOI.VariableIndex` in the model. This map should be defined for all variables
tha appear in teh constraint.

* `distance` is an instance of `AbstractDistance`. The function
`distance_to_set(distance, v, set)` must be implemented for this type for the
set of the constraint `con`.
"""
function constraint_violation(model::MOI.ModelLike,
    ::Type{F}, ::Type{S};
    varval::Function = variable_primal(model),
    distance::MOD.AbstractDistance = MOD.DefaultDistance()
) where {F, S}
    largest = 0.0
    largest_ref = MOI.ConstraintIndex{F, S}(-1)
    list = MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
    if isempty(list)
        return NaN, nothing
    end
    for con in list
        val = constraint_violation(model, con, varval = varval, distance = distance)
        if val >= largest
            largest = val
            largest_ref = con
        end
    end
    return largest, largest_ref
end

"""
    constraint_violation(model::MOI.ModelLike, con::MOI.ConstraintIndex)

Given a Model `model` and one of it's constraints `con`, return the violation of
the constraint with respect the currently available solution and the default
distance for the constraint set.

    constraint_violation(model::MOI.ModelLike, con::MOI.ConstraintIndex;
                         varval::Function = variable_primal(model),
                         distance::MOD.AbstractDistance = MOD.DefaultDistance())

Expanded method to prodive more flexibility to the user.

* `varval` can be used to define a map `vi -> value` where `vi` is a
`MOI.VariableIndex` in the model. This map should be defined for all variables
tha appear in teh constraint.

* `distance` is an instance of `AbstractDistance`. The function
`distance_to_set(distance, v, set)` must be implemented for this type for the
set of the constraint `con`.
"""
function constraint_violation(model::MOI.ModelLike, con::MOI.ConstraintIndex;
    varval::Function = variable_primal(model),
    distance::MOD.AbstractDistance = MOD.DefaultDistance()
)
    func = MOI.get(model, MOI.ConstraintFunction(), con)
    set  = MOI.get(model, MOI.ConstraintSet(), con)
    val  = MOIU.eval_variables(varval, func)
    dist = distance_to_set(distance, val, set)
    return dist
end
