# Fundamentals in Julia and JuMP

This class is the first part of an introduction to the programming language Julia and the JuMP library.

Julia is a "high-level, high-performance dynamic programming language for technical computing", and JuMP is a library that allows us to easily formulate optimization problems and solve them using a variety of solvers.

## Preassignment - Install Julia and basic packages

The first step is to install a recent version of Julia. The current version is 1.0.2\. Binaries of Julia for all platforms are available [here](http://julialang.org/downloads/).

Open a Julia session and install basic packages we will use in the class by running the following command:
```jl
using Pkg
Pkg.add(["IJulia", "DataFrames", "CSV", "JLD2", "Plots"])
```

[IJulia](https://github.com/JuliaLang/IJulia.jl) is the Julia version of IPython/Jupyter, that provides a nice notebook interface to run Julia code, together with text and visualization. [DataFrames](https://github.com/JuliaData/DataFrames.jl) and [CSV](https://github.com/JuliaData/CSV.jl) help load, write and manipulate datasets. [JLD2](https://github.com/JuliaIO/JLD2.jl) enables easing storing of Julia objects (the equivalent of pickle in Python). [Plots](https://github.com/JuliaPlots/Plots.jl) is a very powerful visualization library.

If you encounter any difficulty in the installation or set up of those packages, you can refer to the instructions on their respective github page.

## Preassignment - Gurobi and JuMP

For this class, we will be using the Gurobi mixed-integer programming solver.

### Install Gurobi

Gurobi is commercial software, but they have a very permissive (and free!) academic license. If you have an older version of Gurobi (>= 5.5) on your computer, that should be fine.

- Go to [gurobi.com](http://www.gurobi.com) and sign up for an account
- Get an academic license from the website (section 2.1 of the quick-start guide)
- Download and install the Gurobi optimizer (section 3 of the quick-start guide)
- Activate your academic license (section 4.1 of the quick-start guide)
- you need to do the activation step while connected to the MIT network. If you are off-campus, you can use the [MIT VPN](https://ist.mit.edu/vpn) to connect to the network and then activate (get in touch if you have trouble with this).
- Test your license (section 4.6 of the quick-start guide)

### Install the Gurobi and JuMP in Julia

Installing packages in Julia is easy with the Julia package manager. Just open Julia and enter the following command:

```jl
using Pkg
Pkg.add("Gurobi")
```

If you don't have an academic email or cannot get access for Gurobi for another reason, you should be able to follow along with the open source solver GLPK for much of the class. To install, simply do.

```jl
Pkg.add("GLPK")
```

Also install the JuMP package:

```jl
Pkg.add("JuMP")
```

### Check your installation - Solving a simple LP

Let's try a simple LP! Enter the following JuMP code in Julia and submit all the output to Stellar.

```jl
using JuMP, Gurobi

m = Model(solver=GurobiSolver()) # replace this line by "m = Model()"" if Gurobi does not work
@variable(m, 0 <= x <= 2 )
@variable(m, 0 <= y <= 30 )

@objective(m, Max, 5x + 3*y )
@constraint(m, 1x + 5y <= 3.0 )

print(m)

status = JuMP.solve(m)

println("Objective value: ", JuMP.getobjectivevalue(m))
println("x = ", JuMP.getvalue(x))
println("y = ", JuMP.getvalue(y))
```

## Preassignment - RCall

### Installing RCall

In this class, we will also cover interoperability with other programming languages, such as R. To do so, you need to install the RCall packages, by the following command:

```jl
using Pkg
Pkg.add("RCall")
```

### Check your installation

Check that RCall has been properly installed by replicating this simple example

```jl
julia> using RCall
julia> R"1+2"
RObject{RealSxp}
[1] 3
```

If you do not obtain the desired output, RCall most likely failed to detect a proper version of R installed on your computer. Follow the instructions [here](http://juliainterop.github.io/RCall.jl/stable/installation.html) to fix the issue by installing R 3.4.0+ or pointing RCall into the right directory.  

## Questions?

Email jpauph@mit.edu
