# Load the packages we need
using JLD2, Plots, DataFrames

# Load the parameters data frame
@load "params.jld2" params

# Create empty arrays that will hold the R2 values
R2_train = []
R2_val = []

# Let's load all the results we want, and put them into the arrays
for i = 1:nrow(params)
	experiment_id = params[i, :experiment_id]
	@load "results/results-$experiment_id.jld2" R2tr R2va
	push!(R2_train, R2tr)
	push!(R2_val, R2va)
end

# Add the R2 values to the dataframe
params[:R2_train] = R2_train
params[:R2_val] = R2_val

# Subset the dataframe to only some of the results
dataforplot = params[(params[:n_estimators] .== 160) .& (params[:max_features] .== 2),:]

# Make the plot
plot(dataforplot[:max_depth], [dataforplot[:R2_train] dataforplot[:R2_val]],
        label = ["Training" "Validation"], xlabel="Maximum tree depth",
        ylabel = "R2")
Plots.savefig("plot.pdf")
