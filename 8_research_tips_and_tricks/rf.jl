"""
	Function that trains a random forest given the data and the parameters
	Returns training and validation R^2
"""
function trainrandomforest(train::DataFrame, val::DataFrame,
						   max_depth::Int, n_estimators::Int, max_features::Int)
	Xtr = convert(Matrix, train[2:end])
	Ytr = train[1]
	Xva = convert(Matrix, val[2:end])
	Yva = val[1]
	rf = RandomForestClassifier(max_depth=max_depth,
	                            n_estimators=n_estimators,
	                            max_features=max_features)
	fit!(rf, Xtr, Ytr)
	predictTr = predict(rf, Xtr)
	predictVa = predict(rf, Xva)
	R2tr = 1 - sum((predictTr .- Ytr) .^ 2)/sum((Ytr .- mean(Ytr)) .^ 2)
	R2va = 1 - sum((predictVa .- Yva) .^ 2)/sum((Yva .- mean(Ytr)) .^ 2)
	return R2tr, R2va
end
