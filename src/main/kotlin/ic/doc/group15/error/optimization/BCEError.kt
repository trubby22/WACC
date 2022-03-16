package ic.doc.group15.error.optimization

abstract class BCEError(
    message: String
) : OptimizationError(message)

class CannotEvaluateIndicesError() : BCEError("Cannot evaluate all indices " +
        "of array element")