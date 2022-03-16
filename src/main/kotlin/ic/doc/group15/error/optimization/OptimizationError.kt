package ic.doc.group15.error.optimization

import ic.doc.group15.error.WaccError

const val OPTIMIZATION_ERROR_CODE = 1

abstract class OptimizationError(
    message: String
) : WaccError(message) {
    val errorCode: Int = OPTIMIZATION_ERROR_CODE
}
