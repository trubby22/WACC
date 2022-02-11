package ic.doc.group15.semantics.ast

open class SemanticError(message: String) : Throwable(
    "A semantic error occurred: $message"
)

class TypeError(message: String) : SemanticError(message)

class DeclarationError(message: String) : SemanticError(message)

class IdentifierError(message: String) : SemanticError(message)

class ParameterError(message: String) : SemanticError(message)

class IllegalStatementError(message: String) : SemanticError(message)

class OutOfBoundsError(message: String) : SemanticError(message)
