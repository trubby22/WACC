package ic.doc.group15.semantics

open class SemanticError(message: String) : Throwable(message)

class TypeError(message: String) : SemanticError(message)

class DeclarationError(message: String) : SemanticError(message)

class IdentifierError(message: String) : SemanticError(message)

class ParameterError(message: String) : SemanticError(message)

class IllegalStatementError(message: String): SemanticError(message)
