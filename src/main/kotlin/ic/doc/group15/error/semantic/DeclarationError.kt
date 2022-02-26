package ic.doc.group15.error.semantic

import org.antlr.v4.runtime.Token

abstract class DeclarationError protected constructor(
    token: Token,
    message: String
) : SemanticError(token, message)

class FunctionDeclarationInWrongScopeError(
    funcToken: Token
) : DeclarationError(funcToken, "Functions cannot be declared in this scope")

class FunctionAlreadyDeclaredError(
    funcToken: Token,
    funcName: String
) : DeclarationError(funcToken, "Function $funcName already declared")

class ParameterAlreadyDeclaredError(
    paramToken: Token,
    paramName: String
) : DeclarationError(paramToken, "Parameter $paramName already declared in this function")

class IdentifierAlreadyDeclaredError(
    varToken: Token,
    identName: String
) : DeclarationError(varToken, "Identifier $identName already declared in this scope")

class IdentifierNotDefinedError(
    identToken: Token,
    identName: String
) : DeclarationError(identToken, "Identifier $identName not defined")

class IdentifierNotAVariableError(
    identToken: Token,
    identName: String
) : DeclarationError(identToken, "Identifier $identName is not a variable")
