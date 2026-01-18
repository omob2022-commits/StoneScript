#include <iostream>
#include <string>
#include <cctype>
#include <vector>
#include <memory>

enum class TokenType {
    // Keywords
    LET, PRINT,
    
    // Identifiers and Literals
    IDENTIFIER, INTEGER,
    
    // Operators
    ASSIGN, PLUS, MINUS,
    
    // Delimiters
    SEMICOLON,
    
    // Special
    EOF_TYPE
};

struct Token {
    TokenType type;
    std::string value;
};

class Lexer {
private:
    std::string input;
    size_t cursor;

    void skipWhitespace() {
        while (cursor < input.size() && std::isspace(input[cursor])) {
            cursor++;
        }
    }

    std::string readNumber() {
        std::string num;
        while (cursor < input.size() && std::isdigit(input[cursor])) {
            num += input[cursor];
            cursor++;
        }
        return num;
    }

    std::string readIdentifier() {
        std::string ident;
        while (cursor < input.size() && (std::isalnum(input[cursor]) || input[cursor] == '_')) {
            ident += input[cursor];
            cursor++;
        }
        return ident;
    }

    TokenType getKeywordType(const std::string& word) {
        if (word == "let") return TokenType::LET;
        if (word == "print") return TokenType::PRINT;
        return TokenType::IDENTIFIER;
    }

public:
    Lexer(const std::string& source) : input(source), cursor(0) {}

    Token getNextToken() {
        skipWhitespace();

        if (cursor >= input.size()) {
            return Token{TokenType::EOF_TYPE, ""};
        }

        char current = input[cursor];

        if (std::isdigit(current)) {
            std::string numStr = readNumber();
            return Token{TokenType::INTEGER, numStr};
        }

        if (std::isalpha(current) || current == '_') {
            std::string ident = readIdentifier();
            TokenType type = getKeywordType(ident);
            return Token{type, ident};
        }

        cursor++;
        switch (current) {
        case '=':
            return Token{TokenType::ASSIGN, "="};
        case '+':
            return Token{TokenType::PLUS, "+"};
        case '-':
            return Token{TokenType::MINUS, "-"};
        case ';':
            return Token{TokenType::SEMICOLON, ";"};
        default:
            throw std::runtime_error(std::string("Unexpected character: ") + current);
        }
    }

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (true) {
            Token token = getNextToken();
            tokens.push_back(token);
            if (token.type == TokenType::EOF_TYPE) {
                break;
            }
        }
        return tokens;
    }
};

// AST Node Types
struct ASTNode {
    virtual ~ASTNode() = default;
};

struct IntegerNode : ASTNode {
    int value;
    IntegerNode(int val) : value(val) {}
};

struct IdentifierNode : ASTNode {
    std::string name;
    IdentifierNode(const std::string& n) : name(n) {}
};

struct BinaryOpNode : ASTNode {
    std::shared_ptr<ASTNode> left;
    std::shared_ptr<ASTNode> right;
    TokenType op;
    BinaryOpNode(std::shared_ptr<ASTNode> l, std::shared_ptr<ASTNode> r, TokenType o)
        : left(l), right(r), op(o) {}
};

struct VariableDeclNode : ASTNode {
    std::string name;
    std::shared_ptr<ASTNode> value;
    VariableDeclNode(const std::string& n, std::shared_ptr<ASTNode> v)
        : name(n), value(v) {}
};

struct PrintNode : ASTNode {
    std::shared_ptr<ASTNode> expression;
    PrintNode(std::shared_ptr<ASTNode> expr) : expression(expr) {}
};

struct ProgramNode : ASTNode {
    std::vector<std::shared_ptr<ASTNode>> statements;
};

// Parser
class Parser {
private:
    std::vector<Token> tokens;
    size_t current;

    Token peek() {
        if (current < tokens.size()) {
            return tokens[current];
        }
        return Token{TokenType::EOF_TYPE, ""};
    }

    Token advance() {
        Token token = peek();
        if (current < tokens.size()) {
            current++;
        }
        return token;
    }

    void expect(TokenType type) {
        if (peek().type != type) {
            throw std::runtime_error("Unexpected token type");
        }
        advance();
    }

    std::shared_ptr<ASTNode> parseExpression() {
        return parseAddition();
    }

    std::shared_ptr<ASTNode> parseAddition() {
        auto left = parseMultiplication();

        while (peek().type == TokenType::PLUS || peek().type == TokenType::MINUS) {
            TokenType op = peek().type;
            advance();
            auto right = parseMultiplication();
            left = std::make_shared<BinaryOpNode>(left, right, op);
        }

        return left;
    }

    std::shared_ptr<ASTNode> parseMultiplication() {
        return parsePrimary();
    }

    std::shared_ptr<ASTNode> parsePrimary() {
        Token token = peek();

        if (token.type == TokenType::INTEGER) {
            advance();
            return std::make_shared<IntegerNode>(std::stoi(token.value));
        }

        if (token.type == TokenType::IDENTIFIER) {
            advance();
            return std::make_shared<IdentifierNode>(token.value);
        }

        throw std::runtime_error("Unexpected token in expression");
    }

    std::shared_ptr<ASTNode> parseStatement() {
        Token token = peek();

        if (token.type == TokenType::LET) {
            return parseVariableDeclaration();
        }

        if (token.type == TokenType::PRINT) {
            return parsePrintStatement();
        }

        throw std::runtime_error("Unexpected statement");
    }

    std::shared_ptr<ASTNode> parseVariableDeclaration() {
        expect(TokenType::LET);
        Token nameToken = peek();
        expect(TokenType::IDENTIFIER);
        expect(TokenType::ASSIGN);
        auto value = parseExpression();
        expect(TokenType::SEMICOLON);

        return std::make_shared<VariableDeclNode>(nameToken.value, value);
    }

    std::shared_ptr<ASTNode> parsePrintStatement() {
        expect(TokenType::PRINT);
        auto expr = parseExpression();
        expect(TokenType::SEMICOLON);

        return std::make_shared<PrintNode>(expr);
    }

public:
    Parser(const std::vector<Token>& t) : tokens(t), current(0) {}

    std::shared_ptr<ProgramNode> parse() {
        auto program = std::make_shared<ProgramNode>();

        while (peek().type != TokenType::EOF_TYPE) {
            program->statements.push_back(parseStatement());
        }

        return program;
    }
};

int main() {
    // Test the Lexer and Parser
    std::string code = "let x = 10 + 5; print x;";
    
    // Lexing
    Lexer lexer(code);
    std::vector<Token> tokens = lexer.tokenize();

    std::cout << "=== TOKENS ===" << std::endl;
    for (const auto& token : tokens) {
        std::cout << "Token: ";
        switch (token.type) {
        case TokenType::LET:
            std::cout << "LET";
            break;
        case TokenType::PRINT:
            std::cout << "PRINT";
            break;
        case TokenType::IDENTIFIER:
            std::cout << "IDENTIFIER(" << token.value << ")";
            break;
        case TokenType::INTEGER:
            std::cout << "INTEGER(" << token.value << ")";
            break;
        case TokenType::ASSIGN:
            std::cout << "ASSIGN";
            break;
        case TokenType::PLUS:
            std::cout << "PLUS";
            break;
        case TokenType::MINUS:
            std::cout << "MINUS";
            break;
        case TokenType::SEMICOLON:
            std::cout << "SEMICOLON";
            break;
        case TokenType::EOF_TYPE:
            std::cout << "EOF";
            break;
        }
        std::cout << std::endl;
    }

    // Parsing
    std::cout << "\n=== AST ===" << std::endl;
    Parser parser(tokens);
    auto ast = parser.parse();
    std::cout << "Program has " << ast->statements.size() << " statements" << std::endl;

    return 0;
}