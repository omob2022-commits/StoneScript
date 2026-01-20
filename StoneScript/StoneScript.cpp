#include <iostream>
#include <string>
#include <cctype>
#include <vector>
#include <memory>
#include <map>
#include <sstream>
#include <set>

enum class TokenType {
    // Keywords
    LET, PRINT, IF, ELSE, WHILE,

    // Identifiers and Literals
    IDENTIFIER, INTEGER,

    // Operators
    ASSIGN, PLUS, MINUS, MULTIPLY, DIVIDE,
    EQ, NE, LT, GT, LE, GE,

    // Delimiters
    SEMICOLON, LBRACE, RBRACE,

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

    char peekChar(size_t offset = 0) {
        if (cursor + offset < input.size()) {
            return input[cursor + offset];
        }
        return '\0';
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
        if (word == "if") return TokenType::IF;
        if (word == "else") return TokenType::ELSE;
        if (word == "while") return TokenType::WHILE;
        return TokenType::IDENTIFIER;
    }

public:
    Lexer(const std::string& source) : input(source), cursor(0) {}

    Token getNextToken() {
        skipWhitespace();

        if (cursor >= input.size()) {
            return Token{ TokenType::EOF_TYPE, "" };
        }

        char current = input[cursor];

        if (std::isdigit(current)) {
            std::string numStr = readNumber();
            return Token{ TokenType::INTEGER, numStr };
        }

        if (std::isalpha(current) || current == '_') {
            std::string ident = readIdentifier();
            TokenType type = getKeywordType(ident);
            return Token{ type, ident };
        }

        if (current == '=') {
            cursor++;
            if (peekChar(cursor - 1) == '=' && peekChar(cursor) == '=') {
                cursor++;
                return Token{ TokenType::EQ, "==" };
            }
            return Token{ TokenType::ASSIGN, "=" };
        }

        if (current == '!') {
            cursor++;
            if (peekChar(cursor - 1) == '!' && peekChar(cursor) == '=') {
                cursor++;
                return Token{ TokenType::NE, "!=" };
            }
            throw std::runtime_error("Unexpected character: !");
        }

        if (current == '<') {
            cursor++;
            if (peekChar(cursor - 1) == '<' && peekChar(cursor) == '=') {
                cursor++;
                return Token{ TokenType::LE, "<=" };
            }
            return Token{ TokenType::LT, "<" };
        }

        if (current == '>') {
            cursor++;
            if (peekChar(cursor - 1) == '>' && peekChar(cursor) == '=') {
                cursor++;
                return Token{ TokenType::GE, ">=" };
            }
            return Token{ TokenType::GT, ">" };
        }

        cursor++;
        switch (current) {
        case '+':
            return Token{ TokenType::PLUS, "+" };
        case '-':
            return Token{ TokenType::MINUS, "-" };
        case '*':
            return Token{ TokenType::MULTIPLY, "*" };
        case '/':
            return Token{ TokenType::DIVIDE, "/" };
        case ';':
            return Token{ TokenType::SEMICOLON, ";" };
        case '{':
            return Token{ TokenType::LBRACE, "{" };
        case '}':
            return Token{ TokenType::RBRACE, "}" };
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
        : left(l), right(r), op(o) {
    }
};

struct VariableDeclNode : ASTNode {
    std::string name;
    std::shared_ptr<ASTNode> value;
    VariableDeclNode(const std::string& n, std::shared_ptr<ASTNode> v)
        : name(n), value(v) {
    }
};

struct AssignmentNode : ASTNode {
    std::string name;
    std::shared_ptr<ASTNode> value;
    AssignmentNode(const std::string& n, std::shared_ptr<ASTNode> v)
        : name(n), value(v) {
    }
};

struct PrintNode : ASTNode {
    std::shared_ptr<ASTNode> expression;
    PrintNode(std::shared_ptr<ASTNode> expr) : expression(expr) {}
};

struct BlockNode : ASTNode {
    std::vector<std::shared_ptr<ASTNode>> statements;
};

struct IfStatementNode : ASTNode {
    std::shared_ptr<ASTNode> condition;
    std::shared_ptr<BlockNode> thenBlock;
    std::shared_ptr<BlockNode> elseBlock;
    IfStatementNode(std::shared_ptr<ASTNode> cond, std::shared_ptr<BlockNode> then, std::shared_ptr<BlockNode> els = nullptr)
        : condition(cond), thenBlock(then), elseBlock(els) {
    }
};

struct WhileStatementNode : ASTNode {
    std::shared_ptr<ASTNode> condition;
    std::shared_ptr<BlockNode> body;
    WhileStatementNode(std::shared_ptr<ASTNode> cond, std::shared_ptr<BlockNode> b)
        : condition(cond), body(b) {
    }
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
        return Token{ TokenType::EOF_TYPE, "" };
    }

    Token peek(size_t offset) {
        if (current + offset < tokens.size()) {
            return tokens[current + offset];
        }
        return Token{ TokenType::EOF_TYPE, "" };
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
        return parseComparison();
    }

    std::shared_ptr<ASTNode> parseComparison() {
        auto left = parseAddition();

        while (peek().type == TokenType::EQ || peek().type == TokenType::NE ||
            peek().type == TokenType::LT || peek().type == TokenType::GT ||
            peek().type == TokenType::LE || peek().type == TokenType::GE) {
            TokenType op = peek().type;
            advance();
            auto right = parseAddition();
            left = std::make_shared<BinaryOpNode>(left, right, op);
        }

        return left;
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
        auto left = parsePrimary();

        while (peek().type == TokenType::MULTIPLY || peek().type == TokenType::DIVIDE) {
            TokenType op = peek().type;
            advance();
            auto right = parsePrimary();
            left = std::make_shared<BinaryOpNode>(left, right, op);
        }

        return left;
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

    std::shared_ptr<BlockNode> parseBlock() {
        expect(TokenType::LBRACE);
        auto block = std::make_shared<BlockNode>();

        while (peek().type != TokenType::RBRACE && peek().type != TokenType::EOF_TYPE) {
            block->statements.push_back(parseStatement());
        }

        expect(TokenType::RBRACE);
        return block;
    }

    std::shared_ptr<ASTNode> parseStatement() {
        Token token = peek();

        if (token.type == TokenType::LET) {
            return parseVariableDeclaration();
        }

        if (token.type == TokenType::PRINT) {
            return parsePrintStatement();
        }

        if (token.type == TokenType::IF) {
            return parseIfStatement();
        }

        if (token.type == TokenType::WHILE) {
            return parseWhileStatement();
        }

        if (token.type == TokenType::IDENTIFIER && peek(1).type == TokenType::ASSIGN) {
            return parseAssignment();
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

    std::shared_ptr<ASTNode> parseAssignment() {
        Token nameToken = peek();
        expect(TokenType::IDENTIFIER);
        expect(TokenType::ASSIGN);
        auto value = parseExpression();
        expect(TokenType::SEMICOLON);

        return std::make_shared<AssignmentNode>(nameToken.value, value);
    }

    std::shared_ptr<ASTNode> parsePrintStatement() {
        expect(TokenType::PRINT);
        auto expr = parseExpression();
        expect(TokenType::SEMICOLON);

        return std::make_shared<PrintNode>(expr);
    }

    std::shared_ptr<ASTNode> parseIfStatement() {
        expect(TokenType::IF);
        auto condition = parseExpression();
        auto thenBlock = parseBlock();
        std::shared_ptr<BlockNode> elseBlock = nullptr;

        if (peek().type == TokenType::ELSE) {
            advance();
            elseBlock = parseBlock();
        }

        return std::make_shared<IfStatementNode>(condition, thenBlock, elseBlock);
    }

    std::shared_ptr<ASTNode> parseWhileStatement() {
        expect(TokenType::WHILE);
        auto condition = parseExpression();
        auto body = parseBlock();

        return std::make_shared<WhileStatementNode>(condition, body);
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

// Semantic Analyzer with Scope Stack
class SemanticAnalyzer {
private:
    std::vector<std::map<std::string, bool>> scopeStack;

    bool isVariableDefined(const std::string& name) {
        for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
            if (it->find(name) != it->end()) {
                return true;
            }
        }
        return false;
    }

    void defineVariable(const std::string& name) {
        if (!scopeStack.empty()) {
            scopeStack.back()[name] = true;
        }
    }

    void pushScope() {
        scopeStack.push_back(std::map<std::string, bool>());
    }

    void popScope() {
        if (!scopeStack.empty()) {
            scopeStack.pop_back();
        }
    }

    void checkVariable(const std::shared_ptr<ASTNode>& node) {
        if (auto id = std::dynamic_pointer_cast<IdentifierNode>(node)) {
            if (!isVariableDefined(id->name)) {
                throw std::runtime_error("Symbol not found: " + id->name);
            }
        }
        else if (auto binOp = std::dynamic_pointer_cast<BinaryOpNode>(node)) {
            checkVariable(binOp->left);
            checkVariable(binOp->right);
        }
        else if (auto print = std::dynamic_pointer_cast<PrintNode>(node)) {
            checkVariable(print->expression);
        }
    }

    void analyzeNode(const std::shared_ptr<ASTNode>& node) {
        if (auto varDecl = std::dynamic_pointer_cast<VariableDeclNode>(node)) {
            checkVariable(varDecl->value);
            defineVariable(varDecl->name);
        }
        else if (auto assign = std::dynamic_pointer_cast<AssignmentNode>(node)) {
            if (!isVariableDefined(assign->name)) {
                throw std::runtime_error("Cannot assign to undefined variable: " + assign->name);
            }
            checkVariable(assign->value);
        }
        else if (auto printStmt = std::dynamic_pointer_cast<PrintNode>(node)) {
            checkVariable(printStmt->expression);
        }
        else if (auto ifStmt = std::dynamic_pointer_cast<IfStatementNode>(node)) {
            checkVariable(ifStmt->condition);
            pushScope();
            for (const auto& stmt : ifStmt->thenBlock->statements) {
                analyzeNode(stmt);
            }
            popScope();
            if (ifStmt->elseBlock) {
                pushScope();
                for (const auto& stmt : ifStmt->elseBlock->statements) {
                    analyzeNode(stmt);
                }
                popScope();
            }
        }
        else if (auto whileStmt = std::dynamic_pointer_cast<WhileStatementNode>(node)) {
            checkVariable(whileStmt->condition);
            pushScope();
            for (const auto& stmt : whileStmt->body->statements) {
                analyzeNode(stmt);
            }
            popScope();
        }
    }

public:
    void analyze(const std::shared_ptr<ProgramNode>& program) {
        scopeStack.clear();
        pushScope();

        for (const auto& stmt : program->statements) {
            analyzeNode(stmt);
        }

        popScope();
    }
};

// Transpiler (generates C++ code)
class Transpiler {
private:
    std::stringstream code;
    int indentLevel;
    std::vector<std::set<std::string>> declaredVars;

    std::string getIndent() {
        return std::string(indentLevel * 4, ' ');
    }

    bool isVariableDeclaredInScope(const std::string& name) {
        if (!declaredVars.empty()) {
            return declaredVars.back().find(name) != declaredVars.back().end();
        }
        return false;
    }

    void markVariableDeclared(const std::string& name) {
        if (!declaredVars.empty()) {
            declaredVars.back().insert(name);
        }
    }

    void pushScope() {
        declaredVars.push_back(std::set<std::string>());
    }

    void popScope() {
        if (!declaredVars.empty()) {
            declaredVars.pop_back();
        }
    }

    std::string transpileExpression(const std::shared_ptr<ASTNode>& node) {
        if (auto intNode = std::dynamic_pointer_cast<IntegerNode>(node)) {
            return std::to_string(intNode->value);
        }
        else if (auto idNode = std::dynamic_pointer_cast<IdentifierNode>(node)) {
            return idNode->name;
        }
        else if (auto binOp = std::dynamic_pointer_cast<BinaryOpNode>(node)) {
            std::string left = transpileExpression(binOp->left);
            std::string right = transpileExpression(binOp->right);
            std::string op;

            switch (binOp->op) {
            case TokenType::PLUS: op = "+"; break;
            case TokenType::MINUS: op = "-"; break;
            case TokenType::MULTIPLY: op = "*"; break;
            case TokenType::DIVIDE: op = "/"; break;
            case TokenType::EQ: op = "=="; break;
            case TokenType::NE: op = "!="; break;
            case TokenType::LT: op = "<"; break;
            case TokenType::GT: op = ">"; break;
            case TokenType::LE: op = "<="; break;
            case TokenType::GE: op = ">="; break;
            default: op = "?"; break;
            }

            return "(" + left + " " + op + " " + right + ")";
        }
        return "";
    }

    void transpileStatement(const std::shared_ptr<ASTNode>& node) {
        if (auto varDecl = std::dynamic_pointer_cast<VariableDeclNode>(node)) {
            code << getIndent() << "int " << varDecl->name << " = " << transpileExpression(varDecl->value) << ";\n";
            markVariableDeclared(varDecl->name);
        }
        else if (auto assign = std::dynamic_pointer_cast<AssignmentNode>(node)) {
            code << getIndent() << assign->name << " = " << transpileExpression(assign->value) << ";\n";
        }
        else if (auto printStmt = std::dynamic_pointer_cast<PrintNode>(node)) {
            code << getIndent() << "std::cout << " << transpileExpression(printStmt->expression) << " << std::endl;\n";
        }
        else if (auto ifStmt = std::dynamic_pointer_cast<IfStatementNode>(node)) {
            code << getIndent() << "if (" << transpileExpression(ifStmt->condition) << ") {\n";
            indentLevel++;
            pushScope();
            for (const auto& stmt : ifStmt->thenBlock->statements) {
                transpileStatement(stmt);
            }
            popScope();
            indentLevel--;
            if (ifStmt->elseBlock) {
                code << getIndent() << "} else {\n";
                indentLevel++;
                pushScope();
                for (const auto& stmt : ifStmt->elseBlock->statements) {
                    transpileStatement(stmt);
                }
                popScope();
                indentLevel--;
            }
            code << getIndent() << "}\n";
        }
        else if (auto whileStmt = std::dynamic_pointer_cast<WhileStatementNode>(node)) {
            code << getIndent() << "while (" << transpileExpression(whileStmt->condition) << ") {\n";
            indentLevel++;
            pushScope();
            for (const auto& stmt : whileStmt->body->statements) {
                transpileStatement(stmt);
            }
            popScope();
            indentLevel--;
            code << getIndent() << "}\n";
        }
    }

public:
    Transpiler() : indentLevel(0) {}

    std::string transpile(const std::shared_ptr<ProgramNode>& program) {
        code.str("");
        declaredVars.clear();
        pushScope();

        code << "#include <iostream>\n\n";
        code << "int main() {\n";
        indentLevel = 1;

        for (const auto& stmt : program->statements) {
            transpileStatement(stmt);
        }

        indentLevel = 0;
        code << "    return 0;\n";
        code << "}\n";

        popScope();
        return code.str();
    }
};

int main() {
    std::string code = R"(
       
        let x = 10;
        let y = 5;
        print x + y;
        print x + y* x;
        if x > y {
            print 1;
        } else {
            print 0;
        }
        
        let i = 0;
        while i < 3 {
            print i;
            i = i + 1;
        }
    )";

    try {
        // Lexing
        Lexer lexer(code);
        std::vector<Token> tokens = lexer.tokenize();

        std::cout << "=== TOKENS ===" << std::endl;
        for (const auto& token : tokens) {
            if (token.type == TokenType::EOF_TYPE) break;
            std::cout << "Token: " << static_cast<int>(token.type) << " Value: " << token.value << std::endl;
        }

        // Parsing
        std::cout << "\n=== PARSING ===" << std::endl;
        Parser parser(tokens);
        auto ast = parser.parse();
        std::cout << "Program has " << ast->statements.size() << " statements" << std::endl;

        // Semantic Analysis
        std::cout << "\n=== SEMANTIC ANALYSIS ===" << std::endl;
        SemanticAnalyzer analyzer;
        analyzer.analyze(ast);
        std::cout << "Semantic analysis passed!" << std::endl;

        // Transpilation
        std::cout << "\n=== TRANSPILED C++ CODE ===" << std::endl;
        Transpiler transpiler;
        std::string cppCode = transpiler.transpile(ast);
        std::cout << cppCode << std::endl;

    }
    catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}