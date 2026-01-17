#include <iostream>
#include <string>
#include <cctype>
#include <vector>

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

    // Skip whitespace and return the next non-whitespace character
    void skipWhitespace() {
        while (cursor < input.size() && std::isspace(input[cursor])) {
            cursor++;
        }
    }

    // Read a number and return its string representation
    std::string readNumber() {
        std::string num;
        while (cursor < input.size() && std::isdigit(input[cursor])) {
            num += input[cursor];
            cursor++;
        }
        return num;
    }

	// Read an identifier or keyword and return its string
    std::string readIdentifier() {
        std::string ident;
        while (cursor < input.size() && (std::isalnum(input[cursor]) || input[cursor] == '_')) {
            ident += input[cursor];
            cursor++;
        }
        return ident;
    }

    // Check if a string is a keyword, return the appropriate TokenType
    TokenType getKeywordType(const std::string& word) {
        if (word == "let") return TokenType::LET;
        if (word == "print") return TokenType::PRINT;
        return TokenType::IDENTIFIER;
    }

public:
    Lexer(const std::string& source) : input(source), cursor(0) {}

    // Get the next token from the input
    Token getNextToken() {
        skipWhitespace();

        // End of file
        if (cursor >= input.size()) {
            return Token{TokenType::EOF_TYPE, ""};
        }

        char current = input[cursor];

        // Numbers
        if (std::isdigit(current)) {
            std::string numStr = readNumber();
            return Token{TokenType::INTEGER, numStr};
        }

        // Identifiers and Keywords
        if (std::isalpha(current) || current == '_') {
            std::string ident = readIdentifier();
            TokenType type = getKeywordType(ident);
            return Token{type, ident};
        }

        // Single character tokens
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

    // Tokenize the entire input and return all tokens
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

int main() {
    // Test the Lexer
    std::string code = "let x = 10 + 5;";
    Lexer lexer(code);
    std::vector<Token> tokens = lexer.tokenize();

    // Print tokens
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

    return 0;
}