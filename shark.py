TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'

DIGITS = '0123456789'


class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def __str__(self):
        result = f'{self.error_name}: {self.details}' + '\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        return result


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)


class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, curr_char):
        self.idx += 1
        self.col += 1

        if curr_char == '\n':
            self.ln += 1
            self.col = 0

        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)


class Token:
    def __init__(self, type_, value=None):
        self.type = type_
        self.value = value

    def __repr__(self):
        if self.value:
            return f'{self.type}: {self.value}'
        return self.type


class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.curr_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.curr_char)
        self.curr_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def make_token(self):
        tokens = []

        while self.curr_char != None:
            if self.curr_char in [' ', '\t']:
                self.advance()
            elif self.curr_char in DIGITS:
                tokens.append(self.make_number())
                self.advance()
            elif self.curr_char == '+':
                tokens.append(Token(TT_PLUS))
                self.advance()
            elif self.curr_char == '-':
                tokens.append(Token(TT_MINUS))
                self.advance()
            elif self.curr_char == '*':
                tokens.append(Token(TT_MUL))
                self.advance()
            elif self.curr_char == '/':
                tokens.append(Token(TT_DIV))
                self.advance()
            elif self.curr_char == '(':
                tokens.append(Token(TT_LPAREN))
                self.advance()
            elif self.curr_char == ')':
                tokens.append(Token(TT_RPAREN))
                self.advance()
            else:
                pos_start = self.pos.copy()
                char = self.curr_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        return tokens, None

    def make_number(self):
        num_str = ''
        dot_count = 0

        while self.curr_char != None and self.curr_char in DIGITS + '.':
            if self.curr_char == '.':
                if dot_count == 1: break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.curr_char
            self.advance()

        return Token(TT_INT, int(num_str)) if dot_count == 0 else Token(TT_FLOAT, float(num_str))


def run(fn, text):
    lexer = Lexer(fn, text)
    tokens, errors = lexer.make_token()

    return tokens, errors
