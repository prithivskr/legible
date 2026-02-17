from dataclasses import dataclass
from typing import Union

@dataclass
class TNum:
    value: float

@dataclass
class TPlus: pass

@dataclass
class TMinus: pass

@dataclass
class TStar: pass

@dataclass
class TSlash: pass

@dataclass
class TEof: pass

Token = Union[TNum, TPlus, TMinus, TStar, TSlash, TEof]


# --- Lexer ---
def lex(source: str) -> list[Token]:
    tokens: list[Token] = []
    i = 0
    while i < len(source):
        c = source[i]
        if c in ' \t\n':
            i += 1
        elif c == '+':
            tokens.append(TPlus()); i += 1
        elif c == '-':
            tokens.append(TMinus()); i += 1
        elif c == '*':
            tokens.append(TStar()); i += 1
        elif c == '/':
            tokens.append(TSlash()); i += 1
        elif c.isdigit() or c == '.':
            start = i
            while i < len(source) and (source[i].isdigit() or source[i] == '.'):
                i += 1
            tokens.append(TNum(float(source[start:i])))
        else:
            raise ValueError(f"unexpected character: {c!r}")
    tokens.append(TEof())
    return tokens


# --- Evaluator ---
def eval_rpn(tokens: list[Token]) -> float:
    stack: list[float] = []

    def pop() -> float:
        if not stack:
            raise ValueError("stack underflow")
        return stack.pop()

    
    for tok in tokens:
        if isinstance(tok, TNum):
            stack.append(tok.value)

        elif isinstance(tok, TPlus):
            b, a = pop(), pop()
            stack.append(a + b)

        elif isinstance(tok, TMinus):
            b, a = pop(), pop()
            stack.append(a - b)

        elif isinstance(tok, TStar):
            b, a = pop(), pop()
            stack.append(a * b)

        elif isinstance(tok, TSlash):
            b, a = pop(), pop()
            if b == 0.0:
                raise ValueError("division by zero")
            stack.append(a / b)

        elif isinstance(tok, TEof):
            break


    if len(stack) == 0:
        raise ValueError("empty expression")
    if len(stack) > 1:
        raise ValueError("too many values on stack")
    return stack[0]


# --- Tests ---
def run_tests() -> None:
    def check(expr: str, expected: float) -> None:
        result = eval_rpn(lex(expr))
        assert abs(result - expected) < 1e-9, \
            f"FAIL: {expr!r} = {result}, expected {expected}"

    check("3 4 +",           7.0)
    check("10 2 /",          5.0)
    check("2 3 4 * +",       14.0)
    check("5 1 2 + 4 * + 3 -", 14.0)
    check("0.5 2 *",          1.0)
    print("all tests passed")


# --- Entry point ---
def main() -> None:
    import sys
    line = input("rpn> ")
    try:
        result = eval_rpn(lex(line))
        print(result)
    except ValueError as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    import sys
    if "--test" in sys.argv:
        run_tests()
    else:
        main()
