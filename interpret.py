from abc import ABC, abstractmethod
from enum import IntFlag, Enum, IntEnum
import re
import sys
import xml.sax as xml


class ILogger(ABC):
    @abstractmethod
    def log(self, message):
        pass

    @abstractmethod
    def debug(self, message):
        pass

    @abstractmethod
    def error(self, message):
        pass


class ParserLogger(ILogger):
    def __init__(self, filename):
        super().__init__()
        self._locator = None
        self._filename = filename

    def setLocator(self, locator):
        self._locator = locator

    def log(self, message):
        sys.stdout.write(
            f"[{self._filename}:{self._locator.getLineNumber()}:{self._locator.getColumnNumber()}] {message}\n")

    def debug(self, message):
        sys.stderr.write(
            f"[{self._filename}:{self._locator.getLineNumber()}:{self._locator.getColumnNumber()}] {message}\n")

    def error(self, message):
        sys.stderr.write(
            f"[{self._filename}:{self._locator.getLineNumber()}:{self._locator.getColumnNumber()}] {message}\n")


def isSingle(iterable, element):
    seen = False
    for e in iterable:
        if e == element:
            if seen:
                return False
            else:
                seen = True
    return True


class IPPError(IntEnum):
    MISSING_PARAM = 10
    FILE_OPEN = 11
    FILE_OUT_OPEN = 12
    ILL_FORMED_XML = 31
    SYNTAX_ERROR = 32
    SEMANTIC = 52
    INCOMPATIBLE_OPERANDS = 53
    VARIABLE_DOES_NOT_EXIST = 54
    FRAME_DOES_NOT_EXIST = 55
    VALUE_MISSING = 56
    INVALID_VALUE_OF_OPERAND = 57
    INVALID_STRING_OPERATION = 58
    INTERNAL = 99


class IPPInterpreterCommand(IntEnum):
    NEXT = 0
    CALL = 1
    JUMP = 2
    RETURN = 3
    DEBUG = 4


class ParsingDone(Exception):
    pass


class Element(object):
    def __init__(self, filename: str, name: str, lineNumber: int, column: int):
        super().__init__()
        self.filename = filename
        self.name: str = name
        self.lineNumber: int = lineNumber
        self.column: int = column

    def log(self, message):
        sys.stdout.write(
            f"[{self.filename}:{self.lineNumber}:{self.column}] {message}\n")

    def debug(self, message):
        sys.stderr.write(
            f"[{self.filename}:{self.lineNumber}:{self.column}] {message}\n")

    def error(self, message):
        sys.stderr.write(
            f"[{self.filename}:{self.lineNumber}:{self.column}] {message}\n")

    def errorMessage(self, message):
        return f"[{self.filename}:{self.lineNumber}:{self.column}] {message}"

    def raiseError(self, error):
        self.error(str(error))
        raise error


class InterpreterError(Exception):
    def __init__(self, relatedElement: Element, code: IPPError, message: str):
        super().__init__(relatedElement.errorMessage(
            message) if relatedElement else message)
        self.status = code
        self.element = relatedElement


class ProgramExit(Exception):
    def __init__(self, status):
        super().__init__(f"Program exited with exit code: {status}")
        self.status = status


class InterpreterState(IntFlag):
    READING = 1
    COMPILING = 2
    INTERPRETING = 4


class Elements(Enum):
    PROGRAM = 0
    INSTRUCTION = 1
    ARG = 2


class ArgumentType(Enum):
    VAR = 0
    INT = 1
    BOOL = 2
    NIL = 3
    LABEL = 4
    STRING = 5
    TYPE = 6
    FLOAT = 7


def tryParseVar(value, scope):
    return (scope, value[len(scope) + 1:]) if value.startswith(f"{scope}@") else None

#stringRegex = re.compile(r"^(\\\d{3}|[^\\\000-\040\043\057\177])*$", re.IGNORECASE | re.MULTILINE | re.UNICODE)
class ParseStringState(IntEnum):
    START = 0
    ESCAPE = 1

def parseString(string: str, element):
    state = ParseStringState.START
    code = ""
    newString = ""

    for i, c in enumerate(string):
        val = ord(c)
        if state == ParseStringState.START:
            # backslash
            if val == 0o134:
                state = ParseStringState.ESCAPE
            elif val > 0o040 and val != 0o043 and val != 0o177:
                newString += c
            else:
                raise InterpreterError(element, IPPError.SYNTAX_ERROR, f"Invalid character '{c} - code: {ord(c)}' at {i}")
        elif state == ParseStringState.ESCAPE:
            if 48 <= val <= 57:
                 code += c
            else:
                raise InterpreterError(element, IPPError.SYNTAX_ERROR, f"Invalid octal value '{c} - code: {ord(c)}' at {i}")

            if len(code) == 3:
                code = int(code, 10)
                if code > 999:
                    raise InterpreterError(element, IPPError.SYNTAX_ERROR, f"Invalid octal value {code}. Must be between 000-999")
                newString += chr(code)
                code = ""
                state = ParseStringState.START
        else:
            raise InterpreterError(element, IPPError.INTERNAL, f"Not Implemented")
    
    return newString

def parseType(tipe: str, element: Element):
    tipe = tipe.upper()
    try:
        return ArgumentType[tipe]
    except KeyError:
        raise InterpreterError(
            element, IPPError.SYNTAX_ERROR, f"Type does not exist {tipe}")


def parseToTokens(tipe: ArgumentType, value: str, element: Element):
    if tipe == ArgumentType.INT:
        try:
            if value[:1] == "00":
                raise Exception()

            value = int(value)
        except:
            raise InterpreterError(
                element, IPPError.SYNTAX_ERROR, f"Invalid value. Expecting integer, got: {value}")
    elif tipe == ArgumentType.FLOAT:
        try:
            value = float.fromhex(value)
        except:
            raise InterpreterError(
                element, IPPError.SYNTAX_ERROR, f"Invalid value. Expecting hex float, got: {value}")

    elif tipe == ArgumentType.BOOL:
        if value.lower() == "true":
            value = True
        elif value.lower() == "false":
            value = False
        else:
            raise InterpreterError(
                element, IPPError.SYNTAX_ERROR, f"Invalid boolean: {value}")

    elif tipe == ArgumentType.LABEL:
        # TODO: add check
        pass

    elif tipe == ArgumentType.NIL:
        if value == "nil":
            value = "nil"
        else:
            raise InterpreterError(
                element, IPPError.SYNTAX_ERROR, f"Invalid nil {value}")
    elif tipe == ArgumentType.VAR:
        try:
            value = next(var for var in [tryParseVar(value, "GF"), tryParseVar(
                value, "LF"), tryParseVar(value, "TF")] if var is not None)
        except StopIteration:
            raise InterpreterError(
                element, IPPError.SYNTAX_ERROR, f"Invalid variable {value}")
    elif tipe == ArgumentType.STRING:
        value = parseString(value, element)
    elif tipe == ArgumentType.TYPE:
        value = parseType(value, element)
    else:
        raise InterpreterError(
            element, IPPError.SYNTAX_ERROR, f"Unknown type {tipe}")

    return (tipe, value)


class Instruction(Element):
    instructions = dict()

    def __init__(self, filename, lineNumber, column, opcode: str, order: int):
        super().__init__(filename, Elements.INSTRUCTION.name, lineNumber, column)
        self.opcode = opcode
        self.order = order
        self.args = None
        self.function = None

    @staticmethod
    def register(fn=None, *args, **kwargs):
        if fn:
            Instruction.instructions[fn.__name__.upper()] = fn
            return fn

        def decorator(fn):
            Instruction.instructions[kwargs["name"].upper()] = fn
            return fn

        return decorator

    def __str__(self):
        args = ", ".join(f"{arg.value}:{arg.type}" for arg in self.args)
        return f"[{self.order}] {self.opcode} {args}"


class Argument(Element):
    VARS = {ArgumentType.VAR}
    LITERALS = {ArgumentType.INT, ArgumentType.BOOL,
                ArgumentType.NIL, ArgumentType.STRING,
                ArgumentType.TYPE, ArgumentType.FLOAT}
    SYMB = set((*LITERALS, ArgumentType.VAR))
    LABEL = {ArgumentType.LABEL}

    def __init__(self, filename, lineNumber, column, name: str, tipe: str):
        super().__init__(filename, name, lineNumber, column)
        self.type = tipe.upper()
        self.scope: str = None
        self.value = ""

    @staticmethod
    def fromStack(valueTuple):
        datatype, value = valueTuple
        arg = Argument(None, 0, 0, None, "")
        arg.type = datatype
        arg.value = value
        return arg

    def parse(self):
        try:
            (tipe, value) = parseToTokens(
                parseType(self.type, self), self.value, self)

            if tipe == ArgumentType.VAR:
                scope, variableName = value
                self.type = tipe
                self.scope = scope
                self.value = variableName
            else:
                self.type = tipe
                self.value = value
        except KeyError:
            raise InterpreterError(
                self, IPPError.SYNTAX_ERROR, f"Unknown type {self.type}")

    def getOrder(self):
        return int(self.name[len(Elements.ARG.name):])

    def __str__(self):
        scope = ""
        if self.scope:
            scope = f"{self.scope}@"
        return f"{scope}{self.value}:{self.type}"


class Program(Element):
    def __init__(self, filename, lineNumber, column):
        super().__init__(filename, Elements.PROGRAM.name, lineNumber, column)


class Memory(object):
    def __init__(self):
        super().__init__()
        self.gf = dict()
        self.lf = None
        self.tf = None
        self.memoryStack = list()
        self.stack = list()

    def __str__(self):
        return f"LF = {self.lf},TF = {self.tf}, GF = {self.gf}, Frame stack: {self.memoryStack}, Data stack: {self.stack}"

    def createFrame(self):
        self.tf = dict()

    def pushFrame(self, element: Element):
        if self.tf is None:
            raise InterpreterError(
                element, IPPError.FRAME_DOES_NOT_EXIST, "Cannot push frame. TF is not initialized")

        self.memoryStack.append(self.tf)
        self.lf = self.tf
        self.tf = None

    def popFrame(self, element: Element):
        try:
            self.tf = self.memoryStack.pop()
            self.lf = self.memoryStack[-1] if len(self.memoryStack) > 0 else None
        except IndexError:
            raise InterpreterError(element,
                                   IPPError.FRAME_DOES_NOT_EXIST, "Cannot pop frame. Frame stack is empty")

    def _getFrame(self, scope: str, element: Element):
        if scope == "GF":
            return self.gf
        elif scope == "LF":
            return self.lf
        elif scope == "TF":
            return self.tf
        else:
            raise InterpreterError(element,
                        IPPError.SYNTAX_ERROR, f"Unknown frame {scope}")

    def declareVariable(self, scope: str, name: str, element: Element):
        try:
            self._getFrame(scope, element)[name] = (ArgumentType.NIL, None)
        except:
            raise InterpreterError(element,
                                   IPPError.FRAME_DOES_NOT_EXIST, "Cannot get frame. Have you created one?")

    def setVariable(self, scope: str, name: str, datatype: ArgumentType, value, element: Element):
        try:
            frame = self._getFrame(scope, element)
            frame[name] = (datatype, value)
        except KeyError:
            raise InterpreterError(element, IPPError.VARIABLE_DOES_NOT_EXIST,
                                f"Variable {name} was not declared before assignment")
        except (TypeError, AttributeError):
            raise InterpreterError(element,
                                   IPPError.FRAME_DOES_NOT_EXIST, f"Frame {scope} does not exist")  
                                         
    def getVariable(self, scope: str, name: str, element: Element):
        try:
            return self._getFrame(scope, element)[name]
        except KeyError:
            raise InterpreterError(element,
                                   IPPError.VARIABLE_DOES_NOT_EXIST, f"Variable {name} was not declared")
        except (TypeError, AttributeError):
            raise InterpreterError(element,
                                   IPPError.FRAME_DOES_NOT_EXIST, f"Frame {scope} does not exist")
        

    def pushToStack(self, symbType: ArgumentType, symbValue):
        self.stack.append((symbType, symbValue))

    def popFromStack(self, element: Element):
        try:
            return self.stack.pop()
        except IndexError:
            raise InterpreterError(
                element, IPPError.VALUE_MISSING, "Stack is empty")

    def clearStack(self):
        self.stack.clear()


class IPPInterpreter(object):
    def __init__(self, instructions):
        super().__init__()
        self._instructions = instructions

        # dict structure [labelName => (optimized, index)]
        self._labels = dict()
        self._compiledCode = dict()
        self.memory = Memory()
        self.callStack = []
        self.interpreting: bool = False

    def compile(self, instruction: Instruction):
        if self.interpreting:
            raise Exception("Cannot compile code again")

        try:
            ins: Instruction = self._compiledCode[instruction.order]
            raise InterpreterError(instruction, IPPError.SYNTAX_ERROR,
                                   f"Instruction with same order ({instruction.order}) already exists on line {ins.order}")
        except KeyError:
            pass
        except InterpreterError as e:
            raise e

        try:
            instruction.function = self._instructions[instruction.opcode]

            if instruction.opcode == "LABEL":
                instruction.function(self, instruction, *instruction.args)
                self._compiledCode[instruction.order] = None
            else:
                self._compiledCode[instruction.order] = instruction
        except KeyError:
            raise InterpreterError(
                instruction,
                IPPError.SYNTAX_ERROR,
                f"Syntax error. Instruction '{instruction.opcode}' does not exist")

    def compileCleanup(self):
        self._instructions = None

    def _getLabelIndex(self, label: str, order, element: Element):
        try:
            optimized, index = self._labels[label]

            if optimized:
                return index
            else:
                # remap index to sorted op order index
                index = order.index(index)
                self._labels[label] = (True, index)
                return index
        except KeyError:
            raise InterpreterError(
                element, IPPError.SEMANTIC, f"Label {label} is not defined")
        except ValueError as e:
            raise e
            raise InterpreterError(
                element, IPPError.SEMANTIC, f"Label {label} is not defined")

    def interpretAll(self):
        self.interpreting = True
        order = sorted(self._compiledCode.keys())
        currentOrder = 0

        while currentOrder < len(order):
            instruction = self._compiledCode[order[currentOrder]]
            # ignore already defined label
            if instruction is None:
                currentOrder += 1
                continue

            try:
                metadata = instruction.function(
                    self, instruction, *instruction.args)

                if metadata is None:
                    currentOrder += 1
                    continue

                command, data = metadata

                if command == IPPInterpreterCommand.NEXT:
                    currentOrder += 1
                if command == IPPInterpreterCommand.JUMP:
                    currentOrder = self._getLabelIndex(
                        data["label"], order, instruction) + 1
                elif command == IPPInterpreterCommand.CALL:
                    self.callStack.append(currentOrder + 1)
                    currentOrder = self._getLabelIndex(
                        data["label"], order, instruction) + 1
                elif command == IPPInterpreterCommand.RETURN:
                    try:
                        currentOrder = self.callStack.pop()
                    except:
                        raise InterpreterError(
                            instruction, IPPError.VALUE_MISSING, "RETURN was called on empty call stack")
                elif command == IPPInterpreterCommand.DEBUG:
                    sys.stderr.write(f"Current position: {self.filename}:{self.lineNumber}\n")
                    sys.stderr.write(f"Current opcode: {order[currentOrder]}\n")
                    sys.stderr.write(f"Next opcode: {order[currentOrder]}\n")
                    sys.stderr.write(f"Memory: {self.memory}\n")
                    currentOrder += 1
                else:
                    raise Exception(f"Invalid command type {command}")
            except InterpreterError as e:
                raise e
            except TypeError as e:
                args = ", ".join([arg.type.name for arg in instruction.args])
                raise InterpreterError(
                    instruction, IPPError.SYNTAX_ERROR, f"Invalid number of arguments for {instruction.opcode}({args}).\n\tDetailed message: {e}")

    def interpret(self, instruction: Instruction):
        print(f"Interpreting: {instruction}")
        return self._instructions[instruction.opcode](self, *instruction.args)



def isSequence(args):
    nextExpectedNumber = 1
    for arg in args:
        order = arg.getOrder()
        if order != nextExpectedNumber:
            return False
        nextExpectedNumber += 1
    return True

class IPPParser(xml.ContentHandler):
    elements = dict()
    MAX_DEPTH = 4

    def __init__(self, interpreter: IPPInterpreter, filename: str):
        super().__init__()
        self._stack = list()
        self._depth = 0
        self.args = None
        self.interpreter: IPPInterpreter = interpreter
        self._locator: xml.xmlreader.Locator = None
        self._filename = filename
        self._logger = ParserLogger(self._filename)

        oldAppend = self._stack.append
        oldPop = self._stack.append

    def setDocumentLocator(self, locator: xml.xmlreader.Locator):
        self._locator = locator
        self._logger.setLocator(locator)

    def startElement(self, name: str, attrs):
        name = name.upper()
        self._depth += 1

        if self._depth > IPPParser.MAX_DEPTH:
            self.raiseError(
                InterpreterError(
                    Element(self._filename,
                            name,
                            self._locator.getLineNumber(),
                            self._locator.getColumnNumber()),
                    IPPError.ILL_FORMED_XML,
                    f"Max depth exceeded"
                )
            )

        try:
            elementEnum = Elements[name]
            if elementEnum == Elements.PROGRAM:
                self._stack.append(Program(
                    self._filename, self._locator.getLineNumber(), self._locator.getColumnNumber()))
                return
            elif elementEnum == Elements.INSTRUCTION:
                self.args = list()
                try:
                    order = int(attrs["order"])

                    if order <= 0:
                        raise ValueError(attrs["order"])

                    self._stack.append(Instruction(self._filename,
                                                   self._locator.getLineNumber(),
                                                   self._locator.getColumnNumber(),
                                                   attrs["opcode"].upper(),
                                                   order))
                except KeyError:
                    self.raiseError(InterpreterError(
                        Element(
                            self._filename,
                            name,
                            self._locator.getLineNumber(),
                            self._locator.getColumnNumber()),
                        IPPError.SYNTAX_ERROR,
                        "Instruction attribute is either missing opcode or order."))
                except ValueError:
                    order = attrs["order"]
                    self.raiseError(InterpreterError(
                        Element(
                            self._filename,
                            name,
                            self._locator.getLineNumber(),
                            self._locator.getColumnNumber()),
                        IPPError.SYNTAX_ERROR,
                        f"Invalid order value '{order}'"))
                return
        except KeyError:
            if name.startswith(Elements.ARG.name):
                self._stack.append(Argument(
                    self._filename,
                    self._locator.getLineNumber(),
                    self._locator.getColumnNumber(),
                    name, attrs["type"]))
                return

        self.raiseError(InterpreterError(
            Element(self._filename,
                    name,
                    self._locator.getLineNumber(),
                    self._locator.getColumnNumber()),
            IPPError.SYNTAX_ERROR,
            f"Unknown element {name}"))

    def endElement(self, name):
        name = name.upper()
        self._depth -= 1
        parentElement: Element = self._stack[-1]

        try:
            elementEnum = Elements[name]
            if elementEnum == Elements.PROGRAM:
                if parentElement.name != name:
                    self.raiseError(IPPInterpreter(
                        Element(self._filename,
                                name,
                                self._locator.getLineNumber(),
                                self._locator.getColumnNumber()),
                        IPPError.ILL_FORMED_XML,
                        f"Invalid program tag position"))
                elif len(self._stack) > 1:
                    self.raiseError(IPPInterpreter(
                        Element(self._filename,
                                name,
                                self._locator.getLineNumber(),
                                self._locator.getColumnNumber()),
                        IPPError.ILL_FORMED_XML,
                        f"Invalid xml ipp structure"))
                else:
                    def stoppedParsing(self, name: str, attrs):
                        raise InterpreterError(
                            None, IPPError.ILL_FORMED_XML, f"Parser stopped parsing but new element with name {name} appeared")
                    self.startElement = stoppedParsing
                    return
            elif elementEnum == Elements.INSTRUCTION:
                if parentElement.name != name:
                    self.raiseError(IPPInterpreter(
                        Element(self._filename,
                                name,
                                self._locator.getLineNumber(),
                                self._locator.getColumnNumber()),
                        IPPError.ILL_FORMED_XML,
                        f"Invalid instruction tag position"))
                else:
                    instruction: Instruction = self._stack.pop()
                    self.validateArgs(self.args)
                    instruction.args = sorted(self.args, key=lambda x: x.getOrder())
                    if not isSequence(instruction.args):
                        self.raiseError(InterpreterError(
                            Element(self._filename,
                                    name,
                                    self._locator.getLineNumber(),
                                    self._locator.getColumnNumber()),
                            IPPError.SYNTAX_ERROR,
                            f"Args must be sequence starting from 1 to n"))

                    self.interpreter.compile(instruction)
                    return
        except KeyError as e:
            if name.startswith(Elements.ARG.name):
                if parentElement.name != name:
                    self.raiseError(InterpreterError(
                        Element(self._filename,
                                name,
                                self._locator.getLineNumber(),
                                self._locator.getColumnNumber()),
                        IPPError.ILL_FORMED_XML,
                        f"Invalid arg tag position"))
                else:
                    argument: Argument = self._stack.pop()
                    argument.parse()
                    self.args.append(argument)
                    return
            else:
                self.raiseError(InterpreterError(
                    Element(self._filename,
                            name,
                            self._locator.getLineNumber(),
                            self._locator.getColumnNumber()),
                    IPPError.ILL_FORMED_XML,
                    f"Unknown element {name}"))

        self.raiseError(InterpreterError(
            Element(self._filename,
                    name,
                    self._locator.getLineNumber(),
                    self._locator.getColumnNumber()),
            IPPError.ILL_FORMED_XML,
            f"Unknown element {name}"))

    def raiseError(self, error):
        raise error

    def characters(self, content: str):
        parentElement: Element = self._stack[-1]
        if parentElement.name.startswith(Elements.ARG.name):
            parentElement.value += content
        elif content.strip():
            raise Exception(
                f"Invalid chars in {parentElement} tag. Characters: {content}")

    def validateArgs(self, args):
        orders = map(lambda x: x.getOrder(), args)

        if any(order <= 0 or not isSingle(orders, order) for order in orders):
            raise IPPInterpreter(None,
                                 IPPError.ILL_FORMED_XML,
                                 f"Invalid arg element on one of: {args}")


def expect(argument: Argument, supportedArguments):
    if argument.type not in supportedArguments:
        raise InterpreterError(argument,
                               IPPError.INCOMPATIBLE_OPERANDS, "Unsupported argument type")


def expectType(argument: Argument, argumentTypes: ArgumentType):
    if argument.type not in argumentTypes:
        raise InterpreterError(argument,
                               IPPError.INCOMPATIBLE_OPERANDS, "Unsupported argument data type")


def getSymbol(interpreter: IPPInterpreter, symb: Argument, expectedTypes):
    if symb.type != ArgumentType.VAR and symb.type in expectedTypes:
        return (symb.type, symb.value)
    elif symb.type == ArgumentType.VAR:
        (symbType, symbValue) = interpreter.memory.getVariable(
            symb.scope,
            symb.value,
            symb
        )
        if symbType in expectedTypes:
            return (symbType, symbValue)
        else:
            types = ", ".join(t.name for t in expectedTypes)
            raise InterpreterError(symb, IPPError.INCOMPATIBLE_OPERANDS,
                                   f"Variable value ({symb}:{symbType}) is not compatible with following types: {types}")
    else:
        types = ", ".join(t.name for t in expectedTypes)
        raise InterpreterError(symb, IPPError.INCOMPATIBLE_OPERANDS,
                               f"Variable ({symb}) is not compatible with following types: {types}")


@Instruction.register
def move(interpreter: IPPInterpreter, instruction: Instruction, destination: Argument, source: Argument):
    expect(destination, Argument.VARS)
    expect(source, Argument.SYMB)

    datatype = None
    value = None

    if source.type in Argument.LITERALS:
        datatype = source.type
        value = source.value
    else:
        datatype, value = interpreter.memory.getVariable(
            source.scope, source.value, source)

    interpreter.memory.setVariable(
        destination.scope,
        destination.value,
        datatype,
        value,
        instruction
    )


@Instruction.register
def label(interpreter: IPPInterpreter, instruction: Instruction, var: Argument):
    expect(var, Argument.LABEL)
    if var.value in interpreter._labels:
        raise InterpreterError(instruction, IPPError.SEMANTIC, f"Duplicate label. First seen at {interpreter._labels[var.value][1]}")

    interpreter._labels[var.value] = (False, instruction.order)


@Instruction.register
def jump(interpreter: IPPInterpreter, instruction: Instruction, var: Argument):
    expect(var, Argument.LABEL)
    return (IPPInterpreterCommand.JUMP, {"label": var.value})


def conditionalJump(interpreter: IPPInterpreter, instruction: Instruction, label: Argument, symb1: Argument, symb2: Argument, predicate):
    expect(label, Argument.LABEL)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    (symb1Type, symb1Value) = getSymbol(interpreter, symb1, Argument.SYMB)
    (symb2Type, symb2Value) = getSymbol(interpreter, symb2, Argument.SYMB)

    result = predicate((symb1Type, symb1Value), (symb2Type, symb2Value))

    if result:
        return (IPPInterpreterCommand.JUMP, {"label": label.value})
    else:
        return None


@Instruction.register
def clears(interpreter: IPPInterpreter, instruction: Instruction):
    interpreter.memory.clearStack()


@Instruction.register
def jumpifeq(interpreter: IPPInterpreter, instruction: Instruction, label: Argument, symb1: Argument, symb2: Argument):
    def helper(a, b):
        atype, avalue = a
        btype, bvalue = b

        if atype == btype and avalue == bvalue:
            return True
        elif atype == btype or atype == ArgumentType.NIL or btype == ArgumentType.NIL:
            return False
        else:
            raise InterpreterError(
                instruction, IPPError.INCOMPATIBLE_OPERANDS, f"Operands cannot be compared - {atype} x {btype}")

    return conditionalJump(interpreter, instruction, label, symb1, symb2, helper)


@Instruction.register
def jumpifeqs(interpreter: IPPInterpreter, instruction: Instruction, label: Argument):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    return jumpifeq(interpreter, instruction, label, symb1, symb2)


@Instruction.register
def jumpifneq(interpreter: IPPInterpreter, instruction: Instruction, label: Argument, symb1: Argument, symb2: Argument):
    def helper(a, b):
        atype, avalue = a
        btype, bvalue = b

        if (atype == btype and avalue != bvalue) or atype == ArgumentType.NIL or btype == ArgumentType.NIL:
            return True
        elif atype == btype and avalue == bvalue:
            return False
        else:
            raise InterpreterError(
                instruction, IPPError.INCOMPATIBLE_OPERANDS, f"Operands cannot be compared - {atype} x {btype}")

    return conditionalJump(interpreter, instruction, label, symb1, symb2, helper)

@Instruction.register
def jumpifneqs(interpreter: IPPInterpreter, instruction: Instruction, label: Argument):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    return jumpifneq(interpreter, instruction, label, symb1, symb2)


@Instruction.register
def defvar(interpreter: IPPInterpreter, instruction: Instruction, var: Argument):
    expect(var, Argument.VARS)
    interpreter.memory.declareVariable(var.scope, var.value, instruction)


@Instruction.register
def createframe(interpreter: IPPInterpreter, instruction: Instruction):
    interpreter.memory.createFrame()


@Instruction.register
def pushframe(interpreter: IPPInterpreter, instruction: Instruction):
    interpreter.memory.pushFrame(instruction)


@Instruction.register
def popframe(interpreter: IPPInterpreter, instruction: Instruction):
    interpreter.memory.popFrame(instruction)


@Instruction.register
def call(interpreter, instruction: Instruction, label: Argument):
    expect(label, Argument.LABEL)
    return (IPPInterpreterCommand.CALL, {"label": label.value})


@Instruction.register
def exit(interpreter, instruction: Instruction, status: Argument):
    expectType(status, {ArgumentType.INT})
    if 0 <= status.value <= 49:
        raise ProgramExit(status.value)
    else:
        raise InterpreterError(instruction, IPPError.INVALID_VALUE_OF_OPERAND, f"Exit code out of range. Must be between 0 and 49. Got {status.value}")

@Instruction.register(name="return")
def i_return(interpreter, instruction: Instruction):
    return (IPPInterpreterCommand.RETURN, None)


@Instruction.register
def pushs(interpreter: IPPInterpreter, instruction: Instruction, symb: Argument):
    expectType(symb, Argument.SYMB)
    interpreter.memory.pushToStack(
        *getSymbol(interpreter, symb, Argument.SYMB))

@Instruction.register
def pops(interpreter: IPPInterpreter, instruction: Instruction, var: Argument):
    expectType(var, Argument.VARS)
    (tipe, value) = interpreter.memory.popFromStack(var)
    interpreter.memory.setVariable(
        var.scope,
        var.value,
        tipe,
        value,
        instruction
    )

# args: (allowed types, arg object)


def binaryOperator(interpreter: IPPInterpreter, instruction: Instruction, memoryArg: Argument, args, operator):
    def convert(arg):
        types, arg = arg
        return getSymbol(interpreter, arg, types)

    values = map(convert, args)

    datatype, value = operator(*values)

    if memoryArg:
        interpreter.memory.setVariable(
            memoryArg.scope,
            memoryArg.value,
            datatype,
            value,
            memoryArg
        )
    else:
        interpreter.memory.pushToStack(datatype, value)


@Instruction.register
def add(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var is not None:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        result = a[1] + b[1]
        datatype = ArgumentType.INT if isinstance(
            result, int) else ArgumentType.FLOAT

        return (datatype, result)

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb1),
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb2)
                   ], helper)


@Instruction.register
def adds(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    add(interpreter, instruction, None, symb1, symb2)


@Instruction.register
def sub(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var is not None:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        result = a[1] - b[1]
        datatype = ArgumentType.INT if isinstance(
            result, int) else ArgumentType.FLOAT

        return (datatype, result)

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb1),
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb2)
                   ], helper)


@Instruction.register
def subs(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    sub(interpreter, instruction, None, symb1, symb2)


@Instruction.register
def mul(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var is not None:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        result = a[1] * b[1]
        datatype = ArgumentType.INT if isinstance(
            result, int) else ArgumentType.FLOAT

        return (datatype, result)

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb1),
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb2)
                   ], helper)


@Instruction.register
def muls(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    mul(interpreter, instruction, None, symb1, symb2)


@Instruction.register
def idiv(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var is not None:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        if b[1] != 0:
            return (ArgumentType.INT, a[1] // b[1])
        else:
            raise InterpreterError(
                symb2, IPPError.INVALID_VALUE_OF_OPERAND, "Division by 0")

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.INT}, symb1),
                       ({ArgumentType.INT}, symb2)
                   ], helper)


@Instruction.register
def idivs(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    idiv(interpreter, instruction, None, symb1, symb2)


@Instruction.register
def div(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var is not None:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        if b[1] != 0:
            return (ArgumentType.FLOAT, a[1] / b[1])
        else:
            raise InterpreterError(
                symb2, IPPError.INVALID_VALUE_OF_OPERAND, "Division by 0")

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb1),
                       ({ArgumentType.INT, ArgumentType.FLOAT}, symb2)
                   ], helper)


@Instruction.register
def divs(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    div(interpreter, instruction, None, symb1, symb2)

def relationOperator(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument, operator, supportsNil = False):
    if var != None:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        atype, avalue = a
        btype, bvalue = b
        if atype == btype:
            return (ArgumentType.BOOL, operator(avalue, bvalue))
        elif (atype == ArgumentType.NIL or btype == ArgumentType.NIL) and supportsNil:
            return (ArgumentType.BOOL, False)
        else:
            raise InterpreterError(
                instruction, IPPError.INCOMPATIBLE_OPERANDS, "Argument are not same of type")

    binaryOperator(interpreter, instruction, var,
                   [
                       (Argument.LITERALS, symb1),
                       (Argument.LITERALS, symb2)
                   ], helper)


@Instruction.register
def lt(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    relationOperator(interpreter, instruction, var,
                     symb1, symb2, lambda a, b: a < b)


@Instruction.register
def lts(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    lt(interpreter, instruction, None, symb1, symb2)


@Instruction.register
def gt(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    relationOperator(interpreter, instruction, var,
                     symb1, symb2, lambda a, b: a > b)


@Instruction.register
def gts(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    gt(interpreter, instruction, None, symb1, symb2)


@Instruction.register
def eq(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    relationOperator(interpreter, instruction, var,
                     symb1, symb2, lambda a, b: a == b, True)


@Instruction.register
def eqs(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    eq(interpreter, instruction, None, symb1, symb2)


@Instruction.register(name="and")
def i_and(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.BOOL}, symb1),
                       ({ArgumentType.BOOL}, symb2)
                   ], lambda a, b: (ArgumentType.BOOL, a[1] and b[1]))


@Instruction.register
def ands(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    i_and(interpreter, instruction, None, symb1, symb2)


@Instruction.register(name="or")
def i_or(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.BOOL}, symb1),
                       ({ArgumentType.BOOL}, symb2)
                   ], lambda a, b: (ArgumentType.BOOL, a[1] or b[1]))


@Instruction.register
def ors(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    i_or(interpreter, instruction, None, symb1, symb2)


@Instruction.register(name="not")
def i_not(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument):
    if var:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.BOOL}, symb1),
                   ], lambda a: (ArgumentType.BOOL, not a[1]))


@Instruction.register
def nots(interpreter: IPPInterpreter, instruction: Instruction):
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    i_not(interpreter, instruction, None, symb1)


@Instruction.register
def int2char(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument):
    if var:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)

    def helper(ch):
        try:
            return (ArgumentType.STRING, chr(ch[1]))
        except ValueError:
            raise InterpreterError(
                symb1, IPPError.INVALID_STRING_OPERATION, "Integer value out of unicode range")

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.INT}, symb1),
                   ], helper)


@Instruction.register
def int2chars(interpreter: IPPInterpreter, instruction: Instruction):
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    int2char(interpreter, instruction, None, symb1)


@Instruction.register
def stri2int(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    if var:
        expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(string, position):
        _, string = string
        _, position = position
        try:
            return (ArgumentType.INT, ord(string[position]))
        except IndexError:
            raise InterpreterError(symb1, IPPError.INVALID_STRING_OPERATION,
                                   f"Index is out of string. Index: {position}, String length: {len(string)}")

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.STRING}, symb1),
                       ({ArgumentType.INT}, symb2)
                   ], helper)


@Instruction.register
def stri2ints(interpreter: IPPInterpreter, instruction: Instruction):
    symb2 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    symb1 = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    stri2int(interpreter, instruction, None, symb1, symb2)


def convert2type(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb: Argument, destinationType, expectedTypes, converter):
    if var is not None:
        expect(var, Argument.VARS)
    expect(symb, Argument.SYMB)

    (symbType, symbValue) = getSymbol(interpreter, symb, expectedTypes)

    if var:
        interpreter.memory.setVariable(
            var.scope,
            var.value,
            destinationType,
            converter(symbValue),
            var
        )
    else:
        interpreter.memory.pushToStack(destinationType, converter(symbValue))


@Instruction.register
def int2float(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb: Argument):
    convert2type(interpreter, instruction, var, symb,
                 ArgumentType.FLOAT, {ArgumentType.INT}, int)


@Instruction.register
def int2floats(interpreter: IPPInterpreter, instruction: Instruction):
    symb = Argument.fromStack(interpreter.memory.popFromStack(instruction))
    convert2type(interpreter, instruction, None, symb,
                 ArgumentType.FLOAT, {ArgumentType.INT}, int)


@Instruction.register
def float2int(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb: Argument):
    convert2type(interpreter, instruction, var, symb,
                 ArgumentType.INT, {ArgumentType.FLOAT}, float)


@Instruction.register(name="read")
def i_read(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, arg: Argument):
    expect(var, Argument.VARS)
    expect(arg, {ArgumentType.TYPE})

    try:
        content = input()
        inputType, inputValue = None, None

        if arg.value == ArgumentType.STRING:
            inputType = ArgumentType.STRING
            inputValue = content
        else:
            inputType, inputValue = parseToTokens(arg.value, content, instruction)

        if inputType not in {ArgumentType.STRING, ArgumentType.BOOL, ArgumentType.INT, ArgumentType.FLOAT}:
            raise InterpreterError(arg, IPPError.INVALID_VALUE_OF_OPERAND,
                                   "Read only support types {string, bool, int, float}")

        interpreter.memory.setVariable(
            var.scope, var.value, inputType, inputValue, instruction)
    except EOFError:
        interpreter.memory.setVariable(
            var.scope, var.value, ArgumentType.NIL, "nil", var)


@Instruction.register
def write(interpreter: IPPInterpreter, instruction: Instruction, symb: Argument):
    expect(symb, Argument.SYMB)

    value = symb.value
    tipe = symb.type
    text = None

    if symb.type == ArgumentType.VAR:
        (tipe, value) = interpreter.memory.getVariable(
            symb.scope,
            symb.value,
            symb
        )

    if tipe == ArgumentType.BOOL:
        text = "true" if value else "false"
    elif tipe == ArgumentType.NIL:
        text = ""
    elif tipe == ArgumentType.TYPE:
        text = value.name.lower() if value != "" else value
    elif tipe == ArgumentType.FLOAT:
        text = float.hex(value)
    else:
        text = value

    print(text, end="")


@Instruction.register(name="concat")
def i_concat(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(a, b):
        return (ArgumentType.STRING, a[1] + b[1])

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.STRING}, symb1),
                       ({ArgumentType.STRING}, symb2)
                   ], helper)

@Instruction.register
def strlen(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb: Argument):
    expect(var, Argument.VARS)
    expect(symb, Argument.SYMB)

    symbType, symbValue = getSymbol(interpreter, symb, {ArgumentType.STRING})

    interpreter.memory.setVariable(
        var.scope,
        var.value,
        ArgumentType.INT,
        len(symbValue),
        var
    )


@Instruction.register
def getchar(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    def helper(string, index):
        try:
            return (ArgumentType.STRING, string[1][index[1]])
        except IndexError:
            raise InterpreterError(instruction, IPPError.INVALID_STRING_OPERATION,
                                   f"Index is out of string range. String: {string[1]}, Index: {index[1]}")

    binaryOperator(interpreter, instruction, var,
                   [
                       ({ArgumentType.STRING}, symb1),
                       ({ArgumentType.INT}, symb2)
                   ], helper)


@Instruction.register
def setchar(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb1: Argument, symb2: Argument):
    expect(var, Argument.VARS)
    expect(symb1, Argument.SYMB)
    expect(symb2, Argument.SYMB)

    (varType, varValue) = interpreter.memory.getVariable(
        var.scope,
        var.value,
        var
    )

    index = None
    char = None

    if varType is not ArgumentType.STRING:
        raise InterpreterError(
            var, IPPError.INVALID_VALUE_OF_OPERAND, "Var must be string variable")

    (indexType, index) = getSymbol(interpreter, symb1, {ArgumentType.INT})
    (stringType, char) = getSymbol(interpreter, symb2, {ArgumentType.STRING})

    try:
        varValue[index] = char
        interpreter.memory.setVariable(
            var.scope,
            var.value,
            ArgumentType.STRING,
            varValue,
            instruction
        )
    except IndexError:
        raise InterpreterError(
            symb2, IPPError.INVALID_STRING_OPERATION, "Index out of string range")


@Instruction.register(name="type")
def i_type(interpreter: IPPInterpreter, instruction: Instruction, var: Argument, symb: Argument):
    expect(var, Argument.VARS)
    expect(symb, Argument.SYMB)

    symbType = None

    try:
        (symbType, _) = getSymbol(interpreter, symb, Argument.SYMB)
    except InterpreterError as e:
        if e.status == IPPError.VARIABLE_DOES_NOT_EXIST:
            interpreter.memory.setVariable(
                var.scope,
                var.value,
                ArgumentType.STRING,
                "",
                var
            )
            return
        else:
            raise e

    interpreter.memory.setVariable(
        var.scope,
        var.value,
        ArgumentType.STRING,
        symbType.name.lower(),
        var
    )

@Instruction.register
def dprint(interpreter: IPPInterpreter, instruction: Instruction, symb: Argument):
    (_, value) = getSymbol(interpreter, symb, Argument.SYMB)
    sys.stderr.write(f"{value}\n")

@Instruction.register(name="break")
def i_break(interpreter: IPPInterpreter, instruction: Instruction):
    return (IPPInterpreterCommand.DEBUG, {})

class Args(object):
    def __init__(self, args):
        super().__init__()
        self._args = self._parseArgs(args)
        self._usedArgs = set()

    def _parseArg(self, arg: str):
        arg = arg.strip()
        index = arg.find("=")

        if not arg or not arg.startswith("--"):
            raise InterpreterError(
                None, IPPError.MISSING_PARAM, f"Invalid argument {arg}")

        name, value = (arg[2:index], arg[index+1:]) if index > 0 else (arg[2:], True)

        if isinstance(value, str) and  value.startswith("\"") and value.endswith("\""):
            value = value[1:len(value) - 1]

        return (name, value)

    def _parseArgs(self, args):
        argsDict = dict()

        for arg in args:
            name, value = self._parseArg(arg)

            argList = argsDict.setdefault(name, list())
            argList.append(value)
            argsDict[name] = argList

        return argsDict

    def getSingle(self, key):
        try:
            self._usedArgs.add(key)
            args = self._args[key]
            if len(args) > 1:
                raise InterpreterError(None, IPPError.MISSING_PARAM, f"Argument '{key}' must be unique")
            return args[0]
        except KeyError:
            raise InterpreterError(None, IPPError.MISSING_PARAM, f"Arguments '{key}' were not provided")

    def getAll(self, key):
        try:
            self._usedArgs.add(key)
            return self._args[key]
        except KeyError:
            raise InterpreterError(None, IPPError.MISSING_PARAM, f"Arguments '{key}' were not provided")

    def usedAllArguments(self):
        return len(self._usedArgs) == len(self._args)

    def getUnusedArguments(self):
        return [x for x in self._args.keys() if x not in self._usedArgs]

    def __contains__(self, key):
        return self._args.get(key, False) != False

    def __len__(self):
        return len(self._args)

    def __str__(self):
        return str(self._args)

def main():
    try:
        source = sys.stdin
        stdin = sys.stdin
        args = Args(sys.argv[1:])
        sourceFilename = "-"

        if "help" in args:
            if len(args) != 1 and len(args["help"]) == 1:
                raise InterpreterError(
                    None, IPPError.MISSING_PARAM, f"Invalid combination with help argument. Help can be used without specifying other arguments.")

            print("interpret.py ([--help]|[--input] [--source])")
            print("\t--help - prints following message")
            print("\t--source=file - reads program from file")
            print("\t--input=file - passes input from file to interpreted program")
            sys.exit(0)

        hasSourceOrInput = False

        if "source" in args:
            source = args.getSingle("source")
            sourceFilename = source

            if source == True:
                raise InterpreterError(
                    None, IPPError.MISSING_PARAM, f"--source is missing file value")

            source = open(source, "r")
            hasSourceOrInput = True

        if "input" in args:
            stdin = args.getSingle("input")

            if stdin == True:
                raise InterpreterError(
                    None, IPPError.MISSING_PARAM, f"--input is missing file value")

            stdin = open(stdin, "r")
            sys.stdin = stdin
            hasSourceOrInput = True

        if not args.usedAllArguments():
            raise InterpreterError(
                None, IPPError.MISSING_PARAM, f"Unknown argument in {args.getUnusedArguments()}")

        if not hasSourceOrInput:
            raise InterpreterError(
                None, IPPError.MISSING_PARAM, f"--source or --input argument must be specified")

        interpreter = IPPInterpreter(Instruction.instructions)
        parser = xml.make_parser()
        parser.setContentHandler(IPPParser(interpreter, sourceFilename))
        parser.parse(source)
        interpreter.compileCleanup()
        interpreter.interpretAll()

    except ProgramExit as e:
        sys.exit(e.status)
    except InterpreterError as e:
        sys.stderr.write(str(e) + "\n")
        sys.stderr.write(f"Error status: {e.status}\n")
        sys.exit(e.status)
    except FileNotFoundError as e:
        sys.stderr.write(str(e) + "\n")
        sys.stderr.write(f"Error status: {IPPError.FILE_OPEN}\n")
        sys.exit(IPPError.FILE_OPEN)
    except PermissionError as e:
        sys.stderr.write(str(e) + "\n")
        sys.stderr.write(f"Error status: {IPPError.FILE_OPEN}\n")
        sys.exit(IPPError.FILE_OPEN)
    except xml.SAXException as e:
        sys.stderr.write(str(e) + "\n")
        sys.stderr.write(f"Error status: {IPPError.FILE_OPEN}\n")
        sys.exit(IPPError.ILL_FORMED_XML)
    finally:
        if source and source != sys.stdin and not isinstance(source, str):
            source.close()
        if stdin and stdin != sys.stdin and not isinstance(stdin, str):
            stdin.close()

    sys.exit(0)


if __name__ == "__main__":
    main()
