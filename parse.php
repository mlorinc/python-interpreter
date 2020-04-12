<?php

/**
 * Transform arguments from longopts format to command line format
 */
function transformArguments($supportedArguments) {
    $args = [];
    foreach ($supportedArguments as $index => $arg) {
        if (substr($arg, 0, 2) !== "--") {
            $arg = "--$arg";
        }
        if (substr($arg, strlen($arg) - 2) === "::") {
            $arg = substr($arg, 0, strlen($arg) - 2);
        }
        if (substr($arg, strlen($arg) - 2) === ":") {
            $arg = substr($arg, 0, strlen($arg) - 2);
        }
        $args[$index] = $arg;
    }
    return $args;
}

/**
 * Verify if all arguments are supported by script
 */
function verifyArguments($args, $supportedArguments) {
    $args = array_slice($args, 1);

    foreach ($args as $arg) {
        $eqIndex = strpos($arg, "=");

        if ($eqIndex !== false) {
            $arg = substr($arg, 0, $eqIndex);
        }

        if (array_search($arg, $supportedArguments) === false) {
            throw new Exception("Unsupported argument '$arg'");
        }
    }
}


abstract class Errors
{
    const MISSING_PARAM = 10;
    const FILE_OPEN = 11;
    const FILE_OUT_OPEN = 12;
    const MISSING_HEADER = 21;
    const INVALID_OPCODE = 22;
    const LEX_SYNTAX = 23;
    const INTERNAL = 99;
}

/**
 * Stats counter class which provides utility functions
 */
class Stats {
    /**
     * Stats stored in dictionary
     */
    private $data;

    public function __construct($supportedStats) {
        $this->data = [];
        foreach ($supportedStats as $supportedStat) {
            if ($supportedStat !== "labels") {
                $this->data[$supportedStat] = 0;
            }
            else {
                $this->data[$supportedStat] = [];
            }
        }
    }

    /**
     * Increment line of codes stat
     */
    public function incrementLoc()
    {
        $this->data["loc"]++;
    }

    /**
     * Increment comments stat
     */
    public function incrementComments()
    {
        $this->data["comments"]++;
    }

    /**
     * Add label to stats for later use
     */
    public function addLabel($name)
    {
        if (array_search($name, $this->data["labels"]) === false) {
            array_push($this->data["labels"], $name);
        }
    }

    /**
     * Increment jumps stat
     */
    public function incrementJumps()
    {
        $this->data["jumps"]++;
    }

    /**
     * Get line of codes stat as integer
     */
    public function getLoc()
    {
        return $this->data["loc"];
    }

    /**
     * Get comments stat as integer
     */
    public function getComments()
    {
        return $this->data["comments"];
    }

    /**
     * Get unique labels count as integer
     */
    public function getLabelsCount()
    {
        return count($this->data["labels"]);
    }

    /**
     * Get jumps count
     */
    public function getJumps()
    {
        return $this->data["jumps"];
    }

    /**
     * Get stat count by name
     */
    public function get($name) {
        if ($name === "labels") {
            return $this->getLabelsCount();
        }
        else {
            return $this->data[$name];
        }
    }
}

/**
 * Represents argument in instruction 
 */
abstract class Argument
{

    /**
     * Print xml node of argument to stdout
     */
    public abstract function renderNode($index);

    /**
     * Validate argument. Exit if it is invalid.
     */
    public abstract function validate();

    /**
     * Get string representation
     */
    public abstract function toString();

    /**
     * Create argument from string
     * @arg rawArgument - argument in string form
     * @arg expectedTypes - list of expected argument types
     */
    public static function from($rawArgument, $expectedTypes)
    {
        $rawArgument = trim($rawArgument);
        $index = strpos($rawArgument, "@");

        # its most likely label or type
        if ($index === false) {
            $expectedType = $expectedTypes[0];
            if ($expectedType == Label::class) {
                return new Label($rawArgument);
            }
            else if ($expectedType == Type::class) {
                return new Type($rawArgument);
            }
            else {
                fprintf(STDERR, "Unknown type $rawArgument, expecting $expectedType\n");
                exit(Errors::LEX_SYNTAX);
            }
        }

        $type = substr($rawArgument, 0, $index);
        $value = substr($rawArgument, $index + 1);

        switch ($type) {
            case "GF":
                return new GlobalVariable($value);
            case "TF":
                return new TemporaryVariable($value);
            case "LF":
                return new LocalVariable($value);
            default:
                return new Literal($type, $value);
        }
    }
}

/**
 * Represent literal in language
 */
class Literal extends Argument
{
    /**
     * Type of literal
     */
    private $type;

    /**
     * Value of literal
     */
    private $value;

    /**
     * List of all supported literals
     */
    private static $registeredLiterals = [];

    public function renderNode($index)
    {
        $val = htmlspecialchars($this->value, ENT_XML1);
        return "<arg$index type=\"{$this->type}\">$val</arg$index>\n";
    }

    public function validate()
    {
        if (array_key_exists($this->type, Literal::$registeredLiterals)) {
            $regex = Literal::$registeredLiterals[$this->type];
            return preg_match($regex, $this->value);
        } else {
            fprintf(STDERR, "Unsupported literal of type '%s'\n", $this->type);
            exit(Errors::LEX_SYNTAX);
        }
    }

    public function toString()
    {
        return "$this->type - $this->value";
    }

    /**
     * Creates literal
     * @arg type - type of literal
     * @arg value - value of literal
     */
    public function __construct($type, $value)
    {
        $this->type = $type;
        $this->value = $value;
    }

    /**
     * Register supported literal
     * @arg type - literal type
     * @arg regex - validation regex for value
     */
    public static function registerLiteral($type, $regex)
    {
        Literal::$registeredLiterals[trim($type)] = $regex;
    }
}

/**
 * Represent type in language
 */
class Type extends Argument
{
    /**
     * Type
     */
    private $value;

    /**
     * Supported type
     */
    private static $registeredTypes = [];

    public function renderNode($index)
    {
        return "<arg$index type=\"type\">{$this->value}</arg$index>\n";
    }

    public function validate()
    {
        if (array_search($this->value, Type::$registeredTypes) !== false) {
            return true;
        } else {
            fprintf(STDERR, "Unsupported type '%s'\n", $this->value);
            exit(Errors::LEX_SYNTAX);
        }
    }

    public function toString()
    {
        return "Type: {$this->value}";
    }

    /**
     * Construct new type from value
     * @arg value - type value
     */
    public function __construct($value)
    {
        $this->value = $value;
    }

    /**
     * Register supported type
     * @arg type - type to be registered
     */
    public static function registerType($type)
    {
        array_push(Type::$registeredTypes, $type);
    }
}

/**
 * Represent variable in language
 */
abstract class Variable extends Argument
{
    /**
     * Regex to validate if variable name is correct
     */
    public static $REGEX = "/^[a-z_\-\$&%\*!\?][a-z0-9_\-\$&%\*!\?]*$/i";

    /**
     * Scope of variable
     */
    private $scope;
    
    /**
     * Identifier of variable
     */
    private $id;

    /**
     * Create new variable with assigned scope and identifier
     * @arg scope - variable scope
     * @arg id - name of variable
     */
    public function __construct($scope, $id)
    {
        $this->scope = $scope;
        $this->id = $id;
    }

    public function renderNode($index)
    {
        $val = htmlspecialchars($this->id, ENT_XML1);
        return "<arg$index type=\"var\">{$this->scope}@$val</arg$index>\n";
    }

    public function validate()
    {
        return preg_match(Variable::$REGEX, $this->id);
    }

    public function toString()
    {
        return "$this->scope@$this->id";
    }
}

/**
 * Represent local variable
 */
class LocalVariable extends Variable
{
    /**
     * Create new local variable
     * @arg id - name of variable
     */
    public function __construct($id)
    {
        parent::__construct("LF", $id);
    }
}

class GlobalVariable extends Variable
{
    /**
     * Create new global variable
     * @arg id - name of variable
     */
    public function __construct($id)
    {
        parent::__construct("GF", $id);
    }
}

class TemporaryVariable extends Variable
{
    /**
     * Create new temporary variable
     * @arg id - name of variable
     */
    public function __construct($id)
    {
        parent::__construct("TF", $id);
    }
}

/**
 * Represent new label
 */
class Label extends Argument
{
    /**
     * Name of label
     */
    private $value;

    /**
     * Create new label
     * @arg value - name of label
     */
    public function __construct($value)
    {
        $this->value = $value;
    }

    public function renderNode($index)
    {
        $val = htmlspecialchars($this->value, ENT_XML1);
        return "<arg$index type=\"label\">$val</arg$index>\n";
    }

    public function validate()
    {
        return preg_match(Variable::$REGEX, $this->value);
    }

    public function toString()
    {
        return "Label: $this->value";
    }
}

/**
 * Utility class to parse arguments and validate them
 */
class ArgumentConsumer
{
    /**
     * Arguments of type Argument
     */
    private $args;

    /**
     * Current offset in array
     */
    private $offset;

    /**
     * Validated variable storage
     */
    private $arguments;

    /**
     * Creates new argument consumer from parsed args
     * @arg args - parsed args
     */
    public function __construct($args)
    {
        $this->args = $args;
        $this->offset = 0;
        $this->arguments = [];
    }

    function __destruct()
    {
        $this->finish();
    }

    /**
     * Validate argument type with expected types. If argument is invalid, the script exits.
     * @arg expectedTypes - types to be checked
     */
    public function expect($expectedTypes)
    {
        if ($this->offset >= count($this->args)) {
            fprintf(STDERR, "Too many arguments\n");
            exit(Errors::LEX_SYNTAX);
        }

        $arg = Argument::from($this->args[$this->offset], $expectedTypes);
        $this->offset++;

        foreach ($expectedTypes as $expectedType) {
            if (is_a($arg, $expectedType)) {
                array_push($this->arguments, $arg);
                if ($arg->validate()) {
                    return;
                }
                else {
                    fprintf(STDERR, "Argument %s is invalid\n", $arg->toString());
                    exit(Errors::LEX_SYNTAX);
                }
            }
        }

        fprintf(STDERR, "Unsupported argument type %s, string value: %s.\nExpecting one of: [%s]\nArguments: %s\n", get_class($arg), $arg->toString(), join(", ", $expectedTypes), join(", ", $this->args));
        exit(Errors::LEX_SYNTAX);
    }

    /**
     * Finish argument checking and return validated arguments.
     */
    public function finish()
    {
        if ($this->offset !== count($this->args)) {
            $arguments = join(", ", $this->args);
            fprintf(STDERR, "Arguments were left unprocessed [%s]\n", $arguments);
            exit(Errors::LEX_SYNTAX);
        }
        return $this->arguments;
    }
}

/**
 * Represent instruction in language
 */
class Instruction
{
    /**
     * Name of instruction
     */
    private $name;

    /**
     * Arguments of instruction
     */
    private $arguments;

    /**
     * Create new instruction
     * @arg name - name of instruction
     * @arg arguments - arguments of instruction 
     */
    public function __construct($name, $arguments)
    {
        $this->name = strtoupper($name);
        $this->arguments = $arguments;
    }

    /**
     * Get name of instruction
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * Get arguments on instruction
     */
    public function getArguments()
    {
        return $this->arguments;
    }

    private function validate()
    {
        foreach ($this->arguments as $argument) {
            $argument->validate();
        }
    }

    /**
     * Create instruction object from line of source code
     * @arg lineOriginal - line of source code
     * @arg stats - stats object where all stats will be stored
     */
    public static function from($lineOriginal, $stats)
    {
        $line = removeComment($lineOriginal);

        if ($line !== $lineOriginal) {
            $stats->incrementComments();
        }

        $line = trim($line);
        if ($line === "") {
            return null;
        }

        $stats->incrementLoc();

        # split words by whitespace and find comments
        $segments = preg_split("/\s+/", trim($line));

        $args = count($segments) > 1 ? array_slice($segments, 1) : [];
        $consumer = new ArgumentConsumer($args);

        $instruction = null;
        switch (strtoupper($segments[0])) {
            case "NOT":
            case "MOVE":
            case "INT2CHAR":
            case "STRLEN":
            case "TYPE":
                $consumer->expect([Variable::class]);
                $consumer->expect([Variable::class, Literal::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "RETURN":
                $stats->incrementJumps();
            case "POPFRAME":
            case "CREATEFRAME":
            case "PUSHFRAME":
            case "BREAK":
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "DEFVAR":
            case "POPS":
                $consumer->expect([Variable::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "CALL":
                $stats->incrementJumps();
                $consumer->expect([Label::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "PUSHS":
                $consumer->expect([Variable::class, Literal::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "ADD":
            case "SUB":
            case "MUL":
            case "IDIV":
            case "LT":
            case "GT":
            case "EQ":
            case "AND":
            case "OR":
            case "STRI2INT":
            case "CONCAT":
            case "GETCHAR":
            case "SETCHAR":
                $consumer->expect([Variable::class]);
                $consumer->expect([Variable::class, Literal::class]);
                $consumer->expect([Variable::class, Literal::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "JUMPIFEQ":
            case "JUMPIFNEQ":
                $stats->incrementJumps();
                $consumer->expect([Label::class]);
                $consumer->expect([Variable::class, Literal::class]);
                $consumer->expect([Variable::class, Literal::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "READ":
                $consumer->expect([Variable::class]);
                $consumer->expect([Type::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "WRITE":
            case "EXIT":
            case "DPRINT":
                $consumer->expect([Variable::class, Literal::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            case "LABEL":
            case "JUMP":
                $consumer->expect([Label::class]);
                $instruction = new Instruction($segments[0], $consumer->finish());
                break;
            default:
                fprintf(STDERR, "Unknown op code %s\n", $segments[0]);
                exit(Errors::INVALID_OPCODE);
                break;
        }
        $instruction->validate();

        if($instruction->getName() === "LABEL") {
            $stats->addLabel($instruction->getArguments()[0]);
        }
        else if($instruction->getName() === "JUMP") {
            $stats->incrementJumps();
        }

        return $instruction;
    }
}

function removeComment($line)
{
    $out = strchr($line, "#", true);

    if ($out === false) {
        return $line;
    } else if (empty($out)) {
        # this is only line comment
        return "";
    } else {
        # line contains comment
        return $out;
    }
}

/**
 * Returns true if arguments was passed, false otherwise
 */
function getFlag($options, $key)
{
    return array_key_exists($key, $options);
}

function main()
{
    global $argv;
    global $argc;

    $shortopts  = "";

    $longopts  = array(
        "stats::",
        "help",
        "loc",
        "comments",
        "labels",
        "jumps",
    );

    $supportedStats = array_slice($longopts, 1);
    $stats = new Stats($supportedStats);
    $options = getopt($shortopts, $longopts);

    if (array_key_exists("help", $options)) {
        if ($argc !== 2) {
            fprintf(STDERR, "Invalid combination of arguments\n");
            exit(Errors::MISSING_PARAM);
        }

        print("Perform lexical and syntax analysis.\nUsage:\n\tphp parse.php --help\t- show help\n\tphp parse.php < source_code.ipp20\t- perform analysis\n");
        exit(0);
    }

    $hasAnyStatOptions = false;

    foreach ($supportedStats as $supportedStat) {
        if (array_key_exists($supportedStat, $options)) {
            $hasAnyStatOptions = true;
            break;
        }
    }

    if($hasAnyStatOptions && !getFlag($options, "stats")) {
        fprintf(STDERR, "Invalid combination of arguments\n");
        exit(Errors::MISSING_PARAM);
    }

    if (getFlag($options, "stats")) {
        $statsFilename = $options["stats"];

        if ($statsFilename === false) {
            fprintf(STDERR, "Stats param requires value\n");
            exit(Errors::MISSING_PARAM);
        }

        if ($statsFilename[0] === "\"" && $statsFilename[count($statsFilename) - 1] === "\"") {
            $statsFilename = substr($statsFilename, 1);
            $statsFilename = substr($statsFilename, 0, count($statsFilename) - 1);
        }
    }

    try {
        verifyArguments($argv, transformArguments($longopts));
    }
    catch(Exception $e) {
        fprintf(STDERR, "Error: %s\n", $e->getMessage());
        exit(Errors::MISSING_PARAM);
    }

    Literal::registerLiteral("string", "/^(\\\\\d{3}|[^\\r\\n\\t\\f\\v \\\\])*$/");
    Literal::registerLiteral("nil", "/^nil$/");
    Literal::registerLiteral("int", "/^(\\+|-|)\d+$/");
    Literal::registerLiteral("bool", "/^(true|false)$/i");
    Type::registerType("int");
    Type::registerType("bool");
    Type::registerType("string");

    while (($f = fgets(STDIN)) !== false) {
        $line = removeComment($f);

        # check if comment was removed
        if ($line !== $f) {
            $stats->incrementComments();
        }

        $line = trim($line);
        if ($line === "") {
            continue;
        }

        if (strtolower($line) == ".ippcode20") {
            break;
        } else {
            fprintf(STDERR, "Missing header: $f\n");
            exit(Errors::MISSING_HEADER);
        }
    }

    print("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    print("<program language=\"IPPcode20\">\n");
    $counter = 1;

    while (($line = fgets(STDIN)) !== false) {
        $instruction = Instruction::from($line, $stats);
        if ($instruction === null) {
            fprintf(STDERR, "Skipping line: $line\n");
        } else {
            print("<instruction order=\"$counter\" opcode=\"{$instruction->getName()}\">\n");
            foreach ($instruction->getArguments() as $index => $arg) {
                $node = $arg->renderNode($index + 1);
                print("$node\n");
            }
            print("</instruction>");
            $counter++;
        }
    }
    print("</program>\n");

    if (getFlag($options, "stats")) {
        $file = fopen($statsFilename, "w");

        if ($file) {
            foreach ($argv as $arg) {
                $arg = substr($arg, 2);
                if(array_search($arg, $supportedStats) !== false) {
                    fprintf($file, "%d\n", $stats->get($arg));
                }
            }
            fclose($file);
        }
        else {
            fprintf(STDERR, "Could not open $statsFilename\n");
            exit(Errors::FILE_OUT_OPEN);
        }
    }

    exit(0);
}
main();
