<?php


class TestError extends Error
{
    const MISSING_PARAM = 10;
    const FILE_OPEN = 11;
    const FILE_OUT_OPEN = 12;
    const INTERNAL = 99;

    public $statusCode;

    function __construct($code, $message)
    {
        parent::__construct($message, $code);
        $this->statusCode = $code;
    }
}

class MissingValueError extends TestError {

}

function insertIntoSet(&$array, $value) {
    if (array_search($value, $array) === false) {
        array_push($array, $value);
    }
}

class Args implements Countable {
    private $args;
    private $usedArgs;

    function __construct($args) {
        $this->args = $this->parseArgs($args);
        $this->usedArgs = array();
    }

    private function parseArg($arg) {
        $arg = trim($arg);
        $index = strpos($arg, "=");
    
        if($arg === "" || substr($arg, 0, 2) !== "--") {
            throw new TestError(TestError::MISSING_PARAM, "Invalid argument $arg");
        }
    
        return $index > 0 ? array(substr($arg, 2, $index - 2), substr($arg, $index + 1)) : array(substr($arg, 2), true);
    }

    private function parseArgs($args) {
        $argsDict = array();

        foreach ($args as $arg) {
            list($name, $value) = $this->parseArg($arg);

            if (!array_key_exists($name, $argsDict)) {
                $argsDict[$name] = array();
            }

            array_push($argsDict[$name], $value);
        }
        return $argsDict;
    }

    public function getSingleOrDefault($key, $default) {
        try {
            return $this->getSingle($key);
        }
        catch(TestError $e) {
            if (!is_callable($default)) {
                return $default;
            }
            else {
                return $default();
            }
        }
    }

    public function getSingle($key) {
        try {
            insertIntoSet($this->usedArgs, $key);
            $args = $this->args[$key];

            if ($args === null) {
                throw new Error();
            }

            if (count($args) > 1) {
                throw new TestError(TestError::MISSING_PARAM, "Argument '$key' must be unique");
            }
            return $args[0];
        }
        catch(Error $e) {
            throw new TestError(TestError::MISSING_PARAM, "Arguments '$key' were not provided $e");
        }
    }

    public function getAll($key) {
        try {
            insertIntoSet($this->usedArgs, $key);
            $args = $this->args[$key];

            if ($args === null) {
                throw new Error();
            }

            return $args;
        }
        catch (Error $e) {
            throw new TestError(TestError::MISSING_PARAM, "Arguments '$key' were not provided $e");
        }
    }

    public function usedAllArguments() {
        return count($this->usedArgs) == count($this->args);
    }

    public function getUnusedArguments() {
        $unusedArgs = array();

        foreach ($this->args as $key => $args) {
            if (array_search($key, $this->usedArgs) === false) {
                array_push($unusedArgs, $key);
            }
        }
        return $unusedArgs;
    }

    public function hasArg($key){
        return array_key_exists($key, $this->args);
    }
    public function count() {
        return count($this->args);
    }
        
    public function __toString()
    {
        return  $this->args;
    }
}

function findFileInCwd($filename) {
    $cwd = getcwd();

    if ($cwd !== false) {
        return $filename ? $cwd . DIRECTORY_SEPARATOR . $filename : $cwd;
    }
    else {
        throw new TestError(TestError::FILE_OPEN, "Could not get current working directory");
    }
}

function getFile($args, $key, $default) {
    try {
        $dir = $args->getSingle($key);

        if ($dir !== true) {
            return $dir;
        }
        else {
            throw new MissingValueError(TestError::MISSING_PARAM, "--$key=file is missing value");
        }
    }
    catch (TestError $e) {
        if ($default === null) {
            throw new MissingValueError(TestError::MISSING_PARAM, "Required argument --$key is missing");
        }

        if (is_callable($default)) {
            return $default($key);
        }
        else {
            return $default;
        }
    }
    catch(MissingValueError $e) {
        throw $e;
    }
}

function walk($file, $recursive = false)
{
    $fileStack = array($file);
    $started = false;

    while (!empty($fileStack)) {
        $filename = array_pop($fileStack);
        if (is_dir($filename)) {
            if (!$recursive && !$started) {
                $started = true;
            }
            else if (!$recursive && $started) {
                continue;
            }

            $filteredFiles = array_filter(scandir($filename), function($filename) {
                return $filename !== "." && $filename !== "..";
            });

            $absolutePathFiles = array_map(function ($f) use(&$filename) { return $filename . DIRECTORY_SEPARATOR . $f; } , $filteredFiles);
            array_push($fileStack, ...$absolutePathFiles);
        }
        else {
            yield $filename;
        }
    }
}

class TestCase {
    public $in;
    public $src;
    public $out;
    public $rc;
    public $name;

    public function __construct($name)
    {
        $this->in = null;
        $this->src = null;
        $this->out = null;
        $this->rc = null;
        $this->name = $name;
    }

    private function getFileNameFor($extension) {
        return dirname($this->name) . DIRECTORY_SEPARATOR . basename($this->name) . ".$extension";
    }

    public function isValid() {
        return $this->name !== null && $this->src !== null;
    }

    public function __toString() {
        return "TestCase(name='$this->name', in='$this->in', src='$this->src', out='$this->out', rc='$this->rc')";
    }

    public function prepareFiles()
    {
        if ($this->rc === null || !file_exists($this->rc)) {
            if ($f = fopen($this->getFileNameFor("rc"), "w")) {
                fprintf($f, "%d\n", 0);
                fclose($f);
            }
            else {
                throw new TestError(TestError::FILE_OUT_OPEN, "Could not create RC file {$this->getFileNameFor("rc")}");
            }
        }
        if ($this->out === null || !file_exists($this->out)) {
            touch($this->getFileNameFor("out"));
        }
        if ($this->in === null || !file_exists($this->in)) {
            touch($this->getFileNameFor("in"));
        }
    }

    public function getRc()
    {
        if ($f = fopen($this->getFileNameFor("rc"), "r")) {
            $rc = 0;
            fscanf($f, "%d", $rc);
            return $rc;
        }
        else {
            throw new TestError(TestError::FILE_OUT_OPEN, "Could not get RC from file {$this->getFileNameFor("rc")}");
        }
    }

    public function compareParseOnly($php, $jar, $parseScript)
    {
        $rc = null;
        $out = null;
        unset($out);
        exec("$php $parseScript < $this->src > " . $this->getFileNameFor(".xml"), $out, $rc);

        if ($rc === 0) {
            exec("$jar ");
        }
        else {
            return $rc !== $this->getRc();
        }
    }
}

function getTestName($file)
{
    return dirname($file) . DIRECTORY_SEPARATOR . pathinfo($file, PATHINFO_FILENAME);
}

function groupFiles($generator) {
    $currentTestCase = new TestCase(null);
    foreach ($generator as $file) {
        $testName = getTestName($file);

        if ($testName != $currentTestCase->name) {
            if ($currentTestCase->isValid()) {
                yield $currentTestCase;
            }

            $currentTestCase = new TestCase($testName);
        }

        $fileExtension = pathinfo($file, PATHINFO_EXTENSION);

        switch ($fileExtension) {
            case "src":
                $currentTestCase->src = $file;
                break;
            case "out":
                $currentTestCase->out = $file;
                break;
            case "rc":
                $currentTestCase->rc = $file;
                break;  
            case "in":
                $currentTestCase->in = $file;
                break;  
            default:
                // ignore file
                break;
        }
    }
}

function main()
{
    global $argc;
    global $argv;

    try {
        walk(getcwd());
        $args = new Args(array_slice($argv, 1));

        if ($args->hasArg("help")) {
            $help = $args->getSingle("help");

            if ($args->usedAllArguments()) {
                print("test.php ([--help]|[--directory=path] [--recursive] [--parse-script=file] [--int-script=file] [--parse-only] [--int-only] [--jexamxml=file])\n");
                print("\t--directory=path - test directory\n");
                print("\t--recursive - traverse all directories\n");
                print("\t--parse-script=file - path to parse.php\n");
                print("\t--int-script=file - path to interpret.php\n");
                print("\t--parse-only - test parser only\n");
                print("\t--int-only - test interpreter only\n");
                print("\t--jexamxml=file - path to jexamxml.jar. Can be used only with --parse-only\n");
                exit(0);
            }
            else {
                throw new TestError(TestError::MISSING_PARAM, "Cannot combine help argument with other arguments");
            }
        }

        $directory = getFile($args, "directory", function() { return findFileInCwd(null); });
        $recursive = $args->getSingleOrDefault("recursive", false);
        $parseScript = getFile($args, "parse-script", function() { return findFileInCwd("parse.php"); });
        $intScript = getFile($args, "int-script", function() { return findFileInCwd("interpret.py"); });
        $parseOnly = $args->getSingleOrDefault("parse-only", false);
        $intOnly = $args->getSingleOrDefault("int-only", false);
        $jexamxml = getFile($args, "jexamxml", function() { return findFileInCwd("jexamxml.jar"); });
        
        $groupedFiles = groupFiles(walk($directory, $recursive));

        foreach ($groupedFiles as $testCase) {
            print("$testCase\n");
        }
    }
    catch(TestError $e) {
        fprintf(STDERR, "Error: {$e->getMessage()}\nStatus code: {$e->statusCode}\n");
    }
}

main();

?>