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
    if (!file_exists($file)) {
        throw new TestError(TestError::FILE_OPEN, "Test directory does not exist - $file");
    }

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

function diffFiles($expectedOutputFile, $actualOutputFile)
{
    $expectedFile = false;
    $outputFile = false;
    if(($expectedFile = fopen($expectedOutputFile, "r")) && ($outputFile = fopen($actualOutputFile, "r"))) {
        while (true) {
            $line1 = fgets($expectedFile);
            $line2 = fgets($outputFile);

            if ($line1 !== $line2) {
                fclose($expectedFile);
                fclose($outputFile);
                return false;
            }
            if ($line1 === false) {
                fclose($expectedFile);
                fclose($outputFile);
                return true;
            }
        }
    }
    else {
        if ($expectedFile) {
            fclose($expectedFile);
            throw new TestError(TestError::FILE_OPEN, "Could not open file $actualOutputFile");
        }
        else {
            throw new TestError(TestError::FILE_OPEN, "Could not open file $expectedOutputFile");
        }
    }
}

class TestCase {
    public $in;
    public $src;
    public $out;
    public $rc;
    public $name;
    private $cachedRc = null;

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
            $this->rc = $this->getFileNameFor("rc");
            if ($f = fopen($this->rc, "w")) {
                fprintf($f, "%d\n", 0);
                fclose($f);
            }
            else {
                throw new TestError(TestError::FILE_OUT_OPEN, "Could not create RC file $this->rc");
            }
        }
        if ($this->out === null || !file_exists($this->out)) {
            $this->out = $this->getFileNameFor("out");
            touch($this->out);
        }
        if ($this->in === null || !file_exists($this->in)) {
            $this->in = $this->getFileNameFor("in");
            touch($this->in);
        }
    }

    public function getRc()
    {
        if ($this->cachedRc !== null) {
            return $this->cachedRc;
        }

        if ($f = fopen($this->getFileNameFor("rc"), "r")) {
            $rc = 0;
            fscanf($f, "%d", $rc);
            $this->cachedRc = $rc;
            return $rc;
        }
        else {
            throw new TestError(TestError::FILE_OUT_OPEN, "Could not get RC from file {$this->getFileNameFor("rc")}");
        }
    }

    public function parse($php, $parseScript, $sourceFile, $outputFile) {
        $rc = -1;
        $out = null;
        exec("$php $parseScript < $sourceFile > " . $outputFile, $out, $rc);
        return $rc;
    }

    public function interpret($python, $intScript, $sourceFile, $inputFile, $outputFile) {
        $rc = -1;
        exec("$python $intScript --source=\"$sourceFile\" --input=\"$inputFile\" > " . $outputFile, $out, $rc);
        return $rc;
    }

    public function compareParseOnly($php, $jar, $parseScript)
    {
        $file = $this->getFileNameFor("xml");
        $expectedOutput = $this->out;
        $rc = $this->parse($php, $parseScript, $this->src, $file);

        if ($rc === 0 && $this->getRc() === 0) {
            $out = null;
            exec("java -jar $jar \"$file\" \"$expectedOutput\" options", $out, $rc);
            unlink($file);
            return $rc === 0;
        }
        else {
            unlink($file);
            if ($rc === $this->getRc()) {
                return true;
            }
            else {
                fprintf(STDERR, "Incorrect return code. Expecting {$this->getRc()}. Got $rc\n");
                return false;
            }
        }
    }

    public function compareIntOnly($python, $intScript)
    {
        $file = $this->getFileNameFor("result");
        $expectedOutput = $this->out;
        $rc = $this->interpret($python, $intScript, $this->src, $this->in, $file);

        if ($rc === 0 && $this->getRc() === 0) {
            $result = diffFiles($expectedOutput, $file);
            unlink($file);
            return $result;
        }
        else {
            unlink($file);
            if ($rc === $this->getRc()) {
                return true;
            }
            else {
                fprintf(STDERR, "Incorrect return code. Expecting {$this->getRc()}. Got $rc\n");
                return false;
            }
        }
    }

    public function compare($php, $parseScript, $python, $intScript)
    {
        $xml = $this->getFileNameFor("xml");
        $programOutputFile = $this->getFileNameFor("output");
        $rc = $this->parse($php, $parseScript, $this->src, $xml);

        if ($rc !== 0) {
            unlink($xml);
            return $rc === $this->getRc();
        }

        $rc = $this->interpret($python, $intScript, $xml, $this->in, $programOutputFile);

        if ($rc === 0 && $this->getRc() === 0) {
            $result = diffFiles($this->out, $programOutputFile);
            unlink($xml);
            unlink($programOutputFile);
            return $result;
        }
        else {
            unlink($xml);
            unlink($programOutputFile);
            if ($rc === $this->getRc()) {
                return true;
            }
            else {
                fprintf(STDERR, "Incorrect return code. Expecting {$this->getRc()}. Got $rc\n");
                return false;
            }
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
    if ($currentTestCase->isValid()) {
        yield $currentTestCase;
    }
}

function parseOnlyTest($parseScript, $jexamxml)
{
    return function($testCase) use($parseScript, $jexamxml) {
        return $testCase->compareParseOnly("php7.4", $jexamxml, $parseScript);
    };
}

function intOnlyTest($intScript)
{
    return function($testCase) use($intScript) {
        return $testCase->compareIntOnly("python3.8", $intScript);
    };
}

function bothTest($parseScript, $intScript)
{
    return function($testCase) use($parseScript, $intScript) {
        return $testCase->compare("php7.4", $parseScript, "python3.8", $intScript);
    };
}

function main()
{
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
        $parseOnly = $args->getSingleOrDefault("parse-only", false);
        $intOnly = $args->getSingleOrDefault("int-only", false);
        $parseScript = getFile($args, "parse-script", false);
        $intScript = getFile($args, "int-script", false);
        $jexamxml = getFile($args, "jexamxml", function() { return findFileInCwd("jexamxml.jar"); });
        
        if (!is_dir($directory)) {
            throw new TestError(TestError::FILE_OPEN, "--directory does not point to directory");
        }

        $testFn = null;
        $errors = array();

        if ($intOnly) {
            if ($parseOnly) {
                array_push($errors, "--int-only cannot be combine with --parse-only");
            }
            if ($parseScript) {
                array_push($errors, "--int-only cannot be combine with --parse-script");
            }
            if (count($errors) === 0) {
                $intScript = getFile($args, "int-script", function() { return findFileInCwd("interpret.py"); });
                $testFn = intOnlyTest($intScript);
            }
        }
        else if ($parseOnly) {
            if ($intOnly) {
                array_push($errors, "--parse-only cannot be combine with --int-only");
            }
            if ($intScript) {
                array_push($errors, "--parse-only cannot be combine with --int-script");
            }
            if (count($errors) === 0) {
                $parseScript = getFile($args, "parse-script", function() { return findFileInCwd("parse.php"); });
                $testFn = parseOnlyTest($parseScript, $jexamxml);
            }
        }
        else {
            $parseScript = getFile($args, "parse-script", function() { return findFileInCwd("parse.php"); });
            $intScript = getFile($args, "int-script", function() { return findFileInCwd("interpret.py"); });
            $testFn = bothTest($parseScript, $intScript);
        }

        if (count($errors) > 0) {
            throw new TestError(TestError::MISSING_PARAM, join("\n", $errors));
        }

        $passed = 0;
        $failed = 0;

        $groupedFiles = groupFiles(walk($directory, $recursive));

        foreach ($groupedFiles as $testCase) {
            $testCase->prepareFiles();
            $result = $testFn($testCase);
            if ($result === true) {
                $passed++;
            }
            else {
                $failed++;
            }
            fprintf(STDERR, "%s - %s\n", $testCase->name, $result === true ? "PASSED" : "FAILED");
        }

        fprintf(STDERR, "PASSED: %d\nFAILED: %d\nTOTAL: %d\n", $passed, $failed, $passed + $failed);
    }
    catch(TestError $e) {
        fprintf(STDERR, "Error: {$e->getMessage()}\nStatus code: {$e->statusCode}\n");
        exit($e->statusCode);
    }
}

main();
