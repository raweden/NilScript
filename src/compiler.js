/*
    compiler.js
    (c) 2013-2014 musictheory.net, LLC
    MIT license, http://www.opensource.org/licenses/mit-license.php
*/

"use strict";

var esprima     = require("./esprima");
var Syntax      = esprima.Syntax;

var Builder     = require("./builder");
var Transformer = require("./transformer");
var TypeChecker = require("./typechecker");

var OJError     = require("./errors").OJError;
var OJModel     = require("./model").OJModel;

var _           = require("lodash");
var fs          = require("fs");
var escodegen   = require("escodegen");
var util        = require("util");


function errorForEsprimaError(inError)
{
    throw inError;
    var line = inError.lineNumber;

    var message = inError.description ? inError.description : ("" + inError);
    message = message.replace(/$.*Line:/, "");

    var outError = new Error(message);

    outError.line   = line;
    outError.column = inError.column;
    outError.name   = OJError.ParseError;
    outError.reason = message;

    return outError;
}


class CompilerFile {

constructor(arg)
{
    if (_.isString(arg)) {
        this.path     = arg;
        this.contents = fs.readFileSync(f).toString();
        this.mtime    = Infinity;

    } else {
        if (arg.path && arg.contents) {
            this.path     = arg.path;
            this.contents = arg.contents;
        } else {
            throw new Error("File object must contain 'path' and 'contents' keys");
        }

        this.mtime = arg.mtime ? arg.mtime : Infinity;      
    }

    this.ast = null;

}

}


function Compiler(options)
{
    options = options || { };

    var files        = (options.files || { }).map( f => new CompilerFile(f) );

    this._model = new OJModel();

    if (options.state) {
        this._model.loadState(options.state);
    }

    if (options["squeeze"]) {
        this._model.setupSqueezer(
            options["squeeze-start-index"] || 0,
            options["squeeze-end-index"]   || 0
        );
    }

    this._files   = files;
    this._options = options;
}


Compiler.prototype._getFileAndLineForLine = function(inLine)
{
    var files      = this._inputFiles;
    var lineCounts = this._inputLineCounts;

    var startLineForFile = 0; 
    var endLineForFile   = 0;

    for (var i = 0, length = files.length; i < length; i++) {
        var lineCount = lineCounts[i] || 0;
        endLineForFile = startLineForFile + lineCount;

        if (inLine >= startLineForFile && inLine <= endLineForFile) {
            return [ files[i], inLine - startLineForFile ];
        }

        startLineForFile += lineCount;
    }

    return null;
}


Compiler.prototype._cleanupError = function(e)
{
    if (e.line && !e.file) {
        var fileAndLine = this._getFileAndLineForLine(e.line);

        if (fileAndLine) {
            e.file = fileAndLine[0];
            e.line = fileAndLine[1];
        }
    }
}


Compiler.prototype.compile = function(callback)
{
    var dumpTime       = 1 || this._options["dump-time"];
    var outputLanguage = this._options["output-language"];

    var waitingForChecker = true;

    var cleanupError = function(err) {
        if (err) this._cleanupError(err);
    }.bind(this);

    function getLines(stringOrLines) {
        if (typeof stringOrLines == "string") {
            return stringOrLines.split("\n")
        } else {
            return stringOrLines;
        }
    }

    function finish(err, result) {
        if (err || !waitingForChecker) {
            cleanupError(err);
            callback(err, result);
        }
    }

    function printTime(name, start) {
        if (dumpTime) {
            console.error(name, Math.round(process.hrtime(start)[1] / (1000 * 1000)) + "ms");
        }
    }

    function time(name, f) {
        var start = process.hrtime();
        f();
        printTime(name, start);
    }

    try {
        var compiler  = this;
        var files     = this._files;
        var model     = this._model;
        var options   = this._options;

        var hasOutput  = options["output-language"] != "none";
        var checkTypes = options["check-types"];

        var result  = { };
        var lineMap;
        var primaryInputASTs     = [ ];
        var typecheckerInputASTs = [ ];

        var typecheckerOutputAst;

        // Parse each file's contents into an AST
        time("Parse", function() {
            for (let file of files) {
                try { 
                    file.ast = esprima.parse(file.contents, { loc: true });
                } catch (e) {
                    throw errorForEsprimaError(e);
                }
            }
        });

        // Do first pass with Builder and save into model
        time("Build", function() {
            for (let file of files) {
                (new Builder(file.ast, model, options)).build();
                primaryInputASTs.push(file.ast);
            }
        });

        // Transfer or clone AST (if needed)
        time("AST Clone", function() {
            if (checkTypes && hasOutput) {
                typecheckerInputASTs = _.cloneDeep(primaryInputASTs);
            } else if (checkTypes) {
                typecheckerInputASTs = primaryInputASTs;
                primaryInputASTs = null;
            }
        })

        // Transform Output AST, generate code, and concatenate
        if (hasOutput) {
            let outputAST  = null;
            let outputMap  = null;
            let outputCode = "";

            time("Transform", function() {
                // profiler.startProfiling("transformer");

                let t = Transformer.transform(primaryInputASTs, model, options);
                outputAST = t.ast;
                result.warnings = (result.warnings || [ ]).concat(t.warnings);

                // var cpuProfile = profiler.stopProfiling("transformer");
                // fs.writeFileSync("out.cpuprofile", JSON.stringify(cpuProfile));
            });

            time("Generate", function() {
                if (0) {
                    let output = escodegen.generate(outputAST, {
                        sourceMap: "internal",
                        sourceMapWithCode: true
                    });

                    outputCode = output.code;
                    outputMap  = output.map;
                    } else {
                    outputCode = escodegen.generate(outputAST);
                }

            });

            time("Concatenate", function() {
                function getString(s) {
                    if (Array.isArray(s)) return s.join("\n");
                    else if (s) return s;
                    else return "";
                }

                result.code = getString(options.prepend) + outputCode + getString(options.append);
            });

            time("Archive", function() {
                result.state = model.saveState();
            });
        }

        if (options["dump-ast"]) {
            result.ast = JSON.stringify(ast, function(key, value) {
                if (key == "oj_parent" || key == "oj_scope") {
                    return undefined;
                }
                return value;
            }, 4)
        }

        if (options["dump-scope"]) {
            result.scope = model.scope.toString();
        }        

        result.cache = options["cache"];


        // Type checker
        //
        if (0 && checkTypes) {
            let outputAST  = null;
            let outputCode = "";

            var noImplicitAny = options["no-implicit-any"];

            time("Transform", function() {
                let t = Transformer.transform(primaryInputASTs, model, options);
                outputAST = t.ast;
                result.warnings = (result.warnings || [ ]).concat(t.warnings);

            });

            time("Type Check", function() {
                var checker = new TypeChecker(model, typecheckerInputASTs, inputFiles, noImplicitAny);

                checker.check(function(err, warnings) {
                    waitingForChecker = false;
                    result.warnings = (result.warnings || [ ]).concat(warnings);
                    finish(err, result);
                });
            });
        } else {
            waitingForChecker = false;
            finish(null, result);
        }

    } catch (e) {
        if (e.name && e.name.indexOf("OJ") !== 0) {
            console.error("Internal oj error!")
            console.error("------------------------------------------------------------")
            console.error(e);
            console.error(e.stack);
            console.error("------------------------------------------------------------")
        }

        cleanupError(e);

        callback(e, null);
    }
}


module.exports = {
    compile(options, callback) {
        var compiler = new Compiler(options);
        compiler.compile(callback);
    }
};
