/*
    transformer.js
    (c) 2013-2014 musictheory.net, LLC
    MIT license, http://www.opensource.org/licenses/mit-license.php
*/

var _          = require("lodash");
var esprima    = require("./esprima");
var Syntax     = esprima.Syntax;

var Modifier   = require("./modifier");
var Traverser  = require("./traverser");
var Utils      = require("./utils");
var Tree       = require("./tree");

var OJModel    = require("./model").OJModel;
var OJError    = require("./errors").OJError;
var OJWarning  = require("./errors").OJWarning;

var OJGlobalVariable          = "$oj_oj";

var OJClassPrefix             = "$oj_c_";
var OJMethodPrefix            = "$oj_f_";
var OJIvarPrefix              = "$oj_i_";
var OJClassMethodsVariable    = "$oj_s";
var OJInstanceMethodsVariable = "$oj_m";
var OJTemporaryReturnVariable = "$oj_r";
var OJSuperVariable           = "$oj_super";

var LanguageEcmascript5 = "ecmascript5";

function _makeNode(str)
{
    var program = esprima.parse(str);
    return program.body[0];
}

function x(node)
{
    console.log(require("util").inspect(node, { depth: null }));
}


function unshiftContents(array, array2)
{
    Array.prototype.unshift.apply(array, array2);
}


function pushContents(array, array2)
{
    Array.prototype.push.apply(array, array2);
}


function Transformer(ast, model, modifier, forTypechecker, options)
{
    this._ast      = ast;
    this._model    = model;
    this._modifier = modifier;
    this._options  = options;
    this._warnings = [ ];

    var inlines = { };

    // Typechecker inlines anonymous enums
    if (options["inline-enum"] || forTypechecker) {
        _.each(model.enums, function(e) {
            var enumName = e.name;

            _.each(e.values, function(value, name) {
                if (enumName && forTypechecker) {
                    inlines[name] = enumName + "." + name;
                } else {
                    inlines[name] = value;
                }
            });
        });
    }

    // Typechecker forces 'inline-const'
    if (options["inline-const"] || forTypechecker) {
        _.each(model.consts, function(value, name) {
            if (inlines[name] === undefined) {
                inlines[name] = value;
            }
        });
    }

    var additionalInlines = options["additional-inlines"];
    if (additionalInlines) {
        for (var key in additionalInlines) {
            if (additionalInlines.hasOwnProperty(key)) {
                inlines[key] = JSON.stringify(additionalInlines[key]);
            }
        }
    }

    this._inlines = inlines;
    this._squeeze = options["squeeze"];
}


Transformer.prototype.getSymbolForClassName = function(className)
{
    if (!className) return;

    if (!Utils.isBaseObjectClass(className)) {
        if (this._squeeze) {
            return this._model.getSqueezedName(OJClassPrefix + className, true);
        } else {
            return OJClassPrefix + className;
        }
    }

    return className;
}


Transformer.prototype.getSymbolForSelectorName = function(selectorName)
{
    var replacedName = selectorName;
    replacedName = replacedName.replace(/_/g,   "__");
    replacedName = replacedName.replace(/^__/g, "_");
    replacedName = replacedName.replace(/\:/g,  "_");

    if (!Utils.isBaseObjectSelectorName(selectorName)) {
        if (this._squeeze) {
            return this._model.getSqueezedName(OJMethodPrefix + replacedName, true);
        } else {
            return OJMethodPrefix + replacedName;
        }
    }

    return replacedName;
}


Transformer.prototype.getSymbolForClassNameAndIvarName = function(className, ivarName)
{
    var result = OJIvarPrefix + className + "$" + ivarName;
    if (this._squeeze) result = this._model.getSqueezedName(result, true);
    return result;
}


Transformer.prototype.makeNodeWithString = function(str)
{
    return esprima.parse(str).body[0];
}


Transformer.prototype.generate = function()
{
    var traverser = new Traverser(this._ast);

    var generator = this;
    var transformer = this;

    var model    = this._model;
    var modifier = this._modifier;
    var options  = this._options;
    var inlines  = this._inlines;
    var scope    = this._model.scope;

    var methodNodes = [ ];
    var methodNodeClasses = [ ];
    var currentClass;
    var currentMethodNode;

    var classReplacement;

    var methodUsesSelfVar        = false;
    var methodUsesTemporaryVar   = false;
    var methodUsesLoneExpression = false;

    var optionWarnOnThisInMethods    = options["warn-this-in-methods"];
    var optionWarnOnUnknownSelectors = options["warn-unknown-selectors"];
    var optionWarnOnUnusedIvars      = options["warn-unused-ivars"];
    var optionWarnOnUnknownIvars     = options["warn-unknown-ivars"];

    var optionSqueeze = this._squeeze;

    var removeEnums    = options["inline-enum"];
    var removeConsts   = options["inline-const"];
    var removeTypes    = true;
    var knownSelectors = optionWarnOnUnknownSelectors ? model.selectors : null;

    var warnings = this._warnings;

    function getClassAsRuntimeVariable(className)
    {
        return OJGlobalVariable + "._cls." + generator.getSymbolForClassName(className);
    }

    function generateThisIvar(className, ivarName, useSelf)
    {
        return (useSelf ? "self" : "this") + "." + generator.getSymbolForClassNameAndIvarName(className, ivarName);
    }

    function canBeInstanceVariableOrSelf(node)
    {
        var parent = node.oj_parent;

        if (parent.type == Syntax.MemberExpression && !parent.computed) {
            return parent.object == node;
        }

        return true;   
    }

    function replaceMessageExpression(node)
    {
        return Traverser.RemoveNode;

    /*
        var selectorName = node.selectorName;
        var methodName   = generator.getSymbolForSelectorName(selectorName);

        var messageSelectors = node.messageSelectors;
        var receiverNode     = node.receiver.value;
        var argumentNodes    = [ ];

        var replacementPiece;

        for (var i = 0, length = messageSelectors.length; i < length; i++) {
            argumentNodes.push(messageSelectors[i].argument);
        }

        if (knownSelectors && !knownSelectors[node.selectorName]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + node.selectorName + "'", node));
        }

        // Optimization cases
        if (receiverNode.type == Syntax.Identifier && currentMethodNode) {
            var usesSelf   = methodUsesSelfVar;
            var selfOrThis = usesSelf ? "self" : "this";
            var useProto   = (currentMethodNode.selectorType != "+");

            var classSymbol, methodSymbol;

            if (receiver.name == "super") {
                classSymbol = transformer.getSymbolForClassName(className);

                if (currentMethodNode.selectorType == "+") {
                    replacementPiece = Tree.makeCallSuperClassMethodPiece(classSymbol, methodSymbol);
                } else  {
                    replacementPiece = Tree.makeCallSuperInstanceMethodPiece(classSymbol, methodSymbol);
                }

            } else if (model.classes[receiver.name]) {
                classSymbol = transformer.getSymbolForClassName(receiver.name);

                if (methodName == "alloc") {
                    replacementPiece = Tree.makeClassPiece(classSymbol);
                    return Tree.wrapInNew(replacementPiece.result);
                } else {
                    replacementPiece = 

                }

                return OJGlobalVariable + "._cls." + generator.getSymbolForClassName(className);


            }


            getTemplate()


            // [super foo] -> s
            if (receiver.name == "super") {
                doCommonReplacement(currentClass.name + "." + OJSuperVariable + "." + (useProto ? "prototype." : "") + methodName + ".call(this" + (hasArguments ? "," : ""), ")");
                return;

            } else if (model.classes[receiver.name]) {
                var classVariable = getClassAsRuntimeVariable(receiver.name);

                if (methodName == "alloc") {
                    node.receiver.oj_skip = true;
                    modifier.select(node).replace("new " + classVariable + "()");
                    return;
                }

                doCommonReplacement(classVariable + "." + methodName + "(", ")");
                return;

            } else if (receiver.name == "self") {
                doCommonReplacement(selfOrThis + "." + methodName + "(", ")");
                return;

            } else if (currentClass.isIvar(receiver.name)) {
                var ivar = generateThisIvar(currentClass.name, receiver.name, usesSelf);

                methodUsesLoneExpression = true;

                doCommonReplacement("(" + ivar + " && " + ivar + "." + methodName + "(", "))");

                return;

            } else {
                methodUsesLoneExpression = true;

                doCommonReplacement("(" + receiver.name + " && " + receiver.name + "." + methodName + "(", "))");

                return;
            }

        } else if (currentMethodNode) {
            methodUsesTemporaryVar   = true;
            methodUsesLoneExpression = true;

            replaceMessageSelectors();

            modifier.from(node).to(receiver).replace("((" + OJTemporaryReturnVariable + " = (");

            if (receiver.type == Syntax.Identifier && model.classes[receiver.name]) {
                modifier.select(receiver).replace(getClassAsRuntimeVariable(receiver.name));
            }

            modifier.from(receiver).to(firstSelector).replace(")) && " + OJTemporaryReturnVariable + "." + methodName + "(");
            modifier.from(lastSelector).to(node).replace("))");

            return;
        }

        // Slow path
        replaceMessageSelectors();

        modifier.from(node).to(receiver).replace(OJGlobalVariable + ".msgSend(");

        if (receiver.type == Syntax.Identifier && model.classes[receiver.name]) {
            modifier.select(receiver).replace(getClassAsRuntimeVariable(receiver.name));
        }

        var selector;
        if (Utils.isJScriptReservedWord(methodName)) {
            selector = "{ \"" + methodName + "\": " + "1 }";
        } else {
            selector = "{ " + methodName + ": " + "1 }";
        }

        modifier.from(receiver).to(firstSelector).replace("," + selector + (hasArguments ? "," : ""));
        modifier.from(lastSelector).to(node).replace(")");
*/
    }

    function replaceClassImplementation(inNode)
    {
        var className  = currentClass.name;
        var superName  = currentClass.superclassName;
        var ivars      = currentClass.getAllIvars();
        var model      = currentClass.model;

        var piece       = Tree.makeRegisterClassPiece(className);
        var replacement = piece.result;

        var toAppend = [ ];

        piece.arguments[0] = Tree.makeSelectorPiece(transformer.getSymbolForClassName(className)).result;

        if (superName) {
            var superSymbol = transformer.getSymbolForClassName(superName);
            piece.arguments[1] = Tree.makeSelectorPiece(superSymbol).result;
            toAppend.push(Tree.makeConstructorCallSuperPiece(superSymbol).result);
        } else {
            piece.arguments[1] = Tree.getNullLiteral();
        }

        var ivarMap = currentClass.getTypeToSortedIvarNameMap();

        _.each(ivarMap.object, function(ivar) {
            var symbol = transformer.getSymbolForClassNameAndIvarName(className, ivar);
            var node   = Tree.makeConstructorInitIvarPiece(symbol, null).result;

            toAppend.push( Tree.wrapInExpressionStatement(node) );
        });

        _.each(ivarMap.number, function(ivar) {
            var symbol = transformer.getSymbolForClassNameAndIvarName(className, ivar);
            var node   = Tree.makeConstructorInitIvarPiece(symbol, 0).result;

            toAppend.push( Tree.wrapInExpressionStatement(node) );
        });

        _.each(ivarMap.boolean, function(ivar) {
            var symbol = transformer.getSymbolForClassNameAndIvarName(className, ivar);
            var node   = Tree.makeConstructorInitIvarPiece(symbol, false).result;
            
            toAppend.push( Tree.wrapInExpressionStatement(node) );
        });

        var constructorPiece = Tree.makeConstructorPiece(className);

        piece.body.push(constructorPiece.result);

        unshiftContents( constructorPiece.body, toAppend );

        // It's important that we flatten() here, as replaceAtPropertyDirective() can return
        // multiple nodes, resulting in an invalid AST
        //
        // See: https://github.com/estools/estraverse/issues/38
        // 
        pushContents( piece.body, _.flatten(inNode.body.body) );

        piece.body.push( Tree.wrapInReturn(Tree.getIdentifier(className)) );

        return replacement;
    }

    function replaceAtPropertyDirective(node)
    {
        var propertyName = node.id.name;

        var makeGetter = currentClass.shouldGenerateGetterImplementationForPropertyName(propertyName);
        var makeSetter = currentClass.shouldGenerateSetterImplementationForPropertyName(propertyName);

        if (!makeSetter && !makeGetter) {
            return Traverser.RemoveNode;
        }

        var property   = currentClass.getPropertyWithName(propertyName);
        var ivarSymbol = transformer.getSymbolForClassNameAndIvarName(currentClass.name, property.ivar);

        var result = [ ];

        if (makeSetter) {
            var setterSymbol = transformer.getSymbolForSelectorName(property.setter);
            var node = Tree.makePropertySetterPiece(setterSymbol, ivarSymbol).result;

            result.push( Tree.wrapInExpressionStatement(node) );
        }

        if (makeGetter) {
            var getterSymbol = transformer.getSymbolForSelectorName(property.getter);
            var node = Tree.makePropertyGetterPiece(getterSymbol, ivarSymbol).result;

            result.push( Tree.wrapInExpressionStatement(node) );
        }

        return result;
    }

    function replaceMethodDefinition(node)
    {
        var methodName = generator.getSymbolForSelectorName(node.selectorName);
        var isClassMethod = node.selectorType == "+";
        var args = [ ];

        if (Utils.isReservedSelectorName(node.selectorName)) {
            Utils.throwError(OJError.ReservedMethodName, "The method name \"" + node.selectorName + "\" is reserved by the runtime and may not be overridden.", node);
        }

        for (var i = 0, length = node.methodSelectors.length; i < length; i++) {
            var variableName = node.methodSelectors[i].variableName;
            var methodType   = node.methodSelectors[i].methodType;

            if (variableName) {
                args.push(variableName.name);
            }
        }

        var string = (isClassMethod ? OJClassMethodsVariable : OJInstanceMethodsVariable) + "." + methodName + " = function(" + args.join(", ") + ") { ";

        if (methodUsesSelfVar || methodUsesTemporaryVar || methodUsesLoneExpression) {
            var varParts = [ ];

            if (methodUsesSelfVar) varParts.push("self = this");
            if (methodUsesTemporaryVar) varParts.push(OJTemporaryReturnVariable);

            if (methodUsesLoneExpression) {
                string += "/* jshint expr: true */";
            }

            if (varParts.length) {
                string += "var " + varParts.join(",") + ";";
            }
        }

        string += "}";

        var replacement = transformer.makeNodeWithString(string);

        var fromBody = node.body.body;
        var toBody   = replacement.expression.right.body.body;
        Array.prototype.push.apply(toBody, fromBody);

        return replacement;
    }

    function replaceIdentifier(node)
    {
        var name = node.name;
        var replacement = node;

/*i: Move to builder phase
        if (name.indexOf("$oj") == 0) {
            if (name[3] == "$" || name[3] == "_") {
                Utils.throwError(OJError.DollarOJIsReserved, "Identifiers may not start with \"$oj_\" or \"$oj$\"", node);
            }
        }
*/

        if (currentMethodNode && currentClass && canBeInstanceVariableOrSelf(node)) {
            if (currentClass.isIvar(name) || name == "self") {
                var usesSelf = currentMethodNode && methodUsesSelfVar;

                if (name == "self") {
                    replacement = Tree.makeIvarPiece(usesSelf, null).result;
                } else {
                    var ivarSymbol = transformer.getSymbolForClassNameAndIvarName(currentClass.name, name);
                    replacement = Tree.makeIvarPiece(usesSelf, ivarSymbol).result;

                    // remove ivar from unusedIvars
                    if (optionWarnOnUnusedIvars) {
                        if (unusedIvars && unusedIvars.indexOf(name) >= 0) {
                            unusedIvars = _.without(unusedIvars, name);
                        }
                    }
                }

            } else {
                if (name[0] == "_" && optionWarnOnUnknownIvars && (name.length > 1)) {
                    warnings.push(Utils.makeError(OJWarning.UndeclaredInstanceVariable, "Use of undeclared instance variable " + node.name, node));
                }
            } 
        }

        if (inlines) {
            var result = inlines[name];
            if (result !== undefined) {
                if (inlines.hasOwnProperty(name)) {
                    throw "Fix this"
//                    modifier.select(node).replace("" + result);
                    return;
                }
            }
        }

        if (optionSqueeze) {
            var result = model.getSqueezedName(name, false);
            if (result !== undefined) {
                throw "Fix this"
//                modifier.select(node).replace("" + result);
                return;
            }
        }

        if (node.annotation) {
            delete(node.annotation);
        }

        return replacement;
    }


    function replaceAtSelectorDirective(node)
    {
        var symbol = transformer.getSymbolForSelectorName(node.name);
        return Tree.makeSelectorPiece(symbol);
    }


    function handleAtSelectorDirective(node)
    {
        var name = generator.getSymbolForSelectorName(node.name);

        if (knownSelectors && !knownSelectors[node.name]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + node.selectorName + "'", node));
        }
    }

    function replaceEnumDeclaration(node)
    {
        var inDeclarationArray  = node.declarations;
        var length = inDeclarationArray ? inDeclarationArray.length : 0;

        if (length) {
            var piece = Tree.makeVariableDeclarationPiece();

            var outDeclarationArray = piece.declarations;
            var inDeclaration, outDeclaration, i;

            for (i = 0; i < length; i++) {
                inDeclaration  = inDeclarationArray[i];
                outDeclaration = Tree.makeNumericVariableDeclaratorPiece(inDeclaration.id.name, inDeclaration.enumValue).result;
                outDeclarationArray.push(outDeclaration);
            }

            return piece.result;

        } else {
            return Traverser.RemoveNode;
        }
    }

    function replaceConstDeclaration(node)
    {
        var length = node.declarations ? node.declarations.length : 0;
        var values = [ ];

        if (length) {
            var firstDeclaration = node.declarations[0];
            modifier.from(node).to(firstDeclaration.id).replace("var ");

        } else {
            return Traverser.RemoveNode;
        }
    }

    function handleAtCastExpression(node)
    {
        var before = "(";
        modifier.from(node).to(node.argument).replace(before);
        modifier.from(node.argument).to(node).replace(")");
    }

    function handleEachStatement(node)
    {
        var scope  = node.oj_scope;
        var i      = scope.makeInternalVariable();
        var length = scope.makeInternalVariable();

        var object;
        var initLeft = "var ";
        var initRight = "";
        var expr = false;

        // The left side is "var foo", "let foo", etc
        if (node.left.type == Syntax.VariableDeclaration) {
            object = node.left.declarations[0].id.name;
            initLeft  += object + ", ";

        // The left side is just an identifier
        } else if (node.left.type == Syntax.Identifier) {
            if (currentClass && currentClass.isIvar(node.left.name)) {
                Utils.throwError(OJError.CannotUseIvarHere, "Cannot use ivar \"" + node.left.name + "\" on left-hand side of @each", node);
            }

            object = node.left.name;
        }

        // The right side is a simple identifier
        if (node.right.type == Syntax.Identifier && currentClass && !currentClass.isIvar(node.right.name)) {
            array = node.right.name;

        // The right side is an expression, we need an additional variable
        } else {
            array = scope.makeInternalVariable();
            initLeft  += array + " = (";
            initRight = initRight + "), ";
            expr = true;
        }

        initRight += i + " = 0, " + length + " = " + array + ".length";

        var test      = "(" + i + " < " + length + ") && (" + object + " = " + array + "[" + i + "])";
        var increment = i + "++";

        if (expr) {
            modifier.from(node).to(node.right).replace("for (" + initLeft);
            modifier.from(node.right).to(node.body).replace(initRight + "; " + test + "; " + increment + ") ");
        } else {
            modifier.from(node).to(node.body).replace("for (" + initLeft + initRight + "; " + test + "; " + increment + ") ");
        }
    }

    function checkThis(thisNode, path)
    {
        var inFunction = false;
        var inMethod   = true;

        for (var i = path.length - 1; i >= 0; i--) {
            var node = path[i];

            if (node.type == Syntax.OJMethodDefinition ||
                node.type == Syntax.OJClassImplementation ||
                node.type == Syntax.OJMessageExpression)
            {
                warnings.push(Utils.makeError(OJWarning.UseOfThisInMethod, "Use of 'this' keyword in oj method definition", thisNode));

            } else if (node.type == Syntax.FunctionDeclaration ||
                       node.type == Syntax.FunctionExpression) {
                break;
            }
        }
    }

    traverser.replace(function(node, parent) {
        var type = node.type;

        if (type === Syntax.OJProtocolDefinition                 ||
            type === Syntax.OJAtClassDirective                   ||
            type === Syntax.OJAtSqueezeDirective                 ||
            type === Syntax.OJInstanceVariableDeclarations       ||
            type === Syntax.OJAtSynthesizeDirective              ||
            type === Syntax.OJAtDynamicDirective                 ||
            type === Syntax.OJAtTypedefDeclaration               ||
          ((type === Syntax.OJEnumDeclaration)  && removeEnums)  ||
          ((type === Syntax.OJConstDeclaration) && removeConsts) ||
          ((type === Syntax.OJTypeAnnotation)   && removeTypes)
        ) {
            return Traverser.RemoveNode;

        } else if (type === Syntax.OJClassImplementation) {
            currentClass = model.classes[node.id.name];

            if (optionWarnOnUnusedIvars) {
                unusedIvars = currentClass.getAllIvarNamesWithoutProperties();
            }

        } else if (type === Syntax.OJMethodDefinition) {
            currentMethodNode        = node;
            methodUsesSelfVar        = false;
            methodUsesTemporaryVar   = false;
            methodUsesLoneExpression = false;

        } else if (type === Syntax.OJAtCastExpression) {
            handleAtCastExpression(node);

        } else if (type === Syntax.OJAtEachStatement) {
            handleEachStatement(node);

        } else if (type === Syntax.ThisExpression) {
            if (optionWarnOnThisInMethods) {
                checkThis(node, traverser.getParents());
            }

        } else if (type === Syntax.OJAtSelectorDirective) {
            handleAtSelectorDirective(node);

        } else if (type === Syntax.AssignmentExpression) {
            if (currentMethodNode &&
                node.left &&
                node.left.type == Syntax.Identifier &&
                node.left.name == "self")
            {
                methodUsesSelfVar = true;
            }

        } else if (type === Syntax.FunctionDeclaration || type === Syntax.FunctionExpression) {
            methodUsesSelfVar = true;
        }

    }, function(node, parent) {
        var type = node.type;
        var replacement;

        if (type === Syntax.OJClassImplementation) {
            replacement = replaceClassImplementation(node);

            currentClass = null;

            if (optionWarnOnUnusedIvars && unusedIvars && unusedIvars.length) {
                _.each(unusedIvars, function(unusedIvar) {
                    warnings.push(Utils.makeError(OJWarning.UnusedInstanceVariable, "Unused instance variable " + unusedIvar, node));
                });

                unusedIvars = null;
            }

        } else if (type === Syntax.OJMethodDefinition) {
            replacement = replaceMethodDefinition(node);
            currentMethodNode = null;

        } else if (type == Syntax.OJMessageExpression) {
            return replaceMessageExpression(node);

        } else if (type === Syntax.OJEnumDeclaration) {
            return replaceEnumDeclaration(node);

        } else if (type === Syntax.OJConstDeclaration) {
            return replaceConstDeclaration(node);

        } else if (type === Syntax.OJAtPropertyDirective) {
            return replaceAtPropertyDirective(node);

        } else if (type === Syntax.Identifier) {
            return replaceIdentifier(node);

        } else if (type === Syntax.OJAtSelectorDirective) {
            return replaceAtSelectorDirective(node);

        } else if (type === Syntax.OJTypeAnnotation) {
            return Traverser.RemoveNode;
        }

        if (replacement === null) {
            return Traverser.RemoveNode;
        } else if (replacement) {
            return replacement;
        }
    });
}



module.exports = Transformer;
