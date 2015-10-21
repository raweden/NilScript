/*
    transformer.js
    (c) 2013-2014 musictheory.net, LLC
    MIT license, http://www.opensource.org/licenses/mit-license.php
*/

var _          = require("lodash");
var esprima    = require("./esprima");
var Syntax     = esprima.Syntax;

var Traverser  = require("./traverser");
var Utils      = require("./utils");
var Tree       = require("./tree");

var OJModel    = require("./model").OJModel;
var OJError    = require("./errors").OJError;
var OJWarning  = require("./errors").OJWarning;

var OJClassPrefix             = "$oj_c_";
var OJMethodPrefix            = "$oj_f_";
var OJIvarPrefix              = "$oj_i_";

// Goes away
var OJTemporaryReturnVariable = "$oj_r";


function unshiftContents(array, array2)
{
    Array.prototype.unshift.apply(array, array2);
}


function pushContents(array, array2)
{
    Array.prototype.push.apply(array, array2);
}


function transform(ast, model, options)
{
    var warnings = [ ];
    var inlines  = { };

    var traverser = new Traverser(ast);

    var scope = model.scope;

    var currentClass      = null;
    var currentMethodNode = null;
    var inClass      = false;
    var inMethodNode = false;

    var methodUsesSelfVar        = false;
    var methodUsesTemporaryVar   = false;
    var methodUsesLoneExpression = false;

    var optionWarnOnThisInMethods    = options["warn-this-in-methods"];
    var optionWarnOnUnknownSelectors = options["warn-unknown-selectors"];
    var optionWarnOnUnusedIvars      = options["warn-unused-ivars"];
    var optionWarnOnUnknownIvars     = options["warn-unknown-ivars"];
    var optionInlineEnum             = options["inline-enum"];
    var optionInlineConst            = options["inline-const"];
    var optionSqueeze                = options["squeeze"];

    var knownSelectors = optionWarnOnUnknownSelectors ? model.selectors : null;

    (function setup()
    {
        if (optionInlineEnum) {
            _.each(model.enums, function(e) {
                var enumName = e.name;

                _.each(e.values, function(value, name) {
                    inlines[name] = value;
                });
            });
        }

        if (optionInlineConst) {
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
    }());

    function getSymbolForClassName(className) {
        if (!className) return;

        if (!Utils.isBaseObjectClass(className)) {
            if (optionSqueeze) {
                return model.getSqueezedName(OJClassPrefix + className, true);
            } else {
                return OJClassPrefix + className;
            }
        }

        return className;
    }


    function getSymbolForSelectorName(selectorName) {
        var replacedName = selectorName;
        replacedName = replacedName.replace(/_/g,   "__");
        replacedName = replacedName.replace(/^__/g, "_");
        replacedName = replacedName.replace(/\:/g,  "_");

        if (!Utils.isBaseObjectSelectorName(selectorName)) {
            if (optionSqueeze) {
                replacedName = model.getSqueezedName(OJMethodPrefix + replacedName, true);
            } else {
                replacedName = OJMethodPrefix + replacedName;
            }
        }

        return replacedName;
    }


    function getSymbolForClassNameAndIvarName(className, ivarName)
    {
        var result = OJIvarPrefix + className + "$" + ivarName;
        if (optionSqueeze) result = model.getSqueezedName(result, true);
        return result;
    }


    function replaceMessageExpression(node)
    {
        var selectorName = node.selectorName;
        var methodSymbol = getSymbolForSelectorName(selectorName);

        var messageSelectors = node.messageSelectors;
        var receiverNode     = node.receiver.value;
        var argumentNodes    = [ ];

        var classSymbol;
        var replacementPiece;
        var argumentNode;

        for (var i = 0, length = messageSelectors.length; i < length; i++) {
            argumentNode = messageSelectors[i].argument;
            if (argumentNode) argumentNodes.push(argumentNode);
        }

        if (knownSelectors && !knownSelectors[selectorName]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + selectorName + "'", node));
        }

        // Slow case, use oj.msgSend()
        if (!inMethodNode) {
            if ((receiverNode.type == Syntax.Identifier) && model.classes[receiverNode.name]) {
                classSymbol = getSymbolForClassName(receiverNode.name);
                receiverNode = Tree.makeClassPiece(classSymbol).result;
            }

            replacementPiece = Tree.makeMsgSendPiece();
            replacementPiece.arguments.push(receiverNode);
            replacementPiece.arguments.push(Tree.makeSelectorPiece(methodSymbol).result);

        // Optimized case for Syntax.ThisExpression
        } else if (receiverNode.type == Syntax.ThisExpression) {
            replacementPiece = Tree.makeCallToSelfPiece(methodUsesSelfVar, methodSymbol);

        // Optimized cases forSyntax.Identifier
        } else if (receiverNode.type == Syntax.Identifier) {
            var receiverName = receiverNode.name;

            if (!inMethodNode) {
                //!i: Throw here

                if (receiverName == "super" && receiverName == "self") {

                }
            }

            if (receiverName == "super") {
                //!i: This can probably be optimized to call the superclass symbol directly
                if (currentMethodNode.selectorType == "+") {
                    replacementPiece = Tree.makeCallSuperClassMethodPiece(currentClass.name, methodSymbol);
                } else  {
                    replacementPiece = Tree.makeCallSuperInstanceMethodPiece(currentClass.name, methodSymbol);
                }

            } else if (model.classes[receiverName]) {
                classSymbol = getSymbolForClassName(receiverName);

                if (selectorName == "alloc") {
                    replacementPiece = Tree.makeClassPiece(classSymbol);
                    return Tree.wrapInNew(replacementPiece.result);
                } else {
                    receiverNode = Tree.makeClassPiece(classSymbol).result;
                    replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, false);
                }

            } else if (receiverName == "self") {
                replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, false);

            } else if (inClass && currentClass.isIvar(receiverName)) {
                methodUsesLoneExpression = true;

                receiverNode = Tree.makeIvarPiece(methodUsesSelfVar, getSymbolForClassNameAndIvarName(currentClass.name, receiverName)).result;
                replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, true);

            } else {
                methodUsesLoneExpression = true;
                replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, true);
            }

        } else if (inMethodNode) {
            methodUsesTemporaryVar   = true;
            methodUsesLoneExpression = true;

            replacementPiece = Tree.makeCallToExpressionPiece(receiverNode, methodSymbol);
        }

        pushContents(replacementPiece.arguments, argumentNodes);

        return replacementPiece.result;
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

        piece.arguments[0] = Tree.makeSelectorPiece(getSymbolForClassName(className)).result;

        if (superName) {
            var superSymbol = getSymbolForClassName(superName);
            piece.arguments[1] = Tree.makeSelectorPiece(superSymbol).result;
            toAppend.push(Tree.wrapInExpressionStatement( Tree.makeConstructorCallSuperPiece(superSymbol).result ) );
        } else {
            piece.arguments[1] = Tree.getNullLiteral();
        }

        var ivarMap = currentClass.getTypeToSortedIvarNameMap();

        _.each(ivarMap.object, function(ivar) {
            var symbol = getSymbolForClassNameAndIvarName(className, ivar);
            var node   = Tree.makeConstructorInitIvarPiece(symbol, null).result;

            toAppend.push( Tree.wrapInExpressionStatement(node) );
        });

        _.each(ivarMap.number, function(ivar) {
            var symbol = getSymbolForClassNameAndIvarName(className, ivar);
            var node   = Tree.makeConstructorInitIvarPiece(symbol, 0).result;

            toAppend.push( Tree.wrapInExpressionStatement(node) );
        });

        _.each(ivarMap.boolean, function(ivar) {
            var symbol = getSymbolForClassNameAndIvarName(className, ivar);
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
        var ivarSymbol = getSymbolForClassNameAndIvarName(currentClass.name, property.ivar);

        var result = [ ];

        if (makeSetter) {
            var setterSymbol = getSymbolForSelectorName(property.setter);
            var node = Tree.makePropertySetterPiece(setterSymbol, ivarSymbol).result;

            result.push( Tree.wrapInExpressionStatement(node) );
        }

        if (makeGetter) {
            var getterSymbol = getSymbolForSelectorName(property.getter);
            var node = Tree.makePropertyGetterPiece(getterSymbol, ivarSymbol).result;

            result.push( Tree.wrapInExpressionStatement(node) );
        }

        return result;
    }


    function replaceMethodDefinition(node)
    {
        var methodSymbol = getSymbolForSelectorName(node.selectorName);
        var piece = Tree.makeMethodDeclarationPiece(node.selectorType == "+", methodSymbol);

        var params = piece.params;
        var variableName;

        for (var i = 0, length = node.methodSelectors.length; i < length; i++) {
            variableName = node.methodSelectors[i].variableName;

            if (variableName) {
                params.push( Tree.getIdentifier(variableName.name) );
            }
        }

        if (methodUsesSelfVar || methodUsesTemporaryVar) { //  || methodUsesLoneExpression) {
            var declarationPiece = Tree.makeVariableDeclarationPiece();

            if (methodUsesSelfVar) {
                declarationPiece.declarations.push(Tree.wrapInVariableDeclarator(
                    Tree.getIdentifier("self"),
                    Tree.getThisExpression()
                ))
            }

            if (methodUsesTemporaryVar) {
                declarationPiece.declarations.push(Tree.wrapInVariableDeclarator(
                    Tree.getIdentifier(OJTemporaryReturnVariable),
                    null
                ))
            }
            // var varParts = [ ];

            // if (methodUsesSelfVar) varParts.push("self = this");
            // if (methodUsesTemporaryVar) varParts.push();

            // if (methodUsesLoneExpression) {
            //     string += "/* jshint expr: true */";
            // }

            // if (varParts.length) {
            //     string += "var " + varParts.join(",") + ";";
            // }

            piece.body.push(declarationPiece.result);
        }

        Array.prototype.push.apply(piece.body, node.body.body);

        return Tree.wrapInExpressionStatement(piece.result);
    }


    function replaceIdentifier(node, parent)
    {
        var name = node.name;
        var replacement = node;

        if (node.loc && (name.indexOf("$oj") == 0)) {
            if (name[3] == "$" || name[3] == "_") {
                Utils.throwError(OJError.DollarOJIsReserved, "Identifiers may not start with \"$oj_\" or \"$oj$\"", node);
            }
        }

        var canBeInstanceVariableOrSelf = true;

        if (parent.type == Syntax.MemberExpression && !parent.computed) {
            canBeInstanceVariableOrSelf = (parent.object == node);
        }

        if (inMethodNode && canBeInstanceVariableOrSelf) {
            if (currentClass.isIvar(name) || name == "self") {
                var usesSelf = inMethodNode && methodUsesSelfVar;

                if (name == "self") {
                    replacement = Tree.makeIvarPiece(usesSelf, null).result;
                } else {
                    var ivarSymbol = getSymbolForClassNameAndIvarName(currentClass.name, name);
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

        return replacement;
    }


    function replaceAtSelectorDirective(node)
    {
        var symbol = getSymbolForSelectorName(node.name);

        if (knownSelectors && !knownSelectors[node.name]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + node.selectorName + "'", node));
        }

        return Tree.makeSelectorPiece(symbol).result;
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


    function replaceAtEachStatement(node)
    {
        return Traverser.RemoveNode;

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

        // if (expr) {
        //     modifier.from(node).to(node.right).replace("for (" + initLeft);
        //     modifier.from(node.right).to(node.body).replace(initRight + "; " + test + "; " + increment + ") ");
        // } else {
        //     modifier.from(node).to(node.body).replace("for (" + initLeft + initRight + "; " + test + "; " + increment + ") ");
        // }
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

        if (type === Syntax.OJProtocolDefinition           ||
            type === Syntax.OJAtClassDirective             ||
            type === Syntax.OJAtSqueezeDirective           ||
            type === Syntax.OJInstanceVariableDeclarations ||
            type === Syntax.OJAtSynthesizeDirective        ||
            type === Syntax.OJAtDynamicDirective           ||
            type === Syntax.OJAtTypedefDeclaration         ||
            type === Syntax.OJTypeAnnotation
        ) {
            return Traverser.RemoveNode;

        } else if ((type === Syntax.OJEnumDeclaration) && optionInlineEnum) {
            return Traverser.RemoveNode;

        } else if ((type === Syntax.OJConstDeclaration) && optionInlineConst) {
            return Traverser.RemoveNode;

        } else if (type === Syntax.OJClassImplementation) {
            currentClass = model.classes[node.id.name];
            inClass = true;

            if (optionWarnOnUnusedIvars) {
                unusedIvars = currentClass.getAllIvarNamesWithoutProperties();
            }

        } else if (type === Syntax.OJMethodDefinition) {
            currentMethodNode        = node;
            inMethodNode             = true;
            methodUsesSelfVar        = false;
            methodUsesTemporaryVar   = false;
            methodUsesLoneExpression = false;

        } else if (type === Syntax.ThisExpression) {
            if (optionWarnOnThisInMethods) {
                checkThis(node, traverser.getParents());
            }

        } else if (type === Syntax.AssignmentExpression) {
            if (inMethodNode &&
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
            inClass = false;

            if (optionWarnOnUnusedIvars && unusedIvars && unusedIvars.length) {
                _.each(unusedIvars, function(unusedIvar) {
                    warnings.push(Utils.makeError(OJWarning.UnusedInstanceVariable, "Unused instance variable " + unusedIvar, node));
                });

                unusedIvars = null;
            }

        } else if (type === Syntax.OJMethodDefinition) {
            replacement = replaceMethodDefinition(node);
            currentMethodNode = null;
            inMethodNode = false;

        } else if (type == Syntax.OJMessageExpression) {
            replacement = replaceMessageExpression(node);

        } else if (type === Syntax.OJEnumDeclaration) {
            replacement = replaceEnumDeclaration(node);

        } else if (type === Syntax.OJAtPropertyDirective) {
            replacement = replaceAtPropertyDirective(node);

        } else if (type === Syntax.Identifier) {
            replacement = replaceIdentifier(node, parent);

        } else if (type === Syntax.OJAtSelectorDirective) {
            replacement = replaceAtSelectorDirective(node);

        } else if (type === Syntax.OJAtEachStatement) {
            replacement = replaceAtEachStatement(node);

        } else if (type === Syntax.OJAtCastExpression) {
            replacement = node.argument;

        } else if (type === Syntax.OJConstDeclaration) {
            node.type = Syntax.VariableDeclaration;
            node.kind = "var";
        }

        return replacement;
    });

    return warnings;
}


module.exports = {
    transform: transform
};
