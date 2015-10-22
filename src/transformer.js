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

var OJGlobalVariable          = "$oj_oj";
var OJClassMethodsVariable    = "$oj_s";
var OJInstanceMethodsVariable = "$oj_m";
var OJTemporaryReturnVariable = "$oj_r";
var OJSuperVariable           = "$oj_super";


// Goes away
var OJTemporaryReturnVariable = "$oj_r";


/* Tree helpers.  These build nodes */

function NullLiteral()    { return { type: "Literal", value: null,  raw: "null"  }; }
function ZeroLiteral()    { return { type: "Literal", value: 0,     raw: "0"     }; }
function FalseLiteral()   { return { type: "Literal", value: false, raw: "false" }; }
function ThisExpression() { return { type: "ThisExpression" }; }

function Identifier(n)       { return { type: "Identifier",      name:     n }; }
function ReturnStatement(a)  { return { type: "ReturnStatement", argument: a }; }
function NewExpression(c, a) { return { type: "NewExpression", "callee": c, "arguments": a || [] }; }



function VariableDeclaration(d)
{
    return { "type": "VariableDeclaration", "declarations": d || [ ], "kind": "var" };
}


function ConditionalExpression(t, c, a)
{
    return { "type": "ConditionalExpression", "test": t, "consequent": c, "alternate": a };
}

function VariableDeclarator(id, init)
{
    return { "type": "VariableDeclarator", "id": id, "init": init };
}

function MemberExpression(object, property, computed)
{
    return { "type": "MemberExpression", "computed": !!computed, "object": object, "property": property };
}

function AssignmentExpression(left, right)
{
    return { "type": "AssignmentExpression", "operator": "=", "left": left, "right": right };
}

function ExpressionStatement(node)
{
    return { "type": "ExpressionStatement", "expression": node };
}


function makeClassPiece(classSymbol)
{
    return MemberExpression(
        MemberExpression( Identifier(OJGlobalVariable), Identifier("_cls") ),
        Identifier(classSymbol)
    );
}


// { NAME: 1 }
//
function makeSelectorPiece(name)
{
    var identifier = Identifier(name);
    var one = { type: "Literal", value: 1, raw: "1" };

    if (Utils.isJScriptReservedWord(name)) {
        identifier.raw = '"' + name + '"';
    }

    return {
        type: "ObjectExpression",
        properties: [{ type: "Property", key: identifier, value: one, kind: "init" }]
    };
}


function makeNumericPiece(value)
{
    if (value < 0) {
        return {
            type: "UnaryExpression",
            operator: "-",
            argument: { "type": "Literal", "value": -value, "raw": ('"' + (-value) + '"') },
            prefix: true
        };

    } else {
        return { type: "Literal", value: value, raw: ('"' + value + '"') };
    }
}


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

        var messageSelectors  = node.messageSelectors;
        var receiverContainer = node.receiver;
        var receiverNode      = receiverContainer.value;
        var argumentNodes     = [ ];

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
                receiverNode = makeClassPiece(classSymbol);
            }

            replacementPiece = Tree.makeMsgSendPiece();
            replacementPiece.arguments.push(receiverNode);
            replacementPiece.arguments.push(makeSelectorPiece(methodSymbol));

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
                    return NewExpression( makeClassPiece(classSymbol) );
                } else {
                    receiverNode = makeClassPiece(classSymbol);
                    replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, false);
                }

            } else if (receiverName == "self") {
                receiverNode = replaceIdentifier(receiverNode, receiverContainer);
                replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, false);

            } else if (inClass && currentClass.isIvar(receiverName)) {
                methodUsesLoneExpression = true;

                receiverNode = Tree.makeIvarPiece(methodUsesSelfVar, getSymbolForClassNameAndIvarName(currentClass.name, receiverName)).result;
                replacementPiece = Tree.makeCallToReceiverPiece(receiverNode, methodSymbol, true);

            } else {
                methodUsesLoneExpression = true;
                receiverNode = replaceIdentifier(receiverNode, receiverContainer);
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

        piece.arguments[0] = makeSelectorPiece(getSymbolForClassName(className));

        function pushInitChain(ivars, valueNode) {
            if (!ivars.length) return;

            var currentRight = valueNode;
            for (var i = ivars.length - 1; i >= 0; i--) {
                var symbol = getSymbolForClassNameAndIvarName(className, ivars[i]);

                currentRight = AssignmentExpression(
                    MemberExpression(ThisExpression(), Identifier(symbol)),
                    currentRight
                );
            }

            toAppend.push( ExpressionStatement(currentRight) );
        }

        if (superName) {
            var superSymbol = getSymbolForClassName(superName);
            piece.arguments[1] = makeSelectorPiece(superSymbol);
            toAppend.push( ExpressionStatement( Tree.makeConstructorCallSuperPiece(superSymbol).result ) );
        } else {
            piece.arguments[1] = NullLiteral();
        }

        var ivarMap = currentClass.getTypeToSortedIvarNameMap();
        pushInitChain(ivarMap.object,  NullLiteral()  );
        pushInitChain(ivarMap.number,  ZeroLiteral()  );
        pushInitChain(ivarMap.boolean, FalseLiteral() );



        var constructorPiece = Tree.makeConstructorPiece(className);

        piece.body.push(constructorPiece.result);

        unshiftContents( constructorPiece.body, toAppend );

        // It's important that we flatten() here, as replaceAtPropertyDirective() can return
        // multiple nodes, resulting in an invalid AST
        //
        // See: https://github.com/estools/estraverse/issues/38
        // 
        pushContents( piece.body, _.flatten(inNode.body.body) );

        piece.body.push( ReturnStatement( Identifier(className) ) );

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

            result.push( ExpressionStatement(node) );
        }

        if (makeGetter) {
            var getterSymbol = getSymbolForSelectorName(property.getter);
            var node = Tree.makePropertyGetterPiece(getterSymbol, ivarSymbol).result;

            result.push( ExpressionStatement(node) );
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
                params.push( Identifier(variableName.name) );
            }
        }

        if (methodUsesSelfVar || methodUsesTemporaryVar) { //  || methodUsesLoneExpression) {
            var declarations = [ ];

            if (methodUsesSelfVar) {
                declarations.push(VariableDeclarator(
                    Identifier("self"),
                    ThisExpression()
                ))
            }

            if (methodUsesTemporaryVar) {
                declarations.push(VariableDeclarator(
                    Identifier(OJTemporaryReturnVariable),
                    null
                ))
            }

            //!i: FIX?
            // if (methodUsesLoneExpression) {
            //     string += "/* jshint expr: true */";
            // }

            piece.body.push(VariableDeclaration(declarations));
        }

        Array.prototype.push.apply(piece.body, node.body.body);

        return ExpressionStatement(piece.result);
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
                    return Identifier(result);
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

        return makeSelectorPiece(symbol);
    }


    function replaceEnumDeclaration(node)
    {
        var inDeclarationArray  = node.declarations;
        var length = inDeclarationArray ? inDeclarationArray.length : 0;

        if (length) {
            var outDeclarationArray = [ ];
            var inDeclaration, i;

            for (i = 0; i < length; i++) {
                inDeclaration = inDeclarationArray[i];
                outDeclarationArray.push( VariableDeclarator(
                    inDeclaration.id,
                    makeNumericPiece(inDeclaration.enumValue)
                ));
            }

            return VariableDeclaration(outDeclarationArray);

        } else {
            return Traverser.RemoveNode;
        }
    }


    function replaceAtEachStatement(node)
    {
        var scope  = node.oj_scope;
        var iVariable      = scope.makeInternalVariable();
        var lengthVariable = scope.makeInternalVariable();
        var arrayVariable, objectVariable;
        var declaration;
    
        // The left side is "var foo", "let foo", etc
        if (node.left.type == Syntax.VariableDeclaration) {
            objectVariable = node.left.declarations[0].id.name;
            declaration = node.left;

        // The left side is just an identifier
        } else if (node.left.type == Syntax.Identifier) {
            if (currentClass && currentClass.isIvar(node.left.name)) {
                Utils.throwError(OJError.CannotUseIvarHere, "Cannot use ivar \"" + node.left.name + "\" on left-hand side of @each", node);
            }

            objectVariable = node.left.name;
            declaration = VariableDeclaration();
        }

        // The right side is a simple identifier
        if (node.right.type == Syntax.Identifier && currentClass && !currentClass.isIvar(node.right.name)) {
            arrayVariable = node.right.name;

        // The right side is an expression, we need an additional variable
        } else {
            arrayVariable = scope.makeInternalVariable();

            declaration.declarations.push(VariableDeclarator(
                Identifier(arrayVariable),
                node.right
            ));
        }

        declaration.declarations.push(
            VariableDeclarator( Identifier(iVariable),      ZeroLiteral() ),
            VariableDeclarator( Identifier(lengthVariable), ConditionalExpression(
                Identifier(arrayVariable),
                MemberExpression(Identifier(arrayVariable), Identifier("length") ),
                ZeroLiteral()
            ) )
        );

        return {
            type: "ForStatement",
            init: declaration,
            test: {
                type: "LogicalExpression",
                operator: "&&",
                left: {
                    "type": "BinaryExpression",
                    "operator": "<",
                    "left":  Identifier(iVariable),
                    "right": Identifier(lengthVariable)
                },
                right: AssignmentExpression(
                    Identifier(objectVariable),
                    MemberExpression(
                        Identifier(arrayVariable),
                        Identifier(iVariable),
                        true
                    )
                )
            },
            update: {
                type: "UpdateExpression",
                operator: "++",
                prefix: false,
                argument: Identifier(iVariable)
            },
            body: node.body
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
            if (parent.type != Syntax.OJMessageReceiver) {
                replacement = replaceIdentifier(node, parent);
            }

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
