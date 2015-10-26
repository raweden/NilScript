/*
    transformer.js
    (c) 2013-2014 musictheory.net, LLC
    MIT license, http://www.opensource.org/licenses/mit-license.php
*/

"use strict";

const _      = require("lodash");
const Syntax = require("./esprima").Syntax;

const Traverser  = require("./traverser");
const Utils      = require("./utils");

const OJModel    = require("./model").OJModel;
const OJError    = require("./errors").OJError;
const OJWarning  = require("./errors").OJWarning;

const OJGlobalVariable          = "$oj_oj";
const OJClassMethodsVariable    = "$oj_s";
const OJInstanceMethodsVariable = "$oj_m";
const OJSuperVariable           = "$oj_super";
const OJTemporaryVariablePrefix = "$oj_t_";


/* Tree helpers.  These build nodes */

function NullLiteral()              { return { type: "Literal", value: null,  raw: "null"  }; }
function ZeroLiteral()              { return { type: "Literal", value: 0,     raw: "0"     }; }
function FalseLiteral()             { return { type: "Literal", value: false, raw: "false" }; }
function ThisExpression()           { return { type: "ThisExpression" }; }
function Identifier(n)              { return { type: "Identifier", name: n }; }
function BlockStatement(b)          { return { type: "BlockStatement", body: b }; }
function ReturnStatement(a)         { return { type: "ReturnStatement", argument: a }; }
function NewExpression(c, a)        { return { type: "NewExpression",  callee: c, arguments: a || [ ] }; }
function CallExpression(c, a)       { return { type: "CallExpression", callee: c, arguments: a }; }
function ExpressionStatement(n)     { return { type: "ExpressionStatement", expression: n }; }
function LogicalExpression(l, o, r) { return { type: "LogicalExpression", operator: o, left: l, right: r }; }
function VariableDeclarator(id, i)  { return { type: "VariableDeclarator", id: id, init: i }; }
function FunctionExpression(p, b)   { return { type: "FunctionExpression", id: null, params: p || [ ], defaults: [ ], body: b }; }
function VariableDeclaration(d)     { return { type: "VariableDeclaration", declarations: d, kind: "var" }; }
function AssignmentExpression(l, r) { return { type: "AssignmentExpression", operator: "=", left: l, right: r }; }
function SequenceExpression(e)      { return { type: "SequenceExpression", "expressions": e || [ ] }; }

function ConditionalExpression(t, c, a)
{
    return { "type": "ConditionalExpression", "test": t, "consequent": c, "alternate": a };
}

function MemberExpression(object, property, computed)
{
    return { "type": "MemberExpression", "computed": !!computed, "object": object, "property": property };
}

function UpdateExpression(a, b)
{
    var prefix = (a == "++" || a == "--");
    return { type: "UpdateExpression", operator: prefix ? a : b, argument: prefix ? b : a, prefix: prefix };
}

function OJPrefixAnnotation(wrappedNode, stringToInsert)
{
    return { type: "UpdateExpression", operator: stringToInsert, argument: wrappedNode, prefix: true };
}

function OJTypeAnnotation(wrappedNode, typeString)
{
    return { type: "UpdateExpression", operator: " : " + typeString, argument: wrappedNode, prefix: false };
}


/* Tree helpers.  These build nodes */

class Scope
{
    constructor(node, parentScope)
    {
        this.node = node;
        this.parent = parentScope;
        this.declarations = [ ];
        this.variableCount = 0;
    }

    generateVariable(needsDeclaration)
    {
        const name = OJTemporaryVariablePrefix + this.variableCount++;
        if (needsDeclaration) this.declarations.push(name);
        return name;
    }
}


// $oj_oj._cls.classSymbol
//
function makeClassPiece(classSymbol)
{
    return MemberExpression(
        MemberExpression( Identifier(OJGlobalVariable), Identifier("_cls") ),
        Identifier(classSymbol)
    );
}


// { name: 1 }
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


// receiverNode && receiverNode.methodSymbol(argumentNodes)
//
function makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, includeCheck)
{
    var result = CallExpression(
        MemberExpression(receiverNode, Identifier(methodSymbol)),
        argumentNodes
    );

    if (includeCheck) {
        receiverNode.loc = null;
        result = LogicalExpression(receiverNode, "&&", result);
    }

    return result;
}


// self.ivarSymbol, this.ivarSymbol, self, or this
//
function makeIvarPiece(usesSelfVariable, ivarSymbol)
{
    var selfOrThis = usesSelfVariable ? Identifier("self") : ThisExpression();

    if (ivarSymbol) {
        return MemberExpression(selfOrThis, Identifier(ivarSymbol));
    } else {
        return selfOrThis;
    }
}


function makeLiteralPiece(value)
{
    if (value === null) {
        return NullLiteral();
    } else if (_.isNumber(value)) {
        return makeNumericPiece(value);
    } else if (_.isString(value)) {
        return { type: "Literal", value: value, raw: ('"' + value + '"') };
    } else if (value === true || value === false) {
        return { type: "Literal", value: value, raw: value };
    } else {
        throw "Cannot make literal from " + value;
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


function transform(astArray, model, options)
{
    var warnings = [ ];
    var inlines  = { };

    var joinedAst = { "type": "Program", "body": [ ], "sourceType": "script" };

    _.each(astArray, function(inAST) {
        pushContents(joinedAst.body, inAST.body);
    });

    var traverser = new Traverser(joinedAst);

    var currentClass      = null;
    var currentMethodNode = null;
    var currentScope      = new Scope();

    var methodUsesSelfVar = false;

    const optionWarnOnThisInMethods    = options["warn-this-in-methods"];
    const optionWarnOnUnknownSelectors = options["warn-unknown-selectors"];
    const optionWarnOnUnusedIvars      = options["warn-unused-ivars"];
    const optionWarnOnUnknownIvars     = options["warn-unknown-ivars"];
    const optionInlineEnum             = options["inline-enum"];
    const optionInlineConst            = options["inline-const"];
    const isTypechecker                = options["output-language"] == "typechecker";

    const knownSelectors = optionWarnOnUnknownSelectors ? model.selectors : null;

    let unusedIvars = { };

    (function setup()
    {
        if (optionInlineEnum) {
            _.each(model.enums, function(e) {
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

        let additionalInlines = options["additional-inlines"];
        if (additionalInlines) {
            for (let key in additionalInlines) {
                if (additionalInlines.hasOwnProperty(key)) {
                    inlines[key] = additionalInlines[key];
                }
            }
        }

        if (options["output-language"] == "typechecker") {
            optionInlineEnum  = true;
            optionInlineConst = true;
            isTypechecker     = true;
        }
    }());


    function getCurrentMethodInModel()
    {
        if (!currentClass || !currentMethodNode) return null;

        var selectorType = currentMethodNode.selectorType;
        var selectorName = currentMethodNode.selectorName;

        if (selectorType == "+") {
            return currentClass.getClassMethodWithName(selectorName);
        } else {
            return currentClass.getInstanceMethodWithName(selectorName);
        }
    }


    function declareScopeVariables(scope)
    {
        return;
        let node = scope.node;
        let body = node.body;

        if (!(node.type === Syntax.FunctionDeclaration || node.type === Syntax.FunctionExpression)) {
            throw new Error("Scope's node is not a FunctionDeclaration or FunctionExpression");
        }

        if (body.type !== Syntax.BlockStatement) {
            throw new Error("Scope's node.body is not BlockStatement");
        }

        let declaration = VariableDeclaration(
            scope.declarations.map(d => VariableDeclarator(Identifier(d)) )
        );

        body = body.body;
        body.unshift(declaration);
    }

    function replaceMessageExpression(node)
    {
        var selectorName = node.selectorName;
        var methodSymbol = model.getSymbolForSelectorName(selectorName);

        var messageSelectors  = node.messageSelectors;
        var receiverContainer = node.receiver;
        var receiverNode      = receiverContainer.value;
        var argumentNodes     = [ ];

        var classSymbol;
        var replacement;
        var argumentNode;

        for (var i = 0, length = messageSelectors.length; i < length; i++) {
            argumentNode = messageSelectors[i].argument;
            if (argumentNode) argumentNodes.push(argumentNode);
        }

        if (knownSelectors && !knownSelectors[selectorName]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + selectorName + "'", node));
        }

        // Slow case, use oj.msgSend()
        if (!currentMethodNode) {
            if ((receiverNode.type == Syntax.Identifier) && model.classes[receiverNode.name]) {
                classSymbol = model.getSymbolForClassName(receiverNode.name);
                receiverNode = makeClassPiece(classSymbol);
            }

            argumentNodes.unshift(makeSelectorPiece(methodSymbol))
            argumentNodes.unshift(receiverNode);

            // $oj_oj.msgSend(argumentNodes)
            replacement = CallExpression(
                MemberExpression( Identifier(OJGlobalVariable), Identifier("msgSend")),
                argumentNodes
            );

        // Optimized case for Syntax.ThisExpression
        } else if (receiverNode.type == Syntax.ThisExpression) {

            // self.METHOD_SYMBOL() or this.METHOD_SYMBOL()
            replacement = CallExpression(
                MemberExpression(
                    methodUsesSelfVar ? Identifier("self") : ThisExpression(),
                    Identifier(methodSymbol)
                ),
                argumentNodes
            );

        // Optimized cases forSyntax.Identifier
        } else if (receiverNode.type == Syntax.Identifier) {
            var receiverName = receiverNode.name;

            if (receiverName == "super") {
                // Add this to argumentNodes
                argumentNodes.unshift(ThisExpression());

                // Depending on if this is a class method or instance method, the result will either be:
                // CLASS_NAME.$oj_super.prototype.METHOD_SYMBOL.call(argumentNodes)
                // CLASS_NAME.$oj_super.          METHOD_SYMBOL.call(argumentNodes)

                replacement = MemberExpression(Identifier(currentClass.name), Identifier(OJSuperVariable));

                if (currentMethodNode.selectorType == "-") {
                    replacement = MemberExpression(replacement, Identifier("prototype"));
                }

                replacement = CallExpression(
                    MemberExpression(MemberExpression(replacement, Identifier(methodSymbol)), Identifier("call")),
                    argumentNodes
                );

            } else if (model.classes[receiverName]) {
                classSymbol = model.getSymbolForClassName(receiverName);

                if (selectorName == "alloc") {
                    return NewExpression( makeClassPiece(classSymbol) );
                } else {
                    receiverNode = makeClassPiece(classSymbol);
                    replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, false);
                }

            } else if (receiverName == "self") {
                receiverNode = replaceIdentifier(receiverNode, receiverContainer);
                replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, false);

            } else if (currentClass && currentClass.isIvar(receiverName)) {
                receiverNode = makeIvarPiece(methodUsesSelfVar, model.getSymbolForClassNameAndIvarName(currentClass.name, receiverName));
                replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, true);

            } else {
                receiverNode = replaceIdentifier(receiverNode, receiverContainer);
                replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, true);
            }

        } else if (currentMethodNode) {
            let temporaryVariable = currentScope.generateVariable(true);

            // ($oj_temp = <receiverNode>) && $oj_temp.<methodSymbol>(<argumentNodes>)
            //
            replacement = LogicalExpression(
                AssignmentExpression( Identifier(temporaryVariable), receiverNode ),
                "&&",
                CallExpression(
                    MemberExpression( Identifier(temporaryVariable), Identifier(methodSymbol) ),
                    argumentNodes
                )
            );
        }

        return replacement;
    }


    function replaceClassImplementation(inNode)
    {
        var className  = currentClass.name;
        var superName  = currentClass.superclassName;
        var ivars      = currentClass.getAllIvars();
        var model      = currentClass.model;

        // It's important that we flatten() here, as replaceAtPropertyDirective() can return
        // multiple nodes, resulting in an invalid AST
        //
        // See: https://github.com/estools/estraverse/issues/38
        // 
        var existingBody = _.flatten(inNode.body.body);

        // Typechecker is much simpler, as we don't need to have working code
        if (isTypechecker) {
            let functionExpression = FunctionExpression( [ ], BlockStatement(existingBody) )
            currentScope.node = functionExpression;
            return ExpressionStatement(functionExpression);
        }

        var registerBody    = [ ];
        var constructorBody = [ ];

        function pushInitChain(ivars, valueNode) {
            if (!ivars.length) return;

            var currentRight = valueNode;
            for (var i = ivars.length - 1; i >= 0; i--) {
                var symbol = model.getSymbolForClassNameAndIvarName(className, ivars[i]);

                currentRight = AssignmentExpression(
                    MemberExpression(ThisExpression(), Identifier(symbol)),
                    currentRight
                );
            }

            constructorBody.push( ExpressionStatement(currentRight) );
        }

        // Determine _registerClass's 0th argument
        var registerArg0 = makeSelectorPiece(model.getSymbolForClassName(className));

        // Determine _registerClass's 1th argument
        var registerArg1;
        if (superName) {
            var superSymbol = model.getSymbolForClassName(superName);
            registerArg1 = makeSelectorPiece(superSymbol);

            // $oj_oj._cls.<superSymbol>.call(this)
            constructorBody.push( ExpressionStatement( 
                CallExpression(
                    MemberExpression(
                        MemberExpression(
                            MemberExpression( Identifier(OJGlobalVariable), Identifier("_cls") ),
                            Identifier(superSymbol)
                        ),
                        Identifier("call")
                    ),
                    [ ThisExpression() ]
                )
            ) );

        } else {
            registerArg1 = NullLiteral();
        }

        var ivarMap = currentClass.getTypeToSortedIvarNameMap();
        pushInitChain(ivarMap.object,  NullLiteral()  );
        pushInitChain(ivarMap.number,  ZeroLiteral()  );
        pushInitChain(ivarMap.boolean, FalseLiteral() );

        constructorBody.push(
            ExpressionStatement(
                AssignmentExpression( MemberExpression( ThisExpression(), Identifier("constructor") ), Identifier(className) )
            ),
            ExpressionStatement(
                AssignmentExpression(
                    MemberExpression( ThisExpression(), Identifier("$oj_id") ),
                    UpdateExpression( "++", MemberExpression( Identifier(OJGlobalVariable), Identifier("_id") ) )
                )
            )
        );

        registerBody.push({
            "type": "FunctionDeclaration",
            "id": Identifier(className),
            "params": [],
            "defaults": [],
            "body": BlockStatement(constructorBody),
            "rest": null,
        });

        pushContents( registerBody, existingBody );

        registerBody.push( ReturnStatement( Identifier(className) ) );

        // function($oj_s, $oj_m) { <registerBody> }
        var registerArg2 = FunctionExpression(
            [ Identifier(OJClassMethodsVariable), Identifier(OJInstanceMethodsVariable) ],
            BlockStatement(registerBody)
        );

        currentScope.node = registerArg2;

        // var NAME = $oj_oj._registerClass( <registerArg0>, <registerArg1>, <registerArg2> )
        return VariableDeclaration([
            VariableDeclarator(
                Identifier(className),
                CallExpression(
                    MemberExpression( Identifier(OJGlobalVariable), Identifier("_registerClass") ),
                    [ registerArg0, registerArg1, registerArg2 ]
                )
            )
        ]);
    }


    function replaceAtPropertyDirective(node)
    {
        if (isTypechecker) return Traverser.RemoveNode;

        var propertyName = node.id.name;

        var makeGetter = currentClass.shouldGenerateGetterImplementationForPropertyName(propertyName);
        var makeSetter = currentClass.shouldGenerateSetterImplementationForPropertyName(propertyName);

        if (!makeSetter && !makeGetter) {
            return Traverser.RemoveNode;
        }

        var property   = currentClass.getPropertyWithName(propertyName);
        var ivarSymbol = model.getSymbolForClassNameAndIvarName(currentClass.name, property.ivar);

        var result = [ ];

        if (makeSetter) {
            // $oj_m.setterSymbol = function(arg) { this.ivarSymbol = arg; }
            //
            result.push( ExpressionStatement(
                AssignmentExpression(
                    MemberExpression(
                        Identifier(OJInstanceMethodsVariable),
                        Identifier(model.getSymbolForSelectorName(property.setter))
                    ),
                    FunctionExpression( [ Identifier("arg") ], BlockStatement([
                        ExpressionStatement(
                            AssignmentExpression(
                                MemberExpression( ThisExpression(), Identifier(ivarSymbol) ),
                                Identifier("arg")
                            )
                        )
                    ]) )
                )
            ));
        }

        if (makeGetter) {
            // $oj_m.getterSymbol = function() { return this.ivarSymbol; }
            //
            result.push( ExpressionStatement(
                AssignmentExpression(
                    MemberExpression(
                        Identifier( OJInstanceMethodsVariable ),
                        Identifier( model.getSymbolForSelectorName(property.getter) )
                    ),
                    FunctionExpression([], BlockStatement([
                        ReturnStatement( MemberExpression( ThisExpression(), Identifier(ivarSymbol) ) )
                    ]))
                )
            ));
        }

        return result;
    }


    function replaceMethodDefinition(node)
    {
        var paramNodes = [ ];
        var bodyNodes  = [ ];

        if (isTypechecker) {
            paramNodes.push(
                OJTypeAnnotation(Identifier("self"), model.getSymbolForClassName(currentClass.name))
            );
        }

        for (let i = 0, length = node.methodSelectors.length; i < length; i++) {
            let variableName = node.methodSelectors[i].variableName;
            let methodType   = node.methodSelectors[i].methodType;

            if (variableName) {
                if (isTypechecker && methodType) {
                    paramNodes.push( OJTypeAnnotation(Identifier(variableName.name), model.getTypecheckerType(methodType.value) ) );
                } else {
                    paramNodes.push( Identifier(variableName.name) );
                }
            }
        }

        if (methodUsesSelfVar) {
            let declarations = [ ];

            if (methodUsesSelfVar) {
                declarations.push(VariableDeclarator(
                    Identifier("self"),
                    ThisExpression()
                ));
            }

            bodyNodes.push(VariableDeclaration(declarations));
        }

        Array.prototype.push.apply(bodyNodes, node.body.body);

        if (isTypechecker) {
            let returnType = model.getTypecheckerType(getCurrentMethodInModel().returnType, currentClass);
            let expression = FunctionExpression(paramNodes, OJPrefixAnnotation(BlockStatement(bodyNodes), ": " + returnType + " "));

            currentScope.node = expression;

            return expression;
 
        } else {
            let expression = FunctionExpression(paramNodes, BlockStatement(bodyNodes));

            currentScope.node = expression;

            return ExpressionStatement(
                AssignmentExpression(
                    MemberExpression( 
                        Identifier( node.selectorType == "+" ? OJClassMethodsVariable : OJInstanceMethodsVariable ),
                        Identifier( model.getSymbolForSelectorName(node.selectorName) )
                    ),
                    expression
                )
            );
        }
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

        if (currentMethodNode && canBeInstanceVariableOrSelf) {
            if (currentClass.isIvar(name) || name == "self") {
                var usesSelf = currentMethodNode && methodUsesSelfVar;

                if (name == "self") {
                    replacement = makeIvarPiece(usesSelf, null);
                } else {
                    var ivarSymbol = model.getSymbolForClassNameAndIvarName(currentClass.name, name);
                    replacement = makeIvarPiece(usesSelf, ivarSymbol);

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
                    return makeLiteralPiece(result);
                }
            }
        }

        //!i: FIX: squeeze needs to work
        if (model.squeeze) {
            var result = model.getSqueezedName(name, false);
            if (result !== undefined) {
                throw "Fix this"
//                modifier.select(node).replace("" + result);
                return;
            }
        }

        return replacement;
    }

    function replaceFunctionExpressionOrDeclaration(node)
    {
        if (isTypechecker && node.annotation) {
            node.body = OJPrefixAnnotation(node.body, ": " + node.annotation.value + " ");
        }
    }

    function replaceAtSelectorDirective(node)
    {
        var symbol = model.getSymbolForSelectorName(node.name);

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
        var iVariable      = currentScope.generateVariable(false);
        var lengthVariable = currentScope.generateVariable(false);
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
            declaration = VariableDeclaration([ ]);
        }

        // The right side is a simple identifier
        if (node.right.type == Syntax.Identifier && currentClass && !currentClass.isIvar(node.right.name)) {
            arrayVariable = node.right.name;

        // The right side is an expression, we need an additional variable
        } else {
            arrayVariable = currentScope.generateVariable(false);

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
            test: LogicalExpression(
                {
                    "type": "BinaryExpression",
                    "operator": "<",
                    "left":  Identifier(iVariable),
                    "right": Identifier(lengthVariable)
                },
                "&&",
                AssignmentExpression(
                    Identifier(objectVariable),
                    MemberExpression(
                        Identifier(arrayVariable),
                        Identifier(iVariable),
                        true
                    )
                )
            ),
            update: UpdateExpression( Identifier(iVariable), "++" ),
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
            currentScope = new Scope(node, currentScope);

            if (optionWarnOnUnusedIvars) {
                unusedIvars = currentClass.getAllIvarNamesWithoutProperties();
            }

        } else if (type === Syntax.OJMethodDefinition) {
            currentScope      = new Scope(node, currentScope);
            currentMethodNode = node;
            methodUsesSelfVar = false;

        } else if (type === Syntax.ThisExpression) {
            if (optionWarnOnThisInMethods) {
                checkThis(node, traverser.getParents());
            }

        } else if (type === Syntax.AssignmentExpression) {
            if (currentMethodNode &&
                node.left &&
                node.left.type == Syntax.Identifier &&
                node.left.name == "self")
            {
                methodUsesSelfVar = true;
            }

        } else if (type === Syntax.FunctionDeclaration || type === Syntax.FunctionExpression) {
            currentScope = new Scope(node, currentScope);
            methodUsesSelfVar = true;
        }

    }, function(node, parent) {
        var type = node.type;
        var replacement;

        if (type === Syntax.Identifier) {
            if (parent.type != Syntax.OJMessageReceiver) {
                replacement = replaceIdentifier(node, parent);
            }

        } else if (type === Syntax.FunctionExpression || type === Syntax.FunctionDeclaration) {
            replacement = replaceFunctionExpressionOrDeclaration(node);

        } else if (type === Syntax.OJClassImplementation) {
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
            replacement = replaceMessageExpression(node);

        } else if (type === Syntax.OJEnumDeclaration) {
            replacement = replaceEnumDeclaration(node);

        } else if (type === Syntax.OJAtPropertyDirective) {
            replacement = replaceAtPropertyDirective(node);

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

        if (node == currentScope.node) {
            if (currentScope.declarations.length) {
                declareScopeVariables(currentScope);
            }

            currentScope = currentScope.parent;
        }

        return replacement;
    });

    return { ast: joinedAst, warnings: warnings };
}


module.exports = {
    transform: transform
};
