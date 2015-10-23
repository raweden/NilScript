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

function NullLiteral()              { return { type: "Literal", value: null,  raw: "null"  }; }
function ZeroLiteral()              { return { type: "Literal", value: 0,     raw: "0"     }; }
function FalseLiteral()             { return { type: "Literal", value: false, raw: "false" }; }
function ThisExpression()           { return { type: "ThisExpression" }; }
function Identifier(n)              { return { type: "Identifier", name: n }; }
function BlockStatement(b)          { return { type: "BlockStatement", body: b }; }
function ReturnStatement(a)         { return { type: "ReturnStatement", argument: a }; }
function NewExpression(c, a)        { return { type: "NewExpression",  callee: c, arguments: a || [ ] }; }
function CallExpression(c, a)       { return { type: "CallExpression", callee: c, arguments: a || [ ] }; }
function ExpressionStatement(n)     { return { type: "ExpressionStatement", expression: n };
function LogicalExpression(l, o, r) { return { type: "LogicalExpression", operator: o, left: l, right: r }; }
function VariableDeclarator(id, i)  { return { type: "VariableDeclarator", id: id, init: i };
function FunctionExpression(p, b)   { return { type: "FunctionExpression", id: null, params: p || [ ], defaults: [ ], body: b }; }
function VariableDeclaration(d)     { return { type: "VariableDeclaration", declarations: d || [ ], kind: "var" }; }
function AssignmentExpression(l, r) { return { type: "AssignmentExpression", operator: "=", left: l, right: r }; }

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
        if (!inMethodNode) {
            if ((receiverNode.type == Syntax.Identifier) && model.classes[receiverNode.name]) {
                classSymbol = getSymbolForClassName(receiverNode.name);
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
                classSymbol = getSymbolForClassName(receiverName);

                if (selectorName == "alloc") {
                    return NewExpression( makeClassPiece(classSymbol) );
                } else {
                    receiverNode = makeClassPiece(classSymbol);
                    replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, false);
                }

            } else if (receiverName == "self") {
                receiverNode = replaceIdentifier(receiverNode, receiverContainer);
                replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, false);

            } else if (inClass && currentClass.isIvar(receiverName)) {
                methodUsesLoneExpression = true;

                receiverNode = makeIvarPiece(methodUsesSelfVar, getSymbolForClassNameAndIvarName(currentClass.name, receiverName));
                replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, true);

            } else {
                methodUsesLoneExpression = true;
                receiverNode = replaceIdentifier(receiverNode, receiverContainer);
                replacement = makeCallToReceiverPiece(receiverNode, methodSymbol, argumentNodes, true);
            }

        } else if (inMethodNode) {
            methodUsesTemporaryVar   = true;
            methodUsesLoneExpression = true;

            // ($oj_r = <receiverNode>) && $oj_r.<methodSymbol>(<argumentNodes>)
            //
            replacement = LogicalExpression(
                AssignmentExpression( Identifier(OJTemporaryReturnVariable), receiverNode ),
                "&&",
                CallExpression(
                    MemberExpression( Identifier(OJTemporaryReturnVariable), Identifier(methodSymbol) ),
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

        var registerBody    = [ ];
        var constructorBody = [ ];

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

            constructorBody.push( ExpressionStatement(currentRight) );
        }

        // Determine _registerClass's 0th argument
        var registerArg0 = makeSelectorPiece(getSymbolForClassName(className));

        // Determine _registerClass's 1th argument
        var registerArg1;
        if (superName) {
            var superSymbol = getSymbolForClassName(superName);
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

        // It's important that we flatten() here, as replaceAtPropertyDirective() can return
        // multiple nodes, resulting in an invalid AST
        //
        // See: https://github.com/estools/estraverse/issues/38
        // 
        pushContents( registerBody, _.flatten(inNode.body.body) );

        registerBody.push( ReturnStatement( Identifier(className) ) );

        // function($oj_s, $oj_m) { <registerBody> }
        var registerArg2 = FunctionExpression(
            [ Identifier(OJClassMethodsVariable), Identifier(OJInstanceMethodsVariable) ],
            BlockStatement(registerBody)
        );

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
            // $oj_m.setterSymbol = function(arg) { this.ivarSymbol = arg; }
            //
            result.push( ExpressionStatement(
                AssignmentExpression(
                    MemberExpression(
                        Identifier(OJInstanceMethodsVariable),
                        Identifier(getSymbolForSelectorName(property.setter))
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
                        Identifier( getSymbolForSelectorName(property.getter) )
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
        var variableName;

        for (var i = 0, length = node.methodSelectors.length; i < length; i++) {
            variableName = node.methodSelectors[i].variableName;

            if (variableName) {
                paramNodes.push( Identifier(variableName.name) );
            }
        }

        if (methodUsesSelfVar || methodUsesTemporaryVar) { //  || methodUsesLoneExpression) {
            var declarations = [ ];

            if (methodUsesSelfVar) {
                declarations.push(VariableDeclarator(
                    Identifier("self"),
                    ThisExpression()
                ));
            }

            if (methodUsesTemporaryVar) {
                declarations.push(VariableDeclarator(
                    Identifier(OJTemporaryReturnVariable),
                    null
                ));
            }

            //!i: FIX?
            // if (methodUsesLoneExpression) {
            //     string += "/* jshint expr: true */";
            // }

            bodyNodes.push(VariableDeclaration(declarations));
        }

        Array.prototype.push.apply(bodyNodes, node.body.body);

        return ExpressionStatement(
            AssignmentExpression(
                MemberExpression( 
                    Identifier( node.selectorType == "+" ? OJClassMethodsVariable : OJInstanceMethodsVariable ),
                    Identifier( getSymbolForSelectorName(node.selectorName) )
                ),
                FunctionExpression(paramNodes, BlockStatement(bodyNodes))
            )
        );
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
                    replacement = makeIvarPiece(usesSelf, null);
                } else {
                    var ivarSymbol = getSymbolForClassNameAndIvarName(currentClass.name, name);
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
                    return Identifier(result);
                }
            }
        }

        //!i: FIX: squeeze needs to work
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
