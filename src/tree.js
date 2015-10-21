/*
    Tree.js
    (c) 2015 musictheory.net, LLC
    MIT license, http://www.opensource.org/licenses/mit-license.php

    Makes AST nodes for use by transformer.js

    Naming conventions:
    "get..."
    Returns a single AST node
    
    "wrap..."
    Wraps the passed node in a parent node, returns the parent

    "make...Piece"
    For a hierarchy of nodes.  Returns an object.  The "result" key of 
    the object will always refer to the top of the hierarchy.
*/

var Utils = require("./utils");
var _     = require("lodash");


var OJGlobalVariable          = "$oj_oj";
var OJClassMethodsVariable    = "$oj_s";
var OJInstanceMethodsVariable = "$oj_m";
var OJTemporaryReturnVariable = "$oj_r";
var OJSuperVariable           = "$oj_super";


var Tree = { };


function getNullLiteral()
{
    return { "type": "Literal", "value": null, "raw": "null" };
}
Tree.getNullLiteral = getNullLiteral;


function getZeroLiteral()
{
    return { "type": "Literal", "value": 0, "raw": "0" };
}
Tree.getZeroLiteral = getZeroLiteral;


function getFalseLiteral()
{
    return { "type": "Literal", "value": false, "raw": "false" };
}
Tree.getFalseLiteral = getFalseLiteral;


function getThisExpression()
{
    return { "type": "ThisExpression" };
}
Tree.getThisExpression = getThisExpression;


function getNumericLiteral(value)
{
    if (value < 0) {
        return {
            "type": "UnaryExpression",
            "operator": "-",
            "argument": { "type": "Literal", "value": -value, "raw": ('"' + (-value) + '"') },
            "prefix": true
        };

    } else {
        return { "type": "Literal", "value": value, "raw": ('"' + value + '"') };
    }
}


function getIdentifier(name)
{
    return { "type": "Identifier", "name": name };
}
Tree.getIdentifier = getIdentifier;


function wrapInNew(node)
{
    return {
        "type": "NewExpression",
        "callee": node,
        "arguments": []
    };
}
Tree.wrapInNew = wrapInNew;


function wrapInVariableDeclarator(id, init)
{
    return {
        "type": "VariableDeclarator",
        "id": id,
        "init": init
    };
}
Tree.wrapInVariableDeclarator = wrapInVariableDeclarator;


function wrapInExpressionStatement(node)
{
    return {
        "type": "ExpressionStatement",
        "expression": node
    };
}
Tree.wrapInExpressionStatement = wrapInExpressionStatement;


function wrapInReturn(node)
{
    return {
        "type": "ReturnStatement",
        "argument": node
    }
}
Tree.wrapInReturn = wrapInReturn;


// { NAME: 1 }
//
function makeSelectorPiece(name)
{
    var keyPiece;

    var result = {
        "type": "ObjectExpression",
        "properties": [{
            "type": "Property",
            "key":   (keyPiece = { "type": "Identifier", "name": name }),
            "value": { "type": "Literal", "value": 1, "raw": "1" },
            "kind": "init"
        }]
    };

    if (Utils.isJScriptReservedWord(name)) {
        keyPiece.raw = '"' + name + '"';
    }

    return { result: result };
}
Tree.makeSelectorPiece = makeSelectorPiece;


// var NAME = $oj_oj._registerClass(null, null, function($oj_s, $oj_m) { })
//
function makeRegisterClassPiece(name)
{
    var argumentsArray, bodyArray;

    var result = {
        "type": "VariableDeclaration",
        "declarations": [{
            "type": "VariableDeclarator",
            "id": { "type": "Identifier", "name": name },
            "init": {
                "type": "CallExpression",
                "callee": {
                    "type": "MemberExpression",
                    "computed": false,
                    "object": {
                        "type": "Identifier",
                        "name": OJGlobalVariable
                    },
                    "property": {
                        "type": "Identifier",
                        "name": "_registerClass"
                    }
                },
                "arguments": (argumentsArray = [ null, null, {
                    "type": "FunctionExpression",
                    "id": null,
                    "params": [
                        { "type": "Identifier", "name": OJClassMethodsVariable    },
                        { "type": "Identifier", "name": OJInstanceMethodsVariable }
                    ],
                    "defaults": [],
                    "body": {
                        "type": "BlockStatement",
                        "body": (bodyArray = [])
                    }
                }])
            }
        }],
        "kind": "var"
    };

    return {
        result: result,
        arguments: argumentsArray,
        body: bodyArray
    }
}
Tree.makeRegisterClassPiece = makeRegisterClassPiece;


// function NAME() { this.constructor = NAME; this.$oj_id = ++$oj_oj._id;}
//
function makeConstructorPiece(name)
{
    var bodyArray;

    var result = {
        "type": "FunctionDeclaration",
        "id": { "type": "Identifier", "name": name },
        "params": [],
        "defaults": [],
        "body": {
            "type": "BlockStatement",
            "body": (bodyArray = [{
                "type": "ExpressionStatement",
                "expression": {
                    "type": "AssignmentExpression",
                    "operator": "=",
                    "left": {
                        "type": "MemberExpression",
                        "computed": false,
                        "object": { "type": "ThisExpression" },
                        "property": { "type": "Identifier", "name": "constructor" }
                    },
                    "right": { "type": "Identifier", "name": name }
                }
            }, {
                "type": "ExpressionStatement",
                "expression": {
                    "type": "AssignmentExpression",
                    "operator": "=",
                    "left": {
                        "type": "MemberExpression",
                        "computed": false,
                        "object":   { "type": "ThisExpression" },
                        "property": { "type": "Identifier", "name": "$oj_id" }
                    },
                    "right": {
                        "type": "UpdateExpression",
                        "operator": "++",
                        "argument": {
                            "type": "MemberExpression",
                            "computed": false,
                            "object":   { "type": "Identifier", "name": OJGlobalVariable },
                            "property": { "type": "Identifier", "name": "_id" }
                        },
                        "prefix": true
                    }
                }
            }])
        },
        "rest": null,
    };

    return {
        result: result,
        body: bodyArray
    }
}
Tree.makeConstructorPiece = makeConstructorPiece;



// $oj_oj._cls.SYMBOL.call(this)
//
function makeConstructorCallSuperPiece(symbol)
{
    return { result: {
        "type": "CallExpression",
        "callee": {
            "type": "MemberExpression",
            "object": {
                "type": "MemberExpression",
                "object": {
                    "type": "MemberExpression",
                    "object":   { "type": "Identifier", "name": OJGlobalVariable },
                    "property": { "type": "Identifier", "name": "_cls" }
                },
                "property": { "type": "Identifier", "name": symbol }
            },
            "property": { "type": "Identifier", "name": "call" }
        },
        "arguments": [{ "type": "ThisExpression" }]
    }};
}
Tree.makeConstructorCallSuperPiece = makeConstructorCallSuperPiece;


// this.SYMBOL = false|0|null
//
function makeConstructorInitIvarPiece(symbol, value)
{
    var right;

    if (value === 0) {
        right = getZeroLiteral();
    } else if (value === false) {
        right = getFalseLiteral();
    } else {
        right = getNullLiteral();
    }

    return { result: {
        "type": "AssignmentExpression",
        "operator": "=",
        "left": {
            "type": "MemberExpression",
            "computed": false,
            "object":   { "type": "ThisExpression" },
            "property": { "type": "Identifier", "name": symbol }
        },
        "right": right
    }};
}
Tree.makeConstructorInitIvarPiece = makeConstructorInitIvarPiece;


// return NAME;
//
function makeReturnIdentifierPiece(name)
{
    return { result: {
        "type": "ReturnStatement",
        "argument": { "type": "Identifier", "name": name }
    }};
}
Tree.makeReturnIdentifierPiece = makeReturnIdentifierPiece;


// $oj_m.METHOD_SYMBOL = function(arg) { this.IVAR_SYMBOL = arg; }
//
function makePropertySetterPiece(methodSymbol, ivarSymbol)
{
    return { result: {
        "type": "AssignmentExpression",
        "operator": "=",
        "left": {
            "type": "MemberExpression",
            "object":   { "type": "Identifier", "name": OJInstanceMethodsVariable },
            "property": { "type": "Identifier", "name": methodSymbol }
        },
        "right": {
            "type": "FunctionExpression",
            "id": null,
            "params": [{ "type": "Identifier", "name": "arg" }],
            "body": {
                "type": "BlockStatement",
                "body": [{
                    "type": "ExpressionStatement",
                    "expression": {
                        "type": "AssignmentExpression",
                        "operator": "=",
                        "left": {
                            "type": "MemberExpression",
                            "object": { "type": "ThisExpression" },
                            "property": { "type": "Identifier", "name": ivarSymbol }
                        },
                        "right": { "type": "Identifier", "name": "arg" }
                    }
                }]
            },
        }
    } };
}
Tree.makePropertySetterPiece = makePropertySetterPiece;


// $oj_m.METHOD_SYMBOL = function() { return this.IVAR_SYMBOL; }
//
function makePropertyGetterPiece(methodSymbol, ivarSymbol)
{
    return { result: {
        "type": "AssignmentExpression",
        "operator": "=",
        "left": {
            "type": "MemberExpression",
            "object":   { "type": "Identifier", "name": OJInstanceMethodsVariable },
            "property": { "type": "Identifier", "name": methodSymbol }
        },
        "right": {
            "type": "FunctionExpression",
            "id": null,
            "params": [],
            "body": {
                "type": "BlockStatement",
                "body": [{
                    "type": "ReturnStatement",
                    "argument": {
                        "type": "MemberExpression",
                        "object":   { "type": "ThisExpression" },
                        "property": { "type": "Identifier", "name": ivarSymbol }
                    }
                }]
            }
        }
    } }
}
Tree.makePropertyGetterPiece = makePropertyGetterPiece;


function makeMethodDeclarationPiece(isClassMethod, methodSymbol)
{
    var where = (isClassMethod ? OJClassMethodsVariable : OJInstanceMethodsVariable);
    var body;
    var params;

    var result = {
        "type": "AssignmentExpression",
        "operator": "=",
        "left": {
            "type": "MemberExpression",
            "object":   { "type": "Identifier", "name": where        },
            "property": { "type": "Identifier", "name": methodSymbol }
        },
        "right": {
            "type": "FunctionExpression",
            "id": null,
            "params": (params = []),
            "body": {
                "type": "BlockStatement",
                "body": (body = [])
            }
        }
    };

    return {
        result: result,
        params: params,
        body: body
    }
}
Tree.makeMethodDeclarationPiece = makeMethodDeclarationPiece;


// $oj_oj._cls.CLASS_SYMBOL
//
function makeClassPiece(classSymbol)
{
    return { result: {
        "type": "MemberExpression",
        "computed": false,
        "object": {
            "type": "MemberExpression",
            "computed": false,
            "object":   { "type": "Identifier", "name": OJGlobalVariable },
            "property": { "type": "Identifier", "name": "_cls"           }
        },
        "property": { "type": "Identifier", "name": classSymbol }
    }};
}
Tree.makeClassPiece = makeClassPiece;


// CLASS_NAME.$oj_super.METHOD_SYMBOL.call(this)
//
function makeCallSuperClassMethodPiece(className, methodSymbol)
{
    var argumentsArray;

    var result = {
        "type": "CallExpression",
        "callee": {
            "type": "MemberExpression",
            "object": {
                "type": "MemberExpression",
                "object": {
                    "type": "MemberExpression",
                    "object":   { "type": "Identifier", "name": className },
                    "property": { "type": "Identifier", "name": OJSuperVariable }
                },
                "property": { "type": "Identifier", "name": methodSymbol }
            },
            "property": { "type": "Identifier", "name": "call" }
        },
        "arguments": (argumentsArray = [{ "type": "ThisExpression" }])
    };

    return {
        result: result,
        arguments: argumentsArray
    }
}
Tree.makeCallSuperClassMethodPiece = makeCallSuperClassMethodPiece;


// CLASS_NAME.$oj_super.prototype.METHOD_SYMBOL.call(this)
//
function makeCallSuperInstanceMethodPiece(className, methodSymbol)
{
    var argumentsArray;

    var result = {
        "type": "CallExpression",
        "callee": {
            "type": "MemberExpression",
            "object": {
                "type": "MemberExpression",
                "object": {
                    "type": "MemberExpression",
                    "object": {
                        "type": "MemberExpression",
                        "object":   { "type": "Identifier", "name": className },
                        "property": { "type": "Identifier", "name": OJSuperVariable }
                    },
                    "property": { "type": "Identifier", "name": "prototype" }
                },
                "property": { "type": "Identifier", "name": methodSymbol }
            },
            "property": { "type": "Identifier", "name": "call" }
        },
        "arguments": (argumentsArray = [{ "type": "ThisExpression" }])
    };

    return {
        result: result,
        arguments: argumentsArray
    }
}
Tree.makeCallSuperInstanceMethodPiece = makeCallSuperInstanceMethodPiece;



// self.METHOD_SYMBOL() or this.METHOD_SYMBOL()
//
function makeCallToSelfPiece(usesSelfVariable, methodSymbol)
{
    var object;

    if (usesSelfVariable) {
        object = { "type": "Identifier", "name": "self" };
    } else {
        object = { "type": "ThisExpression" };
    }    

    var argumentsArray;

    var result = {
        "type": "CallExpression",
        "callee": {
            "type": "MemberExpression",
            "object":   object,
            "property": { "type": "Identifier", "name": methodSymbol }
        },
        "arguments": (argumentsArray = [])
    };

    return {
        result: result,
        arguments: argumentsArray
    }
}
Tree.makeCallToSelfPiece = makeCallToSelfPiece;


// RECEIVER && RECEIVER.METHOD_SYMBOL()
//
function makeCallToReceiverPiece(receiverNode, methodSymbol, includeCheck)
{
    var argumentsArray;

    var result = {
        "type": "CallExpression",
        "callee": {
            "type": "MemberExpression",
            "object":   receiverNode,
            "property": { "type": "Identifier", "name": methodSymbol }
        },
        "arguments": (argumentsArray = [])
    };

    if (includeCheck) {
        receiverNode.loc = null;

        result = {
            "type": "LogicalExpression",
            "operator": "&&",
            "left": receiverNode,
            "right": result
        }
    }

    return {
        result: result,
        arguments: argumentsArray
    }
}
Tree.makeCallToReceiverPiece = makeCallToReceiverPiece;


// ($oj_r = <EXPRESSION_NODE>) && $oj_r.METHOD_SYMBOL()
//
function makeCallToExpressionPiece(expressionNode, methodSymbol)
{
    var argumentsArray;

    var result = {
        "type": "LogicalExpression",
        "operator": "&&",
        "left": {
            "type": "AssignmentExpression",
            "operator": "=",
            "left":  { "type": "Identifier", "name": OJTemporaryReturnVariable },
            "right": expressionNode
        },
        "right": {
            "type": "CallExpression",
            "callee": {
                "type": "MemberExpression",
                "object":   { "type": "Identifier", "name": OJTemporaryReturnVariable },
                "property": { "type": "Identifier", "name": methodSymbol }
            },
            "arguments": (argumentsArray = [])
        }
    };

    return {
        result: result,
        arguments: argumentsArray
    }
}
Tree.makeCallToExpressionPiece = makeCallToExpressionPiece;


// $oj_oj.msgSend()
//
function makeMsgSendPiece()
{
    var argumentsArray;

    var result = {
        "type": "CallExpression",
        "callee": {
            "type": "MemberExpression",
            "object":   { "type": "Identifier", "name": OJGlobalVariable },
            "property": { "type": "Identifier", "name": "msgSend" }
        },
        "arguments": (argumentsArray = [])
    };

    return {
        result: result,
        arguments: argumentsArray
    }
}
Tree.makeMsgSendPiece = makeMsgSendPiece;


// self.METHOD_SYMBOL, this.IVAR_SYMBOL, self, or this
//
function makeIvarPiece(usesSelfVariable, ivarSymbol)
{
    var object;

    if (usesSelfVariable) {
        object = { "type": "Identifier", "name": "self" };
    } else {
        object = { "type": "ThisExpression" };
    }    

    if (ivarSymbol) {
        return { result: {
            "type": "MemberExpression",
            "object":   object,
            "property": { "type": "Identifier", "name": ivarSymbol }
        }};

    } else {
        return { result: object };
    }
}
Tree.makeIvarPiece = makeIvarPiece;


function makeVariableDeclarationPiece()
{
    var declarations;

    var result = {
        "type": "VariableDeclaration",
        "declarations": (declarations = [ ]),
        "kind": "var"
    };

    return {
        result: result,
        declarations: declarations
    }
}
Tree.makeVariableDeclarationPiece = makeVariableDeclarationPiece;


function makeNumericVariableDeclaratorPiece(name, value)
{
    return { result: {
        "type": "VariableDeclarator",
        "id":   { "type": "Identifier", "name": name },
        "init": getNumericLiteral(value)
    } };
}
Tree.makeNumericVariableDeclaratorPiece = makeNumericVariableDeclaratorPiece;



module.exports = Tree;
