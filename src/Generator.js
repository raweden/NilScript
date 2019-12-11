/*
    Generator.js
    Generates JavaScript or TypeScript from input code/AST/model
    (c) 2013-2018 musictheory.net, LLC
    Modification by Jesper Svensson @ http://www.raweden.se
    MIT license, http://www.opensource.org/licenses/mit-license.php
*/

"use strict";

const _          = require("lodash");
const esprima     = require("../ext/esprima");
const Syntax     = esprima.Syntax;

const Modifier   = require("./Modifier");
const Traverser  = require("./Traverser");
const Utils      = require("./Utils");

const OJModel    = require("./model").OJModel;
const OJError    = require("./Errors").OJError;
const OJWarning  = require("./Errors").OJWarning;
const NSRuntimeModel = require("./model").NSRuntimeModel;

const Location = require("./model/OJSymbolTyper").Location;

const OJRootVariable            = "$oj_oj";
const OJClassMethodsVariable    = "$oj_s";
const OJInstanceMethodsVariable = "$oj_m";
const OJTemporaryVariablePrefix = "$oj_t_";
const OJSuperVariable           = "$oj_super";

const OJRootWithGlobalPrefix = OJRootVariable + "._g."
const OJRootWithClassPrefix  = OJRootVariable + "._cls.";

const LanguageEcmascript5 = "ecmascript5";
const LanguageTypechecker = "typechecker";
const LanguageNone        = "none";


module.exports = class Generator {


constructor(ojFile, model, forTypechecker, options)
{
    this._file     = ojFile;
    this._model    = model;
    this._modifier = new Modifier(ojFile.contents.split("\n"), options);
    this._options  = options;

    let inlines = { };
    let symbolTyper = model.getSymbolTyper();

    let language = options["output-language"];
    if (language && language.match(/typechecker/)) {
        this._language = LanguageTypechecker;
    } else if (language && language.match(/none/)) {
        this._language = LanguageNone;
    } else {
        this._language = LanguageEcmascript5;
    }

    if (forTypechecker || (this._language == LanguageTypechecker)) {
        this._language = LanguageTypechecker;
        forTypechecker = true;

        this._strictFunctions = options["strict-functions"];
    }

    _.each(model.enums, ojEnum => {
        let enumNameSymbol = (ojEnum.name && !ojEnum.anonymous) ? symbolTyper.getSymbolForEnumName(ojEnum.name) : null;

        _.each(ojEnum.values, (value, name) => {
            if (enumNameSymbol && forTypechecker) {
                inlines[name] = enumNameSymbol + "." + name;
            } else {
                inlines[name] = value;
            }
        });
    });

    _.each(model.consts, ojConst => {
        let name = ojConst.name;

        if (inlines[name] === undefined) {
            inlines[name] = ojConst.raw;
        }
    });

    let additionalInlines = options["additional-inlines"];
    if (additionalInlines) {
        for (let key in additionalInlines) {
            if (additionalInlines.hasOwnProperty(key)) {
                inlines[key] = JSON.stringify(additionalInlines[key]);
            }
        }
    }

    this._inlines = inlines;
    this._squeeze = options["squeeze"] && (language != LanguageTypechecker);
}


generate()
{
    let traverser = new Traverser(this._file.ast);

    let model    = this._model;
    let modifier = this._modifier;
    let language = this._language;
    let options  = this._options;
    let inlines  = this._inlines;
    let scope    = null;

    let methodNodes = [ ];
    let methodNodeClasses = [ ];
    let currentClass;
    let currentMethodNode;

    let methodUsesSelfVar = false;

    let optionWarnDebugger            = options["warn-debugger"];
    let optionWarnEmptyArrayElement   = options["warn-empty-array-element"];
    let optionWarnGlobalNoType        = options["warn-global-no-type"];
    let optionWarnThisInMethods       = options["warn-this-in-methods"];
    let optionWarnSelfInNonMethod     = options["warn-self-in-non-methods"];
    let optionWarnUnknownIvars        = options["warn-unknown-ivars"];
    let optionWarnUnknownSelectors    = options["warn-unknown-selectors"];
    let optionWarnUnknownSuperclasses = options["warn-unknown-superclasses"];
    let optionWarnUnusedIvars         = options["warn-unused-ivars"];
    let optionStrictFunctions         = options["strict-functions"];
    let optionStrictObjectLiterals    = options["strict-object-literals"];

    let optionSqueeze = this._squeeze;
    let symbolTyper   = model.getSymbolTyper();

    let knownSelectors = optionWarnUnknownSelectors ? model.selectors : null;

    let rewriteFunctionParameters = (language === LanguageTypechecker) && !optionStrictFunctions;

    let usedIvarMap = null;
    let assignedIvarMap = null;

    // Addition for push/pop based block-scope-model (a model added in Raweden fork).
    let scopeChain = [];
    let currentScope = null;

    let warnings = [ ];

    function makeScope(node)
    {
        scope = { node: node, declarations: [ ], count: 0, previous: scope };
    }

    function canDeclareTemporaryVariable()
    {
        return scope && scope.node && (
            scope.node.type === Syntax.FunctionDeclaration     ||
            scope.node.type === Syntax.FunctionExpression      ||
            scope.node.type === Syntax.ArrowFunctionExpression ||
            scope.node.type === Syntax.OJMethodDefinition
        );
    }

    function makeTemporaryVariable(needsDeclaration)
    {
        let name = OJTemporaryVariablePrefix + scope.count++;
        if (needsDeclaration) scope.declarations.push(name);
        return name;
    }

    function getClassAsRuntimeVariable(className)
    {
        if (language === LanguageEcmascript5) {
            return OJRootWithClassPrefix + symbolTyper.getSymbolForClassName(className);
        }

        return symbolTyper.getSymbolForClassName(className);
    }

    function getCurrentMethodInModel() {
        if (!currentClass || !currentMethodNode) return null;

        let selectorType = currentMethodNode.selectorType;
        let selectorName = currentMethodNode.selectorName;

        if (selectorType == "+") {
            return currentClass.getImplementedClassMethodWithName(selectorName);
        } else {
            return currentClass.getImplementedInstanceMethodWithName(selectorName);
        }
    }

    function generateMethodDeclaration(isClassMethod, selectorName)
    {
        if (language === LanguageEcmascript5) {
            let where = isClassMethod ? OJClassMethodsVariable : OJInstanceMethodsVariable;
            return where + "." + symbolTyper.getSymbolForSelectorName(selectorName);
        }
    }

    function generateThisIvar(className, ivarName, useSelf)
    {
        return (useSelf ? "self" : "this") + "." + symbolTyper.getSymbolForClassNameAndIvarName(className, ivarName);
    }

    /**
     * Generates script that sets the initial values for instance variables.
     * 
     * @param  {OJClass} ojClass
     * @return
     */
    function generateIvarAssignments(ojClass)
    {
        let booleanIvars = [ ];
        let numericIvars = [ ];
        let objectIvars  = [ ];

        let ivars = ojClass.getAllIvars();

        for (let i = 0, length = ivars.length; i < length; i++) {
            let ivar = ivars[i];

            if (model.isNumericType(ivar.type)) {
                numericIvars.push(ivar.name);
            } else if (model.isBooleanType(ivar.type)) {
                booleanIvars.push(ivar.name);
            } else {
                objectIvars.push(ivar.name);
            }
        }

        numericIvars.sort();
        booleanIvars.sort();
        objectIvars.sort();

        let result = "";

        if (objectIvars.length) {
            for (let i = 0, length = objectIvars.length; i < length; i++) {
                result += "this." + symbolTyper.getSymbolForClassNameAndIvarName(ojClass.name, objectIvars[i]) + "="
            }

            result += "null;"
        }

        if (numericIvars.length) {
            for (let i = 0, length = numericIvars.length; i < length; i++) {
                result += "this." + symbolTyper.getSymbolForClassNameAndIvarName(ojClass.name, numericIvars[i]) + "="
            }

            result += "0;"
        }

        if (booleanIvars.length) {
            for (let i = 0, length = booleanIvars.length; i < length; i++) {
                result += "this." + symbolTyper.getSymbolForClassNameAndIvarName(ojClass.name, booleanIvars[i]) + "="
            }

            result += "false;"
        }

        return result;
    }

    function isIdentifierTransformable(node)
    {
        let parent = node.oj_parent;

        if (parent.type === Syntax.MemberExpression) {
            // identifier.x -> true
            if (parent.object === node) {
                return true;

            // x[identifier] =  computed = true
            // x.identifier  = !computed = false
            } else {
                return parent.computed;
            }

        } else if (parent.type === Syntax.Property) {
            // { x: identifier }
            if (parent.value === node) {
                return true;

            // { [identifier]: x } =  computed = true
            // {  identifier : x } = !computed = false
            } else {
                return parent.computed;
            }
        }

        return true;   
    }

    function checkRestrictedUsage(node)
    {
        let name = node.name;

        if (!isIdentifierTransformable(node)) return;

        if (currentMethodNode && currentClass) {
            if (currentClass && currentClass.isIvar(name)) {
                Utils.throwError(OJError.RestrictedUsage, "Cannot use instance variable \"" + name + "\" here.", node);
            }
        }

        if (inlines[name] || model.globals[name]) {
            Utils.throwError(OJError.RestrictedUsage, "Cannot use compiler-inlined \"" + name + "\" here.", node);
        }
    }

    /**
     * Handles NilScript Message Expression 
     *
     * @todo Optimize the case where the returned value is not used. It currently inserts a temporary variable anyways.
     * 
     * @param  {[type]} node [description]
     * @return {[type]}      [description]
     */
    function handleOJMessageExpression(node)
    {
        let receiver     = node.receiver.value;
        let methodName   = symbolTyper.getSymbolForSelectorName(node.selectorName);
        let hasArguments = false;

        let firstSelector, lastSelector;

        if (knownSelectors && !knownSelectors[node.selectorName]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + node.selectorName + "'", node));
        }

        for (let i = 0, length = node.messageSelectors.length; i < length; i++) {
            let messageSelector = node.messageSelectors[i];

            if (messageSelector.arguments || messageSelector.argument) {
                hasArguments = true;
            }
        }

        function replaceMessageSelectors()
        {
            for (let i = 0, length = node.messageSelectors.length; i < length; i++) {
                let messageSelector = node.messageSelectors[i];

                if (!firstSelector) {
                    firstSelector = messageSelector;
                }

                if (messageSelector.arguments) {
                    let lastArgument = messageSelector.arguments[messageSelector.arguments.length - 1];

                    modifier.from(messageSelector).to(messageSelector.arguments[0]).replace("[");
                    modifier.after(lastArgument).insert("]");

                    lastSelector = lastArgument;

                } else if (messageSelector.argument) {
                    modifier.from(messageSelector).to(messageSelector.argument).remove();
                    lastSelector = messageSelector.argument;

                    if (i < (length - 1)) {
                        let nextSelector = node.messageSelectors[i+1];
                        modifier.from(messageSelector.argument).to(nextSelector).replace(",");
                    }

                } else {
                    modifier.select(messageSelector).remove()
                    lastSelector = messageSelector;
                    messageSelector.oj_skip = true;
                }
            }        
        }

        function doCommonReplacement(start, end) {
            replaceMessageSelectors();

            node.receiver.oj_skip = true;

            modifier.from(node).to(firstSelector).replace(start);
            modifier.from(lastSelector).to(node).replace(end);
        }

        // Optimization cases
        if (receiver.type == Syntax.Identifier && currentMethodNode) {
            let usesSelf   = methodUsesSelfVar || (language === LanguageTypechecker);
            let selfOrThis = usesSelf ? "self" : "this";
            let isInstance = (currentMethodNode.selectorType != "+");

            if (receiver.name == "super") {
                if (language === LanguageEcmascript5) {
                    let classSymbol = symbolTyper.getSymbolForClassName(currentClass.name );
                    doCommonReplacement(classSymbol + "." + OJSuperVariable + "." + (isInstance ? "prototype." : "") + methodName + ".call(this" + (hasArguments ? "," : ""), ")");

                } else if (language === LanguageTypechecker) {
                    let method = getCurrentMethodInModel();
                    let cast = "";

                    if (method.returnType == "instancetype") {
                        cast = "<" + symbolTyper.toTypecheckerType(currentClass.name) + ">";
                    }

                    doCommonReplacement(cast + selfOrThis + ".$oj_super()." + methodName + "(", ")");
                }
                return;

            } else if (methodName == "class" && (language !== LanguageTypechecker)) {
                if (model.classes[receiver.name]) {
                    doCommonReplacement(getClassAsRuntimeVariable(receiver.name));
                } else if (receiver.name == "self") {
                    if (isInstance) {
                        doCommonReplacement(selfOrThis + ".constructor");
                    } else {
                        doCommonReplacement(selfOrThis);
                    }

                } else {
                    doCommonReplacement("(" + receiver.name + " ? " + receiver.name + "['class'](", ") : null)");
                }
                return;

            } else if (model.classes[receiver.name]) {
                let classVariable = getClassAsRuntimeVariable(receiver.name);

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
                let ivar = generateThisIvar(currentClass.name, receiver.name, usesSelf);

                if (language === LanguageTypechecker) {
                    doCommonReplacement("(" + ivar + "." + methodName + "(", "))");
                } else {
                    doCommonReplacement("(" + ivar + " && " + ivar + "." + methodName + "(", "))");
                }

                usedIvarMap[receiver.name] = true;

                return;

            } else {
                if (language === LanguageTypechecker) {
                    doCommonReplacement("(" + receiver.name + "." + methodName + "(", "))");
                } else {
                    doCommonReplacement("(" + receiver.name + " && " + receiver.name + "." + methodName + "(", "))");
                }

                return;
            }

        } else if (canDeclareTemporaryVariable()) {
            replaceMessageSelectors();

            if (language === LanguageTypechecker) {
                modifier.from(node).to(receiver).replace("(");

                if (receiver.type == Syntax.Identifier && model.classes[receiver.name]) {
                    modifier.select(receiver).replace(getClassAsRuntimeVariable(receiver.name));
                }

                modifier.from(receiver).to(firstSelector).replace("." + methodName + "(");
                modifier.from(lastSelector).to(node).replace("))");

            } else {
                let temporaryVariable = makeTemporaryVariable(true);

                modifier.from(node).to(receiver).replace("((" + temporaryVariable + " = (");

                if (receiver.type == Syntax.Identifier && model.classes[receiver.name]) {
                    modifier.select(receiver).replace(getClassAsRuntimeVariable(receiver.name));
                }

                modifier.from(receiver).to(firstSelector).replace(")) && " + temporaryVariable + "." + methodName + "(");
                modifier.from(lastSelector).to(node).replace("))");
            }

            return;
        }

        // Slow path
        replaceMessageSelectors();

        modifier.from(node).to(receiver).replace(OJRootVariable + ".msgSend(");

        if (receiver.type == Syntax.Identifier && model.classes[receiver.name]) {
            modifier.select(receiver).replace(getClassAsRuntimeVariable(receiver.name));
        }

        let selector = "{ " + methodName + ": 1 }";

        modifier.from(receiver).to(firstSelector).replace("," + selector + (hasArguments ? "," : ""));
        modifier.from(lastSelector).to(node).replace(")");
    }

    function handleOJClassImplementation(node)
    {
        let superName     = (node.superClass && node.superClass.name);
        let classSymbol   = symbolTyper.getSymbolForClassName(node.id.name);
        let classSelector = "{" + classSymbol + ":1}";
 
        // Only allow whitelisted children inside of an implementation block
        _.each(node.body.body, child => {
            let type = child.type;
            
            if (type !== Syntax.EmptyStatement        &&
                type !== Syntax.FunctionDeclaration   &&
                type !== Syntax.VariableDeclaration   &&
                type !== Syntax.OJMethodDefinition    &&
                type !== Syntax.OJPropertyDirective   &&
                type !== Syntax.OJObserveDirective    &&
                type !== Syntax.OJDynamicDirective    &&
                type !== Syntax.OJSynthesizeDirective)
            {
                Utils.throwError(OJError.ParseError, 'Unexpected implementation child.', child);
            }

            if (type === Syntax.VariableDeclaration) {
                _.each(child.declarations, declarator => {
                    if (declarator.init) {
                        if (declarator.init.type !== Syntax.Literal &&
                            declarator.init.type !== Syntax.FunctionExpression)
                        {
                            Utils.throwError(OJError.ParseError, 'Variable declaration must be initialized to a constant.', declarator.init);
                        }
                    }
                });
            }
        });

        makeScope(node);

        let constructorCallSuper = "";
        let superSelector = null;

        if (superName) {
            constructorCallSuper = getClassAsRuntimeVariable(superName) + ".call(this);";
            superSelector        = "{" + symbolTyper.getSymbolForClassName(superName) + ":1}";

            if (optionWarnUnknownSuperclasses) {
                let superclass = model.classes[superName];

                if (!superclass || superclass.forward == true || superclass.placeholder == true) {
                    warnings.push(Utils.makeError(OJWarning.UnknownSuperclass, "Use of unknown superclass '" + superName + "'.", node.superClass));
                }
            }
        }

        let constructorSetIvars = generateIvarAssignments(currentClass);

        let startText;
        let endText;

        if (language === LanguageEcmascript5) {
            if (node.category) {
                let categorySelector = "{" + symbolTyper.getSymbolForClassName(node.category) + ":1}";

                startText = OJRootVariable + "._registerCategory(" +
                    classSelector + ", ";

            } else {
                startText = OJRootVariable + "._registerClass(" +
                    classSelector + ", " +
                    (superSelector || "null") + ", ";
            }

            startText = startText +
               
                "function(" + OJClassMethodsVariable + ", " + OJInstanceMethodsVariable + ") { " +
                "function " + classSymbol + "() { " +
                constructorCallSuper +
                constructorSetIvars  +
                "this.constructor = " + classSymbol + ";" +
                "this.$oj_id = ++" + OJRootVariable + "._id;" +
                "}";

            endText = "return " + classSymbol + ";});";
        
        } else if (language === LanguageTypechecker) {
            startText = "var $oj_unused = (function(" + OJClassMethodsVariable + " : any, " + OJInstanceMethodsVariable + " : any) { ";
            endText = "});";
        }

        if (!node.ivarDeclarations && !node.body.body.length) {
            modifier.select(node).replace(startText + endText);

        } else {
            modifier.from(node).to(node.ivarDeclarations || node.body).replace(startText);
            modifier.from(node.body).to(node).replace(endText);
        }
    }

    function handleOJInstanceVariableDeclarations_typeCheckerOnly(node)
    {
        if (!node.declarations.length) {
            modifier.select(node).remove();
            return;
        }

        modifier.from(node).to(node.declarations[0]).replace("");

        _.each(node.declarations, declaration => {
            let replacement = "";

            let parameterType = declaration.parameterType;
            let value = parameterType && parameterType.value;

            if (value) {
                replacement = "<" + symbolTyper.toTypecheckerType(value) + "> null;"
            }

            modifier.select(declaration).replace(replacement);
        });

        modifier.from(_.last(node.declarations)).to(node).replace("");
    }

    function handleMethodDefinition(node)
    {
        let methodName = symbolTyper.getSymbolForSelectorName(node.selectorName);
        let isClassMethod = node.selectorType == "+";
        let where = isClassMethod ? OJClassMethodsVariable : OJInstanceMethodsVariable;
        let args = [ ];

        makeScope(node);

        if (Utils.isReservedSelectorName(node.selectorName)) {
            Utils.throwError(OJError.ReservedMethodName, "The method name \"" + node.selectorName + "\" is reserved by the runtime and may not be overridden.", node);
        }

        if (language === LanguageTypechecker) {
            args.push("self" + " : " + symbolTyper.getSymbolForClassName(currentClass.name, isClassMethod) );
        }

        for (let i = 0, length = node.methodSelectors.length; i < length; i++) {
            let variableName = node.methodSelectors[i].variableName;
            let methodType   = node.methodSelectors[i].methodType;

            if (variableName) {
                checkRestrictedUsage(variableName);

                let name = variableName.name;

                if (language === LanguageEcmascript5) {
                    args.push(name);
                } else if (language === LanguageTypechecker) {
                    let outputType = symbolTyper.toTypecheckerType(methodType && methodType.value, Location.ImplementationParameter);
                    args.push(name + (methodType ? (" : " + outputType) : ""));
                }
            }
        }

        let definition = where + "." + methodName + " = function(" + args.join(", ") + ") ";

        if (language === LanguageTypechecker) {
            let returnType = getCurrentMethodInModel().returnType;
            returnType = symbolTyper.toTypecheckerType(returnType, Location.ImplementationReturn, currentClass);
            definition += ": " + returnType;
        }

        modifier.from(node).to(node.body).replace(definition);
        modifier.from(node.body).to(node).replace(";");
    }

    function handleLiteral(node)
    {
        let replacement;

        if (node.value === null) {
            replacement = "null";
        } else if (node.value === true) {
            replacement = "true";
        } else if (node.value === false) {
            replacement = "false";
        }

        if (replacement) {
            modifier.select(node).replace(replacement);
        }
    }

    function handleOJPredefinedMacro(node)
    {
        let name = node.name;

        let className    = currentClass      ? currentClass.name              : null;
        let selectorName = currentMethodNode ? currentMethodNode.selectorName : null;
        
        if (optionSqueeze) {
            className    = className    && symbolTyper.getSymbolForClassName(className);
            selectorName = selectorName && symbolTyper.getSymbolForSelectorName(selectorName);
        }

        if (name === "@CLASS") {
            if (currentClass) {
                modifier.select(node).replace('"' + className + '"');
            } else {
                Utils.throwError(OJError.ParseError, 'Cannot use @CLASS outside of a class implementation');
            }

        } else if (name === "@SEL" || name === "@FUNCTION" || name === "@ARGS" || name === "@FUNCTION_ARGS") {
            let currentMethod = getCurrentMethodInModel();
            let replacement   = null;

            if (className && selectorName && currentMethodNode && currentMethod) {
                let selectorType   = currentMethodNode.selectorType;
                let functionString = `${selectorType}[${className} ${selectorName}]`;
                let argsString     = "[" + (currentMethod.variableNames || [ ]).join(",") + "]";

                if (name === "@SEL") {
                    replacement = '"' + selectorName + '"';
                } else if (name === "@FUNCTION") {
                    replacement = '"' + functionString + '"';

                } else if (name === "@FUNCTION_ARGS") {
                    replacement = '"' + functionString + ' " + ' + argsString;

                } else if (name === "@ARGS") {
                    replacement = argsString;
                }
            }

            if (replacement) {
                modifier.select(node).replace(replacement);
            } else {
                Utils.throwError(OJError.ParseError, 'Cannot use ' + name + ' outside of a method definition');
            }

        } else {
            Utils.throwError(OJError.DollarOJIsReserved, 'Unknown identifier: "' + name + '"');
        }
    }

    function handleOJTypeDefinition(node)
    {
        if (language === LanguageTypechecker) {
            let typesToCheck = [ ];

            _.each(node.params, param => {
                typesToCheck.push( symbolTyper.toTypecheckerType(param.annotation.value) );
            });

            if (node.annotation) {
                typesToCheck.push( symbolTyper.toTypecheckerType(node.annotation.value) );
            }

            // Lay down a cast operation with all needed types.  This will generate a warning due to an unknown type.  
            modifier.select(node).replace("<[ " + typesToCheck.join(", ") + "]> null;");

        } else {
            modifier.select(node).remove();
        }
    }

    //
    // Managing Block-Scope Model, push/pop & getting variables.
    //

    /**
     * This function is called by the Traverser. 
     * 
     * @param  {BlockStatement|Program} blockNode
     * @return {void}
     */
    function pushBlockScope(blockNode){
        let index = scopeChain.indexOf(blockNode);
        if(index !== -1){
            throw Error("pushBlockScope() block-scope are already in scope chain");
        }
        scopeChain.push(blockNode);
        currentScope = blockNode;
    }

    /**
     * This function is called by the Traverser.
     * 
     * @param  {BlockStatement|Program} blockNode 
     * @return {BlockStatement|Program}
     */
    function popBlockScope(blockNode){
        let scope = scopeChain.pop();

        if(scope !== blockNode){
            throw Error("popBlockScope() where not equal to the asserted scope object");
        }
        let len = scopeChain.length;
        if(len > 0){
            currentScope = scopeChain[len -1];
        }else{
            currentScope = null; 
        }

        return scope;
    }

    function scopeChainHasVariable(name){

    }

    function scopeChainHasTypedVariable(name){

    }

    /**
     * Returns the scope variable definition from the first scope closest relative to the current scope.
     * 
     * @param  {String} name 
     * @return {[type]}      [description]
     */
    function scopeChainGetVariable(name){
        if(!currentScope){
            return null;
        }
        let cScope, scopeVars = currentScope._scopeVars;

        if(scopeVars.hasOwnProperty(name)){
            return scopeVars[name];
        }

        let startIdx = scopeChain.length;

        if(startIdx == 1){
            return null;
        }

        startIdx = startIdx - 2;
        // backwards loop over scope chain as the begining of the array is top-level and the end its local.
        for(let i = startIdx; i >= 0;i--){
            cScope = scopeChain[i];
            scopeVars = cScope._scopeVars;
            if(scopeVars.hasOwnProperty(name)){
                return scopeVars[name];
            }
        }

        return null;
    }

    //
    // Begining of Raweden Modification
    //

    /**
     * Validates any Usage of the global property $oj_oj/$obj_ns
     * 
     * @param  {[type]}  node   [description]
     * @param  {[type]}  parent [description]
     * @return {Boolean}        [description]
     */
    function isValidRuntimeUsage(node, parent){
        let oj_parent = parent.oj_parent;
        if(oj_parent.type !== "CallExpression"){
            return false;
        }

        let propertyName = parent.property.name;
        return NSRuntimeModel.hasCallable(propertyName);
    }

    function getInlineBlockStatement(node){
        let chain = [];
        let cnode = node;
        while(cnode !== null){
            if(cnode.type == Syntax.BlockStatement){
                return cnode;
            }
            chain.push(cnode);
            cnode = cnode.oj_parent ? cnode.oj_parent : null;
            // prevents circular references!
            if(chain.indexOf(cnode) !== -1){
                return null;
            }
        }

        return null;
    }

    /**
     * @deprecated Moved to Builder.js
     * @todo We need to take into account that the block scope may have arguments that may be used in the same way as variables.
     * 
     * @param  {}
     * @return {[type]}
     */
    function parseBlockScope(blockNode){
        console.log("-- start: handleIdentifier::parseBlockScope() --");
        
        let blockScope = {};
        let i, len, node, nodes, type;

        // handle block statements which parent are a NilScript method. arguments are part of the closure scoped variables.
        if(blockNode.oj_parent.type == "OJMethodDefinition" && blockNode.oj_parent.methodSelectors){
            let method = blockNode.oj_parent;
            nodes = method.methodSelectors;
            len = nodes.length;
            for(i = 0;i < len;i++){
                node = nodes[i];
                if(!node.variableName){
                    continue;
                }
                let scopeVar = {};
                scopeVar.kind = "arg"; // var/let/const/arg
                scopeVar.name = node.variableName.name;

                scopeVar.typed = node.methodType && node.methodType.type == Syntax.OJParameterType ? node.methodType.value : null;

                scopeVar.declaration = node;

                // we need to assert that scopeVar.name is a string value.
                if(typeof scopeVar.name !== "string"){
                    continue;
                }

                blockScope[scopeVar.name] = scopeVar;
            }
            // Working with Parameters/Arguments.
            // blockNode.oj_parent.methodSelectors[i].methodType.value  === gets the annotateted type for the argument.
            // blockNode.oj_parent.methodSelectors[i].variableName.name === gets the defined argument name.
            // 
            // working with selectors:
            // blockNode.oj_parent.methodSelectors[i].name              === gets the name segment to the argument point.
            // blockNode.oj_parent.methodSelectors[i].returnType        === gets the return type declared by the selector.
            // blockNode.oj_parent.methodSelectors[i].selectorType      === selector type either "-" or "+"
        }else if(blockNode.oj_parent == Syntax.FunctionDeclaration){
            // oj_parent: FunctionDeclaration
            //     annotation: null
            //     async: false
            //     body: BlockStatement {type: "BlockStatement", body: Array(13), loc: {…}, oj_parent: FunctionDeclaration}
            //     expression: false
            //     generator: false
            //     id: Identifier {type: "Identifier", name: "main", annotation: null, loc: {…}, oj_parent: FunctionDeclaration}
            //     loc: {start: {…}, end: {…}}
            //     oj_parent: Script {type: "Program", body: Array(6), sourceType: "script", loc: {…}}
            //     params: Array(1)
            //         0: Identifier || OJIdentifierWithAnnotation
            //             annotation: null
            //             loc: {start: {…}, end: {…}}
            //             name: "argv"
            //             oj_parent: FunctionDeclaration {type: "FunctionDeclaration", id: Identifier, params: Array(1), body: BlockStatement, generator: false, …}
            //             type: "Identifier" // (in either case always this type)
            //             __proto__: Object
            //         length: 1
            //         __proto__: Array(0)
            //     type: "FunctionDeclaration"
            //
            //  Annotation on Parameter
            //     annotation: OJTypeAnnotation
            //          loc: {start: {…}, end: {…}}
            //          oj_parent: OJIdentifierWithAnnotation {type: "Identifier", name: "argv", annotation: OJTypeAnnotation, loc: {…}, oj_parent: FunctionDeclaration}
            //          optional: false
            //          type: "OJTypeAnnotation"
            //          value: "Object"
        }

        // lets get inside the scope and get all defined var(s) and let(s)
        nodes = blockNode.body;
        len = nodes.length;

        for(i = 0;i < len;i++){
            node = nodes[i];
            if(node.type !== Syntax.VariableDeclaration){
                continue;
            }
            let varKind = node.kind;
            let vard, vard_id, vars = node.declarations;
            let noVars = vars.length;

            for(let v = 0;v < noVars;v++){
                vard = vars[v]; // variable declarion object (esprisma: VariableDeclarator)
                if(!vard.id){
                    continue;
                }

                vard_id = vard.id;
                let varName = vard_id.name;

                //let varId = typeof vard.id && vard.id.type == ";
                let scopeVar = {};
                scopeVar.kind = varKind; // var/let/const/arg
                scopeVar.name = varName;
                scopeVar.typed = vard_id.annotation && vard_id.annotation.type == "OJTypeAnnotation" ? vard_id.annotation.value : null;

                scopeVar.declaration = vard;

                blockScope[varName] = scopeVar;
            }

        }
        blockNode.scope = blockScope;
    }

    /**
     * Walks the node chain by parent upwards until it finds the context in which `self` are defined.
     * 
     * @param  {}
     * @return {OJClassImplementation}
     */
    function getSelfContext(node){
        let chain = [];
        let cnode = node;
        while(cnode !== null){
            if(cnode.type == "OJClassImplementation"){
                return cnode;
            }
            chain.push(cnode);
            cnode = cnode.oj_parent ? cnode.oj_parent : null;
            // prevents circular references!
            if(chain.indexOf(cnode) !== -1){
                return null;
            }
        }

        return null;
    }

    /**
     * Helper function which searches within the given class and sub-classing chain to retrive the 
     * data model of a defined property (defined within source code by the @property directive). 
     * 
     * @param  {String} className    The name of the class in which to search for the property.
     * @param  {String} propertyName The property name for which to search for.
     * @return {OJProperty}          A OJProperty object instance or null if the property where not found.
     */
    function getPropertyInClassOrSubclass(className, propertyName){
        if(typeof model._builtClassModelCache == "undefined"){
            // we need to build a resolved class model cache. Where we cache:
            // - a list of all properties defined with @property by the class and all its subclasses.
            // - a list of all ivars defined by the class and all its subclasses.
            // 
            // It comes at the cost of a little bit more allocated memory when compiling.
            // but would however speed up the compilation process as we dont need to loop
            // over each subclass each time we want to know if a class defines a given
            // property.
        }

        let classModels = model.classes;
        let classModel = classModels.hasOwnProperty(className) ? classModels[className] : null;
        let classChain = []; // pushes each name in here to prevent to get stuck in a infintive loop.
        let propertyMap, cName;

        // before we enter the while loop, lets add the first class to the chain.
        classChain.push(className);

        while(classModel !== null){

            propertyMap = classModel._propertyMap

            if(propertyMap.hasOwnProperty(propertyName) && typeof propertyMap[propertyName] == "object"){
                return propertyMap[propertyName];
            }

            // setting class model for next loop, if we have a super class otherwise we return null.
            if(typeof classModel.superclassName == "string" && classChain.indexOf(classModel.superclassName) == -1){
                cName = classModel.superclassName;
                classModel = classModels.hasOwnProperty(cName) ? classModels[cName] : null;
                classChain.push(cName);
            }else{
                return null;
            }
        }

        return null;
    }

    /**
     * Helper function which retrives the data-model for a instance-variable (iVar) declared by a class, it starts by
     * looking at the class referenced by `className` and if its defined by that class its looks within classes
     * declared in the sub-classing chain.
     * 
     * @param  {String} className The name of the class in which to search for the ivar name.
     * @param  {String} ivarName  A string value that specifies the instance-variable name to search for.
     * @return {OJIvar}           A OJIvar object instance or null if the property where not found.
     */
    function getIvarInClassOrSubclass(className, ivarName){
        // use built class model cache as mentioned above.

        let classModels = model.classes;
        let classModel = classModels.hasOwnProperty(className) ? classModels[className] : null;
        let classChain = []; // pushes each name in here to prevent to get stuck in a infintive loop.
        let propertyMap, cName;

        // before we enter the while loop, lets add the first class to the chain.
        classChain.push(className);

        while(classModel !== null){

            propertyMap = classModel._ivarMap

            if(propertyMap.hasOwnProperty(ivarName) && typeof propertyMap[ivarName] == "object"){
                return propertyMap[ivarName];
            }

            // setting class model for next loop, if we have a super class otherwise we return null.
            if(typeof classModel.superclassName == "string" && classChain.indexOf(classModel.superclassName) == -1){
                cName = classModel.superclassName;
                classModel = classModels.hasOwnProperty(cName) ? classModels[cName] : null;
                classChain.push(cName);
            }else{
                return null;
            }
        }

        return null;
    }

    const nativeTypes = ["String", "Boolean", "Number", "RegExp", "Array", "Object", "Function"];

    function typeIsNativeJS(typeName){
        return nativeTypes.indexOf(typeName) !== -1;
    }

    /**
     * Translates a Selector name to its runtime name.
     * @param  {[type]} selName [description]
     * @return {[type]}              [description]
     */
    function translateSelectorName(selName){
        if(selName.indexOf(":")){
            selName = selName.replace(":", "_");
        }
        return "$oj_f_" + selName;
    }

    function AssertSameLine(node1, node2){
        if(node1.loc.end.line !== node1.loc.start.line){
            throw new Error("Expected assignment to be on the same line! At line: " + node1.loc.end.line);
        }
    }

    /**
     * Replaces the content inbetween Two Nodes Used within the modification of the generator to replace 
     * the operator which is not part of the AST.
     * 
     * @param  {[type]} leftNode    [description]
     * @param  {[type]} rightNode   [description]
     * @param  {[type]} replacement [description]
     * @return {[type]}             [description]
     */
    function replaceBetween(leftNode, rightNode, replacement){
        let lineNo = leftNode.loc.end.line;
        let fromColumn = leftNode.loc.end.column;
        let toColumn = rightNode.loc.start.column;
        AssertSameLine(leftNode, rightNode);

        if(typeof replacement != "string"){
            replacement = "";
        }

        modifier._addReplacement(lineNo, fromColumn, toColumn, replacement);
    }

    /**
     * Handles MemberExpression generation based on block scoped defined variables and annoations.
     *
     * Handles compile time generation of:
     * - translation to accessors in code when using dot notation on @property (requires annotation when not in self context)
     * - translation of instance-variables to the compile-time names.          (requires annotation when not in self context)
     *
     * @todo  Walk to parent blockstatement if not found in the first, and so on.
     * @todo  Should check for @property and ivar on subclasses if not found in the first. should walk the whole subclass chain if needed.
     * 
     * @param  {[type]} node   [description]
     * @param  {[type]} parent [description]
     * @return {[type]}        [description]
     */
    function handleMemberExprAdvanced(node, parent){
        // we need to know the block-scope chain to be able to do some assertments.
        //let blockNode = getInlineBlockStatement(node.oj_parent);
        //if(!blockNode){
        //    return;
        //}
        //if(typeof blockNode.scope === "undefined"){
        //    parseBlockScope(blockNode);
        //}

        let expr = node.oj_parent; // no need to assert, as we would not passed the if above otherwise.
        let objectId = expr.object;
        let propertyId = expr.property;
        let propertyName = propertyId.name;

        if(objectId.name == "self"){
            // Handles member expression where the object in the expression is self.
            let selfCtxNode = getSelfContext(node);
            let selfClassName = selfCtxNode.id.name;
            let action = "none";
            let fn;
            let replacement;

            // Case: Getting variable declarion and initial state is MemberExpression, then its a getter operation.
            if(expr.oj_parent.type == Syntax.VariableDeclarator && expr.oj_parent.init === expr){
                action = "get";
            }

            // Case: Getting - The member expression is at the right side in the assignment expression.
            if(expr.oj_parent.type == Syntax.AssignmentExpression && expr.oj_parent.right === expr){
                action = "get";
            }

            // Case: Setting - The member expression is at the left side in the assignment expression.
            if(expr.oj_parent.type == Syntax.AssignmentExpression && expr.oj_parent.left === expr){
                action = "set";
            }
            
  
            //console.log("Getting self in context of " + selfCtxNode.id.name);

            let selfClass = model.classes.hasOwnProperty(selfClassName) ? model.classes[selfClassName] : null;
            // first we check if there is a @property defined in the model for the class.
            //let propertyDef = selfClass._propertyMap && selfClass._propertyMap.hasOwnProperty(propertyId.name) ? selfClass._propertyMap[propertyId.name] : null;
            let propertyDef = getPropertyInClassOrSubclass(selfClassName, propertyName);
            if(propertyDef){
                // handles the case were the dot notation is pointing to a defined setter/getter trough @property directive.
                
                //console.log(selfClassName + " has @property for: " + propertyId.name + " should replace member-expression with " + action + " call");


                // Case: Getting variable declarion and initial state is MemberExpression, then its a getter operation.
                if(expr.oj_parent.type == Syntax.VariableDeclarator && expr.oj_parent.init === expr){
                    replacement = translateSelectorName(propertyDef.getter) + "()";
                    modifier.select(propertyId).replace(replacement);
                }

                // Case: Getting - The member expression is at the right side in the assignment expression.
                if(expr.oj_parent.type == Syntax.AssignmentExpression && expr.oj_parent.right === expr){
                    replacement = translateSelectorName(propertyDef.getter) + "()";
                    modifier.select(propertyId).replace(replacement);
                }

                // Case: Setting - The member expression is at the left side in the assignment expression.
                if(expr.oj_parent.type == Syntax.AssignmentExpression && expr.oj_parent.left === expr){
                    if(expr.oj_parent.right.type == Syntax.Identifier){
                        // getting the genrated name for property setter.
                        replacement = translateSelectorName(propertyDef.setter);
                        // replaces the operator.
                        replaceBetween(expr.oj_parent.left, expr.oj_parent.right, "");

                        modifier.select(propertyId).replace(replacement);
                        replacement = "(" + expr.oj_parent.right.name + ")";
                        modifier.select(expr.oj_parent.right).replace(replacement);
                    }else if(expr.oj_parent.right.type == Syntax.Literal){
                        //console.log("handleMemberExprAdvanced() handle Literal assignment to setter");
                        // litieral can be anyting from String, Number, Boolean... to complex as Array and Object notations.
                        // support String, Number and Boolean inline.
                        // support Object & Array by asigning it to a temporary variable.
                        //console.log(expr.oj_parent.right);
                    }else{
                        // create a temporary variable to to hold the evaluated value of the right parameter 
                        // and then call the setter with that value..
                        //console.log("------ERROR! ADVANCED SETTER COULD NOT BE GENERATED!------")
                    }
                }

                
            }
            // secondly we check if there is a ivar declared with that name.
            //let ivarDef = selfClass._ivarMap && selfClass._ivarMap.hasOwnProperty(propertyId.name) ? selfClass._ivarMap[propertyId.name] : null;
            let ivarDef = getIvarInClassOrSubclass(selfClassName, propertyName);
            if(ivarDef){
                replacement = symbolTyper.getSymbolForClassNameAndIvarName(ivarDef.className, propertyId.name);
                modifier.select(propertyId).replace(replacement);
                //console.log("handleMemberExprAdvanced() " + selfClassName + " has instance-variable for: " + propertyId.name);
            }
            return;
        }

        let varInScope = scopeChainGetVariable(objectId.name);

        //if(blockNode.scope.hasOwnProperty(objectId.name) && blockNode.scope[objectId.name].typed !== null){
        if(varInScope && varInScope.typed !== null){
            // Handles member-expression that finds typed matches in the local block scope. For example:
            // 
            // let obj:MyClass ...code that returns the initital value...
            // obj.myProperty = aValue;
            //
            // Where this block closure looks at the defined class(es) and determine if we need to alter the code to point to a accessor,
            // or that we need to point to a instance-variable within object.
            //let objClassName = blockNode.scope[objectId.name].typed;
            let objClassName = varInScope.typed;
            // if the type declared as native JavaScript type we should not continue.
            let isNative = typeIsNativeJS(objClassName);
            if(isNative){
                return false;
            }

            let objClass = model.classes.hasOwnProperty(objClassName) ? model.classes[objClassName] : null;
            let fn, replacement;
            //console.log("handleMemberExprAdvanced() " + objectId.name + " where found in the block-scope where it annotated type is set to: " + objClassName);
            if(!objClass){
                return false;
            }
            // first of we check for @property defined on the instance of class, and subclasses.
            //let propertyDef = objClass._propertyMap && objClass._propertyMap.hasOwnProperty(propertyId.name) ? objClass._propertyMap[propertyId.name] : null;
            let propertyDef = getPropertyInClassOrSubclass(objClassName, propertyName);
            if(propertyDef){
                // handles the case were the dot notation is pointing to a defined setter/getter trough @property directive.
                //console.log("handleMemberExprAdvanced() " + objClassName + " has @property for: " + propertyId.name + " should replace member-expression with call");

                var expCtx = expr.oj_parent;

                if(expCtx.type == Syntax.VariableDeclarator || (expCtx.type == Syntax.AssignmentExpression && expCtx.right == expr)){
                    // first  condition: A type of Assignment Expression where we set the initial value of a variable.
                    // second condition: Assignment to another variable, therefor we should replace with getter accessor.
                    
                    // Replacing the property with the getter accessor. Simple work!
                    replacement = translateSelectorName(propertyDef.getter) + "()";
                    modifier.select(propertyId).replace(replacement);            

                }else if(expCtx.type == Syntax.AssignmentExpression && expCtx.left == expr){
                    // condition: We are on the left side in a Assignment Expression and should replace property with setter method call.
                    
                    if(expCtx.right.type == Syntax.Literal){
                        // expCtx.right.raw <-- the raw string value as within script.
                        // expCtx.right.value <-- evaluated to JavaScript type.
                        replacement = translateSelectorName(propertyDef.setter);

                        // removes the operator in the output.
                        replaceBetween(expCtx.left, expCtx.right, "");

                        modifier.select(propertyId).replace(replacement);
                        replacement = "(" + expCtx.right.raw + ")";
                        modifier.select(expCtx.right).replace(replacement);
                    }else if(expCtx.right.type == Syntax.Identifier){
                        replacement = translateSelectorName(propertyDef.setter);
                        
                        // removes the operator in the output.
                        replaceBetween(expCtx.left, expCtx.right, "");

                        modifier.select(propertyId).replace(replacement);
                        modifier.before(expCtx.right).insert("(");
                        //replacement = "(" + expCtx.right.name + ")";
                        modifier.after(expCtx.right).insert(")");                        
                    }else{
                        // otherwise should assign it to a temporary variable and then use it for the assignment.
                        // currently this just inlines it with paratences around..
                        replacement = translateSelectorName(propertyDef.setter);
                        
                        // removes the operator in the output.
                        replaceBetween(expCtx.left, expCtx.right, "");

                        modifier.select(propertyId).replace(replacement);
                        modifier.before(expCtx.right).insert("(");
                        //replacement = "(" + expCtx.right.name + ")";
                        modifier.after(expCtx.right).insert(")");                          
                    }
                }

            }
            // secondly we check if there is a ivar declared with that name.
            //let ivarDef = objClass._ivarMap && objClass._ivarMap.hasOwnProperty(propertyId.name) ? objClass._ivarMap[propertyId.name] : null;
            let ivarDef = getIvarInClassOrSubclass(objClassName, propertyName);
            if(ivarDef){
                //console.log("handleMemberExprAdvanced() " + objClassName + " has instance-variable for: " + propertyId.name);
                replacement = symbolTyper.getSymbolForClassNameAndIvarName(ivarDef.className, propertyId.name);
                modifier.select(propertyId).replace(replacement);
            }
        }

        //console.log("handleIdentifier() parent is MemberExpression && grandparent is VariableDeclarator (logging: node.oj_parent.oj_parent)");
        //console.log(node.oj_parent.oj_parent);
        //console.log("-- start: handleIdentifier::BlockStatement{} --");

        //console.log(blockNode);
        //console.log("-- end: handleIdentifier::BlockStatement{} --");
    }

    //
    // End of Raweden Modification.
    //

    /**
     * @param  {[type]}
     * @param  {[type]}
     * @return {void}
     */
    function handleIdentifier(node, parent){

        let name   = node.name;
        let isSelf = (name == "self");

        /*
        // Temporary making some test for implementing at compile-time code generation for setter and getters.
        // @todo: we need to handle the case for computed MemberExpressions!
        if(name == "myStringProperty" || name == "m_MyStringProperty" || name == "myAnyProperty" || name == "m_propInBase"){
            // before we run this code without the predefined name check of "myStringProperty" we need to know if the identifier could be a @property access.
            if(typeof node.oj_parent !== "undefined" && typeof node.oj_parent.oj_parent !== "undefined"){
                // parent is a member-expression and the parent of that is a Assignment Expression, we can be on both sides setter/getter
                if(node.oj_parent.type == Syntax.MemberExpression && (node.oj_parent.oj_parent.type == Syntax.AssignmentExpression || node.oj_parent.oj_parent.type == Syntax.VariableDeclarator)){

                    handleMemberExprAdvanced(node, parent);
                }
            }
        }
        */

        if(parent.type == Syntax.MemberExpression && parent.property == node){
            //console.log("handleIdentifier() --> we are in a MemberExpression where the current node is the property identifier");
            handleMemberExprAdvanced(node, parent);
        }

        if (name[0] === "$"){
            // determines if the access to $ (dollar) is accessing the underlaying objc like callable like $oj_oj.msgSend() then we dont throw a error here.
            let isValidDollarUse = false;
            if(name === OJRootVariable && (parent && parent.type == Syntax.MemberExpression)){
                isValidDollarUse = isValidRuntimeUsage(node, parent);
            }
            // original implementation with a slight modification to check if the above statement retunred true or false.
            if(!isValidDollarUse && name.indexOf("$oj") == 0) {
                if (name[3] == "$" || name[3] == "_") {
                    Utils.throwError(OJError.DollarOJIsReserved, "Identifiers may not start with \"$oj_\" or \"$oj$\"", node);
                }
            }

        } else if (name[0] === "@") {
            handleOJPredefinedMacro(node);
            return;
        }

        //
        // @todo: The code below would be better of replace with a stack based variable scope driven approach.
        //        1. We need to check if the identifier is in local scope, then it should not be replace with the $oj_oj._g replacement.
        //        2. Type check in local scope to allow access to class instance variables. In objc this is done with -> but we can use a simple dot (.)
        //        3. Access to @property defined getters and setters needs to be generated inside impl and when type is annotated.
        //
        //

        if (!isIdentifierTransformable(node)) return;

        let ojGlobal = model.globals[name];
        let replacement;

        if (ojGlobal) {
            replacement = OJRootWithGlobalPrefix + (optionSqueeze ? symbolTyper.getSymbolForIdentifierName(name) : name);

            modifier.select(node).replace(replacement);
            return;

        } else if (currentMethodNode && currentClass) {
            if (currentClass.isIvar(name) || name == "self") {
                let usesSelf = currentMethodNode && (methodUsesSelfVar || (language === LanguageTypechecker));

                if (isSelf) {
                    replacement = usesSelf ? "self" : "this";
                } else {
                    replacement = generateThisIvar(currentClass.name, name, usesSelf);
                    usedIvarMap[name] = true;

                    if (parent.type === Syntax.AssignmentExpression && 
                        parent.left.name == name)
                    {
                        assignedIvarMap[name] = true;
                    }
                }

                modifier.select(node).replace(replacement);
                return;

            }else{
                if (name[0] == "_" && optionWarnUnknownIvars && (name.length > 1)) {
                    warnings.push(Utils.makeError(OJWarning.UndeclaredInstanceVariable, "Use of undeclared instance variable " + node.name, node));
                }
            } 

        } else if (isSelf && optionWarnSelfInNonMethod && !currentMethodNode) {
            warnings.push(Utils.makeError(OJWarning.UseOfSelfInNonMethod, "Use of 'self' in non-method", node));
        }

        if (inlines) {
            let result = inlines[name];
            if (result !== undefined) {
                if (inlines.hasOwnProperty(name)) {
                    modifier.select(node).replace("" + result);
                    return;
                }
            }
        }

        if (optionSqueeze) {
            let result = symbolTyper.getSymbolForIdentifierName(name);
            if (result !== undefined) {
                modifier.select(node).replace("" + result);
                return;
            }
        }
    }


    function handleVariableDeclaration(node, parent)
    {
        for (let declaration of node.declarations) {
            checkRestrictedUsage(declaration.id);
        }
    }

    function handleOJPropertyDirective(node)
    {
        let name = node.id.name;

        let makeGetter = currentClass.shouldGenerateGetterImplementationForPropertyName(name);
        let makeSetter = currentClass.shouldGenerateSetterImplementationForPropertyName(name);
        let property   = currentClass.getPropertyWithName(name);

        let result = "";
        if (makeSetter) {
            if (language === LanguageEcmascript5) {
                let observers = currentClass.getObserversWithName(name) || [ ];
                let s = [ ];
                let ivar = generateThisIvar(currentClass.name, property.ivar, false);

                let hasObservers    = observers.length > 0;
                let changeObservers = [ ];
                let setObservers    = [ ];

                if (hasObservers) {
                    _.each(observers, observer => {
                        if (observer.change) {
                            changeObservers.push(observer);
                        } else {
                            setObservers.push(observer);
                        }
                    });

                    s.push( "var old = " + ivar + ";" );

                    _.each(setObservers, observer => {
                        let before = observer.before && symbolTyper.getSymbolForSelectorName(observer.before);
                        if (before) s.push( "this." + before + "(arg);" );
                    });

                    s.push("if (old !== arg) {");

                    _.each(changeObservers, observer => {
                        let before = observer.before && symbolTyper.getSymbolForSelectorName(observer.before);
                        if (before) s.push( "this." + before + "(arg);" );
                    });
                }

                if (property.copyOnWrite) {
                    s.push(ivar + " = " + OJRootVariable + ".makeCopy(arg);");
                } else {
                    s.push(ivar + " = arg;");
                }

                if (hasObservers) {
                    _.each(changeObservers, observer => {
                        let after = observer.after && symbolTyper.getSymbolForSelectorName(observer.after);
                        if (after) s.push( "this." + after + "(old);" );
                    });

                    if (observers.length) {
                        s.push("}");
                    }

                    _.each(setObservers, observer => {
                        let after = observer.after && symbolTyper.getSymbolForSelectorName(observer.after);
                        if (after) s.push( "this." + after + "(old);" );
                    });
                }

                result += generateMethodDeclaration(false, property.setter) + " = function(arg) { " + s.join(" ")  + "} ;"; 
            }
        }

        if (makeGetter) {
            if (language === LanguageEcmascript5) {
                result += generateMethodDeclaration(false, property.getter);

                if (property.copyOnRead) {
                    result += " = function() { return " + OJRootVariable + ".makeCopy(" + generateThisIvar(currentClass.name, property.ivar, false) + "); } ; ";
                } else {
                    result += " = function() { return " + generateThisIvar(currentClass.name, property.ivar, false) + "; } ; ";
                }
            }
        }

        if (language === LanguageTypechecker) {
            result += "<" + symbolTyper.toTypecheckerType(property.type) + "> null;";
        }

        if (!result) {
            modifier.select(node).remove();
        } else {
            modifier.select(node).replace(result);
        }
    }

    function handleOJSelectorDirective(node)
    {
        let name = symbolTyper.getSymbolForSelectorName(node.name);

        if (knownSelectors && !knownSelectors[node.name]) {
            warnings.push(Utils.makeError(OJWarning.UnknownSelector, "Use of unknown selector '" + node.name + "'", node));
        }

        modifier.select(node).replace("{ " + name + ": 1 }");
    }

    function handleOJEnumDeclaration(node)
    {
        let length = node.declarations ? node.declarations.length : 0;
        let last   = node;

        if (length) {
            let firstDeclaration = node.declarations[0];
            let lastDeclaration  = node.declarations[length - 1];

            for (let i = 0; i < length; i++) {
                let declaration = node.declarations[i];

                if (!declaration.init) {
                    modifier.after(declaration.id).insert("=" + declaration.enumValue);
                }

                if (last == node) {
                    modifier.before(declaration.id).insert("var ");
                    modifier.from(last).to(declaration.id).remove();

                } else {
                    modifier.after(last).insert("; ");
                    modifier.from(last).to(declaration.id).insert("var ");
                }

                last = declaration;
            }

            modifier.after(lastDeclaration).insert(";");
            modifier.from(lastDeclaration).to(node).replace("");

        } else {
            modifier.select(node).remove();
        }
    }

    function handleOJConstDeclaration(node)
    {
        let length = node.declarations ? node.declarations.length : 0;
        let values = [ ];

        if (length) {
            let firstDeclaration = node.declarations[0];
            modifier.from(node).to(firstDeclaration.id).replace("var ");

        } else {
            modifier.select(node).remove();
        }
    }

    function handleOJCastExpression(node)
    {
        let before = "(";
        let after  = ")";

        if (language == LanguageTypechecker) {
            before = "(<" + symbolTyper.toTypecheckerType(node.id.name) + ">(<any>(";
            after  = ")))";
        }

        modifier.from(node).to(node.argument).replace(before);
        modifier.from(node.argument).to(node).replace(after);
    }

    function handleOJAnyExpression(node)
    {
        let before = (language == LanguageTypechecker) ? "(<any>(" : "(";
        let after  = (language == LanguageTypechecker) ? "))"      : ")";

        modifier.from(node).to(node.argument).replace(before);
        modifier.from(node.argument).to(node).replace(after);
    }

    function handleOJTypeAnnotation(node, parent)
    {
        if (language === LanguageTypechecker) {
            let inValue  = node.value;
            let outValue = symbolTyper.toTypecheckerType(inValue);

            if (inValue != outValue) {
                modifier.select(node).replace(": " + outValue);
            }

        } else {
            modifier.select(node).remove();
        }
    }

    function handleOJEachStatement(node)
    {
        if (language === LanguageTypechecker) {
            let object = "";

            if (node.left.type == Syntax.VariableDeclaration) {
                object = node.left.kind + " " +  node.left.declarations[0].id.name;
            } else {
                object = node.left.name;
            }

            modifier.from(node).to(node.right).replace("for (" + object + " = $oj_$AtEachGetMember(");
            modifier.from(node.right).to(node.body).replace(") ; $oj_$AtEachTest() ; ) ");

        } else {
            let i      = makeTemporaryVariable(false);
            let length = makeTemporaryVariable(false);

            let object, array;
            let initLeft = "var ";
            let initRight = "";
            let expr = false;

            // The left side is "var foo", "let foo", etc
            if (node.left.type == Syntax.VariableDeclaration) {
                object = node.left.declarations[0].id.name;
                initLeft = node.left.kind + " " + object + ", ";

            // The left side is just an identifier
            } else if (node.left.type == Syntax.Identifier) {
                if (currentClass && currentClass.isIvar(node.left.name)) {
                    Utils.throwError(OJError.RestrictedUsage, "Cannot use ivar \"" + node.left.name + "\" on left-hand side of @each", node);
                }

                object = node.left.name;
            }

            // The right side is a simple identifier
            if (node.right.type == Syntax.Identifier && currentClass && !currentClass.isIvar(node.right.name)) {
                array = node.right.name;

            // The right side is an expression, we need an additional variable
            } else {
                array = makeTemporaryVariable(false);
                initLeft  += array + " = (";
                initRight = initRight + "), ";
                expr = true;
            }

            initRight += i + " = 0, " + length + " = (" + array + " ? " + array + ".length : 0)";

            let test      = i + " < " + length;
            let increment = i + "++";

            if (expr) {
                modifier.from(node).to(node.right).replace("for (" + initLeft);
                modifier.from(node.right).to(node.body).replace(initRight + "; " + test + "; " + increment + ") ");
            } else {
                modifier.from(node).to(node.body).replace("for (" + initLeft + initRight + "; " + test + "; " + increment + ") ");
            }

            if (node.body.body.length) {
                modifier.from(node.body).to(node.body.body[0]).insert("{" + object + " = " + array + "[" + i + "];");
            }
        }
    }

    function handleOJGlobalDeclaration(node)
    {
        let declaration = node.declaration;
        let declarators = node.declarators;

        if (optionWarnGlobalNoType) {
            let allTyped;

            if (declaration) {
                allTyped = !!declaration.annotation && _.every(declaration.params, param => !!param.annotation);

            } else if (declarators) {
                allTyped = _.every(declarators, declarator => !!declarator.id.annotation);
            }

            if (!allTyped) {
                warnings.push(Utils.makeError(OJWarning.MissingTypeAnnotation, "Missing type annotation on @global", node));
            }
        }

        if (language !== LanguageTypechecker) {
            if (declaration) {
                let name = symbolTyper.getSymbolForIdentifierName(declaration.id.name);

                modifier.from(node).to(declaration).replace(OJRootWithGlobalPrefix + name + "=");
                modifier.select(declaration.id).remove();
                declaration.id.oj_skip = true;

            } else if (declarators) {
                modifier.from(node).to(declarators[0]).remove();

                _.each(declarators, declarator => {
                    let name = symbolTyper.getSymbolForIdentifierName(declarator.id.name);

                    modifier.select(declarator.id).replace(OJRootWithGlobalPrefix + name);
                    declarator.id.oj_skip = true;
                })
            }

        } else {
            if (declaration) {
                modifier.from(node).to(declaration.id).replace("(function ");
                modifier.select(declaration.id).remove();
                modifier.after(node).insert(");");

                declaration.id.oj_skip = true;

            } else if (declarators) {
                modifier.from(node).to(declarators[0]).replace("(function() { var ");
                modifier.after(node).insert("});");

                let index = 0;
                _.each(declarators, function(declarator) {
                    modifier.select(declarator.id).replace("a" + index++);
                    declarator.id.oj_skip = true;
                });
            }
        }
    }

    function handleObjectExpression_typeCheckerOnly(node)
    {
        if (language !== LanguageTypechecker) return;
        if (optionStrictObjectLiterals) return;

        if (node.properties.length == 0) {
            modifier.select(node).replace("<any>{}");
        } else {
            modifier.from(node).to(node.properties[0]).replace("<any>{");
        }
    }

    function handleFunctionDeclarationOrExpression(node)
    {
        makeScope(node);

        _.each(node.params, param => {
            checkRestrictedUsage(param);
        });

        // Unlike JavaScript, TypeScript assumes every parameter to a function is required.
        // This results in many false positives for our JavaScript code
        //
        // Disable this by rewriting the parameter list
        //
        if (rewriteFunctionParameters) {
            let result = "function " + (node.id ? node.id.name : "") + "(";

            for (let i = 0, length = node.params.length; i < length; i++) {
                let param = node.params[i];

                let type = "any";
                if (param.annotation) {
                    type = symbolTyper.toTypecheckerType(param.annotation.value);
                    param.annotation.oj_skip = true;
                }

                result += param.name + "? : " + type + ", ";
            }

            result += "...$oj_rest)";

            if (node.annotation) {
                result += ": " + symbolTyper.toTypecheckerType(node.annotation.value);
                node.annotation.oj_skip = true;
            }

            modifier.from(node).to(node.body).replace(result);
        }
    }

    function handleProperty(node) 
    {
        let key = node.key;

        if (node.computed && (key.type === Syntax.Identifier)) {
            let ojConst = model.consts[key.name];

            if (ojConst && _.isString(ojConst.value)) {
                modifier.from(node).to(node.value).replace(ojConst.raw + ":");
                modifier.from(node.value).to(node).replace("");
                key.oj_skip = true;
            }
        }
    }

    function finishScope(scope, needsSelf)
    {
        let node = scope.node;
        let varParts = [ ];
        let toInsert = "";

        if (needsSelf && (language !== LanguageTypechecker)) varParts.push("self = this");

        _.each(scope.declarations, declaration => {
            varParts.push(declaration);
        });

        if (varParts.length) {
            toInsert += "var " + varParts.join(",") + ";";
        }

        if (toInsert.length && scope.node.body.body.length) {
            modifier.before(scope.node.body.body[0]).insert(toInsert);
        }
    }

    function checkThis(thisNode, path)
    {
        let inFunction = false;
        let inMethod   = true;

        for (let i = path.length - 1; i >= 0; i--) {
            let node = path[i];

            if (node.type == Syntax.OJMethodDefinition ||
                node.type == Syntax.OJClassImplementation ||
                node.type == Syntax.OJMessageExpression)
            {
                warnings.push(Utils.makeError(OJWarning.UseOfThisInMethod, "Use of 'this' keyword in oj method definition", thisNode));

            } else if (node.type == Syntax.FunctionDeclaration ||
                       node.type == Syntax.FunctionExpression  ||
                       node.type == Syntax.ArrowFunctionExpression) {
                break;
            }
        }
    }

    makeScope();

    traverser.traverse(function(node, parent) {
        let type = node.type;

        if (node.oj_skip) return Traverser.SkipNode;

        if (type === Syntax.OJStructDefinition                   || 
            type === Syntax.OJProtocolDefinition                 ||
            type === Syntax.OJForwardDirective                   ||
            type === Syntax.OJObserveDirective                   ||
            type === Syntax.OJSqueezeDirective                   ||
            type === Syntax.OJSynthesizeDirective                ||
            type === Syntax.OJDynamicDirective                   ||
            type === Syntax.OJEnumDeclaration                    ||
            type === Syntax.OJConstDeclaration
        ) {
            modifier.select(node).remove();
            return Traverser.SkipNode;

        } else if (type === Syntax.OJBridgedDeclaration) {
            modifier.from(node).to(node.declaration).remove();

        } else if (type === Syntax.OJClassImplementation) {
            currentClass = model.classes[node.id.name];

            _.each(currentClass.prepareWarnings, warning => {
                warnings.push(warning);
            });

            usedIvarMap = { };
            assignedIvarMap = { }

            handleOJClassImplementation(node);

        } else if (type === Syntax.OJInstanceVariableDeclarations) {
            if (language === LanguageTypechecker) {
                handleOJInstanceVariableDeclarations_typeCheckerOnly(node);
            } else {
                modifier.select(node).remove();
            }

            return Traverser.SkipNode;

        } else if (type === Syntax.OJMethodDefinition) {
            currentMethodNode = node;
            methodUsesSelfVar = false;

            handleMethodDefinition(node);

        } else if (type === Syntax.OJMessageExpression) {
            handleOJMessageExpression(node);

        } else if (type === Syntax.OJPropertyDirective) {
            handleOJPropertyDirective(node);
            return Traverser.SkipNode;

        } else if (type === Syntax.OJSelectorDirective) {
            handleOJSelectorDirective(node);

        } else if (type === Syntax.OJEnumDeclaration) {
            handleOJEnumDeclaration(node);

        } else if (type === Syntax.OJConstDeclaration) {
            handleOJConstDeclaration(node);

        } else if (type === Syntax.OJCastExpression) {
            handleOJCastExpression(node);

        } else if (type === Syntax.OJAnyExpression) {
            handleOJAnyExpression(node);

        } else if (type === Syntax.OJTypeAnnotation) {
            handleOJTypeAnnotation(node, parent);

        } else if (type === Syntax.OJEachStatement) {
            handleOJEachStatement(node);

        } else if (type === Syntax.OJGlobalDeclaration) {
            handleOJGlobalDeclaration(node);

        } else if (type === Syntax.OJPredefinedMacro) {
            handleOJPredefinedMacro(node);

        } else if (type === Syntax.OJTypeDefinition) {
            handleOJTypeDefinition(node);

        } else if (type === Syntax.Literal) {
            handleLiteral(node);

        } else if (type === Syntax.Identifier) {
            handleIdentifier(node, parent);

        } else if (type === Syntax.VariableDeclaration) {
            handleVariableDeclaration(node);

        } else if (type === Syntax.ThisExpression) {
            if (optionWarnThisInMethods) {
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

        } else if (type === Syntax.ObjectExpression) {
            if (language === LanguageTypechecker) {
                handleObjectExpression_typeCheckerOnly(node);
            }

        } else if (type === Syntax.FunctionDeclaration || type === Syntax.FunctionExpression || type === Syntax.ArrowFunctionExpression) {
            handleFunctionDeclarationOrExpression(node);
            methodUsesSelfVar = true;

        } else if (type === Syntax.Property) {
            handleProperty(node);

        // Additional warnings
        } else if (type === Syntax.ArrayExpression) {
            if (optionWarnEmptyArrayElement) {
                _.each(node.elements, element => {
                    if (element === null) {
                        warnings.push(Utils.makeError(OJWarning.UseOfEmptyArrayElement, "Use of empty array element", node));
                    }
                });
            }

        } else if (type === Syntax.DebuggerStatement) {
            if (optionWarnDebugger) {
                warnings.push(Utils.makeError(OJWarning.UseOfDebugger, "Use of debugger statement", node));
            }
        }else if(type === Syntax.BlockStatement){
            pushBlockScope(node);
        }else if(type == Syntax.Program){
            pushBlockScope(node);
        }

    }, function(node, parent) {
        let type = node.type;

        if (type === Syntax.OJClassImplementation && !node.category) {
            if (optionWarnUnusedIvars) {
                _.each(currentClass.getAllIvarNamesWithoutProperties(), ivarName => {
                    if (!usedIvarMap[ivarName]) {
                        warnings.push(Utils.makeError(OJWarning.UnusedInstanceVariable, "Unused instance variable '" + ivarName + "'", node));

                    } else if (!assignedIvarMap[ivarName]) {
                        warnings.push(Utils.makeError(OJWarning.UnassignedInstanceVariable, "Instance variable '" + ivarName + "' used but never assigned", node));
                    }
                });
            }

            currentClass = null;

        } else if (type === Syntax.OJMethodDefinition) {
            finishScope(scope, methodUsesSelfVar);
            currentMethodNode = null;

        } else if (type === Syntax.FunctionDeclaration || type === Syntax.FunctionExpression || type == Syntax.ArrowFunctionExpression) {
            finishScope(scope);
        }else if(type === Syntax.BlockStatement){
            popBlockScope(node);
        }else if(type == Syntax.Program){
            popBlockScope(node);
        }

        if (scope.node === node) {
            //console.log(scope);
            scope = scope.previous;
        }
    });

    let path = this._file.path;

    _.each(warnings, warning => {
        Utils.addFilePathToError(path, warning);
    });

    return {
        lines: this._modifier.finish(),
        warnings: warnings
    };
}

}
