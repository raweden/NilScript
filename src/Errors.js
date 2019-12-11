/*
    Errors.js
    (c) 2013-2018 musictheory.net, LLC
    MIT license, http://www.opensource.org/licenses/mit-license.php
*/

"use strict";

const OJError = { };

OJError.ParseError                     = "NSCParseError";
OJError.NotYetSupported                = "NSCNotYetSupportedError";
OJError.DuplicateDeclaration           = "NSCDuplicateDeclarationError";
OJError.DuplicatePropertyDefinition    = "NSCDuplicatePropertyDefinitionError";
OJError.DuplicateMethodDefinition      = "NSCDuplicateMethodDefinitionError";
OJError.DuplicateIvarDefinition        = "NSCDuplicateIvarDefinitionError";
OJError.DuplicateEnumDefinition        = "NSCDuplicateEnumDefinition";
OJError.UnknownProperty                = "NSCUnknownPropertyError";
OJError.DuplicateJavascriptFunction    = "NSCDuplicateJavascriptFunctionError";
OJError.PropertyAlreadySynthesized     = "NSCPropertyAlreadySynthesizedError";
OJError.PropertyAlreadyDynamic         = "NSCPropertyAlreadyDynamicError";
OJError.InstanceVariableAlreadyClaimed = "NSCInstanceVariableAlreadyClaimedError";
OJError.NonLiteralConst                = "NSCNonLiteralConstError";
OJError.NonLiteralEnum                 = "NSCNonLiteralEnumError";
OJError.NonIntegerEnum                 = "NSCNonIntegerEnumError";
OJError.SelfIsReserved                 = "NSCSelfIsReservedError";
OJError.DollarOJIsReserved             = "NSCDollarNSIsReservedError";
OJError.ReservedMethodName             = "NSCReservedMethodNameError";
OJError.SqueezerReachedEndIndex        = "NSCSqueezerReachedEndIndexError";
OJError.CircularTypeHierarchy          = "NSCCircularTypeHierarchyError";
OJError.VariableAlreadyDeclared        = "NSCVariableAlreadyDeclaredError";
OJError.VariableNotYetDeclared         = "NSCVariableNotYetDeclaredError";
OJError.RestrictedUsage                = "NSCRestrictedUsageError";
OJError.APIMisuse                      = "NSCAPIMisuseError";

const OJWarning = { };

OJWarning.CircularClassHierarchy       = "NSCCircularClassHierarchyWarning";
OJWarning.UnknownSuperclass            = "NSCUnknownSuperclassWarning";
OJWarning.UnknownSelector              = "NSCUnknownSelectorWarning";
OJWarning.UseOfThisInMethod            = "NSCUseOfThisInMethodWarning";
OJWarning.UseOfSelfInNonMethod         = "NSCUseOfSelfInNonMethodWarning";
OJWarning.UseOfDebugger                = "NSCUseOfDebuggerWarning";
OJWarning.UseOfEmptyArrayElement       = "NSCUseOfEmptyArrayElementWarning";
OJWarning.UnusedInstanceVariable       = "NSCUnusedInstanceVariableWarning";
OJWarning.UnassignedInstanceVariable   = "NSCUnassignedInstanceVariableWarning";
OJWarning.UndeclaredInstanceVariable   = "NSCUndeclaredInstanceVariableWarning";
OJWarning.MissingTypeAnnotation        = "NSCMissingTypeAnnotationWarning";
OJWarning.OnCompileFunction            = "NSCOnCompileFunctionWarning";
OJWarning.Typechecker                  = "NSCTypecheckerWarning";

module.exports = {
    OJError:   OJError,
    OJWarning: OJWarning
};
