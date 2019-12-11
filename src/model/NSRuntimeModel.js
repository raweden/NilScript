/**
 *  NSRuntimeModel.js
 *  Runtime Model class for validating in NilScript access to $oj_oj/$obj_ns global object.
 *  (c) 2019 Jesper Svensson, href: www.raweden.se
 *  MIT license, http://www.opensource.org/licenses/mit-license.php
 */

"use strict";

/**
 * NilScript Runtime Model
 *
 * Node: 
 * The implementation and model may change in future. How the model is implemted is mostly temporary,
 * it would be better to generate this from the defined source-code. But this works for now.
 * 
 * @author Jesper Svensson 2019
 */
let NSRuntimeModel = function NSRuntimeModel(){

}

let _model = {
    "noConflict":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Object"
		},
    "getClassList":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Array"
		},
    "getSubclassesOfClass":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Array"
		},
    "getSuperclass":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Boolean"
		},
    "isObject":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Boolean"
		},
    "sel_getName":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"String",
		},
    "sel_isEqual":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Boolean"
		},
    "class_getName":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"String"
		},
    "class_getSuperclass":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Object"
		},
    "class_isSubclassOf":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Boolean"
		},
    "class_respondsToSelector":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Boolean"
		},
    "object_getClass":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"Object"
		},
    "msgSend":{
		"type":"function",
		"arguments":[],
		"noOfArguments":0,
		"returnType":"any"
	}
};


NSRuntimeModel.hasMember = function(name){

}

NSRuntimeModel.hasCallable = function(name){
    return _model.hasOwnProperty(name) && _model[name].type === "function";
}

module.exports = NSRuntimeModel;