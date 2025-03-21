/**
 * This file is generated by jsonrpcstub, DO NOT CHANGE IT MANUALLY!
 */
function Csound(url) {
    this.url = url;
    var id = 1;
    
    function doJsonRpcRequest(method, params, methodCall, callback_success, callback_error) {
        var request = {};
        if (methodCall)
            request.id = id++;
        request.jsonrpc = "2.0";
        request.method = method;
        if (params !== null) {
            request.params = params;
        }
        JSON.stringify(request);
        
        $.ajax({
            type: "POST",
            url: url,
            data: JSON.stringify(request),
            success: function (response) {
                if (methodCall) {
                    if (response.hasOwnProperty("result") && response.hasOwnProperty("id")) {
                        callback_success(response.id, response.result);
                    } else if (response.hasOwnProperty("error")) {
                        if (callback_error != null)
                            callback_error(response.error.code,response.error.message);
                    } else {
                        if (callback_error != null)
                            callback_error(-32001, "Invalid Server response: " + response);
                    }
                }
            },
            error: function () {
                if (methodCall)
                    callback_error(-32002, "AJAX Error");
            },
            dataType: "json"
        });
        return id-1;
    }
    this.doRPC = function(method, params, methodCall, callback_success, callback_error) {
        return doJsonRpcRequest(method, params, methodCall, callback_success, callback_error);
    }
}

Csound.prototype.CompileCsdText = function(csd_text, callbackSuccess, callbackError) {
    var params = {csd_text : csd_text};
    return this.doRPC("CompileCsdText", params, true, callbackSuccess, callbackError);
};
Csound.prototype.CompileOrc = function(orc_code, callbackSuccess, callbackError) {
    var params = {orc_code : orc_code};
    return this.doRPC("CompileOrc", params, true, callbackSuccess, callbackError);
};
Csound.prototype.EvalCode = function(orc_code, callbackSuccess, callbackError) {
    var params = {orc_code : orc_code};
    return this.doRPC("EvalCode", params, true, callbackSuccess, callbackError);
};
Csound.prototype.Get0dBFS = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("Get0dBFS", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetAudioChannel = function(channel_name, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name};
    return this.doRPC("GetAudioChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetControlChannel = function(channel_name, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name};
    return this.doRPC("GetControlChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetDebug = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetDebug", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetKsmps = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetKsmps", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetNchnls = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetNchnls", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetNchnlsInput = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetNchnlsInput", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetScoreOffsetSeconds = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetScoreOffsetSeconds", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetScoreTime = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetScoreTime", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetSr = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("GetSr", params, true, callbackSuccess, callbackError);
};
Csound.prototype.GetStringChannel = function(channel_name, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name};
    return this.doRPC("GetStringChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.InputMessage = function(sco_code, callbackSuccess, callbackError) {
    var params = {sco_code : sco_code};
    return this.doRPC("InputMessage", params, true, callbackSuccess, callbackError);
};
Csound.prototype.IsScorePending = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("IsScorePending", params, true, callbackSuccess, callbackError);
};
Csound.prototype.Message = function(message, callbackSuccess, callbackError) {
    var params = {message : message};
    this.doRPC("Message", params, false, callbackSuccess, callbackError);
};
Csound.prototype.ReadScore = function(sco_code, callbackSuccess, callbackError) {
    var params = {sco_code : sco_code};
    return this.doRPC("ReadScore", params, true, callbackSuccess, callbackError);
};
Csound.prototype.RewindScore = function(callbackSuccess, callbackError) {
    var params = null;
    return this.doRPC("RewindScore", params, true, callbackSuccess, callbackError);
};
Csound.prototype.ScoreEvent = function(opcode_code, pfields, callbackSuccess, callbackError) {
    var params = {opcode_code : opcode_code, pfields : pfields};
    return this.doRPC("ScoreEvent", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetControlChannel = function(channel_name, channel_value, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name, channel_value : channel_value};
    return this.doRPC("SetControlChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetDebug = function(enabled, callbackSuccess, callbackError) {
    var params = {enabled : enabled};
    return this.doRPC("SetDebug", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetMessageCallback = function(callback, callbackSuccess, callbackError) {
    var params = {callback : callback};
    return this.doRPC("SetMessageCallback", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetScoreOffsetSeconds = function(score_time, callbackSuccess, callbackError) {
    var params = {score_time : score_time};
    return this.doRPC("SetScoreOffsetSeconds", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetScorePending = function(pending, callbackSuccess, callbackError) {
    var params = {pending : pending};
    return this.doRPC("SetScorePending", params, true, callbackSuccess, callbackError);
};
Csound.prototype.SetStringChannel = function(channel_name, channel_value, callbackSuccess, callbackError) {
    var params = {channel_name : channel_name, channel_value : channel_value};
    return this.doRPC("SetStringChannel", params, true, callbackSuccess, callbackError);
};
Csound.prototype.TableLength = function(table_number, callbackSuccess, callbackError) {
    var params = {table_number : table_number};
    return this.doRPC("TableLength", params, true, callbackSuccess, callbackError);
};
Csound.prototype.TableGet = function(index, table_number, callbackSuccess, callbackError) {
    var params = {index : index, table_number : table_number};
    return this.doRPC("TableGet", params, true, callbackSuccess, callbackError);
};
Csound.prototype.TableSet = function(index, table_number, value, callbackSuccess, callbackError) {
    var params = {index : index, table_number : table_number, value : value};
    return this.doRPC("TableSet", params, true, callbackSuccess, callbackError);
};
