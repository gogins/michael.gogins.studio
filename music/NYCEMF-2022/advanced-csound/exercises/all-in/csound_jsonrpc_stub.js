/**
 * This class provides a JSON-RPC interface to an already running instance of 
 * a csound_webserver opcode, and thus to Csound itself.
 *
 * All methods are "async" so they can be called either asynchronously or 
 * synchronously.
 */
const diagnostics_enabled = true;

class Csound {
    constructor(url) {
        this.url = url;
        this.id = 0;
    }
    async invoke_rpc(method, parameters) {
        let fetch_url = this.url + '/' + method;
        this.id = this.id + 1;
        let fetch_request = {
            method: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
              jsonrpc: '2.0',
              id: this.id,
              method: method,
              params: parameters
            })
        };    
        if (diagnostics_enabled == true) console.log("\nfetch_request:\n" + JSON.stringify(fetch_request) + "\n");
        const jsonrpc_response = await fetch(fetch_url, fetch_request);
        if (diagnostics_enabled == true) console.log("\jsonrpc_response:\n" + JSON.stringify(jsonrpc_response) + "\n");
        const jsonrpc_result = await jsonrpc_response.json();
        if (diagnostics_enabled == true) console.log("\jsonrpc_result:\n" + jsonrpc_result.result + "\n");
        // Returns not the JSON of the result, but the _value_ of the result.
        return jsonrpc_result.result;
    }
    async CompileCsdText(csd_text) {
        var params = {csd_text : csd_text};
        return this.invoke_rpc("CompileCsdText", params);
    }
    async CompileOrc(orc_code) {
        var params = {orc_code : orc_code};
        return this.invoke_rpc("CompileOrc", params);
    }
    async EvalCode(orc_code) {
        var params = {orc_code : orc_code};
        return this.invoke_rpc("EvalCode", params);
    }
    async Get0dBFS(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("Get0dBFS", params);
    }
    async GetAudioChannel(channel_name) {
        var params = {channel_name : channel_name};
        return this.invoke_rpc("GetAudioChannel", params);
    };
    async GetControlChannel(channel_name) {
        var params = {channel_name : channel_name};
        return this.invoke_rpc("GetControlChannel", params);
    };
    async GetDebug(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetDebug", params);
    };
    async GetKsmps(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetKsmps", params);
    };
    async GetNchnls(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetNchnls", params);
    };
    async GetNchnlsInput(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetNchnlsInput", params);
    };
    async GetScoreOffsetSeconds(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetScoreOffsetSeconds", params);
    };
    async GetScoreTime(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetScoreTime", params);
    };
    async GetSr(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("GetSr", params);
    };
    async GetStringChannel(channel_name) {
        var params = {channel_name : channel_name};
        return this.invoke_rpc("GetStringChannel", params);
    };
    async InputMessage(sco_code) {
        var params = {sco_code : sco_code};
        return this.invoke_rpc("InputMessage", params);
    };
    async IsScorePending(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("IsScorePending", params);
    };
    async Message(message) {
        var params = {message : message};
        this.invoke_rpc("Message", params, false);
    };
    async ReadScore(sco_code) {
        var params = {sco_code : sco_code};
        return this.invoke_rpc("ReadScore", params);
    };
    async RewindScore(callbackSuccess, callbackError) {
        var params = null;
        return this.invoke_rpc("RewindScore", params);
    };
    async ScoreEvent(opcode_code, pfields) {
        var params = {opcode_code : opcode_code, pfields : pfields};
        return this.invoke_rpc("ScoreEvent", params);
    };
    async SetControlChannel(channel_name, channel_value) {
        var params = {channel_name : channel_name, channel_value : channel_value};
        return this.invoke_rpc("SetControlChannel", params);
    };
    async SetDebug(enabled) {
        var params = {enabled : enabled};
        return this.invoke_rpc("SetDebug", params);
    };
    /** 
     * To use this, the Csound orchestra must use the `webserver_send` opcode 
     * to create an event stream and send JSON-encoded messages in it. These 
     * messages will first be decoded, then received in the callback.
     */
    async SetEventSourceCallback(event_stream_name, callback) {
        // The event source URL will become: origin + "/" + event_stream_name.
        let event_source = new EventSource(event_stream_name);
        event_source.onmessage = function(event) {
            try {
                let parsed_data = JSON.parse(event.data);
                callback(parsed_data);
            } catch (e) {
                console.log("Failed to parse: ");
                console.log(typeof event.data);
                console.log(event.data);
                console.log(e);
           };
        };
    };
    /** 
     * Implements csoundSetMessageCallback using server-sent events.
     */
    async SetMessageCallback(callback) {
        var channel_name = "csound_message_callback";
        var params = {channel_name : channel_name};
        this.SetEventSourceCallback(channel_name, callback);
        return this.invoke_rpc("SetMessageCallback", params);
    };
    async SetScoreOffsetSeconds(score_time) {
        var params = {score_time : score_time};
        return this.invoke_rpc("SetScoreOffsetSeconds", params);
    };
    async SetScorePending(pending) {
        var params = {pending : pending};
        return this.invoke_rpc("SetScorePending", params);
    };
    async SetStringChannel(channel_name, channel_value) {
        var params = {channel_name : channel_name, channel_value : channel_value};
        return this.invoke_rpc("SetStringChannel", params);
    };
    async TableLength(table_number) {
        var params = {table_number : table_number};
        return this.invoke_rpc("TableLength", params);
    };
    async TableGet(index, table_number) {
        var params = {index : index, table_number : table_number};
        return this.invoke_rpc("TableGet", params);
    };
    async TableSet(index, table_number, value) {
        var params = {index : index, table_number : table_number, value : value};
        return this.invoke_rpc("TableSet", params);
    };
};
